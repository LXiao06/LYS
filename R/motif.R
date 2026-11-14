# Motif Extraction
# Update date : Sep. 21, 2026

#' Extract vocalization motifs
#'
#' @description
#' A generic function to extract fixed-window motif segments centered around
#' template detection timestamps. For a \code{lys} object, detection hits are
#' mapped to enclosing labeled bouts (e.g. from \code{\link{annotate_vocalizations}}
#' or \code{\link{map_vocalization_sessions}}) so each extracted motif inherits the
#' behavioral condition (e.g. \code{"PD SongBout"} vs. \code{"UD SongBout"}).
#'
#' @param x A \code{lys} object or a data frame containing detection timestamps.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return For a \code{lys} object, the updated object containing extracted
#'   motifs in \code{lys$motifs} (returned invisibly). For the default method,
#'   a data frame of motif segments.
#'
#' @rdname find_motif
#' @export
find_motif <- function(x, ...) {
  UseMethod("find_motif")
}


#' @rdname find_motif
#' @export
find_motif.default <- function(x, ...) {
  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop("ASAP package is required for default find_motif method.", call. = FALSE)
  }
  utils::getFromNamespace("find_motif.default", "ASAP")(x, ...)
}


#' @rdname find_motif
#' @param template_name Character. Name of the template in
#'   \code{x$templates$template_matches}.
#' @param pre_time Numeric. Time in seconds before the detection point.
#' @param lag_time Numeric. Time in seconds after the detection point.
#' @param bout_labels Character vector of bout labels to retain. If \code{NULL},
#'   all bouts are included. Default is \code{c("PD SongBout", "UD SongBout")}.
#' @param label_map Optional named character vector to re-map bout labels
#'   (e.g., \code{c("PD SongBout" = "PD", "UD SongBout" = "UD")}).
#' @param tolerance_sec Numeric. Tolerance in seconds when matching detection
#'   timestamps to bout boundaries. Default \code{0.05}.
#' @param check_duration Logical. If \code{TRUE} (default), verifies motif
#'   bounds against audio file duration on disk if the files exist.
#' @param verbose Logical. Whether to print extraction progress. Default \code{TRUE}.
#'
#' @examples
#' \dontrun{
#' # Extract motifs for PD and UD song bouts
#' lys <- find_motif(
#'   lys,
#'   template_name = "song",
#'   pre_time = 0.4,
#'   lag_time = 0.6,
#'   bout_labels = c("PD SongBout", "UD SongBout"),
#'   label_map = c("PD SongBout" = "PD", "UD SongBout" = "UD")
#' )
#' head(lys$motifs)
#' }
#'
#' @export
find_motif.lys <- function(x,
                           template_name,
                           pre_time = NULL,
                           lag_time = NULL,
                           bout_labels = c("PD SongBout", "UD SongBout"),
                           label_map = NULL,
                           tolerance_sec = 0.05,
                           check_duration = TRUE,
                           verbose = TRUE,
                           ...) {
  if (!inherits(x, "lys")) {
    stop("x must be a LYS object.", call. = FALSE)
  }

  if (missing(template_name) || !is.character(template_name) || length(template_name) != 1L) {
    stop("template_name must be a single string.", call. = FALSE)
  }

  if (is.null(x$templates$template_matches) || !template_name %in% names(x$templates$template_matches)) {
    avail <- names(x$templates$template_matches)
    stop(sprintf(
      "Template '%s' not found in template_matches. Available: %s",
      template_name,
      if (length(avail)) paste(avail, collapse = ", ") else "none"
    ), call. = FALSE)
  }

  if (is.null(pre_time) || is.null(lag_time)) {
    stop("pre_time and lag_time must be provided.", call. = FALSE)
  }

  if (!is.numeric(pre_time) || length(pre_time) != 1L || is.na(pre_time) || pre_time < 0) {
    stop("pre_time must be a single non-negative numeric value.", call. = FALSE)
  }

  if (!is.numeric(lag_time) || length(lag_time) != 1L || is.na(lag_time) || lag_time <= 0) {
    stop("lag_time must be a single positive numeric value.", call. = FALSE)
  }

  detections <- x$templates$template_matches[[template_name]]
  if (!is.data.frame(detections) || nrow(detections) == 0) {
    warning(sprintf("No detections found for template '%s'.", template_name), call. = FALSE)
    return(invisible(x))
  }

  # Identify bout annotations source table
  bouts <- if (!is.null(x$vocalization_annotations) && is.data.frame(x$vocalization_annotations) && nrow(x$vocalization_annotations) > 0) {
    x$vocalization_annotations
  } else if (!is.null(x$vocalizations) && is.data.frame(x$vocalizations) && nrow(x$vocalizations) > 0) {
    x$vocalizations
  } else if (!is.null(x$vocalization_session_map) && is.data.frame(x$vocalization_session_map) && nrow(x$vocalization_session_map) > 0) {
    x$vocalization_session_map
  } else {
    stop("No vocalization bouts found in LYS object. Run detect_vocalization() or label_vocalization() first.", call. = FALSE)
  }

  # Determine label column in bouts table
  label_col <- if ("annotated_label" %in% names(bouts)) {
    "annotated_label"
  } else if ("vocalization_label" %in% names(bouts)) {
    "vocalization_label"
  } else if ("label" %in% names(bouts)) {
    "label"
  } else {
    stop("Cannot find label column (annotated_label, vocalization_label, or label) in bouts.", call. = FALSE)
  }

  # Filter bouts by requested labels if provided
  if (!is.null(bout_labels)) {
    bouts <- bouts[bouts[[label_col]] %in% bout_labels, , drop = FALSE]
    if (nrow(bouts) == 0) {
      warning(sprintf(
        "No bouts found matching specified bout_labels: %s",
        paste(bout_labels, collapse = ", ")
      ), call. = FALSE)
      return(invisible(x))
    }
  }

  if (verbose) {
    message(sprintf(
      "Extracting motifs for template '%s' (pre_time = %.2fs, lag_time = %.2fs) across %d candidate bouts...",
      template_name, pre_time, lag_time, nrow(bouts)
    ))
  }

  # Map detections to parent bouts
  motif_records <- list()
  total_excluded <- 0L

  # Cache wav durations if checking
  wav_durations <- list()

  # Split bouts by filename for fast lookup
  bouts_by_file <- split(bouts, bouts$filename)

  for (i in seq_len(nrow(detections))) {
    det <- detections[i, ]
    fn <- det$filename
    det_time <- det$time

    file_bouts <- bouts_by_file[[fn]]
    if (is.null(file_bouts) || nrow(file_bouts) == 0) next

    # Find matching bout covering the detection time
    idx_match <- which(
      det_time >= (file_bouts$start_time - tolerance_sec) &
      det_time <= (file_bouts$end_time + tolerance_sec)
    )

    if (length(idx_match) == 0) next

    # If multiple, take the closest bout
    matched_bout <- if (length(idx_match) == 1) {
      file_bouts[idx_match, , drop = FALSE]
    } else {
      midpoints <- (file_bouts$start_time[idx_match] + file_bouts$end_time[idx_match]) / 2
      best <- idx_match[which.min(abs(det_time - midpoints))]
      file_bouts[best, , drop = FALSE]
    }

    start_t <- round(det_time - pre_time, 4)
    end_t <- round(det_time + lag_time, 4)

    # Check start time bound
    if (start_t < 0) {
      total_excluded <- total_excluded + 1L
      next
    }

    # Check file duration bound if requested and file is available
    if (check_duration) {
      wav_path <- if ("file_path" %in% names(det) && file.exists(det$file_path)) {
        det$file_path
      } else if (!is.null(x$base_path)) {
        rel <- if ("relative_path" %in% names(det)) det$relative_path else fn
        candidate <- file.path(x$base_path, rel)
        if (file.exists(candidate)) candidate else NULL
      } else {
        NULL
      }

      if (!is.null(wav_path) && file.exists(wav_path)) {
        if (is.null(wav_durations[[wav_path]])) {
          wav_durations[[wav_path]] <- tryCatch({
            hdr <- tuneR::readWave(wav_path, header = TRUE)
            hdr$samples / hdr$sample.rate
          }, error = function(e) Inf)
        }
        if (end_t > wav_durations[[wav_path]]) {
          total_excluded <- total_excluded + 1L
          next
        }
      }
    }

    raw_label <- as.character(matched_bout[[label_col]][1])
    mapped_label <- if (!is.null(label_map) && raw_label %in% names(label_map)) {
      label_map[[raw_label]]
    } else {
      raw_label
    }

    # Derive day_post_hatch / subfolder for ASAP path resolution
    dph <- if ("relative_dir" %in% names(matched_bout) && !is.na(matched_bout$relative_dir)) {
      matched_bout$relative_dir
    } else if ("relative_path" %in% names(det) && dirname(det$relative_path) != ".") {
      dirname(det$relative_path)
    } else {
      ""
    }
    if (is.na(dph) || dph == ".") dph <- ""

    motif_records[[length(motif_records) + 1L]] <- data.frame(
      filename = fn,
      day_post_hatch = dph,
      detection_time = round(det_time, 4),
      start_time = start_t,
      end_time = end_t,
      duration = pre_time + lag_time,
      label = mapped_label,
      bout_label = raw_label,
      score = if ("score" %in% names(det)) det$score else NA_real_,
      session_label = if ("session_label" %in% names(matched_bout)) matched_bout$session_label[1] else NA_character_,
      bird_id = if ("bird_id" %in% names(det)) det$bird_id else if ("bird_id" %in% names(matched_bout)) matched_bout$bird_id[1] else NA_character_,
      stringsAsFactors = FALSE
    )
  }

  if (length(motif_records) == 0) {
    warning("No valid motifs could be extracted matching the criteria.", call. = FALSE)
    return(invisible(x))
  }

  final_motifs <- do.call(rbind, motif_records)
  rownames(final_motifs) <- NULL
  class(final_motifs) <- c("segment", "data.frame")
  attr(final_motifs, "wav_dir") <- x$base_path

  x$motifs <- final_motifs
  x$misc$last_modified <- Sys.time()

  if (verbose) {
    counts <- table(final_motifs$label)
    counts_lines <- paste(sprintf("  - %s: %d", names(counts), counts), collapse = "\n")
    message(sprintf(
      "\n=== Motif Extraction Summary ===\nTotal detections scanned: %d\nValid motifs extracted: %d\n%s\nExcluded by bounds: %d\nStored in x$motifs (duration = %.2fs)\n",
      nrow(detections), nrow(final_motifs), counts_lines, total_excluded, pre_time + lag_time
    ))
  }

  invisible(x)
}


# -------------------------------------------------------------------------
# Visualize Extracted Motifs
# -------------------------------------------------------------------------

#' Visualize Audio Segments or Motifs
#'
#' @description
#' A generic function to visualize spectrograms of extracted motif renditions
#' across conditions or labels. For a \code{lys} object, displays a multi-panel
#' grid comparing samples across labels side-by-side (e.g. PD vs. UD motifs).
#'
#' @param x A \code{lys} object or a data frame containing segment information.
#' @param ... Additional arguments passed to specific methods.
#'
#' @rdname visualize_segments
#' @export
visualize_segments <- function(x, ...) {
  UseMethod("visualize_segments")
}


#' @rdname visualize_segments
#' @export
visualize_segments.default <- function(x, ...) {
  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop("ASAP package is required for default visualize_segments method.", call. = FALSE)
  }
  utils::getFromNamespace("visualize_segments.default", "ASAP")(x, ...)
}


#' @rdname visualize_segments
#' @param labels Optional character vector of labels to include (e.g. \code{c("PD", "UD")}).
#'   If \code{NULL}, all available labels are plotted.
#' @param n_samples Integer. Number of motif renditions to plot per label. Default \code{4}.
#' @param by_column Logical. If \code{TRUE} (default), displays each label in its own column,
#'   with rows corresponding to individual motif renditions.
#' @param seed Optional integer. Random seed for reproducible sampling.
#' @param fft_window_size Integer. FFT window size for spectrograms. Default \code{1024}.
#' @param overlap Numeric. Overlap percentage for spectrograms. Default \code{0.75}.
#' @param dark_mode Logical. Whether to use dark theme for spectrograms. Default \code{TRUE}.
#'
#' @examples
#' \dontrun{
#' # Inspect 4 PD and 4 UD motifs side-by-side
#' visualize_segments(lys, labels = c("PD", "UD"), n_samples = 4)
#' }
#'
#' @export
visualize_segments.lys <- function(x,
                                   labels = NULL,
                                   n_samples = 4,
                                   by_column = TRUE,
                                   seed = NULL,
                                   fft_window_size = 1024,
                                   overlap = 0.75,
                                   dark_mode = TRUE,
                                   ...) {
  if (!inherits(x, "lys")) {
    stop("x must be a LYS object.", call. = FALSE)
  }

  if (is.null(x$motifs) || !is.data.frame(x$motifs) || nrow(x$motifs) == 0) {
    stop("No motifs found in LYS object. Run find_motif() first.", call. = FALSE)
  }

  motifs_df <- x$motifs
  available_labels <- unique(motifs_df$label)

  if (is.null(labels)) {
    labels <- available_labels
  } else {
    missing_labels <- setdiff(labels, available_labels)
    if (length(missing_labels) > 0) {
      stop(sprintf(
        "Labels not found in x$motifs: %s. Available: %s",
        paste(missing_labels, collapse = ", "),
        paste(available_labels, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (!is.null(seed)) set.seed(seed)

  # Sample renditions for each label
  selected_rows <- list()
  for (lbl in labels) {
    idx <- which(motifs_df$label == lbl)
    if (length(idx) == 0) next
    sample_size <- min(n_samples, length(idx))
    selected_rows[[lbl]] <- sample(idx, size = sample_size)
  }

  if (length(selected_rows) == 0) {
    stop("No motifs available for the selected labels.", call. = FALSE)
  }

  n_lbls <- length(selected_rows)
  actual_samples <- max(vapply(selected_rows, length, integer(1)))

  # Set up multi-panel plot layout
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  if (by_column) {
    # Columns = Labels, Rows = Samples
    layout_mat <- matrix(seq_len(n_lbls * actual_samples),
                        nrow = actual_samples,
                        ncol = n_lbls,
                        byrow = FALSE)
  } else {
    # Rows = Labels, Columns = Samples
    layout_mat <- matrix(seq_len(n_lbls * actual_samples),
                        nrow = n_lbls,
                        ncol = actual_samples,
                        byrow = TRUE)
  }
  graphics::layout(layout_mat)

  # Plot each sampled motif spectrogram
  for (col_idx in seq_along(selected_rows)) {
    lbl <- names(selected_rows)[col_idx]
    row_indices <- selected_rows[[lbl]]

    for (s_idx in seq_along(row_indices)) {
      row_num <- row_indices[s_idx]
      motif_row <- motifs_df[row_num, ]

      # Resolve file path
      sound_path <- if ("file_path" %in% names(motif_row) && file.exists(motif_row$file_path)) {
        motif_row$file_path
      } else if (!is.null(x$base_path)) {
        subfolder <- if ("day_post_hatch" %in% names(motif_row) && nzchar(motif_row$day_post_hatch)) {
          motif_row$day_post_hatch
        } else {
          NULL
        }
        cand <- if (!is.null(subfolder)) {
          file.path(x$base_path, subfolder, motif_row$filename)
        } else {
          file.path(x$base_path, motif_row$filename)
        }
        if (file.exists(cand)) cand else NULL
      } else {
        NULL
      }

      if (is.null(sound_path) || !file.exists(sound_path)) {
        graphics::plot.new()
        graphics::title(sprintf("%s [file not found]\n%s", lbl, motif_row$filename), cex.main = 0.8)
        next
      }

      ASAP::visualize_song(
        x = sound_path,
        start_time_in_second = motif_row$start_time,
        end_time_in_second   = motif_row$end_time,
        fft_window_size      = fft_window_size,
        overlap              = overlap,
        dark_mode            = dark_mode,
        legend               = FALSE,
        keep.par             = TRUE,
        ...
      )
      graphics::title(
        sprintf("%s (Sample %d: %s)", lbl, s_idx, basename(sound_path)),
        line = 0.5, cex.main = 0.85
      )
    }
  }

  invisible(x)
}
