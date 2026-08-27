# Map Vocalization Sessions
# Update date : Jun. 10, 2026

#' Map labeled vocalizations across recording sessions
#' @param lys A \code{lys} object with labels in \code{lys$vocalizations}.
#' @param labels Character vector of labels to include. \code{NULL} uses all.
#' @param label_col Character. Column in \code{lys$vocalizations} holding labels.
#'   Default \code{"vocalization_label"}.
#' @param drop_labels Character vector of label values to exclude.
#' @param session Session ID(s) or label(s) to restrict to. \code{NULL} = all.
#' @param plot Logical. Draw the map plot. Default \code{TRUE}.
#' @param save_plot Logical. Save the map plot to disk. Default \code{FALSE}.
#' @param output_dir Character. Output directory; \code{NULL} uses default.
#' @param plot_file Character. Plot filename.
#' @param map_file Character. CSV filename.
#' @param colors Named character vector mapping labels to colors.
#' @param scale_bar_seconds Numeric. Scale-bar length in seconds; \code{NULL}
#'   chooses automatically.
#' @param tz Character. Timezone. Default \code{"UTC"}.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#' @return The updated \code{lys} object (invisibly).
#' @export
map_vocalization_sessions <- function(lys,
                                      labels = NULL,
                                      label_col = "vocalization_label",
                                      drop_labels = "TBD",
                                      session = NULL,
                                      plot = TRUE,
                                      save_plot = FALSE,
                                      output_dir = NULL,
                                      plot_file = "vocalization_session_map.png",
                                      map_file = "vocalization_session_map.csv",
                                      colors = NULL,
                                      scale_bar_seconds = NULL,
                                      tz = "UTC",
                                      verbose = TRUE) {
  if (!inherits(lys, "lys")) {
    stop("lys must be a LYS object.", call. = FALSE)
  }

  if (!is.data.frame(lys$vocalizations) || !nrow(lys$vocalizations)) {
    stop("lys$vocalizations is empty. Run detect_vocalization() and label_vocalization() first.", call. = FALSE)
  }

  vocalizations <- as.data.frame(lys$vocalizations, stringsAsFactors = FALSE)
  required <- c("filename", "start_time", "end_time", label_col)
  missing <- setdiff(required, names(vocalizations))
  if (length(missing)) {
    stop(
      sprintf("lys$vocalizations is missing required column(s): %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  if (is.null(labels)) {
    labels <- sort(unique(vocalizations[[label_col]]))
    labels <- labels[!is.na(labels) & nzchar(labels)]
    labels <- setdiff(labels, drop_labels)
  }
  if (!length(labels)) {
    stop("No labels remain after dropping TBD/undefined labels.", call. = FALSE)
  }

  raw_labels <- vocalizations[[label_col]]
  compound_match <- vapply(
    strsplit(raw_labels, ";", fixed = TRUE),
    function(parts) any(trimws(parts) %in% labels),
    logical(1)
  )
  keep <- !is.na(raw_labels) &
    (raw_labels %in% labels | compound_match) &
    !raw_labels %in% drop_labels
  vocalizations <- vocalizations[keep, , drop = FALSE]
  if (!nrow(vocalizations)) {
    warning("No labeled vocalizations remain after filtering.")
    lys$vocalization_session_map <- data.frame()
    return(invisible(lys))
  }

  vocalizations <- attach_vocalization_metadata(lys, vocalizations)
  if (!is.null(session)) {
    keep_session <- vocalizations$session_id %in% session | vocalizations$session_label %in% session
    vocalizations <- vocalizations[keep_session, , drop = FALSE]
    if (!nrow(vocalizations)) {
      stop("No labeled vocalizations found for the requested session selection.", call. = FALSE)
    }
  }

  mapped <- compute_vocalization_timestamps(vocalizations, tz = tz)
  mapped <- mapped[order(mapped$session_id, mapped$absolute_start, mapped$filename), , drop = FALSE]
  rownames(mapped) <- NULL

  lys$vocalization_session_map <- mapped
  lys$misc$last_modified <- Sys.time()

  color_map <- make_label_colors(unique(mapped[[label_col]]), colors = colors)

  if (plot || save_plot) {
    plot_fn <- function() {
      plot_vocalization_session_map(
        mapped = mapped,
        label_col = label_col,
        color_map = color_map,
        scale_bar_seconds = scale_bar_seconds
      )
    }

    if (plot) {
      plot_fn()
    }

    if (save_plot) {
      output_dir <- resolve_lys_output_dir(lys$base_path, output_dir = output_dir)
      plot_dir <- file.path(output_dir, "plots", "vocalization_session_map")
      dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
      map_path <- file.path(plot_dir, map_file)
      utils::write.csv(mapped, map_path, row.names = FALSE)
      grDevices::png(
        filename = file.path(plot_dir, plot_file),
        width = 1800,
        height = max(700, 180 + 120 * length(unique(mapped$session_label))),
        res = 200
      )
      tryCatch(plot_fn(), finally = grDevices::dev.off())
      if (verbose) {
        message("Saved vocalization session map to: ", file.path(plot_dir, plot_file))
        message("Saved vocalization session map data to: ", map_path)
      }
    }
  }

  if (verbose) {
    message(sprintf("Mapped %d labeled vocalization(s) across %d session(s).",
                    nrow(mapped), length(unique(mapped$session_label))))
    message("Access mapped intervals via: lys$vocalization_session_map")
  }

  invisible(lys)
}

#' Attach session metadata to a vocalization data frame
#'
#' @description
#' Joins session-level metadata columns (session_id, session_label, etc.) from
#' lys$metadata into the vocalizations data frame when they are missing.
#'
#' @param lys A \code{lys} object
#' @param vocalizations Data frame of vocalization detections
#'
#' @return The vocalizations data frame with metadata columns added.
#'
#' @noRd
#' @keywords internal
attach_vocalization_metadata <- function(lys, vocalizations) {
  metadata <- lys$metadata
  metadata_cols <- c(
    "filename", "file_path", "relative_path", "recording_start",
    "recording_day", "recording_date", "recording_time", "bird_id",
    "session_id", "session_number", "session_label"
  )
  metadata_cols <- intersect(metadata_cols, names(metadata))
  metadata_small <- metadata[, metadata_cols, drop = FALSE]

  need_cols <- setdiff(metadata_cols, names(vocalizations))
  if (!length(need_cols)) {
    return(vocalizations)
  }

  match_key <- if ("file_path" %in% names(vocalizations) && "file_path" %in% names(metadata_small)) {
    "file_path"
  } else {
    "filename"
  }

  if (identical(match_key, "file_path")) {
    idx <- match(
      normalizePath(vocalizations$file_path, winslash = "/", mustWork = FALSE),
      normalizePath(metadata_small$file_path, winslash = "/", mustWork = FALSE)
    )
    missing_idx <- which(is.na(idx))
    if (length(missing_idx)) {
      idx[missing_idx] <- match(
        vocalizations$filename[missing_idx],
        metadata_small$filename
      )
    }
  } else {
    idx <- match(vocalizations[[match_key]], metadata_small[[match_key]])
  }
  for (col in need_cols) {
    vocalizations[[col]] <- metadata_small[[col]][idx]
  }

  vocalizations
}

#' Compute absolute and session-relative timestamps for vocalizations
#'
#' @description
#' Adds absolute_start, absolute_end, session_relative_start,
#' session_relative_end, and session_relative_mid columns to the vocalization
#' data frame based on the recording start time of each WAV file.
#'
#' @param vocalizations Data frame of vocalization detections with metadata
#' @param tz Character. Timezone string. Default \code{"UTC"}
#'
#' @return The vocalizations data frame with timestamp columns added.
#'
#' @noRd
#' @keywords internal
compute_vocalization_timestamps <- function(vocalizations, tz = "UTC") {
  recording_start <- vapply(
    seq_len(nrow(vocalizations)),
    function(i) {
      parsed <- parse_sap_filename(vocalizations$filename[i], tz = tz)
      if (identical(parsed$parse_method, "sap2011") && !is.na(parsed$recording_start)) {
        return(as.numeric(parsed$recording_start))
      }

      fallback <- vocalizations$recording_start[i]
      if (inherits(fallback, "POSIXt")) {
        return(as.numeric(fallback))
      }

      fallback_time <- suppressWarnings(as.POSIXct(fallback, tz = tz))
      as.numeric(fallback_time)
    },
    numeric(1)
  )

  if (anyNA(recording_start)) {
    stop("Could not determine recording start time for one or more vocalizations.", call. = FALSE)
  }

  vocalizations$file_recording_start <- as.POSIXct(recording_start, origin = "1970-01-01", tz = tz)
  vocalizations$absolute_start <- vocalizations$file_recording_start + vocalizations$start_time
  vocalizations$absolute_end <- vocalizations$file_recording_start + vocalizations$end_time

  session_origin <- stats::ave(
    as.numeric(vocalizations$file_recording_start),
    vocalizations$session_label,
    FUN = min
  )
  vocalizations$session_origin <- as.POSIXct(session_origin, origin = "1970-01-01", tz = tz)
  vocalizations$session_relative_start <- as.numeric(
    difftime(vocalizations$absolute_start, vocalizations$session_origin, units = "secs")
  )
  vocalizations$session_relative_end <- as.numeric(
    difftime(vocalizations$absolute_end, vocalizations$session_origin, units = "secs")
  )
  vocalizations$session_relative_mid <- (
    vocalizations$session_relative_start + vocalizations$session_relative_end
  ) / 2

  as.data.frame(vocalizations, stringsAsFactors = FALSE)
}

#' Build a label-to-color mapping
#'
#' @description
#' Returns a named character vector of colors for each unique label, either
#' from the user-supplied colors argument or auto-generated via
#' \code{grDevices::hcl.colors()}.
#'
#' @param labels Character vector of unique labels
#' @param colors Named character vector or NULL. User-supplied color overrides
#'
#' @return A named character vector mapping labels to colors.
#'
#' @noRd
#' @keywords internal
make_label_colors <- function(labels, colors = NULL) {
  labels <- sort(unique(labels))
  if (!is.null(colors)) {
    if (is.null(names(colors))) {
      if (length(colors) < length(labels)) {
        stop("colors must contain at least one color per label.", call. = FALSE)
      }
      colors <- stats::setNames(colors[seq_along(labels)], labels)
    }
    missing <- setdiff(labels, names(colors))
    if (length(missing)) {
      stop(
        sprintf("colors is missing value(s) for label(s): %s", paste(missing, collapse = ", ")),
        call. = FALSE
      )
    }
    return(colors[labels])
  }

  palette <- grDevices::hcl.colors(max(3L, length(labels)), palette = "Dark 3")
  stats::setNames(palette[seq_along(labels)], labels)
}

#' Draw the vocalization session map
#'
#' @description
#' Renders a timeline plot with one row per session and colored rectangles
#' representing labeled vocalizations, plus a scale bar.
#'
#' @param mapped Data frame from \code{compute_vocalization_timestamps()}
#' @param label_col Character. Column holding the vocalization label
#' @param color_map Named character vector of label colors
#' @param scale_bar_seconds Numeric or NULL. Scale bar length in seconds
#'
#' @return NULL (called for its side effect).
#'
#' @noRd
#' @keywords internal
plot_vocalization_session_map <- function(mapped,
                                          label_col,
                                          color_map,
                                          scale_bar_seconds = NULL) {
  sessions <- unique(mapped$session_label[order(mapped$session_id)])
  session_y <- rev(seq_along(sessions))
  names(session_y) <- sessions

  max_time <- max(mapped$session_relative_end, na.rm = TRUE)
  xlim <- c(0, max_time * 1.04)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::par(mar = c(4.5, 8, 2, 2), xaxs = "i")
  graphics::plot(
    NA,
    xlim = xlim,
    ylim = c(-0.2, length(sessions) + 0.8),
    xlab = "Time from session start (s)",
    ylab = "",
    yaxt = "n",
    main = "Labeled Vocalizations by Session"
  )
  graphics::axis(2, at = session_y, labels = names(session_y), las = 2)
  graphics::abline(h = session_y, col = "grey90", lwd = 0.8)

  row_height <- 0.34
  for (i in seq_len(nrow(mapped))) {
    y <- session_y[[as.character(mapped$session_label[i])]]
    label <- as.character(mapped[[label_col]][i])
    graphics::rect(
      xleft = mapped$session_relative_start[i],
      ybottom = y - row_height,
      xright = mapped$session_relative_end[i],
      ytop = y + row_height,
      col = color_map[[label]],
      border = NA
    )
  }

  graphics::legend(
    "topright",
    legend = names(color_map),
    fill = unname(color_map),
    border = NA,
    bty = "n",
    title = "Label"
  )

  if (is.null(scale_bar_seconds)) {
    scale_bar_seconds <- choose_scale_bar_seconds(max_time)
  }
  if (is.finite(scale_bar_seconds) && scale_bar_seconds > 0) {
    x0 <- xlim[2] - scale_bar_seconds - 0.02 * diff(xlim)
    x1 <- x0 + scale_bar_seconds
    y0 <- 0.1
    graphics::segments(x0, y0, x1, y0, lwd = 2)
    graphics::segments(c(x0, x1), y0 - 0.035, c(x0, x1), y0 + 0.035, lwd = 1.5)
    graphics::text(
      x = (x0 + x1) / 2,
      y = y0 + 0.12,
      labels = format_duration_label(scale_bar_seconds),
      cex = 0.78
    )
  }
}

#' Choose an appropriate scale bar duration
#'
#' @description
#' Selects a human-readable scale bar length that fits comfortably within
#' the plot x-axis range.
#'
#' @param max_time Numeric. Maximum time value on the x-axis (seconds)
#'
#' @return A numeric scale bar duration in seconds.
#'
#' @noRd
#' @keywords internal
choose_scale_bar_seconds <- function(max_time) {
  if (!is.finite(max_time) || max_time <= 0) {
    return(NA_real_)
  }

  candidates <- c(1, 2, 5, 10, 15, 30, 60, 120, 300, 600, 900, 1800, 3600, 7200)
  eligible <- candidates[candidates <= max_time / 8]
  if (length(eligible)) {
    return(max(eligible))
  }

  smaller <- candidates[candidates <= max_time]
  if (length(smaller)) {
    return(min(smaller))
  }

  max_time
}

#' Format a duration as a human-readable label
#'
#' @description
#' Converts seconds to a compact string, choosing hours, minutes, or seconds
#' depending on magnitude.
#'
#' @param seconds Numeric. Duration in seconds
#'
#' @return A character string such as \code{"5 min"} or \code{"30 s"}.
#'
#' @noRd
#' @keywords internal
format_duration_label <- function(seconds) {
  if (seconds >= 3600 && seconds %% 3600 == 0) {
    return(sprintf("%d h", seconds / 3600))
  }
  if (seconds >= 60 && seconds %% 60 == 0) {
    return(sprintf("%d min", seconds / 60))
  }
  sprintf("%d s", as.integer(seconds))
}
