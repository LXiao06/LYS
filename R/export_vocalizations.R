# Export Vocalizations
# Update date : Jun. 10, 2026


#' Normalise export rules into a canonical data frame
#'
#' @description
#' Validates and coerces the rules argument into the canonical five-column
#' form. When rules is NULL, generates one rule per available label.
#'
#' @param rules NULL or a data frame with (at minimum) a \code{label} column
#' @param available_labels Character vector of labels present in the data
#' @param label_sep Character. Separator used in compound labels
#'
#' @return A validated data frame with columns:
#'   label, match_mode, folder, pad_start_sec, pad_end_sec.
#'
#' @noRd
#' @keywords internal
normalise_export_rules <- function(rules, available_labels, label_sep = ";") {
  # Default: one rule per unique label, exact match, folder = sanitised label
  if (is.null(rules)) {
    rules <- data.frame(
      label      = available_labels,
      match_mode = "exact",
      folder     = sanitize_detection_label(available_labels),
      pad_start_sec = 0,
      pad_end_sec   = 0,
      stringsAsFactors = FALSE
    )
    return(rules)
  }

  if (!is.data.frame(rules) || !nrow(rules)) {
    stop("rules must be a non-empty data frame.", call. = FALSE)
  }

  if (!"label" %in% names(rules)) {
    stop("rules must contain a 'label' column.", call. = FALSE)
  }

  rules$label <- as.character(rules$label)

  # match_mode: "exact"    - vocalization_label == label (including compound)
  #             "contains" - label appears as one component of a compound label
  if (!"match_mode" %in% names(rules)) {
    rules$match_mode <- "exact"
  }
  rules$match_mode <- as.character(rules$match_mode)
  bad_mode <- !rules$match_mode %in% c("exact", "contains")
  if (any(bad_mode)) {
    stop(
      sprintf(
        "Unknown match_mode value(s): %s. Use 'exact' or 'contains'.",
        paste(unique(rules$match_mode[bad_mode]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # folder: output subdirectory name; defaults to sanitised label
  if (!"folder" %in% names(rules)) {
    rules$folder <- sanitize_detection_label(rules$label)
  }
  rules$folder <- as.character(rules$folder)

  # padding (seconds added before onset / after offset before clipping)
  if (!"pad_start_sec" %in% names(rules)) rules$pad_start_sec <- 0
  if (!"pad_end_sec"   %in% names(rules)) rules$pad_end_sec   <- 0
  rules$pad_start_sec <- as.numeric(rules$pad_start_sec)
  rules$pad_end_sec   <- as.numeric(rules$pad_end_sec)

  if (anyNA(rules$pad_start_sec) || any(rules$pad_start_sec < 0) ||
      anyNA(rules$pad_end_sec)   || any(rules$pad_end_sec   < 0)) {
    stop("pad_start_sec and pad_end_sec must be non-negative numbers.", call. = FALSE)
  }

  rules
}


#' Match vocalizations to a single export rule
#'
#' @description
#' Returns a logical vector indicating which vocalization rows match the rule,
#' using either exact or contains matching.
#'
#' @param vocalizations Data frame: lys$vocalizations (labeled)
#' @param rule One-row data frame from the normalised rules table
#' @param label_col Character. Column holding the vocalization label
#' @param label_sep Character. Separator for compound labels
#'
#' @return Logical vector, TRUE for rows that match this rule.
#'
#' @noRd
#' @keywords internal
match_export_rule <- function(vocalizations, rule, label_col, label_sep = ";") {
  labels_vec <- vocalizations[[label_col]]

  if (rule$match_mode == "exact") {
    labels_vec == rule$label
  } else {
    # "contains": the target label appears as at least one component
    vapply(
      strsplit(labels_vec, label_sep, fixed = TRUE),
      function(parts) rule$label %in% trimws(parts),
      logical(1)
    )
  }
}


#' Clip and write one vocalization to disk
#'
#' @description
#' Calls \code{ASAP::create_audio_clip()} to extract and write a vocalization
#' clip. Returns the output path invisibly on success, or NULL on failure.
#'
#' @param row Single-row data frame from lys$vocalizations
#' @param dest_dir Character. Destination directory
#' @param pad_start Numeric. Seconds to prepend before the detected onset
#' @param pad_end Numeric. Seconds to append after the detected offset
#' @param overwrite Logical. Overwrite existing clips
#'
#' @return Output path (character) or NULL on failure.
#'
#' @noRd
#' @keywords internal
clip_one_vocalization <- function(row, dest_dir, pad_start, pad_end, overwrite,
                                   base_path = NULL) {
  # Resolve the source WAV path, with a relative_path fallback for portability
  # across machines (e.g. detect on machine A, export on machine B with a
  # different root but same directory structure).
  wav_path <- NA_character_

  stored_path <- if ("file_path" %in% names(row) && nzchar(row$file_path[1])) {
    row$file_path[1]
  } else {
    NA_character_
  }

  if (!is.na(stored_path) && file.exists(stored_path)) {
    wav_path <- stored_path
  } else if (!is.null(base_path) &&
             "relative_path" %in% names(row) &&
             !is.na(row$relative_path[1]) &&
             nzchar(row$relative_path[1])) {
    candidate <- file.path(base_path, row$relative_path[1])
    if (file.exists(candidate)) wav_path <- candidate
  }

  if (is.na(wav_path)) {
    tried <- paste(
      c(if (!is.na(stored_path)) paste("absolute:", stored_path),
        if (!is.null(base_path) && "relative_path" %in% names(row))
          paste("relative:", file.path(base_path, row$relative_path[1]))),
      collapse = "; "
    )
    return(list(path = NA_character_, status = "failed",
                error_msg = paste("WAV file not found. Tried:", tried)))
  }

  clip_start <- max(0, row$start_time[1] - pad_start)
  clip_end   <- row$end_time[1] + pad_end

  # Build descriptive output filename: <source_basename>_<selec>_<start_ms>-<end_ms>.wav
  src_stem <- tools::file_path_sans_ext(basename(wav_path))
  selec_str <- if ("selec" %in% names(row) && !is.na(row$selec[1])) {
    sprintf("s%03d", as.integer(row$selec[1]))
  } else {
    sprintf("%.0f", row$start_time[1] * 1000)
  }
  out_name <- sprintf(
    "%s_%s_%d-%dms.wav",
    src_stem, selec_str,
    as.integer(round(clip_start * 1000)),
    as.integer(round(clip_end   * 1000))
  )
  out_path <- file.path(dest_dir, out_name)

  if (file.exists(out_path) && !overwrite) {
    return(list(path = out_path, status = "skipped", error_msg = NA_character_))
  }

  tryCatch(
    {
      if (requireNamespace("tuneR", quietly = TRUE)) {
        header <- tuneR::readWave(wav_path, header = TRUE)
        dur <- header$samples / header$sample.rate
        c_from <- max(0, clip_start)
        c_to <- min(dur, clip_end)
        wave_clip <- tuneR::readWave(wav_path, from = c_from, to = c_to, units = "seconds")
        tuneR::writeWave(wave_clip, filename = out_path)
      } else if (requireNamespace("av", quietly = TRUE)) {
        av::av_audio_convert(
          wav_path,
          out_path,
          start_time = clip_start,
          total_time = max(0.001, clip_end - clip_start),
          verbose = FALSE
        )
      } else {
        stop("tuneR or av package is required to export clips.", call. = FALSE)
      }

      if (file.exists(out_path)) {
        list(path = out_path, status = "ok", error_msg = NA_character_)
      } else {
        list(path = NA_character_, status = "failed",
             error_msg = "File was not created after write attempt")
      }
    },
    error = function(e) {
      list(path = NA_character_, status = "failed",
           error_msg = conditionMessage(e))
    }
  )
}


# Exported function -------------------------------------------------------

#' Export vocalization clips to per-label subdirectories
#'
#' @description
#' Extracts each labeled vocalization bout from its source WAV file and saves
#' it as a short WAV clip in a dedicated output folder, organised by label.
#' This is especially useful after running \code{\link{label_vocalization}}
#' with \code{multi_label = TRUE}: compound labels such as
#' \code{"SongBout;BeggingCall"} can be exported either as a single combined
#' folder (\code{match_mode = "exact"}) or routed to the folder of each
#' constituent label (\code{match_mode = "contains"}).
#'
#' @section Export rules:
#' The \code{rules} argument is a data frame with the following columns
#' (only \code{label} is required; all others have sensible defaults):
#'
#' \describe{
#'   \item{\code{label}}{Character. The target label string.  For
#'     \code{match_mode = "exact"} this must exactly match
#'     \code{vocalization_label} (including compound labels like
#'     \code{"SongBout;BeggingCall"}).  For \code{match_mode = "contains"}
#'     it only needs to appear as one component.}
#'   \item{\code{match_mode}}{Character. \code{"exact"} (default) or
#'     \code{"contains"}.  \code{"contains"} lets you route compound-labeled
#'     clips to each participating label's folder simultaneously.}
#'   \item{\code{folder}}{Character. Subdirectory name under \code{output_dir}
#'     where matching clips are written.  Defaults to the sanitised label.}
#'   \item{\code{pad_start_sec}}{Numeric \eqn{\geq 0}. Seconds of audio added
#'     before the detected onset.  Default \code{0}.}
#'   \item{\code{pad_end_sec}}{Numeric \eqn{\geq 0}. Seconds of audio added
#'     after the detected offset.  Default \code{0}.}
#' }
#'
#' If \code{rules = NULL} (default), one rule is generated automatically for
#' every unique non-\code{drop_labels} label found in
#' \code{lys$vocalizations}, using \code{match_mode = "exact"} and zero
#' padding.
#'
#' @param lys A \code{lys} object with labeled vocalizations in
#'   \code{lys$vocalizations}.
#' @param rules Data frame of export rules, or \code{NULL} (see Details).
#' @param label_col Character. Column in \code{lys$vocalizations} holding the
#'   vocalization label.  Default \code{"vocalization_label"}.
#' @param label_sep Character. Separator used in compound labels produced by
#'   \code{label_vocalization(multi_label = TRUE)}.  Default \code{";"}.
#' @param drop_labels Character vector of label values to skip when
#'   \code{rules = NULL}.  Default \code{"TBD"}.
#' @param session Character vector of session IDs or labels to restrict
#'   export to.  \code{NULL} (default) exports all sessions.
#' @param output_dir Character. Root directory for exported clips.
#'   \code{NULL} uses a \code{vocalization_clips} folder next to
#'   \code{lys$base_path}.
#' @param overwrite Logical. Overwrite already-exported clips.
#'   Default \code{FALSE}.
#' @param save_manifest Logical. Write a CSV manifest of all exported clips
#'   to \code{output_dir}.  Default \code{TRUE}.
#' @param cores Integer. Number of parallel workers.  \code{NULL} auto-detects.
#' @param verbose Logical. Print progress messages.  Default \code{TRUE}.
#'
#' @return The \code{lys} object (invisibly).  A \code{lys$misc$last_export}
#'   list is attached with the output directory, manifest path, and per-rule
#'   clip counts.
#'
#' @examples
#' \dontrun{
#' # 1. Default: one folder per unique label, no padding
#' lys <- export_vocalizations(lys, output_dir = "/path/to/clips")
#'
#' # 2. Export only SongBout and BeggingCall, with 0.1 s padding
#' rules <- data.frame(
#'   label         = c("SongBout", "BeggingCall"),
#'   pad_start_sec = 0.1,
#'   pad_end_sec   = 0.1
#' )
#' lys <- export_vocalizations(lys, rules = rules)
#'
#' # 3. Multi-label: route compound clips to each constituent label's folder
#' rules <- data.frame(
#'   label      = c("SongBout", "BeggingCall"),
#'   match_mode = "contains",     # match inside compound labels
#'   folder     = c("songs", "calls"),
#'   pad_start_sec = c(0.05, 0.1),
#'   pad_end_sec   = c(0.05, 0.1)
#' )
#' lys <- export_vocalizations(lys, rules = rules)
#'
#' # 4. Export an exact compound label to its own folder
#' rules <- data.frame(
#'   label      = "SongBout;BeggingCall",
#'   match_mode = "exact",
#'   folder     = "co-occurring"
#' )
#' lys <- export_vocalizations(lys, rules = rules)
#' }
#'
#' @seealso \code{\link{label_vocalization}}, \code{\link{detect_vocalization}}
#' @export
export_vocalizations <- function(lys,
                                 rules        = NULL,
                                 label_col    = "vocalization_label",
                                 label_sep    = ";",
                                 drop_labels  = "TBD",
                                 session      = NULL,
                                 output_dir   = NULL,
                                 overwrite    = FALSE,
                                 save_manifest = TRUE,
                                 cores        = NULL,
                                 verbose      = TRUE) {

  if (verbose) message("\n=== Starting Vocalization Export ===")

  # Validate lys
  if (!inherits(lys, "lys")) {
    stop("lys must be a LYS object.", call. = FALSE)
  }

  if (!is.data.frame(lys$vocalizations) || !nrow(lys$vocalizations)) {
    stop(
      "lys$vocalizations is empty. ",
      "Run detect_vocalization() and label_vocalization() first.",
      call. = FALSE
    )
  }

  if (!label_col %in% names(lys$vocalizations)) {
    stop(
      sprintf("Column '%s' not found in lys$vocalizations.", label_col),
      call. = FALSE
    )
  }

  if (!"file_path" %in% names(lys$vocalizations)) {
    stop(
      "lys$vocalizations must contain a 'file_path' column. ",
      "Re-run detect_vocalization() to ensure file paths are stored.",
      call. = FALSE
    )
  }

  # Subset to requested sessions
  voc <- as.data.frame(lys$vocalizations, stringsAsFactors = FALSE)

  if (!is.null(session)) {
    keep <- logical(nrow(voc))
    if ("session_id"    %in% names(voc)) keep <- keep | voc$session_id    %in% session
    if ("session_label" %in% names(voc)) keep <- keep | voc$session_label %in% session
    voc <- voc[keep, , drop = FALSE]
    if (!nrow(voc)) {
      stop("No vocalizations found for the requested session selection.", call. = FALSE)
    }
  }

  # Resolve output directory
  if (is.null(output_dir)) {
    output_dir <- "vocalization_clips"
  }
  output_dir <- resolve_lys_output_dir(lys$base_path, output_dir = output_dir)

  # Build export rules
  available_labels <- sort(unique(voc[[label_col]]))
  available_labels <- available_labels[
    !is.na(available_labels) & nzchar(available_labels) &
      !available_labels %in% drop_labels
  ]

  if (!length(available_labels)) {
    warning("No exportable labels remain after filtering.", call. = FALSE)
    return(invisible(lys))
  }

  rules <- normalise_export_rules(rules, available_labels, label_sep = label_sep)

  if (verbose) {
    message(sprintf(
      "Export rules: %d rule(s) | output: %s",
      nrow(rules), output_dir
    ))
    for (ri in seq_len(nrow(rules))) {
      r <- rules[ri, ]
      message(sprintf(
        "  [%d] label='%s'  mode='%s'  -> folder='%s'  pad=[%.2f, %.2f]s",
        ri, r$label, r$match_mode, r$folder, r$pad_start_sec, r$pad_end_sec
      ))
    }
  }

  cores <- normalize_detection_cores(cores)

  # Process each rule
  manifest_rows <- list()
  export_summary <- list()

  for (ri in seq_len(nrow(rules))) {
    rule    <- rules[ri, , drop = FALSE]
    matches <- match_export_rule(voc, rule, label_col = label_col,
                                 label_sep = label_sep)
    subset  <- voc[matches, , drop = FALSE]

    if (!nrow(subset)) {
      if (verbose) {
        message(sprintf(
          "\n  Rule [%d] '%s': no matching vocalizations - skipping.",
          ri, rule$label
        ))
      }
      export_summary[[ri]] <- list(rule = rule$label, n_attempted = 0L,
                                   n_exported = 0L, n_skipped = 0L,
                                   n_failed = 0L)
      next
    }

    dest_dir <- file.path(output_dir, rule$folder)
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

    if (verbose) {
      message(sprintf(
        "\n  Rule [%d] '%s': %d clip(s) -> %s",
        ri, rule$label, nrow(subset), dest_dir
      ))
    }

    pad_start <- rule$pad_start_sec
    pad_end   <- rule$pad_end_sec

    row_indices <- seq_len(nrow(subset))

    # Capture the current definition of clip_one_vocalization explicitly in
    # this closure so that PSOCK workers on Linux receive the updated function
    # via serialisation rather than resolving it from the (possibly stale)
    # installed LYS namespace on each worker node.
    .clip_fn  <- clip_one_vocalization
    .base_path <- lys$base_path   # passed to workers for relative_path fallback

    clip_one <- function(i) {
      .clip_fn(
        row       = subset[i, , drop = FALSE],
        dest_dir  = dest_dir,
        pad_start = pad_start,
        pad_end   = pad_end,
        overwrite = overwrite,
        base_path = .base_path
      )
    }

    results <- parallel_apply(
      indices        = row_indices,
      FUN            = clip_one,
      cores          = cores,
      use_preschedule = FALSE
    )

    # Classify results safely regardless of worker return type
    parsed_results <- lapply(results, function(r) {
      if (is.null(r)) {
        return(list(path = NA_character_, status = "failed", error_msg = "worker returned NULL"))
      }
      if (inherits(r, "try-error")) {
        return(list(path = NA_character_, status = "failed", error_msg = as.character(r)))
      }
      if (is.list(r) && !is.null(r[["status"]])) {
        p <- if (is.null(r[["path"]]) || is.na(r[["path"]]) || !nzchar(r[["path"]])) NA_character_ else as.character(r[["path"]])
        msg <- if (!is.null(r[["error_msg"]]) && !is.na(r[["error_msg"]])) as.character(r[["error_msg"]]) else NA_character_
        return(list(path = p, status = as.character(r[["status"]]), error_msg = msg))
      }
      if (is.character(r) && length(r) == 1L) {
        if (is.na(r) || !nzchar(r)) {
          return(list(path = NA_character_, status = "failed", error_msg = "worker returned empty path"))
        }
        if (file.exists(r)) {
          return(list(path = as.character(r), status = "ok", error_msg = NA_character_))
        } else {
          return(list(path = NA_character_, status = "failed",
                      error_msg = paste("returned path does not exist:", r)))
        }
      }
      list(path = NA_character_, status = "failed",
           error_msg = paste("unexpected return type:", class(r)[1]))
    })

    statuses   <- vapply(parsed_results, function(pr) pr$status, character(1))
    out_paths  <- vapply(parsed_results, function(pr) pr$path, character(1))
    error_msgs <- vapply(parsed_results, function(pr) {
      if (is.null(pr$error_msg) || is.na(pr$error_msg)) NA_character_ else pr$error_msg
    }, character(1))

    n_attempted <- nrow(subset)
    n_exported  <- sum(statuses == "ok")
    n_skipped   <- sum(statuses == "skipped")
    n_failed    <- sum(statuses == "failed")

    if (verbose) {
      message(sprintf(
        "    Exported: %d  |  Skipped (already exist): %d  |  Failed: %d",
        n_exported, n_skipped, n_failed
      ))
      if (n_failed > 0) {
        failed_msgs <- unique(error_msgs[statuses == "failed" & !is.na(error_msgs)])
        if (length(failed_msgs)) {
          message("    Failure reason(s): ", paste(utils::head(failed_msgs, 3L), collapse = " | "))
        }
      }
    }

    # Build manifest entries for this rule
    for (i in seq_len(nrow(subset))) {
      out_path <- out_paths[i]
      manifest_rows[[length(manifest_rows) + 1L]] <- data.frame(
        export_rule       = rule$label,
        export_folder     = rule$folder,
        match_mode        = rule$match_mode,
        filename          = subset$filename[i],
        file_path         = subset$file_path[i],
        vocalization_label = subset[[label_col]][i],
        start_time        = subset$start_time[i],
        end_time          = subset$end_time[i],
        pad_start_sec     = pad_start,
        pad_end_sec       = pad_end,
        session_label     = if ("session_label" %in% names(subset)) subset$session_label[i] else NA_character_,
        recording_date    = if ("recording_date" %in% names(subset)) subset$recording_date[i] else NA_character_,
        clip_path         = if (is.na(out_path)) NA_character_ else out_path,
        export_status     = statuses[i],
        stringsAsFactors  = FALSE
      )
    }

    export_summary[[ri]] <- list(
      rule        = rule$label,
      n_attempted = n_attempted,
      n_exported  = n_exported,
      n_skipped   = n_skipped,
      n_failed    = n_failed
    )
  }

  # Save manifest
  manifest_path <- NULL
  if (save_manifest && length(manifest_rows)) {
    manifest <- do.call(rbind, manifest_rows)
    rownames(manifest) <- NULL
    manifest_path <- file.path(output_dir, "export_manifest.csv")
    utils::write.csv(manifest, manifest_path, row.names = FALSE)
    if (verbose) {
      message(sprintf("\nManifest saved to: %s", manifest_path))
    }
  }

  # Final summary
  if (verbose) {
    total_ok   <- sum(vapply(export_summary, `[[`, integer(1), "n_exported"))
    total_fail <- sum(vapply(export_summary, `[[`, integer(1), "n_failed"))
    message(sprintf(
      "\nExport complete. Total clips written: %d  |  Failed: %d",
      total_ok, total_fail
    ))
    message("Output directory: ", output_dir)
  }

  # Attach summary to lys
  lys$misc$last_export <- list(
    output_dir    = output_dir,
    manifest_path = manifest_path,
    rules_used    = rules,
    summary       = export_summary,
    exported_at   = Sys.time()
  )
  lys$misc$last_modified <- Sys.time()

  invisible(lys)
}
