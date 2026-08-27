# Label Vocalization
# Update date : Jun. 10, 2026

#' Label detected vocalizations using template-match rules
#' @param lys A \code{lys} object with detected vocalizations and template
#'   matches.
#' @param rules Data frame with columns \code{template_type}, \code{label},
#'   \code{min_matches}, and \code{priority}.
#' @param default_label Character. Label assigned when no rule matches.
#'   Default \code{"TBD"}.
#' @param multi_label Logical. When \code{TRUE}, vocalizations that satisfy
#'   multiple rules receive a compound label (e.g. \code{"SongBout;BeggingCall"})
#'   instead of only the highest-priority label. Default \code{FALSE}.
#' @param files Character vector of WAV filenames to relabel. \code{NULL}
#'   relabels all vocalizations. Other rows keep their existing labels.
#' @param cores Integer. Number of parallel workers. \code{NULL} auto-detects.
#' @param save_plot Logical. Save review spectrograms to disk. Default
#'   \code{TRUE}.
#' @param save_csv Logical. Save/update labeled vocalization tables (all-sessions
#'   and per-session CSVs) to disk. Default \code{FALSE}.
#' @param plot_percent Numeric. Percentage of files to plot. Default \code{100}.
#' @param output_dir Character. Output directory; \code{NULL} uses default.
#' @param wl Integer. Spectrogram window length in samples. Default \code{1024}.
#' @param ovlp Numeric. Spectrogram overlap percentage. Default \code{50}.
#' @param freq_range Numeric vector \code{c(min, max)} in kHz for RMS bandpass
#'   filter. Default \code{c(3, 5)}.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#' @return The updated \code{lys} object (invisibly).
#'
#' @details
#' Rules are evaluated per vocalization by counting template detections that
#' fall within the vocalization's time window, then applying the first
#' matching rule (lowest priority number). When \code{multi_label = TRUE},
#' all matching labels are concatenated with \code{";"}.
#'
#' @examples
#' \dontrun{
#' rules <- data.frame(
#'   template_type = c("song_bout", "innate_call"),
#'   label         = c("SongBout", "BeggingCall"),
#'   min_matches   = c(2L, 1L)
#' )
#' lys <- label_vocalization(lys, rules = rules)
#' }
#'
#' @export
label_vocalization <- function(lys,
                               rules,
                               default_label = "TBD",
                               multi_label = FALSE,
                               files = NULL,
                               cores = NULL,
                               save_plot = TRUE,
                               save_csv = FALSE,
                               plot_percent = 100,
                               output_dir = NULL,
                               wl = 1024,
                               ovlp = 50,
                               freq_range = c(3, 5),
                               verbose = TRUE) {
  if (verbose) message("\n=== Starting Vocalization Labeling ===")

  if (!inherits(lys, "lys")) {
    stop("lys must be a LYS object.", call. = FALSE)
  }

  if (!is.data.frame(lys$vocalizations) || !nrow(lys$vocalizations)) {
    stop("lys$vocalizations is empty. Run detect_vocalization() first.", call. = FALSE)
  }

  rules <- normalize_label_rules(rules)
  cores <- normalize_detection_cores(cores)
  template_matches <- collect_template_matches(lys)
  template_info <- lys$templates$template_info

  if (!nrow(template_matches)) {
    warning("No template matches found. All vocalizations will be labeled with default_label.")
  }

  if (nrow(template_matches) && !"template" %in% names(template_matches)) {
    stop("Template matches must contain a 'template' column.", call. = FALSE)
  }

  if (nrow(template_matches)) {
    template_matches$template_type <- template_info$template_type[
      match(template_matches$template, template_info$template_name)
    ]
    template_matches <- template_matches[!is.na(template_matches$template_type), , drop = FALSE]
  }

  unknown_rule_types <- setdiff(rules$template_type, unique(template_info$template_type))
  if (length(unknown_rule_types)) {
    warning(
      "The following rule template_type values are not present in template_info: ",
      paste(unknown_rule_types, collapse = ", ")
    )
  }

  labeled <- as.data.frame(lys$vocalizations, stringsAsFactors = FALSE)
  if (!all(c("session_id", "session_label") %in% names(labeled))) {
    labeled <- attach_vocalization_metadata(lys, labeled)
  }
  labeled$vocalization_label <- default_label
  labeled$matched_template_types <- NA_character_
  labeled$matched_rule_labels <- NA_character_

  count_cols <- paste0("n_template_", sanitize_detection_label(rules$template_type))
  for (col in unique(count_cols)) {
    labeled[[col]] <- 0L
  }

  match_key <- choose_match_key(labeled, template_matches)
  matches_by_key <- if (nrow(template_matches)) {
    split(template_matches, template_matches[[match_key]])
  } else {
    list()
  }

  if (!all(c("session_id", "session_label") %in% names(labeled))) {
    stop("Could not determine session_id and session_label for vocalizations.", call. = FALSE)
  }

  # Attach a stable row ID equal to the original row position in lys$vocalizations
  # so we can patch results back when a files filter is active
  labeled$.tmp_row_id <- seq_len(nrow(labeled))
  labeled <- labeled[order(labeled$session_id, labeled$filename, labeled$start_time), , drop = FALSE]

  # Files filter: restrict the labeling loop to the requested files only
  # and patch results back into the full table at the end so all other
  # rows keep their existing labels
  full_labeled <- NULL
  if (!is.null(files)) {
    files <- as.character(files)
    key_values <- basename(labeled[[match_key]])
    keep <- key_values %in% basename(files)
    if (!any(keep)) {
      warning(
        "None of the specified files matched any vocalization rows. ",
        "Returning lys unchanged.",
        call. = FALSE
      )
      return(invisible(lys))
    }
    full_labeled <- labeled   # full table, used for patching at write-back
    labeled     <- labeled[keep, , drop = FALSE]
    if (verbose) {
      message(sprintf(
        "File filter active: relabeling %d vocalization(s) from %d file(s).",
        nrow(labeled),
        length(unique(basename(labeled[[match_key]])))
      ))
    }
  }
  # End of files filter

  session_ids <- unique(labeled$session_id)

  if (verbose) {
    message(sprintf(
      "Starting vocalization labeling for %d vocalization(s) across %d session(s).",
      nrow(labeled),
      length(session_ids)
    ))
  }

  session_results <- list()
  for (current_session_id in session_ids) {
    session_idx <- which(labeled$session_id == current_session_id)
    session_label <- unique(labeled$session_label[session_idx])[1]
    session_display <- sprintf("%s (id=%s)", session_label, current_session_id)

    if (verbose) {
      message(sprintf(
        "\nProcessing %d vocalization(s) for session %s using %d core(s).",
        length(session_idx),
        session_display,
        min(cores, length(session_idx))
      ))
    }

    process_vocalization <- function(i) {
      tryCatch(
        label_one_vocalization(
          row = labeled[i, , drop = FALSE],
          rules = rules,
          count_cols = count_cols,
          match_key = match_key,
          matches_by_key = matches_by_key,
          default_label = default_label,
          multi_label = multi_label
        ),
        error = function(e) {
          new_detection_error(
            file_path = if ("file_path" %in% names(labeled)) labeled$file_path[i] else labeled$filename[i],
            session_label = session_label,
            message = conditionMessage(e)
          )
        }
      )
    }

    current_results <- parallel_apply(
      indices = session_idx,
      FUN = process_vocalization,
      cores = cores,
      use_preschedule = FALSE
    )

    worker_errors <- vapply(current_results, is_detection_error, logical(1))
    worker_try_errors <- vapply(current_results, inherits, logical(1), what = "try-error")
    if (any(worker_errors) || any(worker_try_errors)) {
      formatted_errors <- character(0)

      if (any(worker_errors)) {
        formatted_errors <- vapply(
          current_results[worker_errors],
          function(err) sprintf("%s: %s", err$file_path, err$message),
          character(1)
        )
      }

      if (any(worker_try_errors)) {
        formatted_errors <- c(
          formatted_errors,
          vapply(current_results[worker_try_errors], as.character, character(1))
        )
      }

      stop(
        sprintf(
          "Vocalization labeling failed for %d vocalization(s) in session %s:\n%s",
          length(formatted_errors),
          session_display,
          paste(utils::head(formatted_errors, 10L), collapse = "\n")
        ),
        call. = FALSE
      )
    }

    session_labeled <- do.call(rbind, current_results)
    rownames(session_labeled) <- NULL
    session_results[[as.character(current_session_id)]] <- session_labeled

    if (verbose) {
      label_counts <- table(session_labeled$vocalization_label, useNA = "ifany")
      message(sprintf(
        "\nProcessed session %s. Labeled vocalizations: %d",
        session_display,
        nrow(session_labeled)
      ))
      message("Session labels: ", paste(sprintf("%s=%d", names(label_counts), as.integer(label_counts)), collapse = ", "))
    }
  }

  relabeled <- do.call(rbind, session_results)
  rownames(relabeled) <- NULL

  if (!is.null(full_labeled)) {
    # Patch mode: restore rows outside the files filter with their original labels
    orig <- as.data.frame(lys$vocalizations, stringsAsFactors = FALSE)
    # Add any new columns produced during this labeling run
    for (col in setdiff(names(relabeled), names(orig))) {
      orig[[col]] <- NA
    }
    # .tmp_row_id == the 1-based row index in the original lys$vocalizations
    patch_ids  <- relabeled$.tmp_row_id
    patch_cols <- setdiff(names(relabeled), ".tmp_row_id")
    orig[patch_ids, patch_cols] <- relabeled[, patch_cols, drop = FALSE]
    labeled <- orig
  } else {
    relabeled$.tmp_row_id <- NULL
    labeled <- relabeled
  }

  lys$vocalizations <- as.data.frame(labeled, stringsAsFactors = FALSE)
  lys$misc$last_modified <- Sys.time()

  if (save_csv) {
    resolved_output_dir <- resolve_lys_output_dir(lys$base_path, output_dir = output_dir)
    results_dir <- file.path(resolved_output_dir, "vocalization_detection")
    dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

    utils::write.csv(
      lys$vocalizations,
      file = file.path(results_dir, "all_sessions_vocalizations.csv"),
      row.names = FALSE
    )

    if ("session_id" %in% names(lys$vocalizations)) {
      for (sid in unique(lys$vocalizations$session_id)) {
        sess_rows <- lys$vocalizations[lys$vocalizations$session_id == sid, , drop = FALSE]
        s_label <- if ("session_label" %in% names(sess_rows)) unique(sess_rows$session_label)[1] else as.character(sid)
        utils::write.csv(
          sess_rows,
          file = file.path(
            results_dir,
            paste0(sanitize_detection_label(s_label), "_vocalizations.csv")
          ),
          row.names = FALSE
        )
      }
    }
  }

  if (save_plot) {
    if (!is.null(full_labeled)) {
      plot_labeled <- relabeled  # Only re-plot the targeted files
    } else {
      plot_labeled <- labeled
    }
    save_labeled_vocalization_plots(
      lys = lys,
      labeled = plot_labeled,
      output_dir = output_dir,
      plot_percent = plot_percent,
      wl = wl,
      ovlp = ovlp,
      freq_range = freq_range,
      cores = cores,
      verbose = verbose
    )
  }

  if (verbose) {
    message(sprintf("\nTotal vocalizations labeled across all sessions: %d", nrow(labeled)))
    message("Vocalization labels:")
    print(table(labeled$vocalization_label, useNA = "ifany"))
    message("Access labels via: lys$vocalizations$vocalization_label")
    if (save_csv) {
      resolved_output_dir <- resolve_lys_output_dir(lys$base_path, output_dir = output_dir)
      message("Saved updated labeled vocalization tables to: ", file.path(resolved_output_dir, "vocalization_detection"))
    }
  }

  invisible(lys)
}

#' Label a single vocalization row
#'
#' @description
#' Counts template detections within the vocalization window and applies the
#' first matching rule. Returns the updated row data frame.
#'
#' @param row Single-row data frame from lys$vocalizations
#' @param rules Normalised rules data frame
#' @param count_cols Character vector of per-type count column names
#' @param match_key Character. Column name used to join against template matches
#' @param matches_by_key Named list of template match data frames split by key
#' @param default_label Character. Default label when no rule matches
#' @param multi_label Logical. Assign compound labels when multiple rules match
#'
#' @return The updated single-row data frame.
#'
#' @noRd
#' @keywords internal
label_one_vocalization <- function(row,
                                   rules,
                                   count_cols,
                                   match_key,
                                   matches_by_key,
                                   default_label,
                                   multi_label = FALSE) {
  row$vocalization_label <- default_label
  row$matched_template_types <- NA_character_
  row$matched_rule_labels <- NA_character_
  for (col in unique(count_cols)) {
    row[[col]] <- 0L
  }

  key <- row[[match_key]][1]
  file_matches <- matches_by_key[[key]]
  if (is.null(file_matches) || !nrow(file_matches)) {
    return(row)
  }

  in_window <- file_matches$time >= row$start_time[1] &
    file_matches$time <= row$end_time[1]
  window_matches <- file_matches[in_window, , drop = FALSE]
  if (!nrow(window_matches)) {
    return(row)
  }

  counts <- table(window_matches$template_type)
  for (rule_idx in seq_len(nrow(rules))) {
    count_col <- count_cols[rule_idx]
    row[[count_col]] <- template_type_count(counts, rules$template_type[rule_idx])
  }

  matched <- vapply(
    seq_len(nrow(rules)),
    function(rule_idx) {
      template_type_count(counts, rules$template_type[rule_idx]) >= rules$min_matches[rule_idx]
    },
    logical(1)
  )

  if (any(matched)) {
    matched_rules <- rules[matched, , drop = FALSE]
    matched_rules <- matched_rules[order(matched_rules$priority), , drop = FALSE]
    row$matched_template_types <- paste(matched_rules$template_type, collapse = ";")
    row$matched_rule_labels <- paste(matched_rules$label, collapse = ";")
    # Assign compound label when multi_label is TRUE, otherwise top-priority winner
    if (multi_label && nrow(matched_rules) > 1L) {
      row$vocalization_label <- paste(matched_rules$label, collapse = ";")
    } else {
      row$vocalization_label <- matched_rules$label[1]
    }
  }

  row
}

#' Normalise label rules into a canonical data frame
#'
#' @description
#' Validates and coerces a rules list or data frame to the expected format,
#' filling in optional columns with defaults.
#'
#' @param rules A data frame or list of rules
#'
#' @return A normalised data frame with columns template_type, label,
#'   min_matches, and priority.
#'
#' @noRd
#' @keywords internal
normalize_label_rules <- function(rules) {
  if (is.list(rules) && !is.data.frame(rules)) {
    rules <- as.data.frame(rules, stringsAsFactors = FALSE)
  }

  if (!is.data.frame(rules) || !nrow(rules)) {
    stop("rules must be a non-empty data frame or list.", call. = FALSE)
  }

  required <- c("template_type", "label")
  missing <- setdiff(required, names(rules))
  if (length(missing)) {
    stop(
      sprintf("rules is missing required column(s): %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  if (!"min_matches" %in% names(rules)) {
    rules$min_matches <- 2L
  }
  if (!"priority" %in% names(rules)) {
    rules$priority <- seq_len(nrow(rules))
  }

  rules$template_type <- as.character(rules$template_type)
  rules$label <- as.character(rules$label)
  rules$min_matches <- as.integer(rules$min_matches)
  rules$priority <- as.integer(rules$priority)

  if (anyNA(rules$template_type) || any(!nzchar(rules$template_type)) ||
      anyNA(rules$label) || any(!nzchar(rules$label))) {
    stop("rules$template_type and rules$label must be non-empty strings.", call. = FALSE)
  }
  if (anyNA(rules$min_matches) || any(rules$min_matches < 1L)) {
    stop("rules$min_matches must contain positive integers.", call. = FALSE)
  }
  if (anyNA(rules$priority)) {
    stop("rules$priority must contain integer priorities.", call. = FALSE)
  }

  rules
}

#' Collect all template matches from a LYS object
#'
#' @description
#' Returns the combined template match data frame from
#' \code{lys$templates$template_matches[["all"]]} or falls back to
#' row-binding all per-template frames.
#'
#' @param lys A \code{lys} object
#'
#' @return A data frame of template matches, or an empty data frame.
#'
#' @noRd
#' @keywords internal
collect_template_matches <- function(lys) {
  matches <- lys$templates$template_matches
  if (is.null(matches) || !length(matches)) {
    matches <- lys$templates$matches
  }

  if (is.null(matches) || !length(matches)) {
    return(data.frame())
  }

  if (!is.null(matches[["all"]]) && is.data.frame(matches[["all"]])) {
    return(as.data.frame(matches[["all"]], stringsAsFactors = FALSE))
  }

  dfs <- matches[vapply(matches, is.data.frame, logical(1))]
  dfs <- dfs[vapply(dfs, nrow, integer(1)) > 0L]
  if (!length(dfs)) {
    return(data.frame())
  }

  out <- do.call(rbind, dfs)
  rownames(out) <- NULL
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Choose the column to use when joining vocalizations to template matches
#'
#' @description
#' Prefers file_path for joining when it is available in both tables.
#'
#' @param vocalizations Data frame of labeled vocalizations
#' @param template_matches Data frame of template match detections
#'
#' @return A single character column name.
#'
#' @noRd
#' @keywords internal
choose_match_key <- function(vocalizations, template_matches) {
  if (nrow(template_matches) &&
      "file_path" %in% names(vocalizations) &&
      "file_path" %in% names(template_matches)) {
    return("file_path")
  }

  "filename"
}

#' Save labeled vocalization review plots
#'
#' @description
#' Generates and saves a spectrogram-based review PNG for a sample of
#' labeled WAV files, using the same two-panel layout as detection plots.
#'
#' @param lys A \code{lys} object
#' @param labeled Data frame of labeled vocalizations
#' @param output_dir Character or NULL. Output directory
#' @param plot_percent Numeric. Percentage of files to plot
#' @param wl Integer. Spectrogram window length
#' @param ovlp Numeric. Overlap percentage
#' @param freq_range Numeric vector \code{c(min, max)} in kHz
#' @param cores Integer. Number of parallel workers
#' @param verbose Logical. Print progress messages
#'
#' @return NULL (called for its side effect).
#'
#' @noRd
#' @keywords internal
save_labeled_vocalization_plots <- function(lys,
                                            labeled,
                                            output_dir,
                                            plot_percent,
                                            wl,
                                            ovlp,
                                            freq_range,
                                            cores,
                                            verbose) {
  check_audio_dependencies()

  output_dir <- resolve_lys_output_dir(lys$base_path, output_dir = output_dir)
  plots_dir <- file.path(output_dir, "plots", "vocalization_labels")
  dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

  file_key <- if ("file_path" %in% names(labeled)) "file_path" else "filename"
  session_ids <- if ("session_id" %in% names(labeled)) {
    unique(labeled$session_id)
  } else {
    "labeled"
  }

  for (current_session_id in session_ids) {
    session_rows <- if ("session_id" %in% names(labeled)) {
      labeled[labeled$session_id == current_session_id, , drop = FALSE]
    } else {
      labeled
    }

    session_label <- if ("session_label" %in% names(session_rows)) {
      unique(session_rows$session_label)[1]
    } else {
      "labeled"
    }
    session_display <- if ("session_id" %in% names(session_rows)) {
      sprintf("%s (id=%s)", session_label, current_session_id)
    } else {
      session_label
    }

    unique_files <- unique(session_rows[[file_key]])
    n_plots <- ceiling(length(unique_files) * plot_percent / 100)
    if (n_plots <= 0L) {
      next
    }
    files_to_plot <- sort(sample(unique_files, min(n_plots, length(unique_files))))

    if (verbose) {
      message(sprintf(
        "\nSaving %d labeled review plot(s) for session %s using %d core(s).",
        length(files_to_plot),
        session_display,
        min(cores, length(files_to_plot))
      ))
    }

    plot_one_file <- function(file_value) {
      tryCatch(
        {
          file_rows <- session_rows[session_rows[[file_key]] == file_value, , drop = FALSE]
          wav_file <- if (file_key == "file_path") {
            file_value
          } else {
            file.path(lys$base_path, file_value)
          }

          if (!file.exists(wav_file)) {
            warning("Cannot plot missing WAV file: ", wav_file)
            return(NULL)
          }

          wave <- tuneR::readWave(wav_file)
          filtered_wave <- seewave::bwfilter(
            wave = wave,
            f = wave@samp.rate,
            n = 2,
            from = freq_range[1] * 1000,
            to = freq_range[2] * 1000,
            bandpass = TRUE
          )
          rms_env <- compute_rms_envelope(filtered_wave, wl = wl, ovlp = ovlp)
          stride <- round(wl * (1 - ovlp / 100))
          time_points <- seq(
            from = (wl / 2) / wave@samp.rate,
            by = stride / wave@samp.rate,
            length.out = length(rms_env)
          )

          if (length(rms_env)) {
            scale_value <- max(rms_env, na.rm = TRUE)
            if (is.finite(scale_value) && scale_value > 0) {
              rms_env <- rms_env / scale_value
            }
          }

          plot_dir <- file.path(plots_dir, sanitize_detection_label(session_label))
          dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

          grDevices::png(
            filename = file.path(
              plot_dir,
              paste0(tools::file_path_sans_ext(basename(wav_file)), ".png")
            ),
            width = 1600,
            height = 900,
            res = 200
          )
          tryCatch(
            plot_vocalization_detection(
              wave = wave,
              time_points = time_points,
              rms_env = rms_env,
              rms_threshold = NA_real_,
              detections = file_rows,
              wl = wl,
              ovlp = ovlp,
              title = ""
            ),
            finally = grDevices::dev.off()
          )
          file.path(plot_dir, paste0(tools::file_path_sans_ext(basename(wav_file)), ".png"))
        },
        error = function(e) {
          wav_file <- if (file_key == "file_path") {
            file_value
          } else {
            file.path(lys$base_path, file_value)
          }
          new_detection_error(
            file_path = wav_file,
            session_label = session_label,
            message = conditionMessage(e)
          )
        }
      )
    }

    plot_results <- parallel_apply(
      indices = files_to_plot,
      FUN = plot_one_file,
      cores = cores,
      use_preschedule = FALSE
    )

    worker_errors <- vapply(plot_results, is_detection_error, logical(1))
    worker_try_errors <- vapply(plot_results, inherits, logical(1), what = "try-error")
    if (any(worker_errors) || any(worker_try_errors)) {
      formatted_errors <- character(0)

      if (any(worker_errors)) {
        formatted_errors <- vapply(
          plot_results[worker_errors],
          function(err) sprintf("%s: %s", err$file_path, err$message),
          character(1)
        )
      }

      if (any(worker_try_errors)) {
        formatted_errors <- c(
          formatted_errors,
          vapply(plot_results[worker_try_errors], as.character, character(1))
        )
      }

      stop(
        sprintf(
          "Labeled vocalization plotting failed for %d file(s) in session %s:\n%s",
          length(formatted_errors),
          session_display,
          paste(utils::head(formatted_errors, 10L), collapse = "\n")
        ),
        call. = FALSE
      )
    }
  }

  if (verbose) {
    message("Saved labeled vocalization review plots to: ", plots_dir)
  }

  invisible(NULL)
}

#' Count detections of a given template type
#'
#' @description
#' Looks up a template_type in a table of counts and returns its count as an
#' integer, or 0L when the type is not present.
#'
#' @param counts A named integer table from \code{table()}
#' @param template_type Character. Template type name to look up
#'
#' @return An integer count.
#'
#' @noRd
#' @keywords internal
template_type_count <- function(counts, template_type) {
  if (template_type %in% names(counts)) {
    return(as.integer(counts[[template_type]]))
  }

  0L
}
