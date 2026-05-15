check_template_dependencies <- function() {
  missing <- c(
    if (!requireNamespace("monitoR", quietly = TRUE)) "monitoR",
    if (!requireNamespace("tuneR", quietly = TRUE)) "tuneR"
  )

  if (length(missing)) {
    stop(
      sprintf(
        "Template detection requires these packages to be installed: %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

#' Create a vocalization template
#' @param x A \\code{lys} object or a path to a WAV file.
#' @param ... Additional arguments passed to the method.
#' @return For \\code{lys} input, the updated object; for a WAV path, a
#'   \\code{TemplateList} object.
#' @export
create_template <- function(x, ...) {
  UseMethod("create_template")
}

ensure_template_registry_slots <- function(x) {
  if (is.null(x$templates$template_list)) {
    x$templates$template_list <- list()
  }
  if (is.null(x$templates$template_matches)) {
    x$templates$template_matches <- list()
  }
  if (is.null(x$templates$matches)) {
    x$templates$matches <- x$templates$template_matches
  }

  required_cols <- data.frame(
    template_name = character(),
    template_type = character(),
    wav_path = character(),
    start_time = numeric(),
    end_time = numeric(),
    duration = numeric(),
    freq_min = numeric(),
    freq_max = numeric(),
    detection_threshold = numeric(),
    notes = character(),
    created_at = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )

  missing_cols <- setdiff(names(required_cols), names(x$templates$template_info))
  for (col in missing_cols) {
    x$templates$template_info[[col]] <- rep(required_cols[[col]][NA_integer_], nrow(x$templates$template_info))
  }
  x$templates$template_info <- x$templates$template_info[, names(required_cols), drop = FALSE]

  x
}

#' @export
create_template.default <- function(x,
                                    template_name,
                                    start_time = NULL,
                                    end_time = NULL,
                                    freq_min = 0,
                                    freq_max = 15,
                                    threshold = 0.6,
                                    write_template = FALSE,
                                    template_dir = NULL,
                                    ...) {
  check_template_dependencies()

  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("x must be a single WAV file path.", call. = FALSE)
  }

  x <- normalizePath(x, mustWork = TRUE)
  if (!grepl("\\.[Ww][Aa][Vv]$", x)) {
    stop("x must point to a WAV file.", call. = FALSE)
  }

  if (!is.character(template_name) || length(template_name) != 1L ||
      is.na(template_name) || !nzchar(template_name)) {
    stop("template_name must be a single non-empty string.", call. = FALSE)
  }

  if (xor(is.null(start_time), is.null(end_time))) {
    stop("start_time and end_time must be provided together.", call. = FALSE)
  }

  if (!is.null(start_time) && (!is.numeric(start_time) || !is.numeric(end_time) ||
      length(start_time) != 1L || length(end_time) != 1L ||
      is.na(start_time) || is.na(end_time) || start_time >= end_time)) {
    stop("start_time and end_time must be increasing numeric seconds.", call. = FALSE)
  }

  if (!is.numeric(freq_min) || !is.numeric(freq_max) ||
      length(freq_min) != 1L || length(freq_max) != 1L ||
      is.na(freq_min) || is.na(freq_max) || freq_min >= freq_max) {
    stop("freq_min and freq_max must be increasing numeric kHz values.", call. = FALSE)
  }

  if (!is.numeric(threshold) || length(threshold) != 1L ||
      is.na(threshold) || threshold < 0 || threshold > 1) {
    stop("threshold must be a single number between 0 and 1.", call. = FALSE)
  }

  template_args <- list(
    clip = x,
    frq.lim = c(freq_min, freq_max),
    name = template_name,
    score.cutoff = threshold
  )
  if (!is.null(start_time)) {
    template_args$t.lim <- c(start_time, end_time)
  }

  template <- NULL
  utils::capture.output(
    template <- do.call(monitoR::makeCorTemplate, c(template_args, list(...)))
  )

  if (write_template) {
    if (is.null(template_dir)) {
      template_dir <- dirname(x)
    }
    dir.create(template_dir, recursive = TRUE, showWarnings = FALSE)
    monitoR::writeCorTemplates(template, dir = template_dir)
  }

  template
}

#' @export
create_template.lys <- function(x,
                                template_name,
                                template_type,
                                wav_path = NULL,
                                index = NULL,
                                start_time = NULL,
                                end_time = NULL,
                                freq_min = 0,
                                freq_max = 15,
                                threshold = 0.6,
                                write_template = FALSE,
                                output_dir = NULL,
                                notes = NA_character_,
                                verbose = TRUE,
                                ...) {
  if (!inherits(x, "lys")) {
    stop("x must be a LYS object.", call. = FALSE)
  }
  x <- ensure_template_registry_slots(x)

  if (!template_type %in% x$templates$allowed_types) {
    stop(
      sprintf(
        "template_type must be one of: %s",
        paste(x$templates$allowed_types, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (is.null(wav_path)) {
    if (is.null(index) || length(index) != 1L || is.na(index) ||
        index < 1L || index > nrow(x$metadata)) {
      stop("Provide wav_path or one valid metadata index.", call. = FALSE)
    }
    wav_path <- x$metadata$file_path[index]
  }

  wav_path <- normalizePath(wav_path, mustWork = TRUE)
  output_dir <- resolve_lys_output_dir(x$base_path, output_dir = output_dir)
  template_dir <- file.path(output_dir, "templates")

  template <- create_template.default(
    x = wav_path,
    template_name = template_name,
    start_time = start_time,
    end_time = end_time,
    freq_min = freq_min,
    freq_max = freq_max,
    threshold = threshold,
    write_template = write_template,
    template_dir = template_dir,
    ...
  )

  new_row <- data.frame(
    template_name = template_name,
    template_type = template_type,
    wav_path = wav_path,
    start_time = if (is.null(start_time)) NA_real_ else start_time,
    end_time = if (is.null(end_time)) NA_real_ else end_time,
    duration = if (is.null(start_time)) NA_real_ else end_time - start_time,
    freq_min = freq_min,
    freq_max = freq_max,
    detection_threshold = threshold,
    notes = if (length(notes)) notes else NA_character_,
    created_at = Sys.time(),
    stringsAsFactors = FALSE
  )

  template_row <- which(x$templates$template_info$template_name == template_name)
  if (length(template_row)) {
    x$templates$template_info[template_row[1], ] <- new_row
  } else {
    x$templates$template_info <- rbind(x$templates$template_info, new_row)
  }

  x$templates$template_list[[template_name]] <- template
  if (is.null(x$templates$template_matches[[template_name]])) {
    x$templates$template_matches[[template_name]] <- data.frame()
  }
  x$templates$matches <- x$templates$template_matches
  x$misc$last_modified <- Sys.time()

  if (verbose) {
    message(sprintf("Created template '%s' (%s).", template_name, template_type))
  }

  invisible(x)
}

#' Run template-based vocalization detection
#' @param x A \\code{lys} object or a path to a WAV file.
#' @param ... Additional arguments passed to the method.
#' @return For \\code{lys} input, the updated object; for a WAV path, a
#'   data frame of detections.
#' @export
detect_template <- function(x, ...) {
  UseMethod("detect_template")
}

filter_template_peaks <- function(peaks, proximity_window = NULL) {
  if (is.null(proximity_window)) {
    return(peaks)
  }

  for (template_name in names(peaks@detections)) {
    detections <- peaks@detections[[template_name]]
    if (is.null(detections) || !nrow(detections)) {
      next
    }

    detections <- detections[order(detections$time), , drop = FALSE]
    group_ids <- integer(nrow(detections))
    group_ids[1] <- 1L
    anchor_time <- detections$time[1]
    current_group <- 1L

    if (nrow(detections) > 1L) {
      for (i in 2:nrow(detections)) {
        if (detections$time[i] - anchor_time > proximity_window) {
          current_group <- current_group + 1L
          anchor_time <- detections$time[i]
        }
        group_ids[i] <- current_group
      }
    }

    keep <- unlist(
      tapply(
        seq_len(nrow(detections)),
        group_ids,
        function(idx) idx[which.max(detections$score[idx])]
      ),
      use.names = FALSE
    )
    peaks@detections[[template_name]] <- detections[sort(keep), , drop = FALSE]
  }

  peaks
}

#' @export
detect_template.default <- function(x,
                                    template,
                                    cor.method = "pearson",
                                    proximity_window = NULL,
                                    plot = TRUE,
                                    save_plot = FALSE,
                                    plot_dir = NULL,
                                    ...) {
  check_template_dependencies()

  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("x must be a single WAV file path.", call. = FALSE)
  }

  x <- normalizePath(x, mustWork = TRUE)
  if (is.null(template)) {
    stop("template must be provided.", call. = FALSE)
  }
  if (is.list(template) && !inherits(template, "TemplateList")) {
    template <- do.call(monitoR::combineCorTemplates, template)
  }

  if (save_plot && is.null(plot_dir)) {
    plot_dir <- file.path(dirname(x), "plots", "template_matches")
  }

  scores <- NULL
  utils::capture.output(
    scores <- suppressWarnings(
      suppressMessages(
        monitoR::corMatch(
          survey = x,
          templates = template,
          show.prog = FALSE,
          cor.method = cor.method,
          time.source = "fileinfo",
          quiet = TRUE
        )
      )
    )
  )

  peaks <- NULL
  utils::capture.output(
    peaks <- suppressWarnings(suppressMessages(monitoR::findPeaks(score.obj = scores)))
  )
  peaks <- filter_template_peaks(peaks, proximity_window = proximity_window)
  detections <- NULL
  utils::capture.output(
    detections <- suppressWarnings(monitoR::getDetections(peaks, id = basename(x)))
  )

  if (is.null(detections) || !nrow(detections)) {
    return(NULL)
  }

  names(detections)[names(detections) == "id"] <- "filename"
  detections$date.time <- NULL
  detections <- as.data.frame(detections, stringsAsFactors = FALSE)

  if (plot || save_plot) {
    plot_fn <- function() {
      plot_method <- methods::getMethod("plot", "detectionList", where = asNamespace("monitoR"))
      suppressMessages(plot_method(peaks, ask = FALSE))
    }

    if (plot) {
      tryCatch(
        plot_fn(),
        error = function(e) warning("Error plotting detections for ", basename(x), ": ", e$message)
      )
    }

    if (save_plot) {
      dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
      tryCatch(
        {
          grDevices::png(
            file.path(plot_dir, paste0(tools::file_path_sans_ext(basename(x)), ".png")),
            width = 1200,
            height = 800,
            res = 150
          )
          tryCatch(plot_fn(), finally = grDevices::dev.off())
        },
        error = function(e) {
          warning("Error saving detection plot for ", basename(x), ": ", e$message)
          if (grDevices::dev.cur() > 1) {
            grDevices::dev.off()
          }
        }
      )
    }
  }

  detections
}

combine_lys_templates <- function(template_list, template_names) {
  templates <- template_list[template_names]
  missing <- template_names[!vapply(templates, function(x) inherits(x, "TemplateList"), logical(1))]
  if (length(missing)) {
    stop(
      sprintf("Template object(s) not found: %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  if (length(templates) == 1L) {
    return(templates[[1]])
  }

  do.call(monitoR::combineCorTemplates, templates)
}

set_template_thresholds <- function(template, thresholds) {
  if (is.null(thresholds)) {
    return(template)
  }

  current <- monitoR::templateCutoff(template)
  if (length(thresholds) == 1L && is.null(names(thresholds))) {
    thresholds <- stats::setNames(rep(thresholds, length(current)), names(current))
  }

  if (is.null(names(thresholds)) || any(!names(thresholds) %in% names(current))) {
    stop("threshold must be a scalar or a named vector matching template names.", call. = FALSE)
  }

  current[names(thresholds)] <- thresholds
  monitoR::templateCutoff(template) <- current
  template
}

#' @export
detect_template.lys <- function(x,
                                template_name = NULL,
                                session = NULL,
                                indices = NULL,
                                threshold = NULL,
                                cores = NULL,
                                cor.method = "pearson",
                                proximity_window = NULL,
                                save_plot = TRUE,
                                plot_percent = 100,
                                output_dir = NULL,
                                verbose = TRUE,
                                ...) {
  if (verbose) message("\n=== Starting Template Detection ===")

  if (!inherits(x, "lys")) {
    stop("x must be a LYS object.", call. = FALSE)
  }
  x <- ensure_template_registry_slots(x)

  if (!length(x$templates$template_list)) {
    stop("No templates are stored in this LYS object.", call. = FALSE)
  }

  if (is.null(template_name)) {
    template_name <- names(x$templates$template_list)
  }
  template_name <- unique(template_name)

  template <- combine_lys_templates(x$templates$template_list, template_name)
  template <- set_template_thresholds(template, threshold)

  metadata <- x$metadata
  if (!is.null(session)) {
    keep <- metadata$session_id %in% session | metadata$session_label %in% session
    metadata <- metadata[keep, , drop = FALSE]
    if (!nrow(metadata)) {
      stop("No files found for the requested session selection.", call. = FALSE)
    }
  }
  metadata <- metadata[order(metadata$session_id, metadata$recording_start, metadata$filename), , drop = FALSE]

  cores <- normalize_detection_cores(cores)
  output_dir <- resolve_lys_output_dir(x$base_path, output_dir = output_dir)
  plots_dir <- NULL
  if (save_plot) {
    plots_dir <- file.path(output_dir, "plots", "template_matches")
    dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  }

  session_ids <- unique(metadata$session_id)
  if (verbose) {
    message(sprintf(
      "Detecting %d template(s) in %d file(s) across %d session(s).",
      length(template_name),
      nrow(metadata),
      length(session_ids)
    ))
    message("Templates: ", paste(template_name, collapse = ", "))
    message("Output root: ", output_dir)
  }

  all_results <- list()

  for (current_session_id in session_ids) {
    session_data <- metadata[metadata$session_id == current_session_id, , drop = FALSE]
    session_label <- unique(session_data$session_label)[1]
    session_display <- sprintf("%s (id=%s)", session_label, current_session_id)

    if (!is.null(indices)) {
      valid_indices <- indices[indices >= 1 & indices <= nrow(session_data)]
      if (length(valid_indices)) {
        session_data <- session_data[valid_indices, , drop = FALSE]
      } else {
        if (verbose) message(sprintf("\nNo valid indices for session %s.", session_display))
        next
      }
    }

    unique_files <- which(!duplicated(session_data$file_path))
    if (!length(unique_files)) {
      next
    }

    if (verbose) {
      message(sprintf(
        "\nProcessing %d file(s) for session %s using %d core(s).",
        length(unique_files),
        session_display,
        min(cores, length(unique_files))
      ))
    }

    files_to_plot <- numeric(0)
    if (save_plot) {
      n_plots <- ceiling(length(unique_files) * plot_percent / 100)
      if (!is.null(indices)) {
        files_to_plot <- unique_files
      } else if (n_plots > 0L) {
        files_to_plot <- sort(sample(unique_files, min(n_plots, length(unique_files))))
      }
    }

    process_file <- function(i) {
      tryCatch(
        {
          current_file <- session_data[i, , drop = FALSE]
          should_plot <- save_plot && i %in% files_to_plot
          plot_dir <- if (should_plot) {
            file.path(plots_dir, sanitize_detection_label(session_label))
          } else {
            NULL
          }

          result <- detect_template.default(
            x = current_file$file_path,
            template = template,
            cor.method = cor.method,
            proximity_window = proximity_window,
            plot = FALSE,
            save_plot = should_plot,
            plot_dir = plot_dir,
            ...
          )

          if (!is.null(result) && nrow(result)) {
            result$recording_day <- metadata_scalar(current_file$recording_day)
            result$recording_date <- metadata_scalar(current_file$recording_date)
            result$recording_time <- metadata_scalar(current_file$recording_time)
            result$recording_start <- metadata_scalar(current_file$recording_start)
            result$bird_id <- metadata_scalar(current_file$bird_id)
            result$file_path <- metadata_scalar(current_file$file_path)
            result$relative_path <- metadata_scalar(current_file$relative_path)
            result$source_dir <- metadata_scalar(current_file$source_dir)
            result$session_id <- metadata_integer_scalar(current_file$session_id)
            result$session_number <- metadata_integer_scalar(current_file$session_number)
            result$session_label <- session_label
            result$file_index_within_session <- metadata_integer_scalar(current_file$file_index_within_session)
          }

          result
        },
        error = function(e) {
          current_file <- session_data[i, , drop = FALSE]
          new_detection_error(
            file_path = metadata_scalar(current_file$file_path),
            session_label = session_label,
            message = conditionMessage(e)
          )
        }
      )
    }

    session_results <- parallel_apply(
      indices = unique_files,
      FUN = process_file,
      cores = cores,
      use_preschedule = FALSE
    )

    worker_errors <- vapply(session_results, is_detection_error, logical(1))
    worker_try_errors <- vapply(session_results, inherits, logical(1), what = "try-error")
    if (any(worker_errors) || any(worker_try_errors)) {
      formatted_errors <- character(0)

      if (any(worker_errors)) {
        formatted_errors <- vapply(
          session_results[worker_errors],
          function(err) sprintf("%s: %s", err$file_path, err$message),
          character(1)
        )
      }

      if (any(worker_try_errors)) {
        formatted_errors <- c(
          formatted_errors,
          vapply(session_results[worker_try_errors], as.character, character(1))
        )
      }

      stop(
        sprintf(
          "Template detection failed for %d file(s) in session %s:\n%s",
          length(formatted_errors),
          session_display,
          paste(utils::head(formatted_errors, 10L), collapse = "\n")
        ),
        call. = FALSE
      )
    }

    valid_detections <- session_results[vapply(session_results, is.data.frame, logical(1))]
    if (length(valid_detections)) {
      session_detections <- do.call(rbind, valid_detections)
      rownames(session_detections) <- NULL
      all_results[[as.character(current_session_id)]] <- session_detections

      if (verbose) {
        message(sprintf(
          "\nProcessed session %s. Total template detections: %d",
          session_display,
          nrow(session_detections)
        ))
      }
    } else if (verbose) {
      message(sprintf("\nNo template detections found in session %s.", session_display))
    }
  }

  combined <- combine_vocalization_results(all_results)
  if (is.null(combined) || !nrow(combined)) {
    warning("No template detections found.", call. = FALSE)
    return(invisible(x))
  }

  combined <- combined[order(combined$session_id, combined$filename, combined$template, combined$time), , drop = FALSE]
  combined$global_index <- seq_len(nrow(combined))
  combined <- as.data.frame(combined, stringsAsFactors = FALSE)

  for (nm in unique(combined$template)) {
    x$templates$template_matches[[nm]] <- combined[combined$template == nm, , drop = FALSE]
  }
  x$templates$template_matches[["all"]] <- combined
  x$templates$matches <- x$templates$template_matches
  x$misc$last_modified <- Sys.time()

  if (verbose) {
    message(sprintf("\nTotal template detections across all sessions: %d", nrow(combined)))
    message("Access combined detections via: lys$templates$template_matches[[\"all\"]]")
  }

  invisible(x)
}
