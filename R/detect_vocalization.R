check_audio_dependencies <- function() {
  missing <- c(
    if (!requireNamespace("tuneR", quietly = TRUE)) "tuneR",
    if (!requireNamespace("seewave", quietly = TRUE)) "seewave"
  )

  if (length(missing)) {
    stop(
      sprintf(
        "detect_vocalization() requires these packages to be installed: %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

compute_rms_envelope <- function(samples, wl, ovlp) {
  if (inherits(samples, "Wave")) {
    samples <- samples@left
  } else if (is.matrix(samples) || is.data.frame(samples)) {
    samples <- samples[, 1]
  }

  samples <- as.numeric(samples)
  stride <- round(wl * (1 - ovlp / 100))

  if (stride < 1L) {
    stop("wl and ovlp produce an invalid stride.", call. = FALSE)
  }

  if (length(samples) < wl) {
    return(numeric())
  }

  squared <- samples^2
  window <- rep(1 / wl, wl)
  rms_squared <- stats::filter(squared, filter = window, sides = 1)
  rms_squared <- rms_squared[wl:length(rms_squared)]

  if (!length(rms_squared)) {
    return(numeric())
  }

  indices <- seq(1L, length(rms_squared), by = stride)
  sqrt(rms_squared[indices])
}

plot_vocalization_detection <- function(wave,
                                        time_points,
                                        rms_env,
                                        rms_threshold,
                                        detections,
                                        wl,
                                        ovlp,
                                        title = "Vocalization Detection") {
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  duration <- length(wave@left) / wave@samp.rate
  xlim <- c(0, duration)
  ylim_rms <- c(0, max(c(rms_env, rms_threshold, 1), na.rm = TRUE) * 1.08)
  if (!all(is.finite(ylim_rms)) || diff(ylim_rms) <= 0) {
    ylim_rms <- c(0, 1)
  }

  spectro_data <- tryCatch(
    suppressWarnings(
      seewave::spectro(
        wave = wave,
        f = wave@samp.rate,
        wl = wl,
        ovlp = ovlp,
        plot = FALSE
      )
    ),
    error = function(e) NULL
  )

  spectro_ok <- !is.null(spectro_data) &&
    length(spectro_data$time) > 1L &&
    length(spectro_data$freq) > 1L &&
    all(dim(spectro_data$amp) == c(length(spectro_data$freq), length(spectro_data$time))) &&
    any(is.finite(spectro_data$amp))

  if (spectro_ok) {
    spectro_amp <- spectro_data$amp
    spectro_amp[!is.finite(spectro_amp)] <- -80
    spectro_amp <- pmax(spectro_amp, -80)
  }

  palette <- if (requireNamespace("viridisLite", quietly = TRUE)) {
    viridisLite::magma(256)
  } else {
    grDevices::colorRampPalette(c("black", "purple4", "orangered", "yellow"))(256)
  }

  graphics::par(
    bg = "black",
    fg = "white",
    col.axis = "white",
    col.lab = "white",
    col.main = "white",
    mfrow = c(2, 1),
    oma = c(2.8, 0, 1, 0)
  )

  graphics::par(mar = c(1.1, 4.8, 1.1, 1.5))
  graphics::plot(
    time_points,
    rms_env,
    type = "l",
    col = "red",
    lwd = 1,
    xlim = xlim,
    ylim = ylim_rms,
    xlab = "",
    ylab = "RMS",
    main = "",
    xaxt = "n"
  )
  graphics::abline(h = rms_threshold, col = "green", lty = 2, lwd = 1)

  if (!is.null(detections) && nrow(detections) > 0) {
    onset_y <- stats::approx(time_points, rms_env, xout = detections$start_time, rule = 2)$y
    offset_y <- stats::approx(time_points, rms_env, xout = detections$end_time, rule = 2)$y
    graphics::points(detections$start_time, onset_y, col = "blueviolet", pch = 19)
    graphics::points(detections$end_time, offset_y, col = "orange", pch = 19)
  }

  graphics::legend(
    "topright",
    legend = c("RMS", "Threshold", "Onset", "Offset"),
    col = c("red", "green", "blueviolet", "orange"),
    lty = c(1, 2, NA, NA),
    pch = c(NA, NA, 19, 19),
    bty = "n",
    text.col = "white",
    cex = 0.9
  )

  graphics::par(mar = c(3.6, 4.8, 0.7, 1.5))
  if (spectro_ok) {
    graphics::image(
      x = spectro_data$time,
      y = spectro_data$freq * 1000,
      z = t(spectro_amp),
      col = palette,
      zlim = c(-80, 0),
      xlim = xlim,
      ylim = range(spectro_data$freq * 1000, finite = TRUE),
      xlab = "",
      ylab = "FREQUENCY (Hz)",
      useRaster = TRUE
    )
  } else {
    graphics::plot(
      NA,
      xlim = xlim,
      ylim = c(0, wave@samp.rate / 2),
      xlab = "",
      ylab = "FREQUENCY (Hz)"
    )
  }

  if (!is.null(detections) && nrow(detections) > 0) {
    for (i in seq_len(nrow(detections))) {
      graphics::abline(v = detections$start_time[i], col = "white", lty = 2, lwd = 1.2)
      graphics::abline(v = detections$end_time[i], col = "white", lty = 2, lwd = 1.2)
    }

    label_col <- intersect(c("vocalization_label", "label"), names(detections))[1]
    if (length(label_col) && !is.na(label_col)) {
      usr <- graphics::par("usr")
      label_x <- (detections$start_time + detections$end_time) / 2
      label_y <- usr[4] - 0.04 * diff(usr[3:4])
      graphics::text(
        x = label_x,
        y = label_y,
        labels = detections[[label_col]],
        col = "white",
        cex = 0.8,
        pos = 3,
        xpd = NA
      )
    }
  }

  graphics::mtext("TIME", side = 1, outer = TRUE, line = 1.1, col = "white")
}

#' Detect vocalizations in audio
#' @param x A \\code{lys} object or a path to a WAV file.
#' @param ... Additional arguments passed to the method.
#' @return For \\code{lys} input, the updated object; for a WAV path, a
#'   data frame of detected vocalization bouts.
#' @export
detect_vocalization <- function(x, ...) {
  UseMethod("detect_vocalization")
}

#' @export
detect_vocalization.default <- function(x,
                                        wl = 1024,
                                        ovlp = 50,
                                        norm_method = c("quantile", "max"),
                                        rms_threshold = 0.1,
                                        min_duration = 0.5,
                                        gap_duration = 0.3,
                                        edge_window = 0.05,
                                        freq_range = c(3, 5),
                                        plot = TRUE,
                                        save_plot = FALSE,
                                        plot_dir = NULL,
                                        ...) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("x must be a single WAV file path.", call. = FALSE)
  }

  if (!file.exists(x)) {
    stop(sprintf("WAV file does not exist: %s", x), call. = FALSE)
  }

  if (!grepl("\\.[Ww][Aa][Vv]$", x)) {
    stop("x must point to a WAV file.", call. = FALSE)
  }

  if (!is.numeric(freq_range) || length(freq_range) != 2L || any(is.na(freq_range)) ||
      any(freq_range <= 0) || freq_range[1] >= freq_range[2]) {
    stop("freq_range must be two increasing positive values in kHz.", call. = FALSE)
  }

  if (!is.numeric(wl) || length(wl) != 1L || is.na(wl) || wl <= 1) {
    stop("wl must be a single integer-like value > 1.", call. = FALSE)
  }

  if (!is.numeric(ovlp) || length(ovlp) != 1L || is.na(ovlp) || ovlp < 0 || ovlp >= 100) {
    stop("ovlp must be a single number in [0, 100).", call. = FALSE)
  }

  check_audio_dependencies()
  norm_method <- match.arg(norm_method)
  wl <- as.integer(round(wl))

  if (save_plot && is.null(plot_dir)) {
    plot_dir <- file.path(dirname(x), "plots", "vocalization_detection")
  }

  wave <- tuneR::readWave(x)
  filtered_wave <- seewave::bwfilter(
    wave = wave,
    f = wave@samp.rate,
    n = 2,
    from = freq_range[1] * 1000,
    to = freq_range[2] * 1000,
    bandpass = TRUE
  )

  write_detection_plot <- function(detections = NULL) {
    has_detections <- !is.null(detections) && nrow(detections) > 0

    if ((!plot && !save_plot) || !has_detections) {
      return(invisible(NULL))
    }

    plot_title <- sprintf("Vocalization detection: %s", basename(x))
    plot_fn <- function() {
      plot_vocalization_detection(
        wave = wave,
        time_points = time_points,
        rms_env = rms_env,
        rms_threshold = rms_threshold,
        detections = detections,
        wl = wl,
        ovlp = ovlp,
        title = plot_title
      )
    }

    if (plot) {
      plot_fn()
    }

    if (save_plot) {
      dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
      grDevices::png(
        filename = file.path(
          plot_dir,
          paste0(tools::file_path_sans_ext(basename(x)), ".png")
        ),
        width = 1600,
        height = 900,
        res = 200
      )
      tryCatch(plot_fn(), finally = grDevices::dev.off())
    }

    invisible(NULL)
  }

  rms_env <- compute_rms_envelope(filtered_wave, wl = wl, ovlp = ovlp)
  stride <- round(wl * (1 - ovlp / 100))
  stride_time <- stride / wave@samp.rate
  time_points <- seq(
    from = (wl / 2) / wave@samp.rate,
    by = stride_time,
    length.out = length(rms_env)
  )

  if (!length(rms_env)) {
    return(NULL)
  }

  if (norm_method == "quantile") {
    scale_value <- as.numeric(stats::quantile(rms_env, 0.95, na.rm = TRUE, names = FALSE))
  } else {
    scale_value <- max(rms_env, na.rm = TRUE)
  }

  if (!is.finite(scale_value) || scale_value <= 0) {
    return(NULL)
  }

  rms_env <- rms_env / scale_value
  if (!all(is.finite(rms_env)) || max(rms_env, na.rm = TRUE) <= 0) {
    return(NULL)
  }

  above_threshold <- rms_env > rms_threshold

  edge_window_samples <- ceiling(edge_window / stride_time)
  if (length(above_threshold) > 0 && above_threshold[1]) {
    edge_region <- above_threshold[seq_len(min(edge_window_samples, length(above_threshold)))]
    first_below <- which(!edge_region)[1]

    if (!is.na(first_below)) {
      above_threshold[seq_len(first_below - 1L)] <- FALSE
    }
  }

  crossings <- diff(above_threshold)
  all_onsets <- which(crossings == 1)
  all_offsets <- which(crossings == -1) + 1L

  if (length(above_threshold) > 0 && above_threshold[length(above_threshold)]) {
    all_offsets <- c(all_offsets, length(rms_env))
  }

  gap_samples <- ceiling(gap_duration / stride_time)
  min_duration_samples <- ceiling(min_duration / stride_time)
  bout_onsets <- numeric()
  bout_offsets <- numeric()

  if (length(all_onsets) > 0 && length(all_offsets) > 0) {
    current_onset <- all_onsets[1]
    current_offset <- all_offsets[1]

    if (length(all_onsets) >= 2) {
      for (idx in 2:length(all_onsets)) {
        gap <- all_onsets[idx] - current_offset

        if (gap >= gap_samples) {
          if ((current_offset - current_onset) >= min_duration_samples) {
            bout_onsets <- c(bout_onsets, current_onset)
            bout_offsets <- c(bout_offsets, current_offset)
          }

          current_onset <- all_onsets[idx]
          next_offset_idx <- which(all_offsets > current_onset)[1]
          current_offset <- if (!is.na(next_offset_idx)) all_offsets[next_offset_idx] else length(rms_env)
        } else {
          next_offset_idx <- which(all_offsets > all_onsets[idx])[1]
          current_offset <- if (!is.na(next_offset_idx)) all_offsets[next_offset_idx] else length(rms_env)
        }
      }
    }

    if ((current_offset - current_onset) >= min_duration_samples) {
      bout_onsets <- c(bout_onsets, current_onset)
      bout_offsets <- c(bout_offsets, current_offset)
    }
  }

  if (!length(bout_onsets) || !length(bout_offsets)) {
    return(NULL)
  }

  detections <- data.frame(
    filename = basename(x),
    selec = seq_along(bout_onsets),
    start_time = time_points[bout_onsets],
    end_time = time_points[bout_offsets],
    duration = time_points[bout_offsets] - time_points[bout_onsets],
    stringsAsFactors = FALSE
  )

  write_detection_plot(detections)

  detections
}

combine_vocalization_results <- function(results) {
  results <- results[!vapply(results, is.null, logical(1))]

  if (!length(results)) {
    return(NULL)
  }

  out <- do.call(rbind, results)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

normalize_detection_cores <- function(cores) {
  if (is.null(cores)) {
    detected <- suppressWarnings(as.integer(parallel::detectCores()))
    if (is.na(detected) || detected < 2L) {
      return(1L)
    }
    return(max(1L, detected - 1L))
  }

  cores <- suppressWarnings(as.integer(cores))
  if (is.na(cores) || cores < 1L) {
    return(1L)
  }

  cores
}

sanitize_detection_label <- function(x) {
  gsub("[^A-Za-z0-9_-]", "_", x)
}

metadata_scalar <- function(x, default = NA_character_) {
  if (!length(x) || is.na(x[1])) {
    return(default)
  }

  as.character(x[1])
}

metadata_numeric_scalar <- function(x) {
  if (!length(x) || is.na(x[1])) {
    return(NA_real_)
  }

  as.numeric(x[1])
}

metadata_integer_scalar <- function(x) {
  if (!length(x) || is.na(x[1])) {
    return(NA_integer_)
  }

  as.integer(x[1])
}

new_detection_error <- function(file_path, session_label, message) {
  structure(
    list(
      file_path = file_path,
      filename = basename(file_path),
      session_label = session_label,
      message = message
    ),
    class = "lys_detection_error"
  )
}

is_detection_error <- function(x) {
  inherits(x, "lys_detection_error")
}

resolve_lys_output_dir <- function(base_path, output_dir = NULL) {
  if (!is.null(output_dir)) {
    if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) ||
        !nzchar(output_dir)) {
      stop("output_dir must be a single non-empty path.", call. = FALSE)
    }

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    return(normalizePath(output_dir, mustWork = TRUE))
  }

  parent_dir <- dirname(normalizePath(base_path, mustWork = TRUE))
  dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(parent_dir, mustWork = TRUE)
}

#' @export
detect_vocalization.lys <- function(x,
                                    session = NULL,
	                                   indices = NULL,
	                                   cores = NULL,
	                                   save_plot = TRUE,
	                                   save_csv = FALSE,
	                                   plot_percent = 100,
	                                  output_dir = NULL,
                                    wl = 1024,
                                    ovlp = 50,
                                    norm_method = c("quantile", "max"),
                                    rms_threshold = 0.2,
                                    min_duration = 1,
                                    gap_duration = 0.5,
                                    edge_window = 0.05,
                                    freq_range = c(3, 5),
                                    verbose = TRUE,
                                    ...) {
  if (verbose) message("\n=== Starting Bout Detection ===")

  if (!inherits(x, "lys")) {
    stop("x must be a LYS object.", call. = FALSE)
  }

  metadata <- x$metadata
  if (!nrow(metadata)) {
    stop("LYS object has no metadata rows to process.", call. = FALSE)
  }

  if (!is.null(session)) {
    keep <- metadata$session_id %in% session | metadata$session_label %in% session
    metadata <- metadata[keep, , drop = FALSE]
    if (!nrow(metadata)) {
      stop("No files found for the requested session selection.", call. = FALSE)
    }
  }

  metadata <- metadata[order(metadata$session_id, metadata$recording_start, metadata$filename), , drop = FALSE]

  session_ids <- unique(metadata$session_id)
  cores <- normalize_detection_cores(cores)
  norm_method <- match.arg(norm_method)
  output_dir <- resolve_lys_output_dir(x$base_path, output_dir = output_dir)

  if (verbose) {
    message(sprintf(
      "Starting vocalization detection for %d file(s) across %d session(s).",
      nrow(metadata),
      length(session_ids)
    ))
    message("Output root: ", output_dir)
  }

  results_dir <- file.path(output_dir, "vocalization_detection")
  if (save_csv) {
    dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  }

  plots_dir <- NULL
  if (save_plot) {
    plots_dir <- file.path(output_dir, "plots", "vocalization_detection")
    dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  }

  all_results <- list()
  extra_args <- list(...)

  for (current_session_id in session_ids) {
    session_data <- metadata[metadata$session_id == current_session_id, , drop = FALSE]
    session_label <- unique(session_data$session_label)[1]
    session_display <- sprintf("%s (id=%s)", session_label, current_session_id)

    if (!is.null(indices)) {
      valid_indices <- indices[indices >= 1 & indices <= nrow(session_data)]
      if (length(valid_indices) > 0) {
        session_data <- session_data[valid_indices, , drop = FALSE]
      } else {
        if (verbose) {
          message(sprintf("\nNo valid indices for session %s.", session_display))
        }
        next
      }
    }

    unique_files <- which(!duplicated(session_data$file_path))
    if (!length(unique_files)) {
      if (verbose) {
        message(sprintf("\nNo unique files for session %s.", session_display))
      }
      next
    }

    if (verbose) {
      message(sprintf(
        "\nProcessing %d file(s) for session %s using %d core(s).",
        length(unique_files), session_display, min(cores, length(unique_files))
      ))
    }

    files_to_plot <- numeric(0)
    if (save_plot) {
      if (!is.null(indices)) {
        files_to_plot <- unique_files
      } else {
        n_plots <- ceiling(length(unique_files) * plot_percent / 100)
        if (n_plots > 0L) {
          files_to_plot <- sort(sample(unique_files, min(n_plots, length(unique_files))))
        }
      }
    }

    process_file <- function(i) {
      tryCatch(
        {
          current_file <- session_data[i, , drop = FALSE]
          plot_dir <- NULL
          save_plot_file <- FALSE

          if (save_plot && i %in% files_to_plot) {
            plot_dir <- file.path(plots_dir, sanitize_detection_label(session_label))
            save_plot_file <- TRUE
          }

          detection <- do.call(
            detect_vocalization.default,
            c(
              list(
                x = current_file$file_path,
                wl = wl,
                ovlp = ovlp,
                norm_method = norm_method,
                rms_threshold = rms_threshold,
                min_duration = min_duration,
                gap_duration = gap_duration,
                edge_window = edge_window,
                freq_range = freq_range,
                plot = FALSE,
                save_plot = save_plot_file,
                plot_dir = plot_dir
              ),
              extra_args
            )
          )

          if (!is.null(detection) && nrow(detection) > 0) {
            detection$recording_day <- metadata_scalar(current_file$recording_day)
            detection$recording_date <- metadata_scalar(current_file$recording_date)
            detection$recording_time <- metadata_scalar(current_file$recording_time)
            detection$recording_start <- metadata_scalar(current_file$recording_start)
            detection$bird_id <- metadata_scalar(current_file$bird_id)
            detection$file_path <- metadata_scalar(current_file$file_path)
            detection$relative_path <- metadata_scalar(current_file$relative_path)
            detection$source_dir <- metadata_scalar(current_file$source_dir)
            detection$session_id <- metadata_integer_scalar(current_file$session_id)
            detection$session_number <- metadata_integer_scalar(current_file$session_number)
            detection$session_label <- session_label
            detection$file_index_within_session <- metadata_integer_scalar(current_file$file_index_within_session)
            return(detection)
          }

          NULL
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
          function(err) {
            sprintf("%s: %s", err$file_path, err$message)
          },
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
          "Vocalization detection failed for %d file(s) in session %s:\n%s",
          length(formatted_errors),
          session_display,
          paste(utils::head(formatted_errors, 10L), collapse = "\n")
        ),
        call. = FALSE
      )
    }

    valid_detections <- session_results[vapply(session_results, is.data.frame, logical(1))]
    if (length(valid_detections) > 0) {
      session_detections <- do.call(rbind, valid_detections)
      rownames(session_detections) <- NULL
      all_results[[as.character(current_session_id)]] <- session_detections
      if (save_csv) {
        utils::write.csv(
          session_detections,
          file = file.path(
            results_dir,
            paste0(sanitize_detection_label(session_label), "_vocalizations.csv")
          ),
          row.names = FALSE
        )
      }

      if (verbose) {
        message(sprintf(
          "\nProcessed session %s. Total vocalizations: %d",
          session_display, nrow(session_detections)
        ))
      }
    } else {
      if (verbose) {
        message(sprintf("\nNo vocalizations found in session %s.", session_display))
      }
    }
  }

  combined <- combine_vocalization_results(all_results)

  if (is.null(combined) || !nrow(combined)) {
    if (verbose && save_plot && !is.null(plots_dir)) {
      message("Review plots via: ", plots_dir)
    }
    warning("No vocalizations detected.", call. = FALSE)
    return(invisible(x))
  }

  combined <- combined[order(combined$session_id, combined$filename, combined$start_time), , drop = FALSE]
  combined$global_index <- seq_len(nrow(combined))
  combined <- as.data.frame(combined, stringsAsFactors = FALSE)
  if (save_csv) {
    utils::write.csv(
      combined,
      file = file.path(results_dir, "all_sessions_vocalizations.csv"),
      row.names = FALSE
    )
  }

  x$vocalizations <- combined
  x$misc$last_modified <- Sys.time()

  if (verbose) {
    message(sprintf(
      "\nTotal vocalizations detected across all sessions: %d",
      nrow(combined)
    ))
    message("Access results via: lys$vocalizations")
    if (save_csv) {
      message("Saved vocalization tables to: ", results_dir)
    }
    if (save_plot && !is.null(plots_dir)) {
      message("Review plots via: ", plots_dir)
    }
  }

  invisible(x)
}
