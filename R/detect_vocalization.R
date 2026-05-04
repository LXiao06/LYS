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

plot_vocalization_detection <- function(time_points,
                                        rms_env,
                                        rms_threshold,
                                        detections,
                                        title = "Vocalization Detection") {
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::par(mar = c(4, 4, 3, 2))
  graphics::plot(
    time_points,
    rms_env,
    type = "l",
    col = "firebrick",
    xlab = "Time (s)",
    ylab = "Normalized RMS",
    main = title
  )
  graphics::abline(h = rms_threshold, col = "steelblue", lty = 2)

  if (!is.null(detections) && nrow(detections) > 0) {
    for (i in seq_len(nrow(detections))) {
      graphics::abline(v = detections$start_time[i], col = "darkgreen", lty = 3)
      graphics::abline(v = detections$end_time[i], col = "darkgreen", lty = 3)
    }
  }
}

detect_vocalization <- function(x, ...) {
  UseMethod("detect_vocalization")
}

detect_vocalization.default <- function(x,
                                        wl = 1024,
                                        ovlp = 50,
                                        norm_method = c("quantile", "max"),
                                        rms_threshold = 0.1,
                                        min_duration = 0.5,
                                        gap_duration = 0.3,
                                        edge_window = 0.05,
                                        freq_range = c(3, 5),
                                        plot = FALSE,
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

  rms_env <- compute_rms_envelope(filtered_wave, wl = wl, ovlp = ovlp)
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

  stride <- round(wl * (1 - ovlp / 100))
  stride_time <- stride / wave@samp.rate
  time_points <- seq(
    from = (wl / 2) / wave@samp.rate,
    by = stride_time,
    length.out = length(rms_env)
  )

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

  if (plot || save_plot) {
    plot_title <- sprintf("Vocalization detection: %s", basename(x))
    plot_fn <- function() {
      plot_vocalization_detection(
        time_points = time_points,
        rms_env = rms_env,
        rms_threshold = rms_threshold,
        detections = detections,
        title = plot_title
      )
    }

    if (plot) {
      plot_fn()
    }

    if (save_plot) {
      dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
      png(
        filename = file.path(
          plot_dir,
          paste0(tools::file_path_sans_ext(basename(x)), ".png")
        ),
        width = 1600,
        height = 900,
        res = 200
      )
      plot_fn()
      grDevices::dev.off()
    }
  }

  detections
}

combine_vocalization_results <- function(results) {
  results <- results[!vapply(results, is.null, logical(1))]

  if (!length(results)) {
    return(NULL)
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}

run_session_parallel <- function(session_ids, worker, cores) {
  if (!length(session_ids)) {
    return(list())
  }

  if (is.null(cores) || is.na(cores) || cores < 1L) {
    detected <- suppressWarnings(parallel::detectCores())
    if (is.na(detected) || detected < 2L) {
      cores <- 1L
    } else {
      cores <- max(1L, detected - 1L)
    }
  } else {
    cores <- as.integer(cores)
  }

  if (length(session_ids) == 1L || cores <= 1L) {
    return(lapply(session_ids, worker))
  }

  worker_cores <- min(cores, length(session_ids))

  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    return(parallel::mclapply(session_ids, worker, mc.cores = worker_cores))
  }

  cl <- parallel::makeCluster(worker_cores, type = "PSOCK")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterExport(
    cl,
    varlist = c(
      "check_audio_dependencies",
      "combine_vocalization_results",
      "compute_rms_envelope",
      "worker",
      "detect_vocalization.default",
      "plot_vocalization_detection",
      "summarize_creation_items"
    ),
    envir = environment()
  )
  parallel::parLapply(cl, session_ids, worker)
}

detect_vocalization.lys <- function(x,
                                    session = NULL,
                                    indices = NULL,
                                    cores = NULL,
                                    save_plot = FALSE,
                                    plot_percent = 10,
                                    wl = 1024,
                                    ovlp = 50,
                                    norm_method = c("quantile", "max"),
                                    rms_threshold = 0.1,
                                    min_duration = 0.5,
                                    gap_duration = 0.3,
                                    edge_window = 0.05,
                                    freq_range = c(3, 5),
                                    verbose = TRUE,
                                    ...) {
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
  metadata$file_index <- seq_len(nrow(metadata))

  if (!is.null(indices)) {
    valid_indices <- indices[indices >= 1 & indices <= nrow(metadata)]
    metadata <- metadata[valid_indices, , drop = FALSE]
    if (!nrow(metadata)) {
      stop("No valid indices remain after filtering.", call. = FALSE)
    }
  }

  session_ids <- unique(metadata$session_id)

  if (verbose) {
    message(sprintf(
      "Starting vocalization detection for %d file(s) across %d session(s).",
      nrow(metadata),
      length(session_ids)
    ))
  }

  plot_lookup <- integer()
  if (save_plot) {
    unique_files <- unique(metadata$file_index)
    n_plot <- max(1L, ceiling(length(unique_files) * plot_percent / 100))
    plot_lookup <- if (n_plot >= length(unique_files)) {
      unique_files
    } else {
      sort(sample(unique_files, n_plot))
    }
  }

  worker <- function(current_session_id) {
    session_data <- metadata[metadata$session_id == current_session_id, , drop = FALSE]
    session_label <- unique(session_data$session_label)[1]

    session_results <- vector("list", nrow(session_data))

    for (i in seq_len(nrow(session_data))) {
      plot_dir <- NULL
      save_plot_file <- FALSE

      if (save_plot && session_data$file_index[i] %in% plot_lookup) {
        plot_dir <- file.path(x$base_path, "plots", "vocalization_detection", session_label)
        save_plot_file <- TRUE
      }

      detection <- detect_vocalization.default(
        x = session_data$file_path[i],
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
        plot_dir = plot_dir,
        ...
      )

      if (!is.null(detection) && nrow(detection) > 0) {
        detection$recording_day <- session_data$recording_day[i]
        detection$recording_date <- session_data$recording_date[i]
        detection$recording_time <- session_data$recording_time[i]
        detection$recording_start <- session_data$recording_start[i]
        detection$bird_id <- session_data$bird_id[i]
        detection$file_path <- session_data$file_path[i]
        detection$relative_path <- session_data$relative_path[i]
        detection$source_dir <- session_data$source_dir[i]
        detection$session_id <- session_data$session_id[i]
        detection$session_number <- session_data$session_number[i]
        detection$session_label <- session_label
        detection$file_index_within_session <- session_data$file_index_within_session[i]
      }

      session_results[[i]] <- detection
    }

    combine_vocalization_results(session_results)
  }

  session_results <- run_session_parallel(session_ids, worker, cores = cores)
  combined <- combine_vocalization_results(session_results)

  if (is.null(combined) || !nrow(combined)) {
    warning("No vocalizations detected.", call. = FALSE)
    return(invisible(x))
  }

  combined <- combined[order(combined$session_id, combined$filename, combined$start_time), , drop = FALSE]
  combined$global_index <- seq_len(nrow(combined))

  x$vocalizations <- combined
  x$misc$last_modified <- Sys.time()

  if (verbose) {
    per_session <- stats::aggregate(
      filename ~ session_label,
      data = combined,
      FUN = length
    )
    names(per_session)[names(per_session) == "filename"] <- "n_vocalizations"

    message(sprintf(
      "Detected %d vocalization(s) across %d session(s).",
      nrow(combined),
      length(unique(combined$session_id))
    ))
    message(
      "Session counts: ",
      summarize_creation_items(
        sprintf("%s [%d]", per_session$session_label, per_session$n_vocalizations)
      )
    )
  }

  invisible(x)
}
