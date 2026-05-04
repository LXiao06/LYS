collapse_unique <- function(x) {
  x <- unique(x[!is.na(x) & nzchar(x)])
  paste(x, collapse = ", ")
}

summarize_creation_items <- function(x, max_items = 8L) {
  if (!length(x)) {
    return("none")
  }

  x <- as.character(x)
  if (length(x) <= max_items) {
    return(paste(x, collapse = "; "))
  }

  shown <- paste(utils::head(x, max_items), collapse = "; ")
  sprintf("%s; ... +%d more", shown, length(x) - max_items)
}

print_lys_creation_summary <- function(x) {
  day_items <- sprintf(
    "%s (%d files, %d sessions)",
    x$day_summary$recording_date,
    x$day_summary$n_files,
    x$day_summary$n_sessions
  )
  session_items <- sprintf(
    "%s [%d files]",
    x$session_summary$session_label,
    x$session_summary$n_files
  )

  message(
    sprintf(
      "Created LYS object: %d files across %d day(s) and %d session(s).",
      nrow(x$metadata),
      nrow(x$day_summary),
      nrow(x$session_summary)
    )
  )
  message("Days: ", summarize_creation_items(day_items))
  message("Sessions: ", summarize_creation_items(session_items))
}

build_day_summary <- function(metadata) {
  days <- unique(metadata$recording_day)

  summary_rows <- lapply(days, function(day_value) {
    idx <- metadata$recording_day == day_value
    day_data <- metadata[idx, , drop = FALSE]

    data.frame(
      recording_day = as.Date(day_value),
      recording_date = unique(day_data$recording_date)[1],
      day_index = unique(day_data$day_index)[1],
      n_files = nrow(day_data),
      n_sessions = length(unique(day_data$session_id)),
      first_recording = min(day_data$recording_start),
      last_recording = max(day_data$recording_start),
      bird_ids = collapse_unique(day_data$bird_id),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, summary_rows)
  rownames(out) <- NULL
  out
}

build_session_summary <- function(metadata) {
  sessions <- unique(metadata$session_id)

  summary_rows <- lapply(sessions, function(id) {
    idx <- metadata$session_id == id
    session_data <- metadata[idx, , drop = FALSE]
    session_start <- min(session_data$recording_start)
    last_recording_start <- max(session_data$recording_start)

    data.frame(
      session_id = id,
      recording_day = unique(session_data$recording_day)[1],
      recording_date = unique(session_data$recording_date)[1],
      day_index = unique(session_data$day_index)[1],
      session_number = unique(session_data$session_number)[1],
      session_label = unique(session_data$session_label)[1],
      n_files = nrow(session_data),
      session_start = session_start,
      last_recording_start = last_recording_start,
      session_span_minutes = as.numeric(
        difftime(last_recording_start, session_start, units = "mins")
      ),
      bird_ids = collapse_unique(session_data$bird_id),
      relative_dirs = collapse_unique(session_data$relative_dir),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, summary_rows)
  rownames(out) <- NULL
  out
}

new_template_registry <- function(template_types = c("song_bout", "innate_call", "pupil_beg_call")) {
  structure(
    list(
      allowed_types = template_types,
      template_info = data.frame(
        template_name = character(),
        template_type = character(),
        wav_path = character(),
        start_time = numeric(),
        end_time = numeric(),
        detection_threshold = numeric(),
        notes = character(),
        created_at = as.POSIXct(character()),
        stringsAsFactors = FALSE
      ),
      matches = list()
    ),
    class = "lys_template_registry"
  )
}

new_lys <- function(metadata,
                    base_path,
                    session_gap_hours,
                    templates = new_template_registry(),
                    vocalizations = data.frame(),
                    misc = list(),
                    version = "0.0.0.9000") {
  structure(
    list(
      metadata = metadata,
      day_summary = build_day_summary(metadata),
      session_summary = build_session_summary(metadata),
      base_path = base_path,
      templates = templates,
      vocalizations = vocalizations,
      misc = misc,
      version = version,
      settings = list(session_gap_hours = session_gap_hours)
    ),
    class = "lys"
  )
}

create_lys_object <- function(base_path,
                              recursive = TRUE,
                              exclude_dirs = c("templates", "plots", "temp_plots"),
                              session_gap_hours = 1,
                              template_types = c("song_bout", "innate_call", "pupil_beg_call"),
                              tz = "UTC") {
  metadata <- create_lys_metadata(
    base_path = base_path,
    recursive = recursive,
    exclude_dirs = exclude_dirs,
    tz = tz
  )

  metadata <- assign_recording_sessions(
    metadata = metadata,
    session_gap_hours = session_gap_hours
  )

  lys <- new_lys(
    metadata = metadata,
    base_path = normalizePath(base_path, mustWork = TRUE),
    session_gap_hours = session_gap_hours,
    templates = new_template_registry(template_types = template_types),
    vocalizations = data.frame(),
    misc = list(
      created_at = Sys.time(),
      creation_args = list(
        recursive = recursive,
        exclude_dirs = exclude_dirs,
        session_gap_hours = session_gap_hours,
        template_types = template_types,
        tz = tz
      )
    )
  )

  print_lys_creation_summary(lys)
  lys
}

register_template <- function(lys,
                              template_name,
                              template_type,
                              wav_path,
                              start_time = NA_real_,
                              end_time = NA_real_,
                              detection_threshold = NA_real_,
                              notes = NA_character_) {
  if (!inherits(lys, "lys")) {
    stop("lys must be a LYS object.", call. = FALSE)
  }

  if (!template_type %in% lys$templates$allowed_types) {
    stop(
      sprintf(
        "template_type must be one of: %s",
        paste(lys$templates$allowed_types, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.character(template_name) || length(template_name) != 1L || !nzchar(template_name)) {
    stop("template_name must be a single non-empty string.", call. = FALSE)
  }

  wav_path <- normalizePath(wav_path, mustWork = TRUE)

  if (!grepl("\\.[Ww][Aa][Vv]$", wav_path)) {
    stop("wav_path must point to a WAV file.", call. = FALSE)
  }

  if (!is.numeric(start_time) || length(start_time) != 1L) {
    stop("start_time must be a single numeric value.", call. = FALSE)
  }

  if (!is.numeric(end_time) || length(end_time) != 1L) {
    stop("end_time must be a single numeric value.", call. = FALSE)
  }

  if (!is.na(start_time) && !is.na(end_time) && end_time < start_time) {
    stop("end_time must be greater than or equal to start_time.", call. = FALSE)
  }

  new_row <- data.frame(
    template_name = template_name,
    template_type = template_type,
    wav_path = wav_path,
    start_time = start_time,
    end_time = end_time,
    detection_threshold = detection_threshold,
    notes = if (length(notes)) notes else NA_character_,
    created_at = Sys.time(),
    stringsAsFactors = FALSE
  )

  lys$templates$template_info <- rbind(lys$templates$template_info, new_row)
  lys
}

print.lys <- function(x, ...) {
  cat("LYS Object\n")
  cat("==========\n")
  cat("Base path:", x$base_path, "\n")
  cat("Recording days:", nrow(x$day_summary), "\n")
  cat("Sessions:", nrow(x$session_summary), "\n")
  cat("Files:", nrow(x$metadata), "\n")
  cat("Detected vocalizations:", nrow(x$vocalizations), "\n")
  cat(
    "Template types:",
    paste(x$templates$allowed_types, collapse = ", "),
    "\n"
  )
  invisible(x)
}

summary.lys <- function(object, ...) {
  print(object)
  cat("\nDay summary\n")
  print(object$day_summary, row.names = FALSE)
  cat("\nSession summary\n")
  print(object$session_summary, row.names = FALSE)
  if (nrow(object$vocalizations) > 0) {
    cat("\nVocalization summary\n")
    vocal_summary <- stats::aggregate(
      filename ~ session_label + recording_date,
      data = object$vocalizations,
      FUN = length
    )
    names(vocal_summary)[names(vocal_summary) == "filename"] <- "n_vocalizations"
    print(vocal_summary, row.names = FALSE)
  }
  invisible(object)
}
