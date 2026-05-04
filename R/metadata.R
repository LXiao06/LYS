validate_base_path <- function(base_path) {
  if (!is.character(base_path) || length(base_path) != 1L || is.na(base_path)) {
    stop("base_path must be a single character string.", call. = FALSE)
  }

  normalized <- normalizePath(base_path, mustWork = TRUE)

  if (!dir.exists(normalized)) {
    stop("base_path must point to an existing directory.", call. = FALSE)
  }

  if (file.access(normalized, mode = 4) != 0) {
    stop("base_path is not readable.", call. = FALSE)
  }

  normalized
}

path_has_component <- function(path, components) {
  if (!length(components)) {
    return(rep(FALSE, length(path)))
  }

  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  parts <- strsplit(normalized, "/", fixed = TRUE)
  vapply(parts, function(x) any(x %in% components), logical(1))
}

list_wav_files <- function(base_path,
                           recursive = TRUE,
                           exclude_dirs = c("templates", "plots", "temp_plots")) {
  files <- list.files(
    path = base_path,
    pattern = "\\.[Ww][Aa][Vv]$",
    full.names = TRUE,
    recursive = recursive
  )

  files <- files[file.exists(files)]

  if (length(exclude_dirs)) {
    files <- files[!path_has_component(dirname(files), exclude_dirs)]
  }

  sort(unique(files))
}

infer_bird_id <- function(filename) {
  stem <- tools::file_path_sans_ext(basename(filename))
  match <- regmatches(stem, regexec("^([A-Za-z]+\\d+|[A-Za-z]?\\d+)", stem))

  if (length(match[[1]]) > 1L && nzchar(match[[1]][2])) {
    return(match[[1]][2])
  }

  stem
}

format_datetime_fields <- function(recording_start) {
  list(
    recording_day = as.Date(recording_start, tz = "UTC"),
    recording_date = format(recording_start, "%Y-%m-%d", tz = "UTC"),
    recording_time = format(recording_start, "%H:%M:%S", tz = "UTC")
  )
}

parse_sap_filename <- function(filename, tz = "UTC") {
  base <- basename(filename)
  pattern <- "^(.+?)_(\\d+(?:\\.\\d+)?)_(\\d{1,2})_(\\d{1,2})_(\\d{1,2})_(\\d{1,2})_(\\d{1,2})\\.[Ww][Aa][Vv]$"
  match <- regmatches(base, regexec(pattern, base))

  if (length(match[[1]]) > 0L) {
    bird_id <- match[[1]][2]
    sap_timestamp <- as.numeric(match[[1]][3])
    month <- as.integer(match[[1]][4])
    day <- as.integer(match[[1]][5])
    hour <- as.integer(match[[1]][6])
    minute <- as.integer(match[[1]][7])
    second <- as.integer(match[[1]][8])

    serial_start <- as.POSIXct(sap_timestamp * 86400, origin = "1899-12-30", tz = tz)
    serial_day <- format(serial_start, "%m-%d", tz = tz)
    suffix_day <- sprintf("%02d-%02d", month, day)
    serial_year <- format(serial_start, "%Y", tz = tz)

    suffix_start <- as.POSIXct(
      sprintf(
        "%s-%02d-%02d %02d:%02d:%02d",
        serial_year, month, day, hour, minute, second
      ),
      tz = tz
    )

    recording_start <- if (!is.na(serial_start) && identical(serial_day, suffix_day)) {
      suffix_start
    } else {
      serial_start
    }

    formatted <- format_datetime_fields(recording_start)

    return(list(
      bird_id = bird_id,
      sap_timestamp = sap_timestamp,
      recording_start = recording_start,
      recording_day = formatted$recording_day,
      recording_date = formatted$recording_date,
      recording_time = formatted$recording_time,
      parse_method = "sap2011"
    ))
  }

  info <- file.info(filename)
  recording_start <- info$mtime

  if (is.na(recording_start)) {
    recording_start <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = tz)
  } else {
    attr(recording_start, "tzone") <- tz
  }

  formatted <- format_datetime_fields(recording_start)

  list(
    bird_id = infer_bird_id(filename),
    sap_timestamp = NA_real_,
    recording_start = recording_start,
    recording_day = formatted$recording_day,
    recording_date = formatted$recording_date,
    recording_time = formatted$recording_time,
    parse_method = "fallback_mtime"
  )
}

create_lys_metadata <- function(base_path,
                                recursive = TRUE,
                                exclude_dirs = c("templates", "plots", "temp_plots"),
                                tz = "UTC") {
  base_path <- validate_base_path(base_path)
  wav_files <- list_wav_files(
    base_path = base_path,
    recursive = recursive,
    exclude_dirs = exclude_dirs
  )

  if (!length(wav_files)) {
    stop("No WAV files found under base_path.", call. = FALSE)
  }

  parsed <- lapply(wav_files, parse_sap_filename, tz = tz)

  metadata <- do.call(
    rbind,
    lapply(seq_along(wav_files), function(i) {
      info <- file.info(wav_files[i])
      data.frame(
        filename = basename(wav_files[i]),
        file_path = wav_files[i],
        relative_path = sub(
          paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", base_path), "/?"),
          "",
          wav_files[i]
        ),
        relative_dir = dirname(sub(
          paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", base_path), "/?"),
          "",
          wav_files[i]
        )),
        source_dir = basename(dirname(wav_files[i])),
        bird_id = parsed[[i]]$bird_id,
        sap_timestamp = parsed[[i]]$sap_timestamp,
        recording_start = parsed[[i]]$recording_start,
        recording_day = parsed[[i]]$recording_day,
        recording_date = parsed[[i]]$recording_date,
        recording_time = parsed[[i]]$recording_time,
        file_size_bytes = info$size,
        modified_time = info$mtime,
        parse_method = parsed[[i]]$parse_method,
        stringsAsFactors = FALSE
      )
    })
  )

  metadata$relative_dir[metadata$relative_dir == "."] <- ""
  metadata <- metadata[order(metadata$recording_start, metadata$filename), ]
  rownames(metadata) <- NULL
  metadata
}

assign_recording_sessions <- function(metadata, session_gap_hours = 1) {
  if (!is.data.frame(metadata)) {
    stop("metadata must be a data frame.", call. = FALSE)
  }

  required_cols <- c("filename", "recording_start", "recording_day")
  missing_cols <- setdiff(required_cols, names(metadata))

  if (length(missing_cols)) {
    stop(
      sprintf("metadata is missing required columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  if (!is.numeric(session_gap_hours) || length(session_gap_hours) != 1L ||
      is.na(session_gap_hours) || session_gap_hours <= 0) {
    stop("session_gap_hours must be a single positive number.", call. = FALSE)
  }

  metadata <- metadata[order(metadata$recording_start, metadata$filename), ]
  gap_seconds <- session_gap_hours * 3600
  delta_seconds <- c(NA_real_, diff(as.numeric(metadata$recording_start)))
  day_changed <- c(TRUE, as.character(metadata$recording_day[-1]) !=
                     as.character(metadata$recording_day[-nrow(metadata)]))
  new_session <- is.na(delta_seconds) | day_changed | delta_seconds >= gap_seconds

  metadata$seconds_from_previous <- delta_seconds
  metadata$session_id <- cumsum(new_session)

  day_ids <- unique(metadata$recording_day)
  metadata$day_index <- match(metadata$recording_day, day_ids)

  metadata$session_number <- ave(
    metadata$session_id,
    metadata$recording_day,
    FUN = function(x) match(x, unique(x))
  )

  metadata$session_label <- sprintf(
    "%s_session_%02d",
    metadata$recording_date,
    metadata$session_number
  )

  metadata$file_index_within_session <- ave(
    metadata$session_id,
    metadata$session_id,
    FUN = seq_along
  )

  session_starts <- tapply(metadata$recording_start, metadata$session_id, min)
  last_recording_starts <- tapply(metadata$recording_start, metadata$session_id, max)

  metadata$session_start <- as.POSIXct(
    session_starts[as.character(metadata$session_id)],
    origin = "1970-01-01",
    tz = "UTC"
  )
  metadata$last_recording_start <- as.POSIXct(
    last_recording_starts[as.character(metadata$session_id)],
    origin = "1970-01-01",
    tz = "UTC"
  )

  rownames(metadata) <- NULL
  metadata
}
