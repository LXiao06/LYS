# Metadata
# Update date : Jun. 10, 2026

#' Validate a base path argument
#'
#' @description
#' Checks that base_path is a single, existing, readable directory and
#' returns its normalised absolute path.
#'
#' @param base_path Character. Path to validate
#'
#' @return A normalised absolute path string.
#'
#' @noRd
#' @keywords internal
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


#' Test whether a path contains any of the specified components
#'
#' @description
#' Returns TRUE for each path whose directory tree contains at least one of
#' the named components.
#'
#' @param path Character vector of file paths
#' @param components Character vector of path component names to match
#'
#' @return Logical vector the same length as path.
#'
#' @noRd
#' @keywords internal
path_has_component <- function(path, components) {
  if (!length(components)) {
    return(rep(FALSE, length(path)))
  }

  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  parts <- strsplit(normalized, "/", fixed = TRUE)
  vapply(parts, function(x) any(x %in% components), logical(1))
}


#' List WAV files under a directory
#'
#' @description
#' Recursively (or not) lists all WAV files under base_path, optionally
#' excluding files nested inside named subdirectories.
#'
#' @param base_path Character. Root directory to search
#' @param recursive Logical. Search subdirectories. Default \code{TRUE}
#' @param exclude_dirs Character vector of subdirectory names to skip
#'
#' @return Sorted character vector of WAV file paths.
#'
#' @noRd
#' @keywords internal
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


#' Pool and standardize SAP2011 WAV files
#'
#' @description
#' Copies WAV files from a directory tree into one directory. Files already in
#' the SAP output format are copied unchanged. SAP recorder temporary files
#' named \code{Bird_Month_Day_Year_milliseconds_since_midnight.wav} are renamed
#' to the SAP output format used by LYS.
#'
#' @param input_dir Character. Root directory containing WAV files.
#' @param output_dir Character. Directory to receive the pooled WAV files.
#' @param subdirs Optional character vector of paths, relative to
#'   \code{input_dir}, to include. \code{NULL} includes all subdirectories.
#' @param tz Character. Time zone used to construct timestamps. Default
#'   \code{"UTC"}.
#'
#' @return A data frame mapping source files to their copied filenames.
#'
#' @examples
#' \dontrun{
#' preprocess_sap_wavs("raw_recordings", "pooled_wavs", subdirs = c("661", "662"))
#' }
#'
#' @export
preprocess_sap_wavs <- function(input_dir, output_dir, subdirs = NULL, tz = "UTC") {
  input_dir <- validate_base_path(input_dir)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)

  if (!is.null(subdirs) && (!is.character(subdirs) || anyNA(subdirs) || any(!nzchar(subdirs)))) {
    stop("subdirs must be NULL or a non-empty character vector.", call. = FALSE)
  }

  wav_files <- list_wav_files(input_dir)
  wav_files <- normalizePath(wav_files, winslash = "/", mustWork = TRUE)
  output_prefix <- paste0(output_dir, "/")
  wav_files <- wav_files[!startsWith(wav_files, output_prefix)]

  relative_paths <- substring(wav_files, nchar(input_dir) + 2L)
  if (!is.null(subdirs)) {
    subdirs <- sub("/$", "", gsub("\\\\", "/", subdirs))
    keep <- vapply(relative_paths, function(path) {
      any(vapply(subdirs, function(dir) path == dir || startsWith(path, paste0(dir, "/")), logical(1)))
    }, logical(1))
    wav_files <- wav_files[keep]
  }

  if (!length(wav_files)) {
    stop("No WAV files found for the requested directories.", call. = FALSE)
  }

  output_names <- vapply(wav_files, standardize_sap_wav_name, character(1), tz = tz)
  if (anyDuplicated(output_names)) {
    stop("Multiple source files would produce the same output filename.", call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  destinations <- file.path(output_dir, output_names)
  if (any(file.exists(destinations))) {
    stop("Output directory already contains one or more target filenames.", call. = FALSE)
  }

  copied <- file.copy(wav_files, destinations, copy.date = TRUE)
  if (!all(copied)) {
    stop("Could not copy every WAV file; source files were left unchanged.", call. = FALSE)
  }

  data.frame(
    source = wav_files,
    destination = destinations,
    filename = output_names,
    converted = basename(wav_files) != output_names,
    stringsAsFactors = FALSE
  )
}


#' Standardize one SAP2011 WAV filename
#'
#' @param filename Character. A WAV filename or path.
#' @param tz Character. Time zone used to construct timestamps.
#'
#' @return A SAP output-format filename.
#'
#' @noRd
standardize_sap_wav_name <- function(filename, tz = "UTC") {
  base <- basename(filename)
  permanent <- "^.+?_\\d+(?:\\.\\d+)?_\\d{1,2}_\\d{1,2}_\\d{1,2}_\\d{1,2}_\\d{1,2}\\.[Ww][Aa][Vv]$"
  if (grepl(permanent, base)) {
    return(base)
  }

  temporary <- "^(.+?)_([A-Za-z]+)_(\\d{1,2})_(\\d{4})_(\\d+)\\.[Ww][Aa][Vv]$"
  match <- regmatches(base, regexec(temporary, base))[[1]]
  if (!length(match)) {
    stop("Unsupported WAV filename: ", base, call. = FALSE)
  }

  month <- match(match[3], month.name)
  if (is.na(month)) {
    stop("Unsupported month in filename: ", base, call. = FALSE)
  }

  milliseconds <- as.numeric(match[6])
  if (is.na(milliseconds) || milliseconds < 0 || milliseconds >= 86400000) {
    stop("Invalid milliseconds-since-midnight value: ", base, call. = FALSE)
  }

  recording_start <- as.POSIXct(
    sprintf("%s-%02d-%02d", match[5], month, as.integer(match[4])),
    tz = tz
  ) + milliseconds / 1000
  serial <- (as.numeric(recording_start) - as.numeric(as.POSIXct("1899-12-30", tz = tz))) / 86400

  sprintf(
    "%s_%.8f_%d_%d_%s.wav",
    match[2], serial, month, as.integer(match[4]),
    format(recording_start, "%H_%M_%S", tz = tz)
  )
}


#' Infer a bird ID from a filename
#'
#' @description
#' Extracts a leading alphanumeric token from the filename stem as the bird
#' identifier, falling back to the full stem when no token is found.
#'
#' @param filename Character. File path or basename
#'
#' @return A single character string.
#'
#' @noRd
#' @keywords internal
infer_bird_id <- function(filename) {
  stem <- tools::file_path_sans_ext(basename(filename))
  match <- regmatches(stem, regexec("^([A-Za-z]+\\d+|[A-Za-z]?\\d+)", stem))

  if (length(match[[1]]) > 1L && nzchar(match[[1]][2])) {
    return(match[[1]][2])
  }

  stem
}


#' Format a POSIXct timestamp into standard date/time fields
#'
#' @description
#' Returns a named list containing the recording day, date string, and time
#' string for a given POSIXct timestamp.
#'
#' @param recording_start A single POSIXct value
#'
#' @return A named list with elements recording_day, recording_date, and
#'   recording_time.
#'
#' @noRd
#' @keywords internal
format_datetime_fields <- function(recording_start) {
  list(
    recording_day = as.Date(recording_start, tz = "UTC"),
    recording_date = format(recording_start, "%Y-%m-%d", tz = "UTC"),
    recording_time = format(recording_start, "%H:%M:%S", tz = "UTC")
  )
}


#' Parse a SAP-format filename into metadata fields
#'
#' @description
#' Attempts to decode the SAP2011 filename convention
#' (\code{BirdID_timestamp_MM_DD_HH_MM_SS.wav}). Falls back to the file
#' modification time when the pattern does not match.
#'
#' @param filename Character. Path or filename to parse
#' @param tz Character. Timezone string. Default \code{"UTC"}
#'
#' @return A named list of metadata fields.
#'
#' @noRd
#' @keywords internal
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


#' Create a LYS metadata table from a directory of WAV files
#'
#' @description
#' Scans base_path for WAV files, parses each filename for timestamp metadata,
#' and returns a tidy data frame with one row per file.
#'
#' @param base_path Character. Root directory containing WAV files
#' @param recursive Logical. Search subdirectories. Default \code{TRUE}
#' @param exclude_dirs Character vector of subdirectory names to skip
#' @param tz Character. Timezone string. Default \code{"UTC"}
#'
#' @return A data frame of file metadata.
#'
#' @noRd
#' @keywords internal
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


#' Assign recording sessions to a LYS metadata table
#'
#' @description
#' Groups files into sessions based on inter-file gaps and day boundaries,
#' then annotates each row with session identifiers and timing fields.
#'
#' @param metadata Data frame as returned by \code{create_lys_metadata()}
#' @param session_gap_hours Numeric. Minimum gap in hours between sessions.
#'   Default \code{1}
#'
#' @return The metadata data frame with session columns added.
#'
#' @noRd
#' @keywords internal
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

  metadata$session_number <- stats::ave(
    metadata$session_id,
    metadata$recording_day,
    FUN = function(x) match(x, unique(x))
  )

  metadata$session_label <- sprintf(
    "%s_session_%02d",
    metadata$recording_date,
    metadata$session_number
  )

  metadata$file_index_within_session <- stats::ave(
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
