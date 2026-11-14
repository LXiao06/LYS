# Utilities
# Update date : Jun. 05, 2026

#' Look up metadata row indices for WAV files in a LYS object
#'
#' @description
#' Find indices in the LYS metadata table matching specified WAV files.
#'
#' @param lys A \code{lys} object
#' @param wav_files Character vector of filenames, relative paths, or full
#'   paths to match against \code{lys$metadata}
#'
#' @return An integer vector of matching row indices.
#'
#' @examples
#' \dontrun{
#' indices <- get_wav_indices(lys, "song.wav")
#' }
#'
#' @export
get_wav_indices <- function(lys, wav_files) {
  if (!inherits(lys, "lys")) {
    stop("Input 'lys' must be a LYS object.", call. = FALSE)
  }

  if (!is.character(wav_files) || length(wav_files) == 0L || anyNA(wav_files) ||
    any(!nzchar(wav_files))) {
    stop("Input 'wav_files' must be a non-empty character vector.", call. = FALSE)
  }

  metadata <- lys$metadata
  if (!is.data.frame(metadata) || !nrow(metadata)) {
    warning("LYS object has no metadata rows.")
    return(integer())
  }

  normalized_query <- normalizePath(wav_files, winslash = "/", mustWork = FALSE)
  query_basenames <- basename(wav_files)

  metadata_file_path <- normalizePath(metadata$file_path, winslash = "/", mustWork = FALSE)
  match_matrix <- vapply(
    seq_along(wav_files),
    function(i) {
      metadata$filename == wav_files[i] |
        metadata$filename == query_basenames[i] |
        metadata$relative_path == wav_files[i] |
        metadata$file_path == wav_files[i] |
        metadata_file_path == normalized_query[i]
    },
    logical(nrow(metadata))
  )

  if (is.null(dim(match_matrix))) {
    match_matrix <- matrix(match_matrix, ncol = 1L)
  }

  indices <- which(rowSums(match_matrix) > 0L)

  if (!length(indices)) {
    warning("None of the specified WAV files were found in the LYS metadata.")
    return(integer())
  }

  found <- vapply(
    seq_along(wav_files),
    function(i) any(match_matrix[, i]),
    logical(1)
  )
  if (any(!found)) {
    warning(
      "The following WAV files were not found in the LYS metadata:\n",
      paste(wav_files[!found], collapse = ", ")
    )
  }

  as.integer(indices)
}


#' Check if a package is installed
#'
#' @param pkg Character string of the package name
#'
#' @return Logical indicating if package is installed.
#'
#' @noRd
#' @keywords internal
check_pkg <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}


#' Ensure required packages are installed
#'
#' @param ... Package names as character strings
#'
#' @return Logical TRUE (invisibly) if all packages are installed.
#'
#' @noRd
#' @keywords internal
ensure_pkgs <- function(...) {
  pkgs <- c(...)
  missing_pkgs <- pkgs[!vapply(pkgs, check_pkg, logical(1))]

  if (length(missing_pkgs) > 0) {
    stop(
      sprintf(
        "Missing required package(s): %s. Please install them first.",
        paste(missing_pkgs, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Apply a function in parallel
#'
#' @param indices Integer vector of indices to iterate over
#' @param FUN Function to apply to each index
#' @param cores Integer. Number of parallel cores to use
#' @param use_preschedule Logical. Whether to use prescheduling in Darwin
#' @param cl Optional cluster object for non-Darwin systems
#'
#' @return A list of results from applying FUN.
#'
#' @noRd
#' @keywords internal
parallel_apply <- function(indices, FUN, cores, use_preschedule = FALSE, cl = NULL) {
  if (is.null(cores)) {
    ensure_pkgs("parallel")
    detected <- suppressWarnings(as.integer(parallel::detectCores()))
    if (is.na(detected) || detected < 1L) {
      cores <- 1L
    } else {
      cores <- max(1L, detected - 1L)
    }
  }

  if (cores > 1) {
    if (Sys.info()["sysname"] == "Darwin") {
      ensure_pkgs("pbmcapply")
      result <- pbmcapply::pbmclapply(
        indices,
        FUN,
        mc.cores = cores,
        mc.preschedule = use_preschedule
      )
    } else {
      ensure_pkgs("pbapply", "parallel")
      if (is.null(cl)) {
        cl <- parallel::makeCluster(cores, type = "PSOCK")
        parallel::clusterEvalQ(cl, loadNamespace("LYS"))
        on.exit(parallel::stopCluster(cl), add = TRUE)
      }
      result <- pbapply::pblapply(
        indices,
        FUN,
        cl = cl
      )
    }
  } else {
    ensure_pkgs("pbapply")
    result <- pbapply::pblapply(indices, FUN)
  }

  result
}


#' Remove specified WAV file(s) from a LYS object
#'
#' @description
#' Deletes one or more WAV files from a \code{lys} object, refreshing metadata,
#' session summaries, day summaries, and pruning any downstream vocalization or
#' template match detections. This avoids the need to recreate the entire
#' \code{lys} object from scratch when corrupted, truncated, or abnormally small
#' files cause downstream errors.
#'
#' @param lys A \code{lys} object
#' @param files Character vector of filenames, relative paths, full paths,
#'   or an integer vector of metadata row indices to remove
#' @param min_size_bytes Optional numeric. If provided, any files with
#'   file size in bytes below this threshold will also be removed
#' @param reassign_sessions Logical. If \code{FALSE} (default), preserves
#'   existing session IDs and labels, only updating intra-session counters
#'   and start/end times. If \code{TRUE}, recalculates session boundaries
#'   via \code{assign_recording_sessions()}
#' @param verbose Logical. Whether to print status and summary messages.
#'   Default \code{TRUE}
#'
#' @return The updated \code{lys} object.
#'
#' @examples
#' \dontrun{
#' # Remove by filename
#' lys <- remove_wav_files(lys, "bad_file.wav")
#'
#' # Remove by full path
#' lys <- remove_wav_files(lys, "/path/to/bad_file.wav")
#'
#' # Remove all files smaller than 10 KB
#' lys <- remove_wav_files(lys, min_size_bytes = 10000)
#' }
#'
#' @export
remove_wav_files <- function(lys,
                             files = NULL,
                             min_size_bytes = NULL,
                             reassign_sessions = FALSE,
                             verbose = TRUE) {
  if (!inherits(lys, "lys")) {
    stop("Input 'lys' must be a LYS object.", call. = FALSE)
  }

  if (is.null(files) && is.null(min_size_bytes)) {
    stop("Please specify either 'files' or 'min_size_bytes'.", call. = FALSE)
  }

  metadata <- lys$metadata
  if (!is.data.frame(metadata) || !nrow(metadata)) {
    stop("lys$metadata is empty.", call. = FALSE)
  }

  indices_to_remove <- integer(0)

  # 1. Match specified files
  if (!is.null(files)) {
    if (is.numeric(files)) {
      indices_to_remove <- c(indices_to_remove, as.integer(files))
    } else if (is.character(files)) {
      matched_idx <- get_wav_indices(lys, files)
      indices_to_remove <- c(indices_to_remove, matched_idx)
    } else {
      stop(
        "'files' must be character file paths/names or numeric metadata row indices.",
        call. = FALSE
      )
    }
  }

  # 2. Match abnormally small files by file size if requested
  if (!is.null(min_size_bytes)) {
    if (!is.numeric(min_size_bytes) || length(min_size_bytes) != 1L ||
        is.na(min_size_bytes) || min_size_bytes < 0) {
      stop("min_size_bytes must be a single non-negative number.", call. = FALSE)
    }
    if ("file_size_bytes" %in% names(metadata)) {
      size_idx <- which(metadata$file_size_bytes < min_size_bytes)
    } else {
      sizes <- file.info(metadata$file_path)$size
      size_idx <- which(sizes < min_size_bytes)
    }
    indices_to_remove <- c(indices_to_remove, size_idx)
  }

  indices_to_remove <- sort(unique(indices_to_remove))
  indices_to_remove <- indices_to_remove[
    indices_to_remove >= 1L & indices_to_remove <= nrow(metadata)
  ]

  if (!length(indices_to_remove)) {
    if (verbose) message("No matching files found to remove. Returning original lys object.")
    return(lys)
  }

  if (length(indices_to_remove) == nrow(metadata)) {
    stop("Cannot remove all files from a LYS object.", call. = FALSE)
  }

  removed_metadata <- metadata[indices_to_remove, , drop = FALSE]
  removed_filenames <- unique(removed_metadata$filename)
  removed_paths <- unique(removed_metadata$file_path)

  # Check if any removed files were used as templates in the registry
  if (!is.null(lys$templates$template_info) && nrow(lys$templates$template_info) > 0) {
    used_as_template <- lys$templates$template_info$wav_path %in% removed_paths |
      basename(lys$templates$template_info$wav_path) %in% removed_filenames
    if (any(used_as_template)) {
      warning(
        sprintf(
          "Notice: %d removed file(s) are registered as templates in lys$templates$template_info: %s",
          sum(used_as_template),
          paste(lys$templates$template_info$template_name[used_as_template], collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  # Update metadata
  new_meta <- metadata[-indices_to_remove, , drop = FALSE]

  if (reassign_sessions) {
    gap_hours <- if (!is.null(lys$settings$session_gap_hours)) {
      lys$settings$session_gap_hours
    } else {
      1
    }
    new_meta <- assign_recording_sessions(new_meta, session_gap_hours = gap_hours)
  } else {
    # Preserve existing session IDs and labels; refresh intra-session fields
    new_meta$file_index_within_session <- stats::ave(
      new_meta$session_id,
      new_meta$session_id,
      FUN = seq_along
    )
    session_starts <- tapply(new_meta$recording_start, new_meta$session_id, min)
    last_recording_starts <- tapply(new_meta$recording_start, new_meta$session_id, max)
    new_meta$session_start <- as.POSIXct(
      session_starts[as.character(new_meta$session_id)],
      origin = "1970-01-01",
      tz = "UTC"
    )
    new_meta$last_recording_start <- as.POSIXct(
      last_recording_starts[as.character(new_meta$session_id)],
      origin = "1970-01-01",
      tz = "UTC"
    )
    rownames(new_meta) <- NULL
  }

  lys$metadata <- new_meta
  lys$day_summary <- build_day_summary(new_meta)
  lys$session_summary <- build_session_summary(new_meta)

  # Helper to prune table rows referencing removed files
  prune_table <- function(df) {
    if (!is.data.frame(df) || !nrow(df)) return(df)
    drop_mask <- rep(FALSE, nrow(df))
    if ("filename" %in% names(df)) {
      drop_mask <- drop_mask | (df$filename %in% removed_filenames)
    }
    if ("file_path" %in% names(df)) {
      drop_mask <- drop_mask | (df$file_path %in% removed_paths)
    }
    out <- df[!drop_mask, , drop = FALSE]
    rownames(out) <- NULL
    out
  }

  # Prune vocalizations & downstream tables if present
  if (!is.null(lys$vocalizations) && is.data.frame(lys$vocalizations)) {
    lys$vocalizations <- prune_table(lys$vocalizations)
  }
  if (!is.null(lys$vocalization_session_map) && is.data.frame(lys$vocalization_session_map)) {
    lys$vocalization_session_map <- prune_table(lys$vocalization_session_map)
  }
  if (!is.null(lys$vocalization_annotations) && is.data.frame(lys$vocalization_annotations)) {
    lys$vocalization_annotations <- prune_table(lys$vocalization_annotations)
  }
  if (!is.null(lys$motifs) && is.data.frame(lys$motifs)) {
    lys$motifs <- prune_table(lys$motifs)
  }
  if (!is.null(lys$templates$template_matches) && is.list(lys$templates$template_matches)) {
    lys$templates$template_matches <- lapply(lys$templates$template_matches, prune_table)
    lys$templates$matches <- lys$templates$template_matches
  }

  # Audit trail in misc
  lys$misc$last_modified <- Sys.time()
  removal_record <- data.frame(
    filename = removed_metadata$filename,
    file_path = removed_metadata$file_path,
    session_id = removed_metadata$session_id,
    session_label = removed_metadata$session_label,
    removed_at = Sys.time(),
    stringsAsFactors = FALSE
  )
  if (is.null(lys$misc$removed_files)) {
    lys$misc$removed_files <- removal_record
  } else {
    lys$misc$removed_files <- rbind(lys$misc$removed_files, removal_record)
  }

  if (verbose) {
    message(sprintf(
      "Successfully removed %d file(s) from LYS object. %d files remaining across %d session(s).",
      length(indices_to_remove),
      nrow(lys$metadata),
      nrow(lys$session_summary)
    ))
  }

  lys
}

