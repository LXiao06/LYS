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
