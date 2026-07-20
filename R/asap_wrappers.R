# ASAP Wrappers
# Update date : Jun. 10, 2026

#' Create an audio clip via ASAP
#'
#' @description
#' Thin wrapper around \code{ASAP::create_audio_clip()} that ensures the ASAP
#' package is available before forwarding all arguments.
#'
#' @param x Input passed to \code{ASAP::create_audio_clip()}
#' @param ... Additional arguments forwarded to \code{ASAP::create_audio_clip()}
#'
#' @return Whatever \code{ASAP::create_audio_clip()} returns.
#'
#' @export
create_audio_clip <- function(x, ...) {
  check_asap_dependency()
  ASAP::create_audio_clip(x, ...)
}


#' Visualize a song via ASAP
#'
#' @description
#' Thin wrapper around \code{ASAP::visualize_song()} that ensures the ASAP
#' package is available before forwarding all arguments.
#'
#' @param x Input passed to \code{ASAP::visualize_song()}
#' @param ... Additional arguments forwarded to \code{ASAP::visualize_song()}
#'
#' @return Whatever \code{ASAP::visualize_song()} returns.
#'
#' @export
visualize_song <- function(x, ...) {
  check_asap_dependency()
  ASAP::visualize_song(x, ...)
}


#' Check that the ASAP package is installed
#'
#' @description
#' Stops with an informative message if the ASAP package cannot be loaded.
#'
#' @return NULL (called for its side effect).
#'
#' @noRd
#' @keywords internal
check_asap_dependency <- function() {
  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop(
      "This helper requires the ASAP package. Install or load ASAP before using it.",
      call. = FALSE
    )
  }
}


#' Convert a LYS object to an ASAP Sap object
#'
#' @description
#' Adapts LYS recording metadata for ASAP functions. In particular, LYS
#' relative directories become ASAP's \code{day_post_hatch} paths, allowing
#' ASAP to find WAV files below the LYS base directory.
#'
#' @param x A \code{lys} object.
#'
#' @return An ASAP \code{Sap} object.
#'
#' @examples
#' \dontrun{
#' sap <- as_sap(lys)
#' visualize_song(sap, n_samples = 4, random = TRUE)
#' }
#'
#' @export
as_sap <- function(x) {
  check_asap_dependency()

  if (!inherits(x, "lys")) {
    stop("x must be a LYS object.", call. = FALSE)
  }
  if (!is.data.frame(x$metadata) || !all(c("filename", "relative_dir") %in% names(x$metadata))) {
    stop("LYS metadata must contain filename and relative_dir columns.", call. = FALSE)
  }

  metadata <- x$metadata
  metadata$day_post_hatch <- metadata$relative_dir
  metadata$day_post_hatch[is.na(metadata$day_post_hatch) | metadata$day_post_hatch == "."] <- ""

  # ASAP has no exported low-level constructor; use its constructor so all
  # default template and segment fields are compatible with ASAP methods.
  utils::getFromNamespace("new_sap", "ASAP")(
    metadata = metadata,
    base_path = x$base_path,
    misc = list(lys_version = x$version)
  )
}
