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
