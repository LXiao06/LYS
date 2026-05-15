check_asap_dependency <- function() {
  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop(
      "This helper requires the ASAP package. Install or load ASAP before using it.",
      call. = FALSE
    )
  }
}

#' Create an audio clip via ASAP
#' @param x Input passed to \code{ASAP::create_audio_clip()}.
#' @param ... Additional arguments forwarded to \code{ASAP::create_audio_clip()}.
#' @return Whatever \code{ASAP::create_audio_clip()} returns.
#' @export
create_audio_clip <- function(x, ...) {
  check_asap_dependency()
  ASAP::create_audio_clip(x, ...)
}

#' Visualize a song via ASAP
#' @param x Input passed to \code{ASAP::visualize_song()}.
#' @param ... Additional arguments forwarded to \code{ASAP::visualize_song()}.
#' @return Whatever \code{ASAP::visualize_song()} returns.
#' @export
visualize_song <- function(x, ...) {
  check_asap_dependency()
  ASAP::visualize_song(x, ...)
}
