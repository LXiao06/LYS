check_asap_dependency <- function() {
  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop(
      "This helper requires the ASAP package. Install or load ASAP before using it.",
      call. = FALSE
    )
  }
}

create_audio_clip <- function(x, ...) {
  check_asap_dependency()
  ASAP::create_audio_clip(x, ...)
}

visualize_song <- function(x, ...) {
  check_asap_dependency()
  ASAP::visualize_song(x, ...)
}
