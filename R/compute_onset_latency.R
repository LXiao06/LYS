# Compute Onset Latency
# Update date : Jun. 10, 2026

#' Compute Onset Latency Distribution Between Vocalization Pairs
#'
#' @description
#' For every occurrence of \code{preceding_label}, finds the nearest subsequent
#' \code{following_label} event whose onset falls within \code{window_sec} of
#' the preceding event's offset.  Returns a tidy data frame of latencies that
#' can be used directly for plotting or statistical analysis.
#'
#' @param data A data frame of vocalization events — typically
#'   \code{lys$vocalization_session_map} after
#'   \code{map_vocalization_sessions()}.
#' @param preceding_label Character. Label of the first (trigger) event.
#' @param following_label Character. Label of the event whose onset latency is
#'   measured.
#' @param label_col Character. Column in \code{data} holding the event label.
#'   Default \code{"vocalization_label"}.
#' @param start_col Character. Column holding the event start time (seconds).
#'   Default \code{"session_relative_start"}.
#' @param end_col Character. Column holding the event end time (seconds).
#'   Default \code{"session_relative_end"}.
#' @param session_col Character. Column used to group events into independent
#'   sessions — pairs are only formed within the same session.
#'   Default \code{"session_label"}.
#' @param window_sec Numeric. Maximum time (seconds) from the offset of the
#'   preceding event to the onset of the following event.
#'   Default \code{60}.
#' @param require_adjacent Logical. If \code{TRUE} (default), an intervening
#'   event of type \code{preceding_label} between the pair invalidates the
#'   match (i.e. the SongBout must be the \emph{next} relevant event after the
#'   BeggingCall).  Set to \code{FALSE} to allow any matching following event
#'   within the window regardless of intervening events.
#'
#' @return A data frame with one row per matched pair:
#' \describe{
#'   \item{\code{session}}{Session identifier.}
#'   \item{\code{preceding_start}}{Start time of the preceding event (s).}
#'   \item{\code{preceding_end}}{End time (offset) of the preceding event (s).}
#'   \item{\code{following_start}}{Start time (onset) of the following event (s).}
#'   \item{\code{following_end}}{End time of the following event (s).}
#'   \item{\code{latency_sec}}{Time from preceding offset to following onset (s).
#'     Always \eqn{\geq 0} and \eqn{\leq} \code{window_sec}.}
#' }
#' Returns an empty data frame (with the same columns) when no pairs are found.
#'
#' @examples
#' \dontrun{
#' # Distribution of SongBout onset latency after BeggingCall (within 60 s)
#' latencies <- compute_onset_latency(
#'   data            = lys$vocalization_session_map,
#'   preceding_label = "BeggingCall",
#'   following_label = "SongBout",
#'   window_sec      = 60
#' )
#'
#' hist(latencies$latency_sec,
#'      breaks = 30,
#'      main   = "SongBout onset latency after BeggingCall",
#'      xlab   = "Latency (s)")
#'
#' # Across multiple sessions as a density plot
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(latencies, ggplot2::aes(x = latency_sec)) +
#'     ggplot2::geom_histogram(binwidth = 5, fill = "#1976D2", color = "white") +
#'     ggplot2::facet_wrap(~ session) +
#'     ggplot2::labs(
#'       title = "BeggingCall -> SongBout onset latency",
#'       x     = "Latency (s)", y = "Count"
#'     ) +
#'     ggplot2::theme_minimal()
#' }
#' }
#'
#' @export
compute_onset_latency <- function(data,
                                  preceding_label,
                                  following_label,
                                  label_col        = "vocalization_label",
                                  start_col        = "session_relative_start",
                                  end_col          = "session_relative_end",
                                  session_col      = "session_label",
                                  window_sec       = 60,
                                  require_adjacent = TRUE) {

  # Validation
  if (!is.data.frame(data) || !nrow(data)) {
    stop("'data' must be a non-empty data frame.", call. = FALSE)
  }

  required <- c(label_col, start_col, end_col, session_col)
  missing  <- setdiff(required, names(data))
  if (length(missing)) {
    stop(sprintf("'data' is missing column(s): %s",
                 paste(missing, collapse = ", ")),
         call. = FALSE)
  }

  if (!is.character(preceding_label) || length(preceding_label) != 1L) {
    stop("'preceding_label' must be a single character string.", call. = FALSE)
  }
  if (!is.character(following_label) || length(following_label) != 1L) {
    stop("'following_label' must be a single character string.", call. = FALSE)
  }
  if (!is.numeric(window_sec) || length(window_sec) != 1L || window_sec <= 0) {
    stop("'window_sec' must be a single positive number.", call. = FALSE)
  }

  # Empty result template
  empty_result <- data.frame(
    session        = character(),
    preceding_start = numeric(),
    preceding_end  = numeric(),
    following_start = numeric(),
    following_end  = numeric(),
    latency_sec    = numeric(),
    stringsAsFactors = FALSE
  )

  sessions <- unique(data[[session_col]])
  pair_list <- list()

  for (sess in sessions) {
    sess_data <- data[data[[session_col]] == sess, , drop = FALSE]
    sess_data <- sess_data[order(sess_data[[start_col]]), , drop = FALSE]

    pre_rows <- which(sess_data[[label_col]] == preceding_label)
    fol_rows <- which(sess_data[[label_col]] == following_label)

    if (!length(pre_rows) || !length(fol_rows)) next

    for (pi in pre_rows) {
      pre_start <- sess_data[[start_col]][pi]
      pre_end   <- sess_data[[end_col]][pi]

      # Following events that start within the window after preceding offset
      candidates <- fol_rows[
        sess_data[[start_col]][fol_rows] >= pre_end &
        sess_data[[start_col]][fol_rows] <= pre_end + window_sec
      ]

      if (!length(candidates)) next

      # Optionally require adjacency: reject if an intervening preceding event exists
      if (require_adjacent) {
        first_fol_start <- sess_data[[start_col]][candidates[1]]
        intervening <- pre_rows[
          sess_data[[start_col]][pre_rows] > pre_end &
          sess_data[[start_col]][pre_rows] < first_fol_start
        ]
        if (length(intervening)) next
      }

      fi          <- candidates[1]  # Nearest qualifying following event
      fol_start   <- sess_data[[start_col]][fi]
      fol_end     <- sess_data[[end_col]][fi]

      pair_list[[length(pair_list) + 1L]] <- data.frame(
        session         = as.character(sess),
        preceding_start = pre_start,
        preceding_end   = pre_end,
        following_start = fol_start,
        following_end   = fol_end,
        latency_sec     = fol_start - pre_end,
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(pair_list)) {
    return(empty_result)
  }

  result <- do.call(rbind, pair_list)
  rownames(result) <- NULL
  result
}
