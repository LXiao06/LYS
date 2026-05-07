#' Apply Sequence Rules to Annotate Vocalizations
#'
#' @description
#' Evaluates a set of sequential rules to reclassify vocalizations based on their 
#' temporal relationship to preceding events. The rules are evaluated chronologically 
#' for each session, allowing for complex chaining of annotations.
#'
#' @param lys A LYS object that has been processed through \code{map_vocalization_sessions()}.
#' @param sequence_rules A data frame defining sequence-based reclassification rules.
#'   Rules are evaluated in the order they appear for each vocalization.
#'   Required columns:
#'   \itemize{
#'     \item \code{preceding_label} — label of the first event (e.g., \code{"BeggingCall"}). Use \code{NA} to match unconditionally without a preceding event requirement.
#'     \item \code{following_label} — label of the event to be reclassified (e.g., \code{"SongBout"}).
#'     \item \code{max_gap_sec}     — maximum gap in seconds between the end of the preceding event and the start of the following event. Use \code{NA} or \code{Inf} for no upper limit.
#'     \item \code{annotation}      — the new label to assign to the following event (e.g., \code{"PD SongBout"}).
#'   }
#'   Optional column:
#'   \itemize{
#'     \item \code{min_gap_sec}     — minimum gap in seconds. Default is \code{0}.
#'   }
#' @param label_col Column name in the session map holding the original vocalization label. 
#'   Default \code{"vocalization_label"}.
#'
#' @details
#' The function iterates through all events in a session chronologically. 
#' For each event matching a \code{following_label} in the rules, it looks back 
#' for the most recent event matching \code{preceding_label}. 
#' If the gap between the preceding event's offset and the current event's onset 
#' falls within \code{[min_gap_sec, max_gap_sec]}, the event is reclassified 
#' to \code{annotation}. 
#' Because evaluation is chronological, newly reclassified events can immediately 
#' serve as preceding events for subsequent rules.
#'
#' @return The LYS object with a new data frame \code{lys$vocalization_annotations} 
#'   containing the annotated vocalization events. The new labels are stored in the 
#'   \code{annotated_label} column, alongside original labels.
#'
#' @examples
#' \dontrun{
#' rules <- data.frame(
#'   preceding_label = c("BeggingCall", "PD SongBout", "BeggingCall"),
#'   following_label = c("SongBout", "SongBout", "SongBout"),
#'   min_gap_sec     = c(0, 0, 120),
#'   max_gap_sec     = c(60, 10, Inf),
#'   annotation      = c("PD SongBout", "PD SongBout", "UD SongBout")
#' )
#' 
#' lys <- annotate_vocalizations(lys, sequence_rules = rules)
#' table(lys$vocalization_annotations$annotated_label)
#' }
#'
#' @export
annotate_vocalizations <- function(lys, sequence_rules, label_col = "vocalization_label") {

  # --- Input validation ---
  if (!inherits(lys, "lys")) {
    stop("lys must be a LYS object.", call. = FALSE)
  }

  events <- lys$vocalization_session_map
  if (is.null(events) || !is.data.frame(events) || !nrow(events)) {
    stop(
      "lys$vocalization_session_map is empty. ",
      "Run map_vocalization_sessions() first.",
      call. = FALSE
    )
  }

  required_event_cols <- c("session_label", "session_relative_start", "session_relative_end", label_col)
  missing_cols <- setdiff(required_event_cols, names(events))
  if (length(missing_cols)) {
    stop(
      sprintf("vocalization_session_map is missing column(s): %s",
              paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  # --- Validate Sequence Rules ---
  if (!is.data.frame(sequence_rules) || nrow(sequence_rules) == 0) {
    stop("sequence_rules must be a non-empty data frame.", call. = FALSE)
  }

  required_rule_cols <- c("preceding_label", "following_label", "max_gap_sec", "annotation")
  missing_rule_cols <- setdiff(required_rule_cols, names(sequence_rules))
  if (length(missing_rule_cols)) {
    stop(
      sprintf("sequence_rules is missing required column(s): %s",
              paste(missing_rule_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  # Normalize rule columns
  rules <- sequence_rules
  rules$preceding_label <- as.character(rules$preceding_label)
  rules$following_label <- as.character(rules$following_label)
  rules$annotation      <- as.character(rules$annotation)
  rules$max_gap_sec     <- as.numeric(rules$max_gap_sec)
  
  if (!"min_gap_sec" %in% names(rules)) {
    rules$min_gap_sec <- 0
  }
  rules$min_gap_sec <- as.numeric(rules$min_gap_sec)

  # Prepare annotated output
  events$annotated_label <- as.character(events[[label_col]])
  
  sessions <- unique(events$session_label)

  for (sess in sessions) {
    # Get indices for this session, ordered by start time
    idx <- which(events$session_label == sess)
    idx <- idx[order(events$session_relative_start[idx])]
    
    for (i in seq_along(idx)) {
      curr_idx <- idx[i]
      curr_label <- events$annotated_label[curr_idx]
      
      # Filter rules applicable to the current event's label
      app_rules <- rules[rules$following_label == curr_label, , drop = FALSE]
      if (nrow(app_rules) == 0) next
      
      for (r in seq_len(nrow(app_rules))) {
        rule <- app_rules[r, ]
        
        # Unconditional match
        if (is.na(rule$preceding_label)) {
          events$annotated_label[curr_idx] <- rule$annotation
          break # Matched, move to next event
        }
        
        # Look back for most recent preceding event
        prev_indices <- idx[seq_len(i - 1)]
        prec_match_positions <- which(events$annotated_label[prev_indices] == rule$preceding_label)
        
        if (length(prec_match_positions) == 0) {
          gap <- Inf
        } else {
          last_match_pos <- max(prec_match_positions)
          actual_prec_idx <- prev_indices[last_match_pos]
          gap <- events$session_relative_start[curr_idx] - events$session_relative_end[actual_prec_idx]
        }
        
        min_g <- ifelse(is.na(rule$min_gap_sec), 0, rule$min_gap_sec)
        max_g <- ifelse(is.na(rule$max_gap_sec), Inf, rule$max_gap_sec)
        
        if (gap >= min_g && gap <= max_g) {
          events$annotated_label[curr_idx] <- rule$annotation
          break # Matched, move to next event
        }
      }
    }
  }

  lys$vocalization_annotations <- events
  invisible(lys)
}
