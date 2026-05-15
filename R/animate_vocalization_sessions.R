#' Animate Vocalization Sessions with Sliding Window
#'
#' @description
#' Creates a dynamic animation that displays a sliding time window for each
#' recording session, revealing the temporal structure of labeled vocalizations.
#' Sessions scroll independently — each runs to its own end.
#'
#' Sequence rules allow following events to be \strong{reclassified} visually.
#' For example, a SongBout that immediately follows a BeggingCall is displayed
#' as an "FD" (Feeding-Directed) bar in a distinct color, with an optional arrow
#' marking the gap between the two events.
#'
#' @param lys A LYS object that has been processed through
#'   \code{map_vocalization_sessions()}.
#' @param labels Character vector of vocalization labels to display.
#'   If \code{NULL} (default), all labels in the session map are used.
#' @param label_col Column name in the session map holding the vocalization
#'   label.  Default \code{"vocalization_label"}.
#' @param drop_labels Character vector of labels to exclude from display.
#'   Default \code{"TBD"}.
#' @param sequence_rules A data frame defining sequence-based reclassification
#'   rules.  Each row describes one rule:
#'   \itemize{
#'     \item \code{preceding_label} — label of the first event (e.g. \code{"BeggingCall"})
#'     \item \code{following_label} — label of the second event (e.g. \code{"SongBout"})
#'     \item \code{max_gap_sec}     — maximum gap in seconds between the end of the
#'           preceding event and the start of the following event
#'     \item \code{annotation}      — display label for the reclassified following event
#'           (e.g. \code{"FD"}). This label also appears in the legend.
#'     \item \code{color}           — (optional) color for the reclassified bar and
#'           arrow. Default \code{"#D32F2F"}.
#'     \item \code{show_arrow}      — (optional) logical, whether to draw an arrow in
#'           the gap between the two events. Default \code{TRUE}.
#'   }
#'   If \code{NULL} (default), no reclassification is performed.
#' @param window_duration_min Numeric. Width of the sliding window in minutes.
#'   Default \code{10}.
#' @param step_seconds Numeric. How far the window advances per animation frame
#'   (in seconds).  Default \code{30}.
#' @param max_session_sec Numeric or \code{NULL}. Maximum total duration (in
#'   seconds) shown for every session.  All sessions are clipped to this value
#'   so they end at the same point in the animation.
#'   \itemize{
#'     \item \code{NULL} (default) — use the duration of the shortest session,
#'           so every session reaches its natural end at the same frame.
#'     \item A positive number — clip every session at that many seconds
#'           (e.g. \code{3600} for 1 hour). Events that start after the cap are
#'           excluded; events that overlap the cap boundary are shown truncated.
#'   }
#' @param colors Named character vector mapping \emph{display} labels to colors.
#'   Include both base labels (e.g. \code{"SongBout"}) and annotation labels
#'   (e.g. \code{"FD"}) if you want to override auto-assigned colors.
#'   If \code{NULL} (default), colors are generated automatically.
#' @param fps Integer. Frames per second for the output animation.
#'   Default \code{10}.
#' @param output_dir Character. Directory where the animation file is saved.
#'   If \code{NULL}, uses the default LYS output directory.
#' @param output_file Character. File name for the saved animation.
#'   Default \code{"vocalization_session_animation.gif"}.
#' @param width Integer. Width of each animation frame in pixels.
#'   Default \code{1800}.
#' @param height Integer. Height of each animation frame in pixels.
#'   Default \code{NULL} (auto-calculated from the number of sessions).
#' @param res Integer. Resolution (PPI) for each frame.  Default \code{200}.
#' @param show_progress Logical. Show the gray session progress label
#'   (\code{"session [elapsed / total]"})?  Default \code{FALSE}.
#' @param verbose Logical. Print progress messages?  Default \code{TRUE}.
#'
#' @details
#' \strong{Reclassification logic:}
#' For each rule in \code{sequence_rules}, the function scans every session for
#' consecutive event pairs where:
#' \enumerate{
#'   \item The first event matches \code{preceding_label}.
#'   \item The second event matches \code{following_label} and starts within
#'         \code{max_gap_sec} of the first event's end.
#'   \item No other event with \code{preceding_label} falls between them.
#' }
#' Matched \emph{following} events are rendered in the \code{annotation} color
#' instead of their original color.  If \code{show_arrow = TRUE}, an arrow is
#' also drawn across the gap.
#'
#' Requires the \pkg{gifski} package for GIF rendering.
#'
#' @return The LYS object (invisibly), with the animation saved to disk.
#'
#' @examples
#' \dontrun{
#' # Basic: SongBout following BeggingCall shown as "FD" in red
#' fd_rules <- data.frame(
#'   preceding_label = "BeggingCall",
#'   following_label = "SongBout",
#'   max_gap_sec     = 60,
#'   annotation      = "FD",
#'   color           = "#E53935",
#'   show_arrow      = TRUE
#' )
#' lys <- animate_vocalization_sessions(lys, sequence_rules = fd_rules)
#'
#' # Multiple rules
#' rules <- data.frame(
#'   preceding_label = c("BeggingCall", "SongBout"),
#'   following_label = c("SongBout",    "BeggingCall"),
#'   max_gap_sec     = c(60,            30),
#'   annotation      = c("FD",          "Response"),
#'   color           = c("#E53935",     "#1976D2"),
#'   show_arrow      = c(TRUE,          FALSE)
#' )
#' lys <- animate_vocalization_sessions(lys, sequence_rules = rules)
#' }
#'
#' @export
animate_vocalization_sessions <- function(lys,
                                          labels = NULL,
                                          label_col = "vocalization_label",
                                          drop_labels = "TBD",
                                          sequence_rules = NULL,
                                          window_duration_min = 10,
                                          step_seconds = 30,
                                          max_session_sec = NULL,
                                          colors = NULL,
                                          fps = 10,
                                          output_dir = NULL,
                                          output_file = "vocalization_session_animation.gif",
                                          width = 1800,
                                          height = NULL,
                                          res = 200,
                                          show_progress = FALSE,
                                          verbose = TRUE) {

  # --- Input validation ---
  if (!inherits(lys, "lys")) {
    stop("lys must be a LYS object.", call. = FALSE)
  }

  mapped <- lys$vocalization_session_map
  if (is.null(mapped) || !is.data.frame(mapped) || !nrow(mapped)) {
    stop(
      "lys$vocalization_session_map is empty. ",
      "Run map_vocalization_sessions() first.",
      call. = FALSE
    )
  }

  required <- c("session_label", "session_id",
                 "session_relative_start", "session_relative_end", label_col)
  missing_cols <- setdiff(required, names(mapped))
  if (length(missing_cols)) {
    stop(
      sprintf("vocalization_session_map is missing column(s): %s",
              paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  ensure_pkgs("gifski")

  # --- Resolve base labels ---
  if (is.null(labels)) {
    labels <- sort(unique(mapped[[label_col]]))
    labels <- labels[!is.na(labels) & nzchar(labels)]
  }
  labels <- setdiff(labels, drop_labels)
  if (!length(labels)) {
    stop("No labels remain after dropping.", call. = FALSE)
  }

  # --- Filter to requested labels ---
  keep <- mapped[[label_col]] %in% labels
  mapped <- mapped[keep, , drop = FALSE]
  if (!nrow(mapped)) {
    stop("No vocalizations remain after filtering to the requested labels.",
         call. = FALSE)
  }

  # --- Validate sequence rules ---
  seq_rules <- validate_sequence_rules(sequence_rules)

  # --- Session ordering ---
  sessions <- unique(mapped$session_label[order(mapped$session_id)])
  n_sessions <- length(sessions)

  # --- Detect reclassified events (precompute) ---
  # seq_pairs: data frame of matched pairs + which following events are reclassified
  seq_pairs <- detect_sequence_pairs(
    mapped = mapped,
    label_col = label_col,
    seq_rules = seq_rules
  )

  # Build full display label set (base labels + annotation labels from rules)
  annotation_labels <- unique(seq_rules$annotation)
  display_labels <- union(labels, annotation_labels)

  # --- Color map covers both base and annotation labels ---
  color_map <- make_label_colors(display_labels, colors = colors)

  if (verbose && nrow(seq_pairs)) {
    for (ri in seq_len(nrow(seq_rules))) {
      n_found <- sum(seq_pairs$annotation == seq_rules$annotation[ri])
      max_gap <- seq_rules$max_gap_sec[ri]
      gap_str <- if (is.infinite(max_gap)) "Inf" else as.character(as.integer(max_gap))
      message(sprintf(
        "  Rule '%s' (%s -> %s within %ss): %d event(s) reclassified.",
        seq_rules$annotation[ri],
        seq_rules$preceding_label[ri],
        seq_rules$following_label[ri],
        gap_str,
        n_found
      ))
    }
  }

  # --- Compute per-session max time ---
  session_max <- tapply(mapped$session_relative_end, mapped$session_label, max)
  window_sec  <- window_duration_min * 60

  # --- Apply session duration cap ---
  # Default: shortest session so all sessions end at the same frame.
  # User can override with an explicit number of seconds.
  cap_sec <- if (is.null(max_session_sec)) {
    min(session_max, na.rm = TRUE)
  } else {
    as.numeric(max_session_sec)
  }

  if (verbose) {
    message(sprintf(
      "Session duration cap: %.0f s (%.1f min) [%s].",
      cap_sec, cap_sec / 60,
      if (is.null(max_session_sec)) "shortest session" else "user-specified"
    ))
  }

  # Clip session_max so scrolling stops at the cap for every session
  session_max <- pmin(session_max, cap_sec)

  # Drop events that start at or after the cap, clip those that overlap it
  mapped <- mapped[mapped$session_relative_start < cap_sec, , drop = FALSE]
  mapped$session_relative_end <- pmin(mapped$session_relative_end, cap_sec)

  # Also clip seq_pairs (following events that start before the cap are kept)
  if (nrow(seq_pairs)) {
    seq_pairs <- seq_pairs[seq_pairs$following_start < cap_sec, , drop = FALSE]
    seq_pairs$following_end <- pmin(seq_pairs$following_end, cap_sec)
  }

  global_max <- cap_sec
  n_frames   <- max(1L, ceiling((global_max - window_sec) / step_seconds) + 1L)
  n_frames   <- n_frames + 1L

  if (verbose) {
    message(sprintf(
      "Generating animation: %d session(s), %.0f-min window, %d frame(s) at %d fps.",
      n_sessions, window_duration_min, n_frames, fps
    ))
  }

  # --- Auto-height ---
  if (is.null(height)) {
    height <- max(700L, 180L + 120L * n_sessions)
  }

  # --- Output directory ---
  output_dir <- resolve_lys_output_dir(lys$base_path, output_dir = output_dir)
  anim_dir <- file.path(output_dir, "plots", "vocalization_session_animation")
  dir.create(anim_dir, recursive = TRUE, showWarnings = FALSE)

  frame_dir <- file.path(anim_dir, ".frames")
  dir.create(frame_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(frame_dir, recursive = TRUE), add = TRUE)

  # --- Generate frames ---
  frame_paths <- character(n_frames)
  if (verbose) {
    pb <- utils::txtProgressBar(min = 0, max = n_frames, style = 3)
  }

  for (frame_i in seq_len(n_frames)) {
    frame_path <- file.path(frame_dir, sprintf("frame_%05d.png", frame_i))
    frame_paths[frame_i] <- frame_path

    grDevices::png(filename = frame_path, width = width, height = height, res = res)
    tryCatch(
      draw_animation_frame(
        mapped     = mapped,
        seq_pairs  = seq_pairs,
        sessions   = sessions,
        session_max = session_max,
        color_map  = color_map,
        label_col  = label_col,
        frame_i    = frame_i,
        step_seconds = step_seconds,
        window_sec = window_sec,
        window_duration_min = window_duration_min,
        show_progress = show_progress
      ),
      finally = grDevices::dev.off()
    )

    if (verbose) utils::setTxtProgressBar(pb, frame_i)
  }

  if (verbose) close(pb)

  # --- Stitch into GIF ---
  gif_path <- file.path(anim_dir, output_file)
  gifski::gifski(
    png_files = frame_paths,
    gif_file  = gif_path,
    width     = width,
    height    = height,
    delay     = 1 / fps
  )

  if (verbose) message("Animation saved to: ", gif_path)

  invisible(lys)
}


# ---------------------------------------------------------------------------
# Internal: validate and normalise sequence_rules
# ---------------------------------------------------------------------------
validate_sequence_rules <- function(sequence_rules) {
  empty <- data.frame(
    preceding_label = character(),
    following_label = character(),
    min_gap_sec     = numeric(),
    max_gap_sec     = numeric(),
    annotation      = character(),
    color           = character(),
    show_arrow      = logical(),
    stringsAsFactors = FALSE
  )

  if (is.null(sequence_rules)) return(empty)

  if (!is.data.frame(sequence_rules) || !nrow(sequence_rules)) {
    stop("sequence_rules must be a non-empty data frame.", call. = FALSE)
  }

  required <- c("preceding_label", "following_label", "max_gap_sec", "annotation")
  missing <- setdiff(required, names(sequence_rules))
  if (length(missing)) {
    stop(
      sprintf("sequence_rules is missing required column(s): %s",
              paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  sequence_rules$preceding_label <- as.character(sequence_rules$preceding_label)
  sequence_rules$following_label <- as.character(sequence_rules$following_label)
  sequence_rules$max_gap_sec     <- as.numeric(sequence_rules$max_gap_sec)
  sequence_rules$annotation      <- as.character(sequence_rules$annotation)

  if (!"min_gap_sec" %in% names(sequence_rules)) {
    sequence_rules$min_gap_sec <- 0
  }
  sequence_rules$min_gap_sec <- as.numeric(sequence_rules$min_gap_sec)

  if (!"color" %in% names(sequence_rules)) {
    sequence_rules$color <- "#D32F2F"
  }
  sequence_rules$color <- as.character(sequence_rules$color)

  if (!"show_arrow" %in% names(sequence_rules)) {
    sequence_rules$show_arrow <- TRUE
  }
  sequence_rules$show_arrow <- as.logical(sequence_rules$show_arrow)

  sequence_rules
}


# ---------------------------------------------------------------------------
# Internal: detect sequence pairs and mark following events for reclassification
#
# Returns a data frame with one row per matched pair:
#   session_label, preceding_end, following_start, following_end,
#   annotation, color, show_arrow
#
# The following event is uniquely identified by (session_label, following_start)
# so the drawing code can look it up when rendering bars.
# ---------------------------------------------------------------------------
detect_sequence_pairs <- function(mapped, label_col, seq_rules) {

  empty <- data.frame(
    session_label   = character(),
    preceding_end   = numeric(),
    following_start = numeric(),
    following_end   = numeric(),
    annotation      = character(),
    color           = character(),
    show_arrow      = logical(),
    stringsAsFactors = FALSE
  )

  if (!nrow(seq_rules)) return(empty)

  pair_list <- list()
  sessions  <- unique(mapped$session_label)
  
  # Copy labels to track state sequentially
  mapped$annotated_label <- as.character(mapped[[label_col]])

  for (sess in sessions) {
    idx <- which(mapped$session_label == sess)
    idx <- idx[order(mapped$session_relative_start[idx])]

    for (i in seq_along(idx)) {
      curr_idx <- idx[i]
      curr_label <- mapped$annotated_label[curr_idx]

      app_rules <- seq_rules[seq_rules$following_label == curr_label, , drop = FALSE]
      if (nrow(app_rules) == 0) next

      for (r in seq_len(nrow(app_rules))) {
        rule <- app_rules[r, ]

        if (is.na(rule$preceding_label)) {
          mapped$annotated_label[curr_idx] <- rule$annotation
          pair_list[[length(pair_list) + 1L]] <- data.frame(
            session_label   = sess,
            preceding_end   = NA_real_,
            following_start = mapped$session_relative_start[curr_idx],
            following_end   = mapped$session_relative_end[curr_idx],
            annotation      = rule$annotation,
            color           = rule$color,
            show_arrow      = FALSE,
            stringsAsFactors = FALSE
          )
          break
        }

        prev_indices <- idx[seq_len(i - 1)]
        prec_match_positions <- which(mapped$annotated_label[prev_indices] == rule$preceding_label)

        if (length(prec_match_positions) == 0) {
          gap <- Inf
          actual_prec_idx <- NA
        } else {
          last_match_pos <- max(prec_match_positions)
          actual_prec_idx <- prev_indices[last_match_pos]
          gap <- mapped$session_relative_start[curr_idx] - mapped$session_relative_end[actual_prec_idx]
        }

        min_g <- ifelse(is.na(rule$min_gap_sec), 0, rule$min_gap_sec)
        max_g <- ifelse(is.na(rule$max_gap_sec), Inf, rule$max_gap_sec)

        if (gap >= min_g && gap <= max_g) {
          mapped$annotated_label[curr_idx] <- rule$annotation
          
          show_arrow <- rule$show_arrow
          if (is.infinite(gap)) show_arrow <- FALSE
          prec_end_val <- if (is.na(actual_prec_idx)) NA_real_ else mapped$session_relative_end[actual_prec_idx]
          
          pair_list[[length(pair_list) + 1L]] <- data.frame(
            session_label   = sess,
            preceding_end   = prec_end_val,
            following_start = mapped$session_relative_start[curr_idx],
            following_end   = mapped$session_relative_end[curr_idx],
            annotation      = rule$annotation,
            color           = rule$color,
            show_arrow      = show_arrow,
            stringsAsFactors = FALSE
          )
          break
        }
      }
    }
  }

  if (length(pair_list)) do.call(rbind, pair_list) else empty
}


# ---------------------------------------------------------------------------
# Internal: draw a single animation frame (sliding window)
# ---------------------------------------------------------------------------
draw_animation_frame <- function(mapped,
                                 seq_pairs,
                                 sessions,
                                 session_max,
                                 color_map,
                                 label_col,
                                 frame_i,
                                 step_seconds,
                                 window_sec,
                                 window_duration_min,
                                 show_progress = FALSE) {

  n_sessions <- length(sessions)
  session_y  <- rev(seq_len(n_sessions))
  names(session_y) <- sessions

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(4.5, 8, 3, 2), xaxs = "i", bg = "white")

  # Per-session window boundaries (independent scrolling)
  global_offset  <- (frame_i - 1L) * step_seconds
  session_xleft  <- numeric(n_sessions)
  session_xright <- numeric(n_sessions)

  for (si in seq_len(n_sessions)) {
    sess  <- sessions[si]
    s_max <- session_max[[sess]]
    left  <- min(global_offset, max(0, s_max - window_sec))
    session_xleft[si]  <- left
    session_xright[si] <- left + window_sec
  }

  xlim <- c(0, window_sec)

  graphics::plot(
    NA,
    xlim = xlim,
    ylim = c(-0.2, n_sessions + 0.8),
    xlab = "", ylab = "", yaxt = "n", xaxt = "n",
    main     = sprintf("Vocalization Sessions \u2014 %d min window",
                        as.integer(window_duration_min)),
    cex.main = 0.95,
    font.main = 1
  )

  tick_secs <- seq(0, window_sec, by = 60)
  graphics::axis(1, at = tick_secs,
                 labels = sprintf("%d", tick_secs / 60), cex.axis = 0.75)
  graphics::mtext("Time (min)", side = 1, line = 2.5, cex = 0.8)
  graphics::axis(2, at = session_y, labels = names(session_y),
                 las = 2, cex.axis = 0.7)
  graphics::abline(h = session_y, col = "grey90", lwd = 0.8)

  row_height    <- 0.34
  arrow_y_off   <- 0.48

  # --- Per-session rendering ---
  for (si in seq_len(n_sessions)) {
    sess    <- sessions[si]
    y       <- session_y[[sess]]
    w_left  <- session_xleft[si]
    w_right <- session_xright[si]
    s_max   <- session_max[[sess]]

    # Progress label (optional)
    if (show_progress) {
      graphics::text(
        x = window_sec * 0.99,
        y = y + row_height + 0.15,
        labels = sprintf("%s  [%s / %s]", sess,
                         format_duration_label(w_left),
                         format_duration_label(s_max)),
        adj = c(1, 0), cex = 0.45, col = "grey50", font = 3
      )
    }

    # Visible events in this session
    sess_rows <- mapped$session_label == sess &
      mapped$session_relative_end   >= w_left &
      mapped$session_relative_start <= w_right

    if (!any(sess_rows)) next

    sess_data <- mapped[sess_rows, , drop = FALSE]

    # Reclassified events for this session (identified by following_start)
    sess_pairs <- if (nrow(seq_pairs)) {
      seq_pairs[seq_pairs$session_label == sess, , drop = FALSE]
    } else {
      seq_pairs[integer(0), , drop = FALSE]
    }

    for (ri in seq_len(nrow(sess_data))) {
      ev_start <- sess_data$session_relative_start[ri]
      ev_end   <- sess_data$session_relative_end[ri]
      base_lbl <- as.character(sess_data[[label_col]][ri])

      # Check if this event is a reclassified following event
      reclassify_idx <- if (nrow(sess_pairs)) {
        which(
          abs(sess_pairs$following_start - ev_start) < 1e-6 &
          abs(sess_pairs$following_end   - ev_end)   < 1e-6
        )
      } else {
        integer(0)
      }

      display_lbl <- if (length(reclassify_idx)) {
        sess_pairs$annotation[reclassify_idx[1]]
      } else {
        base_lbl
      }

      bar_color <- color_map[[display_lbl]]

      graphics::rect(
        xleft   = max(0, ev_start - w_left),
        ybottom = y - row_height,
        xright  = min(window_sec, ev_end - w_left),
        ytop    = y + row_height,
        col     = bar_color,
        border  = NA
      )
    }

    # --- Arrows for pairs with show_arrow = TRUE ---
    if (nrow(sess_pairs)) {
      arrow_pairs <- sess_pairs[sess_pairs$show_arrow, , drop = FALSE]
      for (fi in seq_len(nrow(arrow_pairs))) {
        pre_end_local <- arrow_pairs$preceding_end[fi]   - w_left
        fol_sta_local <- arrow_pairs$following_start[fi] - w_left

        if (is.na(pre_end_local)) next
        if (pre_end_local > window_sec || fol_sta_local < 0) next
        if (pre_end_local < 0 && fol_sta_local > window_sec) next

        ax0 <- max(0, pre_end_local)
        ax1 <- min(window_sec, fol_sta_local)
        if (ax1 - ax0 < 2) next

        # Arrow is always black; no text label to avoid clashing with bar colors
        graphics::arrows(
          x0 = ax0, y0 = y + arrow_y_off,
          x1 = ax1, y1 = y + arrow_y_off,
          length = 0.06, angle = 25, code = 2,
          col = "black", lwd = 1.5
        )
      }
    }
  }

  # --- Legend: horizontal row placed below the bottom session bar ---
  # The ylim bottom is -0.2; the lowest session bar ends at y = 1 - row_height = 0.66.
  # We use the empty space between them for the legend.
  graphics::legend(
    x      = xlim[1],
    y      = 0.45,
    legend = names(color_map),
    fill   = unname(color_map),
    border = NA, bty = "n",
    horiz  = TRUE,
    cex    = 0.75
  )

  invisible(NULL)
}
