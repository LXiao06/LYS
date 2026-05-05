#' Animate Vocalization Sessions — Sweeping Reveal
#'
#' @description
#' Creates a dynamic animation that progressively reveals labeled vocalizations
#' across recording sessions using a sweeping playhead.
#' The plot layout matches \code{map_vocalization_sessions()} — all sessions are
#' stacked vertically with a shared absolute time axis (seconds from session
#' start).  A vertical playhead sweeps from left to right, revealing events
#' simultaneously across all sessions.
#'
#' Sequence rules allow following events to be \strong{reclassified} visually.
#' For example, a SongBout that immediately follows a BeggingCall is displayed
#' as an "FD" bar in a distinct color, with an optional black arrow marking the
#' gap between the two events.
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
#'     \item \code{max_gap_sec}     — maximum gap in seconds between the end of
#'           the preceding event and the start of the following event
#'     \item \code{annotation}      — display label for the reclassified following
#'           event (e.g. \code{"FD"}).  This label also appears in the legend.
#'     \item \code{color}           — (optional) color for the reclassified bar.
#'           Default \code{"#D32F2F"}.
#'     \item \code{show_arrow}      — (optional) logical, whether to draw a black
#'           arrow in the gap between the two events.  Default \code{TRUE}.
#'   }
#'   If \code{NULL} (default), no reclassification is performed.
#' @param step_seconds Numeric. How far the playhead advances per animation
#'   frame (in seconds).  Default \code{30}.
#' @param colors Named character vector mapping \emph{display} labels to colors.
#'   Include both base labels and annotation labels to override auto-assigned
#'   colors.  If \code{NULL} (default), colors are generated automatically.
#' @param scale_bar_seconds Numeric. Length of the scale bar in seconds.
#'   If \code{NULL}, chosen automatically.
#' @param fps Integer. Frames per second for the output animation.
#'   Default \code{10}.
#' @param output_dir Character. Directory where the animation file is saved.
#'   If \code{NULL}, uses the default LYS output directory.
#' @param output_file Character. File name for the saved animation.
#'   Default \code{"vocalization_session_sweep.gif"}.
#' @param width Integer. Width of each animation frame in pixels.
#'   Default \code{1800}.
#' @param height Integer. Height of each animation frame in pixels.
#'   Default \code{NULL} (auto-calculated from the number of sessions).
#' @param res Integer. Resolution (PPI) for each frame.  Default \code{200}.
#' @param verbose Logical. Print progress messages?  Default \code{TRUE}.
#'
#' @details
#' The animation preserves the exact layout of
#' \code{plot_vocalization_session_map()}: x-axis is "Time from session start
#' (s)", y-axis lists sessions, rectangles represent vocalizations.
#'
#' Matched \emph{following} events are rendered in the \code{annotation} color.
#' If \code{show_arrow = TRUE}, a black arrow is drawn across the gap once the
#' playhead reaches the following event.
#'
#' Requires the \pkg{gifski} package for GIF rendering.
#'
#' @return The LYS object (invisibly), with the animation saved to disk.
#'
#' @examples
#' \dontrun{
#' # Basic (no sequence rules)
#' lys <- animate_sessions_sweep(lys)
#'
#' # With FD reclassification
#' fd_rules <- data.frame(
#'   preceding_label = "BeggingCall",
#'   following_label = "SongBout",
#'   max_gap_sec     = 60,
#'   annotation      = "FD",
#'   color           = "#E53935",
#'   show_arrow      = TRUE
#' )
#' lys <- animate_sessions_sweep(lys, sequence_rules = fd_rules)
#' }
#'
#' @export
animate_sessions_sweep <- function(lys,
                                   labels = NULL,
                                   label_col = "vocalization_label",
                                   drop_labels = "TBD",
                                   sequence_rules = NULL,
                                   step_seconds = 30,
                                   colors = NULL,
                                   scale_bar_seconds = NULL,
                                   fps = 10,
                                   output_dir = NULL,
                                   output_file = "vocalization_session_sweep.gif",
                                   width = 1800,
                                   height = NULL,
                                   res = 200,
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

  # --- Resolve labels ---
  if (is.null(labels)) {
    labels <- sort(unique(mapped[[label_col]]))
    labels <- labels[!is.na(labels) & nzchar(labels)]
  }
  labels <- setdiff(labels, drop_labels)
  if (!length(labels)) stop("No labels remain after dropping.", call. = FALSE)

  # --- Filter to requested labels ---
  keep <- mapped[[label_col]] %in% labels
  mapped <- mapped[keep, , drop = FALSE]
  if (!nrow(mapped)) {
    stop("No vocalizations remain after filtering to the requested labels.",
         call. = FALSE)
  }

  # --- Validate sequence rules ---
  seq_rules <- validate_sequence_rules(sequence_rules)

  # --- Session ordering (same as plot_vocalization_session_map) ---
  sessions   <- unique(mapped$session_label[order(mapped$session_id)])
  n_sessions <- length(sessions)

  # --- Detect reclassified events (precompute) ---
  seq_pairs <- detect_sequence_pairs(
    mapped    = mapped,
    label_col = label_col,
    seq_rules = seq_rules
  )

  # Build display label set: base labels + annotation labels
  annotation_labels <- unique(seq_rules$annotation)
  display_labels    <- union(labels, annotation_labels)

  # --- Color map covers both base and annotation labels ---
  color_map <- make_label_colors(display_labels, colors = colors)

  if (verbose && nrow(seq_pairs)) {
    for (ri in seq_len(nrow(seq_rules))) {
      n_found <- sum(seq_pairs$annotation == seq_rules$annotation[ri])
      message(sprintf(
        "  Rule '%s' (%s -> %s within %ds): %d event(s) reclassified.",
        seq_rules$annotation[ri],
        seq_rules$preceding_label[ri],
        seq_rules$following_label[ri],
        as.integer(seq_rules$max_gap_sec[ri]),
        n_found
      ))
    }
  }

  # --- Global x-axis ---
  max_time <- max(mapped$session_relative_end, na.rm = TRUE)
  xlim     <- c(0, max_time * 1.04)

  # --- Scale bar ---
  if (is.null(scale_bar_seconds)) {
    scale_bar_seconds <- choose_scale_bar_seconds(max_time)
  }

  # --- Frame count ---
  n_frames <- max(1L, ceiling(xlim[2] / step_seconds)) + 1L

  if (verbose) {
    message(sprintf(
      "Generating sweep animation: %d session(s), %d frame(s) at %d fps.",
      n_sessions, n_frames, fps
    ))
  }

  # --- Auto-height ---
  if (is.null(height)) {
    height <- max(700L, 180L + 120L * n_sessions)
  }

  # --- Output directory ---
  output_dir <- resolve_lys_output_dir(lys$base_path, output_dir = output_dir)
  anim_dir   <- file.path(output_dir, "plots", "vocalization_session_sweep")
  dir.create(anim_dir, recursive = TRUE, showWarnings = FALSE)

  frame_dir <- file.path(anim_dir, ".frames")
  dir.create(frame_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(frame_dir, recursive = TRUE), add = TRUE)

  # --- Generate frames ---
  frame_paths <- character(n_frames)
  if (verbose) pb <- utils::txtProgressBar(min = 0, max = n_frames, style = 3)

  for (frame_i in seq_len(n_frames)) {
    frame_path          <- file.path(frame_dir, sprintf("frame_%05d.png", frame_i))
    frame_paths[frame_i] <- frame_path
    playhead             <- (frame_i - 1L) * step_seconds

    grDevices::png(filename = frame_path, width = width, height = height, res = res)
    tryCatch(
      draw_sweep_frame(
        mapped            = mapped,
        seq_pairs         = seq_pairs,
        sessions          = sessions,
        color_map         = color_map,
        label_col         = label_col,
        xlim              = xlim,
        playhead          = playhead,
        scale_bar_seconds = scale_bar_seconds
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
# Internal: draw a single sweep-reveal frame
# ---------------------------------------------------------------------------
draw_sweep_frame <- function(mapped,
                             seq_pairs,
                             sessions,
                             color_map,
                             label_col,
                             xlim,
                             playhead,
                             scale_bar_seconds) {

  n_sessions <- length(sessions)
  session_y  <- rev(seq_along(sessions))
  names(session_y) <- sessions

  # --- Layout: matches plot_vocalization_session_map exactly ---
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(4.5, 8, 2, 2), xaxs = "i")

  graphics::plot(
    NA,
    xlim = xlim,
    ylim = c(-0.2, n_sessions + 0.8),
    xlab = "Time from session start (s)",
    ylab = "",
    yaxt = "n",
    main = "Labeled Vocalizations by Session"
  )
  graphics::axis(2, at = session_y, labels = names(session_y), las = 2)
  graphics::abline(h = session_y, col = "grey90", lwd = 0.8)

  row_height  <- 0.34
  arrow_y_off <- 0.48

  # --- Draw revealed vocalizations (reclassifying matched following events) ---
  revealed <- mapped$session_relative_start <= playhead
  if (any(revealed)) {
    rev_data <- mapped[revealed, , drop = FALSE]

    for (i in seq_len(nrow(rev_data))) {
      sess      <- as.character(rev_data$session_label[i])
      y         <- session_y[[sess]]
      ev_start  <- rev_data$session_relative_start[i]
      ev_end    <- rev_data$session_relative_end[i]
      base_lbl  <- as.character(rev_data[[label_col]][i])

      # Clip right edge to playhead for partially-revealed events
      x_right <- min(ev_end, playhead)

      # Check if this event is a reclassified following event
      sess_pairs <- if (nrow(seq_pairs)) {
        seq_pairs[seq_pairs$session_label == sess, , drop = FALSE]
      } else {
        seq_pairs[integer(0), , drop = FALSE]
      }

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

      graphics::rect(
        xleft   = ev_start,
        ybottom = y - row_height,
        xright  = x_right,
        ytop    = y + row_height,
        col     = color_map[[display_lbl]],
        border  = NA
      )
    }
  }

  # --- Draw revealed sequence arrows (black, no label) ---
  if (nrow(seq_pairs)) {
    revealed_seqs <- seq_pairs$following_start <= playhead &
                     seq_pairs$show_arrow
    if (any(revealed_seqs)) {
      rev_seqs <- seq_pairs[revealed_seqs, , drop = FALSE]
      for (i in seq_len(nrow(rev_seqs))) {
        y        <- session_y[[as.character(rev_seqs$session_label[i])]]
        arrow_x0 <- rev_seqs$preceding_end[i]
        arrow_x1 <- rev_seqs$following_start[i]

        if (arrow_x1 - arrow_x0 < 2) next

        graphics::arrows(
          x0 = arrow_x0, y0 = y + arrow_y_off,
          x1 = arrow_x1, y1 = y + arrow_y_off,
          length = 0.06, angle = 25, code = 2,
          col = "black", lwd = 1.5
        )
      }
    }
  }

  # --- Playhead line ---
  if (playhead > 0 && playhead < xlim[2]) {
    graphics::abline(v = playhead, col = "#1565C0", lwd = 1.5, lty = 2)
  }

  # --- Legend: horizontal row below bottom session bar ---
  graphics::legend(
    x      = xlim[1],
    y      = 0.45,
    legend = names(color_map),
    fill   = unname(color_map),
    border = NA, bty = "n",
    horiz  = TRUE,
    cex    = 0.75
  )

  # --- Scale bar ---
  if (is.finite(scale_bar_seconds) && scale_bar_seconds > 0) {
    x0 <- xlim[2] - scale_bar_seconds - 0.02 * diff(xlim)
    x1 <- x0 + scale_bar_seconds
    y0 <- 0.1
    graphics::segments(x0, y0, x1, y0, lwd = 2)
    graphics::segments(c(x0, x1), y0 - 0.035, c(x0, x1), y0 + 0.035, lwd = 1.5)
    graphics::text(
      x = (x0 + x1) / 2, y = y0 + 0.12,
      labels = format_duration_label(scale_bar_seconds),
      cex = 0.78
    )
  }

  invisible(NULL)
}
