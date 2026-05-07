# =============================================================================
# LYS — Vocalization sequence statistics
#   plot_reciprocal_latency()      – reciprocal histogram + embedded stats
#   permutation_transition_test()  – label-shuffle randomisation test
#   conditional_rate_ratio()       – foreground vs background event rate ratio
# =============================================================================


# ── plot_reciprocal_latency ───────────────────────────────────────────────────

#' Plot Reciprocal Onset Latency Distributions
#'
#' @description
#' Computes and plots the onset latency distributions between two vocalization
#' labels in both directions (\code{label1} -> \code{label2} and
#' \code{label2} -> \code{label1}) side-by-side.  The histograms share the
#' same X and Y axis scales for easy visual comparison.
#' A two-sample Kolmogorov-Smirnov test and a Wilcoxon rank-sum test compare
#' the latency distributions; a binomial test assesses directional asymmetry
#' in pair counts.
#'
#' @param data A data frame of vocalization events
#'   (e.g., \code{lys$vocalization_session_map}).
#' @param label1 Character. First label.
#' @param label2 Character. Second label.
#' @param window_sec Numeric. Maximum time (seconds) to search for following
#'   event. Default \code{120}.
#' @param breaks Numeric. Approximate number of histogram bins. Default
#'   \code{20}.
#' @param label_col Character. Column holding the event label.
#'   Default \code{"vocalization_label"}.
#' @param start_col Character. Column holding the event start time.
#'   Default \code{"session_relative_start"}.
#' @param end_col Character. Column holding the event end time.
#'   Default \code{"session_relative_end"}.
#' @param session_col Character. Column used to group events into sessions.
#'   Default \code{"session_label"}.
#' @param require_adjacent Logical. If \code{TRUE}, intervening events of the
#'   preceding type invalidate the match. Default \code{TRUE}.
#'
#' @return Invisibly, a list with:
#' \describe{
#'   \item{\code{latencies_1_to_2}}{Matched-pair data frame for
#'     \code{label1} -> \code{label2}.}
#'   \item{\code{latencies_2_to_1}}{Matched-pair data frame for
#'     \code{label2} -> \code{label1}.}
#'   \item{\code{binom_test}}{Result of \code{binom.test()} testing whether
#'     \code{label1->label2} accounts for more than 50\% of all matched pairs
#'     (one-sided, H1: p > 0.5).}
#'   \item{\code{ks_test}}{Result of \code{ks.test()} comparing the two
#'     distributions.}
#'   \item{\code{wilcox_test}}{Result of \code{wilcox.test()} comparing the
#'     two distributions.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- plot_reciprocal_latency(
#'   data       = lys$vocalization_session_map,
#'   label1     = "BeggingCall",
#'   label2     = "SongBout",
#'   window_sec = 120,
#'   breaks     = 20
#' )
#' res$binom_test
#' }
#' @export
plot_reciprocal_latency <- function(data,
                                    label1,
                                    label2,
                                    window_sec       = 120,
                                    breaks           = 20,
                                    label_col        = "vocalization_label",
                                    start_col        = "session_relative_start",
                                    end_col          = "session_relative_end",
                                    session_col      = "session_label",
                                    require_adjacent = TRUE) {

  # Compute 1 -> 2
  lat1 <- compute_onset_latency(
    data             = data,
    preceding_label  = label1,
    following_label  = label2,
    label_col        = label_col,
    start_col        = start_col,
    end_col          = end_col,
    session_col      = session_col,
    window_sec       = window_sec,
    require_adjacent = require_adjacent
  )

  # Compute 2 -> 1
  lat2 <- compute_onset_latency(
    data             = data,
    preceding_label  = label2,
    following_label  = label1,
    label_col        = label_col,
    start_col        = start_col,
    end_col          = end_col,
    session_col      = session_col,
    window_sec       = window_sec,
    require_adjacent = require_adjacent
  )

  brks <- seq(0, window_sec, length.out = breaks + 1)

  h1_counts <- if (nrow(lat1) > 0) {
    hist(lat1$latency_sec, breaks = brks, plot = FALSE)$counts
  } else {
    0
  }

  h2_counts <- if (nrow(lat2) > 0) {
    hist(lat2$latency_sec, breaks = brks, plot = FALSE)$counts
  } else {
    0
  }

  y_max <- max(c(h1_counts, h2_counts, 0))
  if (y_max == 0) y_max <- 1

  # --- Distribution comparison tests ---
  n1      <- nrow(lat1)
  n2      <- nrow(lat2)
  n_total <- n1 + n2
  can_test <- n1 >= 2L && n2 >= 2L

  ks_test     <- NULL
  wilcox_test <- NULL
  binom_test  <- NULL

  fmt_p <- function(p) if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)

  # Binomial test: is label1->label2 more than 50% of all pairs?
  if (n_total > 0L) {
    binom_test <- binom.test(x = n1, n = n_total, p = 0.5,
                             alternative = "greater")
  }

  if (can_test) {
    ks_test     <- ks.test(lat1$latency_sec, lat2$latency_sec)
    wilcox_test <- wilcox.test(lat1$latency_sec, lat2$latency_sec,
                               exact = FALSE, alternative = "two.sided")

    message(
      sprintf("\nReciprocal latency analysis: %s <-> %s\n", label1, label2),
      sprintf("  n(%s -> %s) = %d,  n(%s -> %s) = %d\n",
              label1, label2, n1, label2, label1, n2),
      sprintf("  Binomial test (H1: %s -> %s > 50%%):  p %s\n",
              label1, label2, fmt_p(binom_test$p.value)),
      sprintf("  KS test:      D = %.3f,  p %s\n",
              ks_test$statistic, fmt_p(ks_test$p.value)),
      sprintf("  Wilcoxon:     W = %.0f,  p %s\n",
              wilcox_test$statistic, fmt_p(wilcox_test$p.value))
    )
  } else {
    message(
      sprintf("\nReciprocal latency analysis: %s <-> %s\n", label1, label2),
      sprintf("  n(%s -> %s) = %d,  n(%s -> %s) = %d\n",
              label1, label2, n1, label2, label1, n2),
      if (!is.null(binom_test))
        sprintf("  Binomial test (H1: %s -> %s > 50%%):  p %s\n",
                label1, label2, fmt_p(binom_test$p.value))
      else "  Binomial test: no pairs found\n",
      "  KS / Wilcoxon: insufficient data (need n >= 2 in each direction)\n"
    )
  }

  # --- Bottom annotation: plain-language interpretation ---
  sentences <- character(0)

  # Sentence 1: directional count asymmetry (binomial)
  if (!is.null(binom_test)) {
    dominant <- if (n1 > n2) paste(label1, "->", label2) else paste(label2, "->", label1)
    n_dom    <- max(n1, n2)
    n_non    <- min(n1, n2)
    if (binom_test$p.value < 0.05) {
      sentences <- c(sentences,
        sprintf("%s transitions occurred significantly more often than the reverse (%d vs %d; binomial p %s).",
                dominant, n_dom, n_non, fmt_p(binom_test$p.value)))
    } else {
      sentences <- c(sentences,
        sprintf("No significant directional asymmetry in transition counts (%d vs %d; binomial p %s).",
                n1, n2, fmt_p(binom_test$p.value)))
    }
  }

  # Sentence 2: latency distribution difference
  if (can_test) {
    ks_sig  <- ks_test$p.value     < 0.05
    wil_sig <- wilcox_test$p.value < 0.05
    if (ks_sig || wil_sig) {
      test_str <- if (ks_sig && wil_sig) {
        sprintf("KS p %s; Wilcoxon p %s",
                fmt_p(ks_test$p.value), fmt_p(wilcox_test$p.value))
      } else if (ks_sig) {
        sprintf("KS p %s", fmt_p(ks_test$p.value))
      } else {
        sprintf("Wilcoxon p %s", fmt_p(wilcox_test$p.value))
      }
      sentences <- c(sentences,
        sprintf("The latency distributions also differed significantly between directions (%s).",
                test_str))
    } else {
      sentences <- c(sentences,
        sprintf("Latency distributions did not differ significantly between directions (KS p %s; Wilcoxon p %s).",
                fmt_p(ks_test$p.value), fmt_p(wilcox_test$p.value)))
    }
  }

  bottom_note <- if (length(sentences)) paste(sentences, collapse = " ") else NULL

  y_plot_max <- y_max

  # outer = TRUE requires oma bottom margin
  old_par <- par(mfrow = c(1, 2),
                 mar  = c(4, 4, 4, 1) + 0.1,
                 oma  = c(if (!is.null(bottom_note)) 2.5 else 0.5, 0, 0, 0))
  on.exit(par(old_par))

  # Helper: annotate panel with sample size
  .annotate_panel <- function(n) {
    mtext(sprintf("n = %d", n), side = 3, line = 0.3, adj = 1,
          cex = 0.78, col = "gray40")
  }

  # Plot 1 -> 2
  if (nrow(lat1) > 0) {
    hist(lat1$latency_sec,
         breaks = brks,
         xlim   = c(0, window_sec),
         ylim   = c(0, y_plot_max),
         main   = paste(label1, "->", label2, "latency"),
         xlab   = "Latency (s)",
         col    = "gray80",
         border = "white")
    .annotate_panel(nrow(lat1))
  } else {
    plot(1, type = "n", xlim = c(0, window_sec), ylim = c(0, y_plot_max),
         main = paste(label1, "->", label2, "latency"),
         xlab = "Latency (s)", ylab = "Frequency")
    text(window_sec / 2, y_plot_max / 2, "No events found")
  }

  # Plot 2 -> 1
  if (nrow(lat2) > 0) {
    hist(lat2$latency_sec,
         breaks = brks,
         xlim   = c(0, window_sec),
         ylim   = c(0, y_plot_max),
         main   = paste(label2, "->", label1, "latency"),
         xlab   = "Latency (s)",
         col    = "gray80",
         border = "white")
    .annotate_panel(nrow(lat2))
  } else {
    plot(1, type = "n", xlim = c(0, window_sec), ylim = c(0, y_plot_max),
         main = paste(label2, "->", label1, "latency"),
         xlab = "Latency (s)", ylab = "Frequency")
    text(window_sec / 2, y_plot_max / 2, "No events found")
  }

  # Single bottom annotation spanning the full figure
  if (!is.null(bottom_note)) {
    mtext(bottom_note, side = 1, line = 0.8, outer = TRUE,
          cex = 0.75, col = "gray20", font = 3)
  }

  invisible(list(
    latencies_1_to_2 = lat1,
    latencies_2_to_1 = lat2,
    binom_test       = binom_test,
    ks_test          = ks_test,
    wilcox_test      = wilcox_test
  ))
}


# ── permutation_transition_test ───────────────────────────────────────────────

#' Permutation Test for Directional Transition Asymmetry
#'
#' @description
#' Tests whether \code{label1} -> \code{label2} transitions are more frequent
#' than \code{label2} -> \code{label1} by comparing the observed directional
#' asymmetry score (n1 - n2) against a null distribution generated by randomly
#' shuffling event labels within each session many times.  This controls for
#' both the base rates of each event type and the temporal structure of the
#' recording.
#'
#' @param data A data frame of vocalization events
#'   (e.g., \code{lys$vocalization_session_map}).
#' @param label1 Character. First label (the hypothesised trigger).
#' @param label2 Character. Second label (the hypothesised response).
#' @param window_sec Numeric. Matching window passed to
#'   \code{compute_onset_latency()}. Default \code{120}.
#' @param n_perm Integer. Number of permutation iterations. Default \code{1000}.
#' @param label_col Character. Column holding the event label.
#'   Default \code{"vocalization_label"}.
#' @param start_col Character. Column holding event start time.
#'   Default \code{"session_relative_start"}.
#' @param end_col Character. Column holding event end time.
#'   Default \code{"session_relative_end"}.
#' @param session_col Character. Grouping column for sessions.
#'   Default \code{"session_label"}.
#' @param require_adjacent Logical. Passed to \code{compute_onset_latency()}.
#'   Default \code{TRUE}.
#' @param seed Integer or \code{NULL}. Random seed for reproducibility.
#'   Default \code{42}.
#' @param plot Logical. If \code{TRUE} (default), draw a histogram of the null
#'   distribution with the observed statistic marked.
#'
#' @return Invisibly, a list with:
#' \describe{
#'   \item{\code{observed}}{Observed asymmetry score (n1 - n2).}
#'   \item{\code{null_distribution}}{Numeric vector of permuted scores.}
#'   \item{\code{p_value}}{One-sided p-value: proportion of permuted scores
#'     \eqn{\geq} observed.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- permutation_transition_test(
#'   data       = lys$vocalization_session_map,
#'   label1     = "BeggingCall",
#'   label2     = "SongBout",
#'   window_sec = 120,
#'   n_perm     = 2000
#' )
#' res$p_value
#' }
#' @export
permutation_transition_test <- function(data,
                                        label1,
                                        label2,
                                        window_sec       = 120,
                                        n_perm           = 1000L,
                                        label_col        = "vocalization_label",
                                        start_col        = "session_relative_start",
                                        end_col          = "session_relative_end",
                                        session_col      = "session_label",
                                        require_adjacent = TRUE,
                                        seed             = 42L,
                                        plot             = TRUE) {

  if (!is.null(seed)) set.seed(seed)

  # Helper: count matched pairs in one direction for a given data frame
  .count_pairs <- function(df, pre, fol) {
    nrow(compute_onset_latency(
      data             = df,
      preceding_label  = pre,
      following_label  = fol,
      label_col        = label_col,
      start_col        = start_col,
      end_col          = end_col,
      session_col      = session_col,
      window_sec       = window_sec,
      require_adjacent = require_adjacent
    ))
  }

  # Observed asymmetry
  obs_n1  <- .count_pairs(data, label1, label2)
  obs_n2  <- .count_pairs(data, label2, label1)
  obs_stat <- obs_n1 - obs_n2

  message(sprintf(
    "\nPermutation test: %s -> %s  vs  %s -> %s\n  Observed: n1 = %d, n2 = %d, asymmetry = %+d\n  Running %d permutations...",
    label1, label2, label2, label1, obs_n1, obs_n2, obs_stat, n_perm
  ))

  # Only shuffle labels that are label1 or label2; preserve timing & session
  target_rows <- data[[label_col]] %in% c(label1, label2)

  null_dist <- vapply(seq_len(n_perm), function(i) {
    perm_data                      <- data
    pool                           <- data[[label_col]][target_rows]
    perm_data[[label_col]][target_rows] <- sample(pool)
    .count_pairs(perm_data, label1, label2) -
      .count_pairs(perm_data, label2, label1)
  }, numeric(1L))

  p_val <- mean(null_dist >= obs_stat)

  fmt_p <- function(p) if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)
  message(sprintf("  p (one-sided) %s\n", fmt_p(p_val)))

  if (plot) {
    hist(null_dist,
         breaks = 30,
         col    = "gray80",
         border = "white",
         main   = sprintf("Permutation null: %s <-> %s", label1, label2),
         xlab   = "Asymmetry score (n1 - n2)",
         ylab   = "Count")
    abline(v   = obs_stat,
           col = if (p_val < 0.05) "firebrick" else "steelblue",
           lwd = 2, lty = 2)
    legend("topright",
           legend = sprintf("Observed = %+d\np %s", obs_stat, fmt_p(p_val)),
           bty    = "n", cex = 0.85,
           text.col = if (p_val < 0.05) "firebrick" else "steelblue")
  }

  invisible(list(
    observed         = obs_stat,
    null_distribution = null_dist,
    p_value          = p_val
  ))
}


# ── conditional_rate_ratio ────────────────────────────────────────────────────

#' Conditional Rate Ratio for Vocalization Transitions
#'
#' @description
#' Quantifies how much more (or less) likely \code{response_label} events are
#' to occur in a time window immediately following a \code{trigger_label} event
#' (the \emph{foreground rate}) compared with the background rate of
#' \code{response_label} outside those windows.  The ratio
#' \eqn{\lambda_{fg} / \lambda_{bg}} > 1 indicates that the trigger elevates
#' the response rate.  A 95\% confidence interval is obtained via
#' Poisson rate-ratio approximation.  Both the forward
#' (\code{trigger -> response}) and reverse (\code{response -> trigger})
#' directions are computed so that directional asymmetry can be assessed.
#'
#' @param data A data frame of vocalization events.
#' @param label1 Character. First label.
#' @param label2 Character. Second label.
#' @param window_sec Numeric. Width of the foreground window (seconds) after
#'   each trigger event. Default \code{120}.
#' @param label_col,start_col,end_col,session_col Column name arguments with
#'   the same defaults as \code{compute_onset_latency()}.
#'
#' @return Invisibly, a list with two elements (\code{label1_to_label2} and
#'   \code{label2_to_label1}), each a list containing:
#' \describe{
#'   \item{\code{n_triggers}}{Number of trigger events.}
#'   \item{\code{n_fg}}{Response events observed in foreground windows.}
#'   \item{\code{t_fg_sec}}{Total foreground exposure time (s).}
#'   \item{\code{n_bg}}{Response events observed outside foreground windows.}
#'   \item{\code{t_bg_sec}}{Total background exposure time (s).}
#'   \item{\code{rate_fg}}{Foreground rate (events / s).}
#'   \item{\code{rate_bg}}{Background rate (events / s).}
#'   \item{\code{rate_ratio}}{rate_fg / rate_bg.}
#'   \item{\code{ci_low,ci_high}}{95\% CI on the rate ratio.}
#'   \item{\code{p_value}}{Two-sided p-value (exact Poisson test).}
#' }
#'
#' @examples
#' \dontrun{
#' res <- conditional_rate_ratio(
#'   data       = lys$vocalization_session_map,
#'   label1     = "BeggingCall",
#'   label2     = "SongBout",
#'   window_sec = 120
#' )
#' res$label1_to_label2$rate_ratio
#' }
#' @export
conditional_rate_ratio <- function(data,
                                   label1,
                                   label2,
                                   window_sec  = 120,
                                   label_col   = "vocalization_label",
                                   start_col   = "session_relative_start",
                                   end_col     = "session_relative_end",
                                   session_col = "session_label") {

  # Internal helper: compute rate ratio for trigger -> response
  .crr_one_direction <- function(trigger, response) {
    sessions    <- unique(data[[session_col]])
    n_triggers  <- 0L
    n_fg        <- 0L
    t_fg_sec    <- 0
    n_bg        <- 0L
    t_bg_sec    <- 0

    for (sess in sessions) {
      sd <- data[data[[session_col]] == sess, , drop = FALSE]
      sd <- sd[order(sd[[start_col]]), , drop = FALSE]

      trig_rows <- which(sd[[label_col]] == trigger)
      resp_rows <- which(sd[[label_col]] == response)

      if (!length(trig_rows)) next

      sess_duration <- max(sd[[end_col]]) - min(sd[[start_col]])

      # Build foreground intervals: [trig_end, trig_end + window_sec]
      # Clipped to session bounds
      sess_end <- max(sd[[end_col]])
      fg_intervals <- lapply(trig_rows, function(ti) {
        t0 <- sd[[end_col]][ti]
        t1 <- min(t0 + window_sec, sess_end)
        c(t0, t1)
      })

      # Merge overlapping foreground intervals
      fg_mat    <- do.call(rbind, fg_intervals)
      fg_mat    <- fg_mat[order(fg_mat[, 1]), , drop = FALSE]
      merged    <- list()
      cur       <- fg_mat[1, ]
      for (k in seq_len(nrow(fg_mat))[-1]) {
        if (fg_mat[k, 1] <= cur[2]) {
          cur[2] <- max(cur[2], fg_mat[k, 2])
        } else {
          merged <- c(merged, list(cur))
          cur    <- fg_mat[k, ]
        }
      }
      merged <- c(merged, list(cur))
      fg_mat <- do.call(rbind, merged)

      t_fg_sess <- sum(fg_mat[, 2] - fg_mat[, 1])

      # Classify each response event as foreground or background
      for (ri in resp_rows) {
        t_resp <- sd[[start_col]][ri]
        in_fg  <- any(t_resp >= fg_mat[, 1] & t_resp <= fg_mat[, 2])
        if (in_fg) {
          n_fg <- n_fg + 1L
        } else {
          n_bg <- n_bg + 1L
        }
      }

      n_triggers <- n_triggers + length(trig_rows)
      t_fg_sec   <- t_fg_sec + t_fg_sess
      t_bg_sec   <- t_bg_sec + (sess_duration - t_fg_sess)
    }

    rate_fg <- if (t_fg_sec > 0) n_fg / t_fg_sec else NA_real_
    rate_bg <- if (t_bg_sec > 0) n_bg / t_bg_sec else NA_real_

    # Exact Poisson rate-ratio test (poisson.test compares two Poisson counts
    # given their exposure times)
    ptest <- tryCatch(
      poisson.test(c(n_fg, n_bg), c(t_fg_sec, t_bg_sec)),
      error = function(e) NULL
    )

    rr     <- if (!is.null(ptest)) ptest$estimate   else rate_fg / rate_bg
    ci_low <- if (!is.null(ptest)) ptest$conf.int[1] else NA_real_
    ci_hi  <- if (!is.null(ptest)) ptest$conf.int[2] else NA_real_
    pval   <- if (!is.null(ptest)) ptest$p.value      else NA_real_

    list(
      n_triggers = n_triggers,
      n_fg       = n_fg,
      t_fg_sec   = t_fg_sec,
      n_bg       = n_bg,
      t_bg_sec   = t_bg_sec,
      rate_fg    = rate_fg,
      rate_bg    = rate_bg,
      rate_ratio = unname(rr),
      ci_low     = unname(ci_low),
      ci_high    = unname(ci_hi),
      p_value    = pval
    )
  }

  res1 <- .crr_one_direction(label1, label2)
  res2 <- .crr_one_direction(label2, label1)

  fmt_p  <- function(p) if (is.na(p)) "NA" else if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)
  fmt_rr <- function(r, lo, hi) {
    if (any(is.na(c(r, lo, hi)))) return("NA")
    sprintf("%.2f  [95%% CI: %.2f, %.2f]", r, lo, hi)
  }

  message(sprintf(
    "\nConditional rate ratio: %s <-> %s  (window = %g s)\n",
    label1, label2, window_sec
  ))
  for (d in list(list(res1, label1, label2), list(res2, label2, label1))) {
    r <- d[[1]]; trig <- d[[2]]; resp <- d[[3]]
    message(sprintf(
      "  %s -> %s\n    triggers = %d | fg: %d events / %.1f s = %.4f ev/s | bg: %d events / %.1f s = %.4f ev/s\n    Rate ratio = %s  p %s\n",
      trig, resp,
      r$n_triggers,
      r$n_fg, r$t_fg_sec, r$rate_fg,
      r$n_bg, r$t_bg_sec, r$rate_bg,
      fmt_rr(r$rate_ratio, r$ci_low, r$ci_high),
      fmt_p(r$p_value)
    ))
  }

  invisible(list(
    label1_to_label2 = res1,
    label2_to_label1 = res2
  ))
}
