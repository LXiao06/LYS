# Population-level analysis helpers
# Update date : Aug. 28, 2026

# Pool LYS Session Maps ----

#' Pool vocalization session maps from multiple animals
#'
#' @description
#' Collects \code{vocalization_session_map} data frames from several LYS
#' objects and row-binds them into a single \emph{population-level} data frame
#' that can be passed directly to \code{\link{compute_onset_latency}},
#' \code{\link{plot_reciprocal_latency}},
#' \code{\link{permutation_transition_test}}, and
#' \code{\link{conditional_rate_ratio}}.
#'
#' Sessions are kept independent across animals: the \code{session_col} values
#' are prefixed with \code{<bird_id>::} so that e.g. \code{"Session_1"} from
#' bird A and \code{"Session_1"} from bird B are treated as separate sessions.
#' A \code{bird_id} column is also added so results can be grouped or
#' stratified by individual.
#'
#' @section Inputs - two ways to supply data:
#'
#' **Option A - named list of already-loaded lys objects:**
#' \preformatted{
#' pool <- pool_lys_session_maps(
#'   lys_list = list(bird_A = lys_bird_A, bird_B = lys_bird_B)
#' )
#' }
#'
#' **Option B - a folder of \code{.rds} files + a bird name filter:**
#' \preformatted{
#' pool <- pool_lys_session_maps(
#'   rds_dir   = "/project/lys_objects",
#'   bird_list = c("O703", "O704")   # must match RDS filenames without .rds
#' )
#' }
#' RDS files must be named \code{<bird_id>.rds} (e.g. \code{O703.rds}).
#' Alternatively, supply \code{bird_list = NULL} to load every \code{.rds}
#' in the folder.
#'
#' @param lys_list Named list of \code{lys} objects.  Names are used as
#'   \code{bird_id}s.  Supply either \code{lys_list} or
#'   (\code{rds_dir} + optionally \code{bird_list}), not both.
#' @param rds_dir Character. Path to a folder containing \code{.rds} files,
#'   one per bird, named \code{<bird_id>.rds}.
#' @param bird_list Character vector of bird IDs to load.  \code{NULL} (default)
#'   loads all \code{.rds} files found in \code{rds_dir}.
#' @param session_col Character. Name of the column holding the session
#'   identifier.  Default \code{"session_label"}.
#' @param verbose Logical.  Print a loading/pooling summary.
#'   Default \code{TRUE}.
#'
#' @return A data frame with the same columns as
#'   \code{lys$vocalization_session_map} plus:
#' \describe{
#'   \item{\code{bird_id}}{Character identifier for the source animal.}
#'   \item{\code{session_label}}{Original session label prefixed with
#'     \code{"<bird_id>::"} to ensure global uniqueness.}
#' }
#'
#' @examples
#' \dontrun{
#' # Option A - named list
#' pool <- pool_lys_session_maps(
#'   lys_list = list(O703 = lys_O703, O704 = lys_O704)
#' )
#'
#' # Option B - folder of RDS files
#' pool <- pool_lys_session_maps(
#'   rds_dir   = "/project/lys_objects",
#'   bird_list = c("O703", "O704")
#' )
#'
#' # Run population-level analysis exactly as you would for one bird
#' latencies <- compute_onset_latency(
#'   data            = pool,
#'   preceding_label = "SongBout",
#'   following_label = "BeggingCall",
#'   window_sec      = 120
#' )
#'
#' res <- plot_reciprocal_latency(pool, "BeggingCall", "SongBout",
#'                                window_sec = 120, breaks = 10)
#'
#' perm <- permutation_transition_test(pool, "BeggingCall", "SongBout",
#'                                     window_sec = 120)
#' }
#'
#' @export
pool_lys_session_maps <- function(lys_list  = NULL,
                                   rds_dir   = NULL,
                                   bird_list = NULL,
                                   session_col = "session_label",
                                   verbose   = TRUE) {

  # ---- resolve lys_list from disk if needed --------------------------------
  if (!is.null(lys_list) && !is.null(rds_dir)) {
    stop("Supply either 'lys_list' or 'rds_dir', not both.", call. = FALSE)
  }

  if (!is.null(rds_dir)) {
    if (!is.character(rds_dir) || !nzchar(rds_dir)) {
      stop("'rds_dir' must be a non-empty path string.", call. = FALSE)
    }
    if (!dir.exists(rds_dir)) {
      stop(sprintf("'rds_dir' does not exist: %s", rds_dir), call. = FALSE)
    }

    all_rds <- list.files(rds_dir, pattern = "\\.rds$", full.names = TRUE,
                          ignore.case = TRUE)
    if (!length(all_rds)) {
      stop(sprintf("No .rds files found in: %s", rds_dir), call. = FALSE)
    }

    rds_ids <- tools::file_path_sans_ext(basename(all_rds))

    if (!is.null(bird_list)) {
      # For each requested bird ID, find the best-matching .rds file stem.
      # Strategy (in priority order):
      #   1. Exact match              ("G769"    matches "G769.rds")
      #   2. Prefix match             ("lys_G769" would match "G769" if user
      #                                typed the full stem - already exact)
      #   3. Suffix / substring match ("G769" matches "lys_G769.rds")
      # The user's bird_list entry is always used as the bird_id in the output,
      # regardless of the actual filename.
      matched_files  <- character(length(bird_list))
      matched_ids    <- character(length(bird_list))   # file stems for messages
      not_found      <- character(0)

      for (bi in seq_along(bird_list)) {
        bid <- bird_list[bi]

        # 1. Exact
        idx <- which(rds_ids == bid)

        # 2. Suffix: rds_id ends with "_<bid>" (e.g. "lys_G769" matches "G769")
        if (!length(idx)) {
          idx <- which(endsWith(rds_ids, paste0("_", bid)))
        }

        # 3. Prefix: rds_id starts with "<bid>_"  (e.g. "G769_run1" matches "G769")
        if (!length(idx)) {
          idx <- which(startsWith(rds_ids, paste0(bid, "_")))
        }

        if (!length(idx)) {
          not_found <- c(not_found, bid)
        } else {
          if (length(idx) > 1L) {
            warning(sprintf(
              "Bird ID '%s' matches multiple files (%s); using the first one.",
              bid, paste(rds_ids[idx], collapse = ", ")
            ), call. = FALSE)
          }
          matched_files[bi] <- all_rds[idx[1]]
          matched_ids[bi]   <- rds_ids[idx[1]]
        }
      }

      if (length(not_found) == length(bird_list)) {
        stop(
          sprintf(
            "None of the requested bird IDs (%s) match .rds files in %s.\nAvailable: %s",
            paste(bird_list, collapse = ", "), rds_dir,
            paste(rds_ids, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      if (length(not_found)) {
        warning(sprintf("No .rds file found for: %s",
                        paste(not_found, collapse = ", ")),
                call. = FALSE)
      }

      # Keep only matched entries; preserve user's bird_list as names
      keep        <- nzchar(matched_files)
      all_rds     <- matched_files[keep]
      rds_ids     <- bird_list[keep]   # use user-supplied IDs as bird labels
    }

    if (verbose) {
      message(sprintf("Loading %d lys object(s) from: %s", length(all_rds), rds_dir))
    }

    lys_list <- setNames(
      lapply(seq_along(all_rds), function(i) {
        obj <- readRDS(all_rds[i])
        if (!inherits(obj, "lys")) {
          stop(sprintf("File does not contain a lys object: %s", all_rds[i]),
               call. = FALSE)
        }
        if (verbose) message(sprintf("  Loaded: %s", rds_ids[i]))
        obj
      }),
      rds_ids
    )
  }

  # ---- validate lys_list ---------------------------------------------------
  if (is.null(lys_list) || !length(lys_list)) {
    stop("Supply 'lys_list' (named list of lys objects) or 'rds_dir'.",
         call. = FALSE)
  }
  if (is.null(names(lys_list)) || any(!nzchar(names(lys_list)))) {
    stop("'lys_list' must be a *named* list; names are used as bird IDs.",
         call. = FALSE)
  }
  for (nm in names(lys_list)) {
    if (!inherits(lys_list[[nm]], "lys")) {
      stop(sprintf("lys_list[[\"%s\"]] is not a lys object.", nm), call. = FALSE)
    }
  }

  # ---- extract and pool session maps ---------------------------------------
  parts <- lapply(names(lys_list), function(bird_id) {
    obj <- lys_list[[bird_id]]

    map_df <- obj$vocalization_session_map
    if (is.null(map_df) || !is.data.frame(map_df) || !nrow(map_df)) {
      warning(
        sprintf("Bird '%s': lys$vocalization_session_map is empty or missing. ",
                bird_id),
        "Run map_vocalization_sessions() on this bird first. Skipping.",
        call. = FALSE
      )
      return(NULL)
    }

    map_df <- as.data.frame(map_df, stringsAsFactors = FALSE)

    # Add bird_id column (prepend so it is the first column)
    map_df <- cbind(bird_id = bird_id, map_df, stringsAsFactors = FALSE)

    # Make session labels globally unique by prefixing with bird_id
    if (session_col %in% names(map_df)) {
      map_df[[session_col]] <- paste0(bird_id, "::", map_df[[session_col]])
    } else {
      warning(
        sprintf("Bird '%s': session column '%s' not found in session map; ",
                bird_id, session_col),
        "sessions will not be prefixed.",
        call. = FALSE
      )
    }

    map_df
  })

  parts <- Filter(Negate(is.null), parts)

  if (!length(parts)) {
    stop("No valid session maps found across supplied animals.", call. = FALSE)
  }

  pool <- do.call(rbind, parts)
  rownames(pool) <- NULL

  if (verbose) {
    n_birds    <- length(parts)
    n_sessions <- length(unique(
      if (session_col %in% names(pool)) pool[[session_col]] else character(0)
    ))
    n_events   <- nrow(pool)
    label_tbl  <- sort(table(pool$vocalization_label), decreasing = TRUE)
    label_str  <- paste(
      sprintf("%s (%d)", names(label_tbl), as.integer(label_tbl)),
      collapse = ", "
    )
    message(sprintf(
      "\nPooled %d bird(s) | %d session(s) | %d event(s)\nLabel counts: %s",
      n_birds, n_sessions, n_events, label_str
    ))
  }

  pool
}


# ---------------------------------------------------------------------------
# Summarize Per-Bird Latency ----
# ---------------------------------------------------------------------------

#' Compute per-animal latency and transition statistics
#'
#' @description
#' For each animal in a pooled session-map data frame (created by
#' \code{\link{pool_lys_session_maps}}), computes the number of matched pairs
#' in each direction, the directional asymmetry score, the proportion of
#' transitions in the \code{label1 -> label2} direction, and median / mean
#' onset latencies.  The result is the \emph{unit-of-analysis} for all
#' population-level tests - each row is one animal.
#'
#' @param pool Data frame returned by \code{\link{pool_lys_session_maps}}.
#' @param label1,label2 Character. The two vocalization labels to compare.
#' @param window_sec Numeric. Matching window passed to
#'   \code{\link{compute_onset_latency}}. Default \code{120}.
#' @param label_col,start_col,end_col,session_col Column name arguments with
#'   the same defaults as \code{\link{compute_onset_latency}}.
#' @param require_adjacent Logical. Passed to
#'   \code{\link{compute_onset_latency}}. Default \code{TRUE}.
#'
#' @return A data frame with one row per animal:
#' \describe{
#'   \item{\code{bird_id}}{Animal identifier.}
#'   \item{\code{n_1to2}}{Matched pairs in the \code{label1 -> label2}
#'     direction.}
#'   \item{\code{n_2to1}}{Matched pairs in the \code{label2 -> label1}
#'     direction.}
#'   \item{\code{n_total}}{Total matched pairs (\code{n_1to2 + n_2to1}).}
#'   \item{\code{asymmetry}}{Directional asymmetry score
#'     (\code{n_1to2 - n_2to1}); positive = more \code{label1->label2}.}
#'   \item{\code{prop_1to2}}{Proportion of pairs in the \code{label1->label2}
#'     direction (\code{NA} if no pairs found).}
#'   \item{\code{median_lat_1to2},\code{mean_lat_1to2}}{Median and mean onset
#'     latency (s) for the \code{label1->label2} direction (\code{NA} if no
#'     pairs).}
#'   \item{\code{median_lat_2to1},\code{mean_lat_2to1}}{Same for the
#'     \code{label2->label1} direction.}
#' }
#'
#' @examples
#' \dontrun{
#' pool <- pool_lys_session_maps(lys_list = list(O703 = lys_O703, O704 = lys_O704))
#' per_bird <- summarize_per_bird_latency(pool, "BeggingCall", "SongBout", window_sec = 120)
#' print(per_bird)
#' }
#'
#' @keywords internal
#' @noRd
summarize_per_bird_latency <- function(pool,
                                     label1,
                                     label2,
                                     window_sec       = 120,
                                     label_col        = "vocalization_label",
                                     start_col        = "session_relative_start",
                                     end_col          = "session_relative_end",
                                     session_col      = "session_label",
                                     require_adjacent = TRUE) {

  if (!"bird_id" %in% names(pool)) {
    stop(
      "'pool' must contain a 'bird_id' column. ",
      "Create it with pool_lys_session_maps().",
      call. = FALSE
    )
  }

  birds <- unique(pool$bird_id)

  rows <- lapply(birds, function(bid) {
    bird_data <- pool[pool$bird_id == bid, , drop = FALSE]

    lat1 <- compute_onset_latency(
      data             = bird_data,
      preceding_label  = label1,
      following_label  = label2,
      label_col        = label_col,
      start_col        = start_col,
      end_col          = end_col,
      session_col      = session_col,
      window_sec       = window_sec,
      require_adjacent = require_adjacent
    )
    lat2 <- compute_onset_latency(
      data             = bird_data,
      preceding_label  = label2,
      following_label  = label1,
      label_col        = label_col,
      start_col        = start_col,
      end_col          = end_col,
      session_col      = session_col,
      window_sec       = window_sec,
      require_adjacent = require_adjacent
    )

    n1    <- nrow(lat1)
    n2    <- nrow(lat2)
    ntot  <- n1 + n2

    data.frame(
      bird_id          = bid,
      n_1to2           = n1,
      n_2to1           = n2,
      n_total          = ntot,
      asymmetry        = n1 - n2,
      prop_1to2        = if (ntot > 0L) n1 / ntot else NA_real_,
      median_lat_1to2  = if (n1 > 0L) stats::median(lat1$latency_sec) else NA_real_,
      mean_lat_1to2    = if (n1 > 0L) mean(lat1$latency_sec)           else NA_real_,
      median_lat_2to1  = if (n2 > 0L) stats::median(lat2$latency_sec) else NA_real_,
      mean_lat_2to1    = if (n2 > 0L) mean(lat2$latency_sec)           else NA_real_,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' Format a P Value
#'
#' @description
#' Internal helper used by population-level analysis functions.
#'
#' @param p Numeric p value
#'
#' @keywords internal
#' @noRd
format_p_value <- function(p) {
  if (is.null(p) || is.na(p)) return("NA")
  if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)
}


#' Format a Rate Ratio
#'
#' @description
#' Internal helper used by \code{population_rate_ratio()}.
#'
#' @param ratio Numeric rate ratio
#' @param lower Numeric lower confidence limit
#' @param upper Numeric upper confidence limit
#'
#' @keywords internal
#' @noRd
format_rate_ratio <- function(ratio, lower, upper) {
  if (any(is.na(c(ratio, lower, upper)))) return("NA")
  sprintf("%.2f [95%% CI: %.2f, %.2f]", ratio, lower, upper)
}


#' Format an Odds Ratio
#'
#' @description
#' Internal helper used by \code{population_point_process_glm()}.
#'
#' @param ratio Numeric odds ratio
#' @param lower Numeric lower confidence limit
#' @param upper Numeric upper confidence limit
#'
#' @keywords internal
#' @noRd
format_odds_ratio <- function(ratio, lower, upper) {
  sprintf("%.2fx [%.2f, %.2f]", ratio, lower, upper)
}


# Population Transition Test ----

#' Population-level test for directional transition asymmetry
#'
#' @description
#' Tests whether the \code{label1 -> label2} direction is consistently more
#' frequent than \code{label2 -> label1} \emph{across animals}, using the
#' animal as the unit of replication.  This avoids the two main pitfalls of
#' naive pooling:
#'
#' \enumerate{
#'   \item \strong{Unequal weight} - animals with more events would dominate
#'     naive pooling.  Here every animal contributes exactly one data point
#'     (its asymmetry score or proportion).
#'   \item \strong{Heterogeneity blindness} - a single animal driving the
#'     effect is visible in the per-animal plot and would not survive a
#'     sign test or Wilcoxon signed-rank test.
#' }
#'
#' Three complementary tests are reported:
#' \itemize{
#'   \item \strong{Sign test (binomial)}: across \eqn{N} animals, how many
#'     show \code{n_1to2 > n_2to1}?  Binomial test against \eqn{p = 0.5}.
#'     Only animals with at least one pair in either direction are counted.
#'   \item \strong{Wilcoxon signed-rank test}: on per-animal
#'     \code{prop_1to2} values against the null value of 0.5.  This uses
#'     both the sign and the magnitude of each animal's bias.
#'   \item \strong{One-sample t-test} (if \eqn{N \ge 3}): on
#'     \code{prop_1to2} against 0.5.  Sensitive when proportions are
#'     approximately normal.
#' }
#'
#' @param pool Data frame from \code{\link{pool_lys_session_maps}}.
#' @param label1 Character. First (hypothesised trigger) label.
#' @param label2 Character. Second (hypothesised response) label.
#' @param window_sec Numeric. Matching window (s). Default \code{120}.
#' @param label_col,start_col,end_col,session_col Column names passed to
#'   \code{\link{compute_onset_latency}}.
#' @param require_adjacent Logical. Default \code{TRUE}.
#' @param plot Logical. Draw the per-animal dot plot. Default \code{TRUE}.
#' @param verbose Logical. Print a summary table and test results.
#'   Default \code{TRUE}.
#'
#' @return Invisibly, a list with:
#' \describe{
#'   \item{\code{per_bird}}{Per-animal transition counts, asymmetries,
#'   proportions, and latency summaries.}
#'   \item{\code{sign_test}}{Result of \code{binom.test()}.}
#'   \item{\code{wilcox_test}}{Result of \code{wilcox.test()}.}
#'   \item{\code{t_test}}{Result of \code{t.test()}, or \code{NULL} if
#'     \eqn{N < 3}.}
#'   \item{\code{n_birds_with_pairs}}{Number of animals contributing to tests.}
#' }
#'
#' @examples
#' \dontrun{
#' pool <- pool_lys_session_maps(lys_list = list(O703 = lys_O703, O704 = lys_O704))
#' res  <- population_transition_test(pool, "BeggingCall", "SongBout", window_sec = 120)
#' res$sign_test
#' res$wilcox_test
#' }
#'
#' @export
population_transition_test <- function(pool,
                                       label1,
                                       label2,
                                       window_sec       = 120,
                                       label_col        = "vocalization_label",
                                       start_col        = "session_relative_start",
                                       end_col          = "session_relative_end",
                                       session_col      = "session_label",
                                       require_adjacent = TRUE,
                                       plot             = TRUE,
                                       verbose          = TRUE) {

  pb <- summarize_per_bird_latency(
    pool             = pool,
    label1           = label1,
    label2           = label2,
    window_sec       = window_sec,
    label_col        = label_col,
    start_col        = start_col,
    end_col          = end_col,
    session_col      = session_col,
    require_adjacent = require_adjacent
  )

  # Animals that have at least one pair in either direction
  active <- pb[!is.na(pb$prop_1to2) & pb$n_total > 0L, , drop = FALSE]
  N      <- nrow(active)

  if (N == 0L) {
    warning("No animals had any matched pairs - cannot run population test.",
            call. = FALSE)
    return(invisible(list(per_bird = pb, sign_test = NULL,
                          wilcox_test = NULL, t_test = NULL,
                          n_birds_with_pairs = 0L)))
  }

  n_favour <- sum(active$asymmetry > 0L)  # birds where label1->label2 > label2->label1
  n_tie    <- sum(active$asymmetry == 0L)
  n_active <- N - n_tie   # animals with a clear preference (no tie)

  # ---- Statistical tests (animal as unit) ----------------------------------
  # 1. Unweighted tests
  sign_test <- if (n_active > 0L) {
    stats::binom.test(n_favour, n_active, p = 0.5, alternative = "two.sided")
  } else {
    warning("All animals are tied (asymmetry = 0) - sign test not computable.",
            call. = FALSE)
    NULL
  }
  wilcox_test <- tryCatch(
    stats::wilcox.test(active$prop_1to2, mu = 0.5,
                       alternative = "two.sided", exact = FALSE),
    error = function(e) NULL
  )
  t_test <- if (N >= 3L) {
    tryCatch(
      stats::t.test(active$prop_1to2, mu = 0.5, alternative = "two.sided"),
      error = function(e) NULL
    )
  } else NULL

  # 2. Sample-size / precision-weighted tests (weights = n_total)
  # Animals with more pairs have higher statistical precision and weight
  weighted_mean_prop <- sum(active$n_1to2) / sum(active$n_total)
  weighted_t_test <- if (N >= 3L) {
    tryCatch({
      fit_w <- stats::lm(prop_1to2 ~ 1, data = active, weights = n_total)
      coef_summary <- summary(fit_w)$coefficients
      se_w   <- coef_summary[1, "Std. Error"]
      t_stat <- (coef_summary[1, "Estimate"] - 0.5) / se_w
      p_w    <- 2 * (1 - stats::pt(abs(t_stat), df = fit_w$df.residual))
      list(
        estimate = coef_summary[1, "Estimate"],
        std_error = se_w,
        statistic = t_stat,
        p_value  = p_w,
        df       = fit_w$df.residual
      )
    }, error = function(e) NULL)
  } else NULL

  # 3. Quasibinomial GLM (binomial counts per bird accounting for between-animal variance)
  quasibinomial_glm <- tryCatch({
    glm_fit <- stats::glm(
      cbind(n_1to2, n_2to1) ~ 1,
      data = active,
      family = stats::quasibinomial()
    )
    s_glm <- summary(glm_fit)
    list(
      coefficients = s_glm$coefficients,
      dispersion   = s_glm$dispersion,
      p_value      = s_glm$coefficients[1, 4]
    )
  }, error = function(e) NULL)

  if (verbose) {
    message(sprintf(
      "\nPopulation transition test: %s <-> %s  (window = %g s, N = %d animals)\n",
      label1, label2, window_sec, N
    ))
    message(sprintf("  %-12s  %8s  %8s  %8s  %8s  %8s  %8s",
                    "bird_id", "n_1to2", "n_2to1", "n_total", "asym", "prop_1to2", "weight"))
    message(paste(rep("-", 75), collapse = ""))
    tot_weight <- sum(active$n_total)
    for (i in seq_len(nrow(active))) {
      r   <- active[i, ]
      w_pct <- if (tot_weight > 0) 100 * r$n_total / tot_weight else 0
      tag <- if (r$asymmetry > 0) sprintf("<- favours %s->%s", label1, label2) else
             if (r$asymmetry < 0) sprintf("<- favours %s->%s", label2, label1) else "  tie"
      message(sprintf("  %-12s  %8d  %8d  %8d  %+8d  %8.3f  %6.1f%%  %s",
                      r$bird_id, r$n_1to2, r$n_2to1, r$n_total, r$asymmetry,
                      r$prop_1to2, w_pct, tag))
    }
    message(paste(rep("-", 75), collapse = ""))
    message(sprintf("  Pooled totals :  %8d  %8d  %8d  %+8d  %8.3f  100.0%%",
                    sum(active$n_1to2), sum(active$n_2to1), sum(active$n_total),
                    sum(active$asymmetry), weighted_mean_prop))
    message(sprintf("\n  %d/%d animals favour %s -> %s", n_favour, N, label1, label2))
    message(sprintf("  Unweighted mean prop: %.3f  |  Sample-weighted mean prop: %.3f\n",
                    mean(active$prop_1to2), weighted_mean_prop))
    message("  [Unweighted Animal-Level Tests] (treats each animal equally):")
    message(sprintf("    Sign test         : p %s", format_p_value(if (!is.null(sign_test)) sign_test$p.value else NA)))
    message(sprintf("    Wilcoxon signed-rank (prop vs 0.5): p %s",
                    format_p_value(if (!is.null(wilcox_test)) wilcox_test$p.value else NA)))
    if (!is.null(t_test))
      message(sprintf("    One-sample t-test (prop vs 0.5)   : p %s",
                      format_p_value(t_test$p.value)))
    message("\n  [Sample-Size Weighted Tests] (weights animals by number of pairs):")
    if (!is.null(weighted_t_test))
      message(sprintf("    Weighted t-test   (prop vs 0.5)   : p %s (mean = %.3f, SE = %.3f)",
                      format_p_value(weighted_t_test$p_value), weighted_t_test$estimate, weighted_t_test$std_error))
    if (!is.null(quasibinomial_glm))
      message(sprintf("    Quasibinomial GLM (accounting for overdispersion): p %s",
                      format_p_value(quasibinomial_glm$p_value)))
  }

  # ---- Per-animal dot plot -------------------------------------------------
  if (plot && N > 0L) {
    pb_sorted  <- active[order(active$asymmetry), , drop = FALSE]
    cols       <- ifelse(pb_sorted$asymmetry > 0, "#1976D2",
                  ifelse(pb_sorted$asymmetry < 0, "#D32F2F", "gray60"))

    old_par <- graphics::par(mar = c(5, 7, 4, 2) + 0.1)
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::dotchart(
      pb_sorted$asymmetry,
      labels = pb_sorted$bird_id,
      pch    = 19,
      color  = cols,
      xlab   = sprintf("Asymmetry score (n [%s->%s] minus n [%s->%s])",
                       label1, label2, label2, label1),
      main   = sprintf("Per-animal transition asymmetry\n%s <-> %s  (N = %d)",
                       label1, label2, N),
      cex    = 0.9
    )
    graphics::abline(v = 0, lty = 2, col = "gray50")

    # Population mean line
    graphics::abline(v = mean(active$asymmetry), lty = 1,
                     col = "black", lwd = 2)

    # Annotate with sign-test p and weighted p
    sign_p_str <- if (!is.null(sign_test)) format_p_value(sign_test$p.value) else "NA"
    w_p_str    <- if (!is.null(weighted_t_test)) format_p_value(weighted_t_test$p_value) else "NA"
    graphics::mtext(
      sprintf("Sign test p %s  |  Weighted t-test p %s",
              sign_p_str, w_p_str),
      side = 1, line = 3.5, cex = 0.8, col = "gray20", font = 3
    )
  }

  invisible(list(
    per_bird           = pb,
    sign_test          = sign_test,
    wilcox_test        = wilcox_test,
    t_test             = t_test,
    weighted_t_test    = weighted_t_test,
    quasibinomial_glm  = quasibinomial_glm,
    weighted_mean_prop = weighted_mean_prop,
    n_birds_with_pairs = N
  ))
}


# Population Permutation Test ----

#' Animal-respecting permutation test for population-level transition asymmetry
#'
#' @description
#' An animal-level permutation test that avoids the sample-size and
#' heterogeneity problems of naive pooling.  At each permutation iteration,
#' labels are shuffled \emph{independently within each animal}, and the
#' \emph{mean per-animal asymmetry} across all animals is used as the test
#' statistic.  This:
#'
#' \itemize{
#'   \item Gives every animal \emph{equal weight} regardless of how many
#'     events it contributed.
#'   \item Preserves the timing structure and event rates within each
#'     animal.
#'   \item Produces a null distribution that reflects what would happen if
#'     labels were exchangeable within animals.
#' }
#'
#' @param pool Data frame from \code{\link{pool_lys_session_maps}}.
#' @param label1 Character. First (hypothesised trigger) label.
#' @param label2 Character. Second (hypothesised response) label.
#' @param window_sec Numeric. Matching window (s). Default \code{120}.
#' @param n_perm Integer. Number of permutation iterations. Default \code{1000}.
#' @param label_col,start_col,end_col,session_col Column names passed to
#'   \code{\link{compute_onset_latency}}.
#' @param require_adjacent Logical. Default \code{TRUE}.
#' @param seed Integer or \code{NULL}. Random seed. Default \code{42}.
#' @param plot Logical. Draw the null-distribution histogram. Default \code{TRUE}.
#' @param verbose Logical. Print observed statistic and p-value. Default \code{TRUE}.
#'
#' @return Invisibly, a list with:
#' \describe{
#'   \item{\code{observed_mean_asymmetry}}{Mean per-animal asymmetry score
#'     in the observed data.}
#'   \item{\code{per_bird}}{Per-animal transition counts, asymmetries,
#'     proportions, and latency summaries.}
#'   \item{\code{null_distribution}}{Numeric vector of permuted mean-asymmetry
#'     scores.}
#'   \item{\code{p_value}}{One-sided p-value: proportion of permuted scores
#'     \eqn{\geq} observed.}
#' }
#'
#' @examples
#' \dontrun{
#' pool <- pool_lys_session_maps(lys_list = list(O703 = lys_O703, O704 = lys_O704))
#' perm <- population_permutation_test(pool, "BeggingCall", "SongBout",
#'                                     window_sec = 120, n_perm = 2000)
#' perm$p_value
#' }
#'
#' @export
population_permutation_test <- function(pool,
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
                                        plot             = TRUE,
                                        verbose          = TRUE) {

  if (!"bird_id" %in% names(pool)) {
    stop("'pool' must contain a 'bird_id' column created by pool_lys_session_maps().",
         call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  birds <- unique(pool$bird_id)

  # TODO: extract to standalone helper
  # Calculate mean per-animal asymmetry for a possibly permuted pool
  mean_asymmetry <- function(df) {
    scores <- vapply(birds, function(bid) {
      bd  <- df[df$bird_id == bid, , drop = FALSE]
      n1  <- nrow(compute_onset_latency(
        bd, label1, label2,
        label_col = label_col, start_col = start_col, end_col = end_col,
        session_col = session_col, window_sec = window_sec,
        require_adjacent = require_adjacent
      ))
      n2  <- nrow(compute_onset_latency(
        bd, label2, label1,
        label_col = label_col, start_col = start_col, end_col = end_col,
        session_col = session_col, window_sec = window_sec,
        require_adjacent = require_adjacent
      ))
      as.numeric(n1 - n2)
    }, numeric(1L))
    mean(scores)
  }

  obs_stat <- mean_asymmetry(pool)

  # Identify rows belonging to the two target labels (per animal)
  target_mask <- pool[[label_col]] %in% c(label1, label2)

  if (verbose) {
    message(sprintf(
      "\nAnimal-respecting permutation test: %s <-> %s  (N = %d animals, %d perm)",
      label1, label2, length(birds), n_perm
    ))
    message(sprintf("  Observed mean per-animal asymmetry: %+.3f", obs_stat))
    message(sprintf("  Running %d permutations (labels shuffled within each animal)...",
                    n_perm))
  }

  null_dist <- vapply(seq_len(n_perm), function(i) {
    perm_pool <- pool
    # Shuffle labels within each animal independently
    for (bid in birds) {
      idx  <- which(pool$bird_id == bid & target_mask)
      if (length(idx) < 2L) next
      perm_pool[[label_col]][idx] <- sample(pool[[label_col]][idx])
    }
    mean_asymmetry(perm_pool)
  }, numeric(1L))

  p_val <- mean(null_dist >= obs_stat)

  if (verbose) {
    message(sprintf("  p (one-sided) %s", format_p_value(p_val)))
  }

  if (plot) {
    sig_col <- if (p_val < 0.05) "firebrick" else "steelblue"
    graphics::hist(
      null_dist,
      breaks = 30,
      col    = "gray80",
      border = "white",
      main   = sprintf("Animal-respecting null: %s <-> %s\n(mean per-animal asymmetry, N = %d animals)",
                       label1, label2, length(birds)),
      xlab   = "Mean per-animal asymmetry score (n1 - n2)",
      ylab   = "Count"
    )
    graphics::abline(v = obs_stat, col = sig_col, lwd = 2, lty = 2)
    graphics::legend(
      "topright",
      legend   = sprintf("Observed = %+.2f\np %s", obs_stat, format_p_value(p_val)),
      bty      = "n", cex = 0.85, text.col = sig_col
    )
  }

  invisible(list(
    observed_mean_asymmetry = obs_stat,
    per_bird                = summarize_per_bird_latency(
      pool, label1, label2, window_sec, label_col,
      start_col, end_col, session_col, require_adjacent
    ),
    null_distribution       = null_dist,
    p_value                 = p_val
  ))
}


# Population Rate Ratio ----

#' Population-level Conditional Rate Ratio Analysis
#'
#' @description
#' Evaluates whether \code{response_label} vocalizations occur at a higher rate
#' in time windows immediately following \code{trigger_label} events compared to
#' baseline background periods across multiple animals.
#'
#' For each animal, the conditional rate ratio
#' \eqn{\text{RR} = \lambda_{\text{fg}} / \lambda_{\text{bg}}} is calculated
#' along with its 95\% Poisson confidence interval in both directions
#' (\code{label1 -> label2} and \code{label2 -> label1}).
#'
#' Population-level inference is performed using:
#' \itemize{
#'   \item \strong{Sample-size / Inverse-variance weighted meta-analytic pooling}:
#'     Computes the population-level pooled rate ratio and 95\% CI, weighting
#'     each animal by its statistical precision (downweighting low-sample birds).
#'   \item \strong{Directional contrast test}: Tests whether the rate ratio in
#'     the \code{label1 -> label2} direction is significantly higher than
#'     \code{label2 -> label1} across animals (paired $t$-test on $\ln(\text{RR})$).
#'   \item \strong{Sign test}: Tests how many animals show an elevated rate
#'     ratio ($\text{RR} > 1.0$).
#' }
#'
#' @param pool Data frame from \code{\link{pool_lys_session_maps}}.
#' @param label1 Character. First label (hypothesised trigger).
#' @param label2 Character. Second label (hypothesised response).
#' @param window_sec Numeric. Foreground time window (s) following each trigger.
#'   Default \code{120}.
#' @param label_col,start_col,end_col,session_col Column names passed to
#'   the underlying rate calculation.
#' @param plot Logical. Draw a forest plot of per-animal rate ratios with 95\% CIs.
#'   Default \code{TRUE}.
#' @param verbose Logical. Print summary tables and test results. Default \code{TRUE}.
#'
#' @return Invisibly, a list with:
#' \describe{
#'   \item{\code{per_bird}}{Data frame of per-animal rate ratios, rates, counts,
#'     weights, and confidence intervals in both directions.}
#'   \item{\code{pooled_1to2}}{List with meta-analytic pooled rate ratio, 95\% CI,
#'     and $p$-value for \code{label1 -> label2}.}
#'   \item{\code{pooled_2to1}}{List with meta-analytic pooled rate ratio, 95\% CI,
#'     and $p$-value for \code{label2 -> label1}.}
#'   \item{\code{directional_test}}{Paired test comparing $\ln(\text{RR}_{1 \to 2})$
#'     vs $\ln(\text{RR}_{2 \to 1})$ across animals.}
#' }
#'
#' @examples
#' \dontrun{
#' pool <- pool_lys_session_maps(lys_list = list(O703 = lys_O703, O704 = lys_O704))
#' res  <- population_rate_ratio(pool, "BeggingCall", "SongBout", window_sec = 120)
#' res$pooled_1to2
#' }
#'
#' @export
population_rate_ratio <- function(pool,
                                  label1,
                                  label2,
                                  window_sec  = 120,
                                  label_col   = "vocalization_label",
                                  start_col   = "session_relative_start",
                                  end_col     = "session_relative_end",
                                  session_col = "session_label",
                                  plot        = TRUE,
                                  verbose     = TRUE) {

  if (!"bird_id" %in% names(pool)) {
    stop("'pool' must contain a 'bird_id' column created by pool_lys_session_maps().",
         call. = FALSE)
  }

  birds <- unique(pool$bird_id)

  # Internal helper for one animal & one direction
  # TODO: extract to standalone helper
  calculate_conditional_rate_ratio <- function(df, trigger, response) {
    sessions   <- unique(df[[session_col]])
    n_trig_tot <- 0L
    n_fg_tot   <- 0L
    t_fg_tot   <- 0
    n_bg_tot   <- 0L
    t_bg_tot   <- 0

    for (sess in sessions) {
      sd <- df[df[[session_col]] == sess, , drop = FALSE]
      sd <- sd[order(sd[[start_col]]), , drop = FALSE]

      trig_rows <- which(sd[[label_col]] == trigger)
      resp_rows <- which(sd[[label_col]] == response)

      if (!length(trig_rows) || !nrow(sd)) next

      sess_duration <- max(sd[[end_col]]) - min(sd[[start_col]])
      sess_end      <- max(sd[[end_col]])

      fg_intervals <- lapply(trig_rows, function(ti) {
        t0 <- sd[[end_col]][ti]
        t1 <- min(t0 + window_sec, sess_end)
        c(t0, t1)
      })

      fg_mat <- do.call(rbind, fg_intervals)
      fg_mat <- fg_mat[order(fg_mat[, 1]), , drop = FALSE]
      merged <- list()
      cur    <- fg_mat[1, ]
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

      for (ri in resp_rows) {
        t_resp <- sd[[start_col]][ri]
        in_fg  <- any(t_resp >= fg_mat[, 1] & t_resp <= fg_mat[, 2])
        if (in_fg) {
          n_fg_tot <- n_fg_tot + 1L
        } else {
          n_bg_tot <- n_bg_tot + 1L
        }
      }

      n_trig_tot <- n_trig_tot + length(trig_rows)
      t_fg_tot   <- t_fg_tot + t_fg_sess
      t_bg_tot   <- t_bg_tot + max(0, sess_duration - t_fg_sess)
    }

    rate_fg <- if (t_fg_tot > 0) n_fg_tot / t_fg_tot else NA_real_
    rate_bg <- if (t_bg_tot > 0) n_bg_tot / t_bg_tot else NA_real_

    # Poisson test
    ptest <- tryCatch(
      stats::poisson.test(c(n_fg_tot, n_bg_tot), c(t_fg_tot, t_bg_tot)),
      error = function(e) NULL
    )

    rr     <- if (!is.null(ptest)) unname(ptest$estimate) else rate_fg / rate_bg
    ci_low <- if (!is.null(ptest)) ptest$conf.int[1] else NA_real_
    ci_hi  <- if (!is.null(ptest)) ptest$conf.int[2] else NA_real_
    pval   <- if (!is.null(ptest)) ptest$p.value     else NA_real_

    # Log rate ratio variance for inverse-variance meta-analysis
    log_rr <- if (!is.na(rr) && rr > 0) log(rr) else NA_real_
    var_log_rr <- if (n_fg_tot > 0 && n_bg_tot > 0) (1 / n_fg_tot + 1 / n_bg_tot) else NA_real_

    list(
      n_triggers = n_trig_tot,
      n_fg       = n_fg_tot,
      t_fg_sec   = t_fg_tot,
      n_bg       = n_bg_tot,
      t_bg_sec   = t_bg_tot,
      rate_fg    = rate_fg,
      rate_bg    = rate_bg,
      rate_ratio = rr,
      ci_low     = ci_low,
      ci_high    = ci_hi,
      p_value    = pval,
      log_rr     = log_rr,
      var_log_rr = var_log_rr
    )
  }

  rows <- lapply(birds, function(bid) {
    bd   <- pool[pool$bird_id == bid, , drop = FALSE]
    res1 <- calculate_conditional_rate_ratio(bd, label1, label2)
    res2 <- calculate_conditional_rate_ratio(bd, label2, label1)

    data.frame(
      bird_id        = bid,
      n_trig_1to2    = res1$n_triggers,
      n_fg_1to2      = res1$n_fg,
      n_bg_1to2      = res1$n_bg,
      t_fg_sec_1to2  = res1$t_fg_sec,
      t_bg_sec_1to2  = res1$t_bg_sec,
      rate_fg_1to2   = res1$rate_fg,
      rate_bg_1to2   = res1$rate_bg,
      rr_1to2        = res1$rate_ratio,
      ci_low_1to2    = res1$ci_low,
      ci_high_1to2   = res1$ci_high,
      p_val_1to2     = res1$p_value,
      log_rr_1to2    = res1$log_rr,
      var_1to2       = res1$var_log_rr,

      n_trig_2to1    = res2$n_triggers,
      n_fg_2to1      = res2$n_fg,
      n_bg_2to1      = res2$n_bg,
      t_fg_sec_2to1  = res2$t_fg_sec,
      t_bg_sec_2to1  = res2$t_bg_sec,
      rate_fg_2to1   = res2$rate_fg,
      rate_bg_2to1   = res2$rate_bg,
      rr_2to1        = res2$rate_ratio,
      ci_low_2to1    = res2$ci_low,
      ci_high_2to1   = res2$ci_high,
      p_val_2to1     = res2$p_value,
      log_rr_2to1    = res2$log_rr,
      var_2to1       = res2$var_log_rr,
      stringsAsFactors = FALSE
    )
  })

  df_res <- do.call(rbind, rows)
  rownames(df_res) <- NULL

  # Meta-analytic pooling (inverse-variance weighted fixed/random effects)
  # TODO: extract to standalone helper
  pool_rate_ratios <- function(log_rrs, vars) {
    valid <- !is.na(log_rrs) & !is.na(vars) & vars > 0
    if (!any(valid)) {
      return(list(rr = NA_real_, ci_low = NA_real_, ci_high = NA_real_, p_value = NA_real_))
    }
    w <- 1 / vars[valid]
    pooled_log_rr <- sum(w * log_rrs[valid]) / sum(w)
    pooled_se     <- sqrt(1 / sum(w))
    z             <- pooled_log_rr / pooled_se
    pval          <- 2 * (1 - stats::pnorm(abs(z)))

    list(
      rr      = exp(pooled_log_rr),
      ci_low  = exp(pooled_log_rr - 1.96 * pooled_se),
      ci_high = exp(pooled_log_rr + 1.96 * pooled_se),
      p_value = pval,
      weights = 100 * w / sum(w)
    )
  }

  pool_1to2 <- pool_rate_ratios(df_res$log_rr_1to2, df_res$var_1to2)
  pool_2to1 <- pool_rate_ratios(df_res$log_rr_2to1, df_res$var_2to1)

  # Paired directional contrast across birds (ln(RR_1to2) vs ln(RR_2to1))
  diff_log_rr <- df_res$log_rr_1to2 - df_res$log_rr_2to1
  var_diff    <- df_res$var_1to2 + df_res$var_2to1
  valid_diff  <- which(!is.na(diff_log_rr) & !is.na(var_diff) & var_diff > 0)

  # Meta-analytic pooled directional difference (inverse-variance weighted)
  pooled_diff_res <- if (length(valid_diff) >= 2L) {
    w_diff        <- 1 / var_diff[valid_diff]
    pooled_d_log  <- sum(w_diff * diff_log_rr[valid_diff]) / sum(w_diff)
    pooled_d_se   <- sqrt(1 / sum(w_diff))
    z_diff        <- pooled_d_log / pooled_d_se
    p_diff        <- 2 * (1 - stats::pnorm(abs(z_diff)))
    list(
      ratio_of_rrs = exp(pooled_d_log),
      ci_low       = exp(pooled_d_log - 1.96 * pooled_d_se),
      ci_high      = exp(pooled_d_log + 1.96 * pooled_d_se),
      z_value      = z_diff,
      p_value      = p_diff
    )
  } else NULL

  directional_test <- if (length(valid_diff) >= 3L) {
    tryCatch(stats::t.test(diff_log_rr[valid_diff], mu = 0), error = function(e) NULL)
  } else NULL

  if (verbose) {
    message(sprintf(
      "\n=========================================================================\nPopulation Conditional Rate Ratio Analysis: %s <-> %s (window = %g s, N = %d)\n=========================================================================",
      label1, label2, window_sec, nrow(df_res)
    ))

    message(sprintf("\n[Direction 1: %s -> %s]", label1, label2))
    message(sprintf("  %-10s  %7s  %7s  %10s  %10s  %-23s  %7s",
                    "bird_id", "n_fg", "n_bg", "rate_fg", "rate_bg", "Rate Ratio (95% CI)", "p_val"))
    message(paste(rep("-", 80), collapse = ""))
    for (i in seq_len(nrow(df_res))) {
      r <- df_res[i, ]
      rr_str <- format_rate_ratio(r$rr_1to2, r$ci_low_1to2, r$ci_high_1to2)
      message(sprintf("  %-10s  %7d  %7d  %10.4f  %10.4f  %-23s  %7s",
                      r$bird_id, r$n_fg_1to2, r$n_bg_1to2, r$rate_fg_1to2, r$rate_bg_1to2,
                      rr_str, format_p_value(r$p_val_1to2)))
    }
    message(paste(rep("-", 80), collapse = ""))
    message(sprintf("  Meta-analytic Pooled RR : %s (p %s)",
                    format_rate_ratio(pool_1to2$rr, pool_1to2$ci_low, pool_1to2$ci_high),
                    format_p_value(pool_1to2$p_value)))

    message(sprintf("\n[Direction 2: %s -> %s]", label2, label1))
    message(sprintf("  %-10s  %7s  %7s  %10s  %10s  %-23s  %7s",
                    "bird_id", "n_fg", "n_bg", "rate_fg", "rate_bg", "Rate Ratio (95% CI)", "p_val"))
    message(paste(rep("-", 80), collapse = ""))
    for (i in seq_len(nrow(df_res))) {
      r <- df_res[i, ]
      rr_str <- format_rate_ratio(r$rr_2to1, r$ci_low_2to1, r$ci_high_2to1)
      message(sprintf("  %-10s  %7d  %7d  %10.4f  %10.4f  %-23s  %7s",
                      r$bird_id, r$n_fg_2to1, r$n_bg_2to1, r$rate_fg_2to1, r$rate_bg_2to1,
                      rr_str, format_p_value(r$p_val_2to1)))
    }
    message(paste(rep("-", 80), collapse = ""))
    message(sprintf("  Meta-analytic Pooled RR : %s (p %s)",
                    format_rate_ratio(pool_2to1$rr, pool_2to1$ci_low, pool_2to1$ci_high),
                    format_p_value(pool_2to1$p_value)))

    message(sprintf("\n[Directional Contrast: %s->%s vs %s->%s]",
                    label1, label2, label2, label1))
    if (!is.null(pooled_diff_res)) {
      message(sprintf("  Pooled Ratio of RRs (RR_1to2 / RR_2to1) : %s (p %s)",
                      format_rate_ratio(pooled_diff_res$ratio_of_rrs, pooled_diff_res$ci_low, pooled_diff_res$ci_high),
                      format_p_value(pooled_diff_res$p_value)))
    }
    if (!is.null(directional_test)) {
      message(sprintf("  Unweighted Paired t-test on ln(RR)      : t = %.3f, df = %d, p %s",
                      directional_test$statistic, directional_test$parameter,
                      format_p_value(directional_test$p.value)))
    }
    message("=========================================================================\n")
  }

  # Forest plot
  if (plot && nrow(df_res) > 0L) {
    old_par <- graphics::par(mfrow = c(1, 2), mar = c(5, 6, 4, 2) + 0.1)
    on.exit(graphics::par(old_par), add = TRUE)

    # TODO: extract to standalone helper
    draw_forest <- function(rrs, lows, highs, main_title, pooled_obj) {
      y_pos <- rev(seq_along(rrs))
      x_max <- max(c(highs, 3), na.rm = TRUE)
      x_max <- min(x_max, 15)

      graphics::plot(
        rrs, y_pos,
        xlim = c(0, x_max),
        ylim = c(0.5, length(rrs) + 1.5),
        yaxt = "n",
        xlab = "Rate Ratio (Foreground / Background)",
        ylab = "",
        main = main_title,
        pch  = 19, col = "#1976D2", cex = 1.2
      )
      graphics::axis(2, at = y_pos, labels = rev(df_res$bird_id), las = 2)
      graphics::abline(v = 1.0, lty = 2, col = "gray50", lwd = 1.5)

      for (i in seq_along(rrs)) {
        if (!is.na(lows[i]) && !is.na(highs[i])) {
          graphics::arrows(lows[i], y_pos[i], highs[i], y_pos[i],
                           angle = 90, code = 3, length = 0.04, col = "#1976D2", lwd = 1.5)
        }
      }

      # Pooled diamond / line at bottom
      if (!is.null(pooled_obj) && !is.na(pooled_obj$rr)) {
        graphics::abline(v = pooled_obj$rr, lty = 1, col = "firebrick", lwd = 2)
        graphics::points(pooled_obj$rr, 0.8, pch = 18, col = "firebrick", cex = 2)
        graphics::axis(2, at = 0.8, labels = "POOLED", las = 2, col.axis = "firebrick", font = 2)
      }
    }

    draw_forest(df_res$rr_1to2, df_res$ci_low_1to2, df_res$ci_high_1to2,
                 sprintf("%s -> %s", label1, label2), pool_1to2)
    draw_forest(df_res$rr_2to1, df_res$ci_low_2to1, df_res$ci_high_2to1,
                 sprintf("%s -> %s", label2, label1), pool_2to1)
  }

  invisible(list(
    per_bird         = df_res,
    pooled_1to2      = pool_1to2,
    pooled_2to1      = pool_2to1,
    directional_test = directional_test
  ))
}


# Population PETH ----

#' Peri-Event Time Histogram (PETH) and Multi-Scale Triggering Analysis
#'
#' @description
#' Tests whether \code{trigger_label} actively triggers \code{response_label}
#' by resolving the response probability at fine temporal resolution before and
#' after each trigger event.
#'
#' A fixed wide window (such as 120 s) can dilute a sharp, short-latency triggering
#' effect (e.g. a vocal response within 2-10 s) with baseline noise.
#' \code{population_peth()} solves this via two complementary analyses:
#'
#' \enumerate{
#'   \item \strong{Peri-Event Time Histogram (PETH / Cross-Correlogram)}:
#'     Aligns all response events around trigger offsets in fine time bins
#'     from \code{-time_range} to \code{+time_range} (e.g. -60 s to +60 s).
#'     Directly contrasts the pre-trigger baseline ($t < 0$) against the
#'     post-trigger response ($t > 0$).
#'   \item \strong{Multi-Scale Window Sweep}:
#'     Evaluates the rate ratio and pre-vs-post contrast across multiple window
#'     sizes (e.g. 5, 10, 20, 30, 60, 120 s).  If true triggering occurs,
#'     the rate ratio will peak sharply at short timescales.
#'   \item \strong{Local Pre vs. Post Statistical Contrast}:
#'     Compares response rate in $[0, +W]$ directly to $[-W, 0]$ for each
#'     animal, controlling for non-stationary baseline activity.
#' }
#'
#' @param pool Data frame from \code{\link{pool_lys_session_maps}}.
#' @param trigger_label Character. Label of the trigger event (e.g. \code{"BeggingCall"}).
#' @param response_label Character. Label of the response event (e.g. \code{"SongBout"}).
#' @param align_to Character. Method for temporal alignment:
#'   \itemize{
#'     \item \code{"dual"} (Default, recommended): Uses \strong{Onset alignment for pre-event baseline}
#'       ($t < 0$ is relative to trigger start) and \strong{Offset alignment for post-event response}
#'       ($t > 0$ is relative to trigger end).  This completely removes the trigger's duration from
#'       the analysis, avoiding any zero-rate detection blanking artifact.
#'     \item \code{"offset"}: Aligns all events to trigger offset (end).
#'     \item \code{"onset"}: Aligns all events to trigger onset (start).
#'   }
#' @param time_range Numeric. Pre- and post-trigger time span (s). Default \code{60}.
#' @param bin_sec Numeric. PETH bin width (s). Default \code{2}.
#' @param sweep_windows Numeric vector of window sizes (s) to evaluate.
#'   Default \code{c(5, 10, 20, 30, 60, 120)}.
#' @param label_col,start_col,end_col,session_col Column names.
#' @param plot Logical. Draw the PETH and timescale sweep plots. Default \code{TRUE}.
#' @param verbose Logical. Print summary and statistical tests. Default \code{TRUE}.
#'
#' @return Invisibly, a list with:
#' \describe{
#'   \item{\code{peth_bins}}{Data frame with bin centers, mean response rates (Hz),
#'     and SEM across animals.}
#'   \item{\code{window_sweep}}{Data frame of rate ratios and pre-vs-post paired
#'     test results across window widths.}
#'   \item{\code{align_to}}{Alignment anchor used.}
#' }
#'
#' @examples
#' \dontrun{
#' pool <- pool_lys_session_maps(lys_list = list(O703 = lys_O703, O704 = lys_O704))
#' # Dual alignment (pre-onset baseline vs post-offset response):
#' peth <- population_peth(pool, "BeggingCall", "SongBout", align_to = "dual")
#' }
#'
#' @export
population_peth <- function(pool,
                            trigger_label,
                            response_label,
                            align_to      = c("dual", "offset", "onset"),
                            time_range    = 60,
                            bin_sec       = 2,
                            sweep_windows = c(5, 10, 20, 30, 60, 120),
                            label_col     = "vocalization_label",
                            start_col     = "session_relative_start",
                            end_col       = "session_relative_end",
                            session_col   = "session_label",
                            plot          = TRUE,
                            verbose       = TRUE) {

  if (!"bird_id" %in% names(pool)) {
    stop("'pool' must contain a 'bird_id' column created by pool_lys_session_maps().",
         call. = FALSE)
  }

  align_to <- match.arg(align_to)

  birds     <- unique(pool$bird_id)
  bin_edges <- seq(-time_range, time_range, by = bin_sec)
  bin_mids  <- bin_edges[-length(bin_edges)] + bin_sec / 2
  n_bins    <- length(bin_mids)

  # Calculate per-animal PETH rate vectors
  peth_mat <- matrix(NA_real_, nrow = length(birds), ncol = n_bins,
                     dimnames = list(birds, sprintf("%.1f", bin_mids)))

  bird_trig_counts <- integer(length(birds))
  names(bird_trig_counts) <- birds

  for (bi in seq_along(birds)) {
    bid <- birds[bi]
    bd  <- pool[pool$bird_id == bid, , drop = FALSE]
    sessions <- unique(bd[[session_col]])

    rel_times <- numeric(0)
    total_trigs <- 0L

    for (sess in sessions) {
      sd <- bd[bd[[session_col]] == sess, , drop = FALSE]
      sd <- sd[order(sd[[start_col]]), , drop = FALSE]

      trig_idx <- which(sd[[label_col]] == trigger_label)
      resp_idx <- which(sd[[label_col]] == response_label)

      if (!length(trig_idx) || !length(resp_idx)) next

      total_trigs <- total_trigs + length(trig_idx)

      for (ti in trig_idx) {
        t_start <- sd[[start_col]][ti]
        t_end   <- sd[[end_col]][ti]
        r_starts <- sd[[start_col]][resp_idx]

        if (align_to == "dual") {
          # Pre-trigger: relative to onset (t < 0)
          dts_pre <- r_starts[r_starts < t_start] - t_start
          in_pre  <- dts_pre >= -time_range & dts_pre < 0
          if (any(in_pre)) rel_times <- c(rel_times, dts_pre[in_pre])

          # Post-trigger: relative to offset (t > 0)
          dts_post <- r_starts[r_starts >= t_end] - t_end
          in_post  <- dts_post >= 0 & dts_post <= time_range
          if (any(in_post)) rel_times <- c(rel_times, dts_post[in_post])
        } else {
          t0 <- if (align_to == "offset") t_end else t_start
          dts <- r_starts - t0
          in_range <- dts >= -time_range & dts <= time_range
          if (any(in_range)) rel_times <- c(rel_times, dts[in_range])
        }
      }
    }

    bird_trig_counts[bi] <- total_trigs

    if (total_trigs > 0L) {
      counts <- graphics::hist(rel_times, breaks = bin_edges, plot = FALSE)$counts
      # Rate in events per trigger per second (Hz)
      peth_mat[bi, ] <- counts / (total_trigs * bin_sec)
    } else {
      peth_mat[bi, ] <- 0
    }
  }

  mean_rate <- colMeans(peth_mat, na.rm = TRUE)
  sem_rate  <- apply(peth_mat, 2, function(x) {
    x <- x[!is.na(x)]
    if (length(x) > 1L) stats::sd(x) / sqrt(length(x)) else 0
  })

  df_peth <- data.frame(
    bin_center = bin_mids,
    mean_rate  = mean_rate,
    sem_rate   = sem_rate,
    is_post    = bin_mids > 0
  )

  # Pre vs Post Window Sweep
  # Note: The true pre-stimulus baseline must be measured BEFORE the trigger ONSET (t_start),
  # avoiding the artifact where the duration of the trigger event itself suppresses pre-rate.
  sweep_res <- list()
  for (w in sweep_windows) {
    pre_rates  <- numeric(length(birds))
    post_rates <- numeric(length(birds))

    for (bi in seq_along(birds)) {
      bid <- birds[bi]
      bd  <- pool[pool$bird_id == bid, , drop = FALSE]
      sessions <- unique(bd[[session_col]])

      n_pre <- 0L; n_post <- 0L; n_trig <- 0L

      for (sess in sessions) {
        sd <- bd[bd[[session_col]] == sess, , drop = FALSE]
        sd <- sd[order(sd[[start_col]]), , drop = FALSE]

        trig_idx <- which(sd[[label_col]] == trigger_label)
        resp_idx <- which(sd[[label_col]] == response_label)
        if (!length(trig_idx) || !length(resp_idx)) next

        n_trig <- n_trig + length(trig_idx)
        for (ti in trig_idx) {
          t_start <- sd[[start_col]][ti]
          t_end   <- sd[[end_col]][ti]
          resp_starts <- sd[[start_col]][resp_idx]

          # Post rate: measured after t_end (offset) or after t_start (onset)
          t_post_anchor <- if (align_to == "offset") t_end else t_start
          n_post <- n_post + sum(resp_starts >= t_post_anchor & resp_starts <= (t_post_anchor + w))

          # Pre rate: strictly measured BEFORE trigger onset (t_start)
          # so trigger duration never contaminates the pre-baseline
          n_pre  <- n_pre  + sum(resp_starts >= (t_start - w) & resp_starts < t_start)
        }
      }

      if (n_trig > 0L) {
        pre_rates[bi]  <- n_pre  / (n_trig * w)
        post_rates[bi] <- n_post / (n_trig * w)
      } else {
        pre_rates[bi]  <- NA_real_
        post_rates[bi] <- NA_real_
      }
    }

    valid <- !is.na(pre_rates) & !is.na(post_rates)

    # Paired test pre vs post (paired t-test or Wilcoxon)
    ptest <- if (sum(valid) >= 3L) {
      t_res <- tryCatch(stats::t.test(post_rates[valid], pre_rates[valid], paired = TRUE, alternative = "greater"),
                        error = function(e) NULL)
      if (!is.null(t_res) && !is.na(t_res$p.value)) {
        t_res$p.value
      } else {
        w_res <- tryCatch(stats::wilcox.test(post_rates[valid], pre_rates[valid], paired = TRUE, alternative = "greater", exact = FALSE),
                          error = function(e) NULL)
        if (!is.null(w_res)) w_res$p.value else NA_real_
      }
    } else NA_real_

    m_pre  <- mean(pre_rates[valid], na.rm = TRUE)
    m_post <- mean(post_rates[valid], na.rm = TRUE)
    rr_val <- if (!is.na(m_pre) && m_pre > 0) {
      m_post / m_pre
    } else if (!is.na(m_post) && m_post > 0) {
      Inf
    } else {
      1.0
    }

    sweep_res[[length(sweep_res) + 1L]] <- data.frame(
      window_sec       = w,
      mean_pre_rate    = m_pre,
      mean_post_rate   = m_post,
      mean_rate_ratio  = rr_val,
      n_birds_elevated = sum(post_rates[valid] > pre_rates[valid]),
      n_birds_tested   = sum(valid),
      p_val_paired     = ptest,
      stringsAsFactors = FALSE
    )
  }

  df_sweep <- do.call(rbind, sweep_res)

  if (verbose) {
    anchor_desc <- if (align_to == "dual") {
      "Dual (Pre-Onset vs Post-Offset)"
    } else if (align_to == "offset") {
      "Offset (End of Call)"
    } else {
      "Onset (Start of Call)"
    }
    message(sprintf(
      "\n=========================================================================\nPeri-Event Time Histogram (PETH): %s -> %s (N = %d animals, Align = %s)\n=========================================================================",
      trigger_label, response_label, length(birds), anchor_desc
    ))
    message("\n[Pre vs. Post Window Sweep (Pre-Onset Baseline Contrast)]:")
    message(sprintf("  %10s  %12s  %12s  %12s  %12s  %8s",
                    "Window (s)", "Pre-Onset(Hz)", "Post-Rate(Hz)", "Post/Pre RR", "Birds Elev", "p_val"))
    message(paste(rep("-", 75), collapse = ""))
    for (i in seq_len(nrow(df_sweep))) {
      r <- df_sweep[i, ]
      tag <- if (!is.na(r$p_val_paired) && r$p_val_paired < 0.05) " *" else ""
      message(sprintf("  %8.0f s  %12.4f  %12.4f  %12.2f  %7d/%-4d  %7s%s",
                      r$window_sec, r$mean_pre_rate, r$mean_post_rate,
                      r$mean_rate_ratio, r$n_birds_elevated, r$n_birds_tested,
                      format_p_value(r$p_val_paired), tag))
    }
    message(paste(rep("-", 75), collapse = ""))
    message("  * Significant elevation (paired t-test, post > pre-onset, p < 0.05)")
    message("=========================================================================\n")
  }

  # Plot
  if (plot) {
    old_par <- graphics::par(mfrow = c(1, 2), mar = c(5, 5, 4, 2) + 0.1)
    on.exit(graphics::par(old_par), add = TRUE)

    # Panel 1: PETH Curve
    y_max <- max(df_peth$mean_rate + df_peth$sem_rate, na.rm = TRUE) * 1.25
    if (y_max == 0 || is.na(y_max)) y_max <- 0.01

    xlab_str <- if (align_to == "dual") {
      sprintf("Time relative to %s (s) [Pre-Onset < 0 | Post-Offset > 0]", trigger_label)
    } else {
      sprintf("Time relative to %s %s (s)", trigger_label, align_to)
    }

    graphics::plot(
      df_peth$bin_center, df_peth$mean_rate,
      type = "n",
      xlim = c(-time_range, time_range),
      ylim = c(0, y_max),
      xlab = xlab_str,
      ylab = sprintf("%s rate (/s)", response_label),
      main = "Population PETH"
    )

    # Shaded pre vs post background
    graphics::rect(-time_range, 0, 0, y_max, col = "#F0F0F0", border = NA)
    graphics::rect(0, 0, time_range, y_max, col = "#E3F2FD", border = NA)

    # Error envelope (SEM)
    graphics::polygon(
      c(df_peth$bin_center, rev(df_peth$bin_center)),
      c(pmax(0, df_peth$mean_rate - df_peth$sem_rate), rev(df_peth$mean_rate + df_peth$sem_rate)),
      col = grDevices::adjustcolor("#1976D2", alpha.f = 0.25),
      border = NA
    )

    # Mean line
    graphics::lines(df_peth$bin_center, df_peth$mean_rate, col = "#1976D2", lwd = 2.5)
    graphics::abline(v = 0, lty = 2, col = "firebrick", lwd = 2)

    graphics::legend(
      "topright",
      legend = c("Pre-event", "Post-event", "Mean +/- SEM"),
      fill = c("#F0F0F0", "#E3F2FD", grDevices::adjustcolor("#1976D2", alpha.f = 0.4)),
      bty = "n", cex = 0.8
    )

    # Panel 2: Multi-Scale Rate Ratio Sweep
    valid_sweep <- df_sweep[!is.na(df_sweep$mean_rate_ratio) & is.finite(df_sweep$mean_rate_ratio), ]
    if (nrow(valid_sweep) > 0L) {
      y_top <- max(c(valid_sweep$mean_rate_ratio, 2), na.rm = TRUE) * 1.2
      graphics::plot(
        valid_sweep$window_sec, valid_sweep$mean_rate_ratio,
        type = "b", pch = 19, lwd = 2, col = "#388E3C",
        ylim = c(0, y_top),
        xlab = "Evaluation Window Width (s)",
        ylab = "Post / Pre Rate Ratio",
        main = "Triggering Effect Across Timescales"
      )
      graphics::abline(h = 1.0, lty = 2, col = "gray40", lwd = 1.5)

      pvals <- valid_sweep$p_val_paired
      sig_labels <- ifelse(is.na(pvals), "",
                    ifelse(pvals < 0.001, "***",
                    ifelse(pvals < 0.01, "**",
                    ifelse(pvals < 0.05, "*", ""))))

      sig_mask <- sig_labels != ""
      if (any(sig_mask)) {
        graphics::text(
          valid_sweep$window_sec[sig_mask],
          valid_sweep$mean_rate_ratio[sig_mask] + y_top * 0.06,
          labels = sig_labels[sig_mask],
          cex = 1.2, col = "firebrick", font = 2
        )
      }
    }
  }

  invisible(list(
    peth_bins    = df_peth,
    window_sweep = df_sweep,
    peth_matrix  = peth_mat,
    align_to     = align_to
  ))
}


# Population Point-Process GLM ----

#' Point-Process GLM for Disentangling Triggering vs. Refractory Suppression
#'
#' @description
#' Fits a multivariate point-process Generalized Linear Model (Pillow et al., 2008;
#' Truccolo et al., 2005) to discretized vocalization time series across animals.
#'
#' This model simultaneously estimates:
#' \enumerate{
#'   \item \strong{Distant spontaneous baseline rate} ($\mu$).
#'   \item \strong{Self-history / Refractory filter} ($h_{\text{self}}$):
#'     The post-song quiet/refractory period.
#'   \item \strong{Cross-coupling / Triggering filter} ($h_{\text{pred} \to \text{target}}$):
#'     The effect of a preceding begging call on singing probability at various
#'     time lags ($0\text{-}5\text{s}$, $5\text{-}15\text{s}$, $15\text{-}30\text{s}$, $30\text{-}60\text{s}$),
#'     \strong{conditioned on the bird's own history and distant baseline}.
#' }
#'
#' By controlling for self-refractoriness and the unperturbed baseline, this test
#' rigorously determines whether begging calls \emph{actively trigger} singing
#' or whether apparent temporal sequences are an artifact of post-song suppression.
#'
#' @param pool Data frame from \code{\link{pool_lys_session_maps}}.
#' @param target_label Character. Event being predicted (e.g. \code{"SongBout"}).
#' @param predictor_label Character. Hypothesized trigger (e.g. \code{"BeggingCall"}).
#' @param bin_sec Numeric. Discretization bin width in seconds. Default \code{1}.
#' @param show_ci Logical. Draw 95\% confidence interval error bars on the points.
#'   Default \code{FALSE} to keep the trend curve clean and uncluttered.
#' @param label_stat Character. What to display above each point:
#'   \itemize{
#'     \item \code{"significance"} (Default): Shows significance stars (\code{"*"} $p < 0.05$,
#'       \code{"**"} $p < 0.01$, \code{"***"} $p < 0.001$).
#'     \item \code{"p_value"}: Shows the exact $p$-value (e.g. \code{"p=0.031"}).
#'     \item \code{"odds_ratio"}: Shows the numerical Odds Ratio (e.g. \code{"2.32x"}).
#'     \item \code{"none"}: No text above points.
#'   }
#' @param plot Logical. Plot the estimated coupling filters. Default \code{TRUE}.
#' @param verbose Logical. Print summary tables and coefficient tests. Default \code{TRUE}.
#'
#' @return Invisibly, a list with:
#' \describe{
#'   \item{\code{glm_model}}{The fitted \code{glm} object.}
#'   \item{\code{coefficients_table}}{Data frame of estimated log-odds, odds ratios,
#'     standard errors, and $p$-values.}
#' }
#'
#' @export
population_point_process_glm <- function(pool,
                                         target_label    = "SongBout",
                                         predictor_label = "BeggingCall",
                                         bin_sec         = 0.5,
                                         predictor_lags  = c(seq(0, 5, by = 0.5), 10, 20, 30, 60),
                                         self_lags       = c(0, 5, 10, 20, 30, 60),
                                         show_ci         = FALSE,
                                         label_stat      = c("significance", "p_value", "odds_ratio", "none"),
                                         label_col       = "vocalization_label",
                                         start_col       = "session_relative_start",
                                         end_col         = "session_relative_end",
                                         session_col     = "session_label",
                                         plot            = TRUE,
                                         verbose         = TRUE) {

  if (!"bird_id" %in% names(pool)) {
    stop("'pool' must contain a 'bird_id' column created by pool_lys_session_maps().",
         call. = FALSE)
  }

  predictor_lags <- unique(sort(predictor_lags))
  if (!is.null(self_lags)) self_lags <- unique(sort(self_lags))

  min_pred_width   <- min(diff(predictor_lags))
  min_self_width   <- if (!is.null(self_lags) && length(self_lags) > 1L) min(diff(self_lags)) else Inf
  finest_lag_width <- min(min_pred_width, min_self_width)

  if (bin_sec > finest_lag_width) {
    warning(
      sprintf(
        "'bin_sec' (%g s) is larger than the finest lag window width (%g s).\nTo accurately resolve %g s lag windows without discretization aliasing or parameter instability, set 'bin_sec <= %g'.",
        bin_sec, finest_lag_width, finest_lag_width, finest_lag_width
      ),
      call. = FALSE
    )
  }

  label_stat <- match.arg(label_stat)
  birds    <- unique(pool$bird_id)
  sessions <- unique(pool[[session_col]])

  # Prepare lag predictor names
  n_pred_windows <- length(predictor_lags) - 1L
  pred_names <- vapply(seq_len(n_pred_windows), function(k) {
    sprintf("pred_lag_%g_%gs", predictor_lags[k], predictor_lags[k + 1])
  }, character(1))

  n_self_windows <- if (!is.null(self_lags) && length(self_lags) > 1L) length(self_lags) - 1L else 0L
  self_names <- if (n_self_windows > 0L) {
    vapply(seq_len(n_self_windows), function(k) {
      sprintf("self_lag_%g_%gs", self_lags[k], self_lags[k + 1])
    }, character(1))
  } else character(0)

  # Build discretized dataset per session
  dfs <- list()

  for (sess in sessions) {
    sd <- pool[pool[[session_col]] == sess, , drop = FALSE]
    if (!nrow(sd)) next

    bid <- sd$bird_id[1]
    t_max <- max(sd[[end_col]])
    t_min <- min(sd[[start_col]])

    if (t_max <= t_min + bin_sec) next

    time_bins <- seq(t_min, t_max, by = bin_sec)
    n_time_bins <- length(time_bins)

    # Target event onsets (Y = 1 if target starts in this bin)
    target_starts <- sd[[start_col]][sd[[label_col]] == target_label]
    target_ends   <- sd[[end_col]][sd[[label_col]] == target_label]
    pred_ends     <- sd[[end_col]][sd[[label_col]] == predictor_label]

    # Target onset indicator
    y_vec <- integer(n_time_bins)
    for (ts in target_starts) {
      b_idx <- which(time_bins <= ts & (time_bins + bin_sec) > ts)
      if (length(b_idx)) y_vec[b_idx[1]] <- 1L
    }

    # Predictor lag indicators
    pred_mat <- matrix(0L, nrow = n_time_bins, ncol = n_pred_windows,
                       dimnames = list(NULL, pred_names))
    for (k in seq_len(n_pred_windows)) {
      lo <- predictor_lags[k]
      hi <- predictor_lags[k + 1]
      for (pe in pred_ends) {
        act_idx <- which(time_bins >= (pe + lo) & time_bins < (pe + hi))
        if (length(act_idx)) pred_mat[act_idx, k] <- 1L
      }
    }

    # Self lag indicators (if self_lags is provided)
    self_mat <- if (n_self_windows > 0L) {
      sm <- matrix(0L, nrow = n_time_bins, ncol = n_self_windows,
                   dimnames = list(NULL, self_names))
      for (k in seq_len(n_self_windows)) {
        lo <- self_lags[k]
        hi <- self_lags[k + 1]
        for (te in target_ends) {
          act_idx <- which(time_bins >= (te + lo) & time_bins < (te + hi))
          if (length(act_idx)) sm[act_idx, k] <- 1L
        }
      }
      sm
    } else NULL

    df_sess <- data.frame(
      y       = y_vec,
      bird_id = bid,
      pred_mat,
      stringsAsFactors = FALSE
    )
    if (!is.null(self_mat)) {
      df_sess <- cbind(df_sess, as.data.frame(self_mat))
    }
    dfs[[length(dfs) + 1L]] <- df_sess
  }

  df_all <- do.call(rbind, dfs)

  if (verbose) {
    message(sprintf(
      "\nFitting Point-Process GLM: %d time bins (%g s) across %d animals...",
      nrow(df_all), bin_sec, length(birds)
    ))
  }

  # Fit Logistic GLM with bird_id fixed effects
  active_covars <- c(pred_names, self_names, if (length(birds) > 1L) "bird_id" else NULL)
  formula_str <- paste("y ~", paste(active_covars, collapse = " + "))

  fit <- stats::glm(stats::as.formula(formula_str), data = df_all, family = stats::binomial())
  s_fit <- summary(fit)

  # Extract coefficients table
  coefs <- s_fit$coefficients
  est   <- coefs[, 1]
  se    <- coefs[, 2]
  z_val <- coefs[, 3]
  p_val <- coefs[, 4]

  ci_lo <- exp(est - 1.96 * se)
  ci_hi <- exp(est + 1.96 * se)
  or    <- exp(est)

  df_coefs <- data.frame(
    term        = rownames(coefs),
    estimate    = est,
    std_error   = se,
    odds_ratio  = or,
    ci_lower    = ci_lo,
    ci_upper    = ci_hi,
    z_value     = z_val,
    p_value     = p_val,
    stringsAsFactors = FALSE
  )
  rownames(df_coefs) <- NULL

  if (verbose) {
    message(sprintf(
      "\n=========================================================================\nPoint-Process GLM: Predicting %s Probability (N = %d animals)\n=========================================================================",
      target_label, length(birds)
    ))
    message("\n[Coupling & Self-History Filters (Controlled Odds Ratios)]:")
    message(sprintf("  %-35s  %10s  %10s  %-24s  %8s",
                    "Predictor Term", "Log-Odds", "Std.Error", "Odds Ratio (95% CI)", "p_val"))
    message(paste(rep("-", 95), collapse = ""))

    # Print predictor terms
    for (i in seq_len(nrow(df_coefs))) {
      r <- df_coefs[i, ]
      if (r$term == "(Intercept)" || startsWith(r$term, "bird_id")) next

      term_clean <- r$term
      tag <- ""
      if (startsWith(term_clean, "pred_lag_")) {
        term_clean <- paste0(predictor_label, " -> ", target_label, " (", sub("pred_lag_", "", term_clean), ")")
        if (r$p_value < 0.05 && r$estimate > 0) tag <- " <- Significant Facilitation *"
        if (r$p_value < 0.05 && r$estimate < 0) tag <- " <- Significant Suppression *"
      } else if (startsWith(term_clean, "self_lag_")) {
        term_clean <- paste0("Self-History / Burst (", sub("self_lag_", "", term_clean), ")")
        if (r$p_value < 0.05 && r$estimate > 0) tag <- " <- Significant Self-Clustering *"
        if (r$p_value < 0.05 && r$estimate < 0) tag <- " <- Significant Refractoriness *"
      }

      message(sprintf("  %-35s  %+10.3f  %10.3f  %-24s  %8s%s",
                      term_clean, r$estimate, r$std_error,
                      format_odds_ratio(r$odds_ratio, r$ci_lower, r$ci_upper),
                      format_p_value(r$p_value), tag))
    }
    message(paste(rep("-", 95), collapse = ""))
    message("  * Odds Ratio > 1.0 indicates that begging increases song probability above distant baseline,")
    message("    controlling for self-history burstiness and inter-individual baseline rates.")
    message("=========================================================================\n")
  }

  # Plot coupling filters
  if (plot) {
    # Match predictor terms cleanly
    x_all_mids <- (predictor_lags[-length(predictor_lags)] + predictor_lags[-1]) / 2
    idx_p      <- match(pred_names, df_coefs$term)
    valid_p    <- which(!is.na(idx_p))

    # Match self-history terms cleanly
    valid_s <- integer(0)
    if (length(self_names) > 0L) {
      x_all_self <- (self_lags[-length(self_lags)] + self_lags[-1]) / 2
      idx_s      <- match(self_names, df_coefs$term)
      valid_s    <- which(!is.na(idx_s))
    }

    n_panels <- if (length(valid_s) > 0L) 2 else 1
    old_par  <- graphics::par(mfrow = c(1, n_panels), mar = c(5, 5, 4, 2) + 0.1)
    on.exit(graphics::par(old_par), add = TRUE)

    # TODO: extract to standalone helper
    get_stat_labels <- function(pvals, ors) {
      if (label_stat == "significance") {
        ifelse(pvals < 0.001, "***",
        ifelse(pvals < 0.01, "**",
        ifelse(pvals < 0.05, "*", "")))
      } else if (label_stat == "p_value") {
        ifelse(pvals < 0.05, sprintf("p=%.3f", pvals), "")
      } else if (label_stat == "odds_ratio") {
        sprintf("%.2fx", ors)
      } else {
        rep("", length(pvals))
      }
    }

    # Panel 1: Cross-Coupling Filter (Begging -> Song)
    if (length(valid_p) > 0L) {
      x_p    <- x_all_mids[valid_p]
      y_p    <- df_coefs$odds_ratio[idx_p[valid_p]]
      pval_p <- df_coefs$p_value[idx_p[valid_p]]
      lo_p   <- df_coefs$ci_lower[idx_p[valid_p]]
      hi_p   <- df_coefs$ci_upper[idx_p[valid_p]]

      y_top  <- if (show_ci) max(c(hi_p, 2), na.rm = TRUE) * 1.25 else
                             max(c(y_p, 2), na.rm = TRUE) * 1.35
      graphics::plot(
        x_p, y_p,
        type = "b", pch = 19, lwd = 2.5, col = "#1976D2",
        ylim = c(0, y_top),
        xlab = sprintf("Time after %s (s)", predictor_label),
        ylab = "Controlled Odds Ratio",
        main = sprintf("%s -> %s", predictor_label, target_label)
      )
      graphics::abline(h = 1.0, lty = 2, col = "gray40", lwd = 1.5)

      if (show_ci) {
        for (i in seq_along(x_p)) {
          if (!is.na(lo_p[i]) && !is.na(hi_p[i])) {
            graphics::arrows(x_p[i], lo_p[i], x_p[i], hi_p[i],
                             angle = 90, code = 3, length = 0.04, col = "#1976D2", lwd = 1.2)
          }
        }
      }

      labels_vec <- get_stat_labels(pval_p, y_p)
      sig_mask   <- labels_vec != ""
      if (any(sig_mask)) {
        graphics::text(
          x_p[sig_mask], y_p[sig_mask] + y_top * 0.06,
          labels = labels_vec[sig_mask],
          cex = 1.1, col = "firebrick", font = 2
        )
      }
    }

    # Panel 2: Self-History / Burst Filter (Song -> Song)
    if (length(valid_s) > 0L) {
      x_s    <- x_all_self[valid_s]
      y_s    <- df_coefs$odds_ratio[idx_s[valid_s]]
      pval_s <- df_coefs$p_value[idx_s[valid_s]]
      lo_s   <- df_coefs$ci_lower[idx_s[valid_s]]
      hi_s   <- df_coefs$ci_upper[idx_s[valid_s]]

      y_top_s <- if (show_ci) max(c(hi_s, 2), na.rm = TRUE) * 1.25 else
                              max(c(y_s, 2), na.rm = TRUE) * 1.35
      graphics::plot(
        x_s, y_s,
        type = "b", pch = 19, lwd = 2.5, col = "#D32F2F",
        ylim = c(0, y_top_s),
        xlab = sprintf("Time after %s (s)", target_label),
        ylab = "Controlled Odds Ratio",
        main = sprintf("%s -> %s", target_label, target_label)
      )
      graphics::abline(h = 1.0, lty = 2, col = "gray40", lwd = 1.5)

      if (show_ci) {
        for (i in seq_along(x_s)) {
          if (!is.na(lo_s[i]) && !is.na(hi_s[i])) {
            graphics::arrows(x_s[i], lo_s[i], x_s[i], hi_s[i],
                             angle = 90, code = 3, length = 0.04, col = "#D32F2F", lwd = 1.2)
          }
        }
      }

      labels_vec_self <- get_stat_labels(pval_s, y_s)
      sig_mask_self   <- labels_vec_self != ""
      if (any(sig_mask_self)) {
        graphics::text(
          x_s[sig_mask_self], y_s[sig_mask_self] + y_top_s * 0.06,
          labels = labels_vec_self[sig_mask_self],
          cex = 1.1, col = "firebrick", font = 2
        )
      }
    }
  }

  invisible(list(
    glm_model          = fit,
    coefficients_table = df_coefs
  ))
}


# Population Shuffled PETH ----

#' Shuffled Surrogate PETH for Testing Pointwise Time-Window Significance
#'
#' @description
#' Tests which specific time windows relative to \code{trigger_label} differ
#' significantly from chance by generating a surrogate null distribution where
#' \code{response_label} timestamps are circularly shifted within each session.
#'
#' Circular shifting preserves each bird's internal singing burstiness and
#' overall rate while breaking the temporal relationship with begging calls.
#'
#' The function computes the pointwise 95\% confidence envelope of the shuffled null
#' across all time bins and identifies:
#' \itemize{
#'   \item \strong{Significant Facilitation windows} (Real rate > 97.5th percentile of null, $p < 0.05$).
#'   \item \strong{Significant Suppression windows} (Real rate < 2.5th percentile of null, $p < 0.05$).
#' }
#'
#' @param pool Data frame from \code{\link{pool_lys_session_maps}}.
#' @param trigger_label Character. Label of the trigger event (e.g. \code{"BeggingCall"}).
#' @param response_label Character. Label of the response event (e.g. \code{"SongBout"}).
#' @param align_to Character. \code{"dual"} (default), \code{"offset"}, or \code{"onset"}.
#' @param time_range Numeric. Pre- and post-trigger time span (s). Default \code{60}.
#' @param bin_sec Numeric. PETH bin width (s). Default \code{2}.
#' @param n_perm Integer. Number of surrogate shuffles. Default \code{500}.
#' @param seed Integer. Random seed for reproducibility. Default \code{42}.
#' @param label_col,start_col,end_col,session_col Column names.
#' @param plot Logical. Draw the PETH with null confidence ribbon. Default \code{TRUE}.
#' @param verbose Logical. Print summary table of significant windows. Default \code{TRUE}.
#'
#' @return Invisibly, a list with:
#' \describe{
#'   \item{\code{bins_table}}{Data frame with bin centers, real rate, null mean,
#'     null 95\% bounds, and pointwise $p$-values.}
#'   \item{\code{significant_windows}}{Data frame summarizing all contiguous time
#'     intervals with statistically significant facilitation or suppression.}
#' }
#'
#' @examples
#' \dontrun{
#' pool <- pool_lys_session_maps(lys_list = list(O703 = lys_O703, O704 = lys_O704))
#' shuff_res <- population_shuffled_peth(
#'   pool,
#'   trigger_label  = "BeggingCall",
#'   response_label = "SongBout",
#'   n_perm         = 500
#' )
#' }
#'
#' @param shuffle_method Character. Null generation method:
#'   \itemize{
#'     \item \code{"bin_shuffle"} (Default, fast): Permutes the time-bin rates independently
#'       within each bird across the evaluation window.  This directly tests whether any
#'       specific time bin deviates from the bird's own average rate across the window.
#'     \item \code{"circular_shift"}: Circularly shifts raw song timestamps within each
#'       recording session.  Preserves internal inter-song intervals and burstiness.
#'   }
#' @param align_to Character. \code{"dual"} (default), \code{"offset"}, or \code{"onset"}.
#' @param time_range Numeric. Pre- and post-trigger time span (s). Default \code{60}.
#' @param bin_sec Numeric. PETH bin width (s). Default \code{2}.
#' @param n_perm Integer. Number of surrogate shuffles. Default \code{1000}.
#' @param seed Integer. Random seed for reproducibility. Default \code{42}.
#' @param label_col,start_col,end_col,session_col Column names.
#' @param plot Logical. Draw the PETH with null confidence ribbon. Default \code{TRUE}.
#' @param verbose Logical. Print summary table of significant windows. Default \code{TRUE}.
#'
#' @return Invisibly, a list with:
#' \describe{
#'   \item{\code{bins_table}}{Data frame with bin centers, real rate, null mean,
#'     null 95\% bounds, and pointwise $p$-values.}
#'   \item{\code{significant_windows}}{Data frame summarizing all contiguous time
#'     intervals with statistically significant facilitation or suppression.}
#'   \item{\code{shuffle_method}}{The shuffle method used.}
#' }
#'
#' @examples
#' \dontrun{
#' pool <- pool_lys_session_maps(lys_list = list(O703 = lys_O703, O704 = lys_O704))
#' # Fast within-bird bin rate shuffle:
#' shuff_res <- population_shuffled_peth(pool, shuffle_method = "bin_shuffle")
#' }
#'
#' @export
population_shuffled_peth <- function(pool,
                                     trigger_label  = "BeggingCall",
                                     response_label = "SongBout",
                                     shuffle_method = c("bin_shuffle", "circular_shift"),
                                     align_to       = c("dual", "offset", "onset"),
                                     time_range     = 60,
                                     bin_sec        = 2,
                                     n_perm         = 1000L,
                                     seed           = 42L,
                                     label_col      = "vocalization_label",
                                     start_col      = "session_relative_start",
                                     end_col        = "session_relative_end",
                                     session_col    = "session_label",
                                     plot           = TRUE,
                                     verbose        = TRUE) {

  if (!"bird_id" %in% names(pool)) {
    stop("'pool' must contain a 'bird_id' column created by pool_lys_session_maps().",
         call. = FALSE)
  }

  if (!is.null(seed)) set.seed(seed)
  shuffle_method <- match.arg(shuffle_method)
  align_to       <- match.arg(align_to)

  birds     <- unique(pool$bird_id)
  bin_edges <- seq(-time_range, time_range, by = bin_sec)
  bin_mids  <- bin_edges[-length(bin_edges)] + bin_sec / 2
  n_bins    <- length(bin_mids)

  # Helper to compute per-bird PETH matrix (birds x bins)
  # TODO: extract to standalone helper
  compute_bird_peth_matrix <- function(df) {
    peth_mat <- matrix(NA_real_, nrow = length(birds), ncol = n_bins,
                       dimnames = list(birds, sprintf("%.1f", bin_mids)))
    for (bi in seq_along(birds)) {
      bid <- birds[bi]
      bd  <- df[df$bird_id == bid, , drop = FALSE]
      sessions <- unique(bd[[session_col]])

      rel_times <- numeric(0)
      total_trigs <- 0L

      for (sess in sessions) {
        sd <- bd[bd[[session_col]] == sess, , drop = FALSE]
        sd <- sd[order(sd[[start_col]]), , drop = FALSE]

        trig_idx <- which(sd[[label_col]] == trigger_label)
        resp_idx <- which(sd[[label_col]] == response_label)
        if (!length(trig_idx) || !length(resp_idx)) next

        total_trigs <- total_trigs + length(trig_idx)

        for (ti in trig_idx) {
          t_start  <- sd[[start_col]][ti]
          t_end    <- sd[[end_col]][ti]
          r_starts <- sd[[start_col]][resp_idx]

          if (align_to == "dual") {
            dts_pre <- r_starts[r_starts < t_start] - t_start
            in_pre  <- dts_pre >= -time_range & dts_pre < 0
            if (any(in_pre)) rel_times <- c(rel_times, dts_pre[in_pre])

            dts_post <- r_starts[r_starts >= t_end] - t_end
            in_post  <- dts_post >= 0 & dts_post <= time_range
            if (any(in_post)) rel_times <- c(rel_times, dts_post[in_post])
          } else {
            t0 <- if (align_to == "offset") t_end else t_start
            dts <- r_starts - t0
            in_range <- dts >= -time_range & dts <= time_range
            if (any(in_range)) rel_times <- c(rel_times, dts[in_range])
          }
        }
      }

      if (total_trigs > 0L) {
        counts <- graphics::hist(rel_times, breaks = bin_edges, plot = FALSE)$counts
        peth_mat[bi, ] <- counts / (total_trigs * bin_sec)
      } else {
        peth_mat[bi, ] <- 0
      }
    }
    peth_mat
  }

  real_bird_mat <- compute_bird_peth_matrix(pool)
  real_peth     <- colMeans(real_bird_mat, na.rm = TRUE)

  if (verbose) {
    method_name <- if (shuffle_method == "bin_shuffle") "Within-Bird Bin Rate Permutation" else "Session Circular Time Shift"
    message(sprintf(
      "\nRunning Shuffled Surrogate PETH (%d iterations, Method = %s, N = %d animals)...",
      n_perm, method_name, length(birds)
    ))
  }

  # Generate surrogate null distribution
  shuff_mat <- matrix(NA_real_, nrow = n_perm, ncol = n_bins)

  if (shuffle_method == "bin_shuffle") {
    # Permute bin rates across time within each bird independently
    for (perm_i in seq_len(n_perm)) {
      perm_mat <- real_bird_mat
      for (bi in seq_len(nrow(perm_mat))) {
        perm_mat[bi, ] <- perm_mat[bi, sample(n_bins)]
      }
      shuff_mat[perm_i, ] <- colMeans(perm_mat, na.rm = TRUE)
    }
  } else {
    # Circular shift of response timestamps within session
    sessions_all <- unique(pool[[session_col]])
    for (perm_i in seq_len(n_perm)) {
      shuff_pool <- pool
      for (sess in sessions_all) {
        idx <- which(pool[[session_col]] == sess & pool[[label_col]] == response_label)
        if (length(idx) < 1L) next

        sess_rows <- which(pool[[session_col]] == sess)
        t_min     <- min(pool[[start_col]][sess_rows])
        t_max     <- max(pool[[end_col]][sess_rows])
        span      <- t_max - t_min

        if (span <= 10) next

        shift_amount <- stats::runif(1L, min = 10, max = span - 10)
        shifted_starts <- pool[[start_col]][idx] + shift_amount
        wrap_idx <- shifted_starts > t_max
        shifted_starts[wrap_idx] <- t_min + (shifted_starts[wrap_idx] - t_max)

        shuff_pool[[start_col]][idx] <- shifted_starts
      }
      shuff_mat[perm_i, ] <- colMeans(compute_bird_peth_matrix(shuff_pool), na.rm = TRUE)
    }
  }

  # Calculate pointwise statistics per bin
  null_mean <- colMeans(shuff_mat, na.rm = TRUE)
  null_lo   <- apply(shuff_mat, 2, stats::quantile, probs = 0.025, na.rm = TRUE)
  null_hi   <- apply(shuff_mat, 2, stats::quantile, probs = 0.975, na.rm = TRUE)

  p_vals <- vapply(seq_len(n_bins), function(k) {
    obs <- real_peth[k]
    dist <- shuff_mat[, k]
    p_greater <- mean(dist >= obs)
    p_lesser  <- mean(dist <= obs)
    min(1.0, 2 * min(p_greater, p_lesser))
  }, numeric(1L))

  q_vals <- stats::p.adjust(p_vals, method = "fdr")

  is_facilitation <- real_peth > null_hi
  is_suppression  <- real_peth < null_lo

  df_bins <- data.frame(
    bin_center      = bin_mids,
    bin_start       = bin_edges[-length(bin_edges)],
    bin_end         = bin_edges[-1],
    real_rate       = real_peth,
    null_mean       = null_mean,
    null_ci_lower   = null_lo,
    null_ci_upper   = null_hi,
    p_value         = p_vals,
    q_value_fdr     = q_vals,
    is_facilitation = is_facilitation,
    is_suppression  = is_suppression,
    stringsAsFactors = FALSE
  )

  # Group contiguous significant intervals
  # TODO: extract to standalone helper
  find_contiguous <- function(flags, label) {
    if (!any(flags)) return(data.frame())
    rle_res <- rle(flags)
    ends    <- cumsum(rle_res$lengths)
    starts  <- c(1L, ends[-length(ends)] + 1L)
    sig_idx <- which(rle_res$values)

    lapply(sig_idx, function(si) {
      b_start <- starts[si]
      b_end   <- ends[si]
      data.frame(
        effect_type    = label,
        window_start   = df_bins$bin_start[b_start],
        window_end     = df_bins$bin_end[b_end],
        duration_sec   = df_bins$bin_end[b_end] - df_bins$bin_start[b_start],
        mean_real_rate = mean(df_bins$real_rate[b_start:b_end]),
        mean_null_rate = mean(df_bins$null_mean[b_start:b_end]),
        rate_ratio     = if (mean(df_bins$null_mean[b_start:b_end]) > 0)
          mean(df_bins$real_rate[b_start:b_end]) / mean(df_bins$null_mean[b_start:b_end]) else NA_real_,
        min_p_value    = min(df_bins$p_value[b_start:b_end]),
        min_q_fdr      = min(df_bins$q_value_fdr[b_start:b_end]),
        stringsAsFactors = FALSE
      )
    })
  }

  sig_fac <- find_contiguous(is_facilitation, "Facilitation (Real > Null)")
  sig_sup <- find_contiguous(is_suppression, "Suppression (Real < Null)")
  df_sig  <- do.call(rbind, c(sig_fac, sig_sup))
  if (!is.null(df_sig)) rownames(df_sig) <- NULL

  if (verbose) {
    message(sprintf(
      "\n=========================================================================\nShuffled Surrogate PETH Results: %s -> %s\n=========================================================================",
      trigger_label, response_label
    ))
    if (!is.null(df_sig) && nrow(df_sig) > 0L) {
      message("\n[Statistically Significant Time Windows (p < 0.05 vs. Shuffled Null)]:")
      message(sprintf("  %-28s  %10s  %10s  %12s  %12s  %8s  %8s",
                      "Effect Type", "Start (s)", "End (s)", "Real Rate", "Null Rate", "Min p-val", "FDR q-val"))
      message(paste(rep("-", 95), collapse = ""))
      for (i in seq_len(nrow(df_sig))) {
        r <- df_sig[i, ]
        message(sprintf("  %-28s  %+10.1f  %+10.1f  %12.4f  %12.4f  %8s  %8s",
                        r$effect_type, r$window_start, r$window_end,
                        r$mean_real_rate, r$mean_null_rate,
                        format_p_value(r$min_p_value), format_p_value(r$min_q_fdr)))
      }
      message(paste(rep("-", 95), collapse = ""))
    } else {
      message("\nNo individual time bins differed significantly from the shuffled null distribution (p >= 0.05).")
    }
    message("=========================================================================\n")
  }

  # Plot
  if (plot) {
    y_max <- max(c(df_bins$real_rate, df_bins$null_ci_upper), na.rm = TRUE) * 1.3
    if (y_max == 0 || is.na(y_max)) y_max <- 0.01

    xlab_str <- if (align_to == "dual") {
      sprintf("Time relative to %s (s) [Pre-Onset < 0 | Post-Offset > 0]", trigger_label)
    } else {
      sprintf("Time relative to %s %s (s)", trigger_label, align_to)
    }

    graphics::plot(
      df_bins$bin_center, df_bins$real_rate,
      type = "n",
      xlim = c(-time_range, time_range),
      ylim = c(0, y_max),
      xlab = xlab_str,
      ylab = sprintf("%s rate (/s)", response_label),
      main = sprintf("Shuffled Surrogate PETH (vs. %d Shuffles)", n_perm)
    )

    # Shaded pre vs post background
    graphics::rect(-time_range, 0, 0, y_max, col = "#FAFAFA", border = NA)
    graphics::rect(0, 0, time_range, y_max, col = "#F5F5F5", border = NA)

    # 95% Null envelope
    graphics::polygon(
      c(df_bins$bin_center, rev(df_bins$bin_center)),
      c(df_bins$null_ci_lower, rev(df_bins$null_ci_upper)),
      col = grDevices::adjustcolor("gray70", alpha.f = 0.5),
      border = NA
    )

    # Null mean line
    graphics::lines(df_bins$bin_center, df_bins$null_mean, col = "gray40", lty = 2, lwd = 1.5)

    # Observed rate line
    graphics::lines(df_bins$bin_center, df_bins$real_rate, col = "#1976D2", lwd = 2.5)
    graphics::abline(v = 0, lty = 2, col = "firebrick", lwd = 2)

    # Significant markers along top
    if (any(is_facilitation)) {
      graphics::points(df_bins$bin_center[is_facilitation],
                       rep(y_max * 0.95, sum(is_facilitation)),
                       pch = 8, col = "#2E7D32", cex = 1.1)
    }
    if (any(is_suppression)) {
      graphics::points(df_bins$bin_center[is_suppression],
                       rep(y_max * 0.90, sum(is_suppression)),
                       pch = 8, col = "#C62828", cex = 1.1)
    }

    graphics::legend(
      "topright",
      legend = c("Observed Rate", "Shuffled 95% Null Envelope", "Null Mean",
                 "Facilitation (p < 0.05)", "Suppression (p < 0.05)"),
      col    = c("#1976D2", "gray70", "gray40", "#2E7D32", "#C62828"),
      lty    = c(1, NA, 2, NA, NA),
      pch    = c(NA, 15, NA, 8, 8),
      lwd    = c(2.5, NA, 1.5, NA, NA),
      bty    = "n", cex = 0.8
    )
  }

  invisible(list(
    bins_table           = df_bins,
    significant_windows  = df_sig
  ))
}
