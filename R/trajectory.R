# Trajectory Discrimination Analysis for LYS
# Update date : Sep. 21, 2026

# -------------------------------------------------------------------------
# 1. Create Trajectory Matrix
# -------------------------------------------------------------------------

#' Create Spectrogram Matrices for Trajectory Analysis
#'
#' @description
#' Creates sliding-window spectrogram matrices from segmented motifs or bouts
#' for trajectory analysis.
#'
#' @param x A \code{lys} object or a data frame containing segment information.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return For a \code{lys} object, the updated object with trajectory matrix
#'   and sliding window embeddings stored in \code{lys$trajectory} (returned invisibly).
#'
#' @rdname create_trajectory_matrix
#' @export
create_trajectory_matrix <- function(x, ...) {
  UseMethod("create_trajectory_matrix")
}


#' @rdname create_trajectory_matrix
#' @export
create_trajectory_matrix.default <- function(x, ...) {
  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop("ASAP package is required for default create_trajectory_matrix method.", call. = FALSE)
  }
  utils::getFromNamespace("create_trajectory_matrix.default", "ASAP")(x, ...)
}


#' @rdname create_trajectory_matrix
#' @param labels Optional character vector of labels to include.
#' @param window_size Numeric. Size of sliding window in seconds. Default \code{0.1}.
#' @param step_size Numeric. Step size between sliding windows in seconds. Default \code{0.005}.
#' @param wl Integer. Window length for spectrogram. Default \code{128}.
#' @param ovlp Numeric. Overlap percentage. Default \code{50}.
#' @param fftw Logical. Whether to use FFTW. Default \code{TRUE}.
#' @param flim Numeric vector of length 2. Frequency limits in kHz. Default \code{c(1, 12)}.
#' @param cores Integer. Number of processing cores. If \code{NULL}, uses \code{parallel::detectCores() - 1}.
#' @param balanced Logical. Whether to balance sample counts across groups. Default \code{FALSE}.
#' @param sample_percent Numeric. Percentage of segments to sample (1--100). Default \code{100}.
#' @param max_samples_per_label Optional integer. Maximum samples per label. Default \code{NULL}.
#' @param seed Integer. Random seed for sampling. Default \code{222}.
#' @param verbose Logical. Whether to print progress. Default \code{TRUE}.
#'
#' @export
create_trajectory_matrix.lys <- function(x,
                                         labels = NULL,
                                         window_size = 0.1,
                                         step_size = 0.005,
                                         wl = 128,
                                         ovlp = 50,
                                         fftw = TRUE,
                                         flim = c(1, 12),
                                         cores = NULL,
                                         balanced = FALSE,
                                         sample_percent = 100,
                                         max_samples_per_label = NULL,
                                         seed = 222,
                                         verbose = TRUE,
                                         ...) {
  if (!inherits(x, "lys")) {
    stop("x must be a LYS object.", call. = FALSE)
  }

  if (is.null(x$motifs) || !is.data.frame(x$motifs) || nrow(x$motifs) == 0) {
    stop("No motifs found in LYS object. Run find_motif() first.", call. = FALSE)
  }

  motifs_df <- x$motifs

  if (!is.null(labels)) {
    motifs_df <- motifs_df[motifs_df$label %in% labels, , drop = FALSE]
    if (nrow(motifs_df) == 0) {
      stop(sprintf("No motifs match the specified 'labels': %s", paste(labels, collapse = ", ")), call. = FALSE)
    }
  }

  if (!"day_post_hatch" %in% names(motifs_df)) {
    motifs_df$day_post_hatch <- ""
  }

  # Balance groups across labels if requested (e.g. equal number of PD and UD motifs)
  if (balanced) {
    counts <- table(motifs_df$label)
    n_sample <- min(counts)
    if (verbose) {
      message(sprintf(
        "\nBalancing groups across labels (%s): subsampling each to %d motifs (seed = %d)...",
        paste(sprintf("%s: %d", names(counts), counts), collapse = ", "),
        n_sample, seed
      ))
    }
    set.seed(seed)
    sampled_rows <- lapply(names(counts), function(lbl) {
      idx <- which(motifs_df$label == lbl)
      sample(idx, size = n_sample)
    })
    motifs_df <- motifs_df[sort(unlist(sampled_rows)), , drop = FALSE]
    rownames(motifs_df) <- NULL
    class(motifs_df) <- c("segment", "data.frame")
  } else if (!is.null(max_samples_per_label)) {
    if (!is.numeric(max_samples_per_label) || max_samples_per_label <= 0) {
      stop("max_samples_per_label must be a positive integer.", call. = FALSE)
    }
    set.seed(seed)
    counts <- table(motifs_df$label)
    sampled_rows <- lapply(names(counts), function(lbl) {
      idx <- which(motifs_df$label == lbl)
      if (length(idx) > max_samples_per_label) {
        sample(idx, size = max_samples_per_label)
      } else {
        idx
      }
    })
    motifs_df <- motifs_df[sort(unlist(sampled_rows)), , drop = FALSE]
    rownames(motifs_df) <- NULL
    class(motifs_df) <- c("segment", "data.frame")
    if (verbose) {
      message(sprintf("Capped each label to at most %d motifs.", max_samples_per_label))
    }
  }

  if (!is.null(sample_percent) && sample_percent < 100) {
    if (!is.numeric(sample_percent) || sample_percent <= 0 || sample_percent > 100) {
      stop("sample_percent must be between 0 and 100.", call. = FALSE)
    }
    set.seed(seed)
    counts <- table(motifs_df$label)
    sampled_rows <- lapply(names(counts), function(lbl) {
      idx <- which(motifs_df$label == lbl)
      n_to_sample <- max(1L, ceiling(length(idx) * sample_percent / 100))
      sample(idx, size = n_to_sample)
    })
    motifs_df <- motifs_df[sort(unlist(sampled_rows)), , drop = FALSE]
    rownames(motifs_df) <- NULL
    class(motifs_df) <- c("segment", "data.frame")
    if (verbose) {
      message(sprintf("Subsampled %.1f%% from each label.", sample_percent))
    }
  }

  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop("ASAP package is required for trajectory matrix creation.", call. = FALSE)
  }

  traj_res <- utils::getFromNamespace("create_trajectory_matrix.default", "ASAP")(
    x = motifs_df,
    wav_dir = x$base_path,
    window_size = window_size,
    step_size = step_size,
    wl = wl,
    ovlp = ovlp,
    fftw = fftw,
    flim = flim,
    cores = cores,
    seed = seed,
    ...
  )

  if (is.null(x$trajectory)) x$trajectory <- list()
  x$trajectory$traj_mat <- traj_res$spectrogram_matrix
  x$trajectory$traj.embeds <- traj_res$sliding_windows
  x$trajectory$motifs <- motifs_df
  x$misc$last_modified <- Sys.time()

  if (verbose) {
    message(sprintf(
      "Generated trajectory matrix: %d windows x %d features across %d motifs.",
      nrow(traj_res$sliding_windows),
      ncol(traj_res$spectrogram_matrix),
      nrow(motifs_df)
    ))
    message("Stored in x$trajectory$traj_mat and x$trajectory$traj.embeds.")
  }

  invisible(x)
}


# -------------------------------------------------------------------------
# 2. Run PCA
# -------------------------------------------------------------------------

#' Run Principal Component Analysis on Trajectory Matrix
#'
#' @description
#' Performs PCA on trajectory spectrogram matrices and appends PC scores
#' directly to trajectory embeddings.
#'
#' @param x A \code{lys} object or matrix.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return For a \code{lys} object, the updated object with PC scores stored in
#'   \code{lys$trajectory$traj.embeds} (returned invisibly).
#'
#' @rdname run_pca
#' @export
run_pca <- function(x, ...) {
  UseMethod("run_pca")
}


#' @rdname run_pca
#' @export
run_pca.default <- function(x, ...) {
  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop("ASAP package is required for default run_pca method.", call. = FALSE)
  }
  utils::getFromNamespace("run_pca.default", "ASAP")(x, ...)
}


#' @rdname run_pca
#' @param method Character. PCA method (\code{"irlba"}, \code{"base"}, or \code{"parallel"}). Default \code{"irlba"}.
#' @param n_components Integer. Number of principal components. Default \code{50}.
#' @param n_cores Optional integer. Number of cores for parallel processing.
#' @param diagnostic_plots Logical. Whether to produce diagnostic plots. Default \code{TRUE}.
#' @param scale_scores Logical. Whether to scale PC scores. Default \code{FALSE}.
#' @param verbose Logical. Whether to print progress. Default \code{TRUE}.
#'
#' @export
run_pca.lys <- function(x,
                        method = c("irlba", "base", "parallel"),
                        n_components = 50,
                        n_cores = NULL,
                        diagnostic_plots = TRUE,
                        scale_scores = FALSE,
                        verbose = TRUE,
                        ...) {
  if (!inherits(x, "lys")) {
    stop("x must be a LYS object.", call. = FALSE)
  }

  if (is.null(x$trajectory$traj_mat)) {
    stop("No trajectory matrix found. Run create_trajectory_matrix() first.", call. = FALSE)
  }

  method <- match.arg(method)

  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop("ASAP package is required for run_pca.", call. = FALSE)
  }

  traj_mat <- x$trajectory$traj_mat

  pca_scores <- utils::getFromNamespace("run_pca.default", "ASAP")(
    x = traj_mat,
    method = method,
    n_components = n_components,
    n_cores = n_cores,
    diagnostic_plots = diagnostic_plots,
    output_scores = TRUE,
    scale_scores = scale_scores,
    ...
  )

  if (!is.null(pca_scores)) {
    pc_df <- as.data.frame(pca_scores)
    colnames(pc_df) <- paste0("PC", seq_len(ncol(pca_scores)))

    for (col in colnames(pc_df)) {
      x$trajectory$traj.embeds[[col]] <- pc_df[[col]]
    }

    x$trajectory$pca_scores <- pc_df
    x$trajectory$pca_params <- list(
      method = method,
      n_components = n_components,
      scale_scores = scale_scores
    )
  }

  if (verbose) {
    message(sprintf(
      "PCA completed (%s, %d components). Added PC scores to x$trajectory$traj.embeds.",
      method, ncol(pca_scores)
    ))
  }

  x$misc$last_modified <- Sys.time()
  invisible(x)
}


# -------------------------------------------------------------------------
# 3. Trajectory Timecourse Analysis
# -------------------------------------------------------------------------

#' Trajectory Timecourse Analysis
#'
#' @description
#' Quantifies continuous, time-resolved trajectory variability along the
#' temporal progression of motif renditions across conditions.
#'
#' @param x A \code{lys} object or trajectory embeddings data frame.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return For a \code{lys} object, the updated object with timecourse results
#'   stored in \code{lys$trajectory$timecourse} (returned invisibly).
#'
#' @rdname trajectory_timecourse
#' @export
trajectory_timecourse <- function(x, ...) {
  UseMethod("trajectory_timecourse")
}


#' @rdname trajectory_timecourse
#' @export
trajectory_timecourse.default <- function(x, ...) {
  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop("ASAP package is required for default trajectory_timecourse method.", call. = FALSE)
  }
  utils::getFromNamespace("trajectory_timecourse.default", "ASAP")(x, ...)
}


#' @rdname trajectory_timecourse
#' @param dims Character vector of dimension columns to use. Default \code{c("PC1", "PC2")}.
#' @param labels Optional character vector of labels to include. If \code{NULL}, all labels are used.
#' @param metric Character. Dispersion metric: \code{"centroid_dist"} (default), \code{"trace"}, or \code{"mad"}.
#' @param min_coverage Numeric in (0, 1]. Minimum fraction of renditions required at a time step. Default \code{0.5}.
#' @param time_digits Integer. Rounding precision for time alignment. Default \code{6}.
#' @param smooth_span Optional numeric. LOESS smoothing span. Default \code{NULL}.
#' @param stats Logical. Whether to run pointwise statistical comparisons. Default \code{TRUE}.
#' @param verbose Logical. Whether to print progress. Default \code{TRUE}.
#'
#' @export
trajectory_timecourse.lys <- function(x,
                                      dims = c("PC1", "PC2"),
                                      labels = NULL,
                                      metric = c("centroid_dist", "trace", "mad"),
                                      min_coverage = 0.5,
                                      time_digits = 6,
                                      smooth_span = NULL,
                                      stats = TRUE,
                                      verbose = TRUE,
                                      ...) {
  if (!inherits(x, "lys")) {
    stop("x must be a LYS object.", call. = FALSE)
  }

  traj_embeds <- x$trajectory$traj.embeds
  if (is.null(traj_embeds)) {
    stop("No trajectory embeddings found in x$trajectory. Run create_trajectory_matrix() and run_pca() first.", call. = FALSE)
  }

  metric <- match.arg(metric)

  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop("ASAP package is required for trajectory_timecourse.", call. = FALSE)
  }

  res <- utils::getFromNamespace("trajectory_timecourse.default", "ASAP")(
    x = traj_embeds,
    dims = dims,
    labels = labels,
    metric = metric,
    min_coverage = min_coverage,
    time_digits = time_digits,
    smooth_span = smooth_span,
    stats = stats,
    verbose = verbose,
    ...
  )

  if (is.null(x$trajectory)) x$trajectory <- list()
  x$trajectory$timecourse <- res
  x$misc$last_modified <- Sys.time()

  invisible(x)
}


# -------------------------------------------------------------------------
# 4. Trajectory Separation Analysis
# -------------------------------------------------------------------------

#' Trajectory Separation and Discriminability Analysis
#'
#' @description
#' Quantifies geometric separation, probabilistic divergence, and statistical
#' discriminability between motif trajectories across experimental conditions.
#'
#' @param x A \code{lys} object or trajectory embeddings data frame.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return For a \code{lys} object, the updated object with separation results
#'   stored in \code{lys$trajectory$separation} (returned invisibly).
#'
#' @rdname trajectory_separation
#' @export
trajectory_separation <- function(x, ...) {
  UseMethod("trajectory_separation")
}


#' @rdname trajectory_separation
#' @export
trajectory_separation.default <- function(x, ...) {
  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop("ASAP package is required for default trajectory_separation method.", call. = FALSE)
  }
  utils::getFromNamespace("trajectory_separation.default", "ASAP")(x, ...)
}


#' @rdname trajectory_separation
#' @param dims Character vector of dimension columns to use. Default \code{c("PC1", "PC2")}.
#' @param labels Optional character vector of labels to include.
#' @param metrics Character vector of metrics: \code{"d_prime"}, \code{"centroid_dist"}, \code{"bhattacharyya"}, \code{"silhouette"}.
#' @param min_coverage Numeric in (0, 1]. Minimum rendition coverage per time step. Default \code{0.5}.
#' @param time_digits Integer. Rounding precision for time alignment. Default \code{6}.
#' @param stats Logical. Whether to run permutation tests. Default \code{TRUE}.
#' @param n_perm Integer. Number of permutations. Default \code{500}.
#' @param seed Integer. Random seed for permutations. Default \code{222}.
#' @param verbose Logical. Whether to print progress. Default \code{TRUE}.
#'
#' @export
trajectory_separation.lys <- function(x,
                                      dims = c("PC1", "PC2"),
                                      labels = NULL,
                                      metrics = c("d_prime", "centroid_dist", "bhattacharyya", "silhouette"),
                                      min_coverage = 0.5,
                                      time_digits = 6,
                                      stats = TRUE,
                                      n_perm = 500,
                                      seed = 222,
                                      verbose = TRUE,
                                      ...) {
  if (!inherits(x, "lys")) {
    stop("x must be a LYS object.", call. = FALSE)
  }

  traj_embeds <- x$trajectory$traj.embeds
  if (is.null(traj_embeds)) {
    stop("No trajectory embeddings found in x$trajectory. Run create_trajectory_matrix() and run_pca() first.", call. = FALSE)
  }

  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop("ASAP package is required for trajectory_separation.", call. = FALSE)
  }

  res <- utils::getFromNamespace("trajectory_separation.default", "ASAP")(
    x = traj_embeds,
    dims = dims,
    labels = labels,
    metrics = metrics,
    min_coverage = min_coverage,
    time_digits = time_digits,
    stats = stats,
    n_perm = n_perm,
    seed = seed,
    verbose = verbose,
    ...
  )

  if (is.null(x$trajectory)) x$trajectory <- list()
  x$trajectory$separation <- res
  x$misc$last_modified <- Sys.time()

  invisible(x)
}


# -------------------------------------------------------------------------
# 5. Plot Trajectory Discrimination
# -------------------------------------------------------------------------

#' Plot Trajectory Discrimination Results
#'
#' @description
#' Visualizes trajectory variability timecourses or separation profiles
#' produced by \code{\link{trajectory_timecourse}} or \code{\link{trajectory_separation}}.
#'
#' @param x A \code{lys} object or result list.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return A patchwork/ggplot object, returned invisibly.
#'
#' @rdname plot_trajectory_discrimination
#' @export
plot_trajectory_discrimination <- function(x, ...) {
  UseMethod("plot_trajectory_discrimination")
}


#' @rdname plot_trajectory_discrimination
#' @export
plot_trajectory_discrimination.default <- function(x, ...) {
  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop("ASAP package is required for default plot_trajectory_discrimination method.", call. = FALSE)
  }
  utils::getFromNamespace("plot_trajectory_discrimination.default", "ASAP")(x, ...)
}


#' @rdname plot_trajectory_discrimination
#' @param data_type Character. Which result to plot: \code{"timecourse"} or \code{"separation"}. Default \code{"timecourse"}.
#' @param score_col Optional character specifying which separation metric to plot (\code{"d_prime"}, \code{"centroid_dist"}, \code{"bhattacharyya"}, \code{"silhouette"}).
#' @param palette Character. RColorBrewer palette name. Default \code{"Set1"}.
#' @param alpha_ribbon Numeric. Transparency of confidence interval ribbons. Default \code{0.25}.
#'
#' @export
plot_trajectory_discrimination.lys <- function(x,
                                               data_type = c("timecourse", "separation"),
                                               score_col = NULL,
                                               palette = "Set1",
                                               alpha_ribbon = 0.25,
                                               ...) {
  if (!inherits(x, "lys")) {
    stop("x must be a LYS object.", call. = FALSE)
  }

  data_type <- match.arg(data_type)
  res <- x$trajectory[[data_type]]
  if (is.null(res)) {
    stop(sprintf(
      "No %s results found in x$trajectory. Run trajectory_%s() first.",
      data_type, data_type
    ), call. = FALSE)
  }

  if (!requireNamespace("ASAP", quietly = TRUE)) {
    stop("ASAP package is required for plot_trajectory_discrimination.", call. = FALSE)
  }

  utils::getFromNamespace("plot_trajectory_discrimination.default", "ASAP")(
    x = res,
    score_col = score_col,
    palette = palette,
    alpha_ribbon = alpha_ribbon,
    ...
  )
}
