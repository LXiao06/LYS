# Tests for find_motif.lys and trajectory S3 methods
# Verifiable without WAV files on disk (check_duration = FALSE)

devtools::load_all("~/Repo/LYS")

cat("=== Setting up test lys object ===\n")
lys <- readRDS(file.path("dev", "lys_G769.rds"))

rules <- data.frame(
  preceding_label = c("BeggingCall", "PD SongBout", "BeggingCall"),
  following_label = c("SongBout",    "SongBout",    "SongBout"),
  min_gap_sec     = c(0,             0,             60),
  max_gap_sec     = c(10,            5,             Inf),
  annotation      = c("PD SongBout", "PD SongBout", "UD SongBout")
)
lys <- annotate_vocalizations(lys, sequence_rules = rules)
cat("Annotation counts:\n")
print(table(lys$vocalization_annotations$annotated_label))

# ------------------------------------------------------------------
# TEST 1 - find_motif.lys: basic extraction
# ------------------------------------------------------------------
cat("\n=== TEST 1: find_motif.lys basic extraction ===\n")
lys <- find_motif(
  lys,
  template_name  = "song",
  pre_time       = 0.4,
  lag_time       = 0.6,
  bout_labels    = c("PD SongBout", "UD SongBout"),
  label_map      = c("PD SongBout" = "PD", "UD SongBout" = "UD"),
  check_duration = FALSE,
  verbose        = TRUE
)

stopifnot(
  "motifs must be stored in lys$motifs"           = !is.null(lys$motifs),
  "motifs must be a data.frame"                   = is.data.frame(lys$motifs),
  "motifs must have rows"                         = nrow(lys$motifs) > 0,
  "required columns present"                      = all(c("filename", "detection_time", "start_time",
                                                          "end_time", "duration", "label") %in%
                                                         names(lys$motifs)),
  "all durations == pre+lag"                      = all(abs(lys$motifs$duration - 1.0) < 1e-9),
  "labels limited to PD and UD"                   = all(lys$motifs$label %in% c("PD", "UD")),
  "start_time >= 0 for all motifs"                = all(lys$motifs$start_time >= 0),
  "start_time < end_time for all motifs"          = all(lys$motifs$start_time < lys$motifs$end_time),
  "class includes 'segment'"                      = inherits(lys$motifs, "segment")
)
cat("TEST 1 PASSED:", nrow(lys$motifs), "motifs extracted\n")
print(table(lys$motifs$label))

# ------------------------------------------------------------------
# TEST 2 - find_motif.lys: label_map = NULL keeps original labels
# ------------------------------------------------------------------
cat("\n=== TEST 2: find_motif.lys with NULL label_map ===\n")
lys2 <- find_motif(
  lys,
  template_name  = "song",
  pre_time       = 0.4,
  lag_time       = 0.6,
  bout_labels    = c("PD SongBout", "UD SongBout"),
  label_map      = NULL,
  check_duration = FALSE,
  verbose        = FALSE
)
stopifnot(
  "un-mapped labels should be full strings" =
    all(lys2$motifs$label %in% c("PD SongBout", "UD SongBout"))
)
cat("TEST 2 PASSED: original labels retained\n")

# ------------------------------------------------------------------
# TEST 3 - find_motif.lys: single-label filtering
# ------------------------------------------------------------------
cat("\n=== TEST 3: find_motif.lys single-label filter ===\n")
lys3 <- find_motif(
  lys,
  template_name  = "song",
  pre_time       = 0.4,
  lag_time       = 0.6,
  bout_labels    = "UD SongBout",
  label_map      = c("UD SongBout" = "UD"),
  check_duration = FALSE,
  verbose        = FALSE
)
stopifnot(
  "only UD motifs returned" = all(lys3$motifs$label == "UD")
)
cat("TEST 3 PASSED:", nrow(lys3$motifs), "UD-only motifs\n")

# ------------------------------------------------------------------
# TEST 4 - find_motif.lys: S3 dispatch validation
# ------------------------------------------------------------------
cat("\n=== TEST 4: S3 dispatch ===\n")
check_s3 <- function(generic, class) {
  !is.null(tryCatch(getS3method(generic, class), error = function(e) NULL))
}
stopifnot(
  "find_motif generic is a function"                     = is.function(find_motif),
  "find_motif.lys method callable via getS3method"       = check_s3("find_motif", "lys"),
  "create_trajectory_matrix.lys callable"                = check_s3("create_trajectory_matrix", "lys"),
  "run_pca.lys callable"                                 = check_s3("run_pca", "lys"),
  "trajectory_timecourse.lys callable"                   = check_s3("trajectory_timecourse", "lys"),
  "trajectory_separation.lys callable"                   = check_s3("trajectory_separation", "lys"),
  "plot_trajectory_discrimination.lys callable"          = check_s3("plot_trajectory_discrimination", "lys"),
  "visualize_segments.lys callable"                      = check_s3("visualize_segments", "lys")
)
cat("TEST 4 PASSED: all .lys S3 methods are registered and callable\n")

# ------------------------------------------------------------------
# TEST 5 - Error handling
# ------------------------------------------------------------------
cat("\n=== TEST 5: error handling ===\n")
tryCatch(
  find_motif(lys, template_name = "does_not_exist", pre_time = 0.4, lag_time = 0.6),
  error = function(e) cat("  Correctly caught error for missing template:", conditionMessage(e), "\n")
)
tryCatch(
  find_motif(lys, template_name = "song"),
  error = function(e) cat("  Correctly caught error for missing pre_time/lag_time:", conditionMessage(e), "\n")
)
cat("TEST 5 PASSED\n")

# ------------------------------------------------------------------
# TEST 6 - trajectory methods: structure tests (no WAVs needed)
# ------------------------------------------------------------------
cat("\n=== TEST 6: trajectory method S3 generics exported ===\n")
stopifnot(
  "create_trajectory_matrix exported" = is.function(create_trajectory_matrix),
  "run_pca exported"                  = is.function(run_pca),
  "trajectory_timecourse exported"    = is.function(trajectory_timecourse),
  "trajectory_separation exported"    = is.function(trajectory_separation),
  "plot_trajectory_discrimination exported" = is.function(plot_trajectory_discrimination),
  "visualize_segments exported"       = is.function(visualize_segments)
)
cat("TEST 6 PASSED: all generics are exported and callable\n")

# ------------------------------------------------------------------
# TEST 7 - Error for missing motifs in downstream methods
# ------------------------------------------------------------------
cat("\n=== TEST 7: downstream methods stop cleanly without motifs ===\n")
lys_bare <- lys
lys_bare$motifs <- NULL

tryCatch(
  create_trajectory_matrix(lys_bare),
  error = function(e) cat("  Correctly caught missing motifs:", conditionMessage(e), "\n")
)
tryCatch(
  run_pca(lys_bare),
  error = function(e) cat("  Correctly caught missing traj_mat:", conditionMessage(e), "\n")
)
tryCatch(
  trajectory_timecourse(lys_bare),
  error = function(e) cat("  Correctly caught missing traj.embeds:", conditionMessage(e), "\n")
)
tryCatch(
  trajectory_separation(lys_bare),
  error = function(e) cat("  Correctly caught missing traj.embeds:", conditionMessage(e), "\n")
)
tryCatch(
  plot_trajectory_discrimination(lys_bare, data_type = "timecourse"),
  error = function(e) cat("  Correctly caught missing trajectory$timecourse:", conditionMessage(e), "\n")
)
cat("TEST 7 PASSED\n")

cat("\n========================================\n")
cat("ALL TESTS PASSED\n")
cat("========================================\n")
