library(LYS)

input_dir <- tempfile("sap-input (B19)-")
output_dir <- tempfile("sap-output-")
dir.create(file.path(input_dir, "661"), recursive = TRUE)
dir.create(file.path(input_dir, "662"), recursive = TRUE)
file.create(file.path(input_dir, "661", "O1002_July_08_2026_39377990.wav"))
file.create(file.path(input_dir, "662", "Y702_46169.57106419_5_27_15_51_46.wav"))

manifest <- preprocess_sap_wavs(input_dir, output_dir, subdirs = "661")
stopifnot(
  nrow(manifest) == 1L,
  manifest$converted,
  basename(manifest$destination) == "O1002_46211.45576377_7_8_10_56_17.wav",
  file.exists(manifest$destination)
)

unlink(input_dir, recursive = TRUE)
unlink(output_dir, recursive = TRUE)
