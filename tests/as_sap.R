library(LYS)

lys <- structure(
  list(
    metadata = data.frame(
      filename = "O1002_46211.45576377_7_8_10_56_17.wav",
      relative_dir = "661",
      stringsAsFactors = FALSE
    ),
    base_path = tempdir(),
    version = "test"
  ),
  class = "lys"
)

sap <- as_sap(lys)
stopifnot(
  inherits(sap, "Sap"),
  identical(sap$base_path, lys$base_path),
  identical(sap$metadata$day_post_hatch, "661")
)
