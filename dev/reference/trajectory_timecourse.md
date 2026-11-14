# Trajectory Timecourse Analysis

Quantifies continuous, time-resolved trajectory variability along the
temporal progression of motif renditions across conditions.

## Usage

``` r
trajectory_timecourse(x, ...)

# Default S3 method
trajectory_timecourse(x, ...)

# S3 method for class 'lys'
trajectory_timecourse(
  x,
  dims = c("PC1", "PC2"),
  labels = NULL,
  metric = c("centroid_dist", "trace", "mad"),
  min_coverage = 0.5,
  time_digits = 6,
  smooth_span = NULL,
  stats = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A `lys` object or trajectory embeddings data frame.

- ...:

  Additional arguments passed to specific methods.

- dims:

  Character vector of dimension columns to use. Default
  `c("PC1", "PC2")`.

- labels:

  Optional character vector of labels to include. If `NULL`, all labels
  are used.

- metric:

  Character. Dispersion metric: `"centroid_dist"` (default), `"trace"`,
  or `"mad"`.

- min_coverage:

  Numeric in (0, 1\]. Minimum fraction of renditions required at a time
  step. Default `0.5`.

- time_digits:

  Integer. Rounding precision for time alignment. Default `6`.

- smooth_span:

  Optional numeric. LOESS smoothing span. Default `NULL`.

- stats:

  Logical. Whether to run pointwise statistical comparisons. Default
  `TRUE`.

- verbose:

  Logical. Whether to print progress. Default `TRUE`.

## Value

For a `lys` object, the updated object with timecourse results stored in
`lys$trajectory$timecourse` (returned invisibly).
