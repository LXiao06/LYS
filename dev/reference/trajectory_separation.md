# Trajectory Separation and Discriminability Analysis

Quantifies geometric separation, probabilistic divergence, and
statistical discriminability between motif trajectories across
experimental conditions.

## Usage

``` r
trajectory_separation(x, ...)

# Default S3 method
trajectory_separation(x, ...)

# S3 method for class 'lys'
trajectory_separation(
  x,
  dims = c("PC1", "PC2"),
  labels = NULL,
  metrics = c("d_prime", "centroid_dist", "bhattacharyya", "silhouette"),
  min_coverage = 0.5,
  time_digits = 6,
  stats = TRUE,
  n_perm = 500,
  seed = 222,
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

  Optional character vector of labels to include.

- metrics:

  Character vector of metrics: `"d_prime"`, `"centroid_dist"`,
  `"bhattacharyya"`, `"silhouette"`.

- min_coverage:

  Numeric in (0, 1\]. Minimum rendition coverage per time step. Default
  `0.5`.

- time_digits:

  Integer. Rounding precision for time alignment. Default `6`.

- stats:

  Logical. Whether to run permutation tests. Default `TRUE`.

- n_perm:

  Integer. Number of permutations. Default `500`.

- seed:

  Integer. Random seed for permutations. Default `222`.

- verbose:

  Logical. Whether to print progress. Default `TRUE`.

## Value

For a `lys` object, the updated object with separation results stored in
`lys$trajectory$separation` (returned invisibly).
