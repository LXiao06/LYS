# Run Principal Component Analysis on Trajectory Matrix

Performs PCA on trajectory spectrogram matrices and appends PC scores
directly to trajectory embeddings.

## Usage

``` r
run_pca(x, ...)

# Default S3 method
run_pca(x, ...)

# S3 method for class 'lys'
run_pca(
  x,
  method = c("irlba", "base", "parallel"),
  n_components = 50,
  n_cores = NULL,
  diagnostic_plots = TRUE,
  scale_scores = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A `lys` object or matrix.

- ...:

  Additional arguments passed to specific methods.

- method:

  Character. PCA method (`"irlba"`, `"base"`, or `"parallel"`). Default
  `"irlba"`.

- n_components:

  Integer. Number of principal components. Default `50`.

- n_cores:

  Optional integer. Number of cores for parallel processing.

- diagnostic_plots:

  Logical. Whether to produce diagnostic plots. Default `TRUE`.

- scale_scores:

  Logical. Whether to scale PC scores. Default `FALSE`.

- verbose:

  Logical. Whether to print progress. Default `TRUE`.

## Value

For a `lys` object, the updated object with PC scores stored in
`lys$trajectory$traj.embeds` (returned invisibly).
