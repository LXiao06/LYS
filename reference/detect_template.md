# Run template-based vocalization detection

Runs correlation-based template detection across a WAV file or all files
in a LYS object, using the monitoR package. Detections are stored in
`lys$templates$template_matches` (LYS method).

## Usage

``` r
detect_template(x, ...)

# Default S3 method
detect_template(
  x,
  template,
  cor.method = "pearson",
  proximity_window = NULL,
  plot = TRUE,
  save_plot = FALSE,
  plot_dir = NULL,
  ...
)

# S3 method for class 'lys'
detect_template(
  x,
  template_name = NULL,
  session = NULL,
  indices = NULL,
  threshold = NULL,
  cores = NULL,
  cor.method = "pearson",
  proximity_window = NULL,
  save_plot = TRUE,
  plot_percent = 100,
  output_dir = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A `lys` object or a path to a WAV file

- ...:

  Additional arguments (currently unused)

- template:

  A `TemplateList` object or a list of them (default method), or `NULL`
  to use all stored templates (LYS method)

- cor.method:

  Character. Correlation method passed to
  [`monitoR::corMatch()`](https://rdrr.io/pkg/monitoR/man/templateMatching.html).
  Default `"pearson"`

- proximity_window:

  Numeric or NULL. Within this window (seconds), only the
  highest-scoring detection is kept per template. `NULL` disables
  filtering

- plot:

  Logical. Draw detection plots interactively. Default `TRUE`

- save_plot:

  Logical. Save detection plots to disk. Default `FALSE` (default
  method), `TRUE` (LYS method)

- plot_dir:

  Character. Directory for saved plots (default method only)

- template_name:

  Character vector. Names of stored templates to run; `NULL` runs all
  (LYS method only)

- session:

  Character or integer vector. Sessions to restrict detection to (LYS
  method only)

- indices:

  Integer vector. File indices within a session to process (LYS method
  only)

- threshold:

  Named numeric vector or scalar to override stored template cutoffs
  (LYS method only)

- cores:

  Integer. Number of parallel workers (LYS method only)

- plot_percent:

  Numeric. Percentage of files to plot (LYS method only)

- output_dir:

  Character. Root output directory (LYS method only)

- verbose:

  Logical. Print progress messages. Default `TRUE` (LYS method only)

## Value

For `lys` input, the updated object (invisibly). For a WAV path, a data
frame of detections or `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Single WAV file
hits <- detect_template("song.wav", template = tmpl)

# LYS object (uses all stored templates)
lys <- detect_template(lys)
} # }
```
