# Extract vocalization motifs

A generic function to extract fixed-window motif segments centered
around template detection timestamps. For a `lys` object, detection hits
are mapped to enclosing labeled bouts (e.g. from
[`annotate_vocalizations`](https://lxiao06.github.io/LYS/dev/reference/annotate_vocalizations.md)
or
[`map_vocalization_sessions`](https://lxiao06.github.io/LYS/dev/reference/map_vocalization_sessions.md))
so each extracted motif inherits the behavioral condition (e.g.
`"PD SongBout"` vs. `"UD SongBout"`).

## Usage

``` r
find_motif(x, ...)

# Default S3 method
find_motif(x, ...)

# S3 method for class 'lys'
find_motif(
  x,
  template_name,
  pre_time = NULL,
  lag_time = NULL,
  bout_labels = c("PD SongBout", "UD SongBout"),
  label_map = NULL,
  tolerance_sec = 0.05,
  check_duration = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A `lys` object or a data frame containing detection timestamps.

- ...:

  Additional arguments passed to specific methods.

- template_name:

  Character. Name of the template in `x$templates$template_matches`.

- pre_time:

  Numeric. Time in seconds before the detection point.

- lag_time:

  Numeric. Time in seconds after the detection point.

- bout_labels:

  Character vector of bout labels to retain. If `NULL`, all bouts are
  included. Default is `c("PD SongBout", "UD SongBout")`.

- label_map:

  Optional named character vector to re-map bout labels (e.g.,
  `c("PD SongBout" = "PD", "UD SongBout" = "UD")`).

- tolerance_sec:

  Numeric. Tolerance in seconds when matching detection timestamps to
  bout boundaries. Default `0.05`.

- check_duration:

  Logical. If `TRUE` (default), verifies motif bounds against audio file
  duration on disk if the files exist.

- verbose:

  Logical. Whether to print extraction progress. Default `TRUE`.

## Value

For a `lys` object, the updated object containing extracted motifs in
`lys$motifs` (returned invisibly). For the default method, a data frame
of motif segments.

## Examples

``` r
if (FALSE) { # \dontrun{
# Extract motifs for PD and UD song bouts
lys <- find_motif(
  lys,
  template_name = "song",
  pre_time = 0.4,
  lag_time = 0.6,
  bout_labels = c("PD SongBout", "UD SongBout"),
  label_map = c("PD SongBout" = "PD", "UD SongBout" = "UD")
)
head(lys$motifs)
} # }
```
