# Visualize Audio Segments or Motifs

A generic function to visualize spectrograms of extracted motif
renditions across conditions or labels. For a `lys` object, displays a
multi-panel grid comparing samples across labels side-by-side (e.g. PD
vs. UD motifs).

## Usage

``` r
visualize_segments(x, ...)

# Default S3 method
visualize_segments(x, ...)

# S3 method for class 'lys'
visualize_segments(
  x,
  labels = NULL,
  n_samples = 4,
  by_column = TRUE,
  seed = NULL,
  fft_window_size = 1024,
  overlap = 0.75,
  dark_mode = TRUE,
  ...
)
```

## Arguments

- x:

  A `lys` object or a data frame containing segment information.

- ...:

  Additional arguments passed to specific methods.

- labels:

  Optional character vector of labels to include (e.g. `c("PD", "UD")`).
  If `NULL`, all available labels are plotted.

- n_samples:

  Integer. Number of motif renditions to plot per label. Default `4`.

- by_column:

  Logical. If `TRUE` (default), displays each label in its own column,
  with rows corresponding to individual motif renditions.

- seed:

  Optional integer. Random seed for reproducible sampling.

- fft_window_size:

  Integer. FFT window size for spectrograms. Default `1024`.

- overlap:

  Numeric. Overlap percentage for spectrograms. Default `0.75`.

- dark_mode:

  Logical. Whether to use dark theme for spectrograms. Default `TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Inspect 4 PD and 4 UD motifs side-by-side
visualize_segments(lys, labels = c("PD", "UD"), n_samples = 4)
} # }
```
