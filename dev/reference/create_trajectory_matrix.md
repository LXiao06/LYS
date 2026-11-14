# Create Spectrogram Matrices for Trajectory Analysis

Creates sliding-window spectrogram matrices from segmented motifs or
bouts for trajectory analysis.

## Usage

``` r
create_trajectory_matrix(x, ...)

# Default S3 method
create_trajectory_matrix(x, ...)

# S3 method for class 'lys'
create_trajectory_matrix(
  x,
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
  ...
)
```

## Arguments

- x:

  A `lys` object or a data frame containing segment information.

- ...:

  Additional arguments passed to specific methods.

- labels:

  Optional character vector of labels to include.

- window_size:

  Numeric. Size of sliding window in seconds. Default `0.1`.

- step_size:

  Numeric. Step size between sliding windows in seconds. Default
  `0.005`.

- wl:

  Integer. Window length for spectrogram. Default `128`.

- ovlp:

  Numeric. Overlap percentage. Default `50`.

- fftw:

  Logical. Whether to use FFTW. Default `TRUE`.

- flim:

  Numeric vector of length 2. Frequency limits in kHz. Default
  `c(1, 12)`.

- cores:

  Integer. Number of processing cores. If `NULL`, uses
  `parallel::detectCores() - 1`.

- balanced:

  Logical. Whether to balance sample counts across groups. Default
  `FALSE`.

- sample_percent:

  Numeric. Percentage of segments to sample (1–100). Default `100`.

- max_samples_per_label:

  Optional integer. Maximum samples per label. Default `NULL`.

- seed:

  Integer. Random seed for sampling. Default `222`.

- verbose:

  Logical. Whether to print progress. Default `TRUE`.

## Value

For a `lys` object, the updated object with trajectory matrix and
sliding window embeddings stored in `lys$trajectory` (returned
invisibly).
