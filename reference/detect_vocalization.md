# Detect vocalizations in audio

Detects vocalization bouts in a WAV file or across all files in a LYS
object using an RMS envelope threshold algorithm. A bandpass filter is
applied before detection; overlapping bouts within `gap_duration` are
merged; bouts shorter than `min_duration` are discarded.

## Usage

``` r
detect_vocalization(x, ...)

# Default S3 method
detect_vocalization(
  x,
  wl = 1024,
  ovlp = 50,
  norm_method = c("quantile", "max"),
  rms_threshold = 0.1,
  min_duration = 0.5,
  gap_duration = 0.3,
  edge_window = 0.05,
  freq_range = c(3, 5),
  plot = TRUE,
  save_plot = FALSE,
  plot_dir = NULL,
  ...
)

# S3 method for class 'lys'
detect_vocalization(
  x,
  session = NULL,
  indices = NULL,
  cores = NULL,
  save_plot = TRUE,
  save_csv = FALSE,
  plot_percent = 100,
  output_dir = NULL,
  wl = 1024,
  ovlp = 50,
  norm_method = c("quantile", "max"),
  rms_threshold = 0.2,
  min_duration = 1,
  gap_duration = 0.5,
  edge_window = 0.05,
  freq_range = c(3, 5),
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A `lys` object or a path to a WAV file

- ...:

  Additional arguments (currently unused)

- wl:

  Integer. Window length in samples for the RMS envelope. Default `1024`

- ovlp:

  Numeric. Overlap percentage between consecutive windows. Default `50`

- norm_method:

  Character. Normalisation method: `"quantile"` (default) or `"max"`

- rms_threshold:

  Numeric. RMS threshold after normalisation. Default `0.1` for WAV
  files, `0.2` for LYS objects

- min_duration:

  Numeric. Minimum bout duration in seconds. Default `0.5` for WAV
  files, `1` for LYS objects

- gap_duration:

  Numeric. Gap shorter than this (seconds) merges adjacent bouts.
  Default `0.3` for WAV files, `0.5` for LYS objects

- edge_window:

  Numeric. Duration (seconds) at the start of the file suppressed to
  avoid edge artefacts. Default `0.05`

- freq_range:

  Numeric vector `c(min, max)` in kHz for bandpass filter. Default
  `c(3, 5)`

- plot:

  Logical. Draw detection plots interactively. Default `TRUE`

- save_plot:

  Logical. Save detection plots to disk. Default `FALSE` for WAV files,
  `TRUE` for LYS objects

- plot_dir:

  Character. Directory for saved plots; `NULL` uses a default location
  relative to the input file

- session:

  Character or integer vector. Session IDs or labels to process. `NULL`
  processes all sessions (LYS method only)

- indices:

  Integer vector. Row indices within a session to process. `NULL`
  processes all files (LYS method only)

- cores:

  Integer. Number of parallel workers. `NULL` auto-detects (LYS method
  only)

- save_csv:

  Logical. Write per-session CSV result files. Default `FALSE` (LYS
  method only)

- plot_percent:

  Numeric. Percentage of files to plot when running on a LYS object.
  Default `100` (LYS method only)

- output_dir:

  Character. Root output directory; `NULL` uses a default location (LYS
  method only)

- verbose:

  Logical. Print progress messages. Default `TRUE` (LYS method only)

## Value

For `lys` input, the updated object with detections stored in
`lys$vocalizations` (invisibly). For a WAV path, a data frame of
detected vocalization bouts.

## Examples

``` r
if (FALSE) { # \dontrun{
# Single WAV file
bouts <- detect_vocalization("song.wav", rms_threshold = 0.15)

# LYS object
lys <- detect_vocalization(lys, rms_threshold = 0.2, cores = 4)
} # }
```
