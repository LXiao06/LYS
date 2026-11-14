# Label detected vocalizations using template-match rules

Label detected vocalizations using template-match rules

## Usage

``` r
label_vocalization(
  lys,
  rules,
  default_label = "TBD",
  multi_label = FALSE,
  files = NULL,
  cores = NULL,
  save_plot = TRUE,
  save_csv = FALSE,
  plot_percent = 100,
  output_dir = NULL,
  wl = 1024,
  ovlp = 50,
  freq_range = c(3, 5),
  verbose = TRUE
)
```

## Arguments

- lys:

  A `lys` object with detected vocalizations and template matches.

- rules:

  Data frame with columns `template_type`, `label`, `min_matches`, and
  `priority`.

- default_label:

  Character. Label assigned when no rule matches. Default `"TBD"`.

- multi_label:

  Logical. When `TRUE`, vocalizations that satisfy multiple rules
  receive a compound label (e.g. `"SongBout;BeggingCall"`) instead of
  only the highest-priority label. Default `FALSE`.

- files:

  Character vector of WAV filenames to relabel. `NULL` relabels all
  vocalizations. Other rows keep their existing labels.

- cores:

  Integer. Number of parallel workers. `NULL` auto-detects.

- save_plot:

  Logical. Save review spectrograms to disk. Default `TRUE`.

- save_csv:

  Logical. Save/update labeled vocalization tables (all-sessions and
  per-session CSVs) to disk. Default `FALSE`.

- plot_percent:

  Numeric. Percentage of files to plot. Default `100`.

- output_dir:

  Character. Output directory; `NULL` uses default.

- wl:

  Integer. Spectrogram window length in samples. Default `1024`.

- ovlp:

  Numeric. Spectrogram overlap percentage. Default `50`.

- freq_range:

  Numeric vector `c(min, max)` in kHz for RMS bandpass filter. Default
  `c(3, 5)`.

- verbose:

  Logical. Print progress messages. Default `TRUE`.

## Value

The updated `lys` object (invisibly).

## Details

Rules are evaluated per vocalization by counting template detections
that fall within the vocalization's time window, then applying the first
matching rule (lowest priority number). When `multi_label = TRUE`, all
matching labels are concatenated with `";"`.

## Examples

``` r
if (FALSE) { # \dontrun{
rules <- data.frame(
  template_type = c("song_bout", "innate_call"),
  label         = c("SongBout", "BeggingCall"),
  min_matches   = c(2L, 1L)
)
lys <- label_vocalization(lys, rules = rules)
} # }
```
