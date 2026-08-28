# Shuffled Surrogate PETH for Testing Pointwise Time-Window Significance

## Usage

``` r
population_shuffled_peth(
  pool,
  trigger_label = "BeggingCall",
  response_label = "SongBout",
  shuffle_method = c("bin_shuffle", "circular_shift"),
  align_to = c("dual", "offset", "onset"),
  time_range = 60,
  bin_sec = 2,
  n_perm = 1000L,
  seed = 42L,
  label_col = "vocalization_label",
  start_col = "session_relative_start",
  end_col = "session_relative_end",
  session_col = "session_label",
  plot = TRUE,
  verbose = TRUE
)
```

## Arguments

- pool:

  Data frame from
  [`pool_lys_session_maps`](https://lxiao06.github.io/LYS/reference/pool_lys_session_maps.md).

- trigger_label:

  Character. Label of the trigger event (e.g. `"BeggingCall"`).

- response_label:

  Character. Label of the response event (e.g. `"SongBout"`).

- shuffle_method:

  Character. Null generation method:

  - `"bin_shuffle"` (Default, fast): Permutes the time-bin rates
    independently within each bird across the evaluation window. This
    directly tests whether any specific time bin deviates from the
    bird's own average rate across the window.

  - `"circular_shift"`: Circularly shifts raw song timestamps within
    each recording session. Preserves internal inter-song intervals and
    burstiness.

- align_to:

  Character. `"dual"` (default), `"offset"`, or `"onset"`.

- time_range:

  Numeric. Pre- and post-trigger time span (s). Default `60`.

- bin_sec:

  Numeric. PETH bin width (s). Default `2`.

- n_perm:

  Integer. Number of surrogate shuffles. Default `1000`.

- seed:

  Integer. Random seed for reproducibility. Default `42`.

- label_col, start_col, end_col, session_col:

  Column names.

- plot:

  Logical. Draw the PETH with null confidence ribbon. Default `TRUE`.

- verbose:

  Logical. Print summary table of significant windows. Default `TRUE`.

## Value

Invisibly, a list with:

`bins_table`

:   Data frame with bin centers, real rate, null mean, null 95\\
    `significant_windows`Data frame summarizing all contiguous time
    intervals with statistically significant facilitation or
    suppression.
