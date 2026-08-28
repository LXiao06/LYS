# Peri-Event Time Histogram (PETH) and Multi-Scale Triggering Analysis

Tests whether `trigger_label` actively triggers `response_label` by
resolving the response probability at fine temporal resolution before
and after each trigger event.

A fixed wide window (such as 120 s) can dilute a sharp, short-latency
triggering effect (e.g. a vocal response within 2-10 s) with baseline
noise. `population_peth()` solves this via two complementary analyses:

1.  **Peri-Event Time Histogram (PETH / Cross-Correlogram)**: Aligns all
    response events around trigger offsets in fine time bins from
    `-time_range` to `+time_range` (e.g. -60 s to +60 s). Directly
    contrasts the pre-trigger baseline (\$t \< 0\$) against the
    post-trigger response (\$t \> 0\$).

2.  **Multi-Scale Window Sweep**: Evaluates the rate ratio and
    pre-vs-post contrast across multiple window sizes (e.g. 5, 10, 20,
    30, 60, 120 s). If true triggering occurs, the rate ratio will peak
    sharply at short timescales.

3.  **Local Pre vs. Post Statistical Contrast**: Compares response rate
    in \$0, +W\$ directly to \$-W, 0\$ for each animal, controlling for
    non-stationary baseline activity.

## Usage

``` r
population_peth(
  pool,
  trigger_label,
  response_label,
  align_to = c("dual", "offset", "onset"),
  time_range = 60,
  bin_sec = 2,
  sweep_windows = c(5, 10, 20, 30, 60, 120),
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

- align_to:

  Character. Method for temporal alignment:

  - `"dual"` (Default, recommended): Uses **Onset alignment for
    pre-event baseline** (\$t \< 0\$ is relative to trigger start) and
    **Offset alignment for post-event response** (\$t \> 0\$ is relative
    to trigger end). This completely removes the trigger's duration from
    the analysis, avoiding any zero-rate detection blanking artifact.

  - `"offset"`: Aligns all events to trigger offset (end).

  - `"onset"`: Aligns all events to trigger onset (start).

- time_range:

  Numeric. Pre- and post-trigger time span (s). Default `60`.

- bin_sec:

  Numeric. PETH bin width (s). Default `2`.

- sweep_windows:

  Numeric vector of window sizes (s) to evaluate. Default
  `c(5, 10, 20, 30, 60, 120)`.

- label_col, start_col, end_col, session_col:

  Column names.

- plot:

  Logical. Draw the PETH and timescale sweep plots. Default `TRUE`.

- verbose:

  Logical. Print summary and statistical tests. Default `TRUE`.

## Value

Invisibly, a list with:

- `peth_bins`:

  Data frame with bin centers, mean response rates (Hz), and SEM across
  animals.

- `window_sweep`:

  Data frame of rate ratios and pre-vs-post paired test results across
  window widths.

- `align_to`:

  Alignment anchor used.

## Examples

``` r
if (FALSE) { # \dontrun{
pool <- pool_lys_session_maps(lys_list = list(O703 = lys_O703, O704 = lys_O704))
# Dual alignment (pre-onset baseline vs post-offset response):
peth <- population_peth(pool, "BeggingCall", "SongBout", align_to = "dual")
} # }
```
