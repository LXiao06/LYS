# Conditional Rate Ratio for Vocalization Transitions

Quantifies how much more (or less) likely `response_label` events are to
occur in a time window immediately following a `trigger_label` event
(the *foreground rate*) compared with the background rate of
`response_label` outside those windows. The ratio \\\lambda\_{fg} /
\lambda\_{bg}\\ \> 1 indicates that the trigger elevates the response
rate. A 95% confidence interval is obtained via Poisson rate-ratio
approximation. Both the forward (`trigger -> response`) and reverse
(`response -> trigger`) directions are computed so that directional
asymmetry can be assessed.

## Usage

``` r
conditional_rate_ratio(
  data,
  label1,
  label2,
  window_sec = 120,
  label_col = "vocalization_label",
  start_col = "session_relative_start",
  end_col = "session_relative_end",
  session_col = "session_label"
)
```

## Arguments

- data:

  A data frame of vocalization events.

- label1:

  Character. First label.

- label2:

  Character. Second label.

- window_sec:

  Numeric. Width of the foreground window (seconds) after each trigger
  event. Default `120`.

- label_col, start_col, end_col, session_col:

  Column name arguments with the same defaults as
  [`compute_onset_latency()`](https://lxiao06.github.io/LYS/reference/compute_onset_latency.md).

## Value

Invisibly, a list with two elements (`label1_to_label2` and
`label2_to_label1`), each a list containing:

- `n_triggers`:

  Number of trigger events.

- `n_fg`:

  Response events observed in foreground windows.

- `t_fg_sec`:

  Total foreground exposure time (s).

- `n_bg`:

  Response events observed outside foreground windows.

- `t_bg_sec`:

  Total background exposure time (s).

- `rate_fg`:

  Foreground rate (events / s).

- `rate_bg`:

  Background rate (events / s).

- `rate_ratio`:

  rate_fg / rate_bg.

- `ci_low,ci_high`:

  95% CI on the rate ratio.

- `p_value`:

  Two-sided p-value (exact Poisson test).

## Examples

``` r
if (FALSE) { # \dontrun{
res <- conditional_rate_ratio(
  data       = lys$vocalization_session_map,
  label1     = "BeggingCall",
  label2     = "SongBout",
  window_sec = 120
)
res$label1_to_label2$rate_ratio
} # }
```
