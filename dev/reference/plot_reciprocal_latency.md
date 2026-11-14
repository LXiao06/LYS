# Plot Reciprocal Onset Latency Distributions

Computes and plots the onset latency distributions between two
vocalization labels in both directions (`label1` -\> `label2` and
`label2` -\> `label1`) side-by-side. The histograms share the same X and
Y axis scales for easy visual comparison. A two-sample
Kolmogorov-Smirnov test and a Wilcoxon rank-sum test compare the latency
distributions; a binomial test assesses directional asymmetry in pair
counts.

## Usage

``` r
plot_reciprocal_latency(
  data,
  label1,
  label2,
  window_sec = 120,
  breaks = 20,
  label_col = "vocalization_label",
  start_col = "session_relative_start",
  end_col = "session_relative_end",
  session_col = "session_label",
  require_adjacent = TRUE
)
```

## Arguments

- data:

  A data frame of vocalization events (e.g.,
  `lys$vocalization_session_map`).

- label1:

  Character. First label.

- label2:

  Character. Second label.

- window_sec:

  Numeric. Maximum time (seconds) to search for following event. Default
  `120`.

- breaks:

  Numeric. Approximate number of histogram bins. Default `20`.

- label_col:

  Character. Column holding the event label. Default
  `"vocalization_label"`.

- start_col:

  Character. Column holding the event start time. Default
  `"session_relative_start"`.

- end_col:

  Character. Column holding the event end time. Default
  `"session_relative_end"`.

- session_col:

  Character. Column used to group events into sessions. Default
  `"session_label"`.

- require_adjacent:

  Logical. If `TRUE`, intervening events of the preceding type
  invalidate the match. Default `TRUE`.

## Value

Invisibly, a list with:

- `latencies_1_to_2`:

  Matched-pair data frame for `label1` -\> `label2`.

- `latencies_2_to_1`:

  Matched-pair data frame for `label2` -\> `label1`.

- `binom_test`:

  Result of [`binom.test()`](https://rdrr.io/r/stats/binom.test.html)
  testing whether `label1->label2` accounts for more than 50% of all
  matched pairs (one-sided, H1: p \> 0.5).

- `ks_test`:

  Result of [`ks.test()`](https://rdrr.io/r/stats/ks.test.html)
  comparing the two distributions.

- `wilcox_test`:

  Result of [`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html)
  comparing the two distributions.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- plot_reciprocal_latency(
  data       = lys$vocalization_session_map,
  label1     = "BeggingCall",
  label2     = "SongBout",
  window_sec = 120,
  breaks     = 20
)
res$binom_test
} # }
```
