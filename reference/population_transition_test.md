# Population-level test for directional transition asymmetry

Tests whether the `label1 -> label2` direction is consistently more
frequent than `label2 -> label1` *across animals*, using the animal as
the unit of replication. This avoids the two main pitfalls of naive
pooling:

1.  **Unequal weight** - animals with more events would dominate naive
    pooling. Here every animal contributes exactly one data point (its
    asymmetry score or proportion).

2.  **Heterogeneity blindness** - a single animal driving the effect is
    visible in the per-animal plot and would not survive a sign test or
    Wilcoxon signed-rank test.

Three complementary tests are reported:

- **Sign test (binomial)**: across \\N\\ animals, how many show
  `n_1to2 > n_2to1`? Binomial test against \\p = 0.5\\. Only animals
  with at least one pair in either direction are counted.

- **Wilcoxon signed-rank test**: on per-animal `prop_1to2` values
  against the null value of 0.5. This uses both the sign and the
  magnitude of each animal's bias.

- **One-sample t-test** (if \\N \ge 3\\): on `prop_1to2` against 0.5.
  Sensitive when proportions are approximately normal.

## Usage

``` r
population_transition_test(
  pool,
  label1,
  label2,
  window_sec = 120,
  label_col = "vocalization_label",
  start_col = "session_relative_start",
  end_col = "session_relative_end",
  session_col = "session_label",
  require_adjacent = TRUE,
  plot = TRUE,
  verbose = TRUE
)
```

## Arguments

- pool:

  Data frame from
  [`pool_lys_session_maps`](https://lxiao06.github.io/LYS/reference/pool_lys_session_maps.md).

- label1:

  Character. First (hypothesised trigger) label.

- label2:

  Character. Second (hypothesised response) label.

- window_sec:

  Numeric. Matching window (s). Default `120`.

- label_col, start_col, end_col, session_col:

  Column names passed to
  [`compute_onset_latency`](https://lxiao06.github.io/LYS/reference/compute_onset_latency.md).

- require_adjacent:

  Logical. Default `TRUE`.

- plot:

  Logical. Draw the per-animal dot plot. Default `TRUE`.

- verbose:

  Logical. Print a summary table and test results. Default `TRUE`.

## Value

Invisibly, a list with:

- `per_bird`:

  Per-animal transition counts, asymmetries, proportions, and latency
  summaries.

- `sign_test`:

  Result of [`binom.test()`](https://rdrr.io/r/stats/binom.test.html).

- `wilcox_test`:

  Result of [`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html).

- `t_test`:

  Result of [`t.test()`](https://rdrr.io/r/stats/t.test.html), or `NULL`
  if \\N \< 3\\.

- `n_birds_with_pairs`:

  Number of animals contributing to tests.

## Examples

``` r
if (FALSE) { # \dontrun{
pool <- pool_lys_session_maps(lys_list = list(O703 = lys_O703, O704 = lys_O704))
res  <- population_transition_test(pool, "BeggingCall", "SongBout", window_sec = 120)
res$sign_test
res$wilcox_test
} # }
```
