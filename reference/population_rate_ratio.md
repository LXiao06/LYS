# Population-level Conditional Rate Ratio Analysis

Evaluates whether `response_label` vocalizations occur at a higher rate
in time windows immediately following `trigger_label` events compared to
baseline background periods across multiple animals.

For each animal, the conditional rate ratio \\\text{RR} =
\lambda\_{\text{fg}} / \lambda\_{\text{bg}}\\ is calculated along with
its 95\\ (`label1 -> label2` and `label2 -> label1`).

Population-level inference is performed using:

- **Sample-size / Inverse-variance weighted meta-analytic pooling**:
  Computes the population-level pooled rate ratio and 95\\ each animal
  by its statistical precision (downweighting low-sample birds).

- **Directional contrast test**: Tests whether the rate ratio in the
  `label1 -> label2` direction is significantly higher than
  `label2 -> label1` across animals (paired \$t\$-test on \$(RR)\$).

- **Sign test**: Tests how many animals show an elevated rate ratio
  (\$RR \> 1.0\$).

## Usage

``` r
population_rate_ratio(
  pool,
  label1,
  label2,
  window_sec = 120,
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

- label1:

  Character. First label (hypothesised trigger).

- label2:

  Character. Second label (hypothesised response).

- window_sec:

  Numeric. Foreground time window (s) following each trigger. Default
  `120`.

- label_col, start_col, end_col, session_col:

  Column names passed to the underlying rate calculation.

- plot:

  Logical. Draw a forest plot of per-animal rate ratios with 95\\
  Default `TRUE`.

- verbose:

  Logical. Print summary tables and test results. Default `TRUE`.

## Value

Invisibly, a list with:

- `per_bird`:

  Data frame of per-animal rate ratios, rates, counts, weights, and
  confidence intervals in both directions.

- `pooled_1to2`:

  List with meta-analytic pooled rate ratio, 95\\ and \$p\$-value for
  `label1 -> label2`.

- `pooled_2to1`:

  List with meta-analytic pooled rate ratio, 95\\ and \$p\$-value for
  `label2 -> label1`.

- `directional_test`:

  Paired test comparing \$(RR*1 2)\$ vs \$(RR*2 1)\$ across animals.

## Examples

``` r
if (FALSE) { # \dontrun{
pool <- pool_lys_session_maps(lys_list = list(O703 = lys_O703, O704 = lys_O704))
res  <- population_rate_ratio(pool, "BeggingCall", "SongBout", window_sec = 120)
res$pooled_1to2
} # }
```
