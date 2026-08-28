# Animal-respecting permutation test for population-level transition asymmetry

An animal-level permutation test that avoids the sample-size and
heterogeneity problems of naive pooling. At each permutation iteration,
labels are shuffled *independently within each animal*, and the *mean
per-animal asymmetry* across all animals is used as the test statistic.
This:

- Gives every animal *equal weight* regardless of how many events it
  contributed.

- Preserves the timing structure and event rates within each animal.

- Produces a null distribution that reflects what would happen if labels
  were exchangeable within animals.

## Usage

``` r
population_permutation_test(
  pool,
  label1,
  label2,
  window_sec = 120,
  n_perm = 1000L,
  label_col = "vocalization_label",
  start_col = "session_relative_start",
  end_col = "session_relative_end",
  session_col = "session_label",
  require_adjacent = TRUE,
  seed = 42L,
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

- n_perm:

  Integer. Number of permutation iterations. Default `1000`.

- label_col, start_col, end_col, session_col:

  Column names passed to
  [`compute_onset_latency`](https://lxiao06.github.io/LYS/reference/compute_onset_latency.md).

- require_adjacent:

  Logical. Default `TRUE`.

- seed:

  Integer or `NULL`. Random seed. Default `42`.

- plot:

  Logical. Draw the null-distribution histogram. Default `TRUE`.

- verbose:

  Logical. Print observed statistic and p-value. Default `TRUE`.

## Value

Invisibly, a list with:

- `observed_mean_asymmetry`:

  Mean per-animal asymmetry score in the observed data.

- `per_bird`:

  Per-animal transition counts, asymmetries, proportions, and latency
  summaries.

- `null_distribution`:

  Numeric vector of permuted mean-asymmetry scores.

- `p_value`:

  One-sided p-value: proportion of permuted scores \\\geq\\ observed.

## Examples

``` r
if (FALSE) { # \dontrun{
pool <- pool_lys_session_maps(lys_list = list(O703 = lys_O703, O704 = lys_O704))
perm <- population_permutation_test(pool, "BeggingCall", "SongBout",
                                    window_sec = 120, n_perm = 2000)
perm$p_value
} # }
```
