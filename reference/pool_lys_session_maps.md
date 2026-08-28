# Pool vocalization session maps from multiple animals

Collects `vocalization_session_map` data frames from several LYS objects
and row-binds them into a single *population-level* data frame that can
be passed directly to
[`compute_onset_latency`](https://lxiao06.github.io/LYS/reference/compute_onset_latency.md),
[`plot_reciprocal_latency`](https://lxiao06.github.io/LYS/reference/plot_reciprocal_latency.md),
[`permutation_transition_test`](https://lxiao06.github.io/LYS/reference/permutation_transition_test.md),
and
[`conditional_rate_ratio`](https://lxiao06.github.io/LYS/reference/conditional_rate_ratio.md).

Sessions are kept independent across animals: the `session_col` values
are prefixed with `<bird_id>::` so that e.g. `"Session_1"` from bird A
and `"Session_1"` from bird B are treated as separate sessions. A
`bird_id` column is also added so results can be grouped or stratified
by individual.

## Usage

``` r
pool_lys_session_maps(
  lys_list = NULL,
  rds_dir = NULL,
  bird_list = NULL,
  session_col = "session_label",
  verbose = TRUE
)
```

## Arguments

- lys_list:

  Named list of `lys` objects. Names are used as `bird_id`s. Supply
  either `lys_list` or (`rds_dir` + optionally `bird_list`), not both.

- rds_dir:

  Character. Path to a folder containing `.rds` files, one per bird,
  named `<bird_id>.rds`.

- bird_list:

  Character vector of bird IDs to load. `NULL` (default) loads all
  `.rds` files found in `rds_dir`.

- session_col:

  Character. Name of the column holding the session identifier. Default
  `"session_label"`.

- verbose:

  Logical. Print a loading/pooling summary. Default `TRUE`.

## Value

A data frame with the same columns as `lys$vocalization_session_map`
plus:

- `bird_id`:

  Character identifier for the source animal.

- `session_label`:

  Original session label prefixed with `"<bird_id>::"` to ensure global
  uniqueness.

## Inputs - two ways to supply data

**Option A - named list of already-loaded lys objects:**


    pool <- pool_lys_session_maps(
      lys_list = list(bird_A = lys_bird_A, bird_B = lys_bird_B)
    )

**Option B - a folder of `.rds` files + a bird name filter:**


    pool <- pool_lys_session_maps(
      rds_dir   = "/project/lys_objects",
      bird_list = c("O703", "O704")   # must match RDS filenames without .rds
    )

RDS files must be named `<bird_id>.rds` (e.g. `O703.rds`).
Alternatively, supply `bird_list = NULL` to load every `.rds` in the
folder.

## Examples

``` r
if (FALSE) { # \dontrun{
# Option A - named list
pool <- pool_lys_session_maps(
  lys_list = list(O703 = lys_O703, O704 = lys_O704)
)

# Option B - folder of RDS files
pool <- pool_lys_session_maps(
  rds_dir   = "/project/lys_objects",
  bird_list = c("O703", "O704")
)

# Run population-level analysis exactly as you would for one bird
latencies <- compute_onset_latency(
  data            = pool,
  preceding_label = "SongBout",
  following_label = "BeggingCall",
  window_sec      = 120
)

res <- plot_reciprocal_latency(pool, "BeggingCall", "SongBout",
                               window_sec = 120, breaks = 10)

perm <- permutation_transition_test(pool, "BeggingCall", "SongBout",
                                    window_sec = 120)
} # }
```
