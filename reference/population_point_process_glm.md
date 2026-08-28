# Point-Process GLM for Disentangling Triggering vs. Refractory Suppression

Fits a multivariate point-process Generalized Linear Model (Pillow et
al., 2008; Truccolo et al., 2005) to discretized vocalization time
series across animals.

This model simultaneously estimates:

1.  **Distant spontaneous baseline rate** (\$\$).

2.  **Self-history / Refractory filter** (\$h_self\$): The post-song
    quiet/refractory period.

3.  **Cross-coupling / Triggering filter** (\$h_pred target\$): The
    effect of a preceding begging call on singing probability at various
    time lags (\$0-5s\$, \$5-15s\$, \$15-30s\$, \$30-60s\$),
    **conditioned on the bird's own history and distant baseline**.

By controlling for self-refractoriness and the unperturbed baseline,
this test rigorously determines whether begging calls *actively trigger*
singing or whether apparent temporal sequences are an artifact of
post-song suppression.

## Usage

``` r
population_point_process_glm(
  pool,
  target_label = "SongBout",
  predictor_label = "BeggingCall",
  bin_sec = 0.5,
  predictor_lags = c(seq(0, 5, by = 0.5), 10, 20, 30, 60),
  self_lags = c(0, 5, 10, 20, 30, 60),
  show_ci = FALSE,
  label_stat = c("significance", "p_value", "odds_ratio", "none"),
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

- target_label:

  Character. Event being predicted (e.g. `"SongBout"`).

- predictor_label:

  Character. Hypothesized trigger (e.g. `"BeggingCall"`).

- bin_sec:

  Numeric. Discretization bin width in seconds. Default `1`.

- show_ci:

  Logical. Draw 95\\ Default `FALSE` to keep the trend curve clean and
  uncluttered.

- label_stat:

  Character. What to display above each point:

  - `"significance"` (Default): Shows significance stars (`"*"` \$p \<
    0.05\$, `"**"` \$p \< 0.01\$, `"***"` \$p \< 0.001\$).

  - `"p_value"`: Shows the exact \$p\$-value (e.g. `"p=0.031"`).

  - `"odds_ratio"`: Shows the numerical Odds Ratio (e.g. `"2.32x"`).

  - `"none"`: No text above points.

- plot:

  Logical. Plot the estimated coupling filters. Default `TRUE`.

- verbose:

  Logical. Print summary tables and coefficient tests. Default `TRUE`.

## Value

Invisibly, a list with:

- `glm_model`:

  The fitted `glm` object.

- `coefficients_table`:

  Data frame of estimated log-odds, odds ratios, standard errors, and
  \$p\$-values.
