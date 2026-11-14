# Plot Trajectory Discrimination Results

Visualizes trajectory variability timecourses or separation profiles
produced by
[`trajectory_timecourse`](https://lxiao06.github.io/LYS/dev/reference/trajectory_timecourse.md)
or
[`trajectory_separation`](https://lxiao06.github.io/LYS/dev/reference/trajectory_separation.md).

## Usage

``` r
plot_trajectory_discrimination(x, ...)

# Default S3 method
plot_trajectory_discrimination(x, ...)

# S3 method for class 'lys'
plot_trajectory_discrimination(
  x,
  data_type = c("timecourse", "separation"),
  score_col = NULL,
  palette = "Set1",
  alpha_ribbon = 0.25,
  ...
)
```

## Arguments

- x:

  A `lys` object or result list.

- ...:

  Additional arguments passed to specific methods.

- data_type:

  Character. Which result to plot: `"timecourse"` or `"separation"`.
  Default `"timecourse"`.

- score_col:

  Optional character specifying which separation metric to plot
  (`"d_prime"`, `"centroid_dist"`, `"bhattacharyya"`, `"silhouette"`).

- palette:

  Character. RColorBrewer palette name. Default `"Set1"`.

- alpha_ribbon:

  Numeric. Transparency of confidence interval ribbons. Default `0.25`.

## Value

A patchwork/ggplot object, returned invisibly.
