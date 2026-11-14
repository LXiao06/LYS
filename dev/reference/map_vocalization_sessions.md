# Map labeled vocalizations across recording sessions

Map labeled vocalizations across recording sessions

## Usage

``` r
map_vocalization_sessions(
  lys,
  labels = NULL,
  label_col = "vocalization_label",
  drop_labels = "TBD",
  session = NULL,
  plot = TRUE,
  save_plot = FALSE,
  output_dir = NULL,
  plot_file = "vocalization_session_map.png",
  map_file = "vocalization_session_map.csv",
  colors = NULL,
  scale_bar_seconds = NULL,
  tz = "UTC",
  verbose = TRUE
)
```

## Arguments

- lys:

  A `lys` object with labels in `lys$vocalizations`.

- labels:

  Character vector of labels to include. `NULL` uses all.

- label_col:

  Character. Column in `lys$vocalizations` holding labels. Default
  `"vocalization_label"`.

- drop_labels:

  Character vector of label values to exclude.

- session:

  Session ID(s) or label(s) to restrict to. `NULL` = all.

- plot:

  Logical. Draw the map plot. Default `TRUE`.

- save_plot:

  Logical. Save the map plot to disk. Default `FALSE`.

- output_dir:

  Character. Output directory; `NULL` uses default.

- plot_file:

  Character. Plot filename.

- map_file:

  Character. CSV filename.

- colors:

  Named character vector mapping labels to colors.

- scale_bar_seconds:

  Numeric. Scale-bar length in seconds; `NULL` chooses automatically.

- tz:

  Character. Timezone. Default `"UTC"`.

- verbose:

  Logical. Print progress messages. Default `TRUE`.

## Value

The updated `lys` object (invisibly).
