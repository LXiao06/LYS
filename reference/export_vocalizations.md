# Export vocalization clips to per-label subdirectories

Extracts each labeled vocalization bout from its source WAV file and
saves it as a short WAV clip in a dedicated output folder, organised by
label. This is especially useful after running
[`label_vocalization`](https://lxiao06.github.io/LYS/reference/label_vocalization.md)
with `multi_label = TRUE`: compound labels such as
`"SongBout;BeggingCall"` can be exported either as a single combined
folder (`match_mode = "exact"`) or routed to the folder of each
constituent label (`match_mode = "contains"`).

## Usage

``` r
export_vocalizations(
  lys,
  rules = NULL,
  label_col = "vocalization_label",
  label_sep = ";",
  drop_labels = "TBD",
  session = NULL,
  output_dir = NULL,
  overwrite = FALSE,
  save_manifest = TRUE,
  cores = NULL,
  verbose = TRUE
)
```

## Arguments

- lys:

  A `lys` object with labeled vocalizations in `lys$vocalizations`.

- rules:

  Data frame of export rules, or `NULL` (see Details).

- label_col:

  Character. Column in `lys$vocalizations` holding the vocalization
  label. Default `"vocalization_label"`.

- label_sep:

  Character. Separator used in compound labels produced by
  `label_vocalization(multi_label = TRUE)`. Default `";"`.

- drop_labels:

  Character vector of label values to skip when `rules = NULL`. Default
  `"TBD"`.

- session:

  Character vector of session IDs or labels to restrict export to.
  `NULL` (default) exports all sessions.

- output_dir:

  Character. Root directory for exported clips. `NULL` uses a
  `vocalization_clips` folder next to `lys$base_path`.

- overwrite:

  Logical. Overwrite already-exported clips. Default `FALSE`.

- save_manifest:

  Logical. Write a CSV manifest of all exported clips to `output_dir`.
  Default `TRUE`.

- cores:

  Integer. Number of parallel workers. `NULL` auto-detects.

- verbose:

  Logical. Print progress messages. Default `TRUE`.

## Value

The `lys` object (invisibly). A `lys$misc$last_export` list is attached
with the output directory, manifest path, and per-rule clip counts.

## Export rules

The `rules` argument is a data frame with the following columns (only
`label` is required; all others have sensible defaults):

- `label`:

  Character. The target label string. For `match_mode = "exact"` this
  must exactly match `vocalization_label` (including compound labels
  like `"SongBout;BeggingCall"`). For `match_mode = "contains"` it only
  needs to appear as one component.

- `match_mode`:

  Character. `"exact"` (default) or `"contains"`. `"contains"` lets you
  route compound-labeled clips to each participating label's folder
  simultaneously.

- `folder`:

  Character. Subdirectory name under `output_dir` where matching clips
  are written. Defaults to the sanitised label.

- `pad_start_sec`:

  Numeric \\\geq 0\\. Seconds of audio added before the detected onset.
  Default `0`.

- `pad_end_sec`:

  Numeric \\\geq 0\\. Seconds of audio added after the detected offset.
  Default `0`.

If `rules = NULL` (default), one rule is generated automatically for
every unique non-`drop_labels` label found in `lys$vocalizations`, using
`match_mode = "exact"` and zero padding.

## See also

[`label_vocalization`](https://lxiao06.github.io/LYS/reference/label_vocalization.md),
[`detect_vocalization`](https://lxiao06.github.io/LYS/reference/detect_vocalization.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Default: one folder per unique label, no padding
lys <- export_vocalizations(lys, output_dir = "/path/to/clips")

# 2. Export only SongBout and BeggingCall, with 0.1 s padding
rules <- data.frame(
  label         = c("SongBout", "BeggingCall"),
  pad_start_sec = 0.1,
  pad_end_sec   = 0.1
)
lys <- export_vocalizations(lys, rules = rules)

# 3. Multi-label: route compound clips to each constituent label's folder
rules <- data.frame(
  label      = c("SongBout", "BeggingCall"),
  match_mode = "contains",     # match inside compound labels
  folder     = c("songs", "calls"),
  pad_start_sec = c(0.05, 0.1),
  pad_end_sec   = c(0.05, 0.1)
)
lys <- export_vocalizations(lys, rules = rules)

# 4. Export an exact compound label to its own folder
rules <- data.frame(
  label      = "SongBout;BeggingCall",
  match_mode = "exact",
  folder     = "co-occurring"
)
lys <- export_vocalizations(lys, rules = rules)
} # }
```
