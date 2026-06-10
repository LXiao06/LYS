# Animate Vocalization Sessions with Sliding Window

Creates a dynamic animation that displays a sliding time window for each
recording session, revealing the temporal structure of labeled
vocalizations. Sessions scroll independently — each runs to its own end.

Sequence rules allow following events to be **reclassified** visually.
For example, a SongBout that immediately follows a BeggingCall is
displayed as an "FD" (Feeding-Directed) bar in a distinct color, with an
optional arrow marking the gap between the two events.

## Usage

``` r
animate_vocalization_sessions(
  lys,
  labels = NULL,
  label_col = "vocalization_label",
  drop_labels = "TBD",
  sequence_rules = NULL,
  window_duration_min = 10,
  step_seconds = 30,
  max_session_sec = NULL,
  colors = NULL,
  fps = 10,
  output_dir = NULL,
  output_file = "vocalization_session_animation.gif",
  width = 1800,
  height = NULL,
  res = 200,
  show_progress = FALSE,
  verbose = TRUE
)
```

## Arguments

- lys:

  A LYS object that has been processed through
  [`map_vocalization_sessions()`](https://lxiao06.github.io/LYS/reference/map_vocalization_sessions.md).

- labels:

  Character vector of vocalization labels to display. If `NULL`
  (default), all labels in the session map are used.

- label_col:

  Column name in the session map holding the vocalization label. Default
  `"vocalization_label"`.

- drop_labels:

  Character vector of labels to exclude from display. Default `"TBD"`.

- sequence_rules:

  A data frame defining sequence-based reclassification rules. Each row
  describes one rule:

  - `preceding_label` — label of the first event (e.g. `"BeggingCall"`)

  - `following_label` — label of the second event (e.g. `"SongBout"`)

  - `max_gap_sec` — maximum gap in seconds between the end of the
    preceding event and the start of the following event

  - `annotation` — display label for the reclassified following event
    (e.g. `"FD"`). This label also appears in the legend.

  - `color` — (optional) color for the reclassified bar and arrow.
    Default `"#D32F2F"`.

  - `show_arrow` — (optional) logical, whether to draw an arrow in the
    gap between the two events. Default `TRUE`.

  If `NULL` (default), no reclassification is performed.

- window_duration_min:

  Numeric. Width of the sliding window in minutes. Default `10`.

- step_seconds:

  Numeric. How far the window advances per animation frame (in seconds).
  Default `30`.

- max_session_sec:

  Numeric or `NULL`. Maximum total duration (in seconds) shown for every
  session. All sessions are clipped to this value so they end at the
  same point in the animation.

  - `NULL` (default) — use the duration of the shortest session, so
    every session reaches its natural end at the same frame.

  - A positive number — clip every session at that many seconds (e.g.
    `3600` for 1 hour). Events that start after the cap are excluded;
    events that overlap the cap boundary are shown truncated.

- colors:

  Named character vector mapping *display* labels to colors. Include
  both base labels (e.g. `"SongBout"`) and annotation labels (e.g.
  `"FD"`) if you want to override auto-assigned colors. If `NULL`
  (default), colors are generated automatically.

- fps:

  Integer. Frames per second for the output animation. Default `10`.

- output_dir:

  Character. Directory where the animation file is saved. If `NULL`,
  uses the default LYS output directory.

- output_file:

  Character. File name for the saved animation. Default
  `"vocalization_session_animation.gif"`.

- width:

  Integer. Width of each animation frame in pixels. Default `1800`.

- height:

  Integer. Height of each animation frame in pixels. Default `NULL`
  (auto-calculated from the number of sessions).

- res:

  Integer. Resolution (PPI) for each frame. Default `200`.

- show_progress:

  Logical. Show the gray session progress label
  (`"session [elapsed / total]"`)? Default `FALSE`.

- verbose:

  Logical. Print progress messages? Default `TRUE`.

## Value

The LYS object (invisibly), with the animation saved to disk.

## Details

**Reclassification logic:** For each rule in `sequence_rules`, the
function scans every session for consecutive event pairs where:

1.  The first event matches `preceding_label`.

2.  The second event matches `following_label` and starts within
    `max_gap_sec` of the first event's end.

3.  No other event with `preceding_label` falls between them.

Matched *following* events are rendered in the `annotation` color
instead of their original color. If `show_arrow = TRUE`, an arrow is
also drawn across the gap.

Requires the gifski package for GIF rendering.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic: SongBout following BeggingCall shown as "FD" in red
fd_rules <- data.frame(
  preceding_label = "BeggingCall",
  following_label = "SongBout",
  max_gap_sec     = 60,
  annotation      = "FD",
  color           = "#E53935",
  show_arrow      = TRUE
)
lys <- animate_vocalization_sessions(lys, sequence_rules = fd_rules)

# Multiple rules
rules <- data.frame(
  preceding_label = c("BeggingCall", "SongBout"),
  following_label = c("SongBout",    "BeggingCall"),
  max_gap_sec     = c(60,            30),
  annotation      = c("FD",          "Response"),
  color           = c("#E53935",     "#1976D2"),
  show_arrow      = c(TRUE,          FALSE)
)
lys <- animate_vocalization_sessions(lys, sequence_rules = rules)
} # }
```
