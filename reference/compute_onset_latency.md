# Compute Onset Latency Distribution Between Vocalization Pairs

For every occurrence of `preceding_label`, finds the nearest subsequent
`following_label` event whose onset falls within `window_sec` of the
preceding event's offset. Returns a tidy data frame of latencies that
can be used directly for plotting or statistical analysis.

## Usage

``` r
compute_onset_latency(
  data,
  preceding_label,
  following_label,
  label_col = "vocalization_label",
  start_col = "session_relative_start",
  end_col = "session_relative_end",
  session_col = "session_label",
  window_sec = 60,
  require_adjacent = TRUE
)
```

## Arguments

- data:

  A data frame of vocalization events — typically
  `lys$vocalization_session_map` after
  [`map_vocalization_sessions()`](https://lxiao06.github.io/LYS/reference/map_vocalization_sessions.md).

- preceding_label:

  Character. Label of the first (trigger) event.

- following_label:

  Character. Label of the event whose onset latency is measured.

- label_col:

  Character. Column in `data` holding the event label. Default
  `"vocalization_label"`.

- start_col:

  Character. Column holding the event start time (seconds). Default
  `"session_relative_start"`.

- end_col:

  Character. Column holding the event end time (seconds). Default
  `"session_relative_end"`.

- session_col:

  Character. Column used to group events into independent sessions —
  pairs are only formed within the same session. Default
  `"session_label"`.

- window_sec:

  Numeric. Maximum time (seconds) from the offset of the preceding event
  to the onset of the following event. Default `60`.

- require_adjacent:

  Logical. If `TRUE` (default), an intervening event of type
  `preceding_label` between the pair invalidates the match (i.e. the
  SongBout must be the *next* relevant event after the BeggingCall). Set
  to `FALSE` to allow any matching following event within the window
  regardless of intervening events.

## Value

A data frame with one row per matched pair:

- `session`:

  Session identifier.

- `preceding_start`:

  Start time of the preceding event (s).

- `preceding_end`:

  End time (offset) of the preceding event (s).

- `following_start`:

  Start time (onset) of the following event (s).

- `following_end`:

  End time of the following event (s).

- `latency_sec`:

  Time from preceding offset to following onset (s). Always \\\geq 0\\
  and \\\leq\\ `window_sec`.

Returns an empty data frame (with the same columns) when no pairs are
found.

## Examples

``` r
if (FALSE) { # \dontrun{
# Distribution of SongBout onset latency after BeggingCall (within 60 s)
latencies <- compute_onset_latency(
  data            = lys$vocalization_session_map,
  preceding_label = "BeggingCall",
  following_label = "SongBout",
  window_sec      = 60
)

hist(latencies$latency_sec,
     breaks = 30,
     main   = "SongBout onset latency after BeggingCall",
     xlab   = "Latency (s)")

# Across multiple sessions as a density plot
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(latencies, ggplot2::aes(x = latency_sec)) +
    ggplot2::geom_histogram(binwidth = 5, fill = "#1976D2", color = "white") +
    ggplot2::facet_wrap(~ session) +
    ggplot2::labs(
      title = "BeggingCall -> SongBout onset latency",
      x     = "Latency (s)", y = "Count"
    ) +
    ggplot2::theme_minimal()
}
} # }
```
