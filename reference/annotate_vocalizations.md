# Apply Sequence Rules to Annotate Vocalizations

Evaluates a set of sequential rules to reclassify vocalizations based on
their temporal relationship to preceding events. The rules are evaluated
chronologically for each session, allowing for complex chaining of
annotations.

## Usage

``` r
annotate_vocalizations(lys, sequence_rules, label_col = "vocalization_label")
```

## Arguments

- lys:

  A LYS object that has been processed through
  [`map_vocalization_sessions()`](https://lxiao06.github.io/LYS/reference/map_vocalization_sessions.md).

- sequence_rules:

  A data frame defining sequence-based reclassification rules. Rules are
  evaluated in the order they appear for each vocalization. Required
  columns:

  - `preceding_label` — label of the first event (e.g.,
    `"BeggingCall"`). Use `NA` to match unconditionally without a
    preceding event requirement.

  - `following_label` — label of the event to be reclassified (e.g.,
    `"SongBout"`).

  - `max_gap_sec` — maximum gap in seconds between the end of the
    preceding event and the start of the following event. Use `NA` or
    `Inf` for no upper limit.

  - `annotation` — the new label to assign to the following event (e.g.,
    `"PD SongBout"`).

  Optional column:

  - `min_gap_sec` — minimum gap in seconds. Default is `0`.

- label_col:

  Column name in the session map holding the original vocalization
  label. Default `"vocalization_label"`.

## Value

The LYS object with a new data frame `lys$vocalization_annotations`
containing the annotated vocalization events. The new labels are stored
in the `annotated_label` column, alongside original labels.

## Details

The function iterates through all events in a session chronologically.
For each event matching a `following_label` in the rules, it looks back
for the most recent event matching `preceding_label`. If the gap between
the preceding event's offset and the current event's onset falls within
`[min_gap_sec, max_gap_sec]`, the event is reclassified to `annotation`.
Because evaluation is chronological, newly reclassified events can
immediately serve as preceding events for subsequent rules.

## Examples

``` r
if (FALSE) { # \dontrun{
rules <- data.frame(
  preceding_label = c("BeggingCall", "PD SongBout", "BeggingCall"),
  following_label = c("SongBout", "SongBout", "SongBout"),
  min_gap_sec     = c(0, 0, 120),
  max_gap_sec     = c(60, 10, Inf),
  annotation      = c("PD SongBout", "PD SongBout", "UD SongBout")
)

lys <- annotate_vocalizations(lys, sequence_rules = rules)
table(lys$vocalization_annotations$annotated_label)
} # }
```
