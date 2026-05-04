# LYS

LYS (Label Your Song) is a lightweight R package scaffold for zebra finch
recordings. It is designed to:

- scan a directory of SAP2011-style WAV files
- parse recording metadata from filenames
- assign recording sessions from inter-file gaps
- summarize recording days and sessions when the object is created
- store template definitions for `song_bout`, `innate_call`, and `pupil_beg_call`

## Current workflow

```r
library(LYS)

lys <- create_lys_object(
  base_path = "/path/to/wav_directory",
  session_gap_hours = 1
)

summary(lys)

lys <- register_template(
  lys,
  template_name = "bird_song_bout_a",
  template_type = "song_bout",
  wav_path = "/path/to/template.wav",
  start_time = 0.5,
  end_time = 1.8
)
```

## Session logic

Files are sorted by parsed recording start time. A new session is opened when:

- the recording day changes, or
- the gap between consecutive WAV start times is at least 1 hour

This means `create_lys_object()` immediately stores:

- file-level metadata in `lys$metadata`
- day-level summaries in `lys$day_summary`
- session-level summaries in `lys$session_summary`

Because SAP2011 filenames encode WAV start times, session summaries report the
first and last recording starts observed within each session.
