# Remove specified WAV file(s) from a LYS object

Deletes one or more WAV files from a `lys` object, refreshing metadata,
session summaries, day summaries, and pruning any downstream
vocalization or template match detections. This avoids the need to
recreate the entire `lys` object from scratch when corrupted, truncated,
or abnormally small files cause downstream errors.

## Usage

``` r
remove_wav_files(
  lys,
  files = NULL,
  min_size_bytes = NULL,
  reassign_sessions = FALSE,
  verbose = TRUE
)
```

## Arguments

- lys:

  A `lys` object

- files:

  Character vector of filenames, relative paths, full paths, or an
  integer vector of metadata row indices to remove

- min_size_bytes:

  Optional numeric. If provided, any files with file size in bytes below
  this threshold will also be removed

- reassign_sessions:

  Logical. If `FALSE` (default), preserves existing session IDs and
  labels, only updating intra-session counters and start/end times. If
  `TRUE`, recalculates session boundaries via
  `assign_recording_sessions()`

- verbose:

  Logical. Whether to print status and summary messages. Default `TRUE`

## Value

The updated `lys` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Remove by filename
lys <- remove_wav_files(lys, "bad_file.wav")

# Remove by full path
lys <- remove_wav_files(lys, "/path/to/bad_file.wav")

# Remove all files smaller than 10 KB
lys <- remove_wav_files(lys, min_size_bytes = 10000)
} # }
```
