# Create a LYS object from a directory of WAV files

Initializes a LYS object by scanning a directory for WAV files, parsing
their metadata, grouping files into recording sessions, and setting up
the template registry.

## Usage

``` r
create_lys_object(
  base_path,
  recursive = TRUE,
  exclude_dirs = c("templates", "plots", "temp_plots"),
  session_gap_hours = 1,
  template_types = c("song_bout", "innate_call", "pupil_beg_call"),
  tz = "UTC"
)
```

## Arguments

- base_path:

  Character. Root directory containing WAV files

- recursive:

  Logical. Search subdirectories. Default `TRUE`

- exclude_dirs:

  Character vector of subdirectory names to skip

- session_gap_hours:

  Numeric. Gap in hours that defines a new session. Default `1`

- template_types:

  Character vector of allowed template type labels

- tz:

  Character. Timezone string. Default `"UTC"`

## Value

A `lys` object.

## Examples

``` r
if (FALSE) { # \dontrun{
lys <- create_lys_object("path/to/wavs")
} # }
```
