# Create a vocalization template

Creates a correlation-based vocalization template from a WAV file using
the monitoR package. For a `lys` object, the template is stored in the
registry and can later be applied with
[`detect_template`](https://lxiao06.github.io/LYS/reference/detect_template.md).

## Usage

``` r
create_template(x, ...)

# Default S3 method
create_template(
  x,
  template_name,
  start_time = NULL,
  end_time = NULL,
  freq_min = 0,
  freq_max = 15,
  threshold = 0.6,
  write_template = FALSE,
  template_dir = NULL,
  ...
)

# S3 method for class 'lys'
create_template(
  x,
  template_name,
  template_type,
  wav_path = NULL,
  index = NULL,
  start_time = NULL,
  end_time = NULL,
  freq_min = 0,
  freq_max = 15,
  threshold = 0.6,
  write_template = FALSE,
  output_dir = NULL,
  notes = NA_character_,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A `lys` object or a path to a WAV file

- ...:

  Additional arguments forwarded to
  [`monitoR::makeCorTemplate()`](https://rdrr.io/pkg/monitoR/man/makeTemplate.html)

- template_name:

  Character. Unique name for the template

- start_time:

  Numeric. Start time (seconds) of the template segment. Must be
  supplied together with `end_time`

- end_time:

  Numeric. End time (seconds) of the template segment

- freq_min:

  Numeric. Lower frequency bound in kHz. Default `0`

- freq_max:

  Numeric. Upper frequency bound in kHz. Default `15`

- threshold:

  Numeric. Correlation score cutoff in \\\[0, 1\]\\. Default `0.6`

- write_template:

  Logical. Write the template to disk. Default `FALSE`

- template_dir:

  Character. Directory for saved templates; `NULL` uses the WAV file
  directory (default method) or a standard templates subdirectory (LYS
  method)

- template_type:

  Character. Template type label; must be one of
  `lys$templates$allowed_types` (LYS method only)

- wav_path:

  Character. Path to the WAV file; overrides `index` (LYS method only)

- index:

  Integer. Metadata row index of the WAV file to use when `wav_path` is
  `NULL` (LYS method only)

- output_dir:

  Character. Root output directory; `NULL` uses the default location
  (LYS method only)

- notes:

  Character. Optional notes stored in the template registry (LYS method
  only)

- verbose:

  Logical. Print a creation message. Default `TRUE` (LYS method only)

## Value

For `lys` input, the updated object (invisibly). For a WAV path, a
`TemplateList` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Single WAV file
tmpl <- create_template("song.wav",
  template_name = "SongBout1",
  start_time = 1.2, end_time = 2.5
)

# LYS object
lys <- create_template(lys,
  template_name = "SongBout1",
  template_type = "song_bout",
  index = 3
)
} # }
```
