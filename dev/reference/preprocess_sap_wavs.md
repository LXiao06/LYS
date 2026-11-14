# Pool and standardize SAP2011 WAV files

Copies WAV files from a directory tree into one directory. Files already
in the SAP output format are copied unchanged. SAP recorder temporary
files named `Bird_Month_Day_Year_milliseconds_since_midnight.wav` are
renamed to the SAP output format used by LYS.

## Usage

``` r
preprocess_sap_wavs(input_dir, output_dir, subdirs = NULL, tz = "UTC")
```

## Arguments

- input_dir:

  Character. Root directory containing WAV files.

- output_dir:

  Character. Directory to receive the pooled WAV files.

- subdirs:

  Optional character vector of paths, relative to `input_dir`, to
  include. `NULL` includes all subdirectories.

- tz:

  Character. Time zone used to construct timestamps. Default `"UTC"`.

## Value

A data frame mapping source files to their copied filenames.

## Examples

``` r
if (FALSE) { # \dontrun{
preprocess_sap_wavs("raw_recordings", "pooled_wavs", subdirs = c("661", "662"))
} # }
```
