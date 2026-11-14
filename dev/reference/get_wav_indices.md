# Look up metadata row indices for WAV files in a LYS object

Find indices in the LYS metadata table matching specified WAV files.

## Usage

``` r
get_wav_indices(lys, wav_files)
```

## Arguments

- lys:

  A `lys` object

- wav_files:

  Character vector of filenames, relative paths, or full paths to match
  against `lys$metadata`

## Value

An integer vector of matching row indices.

## Examples

``` r
if (FALSE) { # \dontrun{
indices <- get_wav_indices(lys, "song.wav")
} # }
```
