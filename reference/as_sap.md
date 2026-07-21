# Convert a LYS object to an ASAP Sap object

Adapts LYS recording metadata for ASAP functions. In particular, LYS
relative directories become ASAP's `day_post_hatch` paths, allowing ASAP
to find WAV files below the LYS base directory.

## Usage

``` r
as_sap(x)
```

## Arguments

- x:

  A `lys` object.

## Value

An ASAP `Sap` object.

## Examples

``` r
if (FALSE) { # \dontrun{
sap <- as_sap(lys)
visualize_song(sap, n_samples = 4, random = TRUE)
} # }
```
