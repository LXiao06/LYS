# Visualize a song via ASAP

Thin wrapper around
[`ASAP::visualize_song()`](https://lxiao06.github.io/ASAP/reference/visualize_song.html)
that ensures the ASAP package is available before forwarding all
arguments.

## Usage

``` r
visualize_song(x, ...)
```

## Arguments

- x:

  Input passed to
  [`ASAP::visualize_song()`](https://lxiao06.github.io/ASAP/reference/visualize_song.html)

- ...:

  Additional arguments forwarded to
  [`ASAP::visualize_song()`](https://lxiao06.github.io/ASAP/reference/visualize_song.html)

## Value

Whatever
[`ASAP::visualize_song()`](https://lxiao06.github.io/ASAP/reference/visualize_song.html)
returns.
