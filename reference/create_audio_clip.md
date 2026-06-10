# Create an audio clip via ASAP

Thin wrapper around
[`ASAP::create_audio_clip()`](https://lxiao06.github.io/ASAP/reference/create_audio_clip.html)
that ensures the ASAP package is available before forwarding all
arguments.

## Usage

``` r
create_audio_clip(x, ...)
```

## Arguments

- x:

  Input passed to
  [`ASAP::create_audio_clip()`](https://lxiao06.github.io/ASAP/reference/create_audio_clip.html)

- ...:

  Additional arguments forwarded to
  [`ASAP::create_audio_clip()`](https://lxiao06.github.io/ASAP/reference/create_audio_clip.html)

## Value

Whatever
[`ASAP::create_audio_clip()`](https://lxiao06.github.io/ASAP/reference/create_audio_clip.html)
returns.
