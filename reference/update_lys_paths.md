# Re-anchor a LYS object to a new data root

When a LYS object is created on one machine (e.g., via
[`detect_vocalization`](https://lxiao06.github.io/LYS/reference/detect_vocalization.md))
and then used on another machine where the raw recordings live under a
different root directory (but the same internal directory structure),
absolute paths stored in the object will be stale. `update_lys_paths()`
updates `lys$base_path`, `lys$metadata$file_path`, and
`lys$vocalizations$file_path` by re-joining each file's `relative_path`
with `new_base_path`.

The function validates that the new root exists and warns if any
reconstructed paths cannot be found on disk.

## Usage

``` r
update_lys_paths(lys, new_base_path, check_files = TRUE)
```

## Arguments

- lys:

  A `lys` object.

- new_base_path:

  Character. The new root directory that contains the same recording
  structure as the original `lys$base_path`.

- check_files:

  Logical. If `TRUE` (default), warn about any reconstructed paths that
  do not exist on disk.

## Value

The updated `lys` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Detect on machine A, save:
saveRDS(lys, "lys.rds")

# On machine B, where data is under a different root:
lys <- readRDS("lys.rds")
lys <- update_lys_paths(lys, new_base_path = "/new/root/O703 (B16)")
} # }
```
