# LYS

**LYS** (Label Your Song) is a lightweight R package for recording management,
session assignment, and simultaneously tracking the interaction of multiple
vocal components across behavioral experiments.

It is designed to:

- scan a directory of WAV files and parse recording metadata from filenames
- assign recording sessions from inter-file gaps
- summarize recording days and sessions when the object is created
- store and manage template definitions for labeling vocal components
- detect and annotate vocalizations across sessions in parallel

See the [function reference](reference/index.html) for documentation, or start
with the [Getting Started tutorial](articles/lys_tutorial.html).

## Installation

LYS is currently available from GitHub. You can install it using the
[`remotes`](https://remotes.r-lib.org/) or
[`devtools`](https://devtools.r-lib.org/) package:

```r
# Install remotes if you don't have it
install.packages("remotes")

# Install LYS from GitHub
remotes::install_github("LXiao06/LYS")
```
