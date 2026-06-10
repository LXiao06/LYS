# LYS

**LYS** (Label Your Song) is a lightweight R package for recording
management, session assignment, and simultaneously tracking the
interaction of multiple vocal components across behavioral experiments.

It is designed to:

- scan a directory of WAV files and parse recording metadata from
  filenames
- assign recording sessions from inter-file gaps
- summarize recording days and sessions when the object is created
- store and manage template definitions for labeling vocal components
- detect and annotate vocalizations across sessions in parallel

Documentation and tutorials are available on the [package
website](https://lxiao06.github.io/LYS/)

## Installation

LYS is currently available from GitHub. You can install it using the
[`remotes`](https://remotes.r-lib.org/) or
[`devtools`](https://devtools.r-lib.org/) package:

``` r

# Install remotes if you don't have it
install.packages("remotes")

# Install LYS from GitHub
remotes::install_github("LXiao06/LYS")
```

### Dependencies

LYS requires R ≥ 4.1.0 and imports the `ASAP` package. The following
packages are suggested for full functionality:

| Package                 | Purpose                               |
|-------------------------|---------------------------------------|
| `seewave`               | Audio analysis and visualization      |
| `tuneR`                 | Reading and writing WAV files         |
| `monitoR`               | Template-based vocalization detection |
| `gifski`                | Animated session visualizations       |
| `pbapply` / `pbmcapply` | Progress bars for parallel processing |
| `viridisLite`           | Color scales for plots                |

Install suggested packages as needed:

``` r

install.packages(c("seewave", "tuneR", "monitoR", "gifski",
                   "pbapply", "pbmcapply", "viridisLite"))
```
