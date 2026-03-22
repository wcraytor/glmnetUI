## R CMD check results

0 errors | 0 warnings | 1 note (new submission)

## Test environments

* macOS Sequoia 15.4 (aarch64), R 4.5.2 — 0 errors, 0 warnings, 0 notes
* Linux (Docker rocker/tidyverse, Debian), R 4.5.3 — 0 errors, 0 warnings, 1 note (new submission)
* Windows Server 2022 (win-builder release), R 4.5.2 — pending
* Windows Server 2022 (win-builder devel) — pending

## Downstream dependencies

This is a new package with no downstream dependencies.

## Changes since last submission

* Fixed report generation (PDF, Word, HTML): LaTeX compilation failures
  from unescaped underscores in variable names within `\text{}` and by
  `print(cv.relaxed)` dumping a 200K+ character `Call:` field that
  exceeded LaTeX's bufsize.
* Report contribution plots now match the interactive Contributions tab:
  scatter with fit line (numeric), box plot (factor), scatter/heatmap/3D
  persp snapshot (interactions).
* Quarto-to-rmarkdown automatic fallback for report rendering.
* HTML reports are self-contained with embedded images.
* All report axes and legends use comma-formatted numbers (no scientific
  notation).
* 29 new unit tests covering report generation (1,158 total tests).
