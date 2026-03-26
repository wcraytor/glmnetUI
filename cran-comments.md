## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* macOS Sequoia 15.4 (aarch64), R 4.5.2 — 0 errors, 0 warnings, 0 notes
* Linux (Docker rocker/tidyverse, Debian), R 4.5.3 — 0 errors, 0 warnings, 1 note (new submission)
* Windows Server 2022 (win-builder release), R 4.5.2 — 0 errors, 0 warnings, 1 note (new submission)
* Windows Server 2022 (win-builder devel) — 0 errors, 0 warnings, 1 note (new submission)

## Resubmission

This is a resubmission addressing reviewer feedback from Benjamin Altmann:

1. **Single-quoted software names**: Package names ('shiny', 'glmnet',
   'ggplot2', 'Quarto', 'rmarkdown') are now single-quoted in Title
   and Description.
2. **References added**: Friedman, Hastie, and Tibshirani (2010)
   <doi:10.18637/jss.v033.i01> and Zou and Hastie (2005)
   <doi:10.1111/j.1467-9868.2005.00503.x> added to Description.
3. **Missing \\value tags**: Added \\value documentation to
   anovaServer.Rd, contributionsServer.Rd, correlationServer.Rd,
   and importanceServer.Rd.

## Additional changes since original submission (version bumped to 0.4.0)

* Fixed report generation (PDF, Word, HTML) for all purpose modes.
* Report contribution plots now match the interactive Contributions tab.
* Added General purpose mode support: skip first row, per-purpose settings
  persistence, hidden RCA/Sales Grid sections, enabled data download.
* Removed earth import code paths.
* 29 new unit tests (1,158 total).

## Downstream dependencies

This is a new package with no downstream dependencies.
