## R CMD check results

0 errors | 0 warnings | 1 note

* The note is "New submission" (the package is not currently on CRAN).

## Test environments

* macOS Tahoe 26.5 (aarch64), R 4.5.3 — local R CMD check --as-cran

## Submission notes

This version (0.5.0) introduces a shared project system ("regProj") used
by the maintainer's family of sibling Shiny apps (earthUI, already on
CRAN; glmnetUI; mgcvUI). Projects, geo reference data, and per-project
settings are stored in SQLite databases under a configurable root
directory. The root is resolved in this order: the REGPROJ_ROOT
environment variable, a `regproj_root` field in the per-user preferences
file (stored under tools::R_user_dir("glmnetUI", "config")), and finally
a per-OS default. These files are created only when the user runs the
interactive application and saves a project — never on package load, in
examples, in tests, or in vignettes. All examples that launch the app or
write files are guarded by `if (interactive())`, and all tests write only
to tempdir().

Reviewer feedback from the earlier 0.4.0 submission (single-quoted
software names in Title/Description, DOI references for the underlying
methods, and \value tags on all exported functions) remains addressed.

## Downstream dependencies

This is a new package with no downstream dependencies.
