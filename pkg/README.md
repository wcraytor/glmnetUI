# glmnetUI

Interactive Shiny interface for elastic net regression modeling with
[glmnet](https://glmnet.stanford.edu/).

## System Requirements

- **R** >= 4.1.0 (RStudio Desktop recommended)
- **All platforms**: HTML and Word reports work out of the box
- **PDF reports**: require a LaTeX installation. If not detected, the
  PDF option is automatically hidden. Install with:
  `tinytex::install_tinytex()`
- **Linux**: may need system libraries before package installation:
  `sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev
  libsqlite3-dev libfontconfig1-dev`

## Installation

```r
# From CRAN (when available):
install.packages("glmnetUI")

# Development version from GitHub (the package lives in the pkg/ subdirectory):
# install.packages("remotes")
remotes::install_github("wcraytor/glmnetUI", subdir = "pkg")
```

## Usage

```r
library(glmnetUI)
glmnetUI()
```

This launches a Shiny application on port 7879 with a point-and-click
interface for:

- **Data import** from CSV or Excel files
- **earthUI model import**: load a saved earthUI result (`.rds`) and refit its
  basis expansion (hinge functions and interactions) with glmnet on your data
- **Variable selection** with per-variable include, type override, expected
  sign, and special column designations
- **Model fitting** with cross-validation, alpha grid search, relaxed lasso,
  sign constraints, and interaction terms
- **Diagnostic tabs**: Equation, Correlation, Summary, Coefficients, Variable
  Importance, Contributions, ANOVA, Diagnostics
- **RCA adjustments** for real estate appraisal (appraisal mode)
- **Sales Comparison Grid** generation (formatted Excel workbook)
- **Report export** in Word, PDF, or HTML format

## Purpose Modes

- **General**: Standard regression modeling
- **Appraisal**: Real estate appraisal with subject property, CQA scores,
  RCA adjustments, and Sales Grid
- **Market**: Market area analysis

## Projects

Work is organized into projects under a shared `regProj` root (default
`~/regProj`, or set `REGPROJ_ROOT`). Each project stores its input data,
outputs, and saved settings, using the same project tree and SQLite
databases (`geo.sqlite`, `projects.sqlite`) as the sibling apps
[earthUI](https://github.com/wcraytor/earthUI) and mgcvUI, so models from
all three can live side by side. Projects are created and written only
through the app.

## Demo

```r
library(glmnetUI)
glmnetUI()
# Then upload inst/extdata/Appraisal_1.csv in the app
```

## License

AGPL (>= 3)
