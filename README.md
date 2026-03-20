# glmnetUI

Interactive Shiny interface for elastic net regression modeling with
[glmnet](https://glmnet.stanford.edu/).

## Installation

```r
# From CRAN (when available):
install.packages("glmnetUI")

# Development version from GitHub:
# install.packages("remotes")
remotes::install_github("wcraytor/glmnetUI")
```

## Usage

```r
library(glmnetUI)
glmnetUI()
```

This launches a Shiny application on port 7879 with a point-and-click
interface for:

- **Data import** from CSV or Excel files
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

## Demo

```r
library(glmnetUI)
glmnetUI()
# Then upload inst/extdata/Appraisal_1.csv in the app
```

## License

AGPL (>= 3)
