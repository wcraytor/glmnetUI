# glmnetUI 0.1.0

* Initial CRAN release.
* Interactive Shiny application for elastic net regression with `glmnet`.
* Point-and-click data import (CSV, Excel) with snake_case column conversion.
* Per-variable include checkboxes, type overrides, and expected sign settings.
* Weight column support with auto-detection; rows with weight = 0 are excluded.
* Force-in checkbox per variable (penalty.factor = 0).
* Enforce sign constraints via coefficient upper/lower bounds.
* Relaxed lasso support (`relax = TRUE`) with automatic gamma selection.
* Alpha grid search to find optimal elastic net mixing parameter.
* Allowed interactions checkbox matrix (earthUI-style) with localStorage persistence.
* Cross-validation with configurable folds and lambda selection (lambda.min / lambda.1se).
* Coefficient table with sign violation warnings.
* Diagnostic plots: coefficient path, CV error, actual vs predicted, residuals vs fitted.
* Report export to Word (.docx) and PDF.
* Nord-themed light/dark mode toggle with localStorage persistence.
* Variable settings persistence via localStorage keyed by filename.
* Alpha value included in Word report model summary.
* lintr-clean codebase with `.lintr` configuration for Shiny conventions.
* Purpose mode: General, Appraisal, or Market Area Analysis.
* Appraisal/Market modes: Effective Date input, Special column roles (contract_date, latitude, longitude).
* Automatic sale_age calculation from effective_date minus contract_date.
* Latitude/longitude rounding to 3 decimal places for appraisal modes.
* "?" help icons with tooltips on all model parameters and settings.
* Numbered sidebar sections matching earthUI layout (1. Import Data, 2. Project Output Folder, 3. Variable Configuration, 4. glmnet Parameters).
* Collapsible `<details>` sections for Variable Configuration and glmnet Parameters.
* Project Output Folder text input.
* Settings persistence: all model parameters saved to localStorage per input file.
* "Use last settings" / "Use default settings" / "glmnet defaults" radio with "Save current as default" button.
* `variableConfigUI()` split from `dataImportUI()` for flexible sidebar layout.
* 84 unit tests covering utilities, glmnet features, and module integration.
