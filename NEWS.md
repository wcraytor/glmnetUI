# glmnetUI 0.1.2

* **Enhanced reports**: Full-featured reports via Quarto (HTML, PDF, Word) including all tab content: Dataset Description, Model Specification, Results Summary, Equation, Coefficients, Variable Importance, Contributions plots, Correlation Matrix, ANOVA, and Diagnostics (Coefficient Path, CV Error, Actual vs Predicted, Residuals vs Fitted, Q-Q Plot).
* **Advanced glmnet parameters** section in sidebar: lambda.min.ratio, nlambda, CV loss metric (MSE/MAE/Deviance), convergence threshold, max iterations, intercept toggle. Visible for documentation/audit purposes even at defaults.
* **Improved Sales Grid formatting**: Fixed cell number formats using openxlsx; all value contribution, adjustment, percentage, and sale price cells render correctly.
* **Sorting**: RCA adjusted spreadsheet sorts comps with weight > 0 by gross_adj_pct ascending, weight = 0 comps at end.
* **Collapsible section arrow spacing**: Fixed CSS for consistent spacing between collapse arrow and section title.
* **HTML report format**: Added HTML as a third report output option alongside Word and PDF.
* **Percentage formatting**: residual_adj_pct, net_adj_pct, gross_adj_pct rounded to 1 decimal place.
* **Date column handling**: Date/POSIXct columns converted to character before Excel export to prevent datetime formatting in Sales Grid.
* **Upload size limit**: Increased to 3 GB for large files.
* **Demo dataset**: Appraisal_1.csv included in inst/extdata.
* Removed backslash escaping in equation display (MathJax handles underscores in `\text{}` natively).

# glmnetUI 0.1.1

* **Sales Grid** (Step 8, appraisal mode): Generate a formatted Sales Comparison Grid Excel workbook from RCA output. Modal dialog for comp selection with recommended comps (gross adj < 25%, sorted by sale age) pre-checked. Up to 30 comps across 10 sheets (3 per sheet). Includes grouped rows (Location/Site/Age), residual feature input cells, Net/Gross adjustment percentages, and Adjusted Sale Price formulas. Sheet protection with unlocked residual value cells for appraiser input.
* **Summary tab** with model fit statistics cards: R², Adj R², GR² (generalized), CV R², RMSE, MAE; appraisal metrics (Median Ratio, COD, PRD) shown in appraisal/market modes. Includes overfitting warning when training R² exceeds CV R² by >0.1. Coefficient table with sign warnings below stats cards.
* **Correlation tab** with heatmap matrix of numeric predictors and response variable; works before model fitting. Adaptive text/axis sizing based on variable count.
* **Variable Importance tab** with bar chart and table showing standardized coefficient magnitude (|β| × sd(x)), aggregated across dummy columns for factor variables.
* **Contributions tab** with per-variable partial effect plots: scatter + line for numeric predictors, histogram for factor/interaction terms. Line segments labeled with slope (e.g., `+1,234.56/unit`) using adaptive units matching earthUI style. Variable selector dropdown.
* **ANOVA tab** with variance decomposition table: per-variable sum of squares (SS), percentage of model SS, and coefficient. Includes intercept and total model SS rows.
* **Equation tab** with LaTeX-rendered model equation via MathJax; non-zero coefficients formatted with proper signs and interaction terms shown as `x₁ × x₂`.
* Variable Importance tab: interactive plotly bar chart with hover tooltips (exact importance, coefficient, relative %) when plotly is available; falls back to ggplot2.
* Diagnostics tab: larger fonts (base_size 16), 15 axis tick marks, comma-formatted labels (no scientific notation), shared `glmnet_diag_theme_()` helper across all 4 plots.
* Contributions and Variable Importance: larger fonts, 20 x-axis tick marks, comma-formatted labels.
* White checkmark indicator on Download Output, Calculate RCA Adjustments, and Download Report buttons after successful completion.
* Fixed ~15 rows out of 1500 getting no predictions due to unseen factor levels in export; unseen levels now get zero-valued dummy columns instead of NA.
* Fixed Contributions plot showing wobbly loess curve; replaced with `geom_line` for exact linear β×x relationship.
* Improved title bar spacing to match earthUI layout (padding above and below).
* Renamed "RCA Analysis" tab to "RCA Adjustments".
* Added "sale_age" as a Special column type, allowing any column to serve as the sale age rather than requiring the column be named "sale_age".
* Added Settings dropdown on title bar with Country and Paper size selectors (matching earthUI).
* Added Locale dropdown right-aligned next to file input for per-import locale control.
* Locale-aware CSV import using country-specific field separator and decimal mark (31 countries supported).
* Locale settings persist via localStorage with "Save as my default" option.
* Switched CSV import from `readr::read_csv` to `utils::read.csv` with locale-aware `sep`/`dec` parameters; removed `readr` dependency.
* Removed separate Weight Column dropdown; weight is now assigned via the "weight" Special type in Predictor Settings (only one weight column allowed).
* Special column (with "weight" option) now always visible, not just in appraisal/market modes.
* Additional Special column types: actual_age, area (market area/neighborhood identifier), effective_age, concessions, dom, listing_date, living_area, lot_size, site_dimensions, display_only.
* Automatic date column detection: character columns matching common date formats are auto-parsed to POSIXct; 2-digit year formats prioritized when no 4-digit year is detected.
* Data Preview split for appraisal/market modes: Subject Property (row 1) and Comparable Sales (rows 2+) in separate tables.
* Sale_age recomputation when Effective Date changes (using contract_date Special type column).
* Fixed dark mode toggle not working (onclick binding moved into `shiny:connected` handler).
* Fixed DT "column name not found" error when loading data with changing column sets between re-renders.
* Fixed `model_fitted` conditional panel flag to use `isTRUE()` instead of `!is.null()`.

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
