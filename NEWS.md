# glmnetUI 0.5.0 — Consistent Theme & Purpose-Aware State Management

## Theme & Visual Consistency

* **Nord palette enforcement**: All interactive buttons now use Frost colors (nord8/nord9/nord10) exclusively. Aurora colors (red/green/yellow) are reserved for warnings and error indicators only — never on buttons. This convention applies consistently across earthUI, glmnetUI, and mgcvUI.
* **Matched browse controls**: The earthUI import browse button (Section 2) now matches the native `fileInput` appearance in Section 1 — same font, color, and layout via CSS class `.glmnet-earth-browse`.
* **Visible dropdown borders**: Variable Configuration dropdowns (Type, Special, Sign) now use `var(--bs-border-color)` instead of the nearly invisible `#ccc`, making them clearly visible in both light and dark modes.
* **Right-aligned info icons**: Section header help icons (`?`) are now right-aligned via flexbox and use Nord Frost colors (nord8 `#88c0d0` / nord10 `#5e81ac`) instead of Bootstrap defaults (`#5bc0de`).
* **Dark mode**: Snow Storm (light, nord6 background) and Polar Night (dark, nord0 background) themes documented and enforced. Theme preference persists via localStorage.

## Purpose-Aware State Management

* **Purpose persistence**: The last-used purpose mode is saved to localStorage (`glmnetUI_last_purpose`) and restored automatically when the app is relaunched. Each sibling app (earthUI, glmnetUI, mgcvUI) persists independently.
* **Full reset on purpose change**: Changing the Purpose radio button now clears all state — data import, earth import, model results, RCA adjustments, and Sales Grid — so no stale results carry over between modes. All result tabs return to their empty/waiting state. FileInput and earth path displays are also cleared visually.

## Documentation

* **CLAUDE.md**: New project-level instructions file with shared UI conventions for all three sibling apps (Nord palette rules, Frost-only button classes, purpose persistence, sales_grid.R exception).
* **Vignettes updated**: Getting Started and User Guide vignettes reflect the new Section 2 (earthUI import), renumbered sidebar workflow (1–10), purpose persistence, and purpose-change reset behavior.
* **User Guide (PDF)**: Updated chapter references, sidebar section numbering, Getting Started steps, and settings persistence documentation.

# glmnetUI 0.4.0

## Report Generation — Fixed and Enhanced

* **Fixed PDF/Word report failures**: LaTeX compilation errors from unescaped underscores in variable names (`\text{sale_price}`) and 200K+ character `Call:` field from `print(cv.relaxed)` exceeding LaTeX bufsize. The model `Call:` is now stripped before saving to report assets.
* **Quarto → rmarkdown automatic fallback**: If Quarto rendering fails or is unavailable, reports fall back to rmarkdown with the bundled `report_template.Rmd`.
* **HTML reports self-contained**: Images embedded via `embed-resources: true` so HTML reports display all plots without external file references.
* **Fixed missing plots in reports**: Switched from `knitr::include_graphics()` to raw markdown image syntax in `results='asis'` chunks; fixed Quarto PDF absolute path issue where LuaTeX prepended `./` to paths.
* **Data alignment fix**: Report contribution plots now work correctly with appraisal data where `nrow(data) != nrow(x_mat)` due to weight=0 rows and subject exclusion.

## Report Contribution Plots — Rewritten

* **Numeric predictors**: Scatter plot with linear fit line (was: `geom_line`).
* **Factor variables**: Box plot by level (was: histogram).
* **Interactions**: Scatter colored by contribution + heatmap of mean contribution + static 3D `persp()` surface snapshot (was: histogram).
* All axes and color legends use comma-formatted numbers (no scientific notation).
* Heatmap legends enlarged for readability.

## General Purpose Mode — 8 Fixes

* **Purpose no longer snaps back to "For Appraisal"**: Removed `purpose` from per-file settings save/restore so user's purpose choice is respected.
* **Skip first row checkbox**: Added after Purpose radio (visible for General/Market modes), excludes row 1 from model fitting.
* **Separate settings per purpose**: localStorage keys now include purpose (`glmnetUI_settings_<file>_<purpose>`) so switching modes preserves each mode's settings independently. Falls back to legacy keys for migration.
* **No Special column for General**: Predictor settings table hides the Special dropdown when purpose=general.
* **Min-widths on predictor columns**: Added `min-width` and `gap` to prevent column collapse and improve readability.
* **Special variable handling skipped for General**: sale_age computation, contract_date handling, latitude/longitude rounding only run in appraisal/market modes.
* **Step 6 Download enabled for General**: Was showing "Skip" for General mode; now shows the download button for all purpose modes.
* **RCA/Sales Grid hidden for non-appraisal**: Sections 7 and 8 completely hidden instead of showing "Skip" for non-appraisal modes.

## Other Changes

* **Earth import removed**: Removed `earth_knots_r` parameter and all earth-basis code paths from `modelingServer`, eliminating "Expected a glmnetUI_earth_import object" errors.
* **Settings JS refactored**: Replaced `sprintf` with `paste0` for predictor settings JavaScript to avoid the 8192-character `sprintf` format limit.
* 29 new unit tests for report generation (1,158 total tests, all passing).
* Updated roxygen documentation for `render_report()`, `reportServer()`, and `modelingServer()`.
* Updated vignettes, user guide (QMD + PDF), and cran-comments.md.

# glmnetUI 0.1.2

* **Random seed** for reproducible cross-validation: text input pre-filled with random integer, seed history (last 5 per file) with clickable recall links, `set.seed()` called before fitting, seed shown in Glmnet Output tab and fit status.
* **Factor checkbox column** ("Fac") in Variable Configuration: dedicated checkbox between Type and Inc columns to designate variables as factors. Replaces the previous "factor" option in the Type dropdown. Persists via localStorage.
* **Block from Main Effect**: Right-click a variable name in the Interaction Matrix to block it from the main effect (interaction only). Bold " 1" indicator shown. State persists via localStorage.
* **Glmnet Output tab**: Shows raw model print, selected lambda, lambda.min/1se, gamma, and full coefficient vector — matching earthUI's "Earth Output" tab.
* **3D surface plots** for interactions in Contributions tab: `persp()` surface + scatter + heatmap for two-numeric interactions. Factor variables show box plots by level.
* **Enhanced reports**: Full-featured reports via Quarto (HTML, PDF, Word) with automatic rmarkdown fallback. Includes: Dataset Description, Model Specification, Results Summary, Equation, Coefficients, Variable Importance, Contributions plots, Correlation Matrix, ANOVA, Diagnostics (Coefficient Path, CV Error, Actual vs Predicted, Residuals vs Fitted, Q-Q Plot), and Model Output. Contribution plots in reports now match the Contributions tab: scatter with fit line (numeric), box plot (factor), and scatter/heatmap/static 3D persp (interactions). Axis labels and color legends use comma-formatted numbers. HTML reports are self-contained with embedded images. Report tab and Step 9 both use the same pipeline. Elapsed timer shown during generation.
* **Report bug fixes**: Fixed LaTeX compilation failures caused by unescaped underscores in variable names within `\text{}` and by `print(cv.relaxed)` dumping a 200K+ character `Call:` field that exceeded LaTeX's bufsize. The model `Call:` is now stripped before saving to report assets.
* **Advanced glmnet parameters** section in sidebar: lambda.min.ratio, nlambda, CV loss metric (MSE/MAE/Deviance), convergence threshold, max iterations, intercept toggle. Visible for documentation/audit purposes even at defaults.
* **Improved Sales Grid formatting**: Fixed cell number formats; all value contribution, adjustment, percentage, and sale price cells render correctly.
* **Sorting**: RCA adjusted spreadsheet sorts comps with weight > 0 by gross_adj_pct ascending, weight = 0 comps at end.
* **Interaction matrix**: JavaScript moved to static file for reliable execution in Shiny modules. Default is all unchecked (Clear All); saved settings restored from localStorage.
* **Contribution plots**: Fixed interaction terms being mixed into parent variable contributions (split on `:` for formula interactions). Linear line for single-coefficient predictors instead of jagged geom_line.
* **Percentage formatting**: residual_adj_pct, net_adj_pct, gross_adj_pct rounded to 1 decimal place.
* **Date column handling**: Date/POSIXct columns converted to character before Excel export to prevent datetime formatting in Sales Grid.
* **Upload size limit**: Increased to 3 GB for large files.
* **Demo dataset**: Appraisal_1.csv included in inst/extdata.
* **Dark mode**: Block-from-degree-1 indicator uses `var(--bs-body-color)` for theme-adaptive color.

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
