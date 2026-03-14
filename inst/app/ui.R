bslib::page_fluid(
  theme = nord_light,
  shiny::withMathJax(),

  # --- Adaptive CSS using Bootstrap CSS variables ---
  shiny::tags$style(shiny::HTML('
    /* Theme toggle button */
    #glmnet-theme-toggle {
      width: 38px; height: 38px; border-radius: 50%;
      border: 2px solid var(--bs-border-color);
      background: var(--bs-body-bg);
      color: var(--bs-body-color);
      font-size: 18px; cursor: pointer;
      display: flex; align-items: center; justify-content: center;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15); transition: all 0.3s;
    }
    #glmnet-theme-toggle:hover {
      box-shadow: 0 2px 10px rgba(0,0,0,0.25);
    }

    /* Variable table adapts via BS CSS vars */
    .glmnet-var-table {
      border-color: var(--bs-border-color);
    }
    .glmnet-var-hdr {
      background: var(--bs-tertiary-bg);
      border-bottom: 1px solid var(--bs-border-color);
      color: var(--bs-body-color);
    }
    .glmnet-var-row {
      border-bottom: 1px solid var(--bs-border-color);
      color: var(--bs-body-color);
    }
    .glmnet-var-row select {
      background-color: var(--bs-body-bg);
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }
    .glmnet-var-row span {
      color: var(--bs-secondary-color);
    }

    /* DT DataTables: adapt to current theme */
    .dataTables_wrapper {
      color: var(--bs-body-color);
    }
    table.dataTable {
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }
    table.dataTable thead th,
    table.dataTable thead td {
      background-color: var(--bs-tertiary-bg) !important;
      color: var(--bs-body-color) !important;
      border-color: var(--bs-border-color) !important;
    }
    table.dataTable tbody td {
      background-color: var(--bs-body-bg);
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }
    table.dataTable tbody tr:hover td {
      background-color: var(--bs-tertiary-bg);
    }
    table.dataTable tbody tr.odd td {
      background-color: var(--bs-secondary-bg);
    }
    .dataTables_info,
    .dataTables_length,
    .dataTables_filter,
    .dataTables_paginate {
      color: var(--bs-body-color);
    }
    .dataTables_paginate .paginate_button {
      color: var(--bs-body-color) !important;
      background: var(--bs-body-bg);
      border-color: var(--bs-border-color);
    }
    .dataTables_paginate .paginate_button.current {
      color: var(--bs-body-color) !important;
      background: var(--bs-tertiary-bg) !important;
      border-color: var(--bs-border-color) !important;
    }
    .dataTables_paginate .paginate_button:hover {
      color: var(--bs-body-color) !important;
      background: var(--bs-secondary-bg) !important;
      border-color: var(--bs-border-color) !important;
    }
    .dataTables_filter input {
      background-color: var(--bs-body-bg);
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }
    .dataTables_length select {
      background-color: var(--bs-body-bg);
      color: var(--bs-body-color);
      border-color: var(--bs-border-color);
    }

    /* Alert success: readable in both modes */
    .alert-success {
      background-color: color-mix(in srgb, var(--bs-success) 20%, var(--bs-body-bg));
      color: color-mix(in srgb, var(--bs-success) 60%, var(--bs-body-color));
      border-color: var(--bs-success);
    }

    /* Settings dropdown on title bar */
    .glmnet-navbar {
      display: flex; align-items: center; padding: 10px 15px; gap: 8px;
      flex-wrap: wrap;
    }
    .glmnet-navbar .dropdown { position: relative; }
    .glmnet-navbar .glmnet-menu-btn {
      background: none; border: 1px solid var(--bs-border-color);
      color: var(--bs-body-color); font-size: 0.9em;
      padding: 6px 12px; cursor: pointer; border-radius: 4px;
    }
    .glmnet-navbar .glmnet-menu-btn:hover {
      background: var(--bs-tertiary-bg);
    }
    .glmnet-navbar .glmnet-dropdown-menu {
      display: none; position: absolute; top: 100%; left: 0;
      background: var(--bs-body-bg);
      border: 1px solid var(--bs-border-color);
      border-radius: 6px; padding: 12px 16px;
      min-width: 280px; z-index: 10001;
      box-shadow: 0 4px 16px rgba(0,0,0,0.2);
    }
    .glmnet-navbar .dropdown.open .glmnet-dropdown-menu { display: block; }
    .glmnet-navbar .glmnet-spacer { flex: 1; }

    /* Locale row in import section */
    .glmnet-locale-row .form-group { margin-bottom: 0; }

    /* Help icon (?) with Bootstrap popover */
    .glmnet-param-help {
      position: absolute; top: 0; right: 0;
      width: 18px; height: 18px; border-radius: 50%;
      background: #5bc0de; color: #fff;
      font-size: 11px; font-weight: bold;
      text-align: center; line-height: 18px;
      cursor: pointer; z-index: 10;
    }
    .glmnet-param-help:hover { background: #31b0d5; }
    .glmnet-help-icon {
      display: inline-block; width: 18px; height: 18px;
      border-radius: 50%; background: #5bc0de; color: #fff;
      text-align: center; font-size: 11px; font-weight: bold;
      line-height: 18px; cursor: pointer;
      margin-left: 6px; vertical-align: middle;
    }
    .glmnet-help-icon:hover { background: #31b0d5; }

    /* Collapsible sections */
    .glmnet-section > summary {
      cursor: pointer;
      list-style: none;
    }
    .glmnet-section > summary::-webkit-details-marker {
      display: none;
    }
    .glmnet-section > summary h4::before {
      content: "\\25B6  ";
      font-size: 0.7em;
      transition: transform 0.2s;
      display: inline-block;
    }
    .glmnet-section[open] > summary h4::before {
      transform: rotate(90deg);
    }
  ')),

  # --- Theme toggle button (repositioned into navbar below) ---

  # --- Bootstrap popover initializer for "?" help icons ---
  shiny::tags$script(shiny::HTML('
    $(document).on("shiny:connected", function() {
      function initPopovers() {
        var els = document.querySelectorAll("[data-bs-toggle=\\"popover\\"]");
        els.forEach(function(el) {
          if (!bootstrap.Popover.getInstance(el)) {
            new bootstrap.Popover(el, { html: false, container: "body" });
          }
        });
      }
      initPopovers();
      var obs = new MutationObserver(function() {
        setTimeout(initPopovers, 200);
      });
      obs.observe(document.body, { childList: true, subtree: true });
    });
  ')),

  # --- Settings dropdown + Theme toggle JS + settings message handlers ---
  shiny::tags$script(src = "glmnetui.js"),

  shiny::tags$head(
    shiny::tags$link(rel = "icon", type = "image/png", href = "favicon.png")
  ),

  shiny::tags$nav(class = "glmnet-navbar",
    shiny::tags$h2(
      shiny::tags$img(src = "logo.png", height = "32px",
                      style = "margin-right: 8px; vertical-align: middle;"),
      "glmnetUI",
      shiny::tags$small(" - Interactive Elastic Net Modeling",
                        style = "font-size: 0.6em; color: var(--bs-secondary-color);"),
      style = "margin: 0;"
    ),
    shiny::tags$div(class = "dropdown", id = "glmnet-settings-dropdown",
      shiny::tags$button(
        class = "glmnet-menu-btn",
        onclick = "glmnetToggleDropdown('glmnet-settings-dropdown')",
        shiny::HTML("&#9881; Settings")
      ),
      shiny::tags$div(class = "glmnet-dropdown-menu",
        shiny::selectInput("locale_country", "Country",
                           choices = glmnetUI:::locale_country_choices_(),
                           selected = "us", width = "100%"),
        shiny::selectInput("locale_paper", "Paper",
                           choices = c("Letter" = "letter", "A4" = "a4"),
                           selected = "letter", width = "100%"),
        shiny::actionLink("locale_save_default", "Save as my default",
                          style = "font-size: 0.85em; color: #5e81ac; display: block; margin-top: 4px;")
      )
    ),
    shiny::tags$div(class = "glmnet-spacer"),
    shiny::tags$button(id = "glmnet-theme-toggle", shiny::HTML("&#9790;"))
  ),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 4,

      # --- Purpose Mode ---
      shiny::tags$div(
        style = "font-weight:bold;",
        shiny::radioButtons("purpose", "Purpose:",
                            choices = c("General" = "general",
                                        "For Appraisal" = "appraisal",
                                        "Market Area Analysis" = "market"),
                            selected = "general", inline = TRUE)
      ),
      shiny::hr(),

      # --- 1. Import Data ---
      shiny::tags$details(class = "glmnet-section",
        shiny::tags$summary(shiny::h4("1. Import Data",
                                      style = "display:inline;")),
        dataImportUI("data")
      ),
      shiny::hr(),

      shiny::conditionalPanel(
        condition = "output['data-data_loaded']",

        # --- 2. Project Output Folder ---
        shiny::tags$details(class = "glmnet-section",
          shiny::tags$summary(shiny::h4("2. Project Output Folder",
                                        style = "display:inline;")),
          shiny::textInput("output_folder", NULL,
                           value = path.expand("~/Downloads"))
        ),
        shiny::hr(),

        # --- 3. Variable Configuration ---
        shiny::tags$details(class = "glmnet-section",
          shiny::tags$summary(shiny::h4("3. Variable Configuration",
                                        style = "display:inline;")),
          shiny::conditionalPanel(
            condition = "input.purpose !== 'general'",
            shiny::dateInput("effective_date", "Effective Date",
                             value = Sys.Date())
          ),
          variableConfigUI("data")
        ),
        shiny::hr(),

        # --- 4. glmnet Call Parameters ---
        shiny::tags$details(class = "glmnet-section",
          shiny::tags$summary(shiny::h4("4. glmnet Call Parameters",
                                        style = "display:inline;")),
          shiny::tags$div(
            style = "margin-bottom: 4px; font-size: 0.85em;",
            shiny::radioButtons(
              "glmnet_defaults_action", NULL,
              choices = c("Use last settings for input file" = "last",
                          "Use default settings" = "use_default",
                          "glmnet defaults" = "glmnet_defaults"),
              selected = "last", inline = TRUE
            )
          ),
          shiny::actionButton(
            "glmnet_save_defaults", "Save current as default",
            class = "btn-dark btn-sm",
            style = paste0("padding: 2px 8px; font-size: 0.85em; ",
                           "margin-bottom: 8px;")
          ),
          modelingUI("model")
        ),
        shiny::hr(),

        # --- 5. Fit Glmnet Model ---
        shiny::tags$details(class = "glmnet-section",
          shiny::tags$summary(shiny::h4("5. Fit Glmnet Model",
                                        style = "display:inline;")),
          fitModelUI("model")
        ),

        # --- 6. Download Estimated Sale Prices & Residuals ---
        shiny::conditionalPanel(
          condition = "output.model_fitted",
          shiny::hr(),
          shiny::tags$details(class = "glmnet-section",
            shiny::tags$summary(shiny::uiOutput("download_heading",
                                                 inline = TRUE)),
            shiny::conditionalPanel(
              condition = "input.purpose !== 'general'",
              shiny::actionButton("export_data", "Download Output (Excel)",
                                  class = "btn-success",
                                  style = "width: 100%;")
            ),
            shiny::conditionalPanel(
              condition = "input.purpose === 'general'",
              shiny::tags$p(
                shiny::tags$em("Skip"),
                style = "color: var(--bs-secondary-color); margin: 4px 0;"
              )
            )
          )
        ),

        # --- 7. Calculate RCA Adjustments (Appraisal only) ---
        shiny::conditionalPanel(
          condition = "output.model_fitted",
          shiny::hr(),
          shiny::tags$details(class = "glmnet-section",
            shiny::tags$summary(shiny::h4("7. Calculate RCA Adjustments & Download",
                                          style = "display:inline;")),
            shiny::conditionalPanel(
              condition = "input.purpose === 'appraisal'",
              shiny::actionButton("rca_output_btn",
                                  "Calculate RCA Adjustments & Download",
                                  class = "btn-success",
                                  style = "width: 100%;")
            ),
            shiny::conditionalPanel(
              condition = "input.purpose !== 'appraisal'",
              shiny::tags$p(
                shiny::tags$em("Skip"),
                style = "color: var(--bs-secondary-color); margin: 4px 0;"
              )
            )
          )
        ),

        # --- 8. Generate Sales Grid (Appraisal only) ---
        shiny::conditionalPanel(
          condition = "output.rca_computed",
          shiny::hr(),
          shiny::tags$details(class = "glmnet-section",
            shiny::tags$summary(shiny::h4(
              "8. Generate Sales Grid & Download",
              style = "display:inline;")),
            shiny::conditionalPanel(
              condition = "input.purpose === 'appraisal'",
              shiny::actionButton("sales_grid_btn",
                                  "Generate Sales Grid & Download",
                                  class = "btn-success",
                                  style = "width: 100%;")
            ),
            shiny::conditionalPanel(
              condition = "input.purpose !== 'appraisal'",
              shiny::tags$p(
                shiny::tags$em("Skip"),
                style = "color: var(--bs-secondary-color); margin: 4px 0;"
              )
            )
          )
        ),

        # --- 9. Download Report ---
        shiny::conditionalPanel(
          condition = "output.model_fitted",
          shiny::hr(),
          shiny::tags$details(class = "glmnet-section",
            shiny::tags$summary(shiny::uiOutput("report_heading",
                                                 inline = TRUE)),
            shiny::selectInput("export_format", "Format",
                               choices = c("Word" = "docx",
                                           "PDF" = "pdf")),
            shiny::actionButton("export_report_btn", "Download Report",
                                class = "btn-success",
                                style = "width: 100%;")
          )
        )
      )
    ),
    shiny::mainPanel(
      width = 8,
      shiny::conditionalPanel(
        condition = "!output['data-data_loaded']",
        shiny::tags$div(
          style = "text-align:center; padding:60px 20px;",
          shiny::tags$h3("Welcome to glmnetUI"),
          shiny::tags$p(
            "Upload a CSV or Excel file to get started.",
            style = "font-size:1.2em; color: var(--bs-secondary-color);"
          ),
          shiny::tags$p(
            "This application provides an interactive interface for ",
            "elastic net regression modeling using glmnet. ",
            "Designed for real estate appraisers and general regression users."
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "output['data-data_loaded']",
        shiny::tabsetPanel(
          id = "main_tabs",
          shiny::tabPanel("Data Preview", dataPreviewUI("data")),
          shiny::tabPanel("Equation",
            shiny::conditionalPanel(
              condition = "output.model_fitted",
              equationUI("eq")
            ),
            shiny::conditionalPanel(
              condition = "!output.model_fitted",
              shiny::tags$div(
                style = "text-align:center; padding:40px 20px;",
                shiny::tags$p("Fit a model to see the equation.",
                              style = "color: var(--bs-secondary-color);")
              )
            )
          ),
          shiny::tabPanel("Correlation", correlationUI("corr")),
          shiny::tabPanel("Summary",
            shiny::conditionalPanel(
              condition = "output.model_fitted",
              summaryUI("summ")
            ),
            shiny::conditionalPanel(
              condition = "!output.model_fitted",
              shiny::tags$div(
                style = "text-align:center; padding:40px 20px;",
                shiny::tags$p("Fit a model to see summary statistics.",
                              style = "color: var(--bs-secondary-color);")
              )
            )
          ),
          shiny::tabPanel("Coefficients", coefficientsUI("coefs")),
          shiny::tabPanel("Variable Importance",
            shiny::conditionalPanel(
              condition = "output.model_fitted",
              importanceUI("imp")
            ),
            shiny::conditionalPanel(
              condition = "!output.model_fitted",
              shiny::tags$div(
                style = "text-align:center; padding:40px 20px;",
                shiny::tags$p("Fit a model to see variable importance.",
                              style = "color: var(--bs-secondary-color);")
              )
            )
          ),
          shiny::tabPanel("Contributions",
            shiny::conditionalPanel(
              condition = "output.model_fitted",
              contributionsUI("contrib")
            ),
            shiny::conditionalPanel(
              condition = "!output.model_fitted",
              shiny::tags$div(
                style = "text-align:center; padding:40px 20px;",
                shiny::tags$p("Fit a model to see variable contributions.",
                              style = "color: var(--bs-secondary-color);")
              )
            )
          ),
          shiny::tabPanel("ANOVA",
            shiny::conditionalPanel(
              condition = "output.model_fitted",
              anovaUI("anova")
            ),
            shiny::conditionalPanel(
              condition = "!output.model_fitted",
              shiny::tags$div(
                style = "text-align:center; padding:40px 20px;",
                shiny::tags$p("Fit a model to see ANOVA decomposition.",
                              style = "color: var(--bs-secondary-color);")
              )
            )
          ),
          shiny::tabPanel("Diagnostics", diagnosticsUI("diag")),
          shiny::tabPanel("Report", reportUI("report")),
          shiny::tabPanel("RCA Adjustments",
            shiny::conditionalPanel(
              condition = "!output.rca_computed",
              shiny::tags$div(
                style = "text-align:center; padding:40px 20px;",
                shiny::tags$p("Run RCA Adjustments (Step 7) to see analysis.",
                              style = "color: var(--bs-secondary-color);")
              )
            ),
            shiny::conditionalPanel(
              condition = "output.rca_computed",
              shiny::plotOutput("rca_resid_pct_plot", height = "350px"),
              shiny::hr(),
              shiny::plotOutput("rca_net_pct_plot", height = "350px"),
              shiny::hr(),
              shiny::plotOutput("rca_gross_pct_plot", height = "350px")
            )
          )
        )
      )
    )
  )
)
