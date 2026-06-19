# ui.R (Complete Version with Dataset Selector + Plotly for Histogram, Boxplot, Scatter)
# Note: light_theme and dark_theme are defined in global.R

ui <- page_sidebar(
  useShinyjs(),
  use_tota11y(),
  
  tags$head(
    tags$script(HTML('
      $(document).ready(function() {
        $("input[placeholder=\'No file selected\']").attr("aria-label", "Selected file name");
      });
    '))
  ),
  
  title = "OpenStat Web App",
  
  sidebar = sidebar(
    materialSwitch(inputId = "dark_mode_switch", label = "Dark Mode", status = "primary"),
    hr(),
    
    # --- Dataset Selector ---
    card(
      class = "p-2",
      card_header(
        class = "d-flex align-items-center gap-2 p-2",
        bsicons::bs_icon("database"),
        "Active Dataset"
      ),
      card_body(
        class = "p-2",
        uiOutput("dataset_selector_ui"),
        actionButton(
          "delete_active_dataset",
          label = "Remove",
          icon  = icon("trash"),
          class = "btn-danger btn-sm w-100 mt-1"
        )
      )
    ),
    # --- END Dataset Selector ---
    
    hr(),
    navset_pill(
      nav_panel("Data Input",              icon = icon("table")),
      nav_panel("Descriptive Statistics",  icon = icon("chart-bar")),
      nav_panel("Inferential Statistics",  icon = icon("flask")),
      nav_panel("Regression & Correlation",icon = icon("chart-line")),
      nav_panel("Probability",             icon = icon("dice")),
      id = "main_nav"
    )
  ),
  
  # ── Data Input ──────────────────────────────────────────────────────────────
  conditionalPanel("input.main_nav == 'Data Input'",
                   h2("Data Input and Editor"),
                   layout_columns(
                     col_widths = c(6, 6),
                     card(
                       card_header("Upload Dataset (.csv, .xlsx)"),
                       fileInput("file_upload", "Choose CSV/Excel File",
                                 multiple = FALSE,
                                 accept   = c(".csv", ".xlsx")),
                       helpText("Note: For Excel files, only the first sheet will be read.")
                     ),
                     card(
                       card_header("Manual Data Entry"),
                       helpText("Click the button below to open a spreadsheet editor for manual data input."),
                       actionButton("open_manual_data_modal", "Open Spreadsheet Editor", icon = icon("edit")),
                       actionButton("load_sample_data",       "Load Sample Data"),
                       actionButton("clear_manual_data",      "Clear Active Dataset")
                     )
                   ),
                   card(
                     card_header("Dataset Preview & Editor"),
                     helpText("The table below shows the currently loaded dataset. You can edit cells directly. Edits are saved automatically to the active dataset."),
                     DTOutput("data_preview_table")
                   )
  ),
  
  # ── Descriptive Statistics ──────────────────────────────────────────────────
  conditionalPanel("input.main_nav == 'Descriptive Statistics'",
                   tagList(
                     h2("Descriptive Statistics"),
                     layout_columns(
                       col_widths = c(4, 8),
                       card(
                         card_header("Variable Selection"),
                         uiOutput("select_descriptive_variable"),
                         uiOutput("select_group_by_variable"),
                         actionButton("analyze_descriptive", "Analyze")
                       ),
                       card(
                         card_header("Summary Statistics"),
                         DTOutput("summary_stats_output") %>% withSpinner()
                       )
                     ),
                     accordion(
                       id   = "descriptive_accordion",
                       open = "Histogram",
                       accordion_panel(
                         title = "Histogram",
                         icon  = bsicons::bs_icon("bar-chart-fill"),
                         layout_columns(
                           col_widths = c(3, 9),
                           card_body(
                             checkboxInput("show_mean_median", "Plot Mean and Median", value = TRUE),
                             numericInput("hist_bins", "Number of Bins", value = 30, min = 5, max = 100),
                             selectInput("hist_yaxis_type", "Y-Axis Represents:",
                                         choices = c("Count (Frequency)" = "count", "Percent" = "percent"))
                           ),
                           # CHANGED: plotOutput -> plotlyOutput
                           plotlyOutput("histogram_plot", height = "500px") %>% withSpinner()
                         )
                       ),
                       accordion_panel(
                         title = "Box Plot",
                         icon  = bsicons::bs_icon("box-seam"),
                         # CHANGED: plotOutput -> plotlyOutput
                         plotlyOutput("boxplot_plot", height = "500px") %>% withSpinner()
                       ),
                       accordion_panel(
                         title = "Density Plot",
                         icon  = bsicons::bs_icon("graph-up"),
                         plotOutput("density_plot", height = "500px") %>% withSpinner()
                       ),
                       accordion_panel(
                         title = "Categorical Plots (Bar & Pie)",
                         icon  = bsicons::bs_icon("pie-chart-fill"),
                         layout_columns(
                           col_widths = c(6, 6),
                           card(
                             card_header("Bar Chart"),
                             selectInput("barchart_yaxis_type", "Y-Axis Represents:",
                                         choices = c("Count (Frequency)" = "count", "Proportion (Relative Frequency)" = "proportion")),
                             plotOutput("barchart_plot") %>% withSpinner()
                           ),
                           card(
                             card_header("Pie Chart"),
                             plotOutput("pie_chart_plot") %>% withSpinner()
                           )
                         )
                       ),
                       accordion_panel(
                         title = "Numeric Relationship (Scatter & Dot)",
                         icon  = bsicons::bs_icon("diagram-3-fill"),
                         layout_columns(
                           col_widths = c(6, 6),
                           card(
                             card_header("Scatter Plot"),
                             uiOutput("select_scatter_x"),
                             uiOutput("select_scatter_y"),
                             actionButton("generate_scatter", "Generate Scatter Plot"),
                             # CHANGED: plotOutput -> plotlyOutput
                             plotlyOutput("scatter_plot") %>% withSpinner()
                           ),
                           card(
                             card_header("Dot Plot"),
                             uiOutput("select_dot_plot_variable"),
                             actionButton("generate_dot_plot",  "Generate Dot Plot"),
                             downloadButton("download_dot_plot","Download Plot"),
                             plotOutput("dot_plot") %>% withSpinner()
                           )
                         )
                       )
                     )
                   )
  ),
  
  # ── Inferential Statistics ──────────────────────────────────────────────────
  conditionalPanel("input.main_nav == 'Inferential Statistics'",
                   h2("Inferential Statistics"),
                   inferential_tab_ui
  ),
  
  # ── Regression & Correlation ────────────────────────────────────────────────
  conditionalPanel("input.main_nav == 'Regression & Correlation'",
                   h2("Regression and Correlation"),
                   accordion(
                     id   = "regression_accordion",
                     open = "Linear Regression",
                     accordion_panel(
                       title = "Linear Regression",
                       icon  = bsicons::bs_icon("graph-up"),
                       layout_columns(
                         col_widths = c(4, 8),
                         card_body(
                           uiOutput("select_regression_dv"),
                           checkboxInput("log_transform_dv_reg", "Apply Log Transform to Dependent Variable (for skew)"),
                           uiOutput("select_regression_iv"),
                           actionButton("run_regression", "Run Linear Regression")
                         ),
                         card_body(
                           navset_card_tab(
                             id = "regression_output_tabs",
                             nav_panel("Summary",           verbatimTextOutput("regression_summary")           %>% withSpinner()),
                             nav_panel("Diagnostic Plots",  plotOutput("regression_diagnostic_plots")          %>% withSpinner()),
                             nav_panel("Assumption Checks", verbatimTextOutput("regression_assumption_checks") %>% withSpinner())
                           )
                         )
                       )
                     ),
                     accordion_panel(
                       title = "Logistic Regression",
                       icon  = bsicons::bs_icon("diagram-2"),
                       card_body(
                         with_info_popover(
                           ui_element = p("Use for modeling a binary outcome (e.g., Yes/No, 1/0)."),
                           title   = "What is Logistic Regression?",
                           content = "Logistic Regression predicts the probability of an outcome occurring. It's used when your dependent variable has only two categories."
                         )
                       ),
                       layout_columns(
                         col_widths = c(4, 8),
                         card_body(
                           uiOutput("select_logistic_dv"),
                           uiOutput("select_logistic_iv"),
                           actionButton("run_logistic", "Run Logistic Regression")
                         ),
                         card_body(verbatimTextOutput("logistic_summary") %>% withSpinner())
                       )
                     ),
                     accordion_panel(
                       title = "Correlation Matrix",
                       icon  = bsicons::bs_icon("grid-3x3"),
                       uiOutput("select_correlation_vars"),
                       actionButton("run_correlation", "Calculate Correlation"),
                       verbatimTextOutput("correlation_matrix") %>% withSpinner()
                     )
                   )
  ),
  
  # ── Probability ─────────────────────────────────────────────────────────────
  conditionalPanel("input.main_nav == 'Probability'",
                   h2("Probability Distributions and Calculations"),
                   navset_card_tab(
                     nav_panel("Basic Event Probability",
                               layout_columns(
                                 col_widths = c(4, 8),
                                 card(
                                   card_header("Select Rule and Input Parameters"),
                                   selectInput("prob_calc_type", "Select Calculation Type:",
                                               choices = c(
                                                 "P(A or B) - Additive Rule"                 = "union",
                                                 "P(A|B) - Conditional Probability"          = "conditional",
                                                 "Check for Independence / Mutually Exclusive" = "check_relationship"
                                               )),
                                   hr(),
                                   uiOutput("prob_required_inputs"),
                                   actionButton("calculate_basic_probs", "Calculate")
                                 ),
                                 card(
                                   card_header("Result and Interpretation"),
                                   (verbatimTextOutput("calculated_output_title") %>% withSpinner()),
                                   verbatimTextOutput("calculated_output")
                                 )
                               )
                     ),
                     nav_panel("Normal Distribution",
                               layout_columns(
                                 col_widths = c(4, 8),
                                 card(
                                   card_header("Distribution Parameters"),
                                   numericInput("normal_mean", "Mean (μ):", value = 0),
                                   numericInput("normal_sd",  "Standard Deviation (σ):", value = 1, min = 0.01),
                                   checkboxInput("show_empirical_rule", "Show Empirical Rule (68-95-99.7)", value = FALSE),
                                   hr(),
                                   selectInput("normal_prob_type", "Select Calculation Type:",
                                               choices = c(
                                                 "Find Probability from x (P(X < x))"          = "less",
                                                 "Find Probability from x (P(X > x))"          = "greater",
                                                 "Find Probability from range (P(a < X < b))"  = "between",
                                                 "Find x from Probability (Solve for x)"       = "inverse"
                                               )),
                                   uiOutput("normal_inputs"),
                                   actionButton("calc_normal", "Calculate")
                                 ),
                                 card(
                                   card_header("Results"),
                                   h3(class = "h4", "Calculated Value:"),
                                   (verbatimTextOutput("normal_result") %>% withSpinner()),
                                   hr(),
                                   h3(class = "h4", "Visual Representation:"),
                                   plotOutput("normal_plot") %>% withSpinner()
                                 )
                               )
                     ),
                     nav_panel("Binomial Distribution",
                               layout_columns(
                                 col_widths = c(4, 8),
                                 card(
                                   card_header("Binomial Distribution Parameters"),
                                   numericInput("binom_size", "Number of Trials (n)", value = 10, min = 1, step = 1),
                                   numericInput("binom_prob", "Probability of Success (p)", value = 0.5, min = 0, max = 1, step = 0.01),
                                   hr(),
                                   h3(class = "h4", "Distribution Summary"),
                                   (verbatimTextOutput("binom_summary_stats") %>% withSpinner()),
                                   hr(),
                                   h3(class = "h4", "Calculate P(X) given x"),
                                   numericInput("binom_k", "Number of Successes (x)", value = 5, min = 0, step = 1),
                                   selectInput("binom_type", "Probability Type", choices = c("P(X = x)", "P(X <= x)", "P(X >= x)")),
                                   actionButton("calc_binom_prob", "Calculate Probability"),
                                   (verbatimTextOutput("binom_prob_output") %>% withSpinner()),
                                   hr(),
                                   h3(class = "h4", "Find x for a given Cumulative Probability P(X ≤ x)"),
                                   numericInput("binom_p_for_k", "Cumulative Probability (e.g., 0.95):", value = 0.95, min = 0, max = 1, step = 0.01),
                                   actionButton("solve_binom_k", "Solve for x"),
                                   (verbatimTextOutput("solve_binom_k_output") %>% withSpinner())
                                 ),
                                 card(
                                   card_header("Binomial Distribution PMF Plot"),
                                   plotOutput("binom_pmf_plot") %>% withSpinner()
                                 )
                               )
                     ),
                     nav_panel("Poisson Distribution",
                               layout_columns(
                                 col_widths = c(4, 8),
                                 card(
                                   card_header("Poisson Distribution Parameters"),
                                   numericInput("pois_lambda", "Lambda (λ - average rate)", value = 3, min = 0.01),
                                   hr(),
                                   h3(class = "h4", "Distribution Summary"),
                                   (verbatimTextOutput("pois_summary_stats") %>% withSpinner()),
                                   hr(),
                                   h3(class = "h4", "Calculate P(X = k) or P(X <= k)"),
                                   numericInput("pois_k",    "Number of Events (k)", value = 2, min = 0, step = 1),
                                   selectInput("pois_type", "Probability Type", choices = c("P(X = k)", "P(X <= k)", "P(X >= k)")),
                                   actionButton("calc_pois_prob", "Calculate Poisson Probability"),
                                   (verbatimTextOutput("pois_prob_output") %>% withSpinner())
                                 ),
                                 card(
                                   card_header("Poisson Distribution PMF Plot"),
                                   plotOutput("pois_pmf_plot") %>% withSpinner()
                                 )
                               )
                     )
                   )
  ),
  
  theme = light_theme
)