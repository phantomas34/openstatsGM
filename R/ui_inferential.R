# R/ui_inferential.R (Complete Corrected Version with Spinners)

inferential_tab_ui <- accordion(
  id = "inferential_accordion",
  
  # Panel 1: Proportion Tests
  accordion_panel(
    title = "Proportion Tests",
    with_info_popover(
      ui_element = h4("One-Proportion Test"),
      title = "What is a One-Proportion Test?",
      content = "This test is used to compare the proportion of successes in a sample to a known or hypothesized proportion."
    ),
    
    checkboxInput("prop_test_manual_mode", "Enter summary data manually (independent of dataset)", value = FALSE),
    
    conditionalPanel(
      condition = "!input.prop_test_manual_mode",
      uiOutput("prop_variable_ui"),
      uiOutput("success_value_ui")
    ),
    
    conditionalPanel(
      condition = "input.prop_test_manual_mode",
      numericInput("prop_manual_successes", "Number of Successes (x)", value = 10, min = 0, step = 1),
      numericInput("prop_manual_trials", "Number of Trials (n)", value = 20, min = 1, step = 1)
    ),
    
    hr(),
    numericInput("prop_null", "Null Hypothesis Proportion (p₀):", value = 0.5, min = 0, max = 1, step = 0.01),
    selectInput("prop_alternative", "Alternative Hypothesis:", choices = c("Two-sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")),
    actionButton("run_prop_test", "Run One-Proportion Test"),
    verbatimTextOutput("prop_test_result") %>% withSpinner(), # <-- Spinner Added
    
    hr(),
    with_info_popover(
      ui_element = h4("Two-Proportion Test"),
      title = "What is a Two-Proportion Test?",
      content = "This test compares the proportions of successes between two different groups."
    ),
    uiOutput("two_prop_var_ui"),
    uiOutput("two_prop_group_var_ui"),
    uiOutput("two_prop_group1_ui"),
    uiOutput("two_prop_group2_ui"),
    uiOutput("two_prop_success_ui"),
    selectInput("two_prop_alternative", "Alternative Hypothesis:", choices = c("Two-sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")),
    actionButton("run_two_prop_test", "Run Two-Proportion Test"),
    verbatimTextOutput("two_prop_test_result") %>% withSpinner() # <-- Spinner Added
  ),
  
  # Panel 2: Categorical Tests
  accordion_panel(
    title = "Categorical Tests",
    with_info_popover(
      ui_element = h4("Chi-Squared Test / Contingency Table"),
      title = "What is a Chi-Squared Test?",
      content = "A Chi-Squared test is used to see if there is a significant relationship between two categorical variables."
    ),
    uiOutput("select_chi_x"),
    uiOutput("select_chi_y"),
    checkboxInput("use_fisher_exact", "Use Fisher's Exact Test (for small samples)", value = FALSE),
    actionButton("run_chi_sq", "Run Chi-Squared Test"),
    
    navset_card_tab(
      id = "chisq_output_tabs",
      nav_panel(
        "Two-Way Table", 
        selectInput("chisq_table_type", "Display:",
                    choices = c("Observed Counts" = "counts",
                                "Row Percentages" = "row_perc",
                                "Column Percentages" = "col_perc",
                                "Total Percentages" = "total_perc")),
        verbatimTextOutput("chi_sq_table_output") %>% withSpinner() # <-- Spinner Added
      ),
      nav_panel(
        "Chi-Squared Test", 
        verbatimTextOutput("chi_sq_test_output") %>% withSpinner() # <-- Spinner Added
      )
    )
  ),
  
  # Panel 3: Normality Check
  accordion_panel(
    title = "Normality Check",
    with_info_popover(
      ui_element = h4("Normality Check (for n < 30)"),
      title = "What is a Normality Check?",
      content = "This check helps you verify the normality assumption required for tests like t-tests and ANOVA."
    ),
    uiOutput("select_normality_var"),
    actionButton("check_normality", "Check Normality"),
    plotOutput("normality_plot") %>% withSpinner(), # <-- Spinner Added
    verbatimTextOutput("shapiro_wilk_output") %>% withSpinner() # <-- Spinner Added
  ),
  
  # Panel 4: Mean Tests
  accordion_panel(
    title = "Mean Tests",
    with_info_popover(
      ui_element = h4("Hypothesis Testing (t-test)"),
      title = "What is a t-test?",
      content = "A t-test is used to compare the average (mean) of one or two groups."
    ),
    uiOutput("select_ht_variable"),
    uiOutput("select_ht_group_variable"),
    conditionalPanel(
      condition = "input.ht_group_variable == 'None'",
      numericInput("ht_mu", "Hypothesized Mean (μ) for One-Sample Test", value = 0)
    ),
    selectInput("ht_alternative", "Alternative Hypothesis", choices = c("two.sided", "less", "greater")),
    checkboxInput("ht_var_equal", "Assume Equal Variances (for Two-Sample Test)", value = TRUE),
    actionButton("run_ht", "Run t-test"),
    verbatimTextOutput("ht_output") %>% withSpinner(), # <-- Spinner Added
    hr(),
    with_info_popover(
      ui_element = h4("Paired t-test"),
      title = "What is a Paired t-test?",
      content = "This t-test is used when each subject is measured twice, creating 'paired' observations (e.g., before-and-after)."
    ),
    uiOutput("paired_var1_ui"),
    uiOutput("paired_var2_ui"),
    selectInput("paired_alternative", "Alternative Hypothesis:", choices = c("Two-sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")),
    actionButton("run_paired_ttest", "Run Paired t-test"),
    verbatimTextOutput("paired_ttest_result") %>% withSpinner(), # <-- Spinner Added
    hr(),
    with_info_popover(
      ui_element = h4("ANOVA (Analysis of Variance)"),
      title = "What is an ANOVA?",
      content = "ANOVA is used to compare the averages of three or more groups at once."
    ),
    uiOutput("select_anova_dv"),
    uiOutput("select_anova_iv"),
    uiOutput("select_anova_iv2"),
    actionButton("run_anova", "Run ANOVA"),
    verbatimTextOutput("anova_output") %>% withSpinner() # <-- Spinner Added
  ),
  
  # Panel 5: Non-Parametric Tests
  accordion_panel(
    title = "Non-Parametric Tests",
    
    with_info_popover(
      ui_element = h4("Mann-Whitney U Test"),
      title = "What is the Mann-Whitney U Test?",
      content = "Also known as the Wilcoxon Rank-Sum test, this is the non-parametric alternative to the independent two-sample t-test. Use it to compare two groups when the data is not normally distributed."
    ),
    uiOutput("select_mw_variable"),
    uiOutput("select_mw_group"),
    actionButton("run_mw_test", "Run Mann-Whitney Test"),
    verbatimTextOutput("mw_test_output") %>% withSpinner(), # <-- Spinner Added
    
    hr(),
    
    with_info_popover(
      ui_element = h4("Kruskal-Wallis Test"),
      title = "What is the Kruskal-Wallis Test?",
      content = "This is the non-parametric alternative to the one-way ANOVA. Use it to compare three or more groups when the data is not normally distributed."
    ),
    uiOutput("select_kw_variable"),
    uiOutput("select_kw_group"),
    actionButton("run_kw_test", "Run Kruskal-Wallis Test"),
    verbatimTextOutput("kw_test_output") %>% withSpinner() # <-- Spinner Added
  )
  
)