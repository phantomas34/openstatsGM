# server.R (Complete Version with Dataset Toggle + Plotly for Histogram, Boxplot, Scatter)

server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  observeEvent(input$dark_mode_switch, {
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode_switch)) dark_theme else light_theme
    )
  })
  
  # --- Reactive Values ---
  
  dataset_list_r        <- reactiveVal(list())
  active_dataset_name_r <- reactiveVal(NULL)
  
  data_r <- reactive({
    name <- active_dataset_name_r()
    if (is.null(name)) return(NULL)
    dataset_list_r()[[name]]
  })
  
  add_dataset <- function(name, df) {
    lst <- dataset_list_r()
    if (name %in% names(lst)) {
      i <- 2
      while (paste0(name, " (", i, ")") %in% names(lst)) i <- i + 1
      name <- paste0(name, " (", i, ")")
    }
    lst[[name]] <- df
    dataset_list_r(lst)
    active_dataset_name_r(name)
  }
  
  modal_data_r <- reactiveVal(NULL)
  current_dot_plot <- reactiveVal(NULL)
  current_normal_plot <- reactiveVal(NULL)
  
  # Reactive values to store the plots and table for the Download Report feature
  desc_results <- reactiveValues(
    hist = NULL,
    box = NULL,
    density = NULL,
    summary = NULL
  )
  
  # --- Data Input and Management Logic ---
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    file_ext <- tools::file_ext(input$file_upload$name)
    df <- NULL
    tryCatch({
      missing_symbols <- c("", "NA", "N/A", "n/a", "*", ".", "-", "Missing")
      if (file_ext == "csv") {
        df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE, na.strings = missing_symbols)
      } else if (file_ext == "xlsx") {
        df <- readxl::read_excel(input$file_upload$datapath, na = missing_symbols)
      } else {
        showNotification("Unsupported file type. Please upload a .csv or .xlsx file.", type = "error")
      }
      if (!is.null(df)) {
        dataset_name <- tools::file_path_sans_ext(input$file_upload$name)
        add_dataset(dataset_name, df)
        showNotification(paste0('"', dataset_name, '" uploaded successfully!'), type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  observeEvent(input$load_sample_data, {
    showModal(modalDialog(
      title = "Select a Sample Dataset",
      selectInput("sample_data_choice", "Choose a dataset:",
                  choices = c("Cars (mtcars)", "Flowers (iris)", "Student Exam Scores")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("load_selected_sample_data", "Load")
      )
    ))
  })
  
  observeEvent(input$load_selected_sample_data, {
    df_to_load <- switch(input$sample_data_choice,
                         "Cars (mtcars)"       = mtcars,
                         "Flowers (iris)"      = iris,
                         "Student Exam Scores" = exam_scores)
    add_dataset(input$sample_data_choice, df_to_load)
    removeModal()
    showNotification(paste(input$sample_data_choice, "dataset loaded."), type = "message")
  })
  
  observeEvent(input$clear_manual_data, {
    name <- active_dataset_name_r()
    if (!is.null(name)) {
      lst <- dataset_list_r()
      lst[[name]] <- NULL
      dataset_list_r(lst)
      if (length(lst) > 0) active_dataset_name_r(names(lst)[1])
      else active_dataset_name_r(NULL)
      showNotification("Dataset cleared.", type = "message")
    } else {
      showNotification("No active dataset to clear.", type = "warning")
    }
  })
  
  observeEvent(input$open_manual_data_modal, {
    if (is.null(data_r())) {
      modal_data_r(data.frame(Variable1 = rep(NA, 5), Variable2 = rep(NA, 5)))
    } else {
      modal_data_r(data_r())
    }
    showModal(modalDialog(
      title = "Manual Data Entry",
      size = "l",
      fluidPage(
        h4("Edit your data below:"),
        checkboxInput("mobile_edit_mode", "Enable Mobile Edit Mode", value = FALSE),
        uiOutput("manual_editor_ui"),
        helpText("Use the toggle above for mobile typing mode. Spreadsheet mode allows adding/removing columns and rows.")
      ),
      footer = tagList(
        actionButton("add_row", "Add Row", class = "btn-primary"),
        actionButton("remove_row", "Remove Last Row", class = "btn-danger"),
        actionButton("add_column", "Add Column", class = "btn-primary"),
        actionButton("remove_column", "Remove Last Column", class = "btn-danger"),
        actionButton("modal_save_data", "Save Changes", class = "btn-success"),
        modalButton("Cancel")
      )
    ))
  })
  
  output$manual_editor_ui <- renderUI({
    if (isTRUE(input$mobile_edit_mode)) {
      df <- modal_data_r()
      if (is.null(df)) return(NULL)
      tagList(
        lapply(1:nrow(df), function(row_idx) {
          fluidRow(
            column(12, strong(paste("Row", row_idx))),
            lapply(seq_along(df), function(col_idx) {
              column(6,
                     textInput(
                       inputId = paste0("cell_", row_idx, "_", col_idx),
                       label = names(df)[col_idx],
                       value = ifelse(is.na(df[row_idx, col_idx]), "", as.character(df[row_idx, col_idx]))
                     )
              )
            })
          )
        })
      )
    } else {
      rHandsontableOutput("modal_spreadsheet")
    }
  })
  
  output$modal_spreadsheet <- renderRHandsontable({
    req(modal_data_r())
    df <- modal_data_r()
    rh <- rhandsontable(df, readOnly = FALSE, useTypes = FALSE)
    for (col in names(df)) {
      rh <- hot_col(rh, col, type = "text")
    }
    rh %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
  })
  
  observeEvent(input$add_column, {
    df <- modal_data_r()
    existing_names <- names(df)
    new_col_name <- paste0("Column", ncol(df) + 1)
    while (new_col_name %in% existing_names) {
      new_col_name <- paste0(new_col_name, "_new")
    }
    df[[new_col_name]] <- NA
    modal_data_r(df)
  })
  
  observeEvent(input$remove_column, {
    df <- modal_data_r()
    if (ncol(df) > 1) {
      df <- df[, -ncol(df), drop = FALSE]
      modal_data_r(df)
    } else {
      showNotification("At least one column must remain.", type = "warning")
    }
  })
  
  observeEvent(input$add_row, {
    df <- modal_data_r()
    new_row <- as.data.frame(as.list(rep(NA, ncol(df))))
    names(new_row) <- names(df)
    df <- rbind(df, new_row)
    modal_data_r(df)
  })
  
  observeEvent(input$remove_row, {
    df <- modal_data_r()
    if (nrow(df) > 1) {
      df <- df[-nrow(df), , drop = FALSE]
      modal_data_r(df)
    } else {
      showNotification("At least one row must remain.", type = "warning")
    }
  })
  
  observeEvent(input$modal_spreadsheet, {
    if (!is.null(input$modal_spreadsheet)) {
      modal_data_r(hot_to_r(input$modal_spreadsheet))
    }
  })
  
  observeEvent(input$modal_save_data, {
    df_to_save <- NULL
    if (isTRUE(input$mobile_edit_mode)) {
      df_temp <- modal_data_r()
      for (row_idx in 1:nrow(df_temp)) {
        for (col_idx in 1:ncol(df_temp)) {
          value <- input[[paste0("cell_", row_idx, "_", col_idx)]]
          if (!is.null(value)) df_temp[row_idx, col_idx] <- value
        }
      }
      df_to_save <- df_temp
    } else {
      if (!is.null(input$modal_spreadsheet)) {
        df_to_save <- hot_to_r(input$modal_spreadsheet)
      }
    }
    
    if (!is.null(df_to_save)) {
      df_converted <- df_to_save %>%
        mutate(across(everything(), ~ type.convert(.x, as.is = TRUE)))
      if (!is.null(active_dataset_name_r()) && grepl("^Manual Entry", active_dataset_name_r())) {
        lst <- dataset_list_r()
        lst[[active_dataset_name_r()]] <- df_converted
        dataset_list_r(lst)
      } else {
        add_dataset("Manual Entry", df_converted)
      }
      removeModal()
      showNotification("Data saved successfully!", type = "message")
    } else {
      removeModal()
      showNotification("No data to save.", type = "warning")
    }
  })
  
  output$data_preview_table <- renderDT({
    df <- data_r()
    if (is.null(df)) {
      return(datatable(data.frame(Message = "No data loaded. Please upload a file or enter data manually."),
                       options = list(dom = 't')))
    }
    datatable(df, editable = TRUE, options = list(pageLength = 10))
  })
  
  observeEvent(input$data_preview_table_cell_edit, {
    info <- input$data_preview_table_cell_edit
    df <- data_r()
    if (!is.null(df)) {
      df[info$row, info$col] <- DT::coerceValue(info$value, df[info$row, info$col])
      lst <- dataset_list_r()
      lst[[active_dataset_name_r()]] <- df
      dataset_list_r(lst)
      showNotification(paste("Cell [", info$row, ",", info$col, "] updated."), type = "message")
    }
  })
  
  # --- Dataset selector UI + observers ---
  
  output$dataset_selector_ui <- renderUI({
    lst <- dataset_list_r()
    if (length(lst) == 0) return(helpText("No datasets loaded yet."))
    selectInput("active_dataset_select", label = NULL,
                choices = names(lst), selected = active_dataset_name_r())
  })
  
  observeEvent(input$active_dataset_select, {
    active_dataset_name_r(input$active_dataset_select)
  }, ignoreNULL = TRUE)
  
  observeEvent(input$delete_active_dataset, {
    name <- active_dataset_name_r()
    req(name)
    lst <- dataset_list_r()
    lst[[name]] <- NULL
    dataset_list_r(lst)
    if (length(lst) > 0) active_dataset_name_r(names(lst)[1])
    else active_dataset_name_r(NULL)
    showNotification(paste0('"', name, '" removed.'), type = "message")
  })
  
  # --- UI Dropdown Generation (for non-inferential tabs) ---
  
  observe({
    df <- data_r()
    if (!is.null(df)) {
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      all_cols <- names(df)
      
      output$select_descriptive_variable <- renderUI({ selectInput("descriptive_variable", "Select Variable for Descriptive Stats", choices = c("", all_cols)) })
      output$select_group_by_variable <- renderUI({ selectInput("group_by_variable", "Group By (Optional)", choices = c("None", all_cols)) })
      output$select_scatter_x <- renderUI({ selectInput("scatter_x", "Select X-axis Variable (Numeric)", choices = c("", numeric_cols)) })
      output$select_scatter_y <- renderUI({ selectInput("scatter_y", "Select Y-axis Variable (Numeric)", choices = c("", numeric_cols)) })
      output$select_dot_plot_variable <- renderUI({ selectInput("dot_plot_variable", "Select Variable for Dot Plot", choices = c("", numeric_cols)) })
      
      output$select_regression_dv <- renderUI({ selectInput("regression_dv", "Dependent Variable (Numeric)", choices = c("", numeric_cols)) })
      output$select_regression_iv <- renderUI({ selectInput("regression_iv", "Independent Variable(s) (Numeric)", choices = numeric_cols, multiple = TRUE) })
      output$select_correlation_vars <- renderUI({ selectInput("correlation_vars", "Select Variables for Correlation (Numeric)", choices = numeric_cols, multiple = TRUE) })
      
      output$select_logistic_dv <- renderUI({ selectInput("logistic_dv", "Dependent Variable (Binary/Categorical):", choices = c("", all_cols)) })
      output$select_logistic_iv <- renderUI({ selectInput("logistic_iv", "Independent Variable(s):", choices = all_cols, multiple = TRUE) })
    } else {
      output$select_descriptive_variable <- renderUI({ selectInput("descriptive_variable", "Select Variable", choices = "") })
      output$select_group_by_variable <- renderUI({ selectInput("group_by_variable", "Group By (Optional)", choices = "") })
      output$select_scatter_x <- renderUI({ selectInput("scatter_x", "Select X-axis Variable", choices = "") })
      output$select_scatter_y <- renderUI({ selectInput("scatter_y", "Select Y-axis Variable", choices = "") })
      output$select_dot_plot_variable <- renderUI({ selectInput("dot_plot_variable", "Select Variable", choices = "") })
      output$select_regression_dv <- renderUI({ selectInput("regression_dv", "Dependent Variable", choices = "") })
      output$select_regression_iv <- renderUI({ selectInput("regression_iv", "Independent Variable(s)", choices = "") })
      output$select_correlation_vars <- renderUI({ selectInput("correlation_vars", "Select Variables", choices = "") })
    }
  })
  
  # --- INFERENTIAL UI DROPDOWN GENERATION ---
  
  observe({
    df <- data_r()
    numeric_cols <- if (is.null(df)) "" else names(df)[sapply(df, is.numeric)]
    char_factor_cols <- if (is.null(df)) "" else names(df)[sapply(df, function(x) {
      is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(x)) < 15)
    })]
    
    output$select_ht_variable <- renderUI({ selectInput("ht_variable", "Select Variable for t-test (Numeric)", choices = c("", numeric_cols)) })
    output$select_ht_group_variable <- renderUI({ selectInput("ht_group_variable", "Grouping Variable (for Two-Sample Test)", choices = c("None", char_factor_cols)) })
    output$paired_var1_ui <- renderUI({ selectInput("paired_var1", "Select First Variable (Numeric):", choices = c("", numeric_cols)) })
    output$paired_var2_ui <- renderUI({ selectInput("paired_var2", "Select Second Variable (Numeric):", choices = c("", numeric_cols)) })
    output$select_anova_dv <- renderUI({ selectInput("anova_dv", "Dependent Variable (Numeric)", choices = c("", numeric_cols)) })
    output$select_anova_iv <- renderUI({ selectInput("anova_iv", "Independent Variable (Categorical)", choices = c("", char_factor_cols)) })
    output$select_anova_iv2 <- renderUI({
      selectInput("anova_iv2", "Second Independent Variable (Optional)", choices = c("None", char_factor_cols))
    })
    
    output$select_chi_x <- renderUI({ selectInput("chi_x", "Row Variable (Categorical)", choices = c("", char_factor_cols)) })
    output$select_chi_y <- renderUI({ selectInput("chi_y", "Column Variable (Categorical)", choices = c("", char_factor_cols)) })
    output$select_normality_var <- renderUI({ selectInput("normality_var", "Select Variable for Normality Check (Numeric)", choices = c("", numeric_cols)) })
    
    output$prop_variable_ui <- renderUI({ selectInput("prop_variable", "Select a categorical variable:", choices = c("", char_factor_cols)) })
    output$two_prop_var_ui <- renderUI({ selectInput("prop_var", "Select Proportion Variable:", choices = c("", char_factor_cols)) })
    output$two_prop_group_var_ui <- renderUI({ selectInput("two_prop_group_var", "Select Grouping Variable:", choices = c("", char_factor_cols)) })
    
    output$select_mw_variable <- renderUI({ selectInput("mw_variable", "Numeric Variable:", choices = c("", numeric_cols)) })
    output$select_mw_group <- renderUI({ selectInput("mw_group", "Grouping Variable (must have 2 levels):", choices = c("", char_factor_cols)) })
    
    output$select_kw_variable <- renderUI({ selectInput("kw_variable", "Numeric Variable:", choices = c("", numeric_cols)) })
    output$select_kw_group <- renderUI({ selectInput("kw_group", "Grouping Variable (2+ levels):", choices = c("", char_factor_cols)) })
  })
  
  observe({
    df <- data_r()
    req(df, input$prop_variable)
    vals <- unique(na.omit(df[[input$prop_variable]]))
    output$success_value_ui <- renderUI({
      selectInput("success_value", "Success Value:", choices = vals, selected = vals[1])
    })
  })
  
  observe({
    df <- data_r()
    req(df, input$prop_var, input$two_prop_group_var)
    success_vals <- unique(na.omit(df[[input$prop_var]]))
    output$two_prop_success_ui <- renderUI({
      selectInput("two_prop_success", "Select Success Value:", choices = success_vals, selected = success_vals[1])
    })
    group_vals <- unique(na.omit(df[[input$two_prop_group_var]]))
    output$two_prop_group1_ui <- renderUI({
      selectInput("two_prop_group1", "Value for Group 1:", choices = group_vals, selected = group_vals[1])
    })
    output$two_prop_group2_ui <- renderUI({
      req(input$two_prop_group1)
      remaining_vals <- setdiff(group_vals, input$two_prop_group1)
      selectInput("two_prop_group2", "Value for Group 2:", choices = remaining_vals, selected = if(length(remaining_vals)>0) remaining_vals[1] else NULL)
    })
  })
  
  # --- START: OPTIMIZED Descriptive Statistics Logic ---
  
  output$summary_stats_output <- renderDT({
    df <- data_r()
    req(df, input$descriptive_variable, input$descriptive_variable %in% names(df))
    var_name <- input$descriptive_variable
    group_var <- input$group_by_variable
    
    if (var_name == "") return(NULL)
    
    is_strictly_categorical <- is.character(df[[var_name]]) || is.factor(df[[var_name]])
    is_categorical_like <- function(vec) {
      is.character(vec) || is.factor(vec) || (is.numeric(vec) && length(unique(na.omit(vec))) < 15)
    }
    
    final_summary_data <- NULL
    
    if (group_var != "None" && group_var %in% names(df)) {
      group_is_cat_like <- is_categorical_like(df[[group_var]])
      if (is_strictly_categorical && group_is_cat_like) {
        tbl <- table(df[[var_name]], df[[group_var]], dnn = c(var_name, group_var))
        final_summary_data <- as.data.frame.matrix(addmargins(tbl))
        datatable(final_summary_data, options = list(dom = 't'), rownames = TRUE, caption = 'Two-Way Contingency Table (Counts)')
      } else if (is.numeric(df[[var_name]]) && group_is_cat_like) {
        final_summary_data <- df %>%
          group_by(.data[[group_var]]) %>%
          summarise(
            N_Valid = sum(!is.na(.data[[var_name]])),
            N_Missing = sum(is.na(.data[[var_name]])),
            Mean = round(mean(.data[[var_name]], na.rm = TRUE), 2),
            Median = round(median(.data[[var_name]], na.rm = TRUE), 2),
            SD = round(sd(.data[[var_name]], na.rm = TRUE), 2),
            Min = round(min(.data[[var_name]], na.rm = TRUE), 2),
            Q1 = round(as.numeric(quantile(.data[[var_name]], 0.25, na.rm = TRUE)), 2),
            Q3 = round(as.numeric(quantile(.data[[var_name]], 0.75, na.rm = TRUE)), 2),
            Max = round(max(.data[[var_name]], na.rm = TRUE), 2)
          )
        datatable(final_summary_data, options = list(dom = 't'), rownames = FALSE, caption = 'Descriptive Statistics (Grouped)')
      } else {
        datatable(data.frame(Message = "This combination is not supported."), options = list(dom = 't'))
      }
    } else {
      if (is_strictly_categorical) {
        final_summary_data <- df %>%
          filter(!is.na(.data[[var_name]])) %>%
          count(.data[[var_name]], name = "Frequency") %>%
          mutate(Relative_Frequency = scales::percent(Frequency / sum(Frequency), accuracy = 0.1))
        datatable(final_summary_data, options = list(dom = 't'), rownames = FALSE, caption = 'Frequency Distribution Table')
      } else {
        final_summary_data <- data.frame(
          Statistic = c("Valid (N)", "Missing (N)", "Mean", "Median", "SD", "Min", "Q1", "Q3", "Max"),
          Value = c(
            sum(!is.na(df[[var_name]])),
            sum(is.na(df[[var_name]])),
            round(mean(df[[var_name]], na.rm = TRUE), 2),
            round(median(df[[var_name]], na.rm = TRUE), 2),
            round(sd(df[[var_name]], na.rm = TRUE), 2),
            round(min(df[[var_name]], na.rm = TRUE), 2),
            round(as.numeric(quantile(df[[var_name]], 0.25, na.rm = TRUE)), 2),
            round(as.numeric(quantile(df[[var_name]], 0.75, na.rm = TRUE)), 2),
            round(max(df[[var_name]], na.rm = TRUE), 2)
          )
        )
        datatable(final_summary_data, options = list(dom = 't'), rownames = FALSE, caption = 'Descriptive Statistics')
      }
    }
    
    desc_results$summary <- final_summary_data
  }, server = FALSE) %>% bindEvent(input$analyze_descriptive)
  
  # --- Histogram (CHANGED: renderPlot -> renderPlotly, wrapped in ggplotly) ---
  output$histogram_plot <- renderPlotly({
    df <- data_r()
    req(df, input$descriptive_variable, input$descriptive_variable %in% names(df))
    var <- input$descriptive_variable
    validate(need(is.numeric(df[[var]]), "Histogram requires a quantitative (numeric) variable."))
    group_var <- input$group_by_variable
    
    y_formatter <- if (input$hist_yaxis_type == "percent") scales::percent_format(accuracy = 1) else NULL
    y_axis_label <- if (input$hist_yaxis_type == "percent") "Percent" else "Count (Frequency)"
    
    gg <- ggplot(df, aes(x = .data[[var]]))
    if (input$hist_yaxis_type == "percent") {
      gg <- gg + geom_histogram(aes(y = after_stat(count / sum(count))), bins = input$hist_bins, fill = "steelblue", color = "white")
    } else {
      gg <- gg + geom_histogram(bins = input$hist_bins, fill = "steelblue", color = "white")
    }
    gg <- gg + scale_y_continuous(labels = y_formatter)
    
    if (group_var != "None" && group_var %in% names(df)) {
      gg <- gg + facet_wrap(vars(.data[[group_var]]), scales = "free") +
        labs(title = paste("Histogram of", var, "by", group_var), x = var, y = y_axis_label)
      if (isTRUE(input$show_mean_median)) {
        summary_lines <- df %>%
          group_by(.data[[group_var]]) %>%
          summarise(mean_val = mean(.data[[var]], na.rm = TRUE),
                    median_val = median(.data[[var]], na.rm = TRUE))
        gg <- gg +
          geom_vline(data = summary_lines, aes(xintercept = mean_val), color = "red", linetype = "dashed") +
          geom_vline(data = summary_lines, aes(xintercept = median_val), color = "green", linetype = "dashed")
      }
    } else {
      gg <- gg + labs(title = paste("Histogram of", var), x = var, y = y_axis_label)
      if (isTRUE(input$show_mean_median)) {
        gg <- gg +
          geom_vline(aes(xintercept = mean(df[[var]], na.rm = TRUE)), color = "red", linetype = "dashed") +
          geom_vline(aes(xintercept = median(df[[var]], na.rm = TRUE)), color = "green", linetype = "dashed")
      }
    }
    desc_results$hist <- gg
    ggplotly(gg)
  }) %>% bindEvent(input$analyze_descriptive)
  
  # --- Box Plot (CHANGED: renderPlot -> renderPlotly, wrapped in ggplotly) ---
  output$boxplot_plot <- renderPlotly({
    df <- data_r()
    req(df, input$descriptive_variable, input$descriptive_variable %in% names(df))
    var_name <- input$descriptive_variable
    group_var <- input$group_by_variable
    p <- NULL
    if (is.numeric(df[[var_name]])) {
      if (group_var != "None" && group_var %in% names(df) && (is.character(df[[group_var]]) || is.factor(df[[group_var]]))) {
        p <- ggplot(df, aes(x = as.factor(.data[[group_var]]), y = .data[[var_name]], fill = as.factor(.data[[group_var]]))) +
          geom_boxplot() + labs(title = paste("Boxplot of", var_name, "by", group_var), x = group_var, y = var_name) + theme(legend.position = "none")
      } else {
        p <- ggplot(df, aes(y = .data[[var_name]])) +
          geom_boxplot(fill = "lightgreen") + labs(title = paste("Boxplot of", var_name), y = var_name, x = NULL) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      }
    } else {
      p <- ggplot() + annotate("text", x = 0, y = 0, label = "Box plot requires a numeric variable.", size = 5) + theme_void()
    }
    desc_results$box <- p
    ggplotly(p)
  }) %>% bindEvent(input$analyze_descriptive)
  
  # --- Density Plot (unchanged) ---
  output$density_plot <- renderPlot({
    df <- data_r()
    req(df, input$descriptive_variable, input$descriptive_variable %in% names(df))
    var <- input$descriptive_variable
    group_var <- input$group_by_variable
    validate(need(is.numeric(df[[var]]), "Density plot requires a numeric variable."))
    p <- NULL
    if (group_var != "None" && group_var %in% names(df)) {
      p <- ggplot(df, aes(x = .data[[var]])) +
        geom_density(fill = "blue", alpha = 0.4) +
        facet_wrap(vars(.data[[group_var]]), scales = "free") +
        labs(title = paste("Density Plot of", var, "by", group_var), x = var, y = "Density")
    } else {
      p <- ggplot(df, aes(x = .data[[var]])) +
        geom_density(fill = "blue", alpha = 0.4) +
        labs(title = paste("Density Plot of", var), x = var, y = "Density")
    }
    desc_results$density <- p
    p
  }) %>% bindEvent(input$analyze_descriptive)
  
  # --- Pie Chart (unchanged) ---
  output$pie_chart_plot <- renderPlot({
    df <- data_r()
    req(df, input$descriptive_variable, input$descriptive_variable %in% names(df))
    var_name <- input$descriptive_variable
    group_var <- input$group_by_variable
    is_categorical_like <- function(vec) {
      is.character(vec) || is.factor(vec) || (is.numeric(vec) && length(unique(na.omit(vec))) < 15)
    }
    validate(need(is_categorical_like(df[[var_name]]), "Pie chart requires a categorical variable (e.g., text or numeric with few values)."))
    if (group_var != "None" && group_var %in% names(df)) {
      df_summary <- df %>%
        filter(!is.na(.data[[var_name]]), !is.na(.data[[group_var]])) %>%
        group_by(.data[[group_var]], .data[[var_name]]) %>%
        summarise(Count = n(), .groups = "drop_last") %>%
        mutate(Percentage = Count / sum(Count), Label = paste0(round(Percentage * 100, 1), "%"))
      ggplot(df_summary, aes(x = "", y = Percentage, fill = as.factor(.data[[var_name]]))) +
        geom_col(width = 1) + coord_polar(theta = "y") +
        facet_wrap(vars(.data[[group_var]])) +
        geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "white", size = 4) +
        theme_void() + labs(title = paste("Pie Chart of", var_name, "by", group_var), fill = var_name)
    } else {
      df_summary <- df %>%
        filter(!is.na(.data[[var_name]])) %>%
        count(.data[[var_name]], name = "Count") %>%
        mutate(Percentage = Count / sum(Count), Label = paste0(round(Percentage * 100, 1), "%"))
      ggplot(df_summary, aes(x = "", y = Percentage, fill = as.factor(.data[[var_name]]))) +
        geom_col(width = 1) + coord_polar(theta = "y") +
        geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "white", size = 4) +
        theme_void() + labs(title = paste("Pie Chart of", var_name), fill = var_name)
    }
  }) %>% bindEvent(input$analyze_descriptive)
  
  # --- Bar Chart (unchanged) ---
  output$barchart_plot <- renderPlot({
    df <- data_r()
    req(df, input$descriptive_variable, input$descriptive_variable %in% names(df))
    var_name <- input$descriptive_variable
    group_var <- input$group_by_variable
    is_categorical_like <- function(vec) {
      is.character(vec) || is.factor(vec) || (is.numeric(vec) && length(unique(na.omit(vec))) < 15)
    }
    validate(need(is_categorical_like(df[[var_name]]), "Bar chart requires a categorical variable (e.g., text or numeric with few values)."))
    y_axis_var <- if (input$barchart_yaxis_type == "proportion") "Proportion" else "Frequency"
    y_axis_label <- if (input$barchart_yaxis_type == "proportion") "Relative Frequency" else "Count"
    if (group_var != "None" && group_var %in% names(df)) {
      df_summary <- df %>%
        filter(!is.na(.data[[var_name]]), !is.na(.data[[group_var]])) %>%
        group_by(.data[[group_var]], .data[[var_name]]) %>%
        summarise(Frequency = n(), .groups = "drop_last") %>%
        mutate(Proportion = Frequency / sum(Frequency))
      gg <- ggplot(df_summary, aes(x = as.factor(.data[[var_name]]), y = .data[[y_axis_var]], fill = as.factor(.data[[var_name]]))) +
        geom_col() + facet_wrap(vars(.data[[group_var]]), scales = "free_y") +
        labs(title = paste("Bar Chart of", var_name, "by", group_var), x = var_name, y = y_axis_label, fill = var_name) +
        theme(legend.position = "none")
    } else {
      df_summary <- df %>%
        filter(!is.na(.data[[var_name]])) %>%
        count(.data[[var_name]], name = "Frequency") %>%
        mutate(Proportion = Frequency / sum(Frequency))
      gg <- ggplot(df_summary, aes(x = as.factor(.data[[var_name]]), y = .data[[y_axis_var]])) +
        geom_col(fill = "cornflowerblue") +
        labs(title = paste("Bar Chart of", var_name), x = var_name, y = y_axis_label)
    }
    if (input$barchart_yaxis_type == "proportion") {
      gg <- gg + scale_y_continuous(labels = scales::percent)
    }
    gg
  }) %>% bindEvent(input$analyze_descriptive)
  
  # --- DOWNLOAD HANDLER (unchanged — uses desc_results, which still hold raw ggplot objects) ---
  output$download_descriptive_report <- downloadHandler(
    filename = function() {
      paste("OpenStats_Report_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      id <- showNotification("Generating report...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      params <- list(
        dataset = data_r(),
        var_name = input$descriptive_variable,
        group_var = input$group_by_variable,
        plot_hist = desc_results$hist,
        plot_box = desc_results$box,
        plot_density = desc_results$density,
        summary_table = desc_results$summary
      )
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
  # --- END OPTIMIZED Descriptive Statistics Logic ---
  
  # --- Scatter Plot (CHANGED: renderPlot -> renderPlotly, wrapped in ggplotly) ---
  output$scatter_plot <- renderPlotly({
    df <- data_r()
    req(df, input$scatter_x, input$scatter_y, input$scatter_x %in% names(df), input$scatter_y %in% names(df))
    x_var <- input$scatter_x
    y_var <- input$scatter_y
    validate(need(is.numeric(df[[x_var]]) && is.numeric(df[[y_var]]), "Scatter plots require numeric variables."))
    p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
      geom_point(color = "darkblue") +
      labs(title = paste("Scatter Plot of", y_var, "vs", x_var), x = x_var, y = y_var)
    ggplotly(p)
  }) %>% bindEvent(input$generate_scatter)
  
  output$dot_plot <- renderPlot({
    df <- data_r()
    req(df, input$dot_plot_variable, input$dot_plot_variable %in% names(df))
    var <- input$dot_plot_variable
    validate(need(is.numeric(df[[var]]), "Dot plot requires a numeric variable."))
    data_vec <- na.omit(df[[var]])
    if (length(data_vec) < 2) {
      return(ggplot() + annotate("text", x=0,y=0, label="Not enough data for dot plot.") + theme_void())
    }
    data_range <- max(data_vec) - min(data_vec)
    dynamic_binwidth <- if (data_range == 0) 1 else data_range / 30
    p <- ggplot(df, aes(x = .data[[var]])) +
      geom_dotplot(binaxis = 'x', stackdir = 'up', dotsize = 0.8, fill = "steelblue", binwidth = dynamic_binwidth) +
      labs(title = paste("Dot Plot of", var), x = var, y = "Frequency") +
      theme_light()
    current_dot_plot(p)
    p
  }) %>% bindEvent(input$generate_dot_plot)
  
  output$download_dot_plot <- downloadHandler(
    filename = function() { paste("dot-plot-", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      req(current_dot_plot())
      ggsave(filename = file, plot = current_dot_plot(), width = 8, height = 6, dpi = 300, units = "in")
    }
  )
  
  # --- All Inferential and Regression Logic (unchanged) ---
  
  observeEvent(input$run_anova, {
    df <- data_r()
    req(df, input$anova_dv, input$anova_iv)
    dv <- input$anova_dv
    iv1 <- input$anova_iv
    iv2 <- input$anova_iv2
    validate(
      need(input$anova_dv %in% names(df), "Dependent variable not found in data."),
      need(input$anova_iv %in% names(df), "Independent variable not found in data.")
    )
    is_categorical_like <- function(vec) {
      is.character(vec) || is.factor(vec) || (is.numeric(vec) && length(unique(na.omit(vec))) < 15)
    }
    validate(
      need(is.numeric(df[[dv]]), "Dependent variable must be numeric."),
      need(is_categorical_like(df[[iv1]]), "Independent variable must be categorical.")
    )
    df[[iv1]] <- as.factor(df[[iv1]])
    output$anova_output <- renderPrint({
      if (!is.null(iv2) && iv2 != "None") {
        validate(need(input$anova_iv2 %in% names(df), "Second independent variable not found."))
        validate(need(is_categorical_like(df[[iv2]]), "Second independent variable must be categorical."))
        df[[iv2]] <- as.factor(df[[iv2]])
        formula_str <- paste(dv, "~", iv1, "*", iv2)
        model <- aov(as.formula(formula_str), data = df)
        cat("Two-Way ANOVA Summary (", formula_str, "):\n", sep = "")
        model_summary <- summary(model)
        print(model_summary)
        if (any(model_summary[[1]]$`Pr(>F)` < 0.05, na.rm = TRUE)) {
          cat("\n-----------------------------------\n")
          cat("Post-Hoc Test (Tukey HSD):\n")
          print(TukeyHSD(model))
        } else {
          cat("\nNo significant main effects or interactions found.\n")
        }
      } else {
        formula_str <- paste(dv, "~", iv1)
        model <- aov(as.formula(formula_str), data = df)
        cat("One-Way ANOVA Summary (", formula_str, "):\n", sep = "")
        model_summary <- summary(model)
        print(model_summary)
        if (model_summary[[1]]$`Pr(>F)`[1] < 0.05) {
          cat("\n-----------------------------------\n")
          cat("Post-Hoc Test (Tukey HSD):\n")
          print(TukeyHSD(model))
        } else {
          cat("\nNo significant differences found between groups.\n")
        }
      }
    })
  })
  
  observeEvent(input$run_prop_test, {
    output$prop_test_result <- renderPrint({
      if (isTRUE(input$prop_test_manual_mode)) {
        req(input$prop_manual_successes, input$prop_manual_trials, input$prop_null)
        successes <- input$prop_manual_successes
        total <- input$prop_manual_trials
        p_null <- input$prop_null
        if (successes > total) {
          cat("Error: Number of successes cannot be greater than the number of trials.")
          return()
        }
        sample_p <- successes / total
        se <- sqrt(p_null * (1 - p_null) / total)
        test <- prop.test(x = successes, n = total, p = p_null, alternative = input$prop_alternative)
        estimate_string <- paste0("Sample Estimate (p\U0302): ", round(sample_p, 4), " \U00B1 ", round(se, 4), " (SE)")
        cat("One-Proportion Test (Manual Input)\n")
        print(test)
        cat("\n----------------------------------\n")
        cat(estimate_string)
      } else {
        df <- data_r()
        req(df, input$prop_variable, input$success_value, input$prop_null)
        var <- input$prop_variable
        success_val <- input$success_value
        p_null <- input$prop_null
        successes <- sum(df[[var]] == success_val, na.rm = TRUE)
        total <- sum(!is.na(df[[var]]))
        if (total == 0) {
          cat("Error: No valid data for this variable.")
          return()
        }
        sample_p <- successes / total
        se <- sqrt(p_null * (1 - p_null) / total)
        test <- prop.test(x = successes, n = total, p = p_null, alternative = input$prop_alternative)
        estimate_string <- paste0("Sample Estimate (p\U0302): ", round(sample_p, 4), " \U00B1 ", round(se, 4), " (SE)")
        cat("One-Proportion Test (from Dataset)\n")
        cat("Variable:", var, "| Success Value:", success_val, "\n")
        print(test)
        cat("\n----------------------------------\n")
        cat(estimate_string)
      }
    })
  })
  
  observeEvent(input$run_two_prop_test, {
    df <- data_r()
    req(df, input$prop_var, input$two_prop_group_var, input$two_prop_group1, input$two_prop_group2, input$two_prop_success)
    prop_var <- input$prop_var
    group_var <- input$two_prop_group_var
    group1 <- input$two_prop_group1
    group2 <- input$two_prop_group2
    success_val <- input$two_prop_success
    is_categorical_like <- function(vec) {
      is.character(vec) || is.factor(vec) || (is.numeric(vec) && length(unique(na.omit(vec))) < 15)
    }
    if (!is_categorical_like(df[[prop_var]])) {
      output$two_prop_test_result <- renderPrint({ "Proportion variable must be categorical or discrete numeric (e.g., 0/1)." })
      return(NULL)
    }
    if (!is_categorical_like(df[[group_var]])) {
      output$two_prop_test_result <- renderPrint({ "Grouping variable must be categorical or discrete numeric (e.g., 0/1)." })
      return(NULL)
    }
    df[[prop_var]] <- as.factor(df[[prop_var]])
    df[[group_var]] <- as.factor(df[[group_var]])
    df_filtered <- df %>% filter(.data[[group_var]] %in% c(group1, group2))
    if (nrow(df_filtered) == 0) {
      output$two_prop_test_result <- renderPrint({"No data found for selected groups."})
      return(NULL)
    }
    group1_data <- df_filtered %>% filter(.data[[group_var]] == group1)
    group2_data <- df_filtered %>% filter(.data[[group_var]] == group2)
    n1 <- nrow(group1_data); n2 <- nrow(group2_data)
    x1 <- sum(group1_data[[prop_var]] == success_val, na.rm = TRUE)
    x2 <- sum(group2_data[[prop_var]] == success_val, na.rm = TRUE)
    if (n1 == 0 || n2 == 0) {
      output$two_prop_test_result <- renderPrint({"One or both groups have no data."})
      return(NULL)
    }
    p1 <- x1 / n1; p2 <- x2 / n2
    se1 <- sqrt(p1 * (1 - p1) / n1); se2 <- sqrt(p2 * (1 - p2) / n2)
    se_unpooled <- sqrt(((p1 * (1 - p1)) / n1) + ((p2 * (1 - p2)) / n2))
    p_pool <- (x1 + x2) / (n1 + n2)
    se_pool <- sqrt(p_pool * (1 - p_pool) * ((1 / n1) + (1 / n2)))
    output$two_prop_test_result <- renderPrint({
      cat("Two-Proportion Test\n\n")
      cat("Group 1:", group1, "| Successes:", x1, "/", n1, "| p\U0302:", round(p1, 4), "| SE:", round(se1, 4), "\n")
      cat("Group 2:", group2, "| Successes:", x2, "/", n2, "| p\U0302:", round(p2, 4), "| SE:", round(se2, 4), "\n")
      cat("------------------------------------------------------------------\n")
      cat("Unpooled Standard Error (For CI): ", round(se_unpooled, 4), "\n")
      cat("Pooled Standard Error (For Test Stat): ", round(se_pool, 4), "\n")
      cat("Pooled Proportion (p\U0302): ", round(p_pool, 4), "\n\n")
      print(prop.test(x = c(x1, x2), n = c(n1, n2), alternative = input$two_prop_alternative))
    })
  })
  
  observeEvent(input$run_ht, {
    req(data_r(), input$ht_variable)
    df <- data_r()
    var_name <- input$ht_variable
    shiny::validate(
      shiny::need(is.numeric(df[[var_name]]), "T-tests require a numeric variable. Please select a different variable (e.g., 'mpg').")
    )
    group_var <- input$ht_group_variable
    mu <- input$ht_mu
    output$ht_output <- renderPrint({
      if (!is.null(group_var) && group_var != "None" && group_var %in% names(df)) {
        df_filtered <- df %>% filter(!is.na(.data[[var_name]]), !is.na(.data[[group_var]]))
        df_filtered[[group_var]] <- as.factor(df_filtered[[group_var]])
        if (nlevels(df_filtered[[group_var]]) != 2) {
          cat("Error: Grouping variable must have exactly two levels for two-sample t-test.\n")
          return()
        }
        sample_sizes <- table(df_filtered[[group_var]])
        n1 <- sample_sizes[1]; n2 <- sample_sizes[2]
        test_result <- t.test(as.formula(paste(var_name, "~", group_var)),
                              data = df_filtered,
                              alternative = input$ht_alternative,
                              var.equal = input$ht_var_equal)
        if (isTRUE(input$ht_var_equal)) {
          se_diff <- test_result$stderr
          cat("Two-Sample t-test (Student's)\n------------------------------\n")
        } else {
          mean_diff <- test_result$estimate[1] - test_result$estimate[2]
          t_stat <- test_result$statistic
          se_diff <- mean_diff / t_stat
          cat("Two-Sample t-test (Welch's)\n----------------------------\n")
        }
        cat("Sample Sizes (n1, n2):", n1, ",", n2, "\n")
        cat("Standard Error of Difference:", round(se_diff, 4), "\n\n")
        print(test_result)
      } else {
        sample_data <- na.omit(df[[var_name]])
        test_result <- t.test(sample_data, mu = mu, alternative = input$ht_alternative)
        n <- length(sample_data); s <- sd(sample_data); se <- s / sqrt(n)
        cat("One-Sample t-test\n-----------------\n")
        cat("Sample Size (n):", n, "\n")
        cat("Sample Mean:", round(test_result$estimate, 4), "\n")
        cat("Standard Error:", round(se, 4), "\n\n")
        print(test_result)
      }
    })
  })
  
  observeEvent(input$run_paired_ttest, {
    df <- data_r()
    req(df, input$paired_var1, input$paired_var2)
    var1 <- input$paired_var1; var2 <- input$paired_var2
    if (var1 == var2) { output$paired_ttest_result <- renderPrint({"Error: Please select two different variables."}); return(NULL) }
    if (!is.numeric(df[[var1]]) || !is.numeric(df[[var2]])) { output$paired_ttest_result <- renderPrint({"Both variables must be numeric."}); return(NULL) }
    df_clean <- df %>% select(all_of(c(var1, var2))) %>% na.omit()
    if (nrow(df_clean) < 2) { output$paired_ttest_result <- renderPrint({"Not enough complete data pairs for a paired t-test."}); return(NULL) }
    n_pairs <- nrow(df_clean)
    differences <- df_clean[[var1]] - df_clean[[var2]]
    se_of_diff <- sd(differences) / sqrt(n_pairs)
    test <- t.test(df_clean[[var1]], df_clean[[var2]], paired = TRUE, alternative = input$paired_alternative)
    output$paired_ttest_result <- renderPrint({
      raw_output <- capture.output(print(test))
      data_line_index <- grep("data:", raw_output)
      raw_output[data_line_index] <- paste("data: ", var1, "and", var2)
      cat("Paired t-test\n"); cat("-----------------\n")
      cat("Number of Pairs (n):", n_pairs, "\n")
      cat("Standard Error of Mean Difference:", round(se_of_diff, 4), "\n\n")
      cat(paste(raw_output, collapse = "\n"))
    })
  })
  
  observeEvent(input$run_chi_sq, {
    output$chi_sq_table_output <- renderPrint({
      contingency_table <- contingency_table_r()
      req(contingency_table)
      table_type <- input$chisq_table_type
      if (table_type == "counts") {
        cat("Two-Way Table (Observed Counts):\n\n")
        print(addmargins(contingency_table))
      } else {
        cat("Two-Way Table (Row Proportions):\n\n")
        print(round(prop.table(contingency_table, margin = 1), 4))
      }
    })
    output$chi_sq_test_output <- renderPrint({
      contingency_table <- contingency_table_r()
      req(contingency_table)
      cat("Chi-Square Test of Independence:\n\n")
      print(chisq.test(contingency_table))
    })
  })
  
  contingency_table_r <- reactive({
    df <- data_r()
    req(df, input$chi_x, input$chi_y,
        input$chi_x %in% names(df), input$chi_y %in% names(df),
        input$chi_x != "", input$chi_y != "")
    table(df[[input$chi_x]], df[[input$chi_y]], dnn = c(input$chi_x, input$chi_y))
  })
  
  observeEvent(input$run_normality, {
    df <- data_r()
    req(df, input$normality_var, input$normality_var %in% names(df))
    var_name <- input$normality_var
    validate(need(is.numeric(df[[var_name]]), "Normality tests require a numeric variable."))
    output$normality_test_output <- renderPrint({
      clean_data <- na.omit(df[[var_name]]); n <- length(clean_data)
      cat("Normality Tests for:", var_name, "\n"); cat("Sample size (n):", n, "\n\n")
      if (n < 3) { cat("Not enough data for normality tests (need at least 3 observations)."); return() }
      if (n <= 5000) { cat("Shapiro-Wilk Test:\n"); print(shapiro.test(clean_data)) }
      else cat("Note: Sample size > 5000; Shapiro-Wilk not applicable.\n")
      if (n >= 4) {
        cat("\nKolmogorov-Smirnov Test (vs. Normal):\n")
        print(ks.test(clean_data, "pnorm", mean = mean(clean_data), sd = sd(clean_data)))
      }
    })
    output$normality_plot_output <- renderPlot({
      clean_data <- na.omit(df[[var_name]])
      par(mfrow = c(1, 2))
      hist(clean_data, main = paste("Histogram of", var_name), xlab = var_name, col = "steelblue", border = "white")
      qqnorm(clean_data, main = paste("Q-Q Plot of", var_name)); qqline(clean_data, col = "red")
    })
  })
  
  observeEvent(input$run_regression, {
    df <- data_r()
    req(df, input$regression_dv, input$regression_iv)
    dv <- input$regression_dv; ivs <- input$regression_iv
    validate(
      need(dv %in% names(df), "Dependent variable not found."),
      need(all(ivs %in% names(df)), "One or more independent variables not found."),
      need(is.numeric(df[[dv]]), "Dependent variable must be numeric."),
      need(all(sapply(ivs, function(v) is.numeric(df[[v]]))), "All independent variables must be numeric.")
    )
    formula_str <- if (isTRUE(input$log_transform_dv_reg)) paste("log(", dv, ") ~", paste(ivs, collapse = " + "))
    else paste(dv, "~", paste(ivs, collapse = " + "))
    model <- lm(as.formula(formula_str), data = df)
    output$regression_summary <- renderPrint({ cat("Linear Regression Summary\nFormula:", formula_str, "\n\n"); print(summary(model)) })
    output$regression_diagnostic_plots <- renderPlot({ par(mfrow = c(2, 2)); plot(model) })
    output$regression_assumption_checks <- renderPrint({
      cat("Breusch-Pagan Test for Heteroscedasticity:\n"); print(car::ncvTest(model))
      cat("\nVariance Inflation Factors (VIF) for Multicollinearity:\n")
      if (length(ivs) > 1) print(car::vif(model)) else cat("VIF requires at least 2 predictors.\n")
      cat("\nDurbin-Watson Test for Autocorrelation:\n"); print(car::durbinWatsonTest(model))
    })
  })
  
  observeEvent(input$run_logistic, {
    df <- data_r()
    req(df, input$logistic_dv, input$logistic_iv)
    dv <- input$logistic_dv; ivs <- input$logistic_iv
    validate(need(dv %in% names(df), "Dependent variable not found."), need(all(ivs %in% names(df)), "One or more IVs not found."))
    df[[dv]] <- as.factor(df[[dv]])
    formula_str <- paste(dv, "~", paste(ivs, collapse = " + "))
    model <- glm(as.formula(formula_str), data = df, family = binomial)
    output$logistic_summary <- renderPrint({ cat("Logistic Regression Summary\n"); print(summary(model)) })
  })
  
  observeEvent(input$run_correlation, {
    df <- data_r()
    req(df, input$correlation_vars)
    vars <- input$correlation_vars
    validate(need(length(vars) >= 2, "Please select at least 2 variables."), need(all(vars %in% names(df)), "Selected variables not found."))
    output$correlation_matrix <- renderPrint({
      cat("Pearson Correlation Matrix:\n\n")
      print(round(cor(df[, vars, drop = FALSE], use = "pairwise.complete.obs"), 4))
    })
  })
  
  # --- Probability Logic (unchanged) ---
  
  output$prob_required_inputs <- renderUI({
    req(input$prob_calc_type)
    type <- input$prob_calc_type
    if (type == "union") {
      tagList(numericInput("prob_pa", "P(A):", value = 0.4, min=0, max=1, step=0.01),
              numericInput("prob_pb", "P(B):", value = 0.3, min=0, max=1, step=0.01),
              numericInput("prob_pab","P(A and B):", value = 0.1, min=0, max=1, step=0.01))
    } else if (type == "conditional") {
      tagList(numericInput("prob_pab","P(A and B):", value = 0.1, min=0, max=1, step=0.01),
              numericInput("prob_pb", "P(B):", value = 0.3, min=0, max=1, step=0.01))
    } else if (type == "check_relationship") {
      tagList(numericInput("prob_pa", "P(A):", value = 0.4, min=0, max=1, step=0.01),
              numericInput("prob_pb", "P(B):", value = 0.3, min=0, max=1, step=0.01),
              numericInput("prob_pab","P(A and B):", value = 0.1, min=0, max=1, step=0.01))
    }
  })
  
  observeEvent(input$calculate_basic_probs, {
    type <- input$prob_calc_type
    output$calculated_output_title <- renderPrint({
      if (type == "union")             cat("P(A or B) =", input$prob_pa + input$prob_pb - input$prob_pab)
      else if (type == "conditional")  cat("P(A|B) =", round(input$prob_pab / input$prob_pb, 4))
      else if (type == "check_relationship") cat("Checking relationships...")
    })
    output$calculated_output <- renderPrint({
      if (type == "check_relationship") {
        pa <- input$prob_pa; pb <- input$prob_pb; pab <- input$prob_pab
        cat("Independence Check:\n")
        if (abs(pab - pa * pb) < 0.0001) cat("  A and B are INDEPENDENT (P(A\u2229B) \u2248 P(A)\u00b7P(B))\n")
        else cat("  A and B are NOT independent.\n")
        cat("\nMutually Exclusive Check:\n")
        if (pab == 0) cat("  A and B are MUTUALLY EXCLUSIVE (P(A\u2229B) = 0)\n")
        else cat("  A and B are NOT mutually exclusive.\n")
      }
    })
  })
  
  output$normal_inputs <- renderUI({
    req(input$normal_prob_type)
    type <- input$normal_prob_type
    if (type == "less" || type == "greater") numericInput("normal_x", "Value of x:", value = 0)
    else if (type == "between") tagList(numericInput("normal_a","Lower bound (a):", value=-1), numericInput("normal_b","Upper bound (b):", value=1))
    else if (type == "inverse") numericInput("normal_p_val","Probability (e.g., 0.95):", value=0.95, min=0, max=1, step=0.01)
  })
  
  observeEvent(input$calc_normal, {
    mu <- input$normal_mean; sigma <- input$normal_sd; type <- input$normal_prob_type
    output$normal_result <- renderPrint({
      if (type == "less")         cat("P(X <",  input$normal_x, ") =", round(pnorm(input$normal_x, mu, sigma), 6))
      else if (type == "greater") cat("P(X >",  input$normal_x, ") =", round(1 - pnorm(input$normal_x, mu, sigma), 6))
      else if (type == "between") cat("P(", input$normal_a, "< X <", input$normal_b, ") =", round(pnorm(input$normal_b,mu,sigma) - pnorm(input$normal_a,mu,sigma), 6))
      else if (type == "inverse") cat("x such that P(X <= x) =", input$normal_p_val, "is:", round(qnorm(input$normal_p_val, mu, sigma), 6))
    })
    output$normal_plot <- renderPlot({
      x_range <- seq(mu - 4*sigma, mu + 4*sigma, length.out=300)
      df_plot <- data.frame(x=x_range, y=dnorm(x_range,mu,sigma))
      p <- ggplot(df_plot, aes(x,y)) + geom_line() + labs(x="x", y="Density", title="Normal Distribution")
      if (type=="less")    p <- p + geom_area(data=subset(df_plot,x<=input$normal_x), aes(x,y), fill="steelblue", alpha=0.5)
      else if (type=="greater") p <- p + geom_area(data=subset(df_plot,x>=input$normal_x), aes(x,y), fill="steelblue", alpha=0.5)
      else if (type=="between") p <- p + geom_area(data=subset(df_plot,x>=input$normal_a&x<=input$normal_b), aes(x,y), fill="steelblue", alpha=0.5)
      else if (type=="inverse") {
        x_val <- qnorm(input$normal_p_val,mu,sigma)
        p <- p + geom_area(data=subset(df_plot,x<=x_val), aes(x,y), fill="steelblue", alpha=0.5) +
          geom_vline(xintercept=x_val, color="red", linetype="dashed")
      }
      if (isTRUE(input$show_empirical_rule)) {
        p <- p +
          geom_vline(xintercept=c(mu-sigma,mu+sigma),     color="darkgreen",linetype="dashed") +
          geom_vline(xintercept=c(mu-2*sigma,mu+2*sigma), color="orange",   linetype="dashed") +
          geom_vline(xintercept=c(mu-3*sigma,mu+3*sigma), color="red",      linetype="dashed")
      }
      current_normal_plot(p); p
    })
  })
  
  observeEvent(input$calc_binom_prob, {
    n <- input$binom_size; p <- input$binom_prob; k <- input$binom_k; type <- input$binom_type
    output$binom_prob_output <- renderPrint({
      if (type=="P(X = x)")   cat("P(X =",  k, ") =", round(dbinom(k,n,p), 6))
      else if (type=="P(X <= x)") cat("P(X <=", k, ") =", round(pbinom(k,n,p), 6))
      else if (type=="P(X >= x)") cat("P(X >=", k, ") =", round(1-pbinom(k-1,n,p), 6))
    })
  })
  
  output$binom_summary_stats <- renderPrint({
    n <- input$binom_size; p <- input$binom_prob
    cat("Mean (\u03bc) =", n*p, "\n")
    cat("Variance (\u03c3\u00b2) =", round(n*p*(1-p),4), "\n")
    cat("Std Dev (\u03c3) =", round(sqrt(n*p*(1-p)),4), "\n")
  })
  
  output$binom_pmf_plot <- renderPlot({
    n <- input$binom_size; p <- input$binom_prob
    df_binom <- data.frame(x=0:n, prob=dbinom(0:n,n,p))
    ggplot(df_binom, aes(x=factor(x), y=prob)) + geom_col(fill="steelblue") +
      labs(title=paste("Binomial PMF (n =",n,", p =",p,")"), x="k", y="P(X = k)")
  })
  
  observeEvent(input$solve_binom_k, {
    n <- input$binom_size; p <- input$binom_prob; target_prob <- input$binom_p_for_k
    output$solve_binom_k_output <- renderPrint({
      k_val <- qbinom(target_prob,n,p)
      cat("Smallest x such that P(X \u2264 x) \u2265", target_prob, "is: x =", k_val, "\n")
      cat("Actual P(X \u2264", k_val, ") =", round(pbinom(k_val,n,p), 6))
    })
  })
  
  output$pois_summary_stats <- renderPrint({
    lambda <- input$pois_lambda
    cat("Mean (\u03bc) = \u03bb =", lambda, "\n")
    cat("Variance (\u03c3\u00b2) = \u03bb =", lambda, "\n")
    cat("Std Dev (\u03c3) =", round(sqrt(lambda),4), "\n")
  })
  
  observeEvent(input$calc_pois_prob, {
    lambda <- input$pois_lambda; k <- input$pois_k; type <- input$pois_type
    output$pois_prob_output <- renderPrint({
      if (type=="P(X = k)")   cat("P(X =",  k, ") =", round(dpois(k,lambda), 6))
      else if (type=="P(X <= k)") cat("P(X <=", k, ") =", round(ppois(k,lambda), 6))
      else if (type=="P(X >= k)") cat("P(X >=", k, ") =", round(1-ppois(k-1,lambda), 6))
    })
  })
  
  output$pois_pmf_plot <- renderPlot({
    lambda <- input$pois_lambda
    k_range <- 0:max(20, qpois(0.999, lambda))
    df_pois <- data.frame(x=k_range, prob=dpois(k_range,lambda))
    ggplot(df_pois, aes(x=factor(x), y=prob)) + geom_col(fill="coral") +
      labs(title=paste("Poisson PMF (\u03bb =",lambda,")"), x="k", y="P(X = k)")
  })
  
}