# server.R (Complete Corrected Version with Faceting)

server <- function(input, output, session) {
  
  thematic::thematic_shiny()
  
  observeEvent(input$dark_mode_switch, {
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode_switch)) dark_theme else light_theme
    )
  })
  
  # --- Reactive Values ---
  data_r <- reactiveVal(NULL)
  modal_data_r <- reactiveVal(NULL)
  current_dot_plot <- reactiveVal(NULL)
  current_normal_plot <- reactiveVal(NULL)
  
  
  # --- Data Input and Management Logic ---
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    file_ext <- tools::file_ext(input$file_upload$name)
    df <- NULL
    tryCatch({
      if (file_ext == "csv") {
        df <- read.csv(input$file_upload$datapath, stringsAsFactors = FALSE, na.strings=c("","NA"))
      } else if (file_ext == "xlsx") {
        df <- readxl::read_excel(input$file_upload$datapath, na=c("","NA"))
      } else {
        showNotification("Unsupported file type. Please upload a .csv or .xlsx file.", type = "error")
      }
      if (!is.null(df)) {
        data_r(df)
        showNotification("Dataset uploaded successfully!", type = "message")
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
                         "Cars (mtcars)" = mtcars,
                         "Flowers (iris)" = iris,
                         "Student Exam Scores" = exam_scores)
    data_r(df_to_load)
    removeModal()
    showNotification(paste(input$sample_data_choice, "dataset loaded."), type = "message")
  })
  
  observeEvent(input$clear_manual_data, {
    data_r(NULL)
    showNotification("All data cleared.", type = "message")
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
      data_r(df_converted)
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
      data_r(df)
      showNotification(paste("Cell [", info$row, ",", info$col, "] updated."), type = "message")
    }
  })
  
  # --- UI Dropdown Generation (for non-inferential tabs) ---
  observe({
    df <- data_r()
    if (!is.null(df)) {
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      all_cols <- names(df)
      
      # Descriptive
      output$select_descriptive_variable <- renderUI({ selectInput("descriptive_variable", "Select Variable for Descriptive Stats", choices = c("", all_cols)) })
      output$select_group_by_variable <- renderUI({ selectInput("group_by_variable", "Group By (Optional)", choices = c("None", all_cols)) })
      output$select_scatter_x <- renderUI({ selectInput("scatter_x", "Select X-axis Variable (Numeric)", choices = c("", numeric_cols)) })
      output$select_scatter_y <- renderUI({ selectInput("scatter_y", "Select Y-axis Variable (Numeric)", choices = c("", numeric_cols)) })
      output$select_dot_plot_variable <- renderUI({ selectInput("dot_plot_variable", "Select Variable for Dot Plot", choices = c("", numeric_cols)) })
      
      # Regression & Correlation
      output$select_regression_dv <- renderUI({ selectInput("regression_dv", "Dependent Variable (Numeric)", choices = c("", numeric_cols)) })
      output$select_regression_iv <- renderUI({ selectInput("regression_iv", "Independent Variable(s) (Numeric)", choices = numeric_cols, multiple = TRUE) })
      output$select_correlation_vars <- renderUI({ selectInput("correlation_vars", "Select Variables for Correlation (Numeric)", choices = numeric_cols, multiple = TRUE) })
      # Additions for Logistic Regression
      output$select_logistic_dv <- renderUI({ selectInput("logistic_dv", "Dependent Variable (Binary/Categorical):", choices = c("", all_cols)) })
      output$select_logistic_iv <- renderUI({ selectInput("logistic_iv", "Independent Variable(s):", choices = all_cols, multiple = TRUE) })
    } else {
      # Clear UI when no data is loaded
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
    
    # Mean Tests Panel
    output$select_ht_variable <- renderUI({ selectInput("ht_variable", "Select Variable for t-test (Numeric)", choices = c("", numeric_cols)) })
    output$select_ht_group_variable <- renderUI({ selectInput("ht_group_variable", "Grouping Variable (for Two-Sample Test)", choices = c("None", char_factor_cols)) })
    output$paired_var1_ui <- renderUI({ selectInput("paired_var1", "Select First Variable (Numeric):", choices = c("", numeric_cols)) })
    output$paired_var2_ui <- renderUI({ selectInput("paired_var2", "Select Second Variable (Numeric):", choices = c("", numeric_cols)) })
    output$select_anova_dv <- renderUI({ selectInput("anova_dv", "Dependent Variable (Numeric)", choices = c("", numeric_cols)) })
    output$select_anova_iv <- renderUI({ selectInput("anova_iv", "Independent Variable (Categorical)", choices = c("", char_factor_cols)) })
    output$select_anova_iv2 <- renderUI({ 
      selectInput("anova_iv2", "Second Independent Variable (Optional)", choices = c("None", char_factor_cols)) 
    })
    
    # Categorical & Normality Panels
    output$select_chi_x <- renderUI({ selectInput("chi_x", "Row Variable (Categorical)", choices = c("", char_factor_cols)) })
    output$select_chi_y <- renderUI({ selectInput("chi_y", "Column Variable (Categorical)", choices = c("", char_factor_cols)) })
    output$select_normality_var <- renderUI({ selectInput("normality_var", "Select Variable for Normality Check (Numeric)", choices = c("", numeric_cols)) })
    
    # Proportion Test Panel
    output$prop_variable_ui <- renderUI({ selectInput("prop_variable", "Select a categorical variable:", choices = c("", char_factor_cols)) })
    output$two_prop_var_ui <- renderUI({ selectInput("prop_var", "Select Proportion Variable:", choices = c("", char_factor_cols)) })
    output$two_prop_group_var_ui <- renderUI({ selectInput("two_prop_group_var", "Select Grouping Variable:", choices = c("", char_factor_cols)) })
    
    # Mann-Whitney U Test
    output$select_mw_variable <- renderUI({ selectInput("mw_variable", "Numeric Variable:", choices = c("", numeric_cols)) })
    output$select_mw_group    <- renderUI({ selectInput("mw_group", "Grouping Variable (must have 2 levels):", choices = c("", char_factor_cols)) })
    
    # Kruskal-Wallis Test
    output$select_kw_variable <- renderUI({ selectInput("kw_variable", "Numeric Variable:", choices = c("", numeric_cols)) })
    output$select_kw_group    <- renderUI({ selectInput("kw_group", "Grouping Variable (2+ levels):", choices = c("", char_factor_cols)) })
    
  })
  
  # Observer for One-Proportion Test's 'Success Value' dropdown
  observe({
    df <- data_r()
    req(df, input$prop_variable)
    vals <- unique(na.omit(df[[input$prop_variable]]))
    output$success_value_ui <- renderUI({
      selectInput("success_value", "Success Value:", choices = vals, selected = vals[1])
    })
  })
  
  # Observer for Two-Proportion Test's dependent dropdowns
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
  
  
  # --- START: ROBUST REPLACEMENT for Descriptive Statistics Logic (with Faceting) ---
  observeEvent(input$analyze_descriptive, {
    
    # --- Summary Statistics Table ---
    output$summary_stats_output <- renderDT({
      df <- data_r()
      req(df, input$descriptive_variable, input$descriptive_variable %in% names(df))
      var_name <- input$descriptive_variable
      group_var <- input$group_by_variable
      
      if (var_name == "") return(NULL)
      
      is_strictly_categorical <- is.character(df[[var_name]]) || is.factor(df[[var_name]])
      is_categorical_like <- function(vec) { is.character(vec) || is.factor(vec) || (is.numeric(vec) && length(unique(na.omit(vec))) < 15) }
      
      if (group_var != "None" && group_var %in% names(df)) {
        group_is_cat_like <- is_categorical_like(df[[group_var]])
        if (is_strictly_categorical && group_is_cat_like) {
          tbl <- table(df[[var_name]], df[[group_var]], dnn = c(var_name, group_var))
          datatable(as.data.frame.matrix(addmargins(tbl)), options = list(dom = 't'), rownames = TRUE, caption = 'Two-Way Contingency Table (Counts)')
        } else if (is.numeric(df[[var_name]]) && group_is_cat_like) {
          grouped_summary <- df %>%
            group_by(.data[[group_var]]) %>%
            summarise(
              N = sum(!is.na(.data[[var_name]])),
              Mean = round(mean(.data[[var_name]], na.rm = TRUE), 2),
              Median = round(median(.data[[var_name]], na.rm = TRUE), 2),
              SD = round(sd(.data[[var_name]], na.rm = TRUE), 2),
              Min = round(min(.data[[var_name]], na.rm = TRUE), 2),
              Q1 = round(quantile(.data[[var_name]], 0.25, na.rm = TRUE), 2),
              Q3 = round(quantile(.data[[var_name]], 0.75, na.rm = TRUE), 2),
              Max = round(max(.data[[var_name]], na.rm = TRUE), 2)
            )
          datatable(grouped_summary, options = list(dom = 't'), rownames = FALSE, caption = 'Descriptive Statistics (Grouped)')
        } else {
          datatable(data.frame(Message = "This combination is not supported."), options = list(dom = 't'))
        }
      } else {
        if (is_strictly_categorical) {
          summary_table <- df %>%
            filter(!is.na(.data[[var_name]])) %>%
            count(.data[[var_name]], name = "Frequency") %>%
            mutate(Relative_Frequency = scales::percent(Frequency / sum(Frequency), accuracy = 0.1))
          datatable(summary_table, options = list(dom = 't'), rownames = FALSE, caption = 'Frequency Distribution Table')
        } else {
          summary_df <- data.frame(
            Statistic = c("N", "Mean", "Median", "SD", "Min", "Q1", "Q3", "Max"),
            Value = c(
              sum(!is.na(df[[var_name]])), round(mean(df[[var_name]], na.rm = TRUE), 2),
              round(median(df[[var_name]], na.rm = TRUE), 2), round(sd(df[[var_name]], na.rm = TRUE), 2),
              round(min(df[[var_name]], na.rm = TRUE), 2), round(quantile(df[[var_name]], 0.25, na.rm = TRUE), 2),
              round(quantile(df[[var_name]], 0.75, na.rm = TRUE), 2), round(max(df[[var_name]], na.rm = TRUE), 2)
            )
          )
          datatable(summary_df, options = list(dom = 't'), rownames = FALSE, caption = 'Descriptive Statistics')
        }
      }
    }, server = FALSE)
    
    # --- Histogram (with Faceting) ---
    output$histogram_plot <- renderPlot({
      df <- data_r()
      req(df, input$descriptive_variable, input$descriptive_variable %in% names(df))
      var <- input$descriptive_variable
      group_var <- input$group_by_variable
      validate(need(is.numeric(df[[var]]), "Histogram requires a quantitative (numeric) variable."))
      
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
        gg <- gg + facet_wrap(vars(.data[[group_var]]), scales = "free_y") +
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
      gg
    })
    
    # --- Box Plot ---
    output$boxplot_plot <- renderPlot({
      df <- data_r()
      req(df, input$descriptive_variable, input$descriptive_variable %in% names(df))
      var_name <- input$descriptive_variable
      group_var <- input$group_by_variable
      if (is.numeric(df[[var_name]])) {
        if (group_var != "None" && group_var %in% names(df) && (is.character(df[[group_var]]) || is.factor(df[[group_var]]))) {
          ggplot(df, aes(x = as.factor(.data[[group_var]]), y = .data[[var_name]], fill = as.factor(.data[[group_var]]))) +
            geom_boxplot() + labs(title = paste("Boxplot of", var_name, "by", group_var), x = group_var, y = var_name) + theme(legend.position = "none")
        } else {
          ggplot(df, aes(y = .data[[var_name]])) +
            geom_boxplot(fill = "lightgreen") + labs(title = paste("Boxplot of", var_name), y = var_name, x = NULL) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
        }
      } else {
        ggplot() + annotate("text", x = 0, y = 0, label = "Box plot requires a numeric variable.", size = 5) + theme_void()
      }
    })
    
    # --- Density Plot (with Faceting) ---
    output$density_plot <- renderPlot({
      df <- data_r()
      req(df, input$descriptive_variable, input$descriptive_variable %in% names(df))
      var <- input$descriptive_variable
      group_var <- input$group_by_variable
      validate(need(is.numeric(df[[var]]), "Density plot requires a numeric variable."))
      
      if (group_var != "None" && group_var %in% names(df)) {
        ggplot(df, aes(x = .data[[var]])) +
          geom_density(fill = "blue", alpha = 0.4) +
          facet_wrap(vars(.data[[group_var]]), scales = "free_y") +
          labs(title = paste("Density Plot of", var, "by", group_var), x = var, y = "Density")
      } else {
        ggplot(df, aes(x = .data[[var]])) +
          geom_density(fill = "blue", alpha = 0.4) +
          labs(title = paste("Density Plot of", var), x = var, y = "Density")
      }
    })
    
    # --- Pie Chart ---
    output$pie_chart_plot <- renderPlot({
      df <- data_r()
      req(df, input$descriptive_variable, input$descriptive_variable %in% names(df))
      var_name <- input$descriptive_variable
      validate(need(!is.numeric(df[[var_name]]), "Pie chart requires a categorical variable."))
      
      df_summary <- df %>%
        filter(!is.na(.data[[var_name]])) %>%
        count(.data[[var_name]], name = "Count") %>%
        mutate(
          Percentage = Count / sum(Count),
          Label = paste0(round(Percentage * 100, 1), "%")
        )
      ggplot(df_summary, aes(x = "", y = Percentage, fill = as.factor(.data[[var_name]]))) +
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "white", size = 4) +
        theme_void() +
        labs(title = paste("Pie Chart of", var_name), fill = var_name)
    })
    
    # --- Bar Chart ---
    output$barchart_plot <- renderPlot({
      df <- data_r()
      req(df, input$descriptive_variable, input$descriptive_variable %in% names(df))
      var_name <- input$descriptive_variable
      validate(need(is.character(df[[var_name]]) || is.factor(df[[var_name]]), "Bar chart is for categorical (text) variables only."))
      
      df_summary <- df %>%
        filter(!is.na(.data[[var_name]])) %>%
        count(.data[[var_name]], name = "Frequency") %>%
        mutate(Proportion = Frequency / sum(Frequency))
      
      y_axis_var <- if (input$barchart_yaxis_type == "proportion") "Proportion" else "Frequency"
      y_axis_label <- if (input$barchart_yaxis_type == "proportion") "Relative Frequency" else "Count"
      
      gg <- ggplot(df_summary, aes(x = as.factor(.data[[var_name]]), y = .data[[y_axis_var]])) +
        geom_col(fill = "cornflowerblue") +
        labs(title = paste("Bar Chart of", var_name), x = var_name, y = y_axis_label)
      
      if (input$barchart_yaxis_type == "proportion") {
        gg <- gg + scale_y_continuous(labels = scales::percent)
      }
      gg
    })
  })
  # --- END ROBUST REPLACEMENT ---
  
  observeEvent(input$generate_scatter, {
    output$scatter_plot <- renderPlot({
      df <- data_r()
      req(df, input$scatter_x, input$scatter_y, input$scatter_x %in% names(df), input$scatter_y %in% names(df))
      x_var <- input$scatter_x
      y_var <- input$scatter_y
      
      validate(
        need(is.numeric(df[[x_var]]) && is.numeric(df[[y_var]]), "Scatter plots require numeric variables.")
      )
      
      ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
        geom_point(color = "darkblue") +
        labs(title = paste("Scatter Plot of", y_var, "vs", x_var), x = x_var, y = y_var)
    })
  })
  
  observeEvent(input$generate_dot_plot, {
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
    })
  })
  
  output$download_dot_plot <- downloadHandler(
    filename = function() { paste("dot-plot-", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      req(current_dot_plot())
      ggsave(filename = file, plot = current_dot_plot(), width = 8, height = 6, dpi = 300, units = "in")
    }
  )
  
  # --- All Inferential and Regression Logic ---
  
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
    
    n1 <- nrow(group1_data)
    n2 <- nrow(group2_data)
    x1 <- sum(group1_data[[prop_var]] == success_val, na.rm = TRUE)
    x2 <- sum(group2_data[[prop_var]] == success_val, na.rm = TRUE)
    
    if (n1 == 0 || n2 == 0) {
      output$two_prop_test_result <- renderPrint({"One or both groups have no data."})
      return(NULL)
    }
    
    p1 <- x1 / n1
    p2 <- x2 / n2
    se1 <- sqrt(p1 * (1 - p1) / n1)
    se2 <- sqrt(p2 * (1 - p2) / n2)
    
    output$two_prop_test_result <- renderPrint({
      cat("Two-Proportion Test\n\n")
      cat("Group 1:", group1, "| Successes:", x1, "/", n1, "| SE:", round(se1, 4), "\n")
      cat("Group 2:", group2, "| Successes:", x2, "/", n2, "| SE:", round(se2, 4), "\n\n")
      print(prop.test(x = c(x1, x2), n = c(n1, n2), alternative = input$two_prop_alternative))
    })
  })
  
  # --- START: FINAL UPGRADED REPLACEMENT for observeEvent(input$run_ht, ...) ---
  # This version adds the sample size(s) 'n' to the t-test outputs.
  
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
        # --- TWO-SAMPLE T-TEST LOGIC ---
        df_filtered <- df %>% filter(!is.na(.data[[var_name]]), !is.na(.data[[group_var]]))
        df_filtered[[group_var]] <- as.factor(df_filtered[[group_var]])
        
        if (nlevels(df_filtered[[group_var]]) != 2) {
          cat("Error: Grouping variable must have exactly two levels for two-sample t-test.\n")
          return()
        }
        
        # --- NEW: Calculate n1 and n2 ---
        sample_sizes <- table(df_filtered[[group_var]])
        n1 <- sample_sizes[1]
        n2 <- sample_sizes[2]
        # --- END NEW ---
        
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
        
        # --- MODIFIED: Add sample sizes to output ---
        cat("Sample Sizes (n1, n2):", n1, ",", n2, "\n")
        cat("Standard Error of Difference:", round(se_diff, 4), "\n\n")
        # --- END MODIFIED ---
        
        print(test_result)
        
      } else {
        # --- ONE-SAMPLE T-TEST LOGIC ---
        sample_data <- na.omit(df[[var_name]])
        test_result <- t.test(sample_data, mu = mu, alternative = input$ht_alternative)
        
        n <- length(sample_data)
        s <- sd(sample_data)
        se <- s / sqrt(n)
        
        cat("One-Sample t-test\n-----------------\n")
        # --- MODIFIED: Add sample size to output ---
        cat("Sample Size (n):", n, "\n")
        cat("Sample Mean:", round(test_result$estimate, 4), "\n")
        cat("Standard Error:", round(se, 4), "\n\n")
        # --- END MODIFIED ---
        
        print(test_result)
      }
    })
  })
  
  # --- END OF REPLACEMENT ---d
  
# --- START: UPGRADED REPLACEMENT for observeEvent(input$run_paired_ttest, ...) ---
# This version fixes the data usage bug and adds n and SE to the output.

observeEvent(input$run_paired_ttest, {
  df <- data_r()
  req(df, input$paired_var1, input$paired_var2)
  
  var1 <- input$paired_var1
  var2 <- input$paired_var2
  
  # --- Validation (unchanged) ---
  if (var1 == var2) {
    output$paired_ttest_result <- renderPrint({"Error: Please select two different variables."})
    return(NULL)
  }
  
  if (!is.numeric(df[[var1]]) || !is.numeric(df[[var2]])) {
    output$paired_ttest_result <- renderPrint({"Both variables must be numeric."})
    return(NULL)
  }
  
  # --- BUG FIX Part 1: Correctly use df_clean ---
  # Create a clean data frame with only complete pairs
  df_clean <- df %>% select(all_of(c(var1, var2))) %>% na.omit()
  
  if (nrow(df_clean) < 2) {
    output$paired_ttest_result <- renderPrint({"Not enough complete data pairs for a paired t-test."})
    return(NULL)
  }
  
  # --- NEW FEATURE: Calculate n and SE ---
  n_pairs <- nrow(df_clean)
  differences <- df_clean[[var1]] - df_clean[[var2]]
  se_of_diff <- sd(differences) / sqrt(n_pairs)
  # --- END NEW FEATURE ---
  
  # --- BUG FIX Part 2: Use the cleaned data in the test ---
  test <- t.test(df_clean[[var1]], df_clean[[var2]], paired = TRUE, alternative = input$paired_alternative)
  
  output$paired_ttest_result <- renderPrint({
    
    # Capture the default R output
    raw_output <- capture.output(print(test))
    
    # --- NEW FEATURE: Clean up the "data:" line ---
    # Find the line that starts with "data:" and replace it with a user-friendly version
    data_line_index <- grep("data:", raw_output)
    raw_output[data_line_index] <- paste("data: ", var1, "and", var2)
    
    # --- Assemble the final, custom output ---
    cat("Paired t-test\n")
    cat("-----------------\n")
    cat("Number of Pairs (n):", n_pairs, "\n")
    cat("Standard Error of Mean Difference:", round(se_of_diff, 4), "\n\n")
    
    # Print the cleaned-up R output
    cat(paste(raw_output, collapse = "\n"))
  })
})

# --- END OF REPLACEMENT ---
  
  observeEvent(input$run_chi_sq, {
    
    output$chi_sq_table_output <- renderPrint({
      contingency_table <- contingency_table_r()
      req(contingency_table) 
      
      table_type <- input$chisq_table_type
      
      if (table_type == "counts") {
        cat("Two-Way Table (Observed Counts):\n\n")
        print(addmargins(contingency_table))
        
      } else {
        prop_table <- switch(table_type,
                             "row_perc" = prop.table(contingency_table, margin = 1),
                             "col_perc" = prop.table(contingency_table, margin = 2),
                             "total_perc" = prop.table(contingency_table))
        
        title <- switch(table_type,
                        "row_perc" = "Two-Way Table (Row Percentages):\n\n",
                        "col_perc" = "Two-Way Table (Column Percentages):\n\n",
                        "total_perc" = "Two-Way Table (Total Percentages):\n\n")
        
        cat(title)
        print(round(prop_table * 100, 1))
      }
    })
    
    output$chi_sq_test_output <- renderPrint({
      contingency_table <- contingency_table_r()
      req(contingency_table)
      
      if (nrow(contingency_table) < 2 || ncol(contingency_table) < 2) {
        cat("The contingency table must have at least 2 rows and 2 columns to perform the test.\n")
        return()
      }
      
      if (input$use_fisher_exact) {
        cat("Fisher's Exact Test for Count Data:\n\n")
        tryCatch({
          test_result <- fisher.test(contingency_table)
          print(test_result)
        }, error = function(e) {
          cat("Fisher's Exact Test could not be performed. Error:\n", e$message)
        })
        
      } else {
        test_result <- suppressWarnings(chisq.test(contingency_table))
        
        cat("Chi-Squared Test Result:\n\n")
        print(test_result)
        
        cat("\n----------------------------------------\n")
        cat("Expected Counts:\n\n")
        print(round(test_result$expected, 2))
        
        if (any(test_result$expected < 5)) {
          cat("\n----------------------------------------\n")
          cat("WARNING: Chi-squared approximation may be incorrect because some expected counts are less than 5.\n")
          cat("Consider using Fisher's Exact Test by checking the box above.\n")
        }
      }
    })
  })
  
  contingency_table_r <- eventReactive(input$run_chi_sq, {
    df <- data_r()
    req(df, input$chi_x, input$chi_y)
    x_var <- input$chi_x
    y_var <- input$chi_y
    
    is_cat <- function(var) {
      is.character(var) || is.factor(var) || (is.numeric(var) && length(unique(na.omit(var))) < 15)
    }
    
    if (!is_cat(df[[x_var]])) {
      showNotification("Row variable must be categorical or discrete numeric (fewer than 15 unique values).", type = "warning", duration = 8)
      return(NULL)
    }
    if (!is_cat(df[[y_var]])) {
      showNotification("Column variable must be categorical or discrete numeric (fewer than 15 unique values).", type = "warning", duration = 8)
      return(NULL)
    }
    
    df_filtered <- df %>%
      filter(!is.na(.data[[x_var]]) & !is.na(.data[[y_var]]))
    
    if (nrow(df_filtered) == 0) {
      showNotification("No valid data pairs found.", type = "error"); return(NULL)
    }
    
    if ("Freq" %in% names(df_filtered) && is.numeric(df_filtered$Freq)) {
      formula_str <- paste("Freq ~", x_var, "+", y_var)
      xtabs(as.formula(formula_str), data = df_filtered)
    } else {
      table(as.factor(df_filtered[[x_var]]), as.factor(df_filtered[[y_var]]))
    }
  })
  
  observeEvent(input$check_normality, {
    df <- data_r()
    req(df, input$normality_var)
    var_name <- input$normality_var
    
    if (!is.numeric(df[[var_name]])) {
      showNotification("Normality check requires a numeric variable.", type = "warning")
      return(NULL)
    }
    
    output$normality_plot <- renderPlot({
      ggplot(df, aes_string(sample = var_name)) +
        stat_qq() +
        stat_qq_line() +
        labs(title = paste("Q-Q Plot of", var_name),
             x = "Theoretical Quantiles", y = "Sample Quantiles")
    })
    
    output$shapiro_wilk_output <- renderPrint({
      data_for_test <- na.omit(df[[var_name]])
      if (length(data_for_test) < 3 || length(data_for_test) > 5000) {
        cat("Shapiro-Wilk test requires between 3 and 5000 data points.\n")
        if (length(data_for_test) < 30) {
          cat("For n < 30, visual inspection of Q-Q plot is more critical.\n")
        }
        return(NULL)
      }
      cat("Shapiro-Wilk Normality Test:\n")
      print(shapiro.test(data_for_test))
    })
  })
  
  regression_model_r <- eventReactive(input$run_regression, {
    df <- data_r()
    req(df, input$regression_dv, input$regression_iv)
    
    dv <- input$regression_dv
    ivs <- input$regression_iv
    
    df_model <- df 
    
    if (isTRUE(input$log_transform_dv_reg)) {
      if (any(df_model[[dv]] <= 0, na.rm = TRUE)) {
        showNotification(
          "Warning: Dependent variable contains 0 or negative values. Using log(x+1) transformation.",
          type = "warning",
          duration = 8
        )
        df_model[[dv]] <- log1p(df_model[[dv]])
      } else {
        df_model[[dv]] <- log(df_model[[dv]])
      }
    }
    
    if (!is.numeric(df_model[[dv]])) {
      showNotification("Dependent variable must be numeric.", type = "warning")
      return(NULL)
    }
    if (length(ivs) == 0) {
      showNotification("Please select at least one independent variable.", type = "warning")
      return(NULL)
    }
    if (!all(sapply(df_model[ivs], is.numeric))) {
      showNotification("All independent variables must be numeric.", type = "warning")
      return(NULL)
    }
    
    formula_str <- paste(dv, "~", paste(ivs, collapse = " + "))
    model <- lm(as.formula(formula_str), data = df_model)
    
    return(model)
  })
  
  output$regression_summary <- renderPrint({
    model <- regression_model_r()
    req(model)
    
    model_summary <- summary(model)
    r_squared <- model_summary$r.squared
    
    cat("Linear Regression Summary:\n")
    print(model_summary)
    cat("\n")
    cat("R-squared:", round(r_squared, 4), "\n")
    if (length(all.vars(formula(model))) == 2) {
      cat("R (correlation):", round(sqrt(r_squared), 4), "\n")
    }
  })
  
  output$regression_diagnostic_plots <- renderPlot({
    model <- regression_model_r()
    req(model)
    
    par(mfrow = c(2, 2))
    plot(model)
    par(mfrow = c(1, 1))
  })
  
  output$regression_assumption_checks <- renderPrint({
    model <- regression_model_r()
    req(model)
    
    ivs <- all.vars(formula(model))[-1] 
    
    if (length(ivs) > 1) {
      cat("Variance Inflation Factor (VIF):\n")
      cat("---------------------------------\n")
      cat("Rule of thumb: VIF > 5 suggests potential multicollinearity.\n")
      cat("VIF > 10 suggests probable multicollinearity.\n\n")
      
      vif_results <- car::vif(model)
      print(vif_results)
    } else {
      cat("Variance Inflation Factor (VIF) is not applicable for simple linear regression.\n")
      cat("It is a measure of multicollinearity among two or more independent variables.")
    }
  })
  
  observeEvent(input$run_correlation, {
    df <- data_r()
    req(input$correlation_vars)
    if (length(input$correlation_vars) < 2) {
      showNotification("Please select at least two variables for correlation.", type = "warning")
      return(NULL)
    }
    corr_data <- df[, input$correlation_vars, drop = FALSE]
    corr_matrix <- cor(corr_data, use = "pairwise.complete.obs")
    output$correlation_matrix <- renderPrint({
      cat("Correlation Matrix:\n")
      print(round(corr_matrix, 4))
    })
  })
  
  observeEvent(input$run_logistic, {
    df <- data_r()
    req(df, input$logistic_dv, input$logistic_iv)
    
    dv <- input$logistic_dv
    ivs <- input$logistic_iv
    
    df_model <- df %>% select(all_of(c(dv, ivs))) %>% na.omit()
    
    unique_dv <- unique(df_model[[dv]])
    if (length(unique_dv) != 2) {
      showNotification("Dependent variable for logistic regression must have exactly two unique outcomes (e.g., Yes/No, 1/0, True/False).", type = "error", duration = 10)
      return(NULL)
    }
    
    df_model[[dv]] <- as.factor(df_model[[dv]])
    
    output$logistic_summary <- renderPrint({
      formula_str <- paste(dv, "~", paste(ivs, collapse = " + "))
      
      model <- glm(as.formula(formula_str), data = df_model, family = "binomial")
      
      model_summary <- summary(model)
      
      cat("Logistic Regression Summary:\n")
      print(model_summary)
      
      cat("\n-------------------------------------------------\n")
      cat("Odds Ratios (Exponentiated Coefficients):\n")
      cat("-------------------------------------------------\n")
      cat("Rule of thumb:\n")
      cat(" - Odds Ratio > 1: Increases the odds of the outcome.\n")
      cat(" - Odds Ratio < 1: Decreases the odds of the outcome.\n\n")
      
      odds_ratios <- exp(coef(model))
      print(odds_ratios)
    })
  })
  
  observeEvent(input$calculate_basic_probs, {
    calc_type <- input$prob_calc_type
    
    output$calculated_output_title <- renderPrint({ "" })
    output$calculated_output <- renderPrint({ "" })
    
    tryCatch({
      
      if (calc_type == "union") {
        req(input$prob_A_union, input$prob_B_union, input$prob_A_and_B_union)
        P_A <- input$prob_A_union
        P_B <- input$prob_B_union
        P_A_and_B <- input$prob_A_and_B_union
        
        P_A_or_B <- P_A + P_B - P_A_and_B
        
        output$calculated_output_title <- renderPrint({ cat("P(A or B) - Additive Rule") })
        output$calculated_output <- renderPrint({
          cat(paste0("Formula: P(A) + P(B) - P(A and B)\n"))
          cat(paste0("Result: ", round(P_A_or_B, 4), "\n"))
        })
        
      } else if (calc_type == "conditional") {
        req(input$prob_A_cond, input$prob_B_cond)
        P_A_and_B <- input$prob_A_cond
        P_B <- input$prob_B_cond
        
        if (P_B <= 0) {
          stop("P(B) (the given condition) cannot be zero or less.")
        }
        
        P_A_given_B <- P_A_and_B / P_B
        
        output$calculated_output_title <- renderPrint({ cat("P(A | B) - Conditional Probability Rule") })
        output$calculated_output <- renderPrint({
          cat(paste0("Formula: P(A and B) / P(B)\n"))
          cat(paste0("Result: ", round(P_A_given_B, 4), "\n"))
        })
        
      } else if (calc_type == "check_relationship") {
        req(input$prob_A_rel, input$prob_B_rel, input$prob_A_and_B_rel)
        P_A <- input$prob_A_rel
        P_B <- input$prob_B_rel
        P_A_and_B <- input$prob_A_and_B_rel
        
        is_independent <- abs(P_A_and_B - (P_A * P_B)) < 1e-9
        is_mutually_exclusive <- P_A_and_B == 0
        
        output$calculated_output_title <- renderPrint({ cat("Event Relationship Check") })
        output$calculated_output <- renderPrint({
          cat(paste0("Check 1 (Independence): Is P(A and B) equal to P(A) * P(B)?\n"))
          cat(paste0("  - P(A) * P(B) = ", round(P_A * P_B, 4), "\n"))
          cat(paste0("  - P(A and B) = ", round(P_A_and_B, 4), "\n\n"))
          
          if (is_independent) {
            cat("Result: Events A and B are Independent.\n\n")
          } else {
            cat("Result: Events A and B are Dependent.\n\n")
          }
          
          cat("Check 2 (Mutually Exclusive): Is P(A and B) equal to 0?\n")
          if (is_mutually_exclusive) {
            cat("Result: Events A and B are Mutually Exclusive.\n")
          } else {
            cat("Result: Events A and B are Not Mutually Exclusive.\n")
          }
        })
      }
      
    }, error = function(e) { 
      output$calculated_output_title <- renderPrint({ cat("Error in Calculation") })
      output$calculated_output <- renderPrint({
        cat(paste("An error occurred:", e$message))
      })
    })
  })
  
  output$normal_inputs <- renderUI({
    req(input$normal_prob_type)
    prob_type <- input$normal_prob_type
    
    if (prob_type == "inverse") {
      numericInput("normal_p", "Cumulative Probability P(X < x):", value = 0.95, min = 0, max = 1, step = 0.01)
    } else if (prob_type %in% c("less", "greater")) {
      numericInput("normal_x", "X Value:", value = 1.96)
    } else if (prob_type == "between") {
      tagList(
        numericInput("normal_a", "Lower Bound (a):", value = -1.96),
        numericInput("normal_b", "Upper Bound (b):", value = 1.96)
      )
    }
  })
  
  output$prob_required_inputs <- renderUI({
    calc_type <- input$prob_calc_type
    
    if (calc_type == "union") {
      tagList(
        numericInput("prob_A_union", "P(A):", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("prob_B_union", "P(B):", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("prob_A_and_B_union", "P(A and B) (Intersection):", value = 0.25, min = 0, max = 1, step = 0.01)
      )
    } else if (calc_type == "conditional") {
      tagList(
        numericInput("prob_A_cond", "P(A and B) (Intersection):", value = 0.25, min = 0, max = 1, step = 0.01),
        numericInput("prob_B_cond", "P(B) (The given condition):", value = 0.5, min = 0, max = 1, step = 0.01)
      )
    } else if (calc_type == "check_relationship") {
      tagList(
        numericInput("prob_A_rel", "P(A):", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("prob_B_rel", "P(B):", value = 0.5, min = 0, max = 1, step = 0.01),
        numericInput("prob_A_and_B_rel", "P(A and B) (Intersection):", value = 0.25, min = 0, max = 1, step = 0.01)
      )
    }
  })
  
  observeEvent(input$calc_normal, {
    req(input$normal_mean, input$normal_sd, input$normal_prob_type)
    
    if (input$normal_sd <= 0) {
      showNotification("Standard deviation must be positive.", type = "error")
      output$normal_result <- renderPrint({ "Invalid input: Standard deviation must be positive." })
      return()
    }
    
    prob_type <- input$normal_prob_type
    
    result_text <- switch(
      prob_type,
      "less" = {
        req(input$normal_x)
        prob <- pnorm(input$normal_x, mean = input$normal_mean, sd = input$normal_sd)
        paste0("P(X < ", input$normal_x, ") = ", round(prob, 4))
      },
      "greater" = {
        req(input$normal_x)
        prob <- 1 - pnorm(input$normal_x, mean = input$normal_mean, sd = input$normal_sd)
        paste0("P(X > ", input$normal_x, ") = ", round(prob, 4))
      },
      "between" = {
        req(input$normal_a, input$normal_b)
        prob <- pnorm(input$normal_b, mean = input$normal_mean, sd = input$normal_sd) -
          pnorm(input$normal_a, mean = input$normal_mean, sd = input$normal_sd)
        paste0("P(", input$normal_a, " < X < ", input$normal_b, ") = ", round(prob, 4))
      },
      "inverse" = {
        req(input$normal_p)
        x_val <- qnorm(input$normal_p, mean = input$normal_mean, sd = input$normal_sd)
        paste0("The x-value for P(X < x) = ", input$normal_p, " is ", round(x_val, 4))
      }
    )
    
    output$normal_result <- renderPrint({ result_text })
  })
  
  output$normal_plot <- renderPlot({
    req(input$normal_mean, input$normal_sd)
    
    mean_val <- input$normal_mean
    sd_val <- input$normal_sd
    x_vals <- seq(mean_val - 4 * sd_val, mean_val + 4 * sd_val, length.out = 500)
    df <- data.frame(x = x_vals, y = dnorm(x_vals, mean = mean_val, sd = sd_val))
    
    gg <- ggplot(df, aes(x, y)) +
      geom_line(color = "steelblue", linewidth = 1) +
      labs(
        title = paste("Normal Distribution (\u03bc =", mean_val, ", \u03c3 =", sd_val, ")"),
        x = "X", y = "Density"
      )
    
    if (isTRUE(input$show_empirical_rule)) {
      one_sd_lower <- mean_val - sd_val
      one_sd_upper <- mean_val + sd_val
      two_sd_lower <- mean_val - 2 * sd_val
      two_sd_upper <- mean_val + 2 * sd_val
      three_sd_lower <- mean_val - 3 * sd_val
      three_sd_upper <- mean_val + 3 * sd_val
      
      gg <- gg +
        geom_area(data = subset(df, x >= three_sd_lower & x <= three_sd_upper), aes(y = y), fill = "lightblue", alpha = 0.5) +
        geom_area(data = subset(df, x >= two_sd_lower & x <= two_sd_upper), aes(y = y), fill = "cornflowerblue", alpha = 0.5) +
        geom_area(data = subset(df, x >= one_sd_lower & x <= one_sd_upper), aes(y = y), fill = "royalblue", alpha = 0.5) +
        
        annotate("text", x = mean_val, y = dnorm(mean_val, mean_val, sd_val) * 0.5, label = "68%", color = "white", size = 5) +
        annotate("text", x = mean_val - 1.5 * sd_val, y = dnorm(mean_val - 1.5 * sd_val, mean_val, sd_val) * 0.2, label = "95%", size = 5) +
        annotate("text", x = mean_val + 1.5 * sd_val, y = dnorm(mean_val + 1.5 * sd_val, mean_val, sd_val) * 0.2, label = "95%", size = 5) +
        annotate("text", x = mean_val - 2.5 * sd_val, y = dnorm(mean_val - 2.5 * sd_val, mean_val, sd_val) * 0.2, label = "99.7%", size = 5) +
        annotate("text", x = mean_val + 2.5 * sd_val, y = dnorm(mean_val + 2.5 * sd_val, mean_val, sd_val) * 0.2, label = "99.7%", size = 5)
      
    } else {
      prob_type <- input$normal_prob_type
      
      if (prob_type == "less") {
        req(input$normal_x)
        gg <- gg + geom_area(data = subset(df, x <= input$normal_x), aes(y = y), fill = "lightblue", alpha = 0.5)
      } else if (prob_type == "greater") {
        req(input$normal_x)
        gg <- gg + geom_area(data = subset(df, x >= input$normal_x), aes(y = y), fill = "lightblue", alpha = 0.5)
      } else if (prob_type == "between") {
        req(input$normal_a, input$normal_b)
        gg <- gg + geom_area(data = subset(df, x >= input$normal_a & x <= input$normal_b), aes(y = y), fill = "lightblue", alpha = 0.5)
      } else if (prob_type == "inverse") {
        req(input$normal_p)
        x_val <- qnorm(input$normal_p, mean = mean_val, sd = sd_val)
        gg <- gg + geom_vline(xintercept = x_val, color = "red", linetype = "dashed", linewidth = 1) +
          geom_area(data = subset(df, x <= x_val), aes(y = y), fill = "lightblue", alpha = 0.5)
      }
    }
    
    gg
  })
  
  output$binom_summary_stats <- renderPrint({
    req(input$binom_size, input$binom_prob)
    n <- input$binom_size
    p <- input$binom_prob
    
    if (n < 1 || p < 0 || p > 1) {
      cat("Invalid parameters: n must be >= 1 and p must be between 0 and 1.")
      return()
    }
    
    expected_value <- n * p
    variance <- n * p * (1 - p)
    std_dev <- sqrt(variance)
    
    cat(
      "Expected Value (E[X] = np): ", round(expected_value, 4), "\n",
      "Variance (\u03c3\u00b2 = np(1-p)): ", round(variance, 4), "\n",
      "Standard Deviation (\u03c3): ", round(std_dev, 4), sep = ""
    )
  })
  
  observeEvent(input$calc_binom_prob, {
    req(input$binom_size, input$binom_prob, input$binom_k)
    if (input$binom_size < 1 || input$binom_prob < 0 || input$binom_prob > 1 || input$binom_k < 0) {
      showNotification("Invalid Binomial parameters.", type = "error")
      output$binom_prob_output <- renderPrint({ cat("Invalid input: Check n, p, and k values.\n") })
      return()
    }
    if (input$binom_k > input$binom_size) {
      showNotification("k cannot be greater than n for Binomial distribution.", type = "error")
      output$binom_prob_output <- renderPrint({ cat("Invalid input: k cannot be greater than n.\n") })
      return()
    }
    
    prob_val <- switch(input$binom_type,
                       "P(X = x)" = dbinom(input$binom_k, size = input$binom_size, prob = input$binom_prob),
                       "P(X <= x)" = pbinom(input$binom_k, size = input$binom_size, prob = input$binom_prob),
                       "P(X >= x)" = 1 - pbinom(input$binom_k - 1, size = input$binom_size, prob = input$binom_prob)
    )
    
    output$binom_prob_output <- renderPrint({
      cat(paste0(input$binom_type, " = ", round(prob_val, 4), "\n"))
    })
  })
  
  observeEvent(input$solve_binom_k, {
    req(input$binom_size, input$binom_prob, input$binom_p_for_k)
    if (input$binom_size < 1 || input$binom_prob < 0 || input$binom_prob > 1 || input$binom_p_for_k < 0 || input$binom_p_for_k > 1) {
      showNotification("Invalid Binomial parameters.", type = "error")
      output$solve_binom_k_output <- renderPrint({ cat("Invalid input: Check n, p, and probability values.\n") })
      return()
    }
    x_val <- qbinom(p = input$binom_p_for_k, size = input$binom_size, prob = input$binom_prob)
    actual_prob <- pbinom(x_val, size = input$binom_size, prob = input$binom_prob)
    output$solve_binom_k_output <- renderPrint({
      cat(paste0("To achieve a cumulative probability of at least ", input$binom_p_for_k, ",\n",
                 "you need ", x_val, " successes (x).\n\n",
                 "The actual probability at this point is P(X <= ", x_val, ") = ", round(actual_prob, 4)))
    })
  })
  
  output$binom_pmf_plot <- renderPlot({
    req(input$binom_size, input$binom_prob)
    if (input$binom_size < 1 || input$binom_prob < 0 || input$binom_prob > 1) return(NULL)
    x_vals <- 0:input$binom_size
    y_vals <- dbinom(x_vals, size = input$binom_size, prob = input$binom_prob)
    df_plot <- data.frame(k = x_vals, probability = y_vals)
    ggplot(df_plot, aes(x = k, y = probability)) +
      geom_bar(stat = "identity", fill = "seagreen") +
      labs(title = paste("Binomial Distribution (n=", input$binom_size, ", p=", input$binom_prob, ")"),
           x = "Number of Successes (x)", y = "Probability") +
      scale_x_continuous(breaks = x_vals)
  })
  
  output$pois_summary_stats <- renderPrint({
    req(input$pois_lambda)
    lambda <- input$pois_lambda
    
    if (lambda <= 0) {
      cat("Invalid parameter: Lambda (\u03bb) must be positive.")
      return()
    }
    
    expected_value <- lambda
    variance <- lambda
    std_dev <- sqrt(variance)
    
    cat(
      "Expected Value (E[X] = \u03bb): ", round(expected_value, 4), "\n",
      "Variance (\u03c3\u00b2 = \u03bb): ", round(variance, 4), "\n",
      "Standard Deviation (\u03c3): ", round(std_dev, 4), sep = ""
    )
  })
  
  observeEvent(input$calc_pois_prob, {
    req(input$pois_lambda, input$pois_k)
    if (input$pois_lambda <= 0 || input$pois_k < 0) {
      showNotification("Invalid Poisson parameters. Lambda must be positive, k non-negative.", type = "error")
      return()
    }
    
    prob_val <- switch(input$pois_type,
                       "P(X = k)" = dpois(input$pois_k, lambda = input$pois_lambda),
                       "P(X <= k)" = ppois(input$pois_k, lambda = input$pois_lambda),
                       "P(X >= k)" = 1 - ppois(input$pois_k - 1, lambda = input$pois_lambda)
    )
    output$pois_prob_output <- renderPrint({
      cat(paste0(input$pois_type, " = ", round(prob_val, 4), "\n"))
    })
  })
  
  output$pois_pmf_plot <- renderPlot({
    req(input$pois_lambda)
    if (input$pois_lambda <= 0) return(NULL)
    
    max_k <- qpois(0.999, lambda = input$pois_lambda) + 5
    if (max_k < 10) max_k <- 10
    x_vals <- 0:max_k
    y_vals <- dpois(x_vals, lambda = input$pois_lambda)
    df_plot <- data.frame(k = x_vals, probability = y_vals)
    
    ggplot(df_plot, aes(x = k, y = probability)) +
      geom_bar(stat = "identity", fill = "darkorange") +
      labs(title = paste("Poisson Distribution (\u03bb =", input$pois_lambda, ")"),
           x = "Number of Events (k)", y = "Probability") +
      scale_x_continuous(breaks = x_vals[x_vals %% 1 == 0])
  })
  
  observeEvent(input$run_mw_test, {
    df <- data_r()
    req(df, input$mw_variable, input$mw_group)
    
    dv <- input$mw_variable
    iv <- input$mw_group
    
    df_clean <- df %>% filter(!is.na(.data[[dv]]) & !is.na(.data[[iv]]))
    df_clean[[iv]] <- as.factor(df_clean[[iv]])
    
    if (nlevels(df_clean[[iv]]) != 2) {
      showNotification("Grouping variable for Mann-Whitney U test must have exactly two levels.", type = "error")
      return(NULL)
    }
    
    output$mw_test_output <- renderPrint({
      formula_str <- paste(dv, "~", iv)
      test_result <- wilcox.test(as.formula(formula_str), data = df_clean)
      
      cat("Mann-Whitney U Test (Wilcoxon rank sum test)\n")
      cat("--------------------------------------------\n")
      print(test_result)
    })
  })
  
  observeEvent(input$run_kw_test, {
    df <- data_r()
    req(df, input$kw_variable, input$kw_group)
    
    dv <- input$kw_variable
    iv <- input$kw_group
    
    df_clean <- df %>% filter(!is.na(.data[[dv]]) & !is.na(.data[[iv]]))
    if(nrow(df_clean) < 1) {
      showNotification("No valid data for the selected variables.", type = "error")
      return(NULL)
    }
    
    output$kw_test_output <- renderPrint({
      formula_str <- paste(dv, "~", iv)
      test_result <- kruskal.test(as.formula(formula_str), data = df_clean)
      
      cat("Kruskal-Wallis rank sum test\n")
      cat("------------------------------\n")
      print(test_result)
    })
  })
  
}