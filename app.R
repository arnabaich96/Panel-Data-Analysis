library(shiny)
library(shinyjs)
library(plotly)
library(dplyr)
library(gt)
library(tidyverse)
library(mice)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    useShinyjs(),  # Enable shinyjs

    # Include CSS directly in the app
    tags$head(
        tags$style(HTML("
            body {
                font-family: 'Arial', sans-serif;
                background-color: #e8f5e9;
                color: #1b5e20;
                padding: 20px;
            }
            .container-fluid {
                padding-left: 50px;
                padding-right: 50px;
            }
            h1, h3 {
                font-size: 22px;
                text-align: center;
                color: #1b5e20;
                margin-bottom: 15px;
                font-weight: bold;
            }
            .form-group {
                margin-bottom: 10px;
            }
            .well {
                background-color: #ffffff;
                padding: 15px;
                border-radius: 10px;
                box-shadow: 0 0 5px rgba(0, 0, 0, 0.1);
                margin-bottom: 10px;
            }
            .col-centered {
                margin: 0 auto;
                float: none;
            }
            .btn-primary {
                background-color: #388e3c;
                border-color: #388e3c;
                font-size: 16px;
            }
            .btn-primary:hover {
                background-color: #2e7d32;
                border-color: #2e7d32;
            }
        "))
    ),

    # Title Panel
    titlePanel("Biomea Trial 111 and 112 Data"),

    # Imputation Input Parameters
    fluidRow(
        column(12,
               div(class = "well",
                   h3("Imputation Input Parameters"),
                   fluidRow(
                       column(4, numericInput("mValue", "Number of Imputations (m):", value = 5, min = 1)),
                       column(4, selectInput("method", "Imputation Method:",
                                             choices = c("pmm", "mean", "norm", "norm.boot", "logreg"),
                                             selected = "pmm")),
                       column(4, numericInput("seedValue", "Random Seed:", value = 123))
                   ),
                   fluidRow(
                       column(12, actionButton("runBtn", "Run", class = "btn-primary", style = "display: block; margin: 0 auto;"))
                   )
               )
        )
    ),

    # Main UI Elements
    fluidRow(
        column(4,
               div(class = "well",
                   h3("Options"),
                   radioButtons("Measurement", "Measurement:", choices = c("Glucose", "C-peptide", "Index"), selected = "C-peptide", inline = TRUE),
                   radioButtons("Treatment", "Visualization:", choices = c("Pooled", "Individual"), selected = "Pooled", inline = TRUE),
                   conditionalPanel(
                       condition = "input.Measurement != 'Index'",
                       radioButtons("plotvariable", "Interaction Plot Variable:", choices = c("Visit", "Time"), selected = "Visit", inline = TRUE)
                   )
               )
        ),
        column(4,
               div(class = "well",
                   h3("Additional Settings"),
                   checkboxInput("Summary", "Summary", value = FALSE),
                   checkboxInput("showAUC", "AUC", value = FALSE),
                   checkboxInput("CI", "Confidence Interval", value = FALSE),
                   checkboxInput("showPlot", "Show Plot", value = TRUE),
                   checkboxInput("showTable", "Show Table", value = TRUE)
               )
        ),
        column(4,
               div(class = "well",
                   h3("Summary Options"),
                   conditionalPanel(
                       condition = "input.Summary == true && input.Treatment != 'Individual'",
                       selectInput("SummaryType", "Summary:", choices = c("mean", "sd", "median", "min", "max", "IQR"), selected = "mean")
                   ),
                   conditionalPanel(
                       condition = "input.showAUC == true && input.Treatment != 'Individual' && input.Measurement != 'Index'",
                       selectInput("AUC_SummaryType", "AUC Summary:", choices = c("mean", "sd", "median", "min", "max", "IQR"), selected = "mean")
                   ),
                   conditionalPanel(
                       condition = "input.Treatment == 'Pooled' && input.CI == true",
                       sliderInput("CI_level", "Confidence Level", min = 0.8, max = 1, value = 0.95, step = 0.025)
                   )
               )
        )
    ),
    fluidRow(
      column(6,
             div(class = "well",
                 h3("Raw Data"),
                 conditionalPanel(
                   condition = "input.showPlot == true && input.Measurement != 'Index'",
                   plotlyOutput("InteractionPlot_raw", height = "400px")  # Adjust the height for better fit
                 ),
                 conditionalPanel(
                   condition = "input.showTable == true && input.Measurement != 'Index'",
                   uiOutput("Summary_raw")
                 ),
                 conditionalPanel(
                   condition = "input.showAUC == true && input.showPlot == true && input.Measurement != 'Index'",
                   plotlyOutput("AUCPlot_raw", height = "400px")  # Adjust the height for better fit
                 ),
                 conditionalPanel(
                   condition = "input.showAUC == true && input.showTable == true && input.Measurement != 'Index'",
                   uiOutput("AUCSummary_raw")
                 ),
                 conditionalPanel(
                   condition = "input.showPlot == true && input.Measurement == 'Index'",
                   plotlyOutput("IndexPlot_raw", height = "400px")  # Adjust the height for better fit
                 ),
                 conditionalPanel(
                   condition = "input.showTable == true && input.Measurement == 'Index'",
                   uiOutput("IndexSummary_raw")
                 )
             )
      ),
      column(6,
             div(class = "well",
                 h3("Imputed Data"),
                 conditionalPanel(
                   condition = "input.showPlot == true && input.Measurement != 'Index'",
                   plotlyOutput("InteractionPlot_imputed", height = "400px")  # Adjust the height for better fit
                 ),
                 conditionalPanel(
                   condition = "input.showTable == true && input.Measurement != 'Index'",
                   uiOutput("Summary_imputed")
                 ),
                 conditionalPanel(
                   condition = "input.showAUC == true && input.showPlot == true && input.Measurement != 'Index'",
                   plotlyOutput("AUCPlot_imputed", height = "400px")  # Adjust the height for better fit
                 ),
                 conditionalPanel(
                   condition = "input.showAUC == true && input.showTable == true && input.Measurement != 'Index'",
                   uiOutput("AUCSummary_imputed")
                 ),
                 conditionalPanel(
                   condition = "input.showPlot == true && input.Measurement == 'Index'",
                   plotlyOutput("IndexPlot_imputed", height = "400px")  # Adjust the height for better fit
                 ),
                 conditionalPanel(
                   condition = "input.showTable == true && input.Measurement == 'Index'",
                   uiOutput("IndexSummary_imputed")
                 )
             )
      )
    )


)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {

  # Source the data_clean.R script to generate data_cpep and data_gluc
  source("data_clean.R")

  # Ensure the datasets data_cpep and data_gluc are loaded and accessible
  req(exists("data_cpep"), exists("data_gluc"))

  # Cpeptide data - Create all combinations to ensure full data coverage
  all_combinations_cpep <- expand.grid(
    visit = unique(data_cpep$visit),
    time = unique(data_cpep$time),
    subject = unique(data_cpep$subject)
  )
  data_cpep1 <- left_join(all_combinations_cpep, data_cpep, by = c("visit", "time", "subject"))

  # Glucose data - Create all combinations to ensure full data coverage
  all_combinations_gluc <- expand.grid(
    visit = unique(data_gluc$visit),
    time = unique(data_gluc$time),
    subject = unique(data_gluc$subject)
  )
  data_gluc1 <- left_join(all_combinations_gluc, data_gluc, by = c("visit", "time", "subject"))

  # Perform multiple imputation with user-customizable parameters
  imputed_data <- reactive({
    req(input$runBtn)  # Ensure the run button is clicked

    method <- input$method
    m_value <- input$mValue
    seed_value <- input$seedValue

    data_cpep_mice <- complete(mice(data_cpep1, m = m_value, method = method, seed = seed_value))
    data_gluc_mice <- complete(mice(data_gluc1, m = m_value, method = method, seed = seed_value))

    list(cpep = data_cpep_mice, gluc = data_gluc_mice)
  })

  # Reactive function to select data based on measurement
  data <- reactive({
    if (input$Measurement == "Glucose") {
      list(raw = data_gluc1, imputed = imputed_data()$gluc)
    } else if (input$Measurement == "C-peptide") {
      list(raw = data_cpep1, imputed = imputed_data()$cpep)
    } else if (input$Measurement == "Index") {
      list(raw = list(cpep = data_cpep1, gluc = data_gluc1),
           imputed = list(cpep = imputed_data()$cpep, gluc = imputed_data()$gluc))
    } else {
      list(raw = NULL, imputed = NULL)
    }
  })

  # UI for selecting individual IDs
  output$individualSelect <- renderUI({
    if (input$Measurement == "Index") {
      # Use the combined Index dataset
      selectInput("selectedID", "Select ID:",
                  choices = unique(data()$raw$cpep$subject),
                  selected = unique(data()$raw$cpep$subject)[1])
    } else if (input$Measurement %in% c("Glucose", "C-peptide")) {
      # Use the dataset corresponding to Glucose or C-peptide
      selectInput("selectedID", "Select ID:",
                  choices = unique(data()$raw$subject),
                  selected = unique(data()$raw$subject)[1])
    } else {
      # Default behavior in case of unexpected input$Measurement value
      selectInput("selectedID", "Select ID:",
                  choices = NULL,
                  selected = NULL)
    }
  })

  # Hide Summary checkbox if Individual is selected
  observe({
    if (input$Treatment == "Individual") {
      updateCheckboxInput(session, "Summary", value = FALSE)
      shinyjs::hide("Summary")
    } else {
      shinyjs::show("Summary")
    }
  })

  # Interaction Plot for Raw Data (Left Column) using plotly
  output$InteractionPlot_raw <- renderPlotly({
    req(input$runBtn)
    plot_data <- data()$raw

    # Ensure that time and visit are factors with levels set to chronological order
    plot_data$time <- factor(plot_data$time, levels = c("0hr", "0.5hr", "1hr", "1.5hr", "2hr"))
    plot_data$visit <- factor(plot_data$visit, levels = c("0", "2", "4", "8", "12", "16", "20", "26"))

    if (input$Treatment == "Pooled") {
      summary_data <- plot_data %>%
        group_by(visit, time) %>%
        summarise(
          Measurement_mean = round(match.fun(input$SummaryType)(result, na.rm = TRUE), 3),
          Measurement_sd = sd(result, na.rm = TRUE),
          Measurement_lower = Measurement_mean - qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * Measurement_sd / sqrt(n()),
          Measurement_upper = Measurement_mean + qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * Measurement_sd / sqrt(n())
        ) %>%
        ungroup()

      plot <- switch(input$plotvariable,
                     "Visit" = {
                       plot_ly(summary_data, x = ~visit, y = ~Measurement_mean, color = ~time, type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Visit"),
                           yaxis = list(title = paste(input$Measurement)),
                           legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
                           title = paste("Summary:", input$SummaryType)
                         )
                     },
                     "Time" = {
                       plot_ly(summary_data, x = ~time, y = ~Measurement_mean, color = ~visit, type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Time"),
                           yaxis = list(title = paste(input$Measurement)),
                           legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
                           title = paste("Summary:", input$SummaryType)
                         )
                     })

      if (input$CI) {
        plot <- plot %>%
          add_ribbons(
            ymin = ~Measurement_lower, ymax = ~Measurement_upper,
            line = list(color = 'transparent'),
            fillcolor = ~paste0("rgba(", col2rgb("red")[1], ",", col2rgb("red")[2], ",", col2rgb("red")[3], ",0.2)"),
            hoverinfo = "none",
            showlegend = FALSE
          )
      }

    } else if (input$Treatment == "Individual") {
      plot_data <- plot_data %>% filter(subject == input$selectedID)

      plot <- switch(input$plotvariable,
                     "Visit" = {
                       plot_ly(plot_data, x = ~visit, y = ~result, color = ~time, type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Visit"),
                           yaxis = list(title = paste(input$Measurement)),
                           legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
                           title = paste("Subject ID:", input$selectedID)
                         )
                     },
                     "Time" = {
                       plot_ly(plot_data, x = ~time, y = ~result, color = ~visit, type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Time"),
                           yaxis = list(title = paste(input$Measurement)),
                           legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
                           title = paste("Subject ID:", input$selectedID)
                         )
                     })
    }

    plot
  })

  # Interaction Plot for Imputed Data (Right Column) using plotly
  output$InteractionPlot_imputed <- renderPlotly({
    req(input$runBtn)
    plot_data <- data()$imputed

    # Ensure that time and visit are factors with levels set to chronological order
    plot_data$time <- factor(plot_data$time, levels = c("0hr", "0.5hr", "1hr", "1.5hr", "2hr"))
    plot_data$visit <- factor(plot_data$visit, levels = c("0", "2", "4", "8", "12", "16", "20", "26"))

    if (input$Treatment == "Pooled") {
      summary_data <- plot_data %>%
        group_by(visit, time) %>%
        summarise(
          Measurement_mean = round(match.fun(input$SummaryType)(result, na.rm = TRUE), 3),
          Measurement_sd = sd(result, na.rm = TRUE),
          Measurement_lower = Measurement_mean - qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * Measurement_sd / sqrt(n()),
          Measurement_upper = Measurement_mean + qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * Measurement_sd / sqrt(n())
        ) %>%
        ungroup()

      plot <- switch(input$plotvariable,
                     "Visit" = {
                       plot_ly(summary_data, x = ~visit, y = ~Measurement_mean, color = ~time, type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Visit"),
                           yaxis = list(title = paste(input$Measurement)),
                           legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
                           title = paste("Summary:", input$SummaryType)
                         )
                     },
                     "Time" = {
                       plot_ly(summary_data, x = ~time, y = ~Measurement_mean, color = ~visit, type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Time"),
                           yaxis = list(title = paste(input$Measurement)),
                           legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
                           title = paste("Summary:", input$SummaryType)
                         )
                     })

      if (input$CI) {
        plot <- plot %>%
          add_ribbons(
            ymin = ~Measurement_lower, ymax = ~Measurement_upper,
            line = list(color = 'transparent'),
            fillcolor = ~paste0("rgba(", col2rgb("red")[1], ",", col2rgb("red")[2], ",", col2rgb("red")[3], ",0.2)"),
            hoverinfo = "none",
            showlegend = FALSE
          )
      }

    } else if (input$Treatment == "Individual") {
      plot_data <- plot_data %>% filter(subject == input$selectedID)

      plot <- switch(input$plotvariable,
                     "Visit" = {
                       plot_ly(plot_data, x = ~visit, y = ~result, color = ~time, type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Visit"),
                           yaxis = list(title = paste(input$Measurement)),
                           legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
                           title = paste("Subject ID:", input$selectedID)
                         )
                     },
                     "Time" = {
                       plot_ly(plot_data, x = ~time, y = ~result, color = ~visit, type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Time"),
                           yaxis = list(title = paste(input$Measurement)),
                           legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
                           title = paste("Subject ID:", input$selectedID)
                         )
                     })
    }

    plot
  })

  # Summary statistics for Raw Data (Left Column) using gt
  output$Summary_raw <- renderUI({
    req(input$runBtn)

    summary_data <- if (input$Treatment == "Pooled") {
      data()$raw %>%
        group_by(visit, time) %>%
        summarise(result = round(match.fun(input$SummaryType)(result, na.rm = TRUE), 3)) %>%
        pivot_wider(names_from = visit, values_from = result)
    } else {
      data()$raw %>%
        filter(subject == input$selectedID) %>%
        group_by(visit, time) %>%
        summarise(result = round(result, 3)) %>%
        pivot_wider(names_from = visit, values_from = result)  # Dynamic orientation
    }

    title_text <- if (input$Treatment == "Pooled") {
      paste("Summary:", input$SummaryType)
    } else {
      paste("Subject ID:", input$selectedID)
    }

    gt_table <- gt(summary_data) %>%
      tab_header(title = title_text) %>%
      fmt_number(columns = everything(), decimals = 2) %>%
      cols_align(align = "center", columns = everything()) %>%
      tab_style(
        style = list(
          cell_fill(color = "lightgreen"),
          cell_text(weight = "bold", color = "black")
        ),
        locations = cells_column_labels(everything())
      ) %>%
      tab_options(table.width = pct(100))

    gt_output <- gt_table %>%
      as_raw_html()

    HTML(gt_output)
  })

  # Summary statistics for Imputed Data (Right Column) using gt
  output$Summary_imputed <- renderUI({
    req(input$runBtn)

    summary_data <- if (input$Treatment == "Pooled") {
      data()$imputed %>%
        group_by(visit, time) %>%
        summarise(result = round(match.fun(input$SummaryType)(result, na.rm = TRUE), 3)) %>%
        pivot_wider(names_from = visit, values_from = result)
    } else {
      data()$imputed %>%
        filter(subject == input$selectedID) %>%
        group_by(visit, time) %>%
        summarise(result = round(result, 3)) %>%
        pivot_wider(names_from = visit, values_from = result)  # Dynamic orientation
    }

    title_text <- if (input$Treatment == "Pooled") {
      paste("Summary:", input$SummaryType)
    } else {
      paste("Subject ID:", input$selectedID)
    }

    gt_table <- gt(summary_data) %>%
      tab_header(title = title_text) %>%
      fmt_number(columns = everything(), decimals = 2) %>%
      cols_align(align = "center", columns = everything()) %>%
      tab_style(
        style = list(
          cell_fill(color = "lightgreen"),
          cell_text(weight = "bold", color = "black")
        ),
        locations = cells_column_labels(everything())
      ) %>%
      tab_options(table.width = pct(100))

    gt_output <- gt_table %>%
      as_raw_html()

    HTML(gt_output)
  })


  # AUC Plot for raw data (Left Column) using plotly with conditional CI
  output$AUCPlot_raw <- renderPlotly({
    req(input$runBtn)
    data <- data()$raw
    data$time <- sapply(data$time, convert_to_minutes)

    auc_trapezoidal <- data %>%
      group_by(subject, visit) %>%
      summarise(auc = trapezoidal_auc(as.numeric(time), result)) %>%
      ungroup()
    auc_trapezoidal$visit <- factor(auc_trapezoidal$visit, levels = c("0", "2", "4", "8", "12", "16", "20", "26"))

    if (input$Treatment == "Pooled") {
      summary_data <- auc_trapezoidal %>%
        group_by(visit) %>%
        summarise(
          auc_mean = match.fun(input$AUC_SummaryType)(auc, na.rm = TRUE),
          auc_sd = sd(auc, na.rm = TRUE),
          auc_lower = auc_mean - qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * auc_sd / sqrt(n()),
          auc_upper = auc_mean + qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * auc_sd / sqrt(n())
        )
      plot <- plot_ly(summary_data, x = ~visit, y = ~auc_mean, name = 'AUC Mean', type = 'scatter', mode = 'lines+markers') %>%
        layout(xaxis = list(title = "Visit"),
               yaxis = list(title = "AUC"),
               showlegend = FALSE,
               title = paste("AUC Summary:", input$AUC_SummaryType)
        )

      if (input$CI) {
        plot <- plot %>%
          add_ribbons(ymin = ~auc_lower, ymax = ~auc_upper, name = 'CI',
                      line = list(color = 'rgba(7, 164, 181, 0.05)'),
                      fillcolor = 'rgba(7, 164, 181, 0.2)')
      }

    } else if (input$Treatment == "Individual") {
      auc_ID <- auc_trapezoidal %>% filter(subject == input$selectedID)
      plot <- plot_ly(auc_ID, x = ~visit, y = ~auc, type = 'scatter', mode = 'lines+markers',
                      name = 'AUC') %>%
        layout(xaxis = list(title = "Visit"),
               yaxis = list(title = "AUC"),
               showlegend = FALSE,
               title = paste("AUC of Subject ID:", input$selectedID)
        )
    }
    plot
  })

  # AUC plot for Imputed data (Right Column) using plotly with conditional CI
  output$AUCPlot_imputed <- renderPlotly({
    req(input$runBtn)
    data <- data()$imputed
    data$time <- sapply(data$time, convert_to_minutes)

    auc_trapezoidal <- data %>%
      filter(!is.na(result)) %>%   # Ensure NA values are excluded
      group_by(subject, visit) %>%
      summarise(auc = trapezoidal_auc(as.numeric(time), result)) %>%
      ungroup()
    auc_trapezoidal$visit <- factor(auc_trapezoidal$visit, levels = c("0", "2", "4", "8", "12", "16", "20", "26"))
    if (input$Treatment == "Pooled") {
      summary_data <- auc_trapezoidal %>%
        group_by(visit) %>%
        summarise(
          auc_mean = match.fun(input$AUC_SummaryType)(auc, na.rm = TRUE),
          auc_sd = sd(auc, na.rm = TRUE),
          auc_lower = auc_mean - qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * auc_sd / sqrt(n()),
          auc_upper = auc_mean + qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * auc_sd / sqrt(n())
        )
      summary_data$visit <- factor(summary_data$visit, levels = c("0", "2", "4", "8", "12", "16", "20", "26"))
      plot <- plot_ly(summary_data, x = ~visit, y = ~auc_mean, name = 'AUC Mean', type = 'scatter', mode = 'lines+markers') %>%
        layout(xaxis = list(title = "Visit"),
               yaxis = list(title = "AUC"),
               showlegend = FALSE,
               title = paste("AUC Summary:", input$AUC_SummaryType, "of Imputed Data")
        )

      if (input$CI) {
        plot <- plot %>%
          add_ribbons(ymin = ~auc_lower, ymax = ~auc_upper, name = 'CI',
                      line = list(color = 'rgba(7, 164, 181, 0.05)'),
                      fillcolor = 'rgba(7, 164, 181, 0.2)')
      }

    } else if (input$Treatment == "Individual") {
      auc_ID <- auc_trapezoidal %>% filter(subject == input$selectedID)
      plot <- plot_ly(auc_ID, x = ~visit, y = ~auc, type = 'scatter', mode = 'lines+markers',
                      name = 'AUC') %>%
        layout(xaxis = list(title = "Visit"),
               yaxis = list(title = "AUC"),
               showlegend = FALSE,
               title = paste("AUC of Subject ID:", input$selectedID)
        )
    }
    plot
  })

  # Interaction Plot for Raw Data Index using plotly
  output$IndexPlot_raw <- renderPlotly({
    req(input$runBtn)

    plot_data <- calculate_index(data()$raw$cpep, data()$raw$gluc)

    if (input$Treatment == "Pooled") {
      summary_data <- plot_data %>%
        group_by(visit) %>%
        summarise(
          index_mean = round(match.fun(input$SummaryType)(index, na.rm = TRUE), 3),
          index_sd = sd(index, na.rm = TRUE),
          index_lower = index_mean - qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * index_sd / sqrt(n()),
          index_upper = index_mean + qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * index_sd / sqrt(n())
        ) %>%
        ungroup()

      plot <- plot_ly(summary_data, x = ~visit, y = ~index_mean, type = 'scatter', mode = 'lines+markers') %>%
        layout(
          xaxis = list(title = "Visit"),
          yaxis = list(title = "Index"),
          title = paste("Index Summary:", input$SummaryType)
        )
      if (input$CI) {
        plot <- plot %>%
          add_ribbons(
            ymin = ~index_lower, ymax = ~index_upper,
            line = list(color = 'transparent'),
            fillcolor = ~paste0("rgba(", col2rgb("red")[1], ",", col2rgb("red")[2], ",", col2rgb("red")[3], ",0.2)"),
            hoverinfo = "none",
            showlegend = FALSE
          )
      }
    } else if (input$Treatment == "Individual") {
      plot_data <- plot_data %>% filter(subject == input$selectedID)

      plot <- plot_ly(plot_data %>% filter(!is.na(index)), x = ~visit, y = ~index, type = 'scatter', mode = 'lines+markers') %>%
        layout(
          xaxis = list(title = "Visit"),
          yaxis = list(title = "Index"),
          title = paste("Subject ID:", input$selectedID)
        )
    }
    plot
  })

  # Interaction Plot for Imputed Data Index using plotly
  output$IndexPlot_imputed <- renderPlotly({
    req(input$runBtn)

    plot_data <- calculate_index(data()$imputed$cpep, data()$imputed$gluc)

    if (input$Treatment == "Pooled") {
      summary_data <- plot_data %>%
        group_by(visit) %>%
        summarise(
          index_mean = round(match.fun(input$SummaryType)(index, na.rm = TRUE), 3),
          index_sd = sd(index, na.rm = TRUE),
          index_lower = index_mean - qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * index_sd / sqrt(n()),
          index_upper = index_mean + qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * index_sd / sqrt(n())
        ) %>%
        ungroup()

      plot <- plot_ly(summary_data, x = ~visit, y = ~index_mean, type = 'scatter', mode = 'lines+markers') %>%
        layout(
          xaxis = list(title = "Visit"),
          yaxis = list(title = "Index"),
          title = paste("Index Summary :", input$SummaryType)
        )

      if (input$CI) {
        plot <- plot %>%
          add_ribbons(
            ymin = ~index_lower, ymax = ~index_upper,
            line = list(color = 'transparent'),
            fillcolor = ~paste0("rgba(", col2rgb("red")[1], ",", col2rgb("red")[2], ",", col2rgb("red")[3], ",0.2)"),
            hoverinfo = "none",
            showlegend = FALSE
          )
      }
    } else if (input$Treatment == "Individual") {
      plot_data <- plot_data %>% filter(subject == input$selectedID)
      plot <- plot_ly(plot_data %>% filter(!is.na(index)), x = ~visit, y = ~index, type = 'scatter', mode = 'lines+markers') %>%
        layout(
          xaxis = list(title = "Visit"),
          yaxis = list(title = "Index"),
          title = paste("Subject ID:", input$selectedID)
        )
    }

    plot
  })

  # Summary for Raw Data Index using gt
  output$IndexSummary_raw <- renderUI({
    req(input$runBtn)

    index_data <- calculate_index(data()$raw$cpep, data()$raw$gluc)

    summary_data <- if (input$Treatment == "Pooled") {
      index_data %>%
        group_by(visit) %>%
        summarise(index_summary = round(match.fun(input$SummaryType)(index, na.rm = TRUE), 3)) %>%
        pivot_wider(names_from = visit, values_from = index_summary)
    } else {
      index_data %>%
        filter(subject == input$selectedID) %>%
        group_by(visit) %>%
        summarise(index_summary = round(index, 3)) %>%
        pivot_wider(names_from = visit, values_from = index_summary)
    }

    title_text <- if (input$Treatment == "Pooled") {
      paste("Index Summary:", input$SummaryType)
    } else {
      paste("Subject ID:", input$selectedID)
    }

    gt_table <- gt(summary_data) %>%
      tab_header(title = title_text) %>%
      fmt_number(columns = everything(), decimals = 2) %>%
      cols_align(align = "center", columns = everything()) %>%
      tab_style(
        style = list(
          cell_fill(color = "lightgreen"),
          cell_text(weight = "bold", color = "black")
        ),
        locations = cells_column_labels(everything())
      ) %>%
      tab_options(table.width = pct(100))

    gt_output <- gt_table %>%
      as_raw_html()

    HTML(gt_output)
  })

  # Summary for Imputed Data Index using gt
  output$IndexSummary_imputed <- renderUI({
    req(input$runBtn)

    index_data <- calculate_index(data()$imputed$cpep, data()$imputed$gluc)

    summary_data <- if (input$Treatment == "Pooled") {
      index_data %>%
        group_by(visit) %>%
        summarise(index_summary = round(match.fun(input$SummaryType)(index, na.rm = TRUE), 3)) %>%
        pivot_wider(names_from = visit, values_from = index_summary)
    } else {
      index_data %>%
        filter(subject == input$selectedID) %>%
        group_by(visit) %>%
        summarise(index_summary = round(index, 3)) %>%
        pivot_wider(names_from = visit, values_from = index_summary)
    }

    title_text <- if (input$Treatment == "Pooled") {
      paste("Index Summary :", input$SummaryType)
    } else {
      paste("Subject ID:", input$selectedID)
    }

    gt_table <- gt(summary_data) %>%
      tab_header(title = title_text) %>%
      fmt_number(columns = everything(), decimals = 2) %>%
      cols_align(align = "center", columns = everything()) %>%
      tab_style(
        style = list(
          cell_fill(color = "lightgreen"),
          cell_text(weight = "bold", color = "black")
        ),
        locations = cells_column_labels(everything())
      ) %>%
      tab_options(table.width = pct(100))

    gt_output <- gt_table %>%
      as_raw_html()

    HTML(gt_output)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
