# Load required libraries
library(shiny)
library(shinyjs)
library(plotly)
library(dplyr)
library(readr)
library(gt)
library(tidyverse)
library(knitr)
library(kableExtra)

# Function to calculate AUC using the trapezoidal rule
trapezoidal_auc <- function(time, value) {
  n <- length(time)
  auc <- sum(diff(time) * (value[-1] + value[-n]) / 2)
  return(abs(auc))
}

# Function to convert "hr" strings to minutes
convert_to_minutes <- function(time_str) {
  hours <- as.numeric(sub("hr", "", time_str))
  return(hours * 60)
}

# Function to calculate the Index
calculate_index <- function(data_cpep, data_gluc) {
  data_cpep$Time <- sapply(data_cpep$Time, convert_to_minutes)
  data_gluc$Time <- sapply(data_gluc$Time, convert_to_minutes)

  auc_cpep <- data_cpep %>%
    group_by(ID, Visit) %>%
    summarise(auc_cpep = trapezoidal_auc(as.numeric(Time), Measurement)) %>%
    ungroup()

  auc_gluc <- data_gluc %>%
    group_by(ID, Visit) %>%
    summarise(auc_gluc = trapezoidal_auc(as.numeric(Time), Measurement)) %>%
    ungroup()

  index_data <- auc_cpep %>%
    inner_join(auc_gluc, by = c("ID", "Visit")) %>%
    mutate(index = (auc_cpep / auc_gluc) * 100)

  return(index_data)
}

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .row {
        margin-bottom: 5px;
      }
      .form-group {
        margin-bottom: 5px;
      }
    "))
  ),

  fluidRow(
    column(9, titlePanel("Summary statistics and AUC calculation for Trial 111 and 112 data")),
    column(3, tags$img(src = "logo.png", height = "80px", style = "float: right;"))
  ),

  fluidRow(
    column(3,
           radioButtons("Measurement", "Measurement:",
                        choices = c("Glucose", "C-peptide", "Index"),
                        selected = "C-peptide"),
           radioButtons("Treatment", "Visualization:",
                        choices = c("Pooled", "Individual"),
                        selected = "Pooled"),
           conditionalPanel(
             condition = "input.Measurement != 'Index'",
             radioButtons("plotvariable", "Variable for X-axis",
                          choices = c("Visit", "Time"),
                          selected = "Visit")
           )
    ),
    column(3,
           checkboxInput("Summary", "Summary", value = FALSE),
           conditionalPanel(
             condition = "input.Measurement != 'Index'",
             checkboxInput("showAUC", "AUC", value = FALSE)
           ),
           conditionalPanel(
             condition = "input.Treatment == 'Pooled' && input.Measurement != 'Index'",
             checkboxInput("CI", "Confidence Interval", value = FALSE)
           ),
           checkboxInput("showPlot", "Show Plot", value = TRUE),
           checkboxInput("showTable", "Show Table", value = TRUE)
    ),
    column(3,
           conditionalPanel(
             condition = "input.Summary == true && input.Treatment != 'Individual'",
             selectInput("SummaryType", "Summary:",
                         choices = c("mean", "sd", "median", "min", "max", "IQR"),
                         selected = "mean")
           ),
           conditionalPanel(
             condition = "input.showAUC == true && input.Treatment != 'Individual' && input.Measurement != 'Index'",
             selectInput("AUC_SummaryType", "AUC Summary:",
                         choices = c("mean", "sd", "median", "min", "max", "IQR"),
                         selected = "mean")
           ),
           conditionalPanel(
             condition = "input.Treatment == 'Pooled' && input.CI == true && input.Measurement != 'Index'",
             sliderInput("CI_level", "Confidence Level",
                         min = 0.8, max = 1, value = 0.95, step = 0.025)
           ),
           conditionalPanel(
             condition = "input.Treatment == 'Individual' && input.Measurement != 'Index'",
             uiOutput("individualSelect")
           )
    ),
    column(3,
           actionButton("runBtn", "Run",
                        style = "color: #fff; background-color: #007bff; border-color: #007bff; padding: 10px 24px; font-size: 16px; display: block; margin: 0 auto;"))
  ),

  fluidRow(
    column(6,
           h3("Raw Data"),
           conditionalPanel(
             condition = "input.showPlot == true && input.Measurement != 'Index'",
             plotlyOutput("InteractionPlot_raw")
           ),
           conditionalPanel(
             condition = "input.showTable == true && input.Measurement != 'Index'",
             uiOutput("Summary_raw")
           ),
           conditionalPanel(
             condition = "input.showAUC == true && input.showPlot == true && input.Measurement != 'Index'",
             plotlyOutput("AUCPlot_raw")
           ),
           conditionalPanel(
             condition = "input.showAUC == true && input.showTable == true && input.Measurement != 'Index'",
             uiOutput("AUCSummary_raw")
           ),
           conditionalPanel(
             condition = "input.showPlot == true && input.Measurement == 'Index'",
             plotlyOutput("IndexPlot_raw")
            ),
           conditionalPanel(
             condition = "input.showTable == true && input.Measurement == 'Index'",
                            uiOutput("IndexSummary_raw")
           )
    ),
    column(6,
           h3("Imputed Data"),
           conditionalPanel(
             condition = "input.showPlot == true && input.Measurement != 'Index'",
             plotlyOutput("InteractionPlot_imputed")
           ),
           conditionalPanel(
             condition = "input.showTable == true && input.Measurement != 'Index'",
             uiOutput("Summary_imputed")
           ),
           conditionalPanel(
             condition = "input.showAUC == true && input.showPlot == true && input.Measurement != 'Index'",
             plotlyOutput("AUCPlot_imputed")
           ),
           conditionalPanel(
             condition = "input.showAUC == true && input.showTable == true && input.Measurement != 'Index'",
             uiOutput("AUCSummary_imputed")
           ),
           conditionalPanel(
             condition = "input.showPlot == true && input.Measurement == 'Index'",
             plotlyOutput("IndexPlot_imputed")
           ),
           conditionalPanel(
             condition = "input.showTable == true && input.Measurement == 'Index'",
             uiOutput("IndexSummary_imputed")
           )
    )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # Load datasets
  data_cpep <- read.csv("data/data_cpep.csv")
  data_gluc <- read.csv("data/data_gluc.csv")
  data_cpep_mice <- read.csv("data/data_cpep_mice.csv")
  data_gluc_mice <- read.csv("data/data_gluc_mice.csv")

  # Reactive function to select data based on measurement
  data <- reactive({
    if (input$Measurement == "Glucose") {
      list(raw = data_gluc, imputed = data_gluc_mice)
    } else if (input$Measurement == "C-peptide") {
      list(raw = data_cpep, imputed = data_cpep_mice)
    } else if (input$Measurement == "Index") {
      list(raw = list(cpep = data_cpep, gluc = data_gluc),
           imputed = list(cpep = data_cpep_mice, gluc = data_gluc_mice))
    } else {
      list(raw = NULL, imputed = NULL)
    }
  })

  # UI for selecting individual IDs
  output$individualSelect <- renderUI({
    if(input$Measurement == "Index"){
      # Use the combined Index dataset
      selectInput("selectedID", "Select ID:",
                  choices = unique(data()$raw$cpep$ID),
                  selected = unique(data()$raw$cpep$ID)[1])
    } else if (input$Measurement %in% c("Glucose", "C-peptide")) {
      # Use the dataset corresponding to Glucose or C-peptide
      selectInput("selectedID", "Select ID:",
                  choices = unique(data()$raw$ID),
                  selected = unique(data()$raw$ID)[1])
    } else {
      # Default behavior in case of unexpected input$Measurement value
      selectInput("selectedID", "Select ID:",
                  choices = NULL,
                  selected = NULL)
    }
  })


  # Interaction Plot for Raw Data (Left Column) using plotly
  output$InteractionPlot_raw <- renderPlotly({
    req(input$runBtn)
    plot_data <- data()$raw

    if (input$Treatment == "Pooled") {
      summary_data <- plot_data %>%
        group_by(Visit, Time) %>%
        summarise(
          Measurement_mean = round(match.fun(input$SummaryType)(Measurement, na.rm = TRUE), 3),
          Measurement_sd = sd(Measurement, na.rm = TRUE),
          Measurement_lower = Measurement_mean - qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * Measurement_sd / sqrt(n()),
          Measurement_upper = Measurement_mean + qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * Measurement_sd / sqrt(n())
        ) %>%
        ungroup()

      plot <- switch(input$plotvariable,
                     "Visit" = {
                       plot_ly(summary_data, x = ~Time, y = ~Measurement_mean, color = ~as.factor(Visit), type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Time"),
                           yaxis = list(title = paste(input$Measurement)),
                           legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
                           title = paste("Summary:", input$SummaryType)
                         )
                     },
                     "Time" = {
                       plot_ly(summary_data, x = ~Visit, y = ~Measurement_mean, color = ~as.factor(Time), type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Visit"),
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
      plot_data <- plot_data %>% filter(ID == input$selectedID)
      plot <- switch(input$plotvariable,
                     "Visit" = {
                       plot_ly(plot_data, x = ~Time, y = ~Measurement, color = ~as.factor(Visit), type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Time"),
                           yaxis = list(title = paste(input$Measurement)),
                           legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
                           title = paste("Subject ID:", input$selectedID)
                         )
                     },
                     "Time" = {
                       plot_ly(plot_data, x = ~Visit, y = ~Measurement, color = ~as.factor(Time), type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Visit"),
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

    if (input$Treatment == "Pooled") {
      summary_data <- plot_data %>%
        group_by(Visit, Time) %>%
        summarise(
          Measurement_mean = round(match.fun(input$SummaryType)(Measurement, na.rm = TRUE), 3),
          Measurement_sd = sd(Measurement, na.rm = TRUE),
          Measurement_lower = Measurement_mean - qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * Measurement_sd / sqrt(n()),
          Measurement_upper = Measurement_mean + qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * Measurement_sd / sqrt(n())
        ) %>%
        ungroup()

      plot <- switch(input$plotvariable,
                     "Visit" = {
                       plot_ly(summary_data, x = ~Time, y = ~Measurement_mean, color = ~as.factor(Visit), type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Time"),
                           yaxis = list(title = paste(input$Measurement)),
                           legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
                           title = paste("Summary:", input$SummaryType)
                         )
                     },
                     "Time" = {
                       plot_ly(summary_data, x = ~Visit, y = ~Measurement_mean, color = ~as.factor(Time), type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Visit"),
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
      plot_data <- plot_data %>% filter(ID == input$selectedID)
      plot <- switch(input$plotvariable,
                     "Visit" = {
                       plot_ly(plot_data, x = ~Time, y = ~Measurement, color = ~as.factor(Visit), type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Time"),
                           yaxis = list(title = paste(input$Measurement)),
                           legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
                           title = paste("Subject ID:", input$selectedID)
                         )
                     },
                     "Time" = {
                       plot_ly(plot_data, x = ~Visit, y = ~Measurement, color = ~as.factor(Time), type = 'scatter', mode = 'lines+markers') %>%
                         layout(
                           xaxis = list(title = "Visit"),
                           yaxis = list(title = paste(input$Measurement)),
                           legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
                           title = paste("Subject ID:", input$selectedID)
                         )
                     })
    }

    plot
  })

  # Summary statistics for Raw Data (Left Column) using gt
  output$Summary_raw <- render_gt({
    req(input$runBtn)

    summary_data <- if (input$Treatment == "Pooled") {
      data()$raw %>%
        group_by(Visit, Time) %>%
        summarise(Measurement = round(match.fun(input$SummaryType)(Measurement, na.rm = TRUE), 3)) %>%
        pivot_wider(names_from = Visit, values_from = Measurement)
    } else {
      data()$raw %>%
        filter(ID == input$selectedID) %>%
        group_by(Visit, Time) %>%
        summarise(Measurement = round(Measurement, 3)) %>%
        pivot_wider(names_from = Visit, values_from = Measurement)
    }

    title_text <- if (input$Treatment == "Pooled") {
      paste("Summary:", input$SummaryType)
    } else {
      paste("Subject ID:", input$selectedID)
    }

    gt(summary_data) %>%
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
  })

  # Summary statistics for Imputed Data (Right Column) using gt
  output$Summary_imputed <- render_gt({
    req(input$runBtn)

    summary_data <- if (input$Treatment == "Pooled") {
      data()$imputed %>%
        group_by(Visit, Time) %>%
        summarise(Measurement = round(match.fun(input$SummaryType)(Measurement, na.rm = TRUE), 3)) %>%
        pivot_wider(names_from = Visit, values_from = Measurement)
    } else {
      data()$imputed %>%
        filter(ID == input$selectedID) %>%
        group_by(Visit, Time) %>%
        summarise(Measurement = round(Measurement, 3)) %>%
        pivot_wider(names_from = Visit, values_from = Measurement)
    }

    title_text <- if (input$Treatment == "Pooled") {
      paste("Summary:", input$SummaryType)
    } else {
      paste("Subject ID:", input$selectedID)
    }

    gt(summary_data) %>%
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

  })

  # AUC Plot for raw data (Left Column) using plotly with conditional CI
  output$AUCPlot_raw <- renderPlotly({
    req(input$runBtn)
    data <- data()$raw
    data$Time <- sapply(data$Time, convert_to_minutes)

    auc_trapezoidal <- data %>%
      group_by(ID, Visit) %>%
      summarise(auc = trapezoidal_auc(as.numeric(Time), Measurement)) %>%
      ungroup()

    if (input$Treatment == "Pooled") {
      summary_data <- auc_trapezoidal %>%
        group_by(Visit) %>%
        summarise(
          auc_mean = match.fun(input$AUC_SummaryType)(auc, na.rm = TRUE),
          auc_sd = sd(auc, na.rm = TRUE),
          auc_lower = auc_mean - qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * auc_sd / sqrt(n()),
          auc_upper = auc_mean + qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * auc_sd / sqrt(n())
        )

      plot <- plot_ly(summary_data, x = ~Visit, y = ~auc_mean, name = 'AUC Mean', type = 'scatter', mode = 'lines+markers') %>%
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
      auc_ID <- auc_trapezoidal %>% filter(ID == input$selectedID)
      plot <- plot_ly(auc_ID, x = ~Visit, y = ~auc, type = 'scatter', mode = 'lines+markers',
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
    data$Time <- sapply(data$Time, convert_to_minutes)

    auc_trapezoidal <- data %>%
      filter(!is.na(Measurement)) %>%   # Ensure NA values are excluded
      group_by(ID, Visit) %>%
      summarise(auc = trapezoidal_auc(as.numeric(Time), Measurement)) %>%
      ungroup()

    if (input$Treatment == "Pooled") {
      summary_data <- auc_trapezoidal %>%
        group_by(Visit) %>%
        summarise(
          auc_mean = match.fun(input$AUC_SummaryType)(auc, na.rm = TRUE),
          auc_sd = sd(auc, na.rm = TRUE),
          auc_lower = auc_mean - qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * auc_sd / sqrt(n()),
          auc_upper = auc_mean + qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * auc_sd / sqrt(n())
        )

      plot <- plot_ly(summary_data, x = ~Visit, y = ~auc_mean, name = 'AUC Mean', type = 'scatter', mode = 'lines+markers') %>%
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
      auc_ID <- auc_trapezoidal %>% filter(ID == input$selectedID)
      plot <- plot_ly(auc_ID, x = ~Visit, y = ~auc, type = 'scatter', mode = 'lines+markers',
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
        group_by(Visit) %>%
        summarise(
          index_mean = round(match.fun(input$SummaryType)(index, na.rm = TRUE), 3),
          index_sd = sd(index, na.rm = TRUE),
          index_lower = index_mean - qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * index_sd / sqrt(n()),
          index_upper = index_mean + qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * index_sd / sqrt(n())
        ) %>%
        ungroup()

      plot <- plot_ly(summary_data, x = ~Visit, y = ~index_mean, type = 'scatter', mode = 'lines+markers') %>%
        layout(
          xaxis = list(title = "Visit"),
          yaxis = list(title = "Index"),
          title = paste("Index Summary:", input$SummaryType)
        )
    } else if (input$Treatment == "Individual") {
      plot_data <- plot_data %>% filter(ID == input$selectedID)

      plot <- plot_ly(plot_data %>% filter(!is.na(index)), x = ~Visit, y = ~index, type = 'scatter', mode = 'lines+markers') %>%
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
        group_by(Visit) %>%
        summarise(
          index_mean = round(match.fun(input$SummaryType)(index, na.rm = TRUE), 3),
          index_sd = sd(index, na.rm = TRUE),
          index_lower = index_mean - qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * index_sd / sqrt(n()),
          index_upper = index_mean + qt(1 - (1 - input$CI_level) / 2, df = n() - 1) * index_sd / sqrt(n())
        ) %>%
        ungroup()

      plot <- plot_ly(summary_data, x = ~Visit, y = ~index_mean, type = 'scatter', mode = 'lines+markers') %>%
        layout(
          xaxis = list(title = "Visit"),
          yaxis = list(title = "Index"),
          title = paste("Index Summary :", input$SummaryType)
        )
    } else if (input$Treatment == "Individual") {
      plot_data <- plot_data %>% filter(ID == input$selectedID)

      plot <- plot_ly(plot_data %>% filter(!is.na(index)), x = ~Visit, y = ~index, type = 'scatter', mode = 'lines+markers') %>%
        layout(
          xaxis = list(title = "Visit"),
          yaxis = list(title = "Index"),
          title = paste("Subject ID:", input$selectedID)
        )
    }

    plot
  })

  # Summary for Raw Data Index using gt
  output$IndexSummary_raw <- render_gt({
    req(input$runBtn)

    index_data <- calculate_index(data()$raw$cpep, data()$raw$gluc)

    summary_data <- if (input$Treatment == "Pooled") {
      index_data %>%
        group_by(Visit) %>%
        summarise(index_summary = round(match.fun(input$SummaryType)(index, na.rm = TRUE), 3)) %>%
        pivot_wider(names_from = Visit, values_from = index_summary)
    } else {
      index_data %>%
        filter(ID == input$selectedID) %>%
        group_by(Visit) %>%
        summarise(index_summary = round(index, 3)) %>%
        pivot_wider(names_from = Visit, values_from = index_summary)
    }

    title_text <- if (input$Treatment == "Pooled") {
      paste("Index Summary:", input$SummaryType)
    } else {
      paste("Subject ID:", input$selectedID)
    }

    gt(summary_data) %>%
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
  })

  # Summary for Imputed Data Index using gt
  output$IndexSummary_imputed <- render_gt({
    req(input$runBtn)

    index_data <- calculate_index(data()$imputed$cpep, data()$imputed$gluc)

    summary_data <- if (input$Treatment == "Pooled") {
      index_data %>%
        group_by(Visit) %>%
        summarise(index_summary = round(match.fun(input$SummaryType)(index, na.rm = TRUE), 3)) %>%
        pivot_wider(names_from = Visit, values_from = index_summary)
    } else {
      index_data %>%
        filter(ID == input$selectedID) %>%
        group_by(Visit) %>%
        summarise(index_summary = round(index, 3)) %>%
        pivot_wider(names_from = Visit, values_from = index_summary)
    }

    title_text <- if (input$Treatment == "Pooled") {
      paste("Index Summary :", input$SummaryType)
    } else {
      paste("Subject ID:", input$selectedID)
    }

    gt(summary_data) %>%
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
  })
}

# Run the application
shinyApp(ui = ui, server = server)
