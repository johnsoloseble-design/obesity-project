library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

obesity <- read_csv("obesity_level_cleaned.csv", show_col_types = FALSE)

numeric_vars <- obesity %>%
  select(where(is.numeric)) %>%
  names()

if (!"risk_level" %in% names(obesity)) stop("Column risk_level not found in the dataset.")

ui <- fluidPage(
  titlePanel("Obesity Risk Factors Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Choose a variable:", choices = numeric_vars),
      helpText("Explore how obesity categories differ across selected risk factors.")
    ),
    mainPanel(
      plotOutput("boxplot"),
      tags$hr(),
      plotOutput("barplot")
    )
  )
)

server <- function(input, output, session) {
  output$boxplot <- renderPlot({
    ggplot(obesity, aes(x = risk_level, y = .data[[input$var]], fill = risk_level)) +
      geom_boxplot(alpha = 0.8, outlier.color = "red") +
      labs(
        x = "Obesity Level",
        y = input$var,
        title = paste(input$var, "by Obesity Level")
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  output$barplot <- renderPlot({
    ggplot(obesity, aes(x = risk_level, fill = risk_level)) +
      geom_bar() +
      labs(
        x = "Obesity Level",
        y = "Count",
        title = "Distribution of Obesity Levels"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)