library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Load dataset
obesity <- read_csv("obesity_level_cleaned.csv", show_col_types = FALSE)

numeric_vars <- obesity %>%
  select(where(is.numeric)) %>%
  names()

ui <- fluidPage(
  titlePanel("Interactive Obesity Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", "Age", min = 0, max = 100, value = 30),
      sliderInput("height", "Height (cm)", min = 100, max = 220, value = 170),
      sliderInput("weight", "Weight (kg)", min = 20, max = 250, value = 70),
      
      selectInput(
        "chart_var",
        "Show bars for",
        choices = c("BMI Category", "Age Group", "Weight Group"),
        selected = "BMI Category"
      ),
      
      selectInput(
        "sort_order",
        "Sort bars",
        choices = c("Ascending", "Descending"),
        selected = "Descending"
      ),
      
      selectInput(
        "palette",
        "Color palette",
        choices = c("Set1", "Set2", "Dark2", "Paired"),
        selected = "Set1"
      )
    ),
    
    mainPanel(
      textOutput("summary_text"),
      plotOutput("barPlot", height = "450px"),
      br(),
      plotOutput("familyPlot", height = "420px")
    )
  )
)

server <- function(input, output, session) {
  
  bmi <- reactive({
    input$weight / ((input$height / 100)^2)
  })
  
  category_data <- reactive({
    data.frame(
      bmi_cat = cut(
        bmi(),
        breaks = c(-Inf, 18.5, 25, 30, Inf),
        labels = c("Underweight", "Normal", "Overweight", "Obese")
      ),
      age_group = cut(
        input$age,
        breaks = c(-Inf, 18, 35, 50, Inf),
        labels = c("Child/Teen", "Young Adult", "Adult", "Older Adult")
      ),
      weight_group = cut(
        input$weight,
        breaks = c(-Inf, 50, 75, 100, Inf),
        labels = c("Low", "Moderate", "High", "Very High")
      )
    )
  })
  
  output$summary_text <- renderText({
    paste0(
      "Age: ", input$age,
      " | Height: ", input$height,
      " cm | Weight: ", input$weight,
      " kg | BMI: ", round(bmi(), 1)
    )
  })
  
  output$barPlot <- renderPlot({
    df <- category_data()
    
    if (input$chart_var == "BMI Category") {
      bar_df <- df %>% count(bmi_cat) %>% rename(category = bmi_cat)
    } else if (input$chart_var == "Age Group") {
      bar_df <- df %>% count(age_group) %>% rename(category = age_group)
    } else {
      bar_df <- df %>% count(weight_group) %>% rename(category = weight_group)
    }
    
    if (input$sort_order == "Ascending") {
      bar_df <- bar_df %>% arrange(n)
    } else {
      bar_df <- bar_df %>% arrange(desc(n))
    }
    
    ggplot(bar_df, aes(x = reorder(category, n), y = n, fill = category)) +
      geom_col(width = 0.7) +
      coord_flip() +
      scale_fill_brewer(palette = input$palette) +
      labs(
        x = NULL,
        y = "Count",
        title = paste("Interactive", input$chart_var, "Bar Chart")
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  output$familyPlot <- renderPlot({
    ggplot(obesity, aes(x = family_history_with_overweight)) +
      geom_bar(fill = "steelblue") +
      labs(
        title = "Family History with Overweight",
        x = "Family history with overweight",
        y = "Count"
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui = ui, server = server)