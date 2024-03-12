library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)

health <- read_csv("https://uwmadison.box.com/shared/static/ao0kuonmmi01h55j0b9s9gcoivthbri0.csv") |>
  separate("Blood Pressure(mmHg)", into = c("high", "low"), sep = "/") |>
  mutate(
    Blood_High = as.integer(high),
    Blood_Low = as.integer(low),
    Height = `Height(cm)`,
    Weight = `Weight(kg)`
  )
health$`Heart Attack` <- factor(health$`Heart Attack`, levels = c(0, 1))


gender <- pull(health, Gender) |>
  unique() |>
  na.omit()

smoker <- pull(health, Smoker) |>
  unique() |>
  na.omit()

ha <- pull(health, 'Heart Attack') |>
  unique() |>
  na.omit()

scatterplot <- function(df) {
  p <- ggplot(mapping = aes(x = Blood_High, y = `Cholesterol(mg/dL)`, text = paste("ID:", ID))) +
    geom_point(data = df |> filter(selected), size = 2, alpha = 1) +
    ylim(170, 230) +
    xlim(105, 135) +
    facet_wrap(~ `Heart Attack`, labeller = labeller(`Heart Attack` = c(`0` = "No Heart Attack", `1` = "Heart Attack")), scales = "free", drop = FALSE) +
    ggtitle("Cholesterol and Blood Pressure Analysis by Heart Attack History")
  ggplotly(p, tooltip = "text") |>
    style(hoveron = "points")
}

ui <- fluidPage(
  titlePanel("Heart Attack"),
  checkboxGroupInput("smoker", "Smoker", smoker, smoker),
  checkboxGroupInput("gender", "Gender", gender, gender),
  sliderInput("Age", "Age", min = min(health$Age), max = max(health$Age), c(30, 60), sep = ""),
  sliderInput("Height", "Height", min = min(health$Height), max = max(health$Height), c(155, 183), sep = ""),
  sliderInput("Weight", "Weight", min = min(health$Weight), max = max(health$Weight), c(60, 89), sep = ""),
  plotlyOutput("health_scatter")
)

server <- function(input, output) {
  health_subset <- reactive({
    health |>
      mutate(selected = (
        (Smoker %in% input$smoker) &
          (Gender %in% input$gender) &
          (Age >= input$Age[1]) &
          (Age <= input$Age[2]) &
          (Height >= input$Height[1]) &
          (Height <= input$Height[2]) &
          (Weight >= input$Weight[1]) &
          (Weight <= input$Weight[2])
      ))
  })
  
  output$health_scatter <- renderPlotly({
    scatterplot(health_subset())
  })
}

shinyApp(ui, server)