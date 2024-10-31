#4 GapMinder Shiny Dashboard

library(arules)
library(arulesViz)
library(dbplyr)
library(gghighlight)
library(ggplot2)
library(ggraph)
library(ggrepel)
library(ggthemes)
library(plotly)
library(shiny)
library(tibble)


ui <- fluidPage(
  titlePanel("Gapminder Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("continent", "Select a Continent:", choices = unique(group_gapminder$continent)),
      sliderInput("yearRange", "Select a Year Range:", min = min(group_gapminder$year), max = max(group_gapminder$year), value = c(1950, 2007), sep = "")
    ),
    mainPanel(
      plotlyOutput("scatterplot"),
      plotlyOutput("lineplot")
    )
  )
)

server <- function(input, output) {
  filteredData <- reactive({
    group_gapminder %>%
      filter(continent == input$continent, year >= input$yearRange[1], year <= input$yearRange[2])
  })
  
  output$scatterplot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = avg_gdp_per_capita, y = avg_life_expectancy)) +
      geom_point(size = 1.5) +
      labs(title = "GDP per Capita vs Life Expectancy", x = "GDP per Capita", y = "Life Expectancy") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$lineplot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = year, y = avg_life_expectancy, group = continent, color = continent)) +
      geom_line(size = 1.0) +
      labs(title = "Life Expectancy Over Time", x = "Year", y = "Life Expectancy") +
      theme_minimal()
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)