# Install necessary packages if not already installed
install.packages("shiny")
install.packages("shinydashboard")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("plotly")
install.packages("ggridges")

# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggridges)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Sports Data Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Histogram of Scores", tabName = "histogram"),
      menuItem("Bar Plot of Sports", tabName = "barplot"),
      menuItem("Scatter Plot", tabName = "scatter"),
      menuItem("Violin Plot", tabName = "violinplot"),
      menuItem("Time Series Plot", tabName = "timeseries"),
      menuItem("Radar Chart", tabName = "radarchart"),
      menuItem("Ridgeline Plot", tabName = "ridgeline")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "histogram",
              fluidRow(
                box(width = 12, plotlyOutput("histPlot"))
              )),
      tabItem(tabName = "barplot",
              fluidRow(
                box(width = 12, plotlyOutput("barPlot"))
              )),
      tabItem(tabName = "scatter",
              fluidRow(
                box(width = 12, plotlyOutput("scatterPlot"))
              )),
      tabItem(tabName = "violinplot",
              fluidRow(
                box(width = 12, plotlyOutput("violinPlot"))
              )),
      tabItem(tabName = "timeseries",
              fluidRow(
                box(width = 12, plotlyOutput("timeSeriesPlot"))
              )),
      tabItem(tabName = "radarchart",
              fluidRow(
                box(width = 12, plotlyOutput("radarChart"))
              )),
      tabItem(tabName = "ridgeline",
              fluidRow(
                box(width = 12, plotlyOutput("ridgePlot"))
              ))
    )
  )
)

# Server
server <- function(input, output) {
  # Load the data
  sports_data <- read.csv("sports_data.csv")
  sports_data$Date <- as.Date(sports_data$Date)
  sports_data$Year <- format(sports_data$Date, "%Y")
  
  # Histogram of Scores
  output$histPlot <- renderPlotly({
    p <- ggplot(sports_data, aes(x = Score)) +
      geom_histogram(binwidth = 5, fill = "blue", color = "black") +
      theme_minimal() +
      labs(title = "Histogram of Scores", x = "Score", y = "Frequency")
    ggplotly(p)
  })
  
  # Bar Plot of Sports
  output$barPlot <- renderPlotly({
    p <- ggplot(sports_data, aes(x = Sport)) +
      geom_bar(fill = "skyblue", color = "black") +
      theme_minimal() +
      labs(title = "Bar Plot of Sports", x = "Sport", y = "Count")
    ggplotly(p)
  })
  
  # Scatter Plot of Score vs Age
  output$scatterPlot <- renderPlotly({
    p <- ggplot(sports_data, aes(x = Age, y = Score)) +
      geom_point(alpha = 0.5, color = "darkgreen") +
      theme_minimal() +
      labs(title = "Scatter Plot of Score vs Age", x = "Age", y = "Score")
    ggplotly(p)
  })
  
  # Violin Plot of Score by Sport
  output$violinPlot <- renderPlotly({
    p <- ggplot(sports_data, aes(x = Sport, y = Score, fill = Sport)) +
      geom_violin() +
      theme_minimal() +
      labs(title = "Violin Plot of Score by Sport", x = "Sport", y = "Score")
    ggplotly(p)
  })
  
  # Time Series Plot of Average Score over Time
  avg_score_by_year <- sports_data %>%
    group_by(Year) %>%
    summarise(Average_Score = mean(Score, na.rm = TRUE))
  
  output$timeSeriesPlot <- renderPlotly({
    p <- ggplot(avg_score_by_year, aes(x = as.integer(Year), y = Average_Score)) +
      geom_line(color = "red") +
      theme_minimal() +
      labs(title = "Average Score Over Time", x = "Year", y = "Average Score")
    ggplotly(p)
  })
  
  # Radar Chart of various metrics by Sport
  radar_data <- sports_data %>%
    group_by(Sport) %>%
    summarise(Average_Score = mean(Score, na.rm = TRUE),
              Average_Age = mean(Age, na.rm = TRUE))
  
  output$radarChart <- renderPlotly({
    plot_ly(type = 'scatterpolar', mode = 'lines') %>%
      add_trace(r = radar_data$Average_Score, theta = radar_data$Sport, name = "Average Score") %>%
      add_trace(r = radar_data$Average_Age, theta = radar_data$Sport, name = "Average Age") %>%
      layout(title = "Radar Chart of Various Metrics by Sport")
  })
  
  # Ridgeline Plot of Score by Sport
  output$ridgePlot <- renderPlotly({
    p <- ggplot(sports_data, aes(x = Score, y = Sport, fill = Sport)) +
      geom_density_ridges() +
      theme_minimal() +
      labs(title = "Ridgeline Plot of Score by Sport", x = "Score", y = "Sport")
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
