library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

sal <- read.csv("salaries.csv")

ui <- fluidPage(
  sidebarPanel(
    sliderInput(inputId = "year",
                label = "Year Range",
                sep = "",
                value= c(1985, 2016), min = 1985, max = 2016),
    selectizeInput(inputId = "teamInput", "Team", choices = c("Braves", "Orioles", "Red Sox", "Angels", "White Sox", "Cubs", "Reds", "Indians", "Tigers", "Astros", "Royals", "Dodgers", "Twins", "Brewers", "Expos", "Yankees", "Mets", "Athletics", "Phillies", "Pirates", "Padres", "Mariners", "Giants", "Cardinals", "Rangers", "Blue Jays", "Rockies", "Marlins", "Diamondbacks", "Rays", "Nationals"), selected = NULL, options= NULL) 
  ),
  mainPanel(
  plotlyOutput(outputId = "line")
  )
)

server <- function(input, output) {
  output$line <- renderPlotly({ 
    filtered <- 
      sal %>% 
      filter(Year >= input$year[1],
             Year <= input$year[2],
             Team == input$teamInput
      )
    ggplot(filtered, aes(Year, Salary)) +
      geom_point()
    })
}

shinyApp(ui = ui, server = server)