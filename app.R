library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)
library(RColorBrewer)

sal <- read.csv("Salaries.csv")
master <- read.csv("Master.csv")

ui <- fluidPage(
  
  theme = shinytheme("superhero"),
  HTML("<h1>Baseball Salaries: 1985-2016</h1>"),
  sidebarPanel(
    sliderInput(inputId = "year",
                label = "Year Range",
                sep = "",
                value= c(1985, 2016), min = 1985, max = 2016),
    selectizeInput(inputId = "teamInput", "Team", choices = c("Braves", "Orioles", "Red Sox", "Angels", "White Sox", "Cubs", "Reds", "Indians", "Tigers", "Astros", "Royals", "Dodgers", "Twins", "Brewers", "Expos", "Yankees", "Mets", "Athletics", "Phillies", "Pirates", "Padres", "Mariners", "Giants", "Cardinals", "Rangers", "Blue Jays", "Rockies", "Marlins", "Diamondbacks", "Rays", "Nationals"), options= list(maxItems=30, placeholder="Add MLB Team")) 
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
    gg <- ggplotly(
      ggplot(filtered, aes(Year, Salary, text=paste(Name), color=Team)) +
        geom_point() + 
        theme(axis.text.x = element_text(size = 10, angle=45)) +
        theme(axis.line = element_line(color = "forestgreen", 
                                       size = 2, linetype = "solid"))
    )
    })
}

shinyApp(ui = ui, server = server)