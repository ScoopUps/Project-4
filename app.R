library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)
library(RColorBrewer)
library(DT)

sal <- read.csv("Salaries.csv")
master <- read.csv("Master.csv")

ui <- fluidPage(
  
  theme = shinytheme("simplex"),
  HTML("<h1>Baseball Salaries: 1985-2016</h1>"),
  sidebarPanel(
    sliderInput(inputId = "year",
                label = "Year Range",
                sep = "",
                value= c(1985, 2016), min = 1985, max = 2016),
    selectizeInput(inputId = "teamInput", "Team", choices = c("Braves", "Orioles", "Red Sox", "Angels", "White Sox", "Cubs", "Reds", "Indians", "Tigers", "Astros", "Royals", "Dodgers", "Twins", "Brewers", "Expos", "Yankees", "Mets", "Athletics", "Phillies", "Pirates", "Padres", "Mariners", "Giants", "Cardinals", "Rangers", "Blue Jays", "Rockies", "Marlins", "Diamondbacks", "Rays", "Nationals"), options= list(maxItems=30, placeholder="Add MLB Team")) 
  ),
  mainPanel(
  plotlyOutput(outputId = "line"),
  br(), br(),
  DT::dataTableOutput("data")
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
        scale_y_continuous(labels = scales::dollar) +
        labs(y = "Salary <br> (in K)") +
        theme(axis.text.x = element_text(size = 9, angle=45)) +
        theme(axis.line = element_line(color = "forestgreen", 
                                       size = 2, linetype = "solid"))
    )
    })
  output$data <- DT::renderDataTable({
    DT::datatable(sal)
  })
}

shinyApp(ui = ui, server = server)