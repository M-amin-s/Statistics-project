library(shinythemes)
library(shiny)

source("serverFunctions.R")

server <- function(input, output, session) {
  
  nav_item <- reactive({
    input$navBar
  })
  
  observeEvent(input$navBar, {
      output$inputSection <- getInputSection(nav_item())
  })
  
  observeEvent(input$submit, {
    output$plot <- getPlot(nav_item(), input)
    output$number <- getNumber(nav_item(), input)
  })
}

ui <- fluidPage(theme=shinytheme("spacelab"),
                
  titlePanel("R project!"),
  navbarPage("Distributions", id="navBar",
    tabPanel("Uniform", value=1),
    tabPanel("Bernouli", value=2),
    tabPanel("Binomial", value=3),
    tabPanel("Geometric", value=4),
    tabPanel("Exponential", value=5),
    tabPanel("Gamma", value=6),
    tabPanel("Poisson", value=7),
    tabPanel("Normal", value=8)
  ),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("inputSection"),
      actionButton("submit", "Submit!")
    ), 
                  
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot") ),
        tabPanel("Generated Number", uiOutput("number") )
      )
    )
  )
)

shinyApp(ui = ui, server = server)