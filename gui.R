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
  
  observeEvent(input$numbersFile, {
    estimation <- getEstimation(nav_item(), input)
    output$est_plot <- estimation[[2]]
    output$est_number <- estimation[[1]]
    output$estimation <- estimation[[3]]
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
        tabPanel("Generated Number", uiOutput("number") ),
        tabPanel("Estimation", mainPanel(
          fileInput("numbersFile", "Choose file:", accept=c("text/plain")),
          uiOutput("estimation")
          ))
      )
    )
  )
)

shinyApp(ui = ui, server = server)