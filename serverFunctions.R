source("phase1.R")
getInputSection <- function(nav_item){
switch(nav_item,
         '1' = return(renderUI({
           
           list(
             numericInput(inputId = "min", 
                          label = "Min:", value = 0),
             numericInput(inputId = "max", 
                          label = "Max:", value = 0)
           )
           
         })),
         '2' = return(renderUI({list(
           numericInput(inputId = "p", 
                        label = "P:", value = 0, max=1, min=0)
         )})),
         '3' = return(renderUI({list(
           numericInput(inputId = "p", 
                        label = "P:", value = 0, max=1, min=0),
           numericInput(inputId = "n", 
                        label = "N:", value = 0)
         )})),
         '4'  = return(renderUI({list(
           numericInput(inputId = "p", 
                        label = "P:", value = 0, max=1, min=0)
         )})),
         '5'  = return(renderUI({list(
           numericInput(inputId = "lambda", 
                        label = "Lambda:", value = 0)
         )})),
         '6'  = return(renderUI({list(
           numericInput(inputId = "lambda", 
                        label = "Lambda:", value = 0),
           numericInput(inputId = "k", 
                        label = "K:", value = 0)
         )})),
         '7'  = return(renderUI({list(
           numericInput(inputId = "lambda", 
                        label = "Lambda:", value = 0),
           numericInput(inputId = "t", 
                        label = "t:", value = 0)
         )})),
          '8'  = return(renderUI({list(
            numericInput(inputId = "u", 
                         label = "Mean:", value = 0),
            numericInput(inputId = "s", 
                         label = "Standard Deviation:", value = 0)
          )}))
  )
}

getPlot <- function(nav_item, input){
  switch (nav_item,
    '1' = return(renderPlot({
      duplot(input$min, input$max)
    })),
    '2' = return(renderPlot({
      brplot(input$p)
    })),
    '3' = return(renderPlot({
      biplot(input$p, input$n)
    })), 
    '4' = return(renderPlot({
      geplot(input$p)
    })),
    '5' = return(renderPlot({
      expplot(input$lambda)
    })),
    '6' = return(renderPlot({
      gaplot(input$lambda, input$k)
    })),
    '7' = return(renderPlot({
      poplot(input$lambda, input$t)
    })),
    '8' = return(renderPlot({
      noplot(input$u, input$s)
    }))
  )
}

getNumber <- function(nav_item, input){
  switch (nav_item,
    '1' = return(renderUI({list(
      h1(paste("Generated Number:", dugen(input$min, input$max), " "))
    )})),
    '2' = return(renderUI({list(
      h1(paste("Generated Number:", brgen(input$p), " "))
    )})),
    '3' = return(renderUI({list(
      h1(paste("Generated Number:", bigen(input$p, input$n), " "))
    )})),
    '4' =  return(renderUI({list(
      h1(paste("Generated Number:", gegen(input$p), " "))
    )})),
    '5' =  return(renderUI({list(
      h1(paste("Generated Number:", expgen(input$lambda), " "))
    )})),
    '6' =  return(renderUI({list(
      h1(paste("Generated Number:", gagen(input$lambda, input$k), " "))
    )})),
    '7' =  return(renderUI({list(
      h1(paste("Generated Number:", pogen(input$lambda, input$t), " "))
    )})),
    '8' =  return(renderUI({list(
      h1(paste("Generated Number:", nogen(input$u, input$s), " "))
    )}))
  )
}

getEstimation <- function(nav_item, input, output){
  file <- input$numbersFile
  fileName <- file$datapath
  file_content <- readChar(fileName, file.info(fileName)$size)
  est_funcs <- c(find_uiniform, find_bernouli, find_binomial, find_geometric, find_exponential, 
                 find_gamma, find_poisson, find_normal)
  params <- est_funcs[[as.numeric(nav_item)]](file_content)
  myInput <- list()
  if (nav_item == '1'){
    myInput$min <- params[[2]]
    myInput$max <- params[[3]]
    number <-getNumber(nav_item, myInput)
    plot <- getPlot(nav_item, myInput)
    return(c(number, plot, renderUI({list(
      h2(paste("min:", params[[2]])),
      h2(paste("max:", params[[3]])),
      uiOutput("est_number"),
      plotOutput("est_plot")
    )})))
  }else if(nav_item == '2'){
    myInput$p <- params[[1]]
    number <-getNumber(nav_item, myInput)
    plot <- getPlot(nav_item, myInput)
    return(c(number, plot, renderUI({list(
      h2(paste("P:", params[[1]])),
      uiOutput("est_number"),
      plotOutput("est_plot")
    )})))
  }else if(nav_item == '3'){
    myInput$p <- params[[1]]
    myInput$n <- params[[3]]
    number <-getNumber(nav_item, myInput)
    plot <- getPlot(nav_item, myInput)
    return(c(number, plot, renderUI({list(
      h2(paste("P:", params[[1]])),
      h2(paste("N:", params[[3]])),
      uiOutput("est_number"),
      plotOutput("est_plot")
    )})))
  }else if(nav_item == '4'){
    myInput$p <- params[[1]]
    number <-getNumber(nav_item, myInput)
    plot <- getPlot(nav_item, myInput)
    return(c(number, plot, renderUI({list(
      h2(paste("P:", params[[1]])),
      uiOutput("est_number"),
      plotOutput("est_plot")
    )})))
  }else if(nav_item == '5'){
    myInput$lambda <- params[[1]]
    number <-getNumber(nav_item, myInput)
    plot <- getPlot(nav_item, myInput)
    return(c(number, plot, renderUI({list(
      h2(paste("Lambda:", params[[1]])),
      uiOutput("est_number"),
      plotOutput("est_plot")
    )})))
  }else if(nav_item == '6'){
    myInput$lambda <- params[[1]]
    myInput$k <- params[[2]]
    number <-getNumber(nav_item, myInput)
    plot <- getPlot(nav_item, myInput)
    return(c(number, plot, renderUI({list(
      h2(paste("Lambda:", params[[1]])),
      h2(paste("K:", params[[2]])),
      uiOutput("est_number"),
      plotOutput("est_plot")
    )})))
  }else if(nav_item == '7'){
    myInput$lambda <- params[[1]]
    myInput$t <- 1
    number <-getNumber(nav_item, myInput)
    plot <- getPlot(nav_item, myInput)
    return(c(number, plot, renderUI({list(
      h2(paste("Lambda:", params[[1]])),
      uiOutput("est_number"),
      plotOutput("est_plot")
    )})))
  }else if(nav_item == '8'){
    myInput$u <- params[[1]]
    myInput$s <- params[[2]]
    number <-getNumber(nav_item, myInput)
    plot <- getPlot(nav_item, myInput)
    return(c(number, plot, renderUI({list(
      h2(paste("Mean:", params[[1]])),
      h2(paste("Standard Deviation:", params[[2]])),
      uiOutput("est_number"),
      plotOutput("est_plot")
    )})))
  }
}
