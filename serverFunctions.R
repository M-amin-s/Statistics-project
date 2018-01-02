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
      h1(paste(dugen(input$min, input$max), " "))
    )})),
    '2' = return(renderUI({list(
      h1(paste(brgen(input$p), " "))
    )})),
    '3' = return(renderUI({list(
      h1(paste(bigen(input$p, input$n), " "))
    )})),
    '4' =  return(renderUI({list(
      h1(paste(gegen(input$p), " "))
    )})),
    '5' =  return(renderUI({list(
      h1(paste(expgen(input$lambda), " "))
    )})),
    '6' =  return(renderUI({list(
      h1(paste(gagen(input$lambda, input$k), " "))
    )})),
    '7' =  return(renderUI({list(
      h1(paste(pogen(input$lambda, input$t), " "))
    )})),
    '8' =  return(renderUI({list(
      h1(paste(nogen(input$u, input$s), " "))
    )}))
  )
}
