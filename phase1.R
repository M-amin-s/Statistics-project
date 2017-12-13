library(ggplot2)

#section1 
rgenerator <- function(){
  #TODO
  return(runif(1,min=1,max=100))
}

#section2
  #step2
dugen <- function(min, max){
  result <- rgenerator()
  result <- (result%%(max - min)) + min
  return(result)
}
  #step3
cugen <- function(){
  return(dugen(0,1))
}

#section3
brgen <- function(p){
  
}

#section4
bigen <-function(p,n){
  x <- 0
  for(i in 1:n)
    x <- x + brgen(p)
}

#section5
gegen <- function(p){
  
}

#section6
expgen <- function(lambda){
  (-1/lambda)*log(cugen())
}

#section7
gagen <- function(lambda, k){
  result <- 0
  for (i in 1:k){
    result <- result + expgen(lambda)
  }
  return(result)
}

#section8
pogen <- function(lambda, t){
  result <- -1
  time <- 0
  while(time < t){
    result <- result + 1
    time <- time + expgen(lambda)
  }
  return(result)
}

#section9
nogen <- function(u,s){
  s <- sqrt(s)
  x <- pogen(10,10)
  x <- x * s / 10
  x <- x + (u - 10 * s)
}

#section10
plotter <- function(func,args){
  y <- c()
  vec <- 1:1000
  for(i in vec)
    y <- c(y,do.call(func,args = args))
  qplot(x = vec,y = y)  
}

duplot <- function(min,max){
  plotter(dugen,list(min,max))
}

cuplot <- function(){
  plotter(cugen,list())
}

brplot <- function(p){
  plotter(brgen,list(p))
}

biplot <- function(p,n){
  plotter(bigen,list(p,n))
}

geplot <- function(p){
  plotter(gegen,list(p))
}

expplot <- function(lambda){
  plotter(expgen,list(lambda))
}


gaplot <- function(lambda,k){
  plotter(gagen,list(lambda,k))
}

poplot <- function(lambda,t){
  plotter(pogen,list(lambda,t))
}

noplot <- function(u,s){
  plotter(nogen,list(u,s))
}