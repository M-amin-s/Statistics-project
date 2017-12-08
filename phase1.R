
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

