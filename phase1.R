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



#section6
expgen <- function(lambda){
  (-1/lambda)*log(cugen())
}

#section7
gagen <- function(lambda, k){
  result <- 0
  for (i in 1:(k-1)){
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

