library(ggplot2)

#section1 
int_to_unit <- function (x, adjustment=2^32) {
  x <- as.numeric(x)
  signs <- sign(x)
  x[signs < 0] <- x[signs < 0] + adjustment
  x
}

t <- 0
x <- 123456789
y <- 362436069
z <- 521288629
w <- 88675123
v <- 5783321
d <- 6615241

rgenerator <- function(){
  
  t <<- (bitwXor(x, bitwShiftR(x, 2)))
  x <<- y
  y <<- z
  z <<- w
  w <<- v
  v <<- bitwXor((bitwXor(v, bitwShiftL(v, 4))), bitwXor(t, bitwShiftL(t, 1)))
  
  d <- d + 362437
  res <- d + v
  if(res < 0)
    res <- int_to_unit(res)
  
  return(res)
}

#section2
  #step2
dugen <- function(min, max){
  result <- abs(rgenerator())
  result <- ((result*(max - min)) / (2**32 - 1)) + min
  return(result)
}
  #step3
cugen <- function(){
  return(dugen(0,1))
}

#section3
brgen <- function(p){
  rand = cugen()
  if(rand <= p){
    return(1)
  }else{
    return(0)
  }
}

#section4
bigen <-function(p,n){
  x <- 0
  for(i in 1:n)
    x <- x + brgen(p)
  x
}

#section5
gegen <- function(p){
  i <- 0
  while(brgen(p = p) != 1){}
  
  while(brgen(p = p) != 1){
    i <- i + 1
  }
  return(i)
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
  print(qplot(x = vec,y = y))
  plot <- qplot(y, geom = "density", fill=I("blue"),col=I("black"),
        xlim=c(floor(min(y)),ceiling(max(y))))
  print(plot)
  return(plot)
}

duplot <- function(min,max){
  return(plotter(dugen,list(min,max)))
}

cuplot <- function(){
  return(plotter(cugen,list()))
}

brplot <- function(p){
  return(plotter(brgen,list(p)))
}

biplot <- function(p,n){
  return(plotter(bigen,list(p,n)))
}

geplot <- function(p){
  return(plotter(gegen,list(p)))
}

expplot <- function(lambda){
  return(plotter(expgen,list(lambda)))
}


gaplot <- function(lambda,k){
  return(plotter(gagen,list(lambda,k)))
}

poplot <- function(lambda,t){
  return(plotter(pogen,list(lambda,t)))
}

noplot <- function(u,s){
  return(plotter(nogen,list(u,s)))
}