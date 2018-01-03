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


#' rgenerator
#' 
#' This function gives you a number from 0 to 2^32-1. It uses xorwow algorithm.
#' 
#' @usage rgenerator()
#' @author AmirHossein Motameni


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

#' cugen
#' 
#' generate uniform random numbers between 0 and 1, using function dugen(a,b) which taking the two integer numbers as input and generates random numbers uniformly distributed between two integer inputs.
#' 
#' @usage cugen()
#' @author MohammadAmin Salarkia
#' @example cugen()
#' @seealso dugen(), cugen(), brgen(), bigen(), gegen(), expgen(), gagen(), pogen(), nogen()
#' related functions: dugen

cugen <- function(){
  return(dugen(0,1))
}

#section3

#' brgen
#' 
#' Bernoulli distribution is one the most simple as well as famous distributions. In each Bernoulli trial there are two possible outcomes, namely success or failure, for the probability of success equal to p, the probability of failure is 1 ??? p.
#' 
#' @usage brgen(p)
#' @param p is the probability
#' @author Reza Mousapour
#' @example brgen(0, 2) 
#' @seealso dugen(), cugen(), gegen(), expgen(), gagen(), pogen(), nogen(), bigen()


brgen <- function(p){
  rand = cugen()
  if(rand <= p){
    return(1)
  }else{
    return(0)
  }
}

#section4

#' bigen
#' 
#' A binomial random variable can be seen as the result of repeated Bernoulli Trials. This function gives you a number which follows binomial distribution. It uses bernoulli distribution function (bergen (p)) to make binomial numbers.
#' 
#' @usage bigen(p, n)
#' @param p is the probability
#' @param n is the number of times you want to repeat the bernoulli trial.
#' @author AmirHossein Motameni
#' @example bigen(0, 5, 10)
#' 
#' @seealso dugen(), cugen(), brgen(), gegen(), expgen(), gagen(), pogen(), nogen()

bigen <-function(p,n){
  x <- 0
  for(i in 1:n)
    x <- x + brgen(p)
  x
}

#section5

#' gegen
#' 
#' The number of failures in Bernoulli trials, between two wins, follows the geometric distributions. 
#' 
#' @usage gegen(p)
#' @param p is the probability
#' @author Reza Mousapour
#' @example gegen(0, 2)
#' @seealso dugen(), cugen(), brgen(), bigen(), gegen(), expgen(), gagen(), pogen(), nogen()
#' related functions: brgen

gegen <- function(p){
  i <- 0
  while(brgen(p = p) != 1){}
  
  while(brgen(p = p) != 1){
    i <- i + 1
  }
  return(i)
}

#section6


#' expgen
#' 
#' Exponential distribution is a popular distribution which is used to model waiting times and memoryless processes. This function gives you a number which follows exponential distribution. It uses uniform distribution function (cugen ()) to make exponential distributed numbers. It is calculated by 1/lambda*log(x).
#' 
#' @usage expgen(lambda)
#' @param lambda
#' @author Ali Ehteshami
#' @example expgen(0, 1)
#' 
#' @seealso dugen(), cugen(), brgen(), bigen(), gegen(), gagen(), pogen(), nogen()

expgen <- function(lambda){
  (-1/lambda)*log(cugen())
}

#section7

#' gagen
#' 
#' Summation of k i.i.d exponential random variables leads to a gamma distributed random variable. 
#' 
#' @usage gagen(theta, k)
#' @param Theta is a float number as the parameter of the underlying exponential distribution 
#' @param k is an integer
#' @author MohammadAmin Salarkia
#' @example gagen(2, 3)
#' @seealso dugen(), cugen(), brgen(), bigen(), gegen(), expgen(), gagen(), pogen(), nogen()
#' related functions: expgen


gagen <- function(lambda, k){
  result <- 0
  for (i in 1:k){
    result <- result + expgen(lambda)
  }
  return(result)
}

#section8

#' pogen
#' 
#' If an exponentially distributed variable is modeled as the waiting time before an arrival, the Poisson distributed variable can be modeled as the number of arrivals during a period of time of length t. 
#' 
#' @usage pogen(lambda, t)
#' @param lambda is a float number as the parameter of the underlying exponential distribution
#' @param t is a float as the length of time interval.
#' @author MohammadAmin Salarkia
#' @example pogen(4, 2)
#' @seealso dugen(), cugen(), brgen(), bigen(), gegen(), expgen(), gagen(), pogen(), nogen() 
#' related functions: expgen


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

#' nogen
#' 
#' This function gives you a number which follows normal distribution. It uses poisson distribution function (pogen (10, 10)) to make normal distributed numbers. It is calculated by (x * sqrt(s) / 10) + ( u - 10 * sqrt(s))
#' 
#' @usage nogen(u, s)
#' @param u is the mean
#' @param s is the variance
#' @author Ali Ehteshami
#' @example nogen(10, 4)
#' 
#' @seealso dugen(), cugen(), brgen(), bigen(), gegen(), expgen(), gagen(), pogen()

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
  geom_type <- "density"
  if (isTRUE(all.equal(func, bigen)) || isTRUE(all.equal(func, gegen)))
    geom_type <- "histogram"
  plot <- qplot(y, geom =geom_type, fill=I("blue"),col=I("black"),
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

#section 3.3

find_uiniform <- function(string){
  nums <- as.numeric(unlist(strsplit(string, " ")))
  return(c(1/(max(nums) - min(nums)), min(nums), max(nums))) #P a b
}

find_bernouli <- function(string){
  nums <- as.numeric(unlist(strsplit(string, " ")))
  return(c(mean(nums), 1 - mean(nums))) #P 1-P
}

find_binomial <- function(string){
  nums <- as.numeric(unlist(strsplit(string, " ")))
  n <- nums[1]
  nums <- nums[2:length(nums)]
  return(c(mean(nums) / n, 1 - mean(nums) / n, n)) #P 1-P n
}

find_geometric <- function(string){
  nums <- as.numeric(unlist(strsplit(string, " ")))
  return(c(1 / mean(nums), 1 - 1 / mean(nums))) #P 1-P
}

find_exponential <- function(string){
  nums <- as.numeric(unlist(strsplit(string, " ")))
  return(1 / mean(nums)) #lambda
}

find_gamma <- function(string){
  nums <- as.numeric(unlist(strsplit(string, " ")))
  s <- log(mean(nums)) - mean(log(nums))
  k <- (3 - s + sqrt((s-3)**2 + 24 * s)) / (12 * s)
  return(c(k / mean(nums) , k)) #lambda k
}

find_poisson <- function(string){
  nums <- as.numeric(unlist(strsplit(string, " ")))
  return(mean(nums)) #lambda
}

find_normal <- function(string){
  nums <- as.numeric(unlist(strsplit(string, " ")))
  return(c(mean(nums), var(nums))) #mean var
}
