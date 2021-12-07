rnormmix<-function(nsize,p,mu,sigma){
  random_mixture<-rep(0,nsize)
  for (z in 1:nsize){
    index<-which(1 == rmultinom(1,1,p))
    arandom<-rnorm(1,mu[index],sigma[index])
    random_mixture[z]<-arandom
  }
  return(random_mixture)
}

evaluate_mixture <- function(sample, p, mu, sigma2){#variance sigma square
  n <- length(sample)
  m <- length(p)
  
  fx <- rep(0,n)
  
  for (i in 1:m) {
    mu_i <- mu[i]
    p_i <- p[i]
    sigma <- sqrt(sigma2[i])#standard deviation
    
    fx = fx+p_i*(1/sqrt(2*pi*sigma2[i]))*exp(-(0.5)*((sample-mu_i)^2/sigma2[i]))#*(1/sqrt(2*pi*sigma2))
  }
  
  return(fx)
}

p <- c(.3,.4,.3)#it need to add up to 1
mu <- c(1,5,10)
sigma <- c(1,1,2)
sigma2 <- c(1,1,4)

sample_test <- rnormmix(10000, p, mu, sigma)
vars <- evaluate_mixture(sample_test, p , mu, sigma2)
plot(sample_test, vars)
