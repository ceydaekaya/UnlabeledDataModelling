rnormmix<-function(nsize,p,mu,sigma){#to activate rnormmix function
  random_mixture<-rep(0,nsize)
  for (z in 1:nsize){
    index<-which(1 == rmultinom(1,1,p))
    arandom<-rnorm(1,mu[index],sigma[index])
    random_mixture[z]<-arandom
  }
  return(random_mixture)
}

evaluate_mix <- function(sample, p, mu, sigma2){#variance sigma square
  
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

em_alg<-function(sample, p, mu, sigma2){
  n<-length(sample)
  m<-length(p)
  p_epr<-rep(0,m) #values to be compared w/ epsilon
  mu_epr<-rep(0,m)
  sigma2_epr<-rep(0,m)
  
  #sigma <- sqrt(sigma2_ep[i])#standard deviation
  ri<- matrix(0, m, n)#responsibility
  
  for(i in 1:m){
    mu_ep<-mu[i]
    p_ep<-p[i]
    sigma2_ep<-sigma2[i]
    ri[i]<-p_ep*exp(-((sample-mu_ep)/sigma2_ep)^2/2)/(sqrt(2*pi)*sigma2_ep)
  }
  
  sums<-colSums(ri)
  for(i in 1:m){#from class notes
    ri_norm<-ri[i]/sums
    
    p_epr[i] <- sum(ri_norm)/n #next estimates
    mu_epr[i] <- sum(ri_norm*sample)/sum(ri_norm)
    sigma2_epr[i] <- sum((ri_norm*(sample-mu_ep[i])^2))/sum(ri_norm)
  }
  
  single_array<-c(p_epr, mu_epr, sigma2_epr)
  return(single_array)
}


EM<-function(sample, p, mu, sigma2, epsilon=0.0001){
  
  epsilon0=0.1#initial value for test
  m<-length(p)
  sigma<-sqrt(sigma2)
  while(epsilon0>epsilon){
    #updating vars until the result is small enough
    old_vars<-c(p,mu,sigma2)
    new_vars<-em_alg(sample, p, mu, sigma2)
    epsilon0<-norm(matrix(new_vars-old_vars, m*3, 1))
    
    #vars = p...p (until mth index), mu...mu, sigma2...sigma2
    p<-new_vars[1:m]
    mu<-new_vars[(m+1):(m*2)]
    sigma2<-new_vars[(m*2+1):(m*3)]
    
  }
  return(new_vars)
}

p <- c(.3,.4,.3)#it need to add up to 1
mu <- c(1,5,10)
sigma <- c(1,1,2)
sigma2 <- c(1,1,4)

sample_test <- rnormmix(10000, p, mu, sigma) #function from
vars <- evaluate_mix(sample_test, p , mu, sigma2)
plot(sample_test, vars)

p <- c(.3,.4,.3)#it need to add up to 1
mu <- c(1,5,10)
sigma <- c(1,1,2)
sigma2 <- c(1,1,4)
ri<-EM(sample_test, p, mu, sigma2, 0.0001)
