p <- c(.3,.4,.3)#it need to add up to 1
mu <- c(1.2,6,12)
sigma <- c(1,1,2)
sigma2 <- c(1,1,4)

sample_test <- rnormmix(10000, p, mu, sigma) #function from
vars <- evaluate_mix(sample_test, p , mu, sigma2)
plot(sample_test, vars)

p <- c(.7,.2,.1)#it need to add up to 1
mu <- c(1,5,10)
sigma <- c(2,1,2)
sigma2 <- c(4,1,4)
ri<-EM(sample_test, p, mu, sigma2, 0.001)


