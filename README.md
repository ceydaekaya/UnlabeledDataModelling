EM algorithm &amp; mixture density functions

PARTA:Develop and test an R function that evaluates a sequence of samples from a mixture of 
normal density functions given values for the proportions, means and variances. Do not use the 
mixtools package in R. Demonstrate that the code works by graphing an example of a mixture of 
three normal densities that show three clear but overlapping density functions.

PART B: R function whose input is the estimate of the proportions, means and variances for a mixture 
model and uses the EM algorithm to get a final estimate. The input should also include a parameter, 
say epsilon, that is used in the stopping rule. The output for the function should be a list containing
all of the model parameters.

PART C: use the function from that generates a graph of the final mixture density and give two examples
where you run your EM code.
