
logitdat <- function(vars = 1, weights, n = 1000, bias=0, fixed_seed=666) {
  if(!is.integer(fixed_seed)){
    set.seed(666)
  } else {
    set.seed(fixed_seed)
  }
  
  if(vars < 1){
    return(NULL)
  }
  if(length(weights) != vars){
    weights <- rep(1, vars)
  }
  
  data <- data.frame(x1=rnorm(n))
  
  for(i in 1:vars){
    data[paste0('x', toString(i))] <- rnorm(n)
  }
  
  z <- bias + rowSums(t(t(as.matrix(data)) * weights)) # linear combination with a bias
  pr <- 1/(1 + exp(-z))  # pass through an inv-logit function
  data['y'] <- rbinom(n, 1, pr)  # bernoulli response variable
  data
}