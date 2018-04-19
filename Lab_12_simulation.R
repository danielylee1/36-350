generate_data = function(n, p){
  covariate = matrix(data = rnorm(n*p, mean=0, sd=1), nrows = n, ncols = p)
  response = rnorm(n, 0, 1)
  return(list(covariate = covariate, response = response))
}
