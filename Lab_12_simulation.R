generate_data = function(n,p) 
{
  covariates = matrix(rnorm(n*p, 0, 1), nrow = n, ncol = p)
  responses = rnorm(n, 0, 1)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses, cutoff) 
{
  regress = lm(responses ~ covariates)
  pval = (summary(regress)$coefficients)[-1, 4]
  withinCut = which(pval <= cutoff)
  if (length(withinCut) == 0) 
  {
    return(c())
  }
  regress = lm(responses ~ covariates[, withinCut])
  pval2 = (summary(resgress)$coefficients)[-1, 4]
  return(pval2)
}

run_simulation = function(n_trials, n, p, cutoff) 
{
  pval = vector()
  for (i in 1:n_trials) {
    lst = generate_data(n, p)
    covariates = lst$covariates
    responses = lst$responses
    model = model_select(covariates, responses, cutoff)
    pval = c(p.value, model)
  }
}
run_simulation(100, 100, 10, 0.05)
run_simulation(1000, 1000, 20, 0.05)
run_simulation(10000, 10000, 50, 0.05)

