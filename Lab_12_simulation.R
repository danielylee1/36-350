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

