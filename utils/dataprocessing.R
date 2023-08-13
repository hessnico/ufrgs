grid_maker = function(variable) {
  lims = range(variable)
  return(seq(from = lims[1], to = lims[2], length.out = length(variable)))
}

bic <- function(model) {
  log_lik <- logLik(model)
  n <- length(model$residuals)
  return((-1)*log_lik[1]+1/2*log(n)*length(model$coefficients))
}

aic <- function(model) {
  log_lik <- logLik(model)
  n <- length(model$residuals)
  return((-1)*log_lik[1]+length(model$coefficients))
           #log(n)*length(model$coefficients))
}

hq <- function(model) {
  log_lik <- logLik(model)
  n <- length(model$residuals)
  return((-1)*log_lik[1]+ log(log(n)*length(model$coefficients)))
           #log(n)*length(model$coefficients))
} 

aic_corrigido <- function(model) {
  aic_t <- aic(model)
  p = length(model$coefficients)
  n = length(model$residuals)
  return(aic_t + (2*(p+1)*(p+2))/n - p - 2)
}
