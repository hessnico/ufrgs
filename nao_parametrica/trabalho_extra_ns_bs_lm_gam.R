library(ISLR)
library(gam)
library(tidyverse)

dados <- College

calcula_significancia <- function(independent, dependent) {
  fit0 <- lm(dependent ~ independent)
  fit1 <- lm(dependent ~ poly(independent, 2))
  fit2 <- lm(dependent ~ poly(independent, 3))
  fit3 <- lm(dependent ~ poly(independent, 4))
  fit4 <- lm(dependent ~ poly(independent, 5))
  return(anova(fit0, fit1, fit2, fit3, fit4))
}

# para variavel tivemos ate o polinomio de grau 4 para o Accept
calcula_significancia(independent = dados$Accept, dependent = dados$Apps)

#segundo e quarto grau
calcula_significancia(independent = dados$Enroll, dependent = dados$Apps)

# segundo, terceiro e quarto grau
calcula_significancia(independent = dados$Outstate, dependent = dados$Apps)

#segundo e quarto
calcula_significancia(independent = dados$F.Undergrad, dependent = dados$Apps)

#segundo, terceiro e quinto grau
calcula_significancia(independent = dados$P.Undergrad, dependent = dados$Apps)

# teremos nosso modelo 
## variaveis: Private, Accept, Enroll, Outstate, F.Undergrad, P.Undergrad
lm_fitted_model <- lm(Apps ~ Private 
                         + poly(Accept, 2) + poly(Accept, 3) + poly(Accept, 4)
                         + poly(Enroll, 2) + poly(Enroll, 4)
                         + poly(Outstate, 2)
                         + poly(F.Undergrad, 2) + poly(F.Undergrad, 4) 
                         + poly(P.Undergrad, 2)
                         ,data = dados)

# summarydo primeiro modelo
summary(lm_fitted_model)
par(mfrow = c(2,2))
plot(lm_fitted_model)

#### Modelo com Basic Splines 
basic_spline_fitted_model <- lm(Apps ~ Private
                                + bs(Accept, df = 6)
                                + bs(Enroll, df = 4)
                                + bs(Outstate, df = 4)
                                + bs(F.Undergrad, df = 4) 
                                + bs(P.Undergrad, df= 4)
                                , data = dados)
summary(basic_spline_fitted_model)
par(mfrow = c(2,2))
plot(basic_spline_fitted_model)

#### Modelo com Natural Spline
natural_spline_fitted_model <- lm(Apps ~ Private
                                + ns(Accept, df = 10)
                                + ns(Enroll, df = 8)
                                + ns(Outstate, df = 5)
                                + ns(F.Undergrad, df = 10) 
                                + ns(P.Undergrad, df= 10)
                                , data = dados)
summary(natural_spline_fitted_model)
par(mfrow = c(2,2))
plot(natural_spline_fitted_model)


#### modelo com GAM
gam_fitted_model <- lm(Apps ~ s(Accept, 4) + 
                         Private
                        , data = dados)
summary(gam_fitted_model)
par(mfrow = c(2,2))
plot(gam_fitted_model)

### 
bic <- function(model) {
  log_lik <- logLik(model)
  n <- length(model$residuals)
  return((-1)*log_lik[1]+1/2*log(n)*length(model$coefficients))
}

aic <- function(model) {
  log_lik <- logLik(model)
  n <- length(model$residuals)
  return((-1)*log_lik[1]+length(model$coefficients))
}

hq <- function(model) {
  log_lik <- logLik(model)
  n <- length(model$residuals)
  return((-1)*log_lik[1]+ log(log(n))*length(model$coefficients))
} 

aic_corrigido <- function(model) {
  aic_t <- aic(model)
  p = length(model$coefficients)
  n = length(model$residuals)
  return(aic_t + (2*(p+1)*(p+2))/n - p - 2)
}

model_AIC <- c(aic(lm_fitted_model),
               aic(basic_spline_fitted_model),
               aic(natural_spline_fitted_model),
               aic(gam_fitted_model))

model_BIC <- c(bic(lm_fitted_model),
               bic(basic_spline_fitted_model),
               bic(natural_spline_fitted_model),
               bic(gam_fitted_model))

model_HQ <- c(hq(lm_fitted_model),
              hq(basic_spline_fitted_model),
              hq(natural_spline_fitted_model),
              hq(gam_fitted_model))

model_aic_c <- c(aic_corrigido(lm_fitted_model),
                 aic_corrigido(basic_spline_fitted_model),
                 aic_corrigido(natural_spline_fitted_model),
                 aic_corrigido(gam_fitted_model))