f_nr <- function(
    par, # par chute inicial (muito importante)
    f1, # f1 função que retorna vetor score
    f2, # f2 função que retorna matriz hessiana
    f_obj, # função objetivo
    mini = T, # TRUE p/ minimização; FALSE p/ maximização
    r = 1, # step size
    eps = 1 / 10000, # critério de convergência
    ... # demais argumentos das funções f1, f2, f_obj.
){
  cc <- 1
  conta <- 0 # contador
  vx <- par
  
  while (cc > eps) {
    par1 <- par - r * solve(f2(par, ...)) %*% f1(par, ...)
    cc <- t(f1(par1, ...)) %*% f1(par1, ...)
    
    cd <- f_obj(par1, ...) - f_obj(par, ...)
    
    if ((mini && cd > 0) || (!mini && cd < 0)) {
      mensagem_erro <- paste(
        "algoritmo na direção errada - tentar r < ",
        r,
        sep = ""
      )
      stop(mensagem_erro)
    }
    
    par <- par1
    conta <- conta + 1
    vx <- cbind(vx, par1)
  }
  
  ## Verificação das CSO ##
  
  hess <- f2(par, ...)
  nh <- ncol(hess)
  det.sign <- sign(hess[1, 1])
  
  for (i in 2:nh) {
    det.aux <- det(hess[1:i, 1:i])
    det.sign[i] <- sign(det.aux)
  }
  
  return(
    list(
      ponto.otimo = par,
      num.iteracoes = conta,
      trajetoria = vx,
      Hess.sign = det.sign,
      Hessiano = hess
    )
  )
}

f_bhhh <- function(
    par, # chute inicial
    f1, # função que retorna a matriz com linha i igual ao vetor score i
    f_obj, # função objetivo
    mini = T, # TRUE p/ minimização; FALSE p/ maximização
    r = 1, # step size
    eps = 1 / 10000, # critério de convergência
    ... # demais argumentos das funções f1, f2, f_obj.
){
  fa <- (-1)^mini # troca para + quando for maximização
  cc <- 1
  conta <- 0 # contador
  vx <- par
  uns <- matrix(
    data = 1,
    ncol = 1,
    nrow = nrow(f1(par, ...))
  )
  
  while (cc > eps) {
    aux_S <- t(f1(par, ...)) %*% uns
    
    par1 <- par + fa * r * solve(t(f1(par, ...)) %*% f1(par, ...)) %*% aux_S
    
    aux_S1 <- t(f1(par1, ...)) %*% uns
    
    cc <- t(aux_S1) %*% aux_S1
    
    cd <- f_obj(par1, ...) - f_obj(par, ...)
    if ((mini && cd > 0) || (!mini && cd < 0)) {
      mensagem_erro <- paste(
        "algoritmo na direção errada - tentar r < ",
        r,
        sep = ""
      )
      stop(mensagem_erro)
    }
    
    par <- par1
    conta <- conta + 1
    vx <- cbind(vx, par1)
  }
  
  return(
    list(
      ponto.otimo = par,
      num.iteracoes = conta,
      trajetoria = vx
    )
  )
}

f_deriv <- function(
    expr, # expressão a ser diferenciada
    var # variáveis com respeito às quais se quer diferenciar
){
  d_aux_f1 <- list()
  
  for (i in seq_along(var)) {
    d_aux_f1[[i]] <- D(expr, var[i])
  }
  
  d_aux_f2 <- list()
  ind_f2 <- 0
  
  for (i in seq_along(var)) {
    for (j in seq_along(var)) {
      ind_f2 <- ind_f2 + 1
      d_aux_f2[[ind_f2]] <- D(d_aux_f1[[i]], var[j])
    }
  }
  
  return(
    list(
      der_primeira = d_aux_f1,
      der_segunda = d_aux_f2
    )
  )
}

### exercicio 

set.seed(1234)

n <- 500
alfa_ver <- 1
beta_ver <- 2

x <- rgamma(n = n, shape = alfa_ver, rate = beta_ver)

f_obj_ex1 <- function(z) {
  a <- z[1]
  b <- z[2]
  n <- length(x)
  res <- sum(
    a * log(b) - log(gamma(a)) + (a - 1) * log(x) - b * x
  )
  return(res)
}

expr <- expression(
  a * log(b) - log(gamma(a)) + (a - 1) * log(x) - b * x
)

aux_deriv <- f_deriv(
  expr = expr,
  var = c("a", "b")
)

print(aux_deriv)

D(
  expr = expression(log(b) - digamma(a) + log(x)),
  name = "a"
)

D(
  expr = expression(log(b) - digamma(a) + log(x)),
  name = "b"
)

D(
  expr = expression(a * (1/b) - x),
  name = "a"
)

D(
  expr = expression(a * (1/b) - x),
  name = "b"
)

f1 <- function(z) {
  a <- z[1]
  b <- z[2]
  
  res <- rbind(
    sum(log(b) - digamma(a) + log(x)),
    sum(a * (1 / b) - x)
  )
  
  return(res)
}

f2 <- function(z) {
  a <- z[1]
  b <- z[2]
  
  res <- matrix(NA, 2, 2)
  
  n <- length(x)
  
  res[1, 1] <- -n * trigamma(a)
  res[1, 2] <- res[2, 1] <- n * (1 / b)
  res[2, 2] <- -n * (a * (1 / b^2))
  
  return(res)
}
chute_ex1 <- c(
  alfa = mean(x)^2 / var(x),
  beta = mean(x) / var(x)
)

est_nr_ex1 <- f_nr(
  par = chute_ex1,
  f1 = f1,
  f2 = f2,
  f_obj = f_obj_ex1,
  mini = FALSE,
  r = 1,
  eps = 1 / 10000
)

print(est_nr_ex1)

### com BHHH

f1_bhhh_ex1 <- function(z) {
  a <- z[1]
  b <- z[2]
  
  res <- cbind(
    log(b) - digamma(a) + log(x),
    a * (1 / b) - x
  )
  
  return(res)
}

est_bhhh_ex1 <- f_bhhh(
  par = chute_ex1,
  f1 = f1_bhhh_ex1,
  f_obj = f_obj_ex1,
  mini = FALSE,
  r = 1,
  eps = 1 / 10000
)

print(est_bhhh_ex1)

###############################################################################

  # NLS - nonlinear least squares with gauss newton method 

#### DGP ####
library(mvtnorm)
library(knitr)

set.seed(12345)
n <- 400
b <- c(1, 1.5, -1.5)
x <- rmvnorm(
  n = n,
  mean = c(0, 0),
  sigma = matrix(c(1, 0.2, 0.2, 1), 2, 2)
)
x <- cbind(1, x)

sig.u <- 1
u <- rnorm(n, 0, sig.u)
y <- b[1] + b[2] * x[, 2] + b[3] * x[, 3] + u

f1_reg <- function(b, x, y) {
  # aqui `b` inclui todos os parâmetros, inclusive sig^2.
  y <- as.matrix(y)
  b1 <- as.matrix(b[-length(b)])
  sig2 <- b[length(b)]
  res <- y - x %*% b1
  
  ## Versão sem loop ##
  
  aux_1 <- t(x) %*% res / sig2
  aux_2 <- sum(-1 / (2 * sig2) + res^2 / (2 * sig2^2))
  S1 <- as.matrix(c(aux_1, aux_2))
  
  ## Resultados ##
  row.names(S1) <- c(paste("b", 1:(length(b) - 1), sep = ""), "sig2")
  return(S1)
}

f2_reg <- function(b, x, y) {
  # aqui `b` inclui todos os parâmetros, inclusive sig^2.
  
  b1 <- as.matrix(b[-length(b)])
  sig2 <- b[length(b)]
  res <- y - x %*% b1
  
  ## Versão sem loop ##
  
  H1 <- matrix(NA, 2, 2)
  
  aux_0 <- -t(x) %*% x / sig2
  aux_1 <- -t(x) %*% res / sig2^2
  aux_2 <- sum((1 / (2 * sig2^2) - res^2 / sig2^3))
  
  # construindo a matriz Hessiana
  
  H1 <- cbind(aux_0, aux_1)
  H1 <- rbind(H1, c(aux_1, aux_2))
  
  row.names(H1) <-
    colnames(H1) <- c(paste("b", 1:(length(b) - 1), sep = ""), "sig2")
  
  ## Retornando o Hessiano ##
  
  return(H1)
}

f_obj_reg <- function(b, x, y) {
  p <- length(b)
  b1 <- as.matrix(b[-p])
  sig2 <- b[p]
  res <- y - x %*% b1
  sum(log(dnorm(res, mean = 0, sd = sqrt(sig2))))
}
