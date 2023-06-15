# 

dados <- read.csv("T1_q3.csv", header = TRUE)

x <- dados$x
y <- dados$y
x0 <- seq(from = 0.1, to = 0.9, by = 0.05)

k_epa <- function(x) {
  0.75 * (1 - x^2) * (x > -1) * (x < 1)
}

dentro <- function(x, y, x0, h, k = k_epa) {
  t <- (x - x0) / h
  v_k <- k(t)
  v_t <- v_k * y
  
  soma_cima <- sum(v_t) 
  soma_baixo <- sum(v_k)
  
  return(soma_cima / soma_baixo)
}

valor = dentro(x, y, x0 = 0.7, h = 0.05)
round(abs(valor), 3) * 1000

vv <- 0

for (i in 1:length(x0)) {
  vv <- vv + sum(dentro(x, y, x0 = x0[i], h = 0.05)) 
}

round(abs(vv/length(x0)), 3) * 1000

