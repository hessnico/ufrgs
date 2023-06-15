# q1 

f_trapezoidal <- function(x, f_x) {
  x <- sort(x)
  n <- length(x)
  res = 1/2*((x[-1] - x[-n])*(f_x(x[-n])+f_x(x[-1])))
  
  return(sum(res))
}

f_2 <- function(x) {
  return(x*sin(x^2))
}

f_3 <- function(x) {
  return(x/log(x))
}

x1 <- seq(from = 2, to = 4, length.out = 1000)

f_trapezoidal(x = x1, f_x = f_3)

#q4 

lambda <- 3
n = 500
x <- rexp(lambda, n = n)

exp_density_func <- function(la, x) {
  return(la*exp(-la*x) * (x>0) * (la > 0))
}

log_vero_func <- function(la, x) {
  return(1/n*sum(log(exp_density_func(la = la, x = x))))
}

la_range <- seq(0.1, 6, length.out = 100)

ll <- rep(NA, length(la_range))
for (i in 1:length(ll)) {
  ll[i] <- log_vero_func(la_range[i], x)
}

plot(y = ll, x = la_range)
