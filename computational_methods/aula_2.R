### Aula_2

f1 <- function(x) {
  return(3*x^2+ 2*x)
}

f_trapezio <- function(x, h) {
  x <- sort(x)
  n <- length(x)
  
  return(0.5*sum(((x[-1]-x[-n])*(h(x[-n])+h(x[-1])))))
}

int.ex1 <- f_trapezio(
  x = seq(from = -1, to = 1, length = 10000),
  h = f1
)
print(int.ex1)

# estimador não-paramétrico de densidade (kernel)

np_density <- function(x, x_0, h, k){
  n <- length(x)
  tmp <-(x-x_0) / h
  return(sum(k(tmp))/(n*h))
}

k_func <- function(x) {
  0.75 * (1 - x^2) * (x > -1) * (x < 1)
}

set.seed(1234)

n <- 300
x_n <- rnorm(n, 0, 1)
h <- 2.7 * sd(x_n) * n^(-1 / 5)
x0 <- 1
f.hat <- np_density(x = x_n, x_0 = x0, h = h, k = k_func)

print(f.hat)