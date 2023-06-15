# regra trapezoidal 

f_1 <- function(x) {
  return(3*(x^2) + 2*x)
}

f_trapezoidal <- function(x, f_x) {
  x <- sort(x)
  n <- length(x)
  res = 1/2*((x[-1] - x[-n])*(f_x(x[-n])+f_x(x[-1])))
  
  return(sum(res))
}

### testando
int_ex1 <- f_trapezoidal(
  x = seq(from = -1, to = 1, length = 10000),
  f_x = f_1
)

# kernel nao parametrico

epa_kernel <- function(x) {
  return(0.75*(1-x^2) * (x>-1) * (x<1)) 
}

f_hat_x_0 <- function(x, x_0, h, k) {
  kernel_input <- (x - x_0) / h 
  res = sum(k(kernel_input))
  n <- length(x)
  return(res/(n*h))
}

### testando 
set.seed(1234)

n <- 300
x <- rnorm(n, 0, 1)
h <- 2.7 * sd(x) * n^(-1 / 5)
x0 <- 1

print(f_hat_x_0(x, x_0, h, k=epa_kernel))

print(dnorm(x_0))


# sendo x_0 um vetor de pontos

f_hat_x_0_grid <- function(x, x_grid, h, k) {
  n <- length(x)
  grid_s <- length(x_grid)
  
  arr <- rep(NA, grid_s)
  
  for (i in 1:grid_s) {
    arr[i] <- f_hat_x_0(x, x_0 = x_grid[i], h, k)
  }
  
  return(arr)
}

### testando
set.seed(1234)
n <- 300
x <- rnorm(300, 0, 1)
h <- 2.7 * sd(x) * n^(-1 / 5)

x0 <- seq(from = -3, to = 3, length = 100)
f.hat <- f_hat_x_0_grid(x, x0, h, epa_kernel)

dnorm(x0[20:25])
f.hat[20:25]


















