# Doze cartelas numeradas de 1 a 12 são misturadas numa urna. 
# Duas cartelas (X,Y) são extraídas da urna sucessivamente e sem reposição.

# Crie uma variável W que assume valor 1 se X+Y é ímpar e 0 caso contrário.
# Qual a probabilidade de que a soma de X+Y seja um número ímpar?
set.seed(1234)

x <- 1:12

sorteio <- sample(x, size = 2, replace = FALSE)

w <- sum(sorteio) %% 2 

## estimando a probabilidade da soma de X+Y ser um número impar 

x_squared <- 2000

w <- rep(NA, x_squared)
mean_for_plotting <- rep(NA, x_squared)

for (i in 1:x_squared) {
  sorteio <- sample(x, size = 2, replace = FALSE)
  
  w[i] = sum(sorteio) %% 2
  mean_for_plotting[i] <- mean(w)
}

mean(w)
plot(mean_for_plotting, type = "l")