# Arquivo: 06-lista-resolucao.R
# Autor(a): David Picarelli Gonçalves
# Data: 14/05/2026
# Objetivo: Resolução da lista de exercícios 6

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)
library(tidyverse)




# Função para implementar o LCG
lcg <- function(seed, a, c, m, n) {
  # Parâmetros:
  # seed: valor inicial (x0)
  # a: multiplicador
  # c: incremento
  # m: módulo
  # n: número de iterações
  
  # Inicializar vetores
  x <- numeric(n + 1)
  u <- numeric(n)
  
  # Definir seed
  x[1] <- seed
  
  # Gerar sequência
  for (i in 1:n) {
    x[i + 1] <- (a * x[i] + c) %% m
    u[i] <- x[i + 1] / m
  }
  
  # Retornar tibble com resultados
  tibble(
    iteracao = 1:n,
    x = x[2:(n + 1)],
    u = u
  )
}

# Exemplo 1: LCG com parâmetros clássicos
# Parâmetros: a=1103515245, c=12345, m=2^31
resultado1 <- lcg(seed = 1, a = 1103515245, c = 12345, m = 2^31, n = 10)

cat("Exemplo 1: LCG com parâmetros clássicos\n")
print(resultado1)

# Exemplo 2: LCG com parâmetros simples (para fins pedagógicos)
# Parâmetros: a=5, c=1, m=16
resultado2 <- lcg(seed = 3, a = 5, c = 1, m = 16, n = 10)

cat("\nExemplo 2: LCG com parâmetros simples (a=5, c=1, m=16)\n")
print(resultado2)

# Análise: Observar o ciclo (período) da sequência
cat("\nAnálise do Ciclo:\n")
cat("No Exemplo 2, note que a sequência se repete após alguns passos.\n")
cat("Isso demonstra o comportamento periódico do LCG (período finito).\n")



# Função para aproximar integral via Monte Carlo
monte_carlo_integral <- function(f, a, b, N, seed = NULL) {
  # Parâmetros:
  # f: função a ser integrada
  # a, b: limites de integração
  # N: número de amostras
  # seed: semente para reprodutibilidade
  
  if (!is.null(seed)) set.seed(seed)
  
  # Gerar N amostras uniformes em [a, b]
  u <- runif(N, min = a, max = b)
  
  # Calcular f(u) para cada amostra
  f_u <- f(u)
  
  # Aproximação de Monte Carlo: (b-a) * média(f(u))
  integral_aprox <- (b - a) * mean(f_u)
  
  # Retornar resultado
  return(integral_aprox)
}

# ===== INTEGRAL 1: ∫_1^3 x² dx =====
# Valor exato: [x³/3]_1^3 = 9 - 1/3 = 26/3 ≈ 8.667

f1 <- function(x) x^2
a1 <- 1
b1 <- 3
N <- 1000

# Aproximação Monte Carlo
aprox1 <- monte_carlo_integral(f1, a1, b1, N, seed = 20260514)

# Valor exato via integrate()
exato1 <- integrate(f1, a1, b1)$value

# Erro absoluto
erro1 <- abs(aprox1 - exato1)

cat("===== INTEGRAL 1: ∫_1^3 x² dx =====\n")
cat("Aproximação Monte Carlo (N=1000):", round(aprox1, 6), "\n")
cat("Valor exato (integrate()):", round(exato1, 6), "\n")
cat("Erro absoluto:", round(erro1, 6), "\n")
cat("Erro relativo (%):", round((erro1 / exato1) * 100, 2), "%\n\n")

# ===== INTEGRAL 2: ∫_0^π sin(x) dx =====
# Valor exato: [-cos(x)]_0^π = -(-1 - 1) = 2

f2 <- function(x) sin(x)
a2 <- 0
b2 <- pi
N <- 1000

# Aproximação Monte Carlo
aprox2 <- monte_carlo_integral(f2, a2, b2, N, seed = 20260514)

# Valor exato via integrate()
exato2 <- integrate(f2, a2, b2)$value

# Erro absoluto
erro2 <- abs(aprox2 - exato2)

cat("===== INTEGRAL 2: ∫_0^π sin(x) dx =====\n")
cat("Aproximação Monte Carlo (N=1000):", round(aprox2, 6), "\n")
cat("Valor exato (integrate()):", round(exato2, 6), "\n")
cat("Erro absoluto:", round(erro2, 6), "\n")
cat("Erro relativo (%):", round((erro2 / exato2) * 100, 2), "%\n\n")

# ===== ANÁLISE DE CONVERGÊNCIA =====
# Testar com diferentes tamanhos de amostra

tamanhos <- c(100, 500, 1000, 5000, 10000)
resultados <- tibble(
  N = tamanhos,
  Aprox_Int1 = NA_real_,
  Erro_Int1 = NA_real_,
  Aprox_Int2 = NA_real_,
  Erro_Int2 = NA_real_
)

for (i in seq_along(tamanhos)) {
  n <- tamanhos[i]
  
  aprox_i1 <- monte_carlo_integral(f1, a1, b1, n, seed = 20260514)
  erro_i1 <- abs(aprox_i1 - exato1)
  
  aprox_i2 <- monte_carlo_integral(f2, a2, b2, n, seed = 20260514)
  erro_i2 <- abs(aprox_i2 - exato2)
  
  resultados$Aprox_Int1[i] <- aprox_i1
  resultados$Erro_Int1[i] <- erro_i1
  resultados$Aprox_Int2[i] <- aprox_i2
  resultados$Erro_Int2[i] <- erro_i2
}

cat("===== ANÁLISE DE CONVERGÊNCIA =====\n")
cat("Integral 1: ∫_1^3 x² dx (Valor exato: 8.667)\n")
print(resultados %>% select(N, Aprox_Int1, Erro_Int1))

cat("\nIntegral 2: ∫_0^π sin(x) dx (Valor exato: 2.000)\n")
print(resultados %>% select(N, Aprox_Int2, Erro_Int2))

cat("\nObservação: Conforme N aumenta, o erro diminui, demonstrando a convergência do método de Monte Carlo.\n")