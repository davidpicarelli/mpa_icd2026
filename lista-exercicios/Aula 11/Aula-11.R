# Arquivo: 11-aula.R
# Autor(a): David Picarelli Gonçalves
# Data: 21/05/2026
# Objetivo: Aula 11

# fixa a semente 
set.seed(42)

# simula U ~ Uniforme(0,1)
u  <- runif(10000)

# define o parâmetro da Exponencial
lambda <- 2

# gera X ~ Exponencial(lambda)
x <- -log(u) / lambda    

# --- 3. Histograma da amostra simulada ------------------------------------
hist(x,
     breaks      = 30,           # número de classes do histograma
     col         = "orange",     # cor das barras
     probability = TRUE,         # escala em densidade (área total = 1)
     main = expression("Exponencial(" * lambda == 2 * ") via U(0,1)"),
     xlab = "x",
     ylab = "densidade")


# --- 4. Densidade teórica sobreposta --------------------------------------
curve(dexp(x, rate = lambda),    # f(x) = lambda * exp(-lambda * x)
      add = TRUE,                # adiciona ao gráfico existente
      col = "blue", lwd = 2)

# --- 5. Legenda -----------------------------------------------------------
legend("topright",
       legend = c("Histograma simulado", "Densidade teórica"),
       col    = c("orange", "blue"),
       lwd    = c(8, 2),
       lty    = 1,
       bty    = "n")
