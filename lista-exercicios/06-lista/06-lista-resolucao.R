
# Arquivo: 06-lista-resolucao.R
# Autor(a): David Picarelli Gonçalves
# Data: 07/05/2026
# Objetivo: Resolução da lista de exercícios 6

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here) # para usar caminhos relativos
library(tidyverse) # meta-pacote que inclui readr, dplyr, tidyr...


# Exercício 1 ---------------------------------------------------------------
  
set.seed(20260507)

# Vetores de retornos possíveis e probabilidades
retornos_possiveis <- c(0.06, 0.02, -0.01, -0.04)
probabilidades <- c(0.15, 0.45, 0.25, 0.15)

# Gerando amostra de 10.000 meses de retornos
retornos <- sample(
  retornos_possiveis, size = 10000, replace = TRUE, prob = probabilidades)

# Calculando estatísticas
valor_esperado <- mean(retornos)
variancia <- var(retornos)
desvio_padrao <- sd(retornos)
prob_retorno_negativo <- mean(retornos < 0)

# Exibindo resultados
cat("Valor Esperado (Média):", round(valor_esperado, 4), "\n")
cat("Variância:", round(variancia, 4), "\n")
cat("Desvio Padrão:", round(desvio_padrao, 4), "\n")
cat("Probabilidade de Retorno Negativo:", round(prob_retorno_negativo, 4), "\n")

# Exercício 2 -------------------------------------------------------------

set.seed(20260508)

lucros_possiveis <- c(900, 150, -3500)
probabilidades <- c(0.88, 0.08, 0.04)

# Parte 1: Simulação de 20.000 operações individuais de crédito
lucros <- sample(
  lucros_possiveis, size = 20000, replace = TRUE, prob = probabilidades)

valor_esperado <- mean(lucros)
desvio_padrao <- sd(lucros)
prob_prejuizo <- mean(lucros < 0)

cat("Parte 1:\n")
cat("Valor esperado por operação:", round(valor_esperado, 2), "\n")
cat("Desvio-padrão:", round(desvio_padrao, 2), "\n")
cat("Probabilidade de prejuízo:", round(prob_prejuizo, 4), "\n\n")

# Parte 2: Simulação de 5.000 carteiras com 80 operações cada
carteiras <- replicate(
  5000, sum(sample(
    lucros_possiveis, size = 80, replace = TRUE, prob = probabilidades)))

valor_esperado_carteira <- mean(carteiras)
esperado_teorico <- 80 * valor_esperado

cat("Parte 2:\n")
cat("Valor esperado da carteira:", round(valor_esperado_carteira, 2), "\n")
cat("Valor esperado teórico (80 * valor médio por operação):", round(
  esperado_teorico, 2), "\n")
cat("Diferença:", round(valor_esperado_carteira - esperado_teorico, 2), "\n")

# Exercício 3 -------------------------------------------------------------

# Definindo a semente para reprodutibilidade
set.seed(20260509)

# Gerando o vetor de estados econômicos para 20.000 meses
estado <- sample(
  c("expansao", "recessao"), size = 20000, replace = TRUE, prob = c(0.7, 0.3))

# Criando o tibble e gerando retornos condicionais com map_dbl
dados <- tibble(estado = estado) %>%
  mutate(
    retorno = map_dbl(estado, ~{
      if (.x == "expansao") {
        sample(c(0.09, 0.04, -0.02), size = 1, prob = c(0.25, 0.50, 0.25))
      } else {
        sample(c(0.03, -0.04, -0.11), size = 1, prob = c(0.15, 0.45, 0.40))
      }
    })
  )

# Valor esperado incondicional
ve_incondicional <- mean(dados$retorno)
cat("Valor esperado incondicional do retorno:", round(
  ve_incondicional, 4), "\n")

# Valor esperado por estado
ve_por_estado <- dados %>%
  group_by(estado) %>%
  summarise(
    valor_esperado = mean(retorno),
    n = n(),
    .groups = "drop"
  )
print(ve_por_estado)

# Probabilidade de retorno negativo
prob_neg <- mean(dados$retorno < 0)
cat("Probabilidade de retorno negativo:", round(prob_neg, 4), "\n")

# Valores teóricos para comparação
ve_exp_teor <- 0.25*0.09 + 0.50*0.04 + 0.25*(-0.02)
ve_rec_teor <- 0.15*0.03 + 0.45*(-0.04) + 0.40*(-0.11)
ve_incond_teor <- 0.7 * ve_exp_teor + 0.3 * ve_rec_teor
prob_neg_exp_teor <- 0.25
prob_neg_rec_teor <- 0.45 + 0.40
prob_neg_teor <- 0.7 * prob_neg_exp_teor + 0.3 * prob_neg_rec_teor

cat("\nValores teóricos:\n")
cat("VE Expansão:", round(ve_exp_teor, 4), "\n")
cat("VE Recessão:", round(ve_rec_teor, 4), "\n")
cat("VE Incondicional:", round(ve_incond_teor, 4), "\n")
cat("Prob. Neg. Expansão:", prob_neg_exp_teor, "\n")
cat("Prob. Neg. Recessão:", prob_neg_rec_teor, "\n")
cat("Prob. Neg. Incondicional:", round(prob_neg_teor, 4), "\n")

# Interpretação e comparação
cat("\nInterpretação e Comparação:\n")
cat("Os valores simulados estão muito próximos dos teóricos, 
    graças ao grande número de simulações (20.000).\n")
cat("O VE incondicional é a média ponderada dos VEs condicionais 
    (70% expansão, 30% recessão).\n")
cat("Na expansão, retornos são mais altos e menos negativos; na recessão, 
    mais baixos e negativos.\n")
cat("Probabilidade de perda é baixa na expansão (25%) e alta na recessão (85%), 
    resultando em ~43% incondicional.\n")

# Exercício 4 -------------------------------------------------------------

# Definir semente para reprodutibilidade
set.seed(20260510)

# Simular 15.000 meses de cenários econômicos
cenario <- sample(c("boom", "estabilidade", "crise"), 
                  size = 15000, 
                  replace = TRUE, 
                  prob = c(0.20, 0.50, 0.30))

# Criar tibble com cenários e calcular retornos RA, RB e RP
simulacao <- tibble(cenario = cenario) %>%
  mutate(
    RA = case_when(
      cenario == "boom" ~ 0.12,
      cenario == "estabilidade" ~ 0.03,
      cenario == "crise" ~ -0.09
    ),
    RB = case_when(
      cenario == "boom" ~ 0.07,
      cenario == "estabilidade" ~ 0.02,
      cenario == "crise" ~ -0.04
    ),
    RP = 0.6 * RA + 0.4 * RB  # Carteira: 60% RA + 40% RB
  )

# Calcular estatísticas solicitadas
media_RA <- mean(simulacao$RA)
dp_RA <- sd(simulacao$RA)
media_RB <- mean(simulacao$RB)
dp_RB <- sd(simulacao$RB)
cov_AB <- cov(simulacao$RA, simulacao$RB)
cor_AB <- cor(simulacao$RA, simulacao$RB)
prob_ambos_neg <- mean(simulacao$RA < 0 & simulacao$RB < 0)
media_RP <- mean(simulacao$RP)
dp_RP <- sd(simulacao$RP)

# Exibir resultados
cat("Média RA:", round(media_RA, 4), "\n")
cat("Desvio Padrão RA:", round(dp_RA, 4), "\n")
cat("Média RB:", round(media_RB, 4), "\n")
cat("Desvio Padrão RB:", round(dp_RB, 4), "\n")
cat("Covariância (RA, RB):", round(cov_AB, 4), "\n")
cat("Correlação (RA, RB):", round(cor_AB, 4), "\n")
cat("Probabilidade (RA < 0 e RB < 0):", round(prob_ambos_neg, 4), "\n")
cat("Média RP:", round(media_RP, 4), "\n")
cat("Desvio Padrão RP:", round(dp_RP, 4), "\n")

# Interpretação da diversificação
cat("\n--- Interpretação da Diversificação ---\n")
cat("DP RA:", round(dp_RA, 4), " | DP RB:", round(
  dp_RB, 4), " | DP RP:", round(dp_RP, 4), "\n")
cat("A diversificação reduz o risco da carteira (DP RP menor que DP RA e DP RB),
    demonstrando o benefício da combinação de ativos correlacionados.\n")

# Exercício 5 -------------------------------------------------------------

set.seed(20260511)

# Função para simular um mês de operações de crédito
simular_mes <- function() {
  # (1) Sorteia o cenário econômico
  cenario <- sample(c("aquecido", "normal", "fraco"), size = 1, 
                    prob = c(0.25, 0.50, 0.25))
  
  # (2) Define número de contratos e probabilidade de inadimplência por cenário
  if (cenario == "aquecido") {
    n_contratos <- 120
    p_inadimplencia <- 0.03
  } else if (cenario == "normal") {
    n_contratos <- 90
    p_inadimplencia <- 0.06
  } else {  # fraco
    n_contratos <- 60
    p_inadimplencia <- 0.10
  }
  
  # (3) Simula o status de cada contrato
  status_contratos <- sample(c("adimplente", "inadimplente"), 
                             size = n_contratos, 
                             prob = c(1 - p_inadimplencia, p_inadimplencia), 
                             replace = TRUE)
  
  # (4) Conta o número de inadimplentes
  n_inadimplentes <- sum(status_contratos == "inadimplente")
  
  # (5) Calcula o lucro: adimplentes * 300 - inadimplentes * 2500
  n_adimplentes <- n_contratos - n_inadimplentes
  lucro <- n_adimplentes * 300 - n_inadimplentes * 2500
  
  # (6) Retorna tibble com os resultados
  tibble(cenario = cenario, 
         n_inadimplentes = n_inadimplentes, 
         lucro = lucro)
}

# Testa a função uma vez
cat("Teste da função simular_mes():\n")
print(simular_mes())

# Simula 10.000 meses
simulacoes <- replicate(10000, simular_mes(), simplify = FALSE) |> 
  bind_rows()

# Cálculos solicitados
media_lucro <- mean(simulacoes$lucro)
desvio_padrao_lucro <- sd(simulacoes$lucro)
lucro_por_cenario <- simulacoes |> 
  group_by(cenario) |> 
  summarise(media_lucro = mean(lucro), .groups = "drop")

covariancia <- cov(simulacoes$n_inadimplentes, simulacoes$lucro)
correlacao <- cor(simulacoes$n_inadimplentes, simulacoes$lucro)
prob_perda <- mean(simulacoes$lucro < 0)

# Exibe os resultados
cat("\nResultados da simulação Monte Carlo (10.000 meses):\n")
cat("Média do lucro: R$", round(media_lucro, 2), "\n")
cat("Desvio padrão do lucro: R$", round(desvio_padrao_lucro, 2), "\n")
cat("\nLucro médio por cenário:\n")
print(lucro_por_cenario)
cat("Covariância (n_inadimplentes, lucro):", round(covariancia, 2), "\n")
cat("Correlação (n_inadimplentes, lucro):", round(correlacao, 4), "\n")
cat("Probabilidade de prejuízo (lucro < 0):", round(prob_perda * 100, 2), "%\n")

# Interpretação da covariância e correlação
cat("\nInterpretação:\n")
cat("- A covariância negativa (", round(covariancia, 2), ") 
    indica que meses com mais inadimplentes tendem a ter lucros menores.\n")
cat("- A correlação negativa (", round(correlacao, 4), ") 
    confirma uma relação inversa forte entre o número de inadimplentes e o 
    lucro.\n")