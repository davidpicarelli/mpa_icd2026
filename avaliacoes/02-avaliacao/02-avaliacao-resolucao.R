# Arquivo: 02-avaliacao-resolucao.R
# Autor(a): David Picarelli Gonçalves
# Data: 11/06/2026
# Objetivo: Resolução da Avaliação 2 - Introdução à Ciência de Dados


# Configurações globais  ----------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# Carrega os pacotes necessários
library(tidyverse) # dplyr, purrr::pmap_dbl() etc.
library(EnvStats)  # distribuição triangular: rtri()
library(tidyquant) # tq_get()
library(xts)
library(PerformanceAnalytics)

# fixa a semente para reprodutibilidade
set.seed(2026)

# número de cenários simulados
n_sim <- 20000

# duração do projeto, em anos (conhecida)
duracao <- 5

# Resolução da Questão 1 ----------------------------------------
# (a.1) Simula os parametros incertos de cada cenário, em R$ mil.
cenarios <- tibble (
  cenario = seq_len(n_sim),
  investimento = rtri(n_sim, min = 850, max = 1200, mode = 1000),
  receita_anual = rtri(n_sim, min = 230, max = 350, mode = 290),
  valor_residual = rtri(n_sim, min = 100, max = 200, mode = 150),
  taxa_desconto = rtri(n_sim, min = 0.11, max = 0.15, mode = 0.13)
)

# (a.2) funcao que calcula o VPL de UM cenario
# traduza a fórmula do enunciado
calcular_vpl <- function(investimento, receita_anual, valor_residual,
                         taxa_desconto, duracao = 5){
 
  # cria o vetor 1, 2, ..., duracao
  anos <- seq_len(duracao)
  
  # valor presente das receitas (constantes) ao longo dos anos
  vp_receitas <- sum(receita_anual / (1 + taxa_desconto)^anos)
  
  # valor presente do valor residual, recebido no final
  vp_residual <- valor_residual / (1 + taxa_desconto)^duracao
  
  # VPL do cenario (combine os três componentes acima)
  vp_receitas + vp_residual - investimento
}

# (a.3) aplique a função a todos os cenarios com
# a funçao apropriada do pacote purrr e armazene em 'vpl'
simulacoes <- cenarios |>
  mutate(
    vpl = pmap_dbl (
      .l = list(
        investimento = investimento,
        receita_anual = receita_anual,
        valor_residual = valor_residual,
        taxa_desconto = taxa_desconto
      ),
      #nome da função a aplicar
      .f = calcular_vpl
    )
  )

# vetor de PLs simulados (use-os nos itens b, c e d)
vpl_sim <- simulacoes$vpl

# (b) probabilidade de o projeto destruir valor: P(VPL < 0).
prob_vpl_neg <- mean(vpl_sim < 0)

#exibe a probabilidade em percentual
prob_vpl_neg * 100

# Interpretação (item b): escreva aqui sua resposta,
# como comentario.
# 23.21

# (c) VPL determinístico (use as MODAS de cada parametro)
# e comparacao.
vpl_det <- calcular_vpl(
  investimento = 1000,
  receita_anual = 290,
  valor_residual = 150,
  taxa_desconto = 0.13
)

# media dos VPLs simulados (compare com o VPL deterministico)
vpl_medio <- mean(vpl_sim)

# desvio-padrao dos VPLs simulados
vpl_desvio <- sd(vpl_sim)

#exibe os valores para comparacao
vpl_det
vpl_medio
vpl_desvio

# interpretacao (intem c): escreva aqui sua resposta,
# como comentario
#  101.41 85.959 114.43

# (d) Histograma da distribuição simulada
hist(vpl_sim,
     breaks = 50,
     col = "lightblue",
     main = "Distribuição simulada do VPL",
     xlab = "VPL (R$ mil)")

# linha vertical em VPL = 0
abline(v = 0, col = "red", lwd = 2, lty = 2)

# linha vertical no VPL medio simulado
abline(v = mean(vpl_sim), col = "blue", lwd = 2)

#legenda das duas linhas
legend("topright",
       legend = c("VPL = 0", "VPL médio simulado"),
       col = c("red", "blue"),
       lwd = 2, lty = c(2,1), bty = "n")

# Resolução da Questão 2 ----------------------------------------

# (a) Importe os preços (ajustando) e calcule os
# retornos log diarios.
precos_vale3 <- "VALE3.SA" |>
  tq_get(get = "stock.prices",
         from = "2024-01-01",
         to = "2026-06-08") |>
  select(date, adjusted)

retornos_vale3 <- precos_vale3 |>
  # formula do retorno log diario (use o preço ajustado)
  mutate(ret_vale3 = log(adjusted / dplyr::lag(adjusted))) |>
  drop_na()

#extrai a coluna ret como um vetor
ret_vale3 <- retornos_vale3 |> pull(ret_vale3)

# (b) Parametros do problema (veja o enunciado).
valor_carteira <- 25000
p <- 0.01
horizonte <- "1 dia"

# (c) VaR histórico: quantil da causa esquerda

# ordena os retornos do pior para o melhor
ret_ordenado <- sort(ret_vale3)

# posicao do quantil de p
k <- ceiling (length(ret_ordenado) * p)

#retorno no ponto de corte
retorno_var <- ret_ordenado[k]

# converte o retorno em perda positiva (%)
var_percentual <- -retorno_var * 100

# VaR em reais
var_monetario <- -retorno_var * valor_carteira

#exibe os valores de VaR
var_percentual
var_monetario

# (d) Expected Shortfall

#media dos retornos nas posições 1 ate k
retorno_medio_cauda <- mean(ret_ordenado[1:k])

# ES em %, como perda positiva
es_percentual <- -retorno_medio_cauda * 100

# ES em reais
es_monetario <- -retorno_medio_cauda * valor_carteira

# exibe os valores do ES
es_percentual
es_monetario

# (e) Interpretação: escreva aqui sua resposta, como comentario
# 4.7405 1185.1
  

# Resolução da Questão 3 ----------------------------------------

# (a) Importe os preços (ajustados), organize em formato largo
# e calcule os retornos SIMPLES diarios de cada ação.

series_precos <- c("ITUB4.SA", "VALE3.SA", "WEGE3.SA") |>
  tq_get(get = "stock.prices",
         from = "2024-01-01",
         to = "2026-06-08") |>
  select(symbol, date, adjusted) |>
  pivot_wider(names_from = symbol,
              values_from = adjusted) |>
  rename(dia = date,
         itub4 = ITUB4.SA,
         vale3 = VALE3.SA,
         wege3 = WEGE3.SA)

# retorno somples = preco / preco anterior -1
retornos <- series_precos |>
  mutate(
    #exemplo: retorno simples do ITUB4
    ret_itub4 = itub4 / dplyr::lag(itub4) - 1,
    #aplique a mesma formula
    ret_vale3 = vale3 / dplyr::lag(vale3) - 1,
    ret_wege3 = wege3 / dplyr::lag(wege3) - 1,
  ) |>
  # remove dados faltantes
  drop_na()

# (b) Calcule o retorno diário da carteira

# pesos da carteira, na ordem: itub4, vale3, wege3
pesos <- c(0.40, 0.35, 0.25)

retornos <- retornos |>
  # combine os tres retornos com os pesos
  mutate(ret_carteira = pesos[1] * ret_itub4 +
           pesos[2] * ret_vale3 +
           pesos[3] * ret_wege3)

# (c)-(d) Parametros e medidas de risco (VaR e ES)

# parametros do problema
valor_carteira <- 100000
p <- 0.01

# ordena os retornos do pior para o melhor
ret_ordenado <- sort(retornos$ret_carteira)

# posicao do quantil de p
k <- ceiling(length(ret_ordenado) * p)

# VaR em %, como perda positiva
var_percentual <- -retorno_var * 100

# VaR em reais
var_monetario <- -retorno_var * valor_carteira

# ES em %, com perda positiva
es_percentual <- -retorno_medio_cauda * 100

# ES em reais
es_monetario <- -retorno_medio_cauda * valor_carteira

# exibe os valores do VaR e do ES
var_percentual
var_monetario
es_percentual
es_monetario

# (e) Interpretação: escreva aqui sua resposta, como cometario.
# 3.8512 3851.2 4.7405 4740.5

