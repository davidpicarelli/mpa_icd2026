# Arquivo: exercicios.R
# Autor(a): David Picarelli Gonçalves
# Data: 28/05/2026
# Objetivo: Resolução dos exercicios 1 e 2 da aula 12

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)
library(tidyverse)
library(tidyquant)

# salva as séries importadas no objeto serie_bivariada
serie_bivariada <- c("PETR4.SA") %>%
  # obtém os dados de preços das ações a partir de 01/01/2024
  tq_get(from = "2024-01-01") %>%
  # seleciona as colunas relevantes
  select(symbol, date, adjusted) %>%
  pivot_wider(names_from = symbol, values_from = adjusted) %>%
  # renomeia as colunas para facilitar a leitura
  rename(
    companhia = symbol,
    dia = date,
    petr4 = PETR4.SA,
    preco_ajustado = adjusted
  )

# exibe as 6 primeiras linhas da data frame
head(serie_bivariada)

# Calcula retornos logarítmicos
retornos <- serie_bivariada %>%
  mutate(
    ret_petr4 = log(petr4 / lag(petr4)),
    #ret_mglu3 = log(mglu3 / lag(mglu3)),
    #ret_itub4 = log(itub4 / lag(itub4))
  ) %>%
  drop_na()

retornos_acoes <- c("PETR4.SA") %>%
  tq_get(from = "2024-01-01") %>%
  group_by(symbol) %>%
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "daily",
    type = "arithmetic",
    col_rename = "retorno"
  )

head(retornos_acoes)