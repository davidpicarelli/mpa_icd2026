# Arquivo: 01-avaliacao-resolucao.R
# Autor(a): David Picarelli Gonçalves
# Data: 16/04/2026
# Objetivo: 
# Resolução da Avaliação 1 - Introdução à Ciência de Dados


# Configurações globais  ----------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)
library(tidyverse)


# Resolução da Questão 1


# 1.a) --------------------------------------------------------
agencias_csv <- here("data/raw/agencias.csv")
agencias_sample <- read.csv(agencias_csv)
glimpse(agencias_sample)

credito_csv <- here("data/raw/credito_trimestral.csv")
credito_sample <- read.csv(credito_csv)
glimpse(credito_sample)

inadimplencia_csv <- here("data/raw/inadimplencia.csv")
inadimplencia_sample <- read.csv(inadimplencia_csv)
glimpse(inadimplencia_sample)

# 1.b) ---------------------------------------------------------
credito_long <- credito_sample|>
  pivot_longer(
    cols = c(T1, T2, T3, T4),
    names_to = "trimestre",
    values_to = "volume_credito"
  )

# 1.c) ---------------------------------------------------------
dados_completos <- credito_long |>
  left_join(agencias_sample, by = "codigo_agencia") |>
  left_join(inadimplencia_sample, by = c("codigo_agencia", "trimestre"))

# 1.d) ---------------------------------------------------------
dados_analise <- dados_completos |> 
  mutate(
    credito_por_cooperado = volume_credito * 1000 / num_cooperados,
    risco = case_when(
      taxa_inadimplencia < 3.0 ~ "Baixo",
      taxa_inadimplencia >= 3.0 & taxa_inadimplencia < 4.5 ~ "Moderado",
      taxa_inadimplencia >= 4.5 ~ "Alto"
    )
  )

# 1.e) ---------------------------------------------------------
dados_analise |> 
  group_by(cidade) |> 
  summarise(
    volume_total = sum(volume_credito),
    media_inadimplencia = mean(taxa_inadimplencia)
  ) |> 
  arrange(desc(volume_total))


# Resolução da Questão 2


# 2.a) ---------------------------------------------------------
calcular_prestacao <- function(valor, taxa_anual, num_meses) {
  taxa_mensal <- taxa_anual / 12
  montante <- valor * ((taxa_mensal*((1+taxa_mensal)^num_meses))/((1+taxa_mensal)^num_meses-1))
  return(montante)
}

calcular_prestacao(valor = 120000, taxa_anual = 0.12, num_meses = 60)

# 2.b) ---------------------------------------------------------
taxas <- c(0.08, 0.10, 0.12, 0.14, 0.16)
prestacoes <- map_dbl(
  taxas,
  \(taxa_anual) calcular_prestacao(120000, taxa_anual, 60)
)

resultado <- tibble(
  taxa_anual = taxas,
  prestacao_mensal = prestacoes
)


# 2.c) ---------------------------------------------------------
resultado |> 
  mutate(
    custo_total = prestacao_mensal * 60,
    juros_totais = custo_total - 120000,
    acessibilidade = case_when(
      prestacao_mensal < 2600 ~ "Acessivel",
      prestacao_mensal >= 2600 & prestacao_mensal < 2800 ~ "Moderado",
      prestacao_mensal >= 2800 ~ "Elevado"
    )
  )
