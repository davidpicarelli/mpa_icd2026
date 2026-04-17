# Arquivo: 04-lista-resolucao.R
# Autor(a): David Picarelli Gonçalves
# Data: 09/04/2026
# Objetivo: Resolução da lista de exercícios 4

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminhos relativos
library(tidyverse) # inclui readr, dplyr, tidyr, ggplot2 etc.


# Exercício 1 ------------------------------------------------
# cria função para calcular montante com capitalização mensal
  calcular_montante_mensal <- function(capital, taxa_anual, meses) {
  taxa_mensal <- taxa_anual / 12
  montante <- capital * (1 + taxa_mensal)^meses
  return(montante)
}

# testa a função
calcular_montante_mensal(capital = 5000, taxa_anual = 0.10, meses = 36)


# Exercício 2 ------------------------------------------------
# cria função para avaliar investimento
  avaliar_investimento <- function(retorno) {
  if (retorno > 0.15) {
    return("Excelente")
  } else if (retorno > 0.05) {
    return("Bom")
  } else if (retorno > 0) {
    return("Fraco")
  } else {
    return("Negativo")
  }
}

# testa a função com os valores solicitados
avaliar_investimento(0.20)
avaliar_investimento(0.08)
avaliar_investimento(0.02)
avaliar_investimento(-0.05)

# Exercício 3 ------------------------------------------------
# cria função que analisa uma carteira de ativos
  analisar_carteira <- function(dados) {
  dados |>
    mutate(
      retorno        = (preco_atual / preco_compra - 1) * 100,
      valor_investido = preco_compra * quantidade,
      valor_atual     = preco_atual * quantidade,
      resultado       = valor_atual - valor_investido,
      situacao        = ifelse(resultado > 0, "Ganho", "Perda")
    )
}

# cria a tibble de teste
  carteira <- tibble(
  ativo        = c("PETR4", "VALE3", "ITUB4", "WEGE3"),
  preco_compra = c(28.50, 68.20, 32.00, 45.00),
  preco_atual  = c(31.00, 65.40, 33.60, 48.50),
  quantidade   = c(200, 100, 300, 150)
)

# testa a função
analisar_carteira(carteira)


# Exercício 4 ------------------------------------------------
# função para calcular valor futuro (definida na Aula 5)
  calcular_valor_futuro <- function(valor_presente, taxa, periodos = 1) {
  valor_futuro <- valor_presente * (1 + taxa)^periodos
  return(valor_futuro)
}

# taxas anuais
taxas_anuais <- c(0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16)

# calcula valor futuro para cada taxa com map_dbl()
vf_20_anos <- map_dbl(
  taxas_anuais,
  \(taxa) calcular_valor_futuro(10000, taxa, 20)
)

# exibe os resultados
vf_20_anos

# cria a tibble comparativa
comparacao_cenarios <- tibble(
  taxa            = taxas_anuais,
  taxa_percentual = taxas_anuais * 100,
  valor_futuro    = vf_20_anos,
  ganho_liquido   = vf_20_anos - 10000
)

# exibe a tibble
comparacao_cenarios

# Exercício 5 ------------------------------------------------
# função para calcular VPL (definida na Aula 5, Exemplo 6)
calcular_vpl <- function(investimento, fluxos, taxa, valor_residual = 0) {
  n <- length(fluxos)
  t <- seq_along(fluxos)
  vpl <- -investimento + sum(fluxos / (1 + taxa)^t) + 
    valor_residual / (1 + taxa)^n
  return(vpl)
}

# parâmetros do projeto
fluxos_projeto <- c(80000, 95000, 110000, 100000)
taxas_desconto <- c(0.08, 0.10, 0.12, 0.14, 0.16, 0.18)

# calcula VPL para cada taxa de desconto com map_dbl()
vpl_por_taxa <- map_dbl(
  taxas_desconto,
  \(taxa) calcular_vpl(300000, fluxos_projeto, taxa, 30000)
)

# organiza os resultados em uma tibble
analise_projeto <- tibble(
  taxa_pct = taxas_desconto * 100,
  vpl      = vpl_por_taxa,
  decisao  = ifelse(vpl_por_taxa > 0, "Viável", "Inviável")
)

# exibe a tibble
analise_projeto


# Exercício 6 ------------------------------------------------
# (resolver em arquivo .qmd separado)


# Exercício 7 (Desafio) --------------------------------------
# define semente para reprodutibilidade
set.seed(123)

# simula 1.000 cenários de VPL
vpl_sim <- map_dbl(1:1000, \(i) {
  fluxos <- rnorm(3, mean = 80000, sd = 15000)
  calcular_vpl(200000, fluxos, 0.10, 20000)
})

# VPL médio
mean(vpl_sim)
# probabilidade de VPL positivo
mean(vpl_sim > 0)
# percentis 5% e 95%
quantile(vpl_sim, c(0.05, 0.95))