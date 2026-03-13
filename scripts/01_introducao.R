# =============================================================================
# Disciplina: Introdução à Ciência de Dados
# =============================================================================
# Arquivo: 01_introducao.R
# Autor: David Picarelli Goncalves
# Data: 12/03/2026
# Objetivo: Usar Rstudio, script R e alguns fundamentos da lingugem R

# Atalho para criar seções de código: Crtl + Shift + R




# Configurações globais ---------------------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
# LEMBRE-SE: Pacotes precisam ser instalados antes de serem carregados
library(here) #para usar caminhos relativos
library(tidyverse) # meta-pacote que inclui readr, dplyr, ggplot2, etc.
library(skimr) # para compreender os dados
library(janitor) # para limpar nomes de colunas



# R como uma grande calculadora -------------------------------------------

# Operacoes aritmeticas basicas

# adicao
15 + 7

# subtracao
20 - 6

# multiplicacao
8 * 9

## divisao
84 / 7

## potenciacao
2^5

# Precedencia de operacoes matematicas
# parenteses primeiro, depois potenciacao, multiplicacao e divisao,
# e por ultimo adicao e subtracao

# parentese primeiro
(15 + 7) * 2
84 / (7 + 5)



# Exemplos de Funções matemáticas -----------------------------------------

# logaritmo natural
log(100)

# logaritmo base 10
log10(100)

# funcao exponencial (e elevado a x)
exp(1)

# valor absoluto
abs(-45)

# raiz quadrada
sqrt(225)

# arredondamento para 2 casas decimais
round(3.14159, digits = 2)



#  Tipos atomicos e classes -----------------------------------------------

# Os tipos de dados definem como os dados
# sao armazenados na memoria.

# tipo double e classe numeric
a <- 3.14
a
# funcao que retorna o tipo do objeto
typeof(a)
# funcao que retorna a classe do objeto
class(a)

# character
b <- "Joao"
b

#logical
c <- TRUE
c

d <- FALSE
d

# NaN (Not a Number) representa um valor indefinido
e <- 0 / 0
e

# Inf (Infinity) representa um valor infinito
f <- 1 / 0
f

# coercao de logical para numeric
# TRUE = 1 e FALSE = 0
g <- as.numeric(c)
g



# Vetores numericos -------------------------------------------------------

# Cria dois vetores numericos com dados de receita e custo diarios

receita_diaria <- c(9200, 8700, 10100, 9800, 11050)
print(receita_diaria)


custo_diario <- c(6400, 6000, 7200, 6800, 7600)
custo_diario

# Vetorizacao: operacoes elemento e elemento
lucro_diario <- receita_diaria - custo_diario
margem_diaria <- lucro_diario / receita_diaria


# Funcoes uteis para vetores numericos ------------------------------------

# logaritmo da receita diaria
log(receita_diaria)

# receita total da semana
sum(receita_diaria)

# receita media da semana
mean(receita_diaria)

# tamanho do vetor de receita
# Nesse caso eh o numero de dias registrados
length(receita_diaria)

# receita minima da semana
min(receita_diaria)

# receita maxima da semana
max(receita_diaria)

#vendo a ajuda de uma funcao
?mean
?lenght


# Vetores de caracteres e logicos -----------------------------------------

# vetores devem conter o mesmo tipo de dados, ou seja,
# todos os elementos devem ser do mesmo tipo

# vetor de caracteres
nome_empresa <- c("Loja A", "Loja B", "Loja C")
# exibe o vetor criado
nome_empresa

# vetor logico (booleano) indicando se a meta de vendas foi batida
meta_batida <- c(TRUE, FALSE, TRUE)
# exibe o vetor criado
meta_batida



# Fatores -----------------------------------------------------------------


# Fatores sao usados para armazenar variaveis categoricas
# nominais ou ordinais.

# vetor de caracteres com meses do ano
meses <- c("Dez", "Abr", "Jan", "Mar")
meses

# um vetor de caracteres eh ordenado por ordem alfabetica
sort(meses)

# definindo os nives dos meses em ordem cronologica
niveis_meses <- c(
  "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
  "Jul", "Ago", "Set", "Out", "Nov", "Dez"
)

# converte o vetor meses para fator, usando os niveis definidos
meses <- factor(meses, levels = niveis_meses)
meses

#ordena os meses
sort(meses)



# Importa arquivo de dados ------------------------------------------------

# define o caminho relativo para o arquivo csv
# usando a funcao here() 
)
