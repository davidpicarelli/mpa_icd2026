# Arquivo: exercicio-aula-11.R
# Autor(a): David Picarelli Gonçalves
# Data: 21/05/2026
# Objetivo: Resolução dos exercicios 1 e 2

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)
library(tidyverse)
library(EnvStats)
#library(triangle)



# Exercicio 1 -------------------------------------------------------------
#A distribuição Logística possui função de distribuição acumulada dada por:
#F(x)=11+e−(x−μ)/β
#Gere 10.000 variáveis aleatórias que seguem esta distribuição usando a
#transformação inversa.
#
#
#

# Exercicio 2 -------------------------------------------------------------
#O Índice de Liquidez Rápida (ILR) mede a capacidade de uma empresa pagar suas
#obrigações de curto prazo sem depender da venda de estoques:
#ILR=Ativo Circulante−Estoques/Passivo Circulante
#Cenário: Você é analista financeiro de uma empresa do setor de varejo e precisa
#avaliar o risco de liquidez sob incerteza.
#Informações dos especialistas (baseadas em dados setoriais):
#Ativo Circulante: entre R$ 800 mil e R$ 1.200 mil (mais provável: R$ 950 mil)
#Estoques: entre R$ 200 mil e R$ 400 mil (mais provável: R$ 280 mil)
#Passivo Circulante: entre R$ 600 mil e R$ 900 mil (mais provável: R$ 720 mil)
#Questões:
#  Use set.seed(2024) e
#  Simule 20.000 cenários do ILR usando distribuições triangulares
#  Calcule P(ILR<1)
#  e interprete o resultado

#Compare o ILR médio simulado com o modelo determinístico (valores mais prováveis)
#Recomende ações se a probabilidade de ILR < 1 for maior que 40%


# sintaxe da função
#a=limite minimo
#b=limite maximo
#c=valor mais provavel
#n=numero de simulações
# sintaxe da função
#rtri(n, a, b, c)

# Define a semente para tornar os resultados reprodutíveis
set.seed(2024)

# Define o número de cenários simulados
n_sim <- 20000

# Simula os parâmetros incertos de cada cenário 
ativo_circulante <- rtri(n_sim, 800, 1200, 950)
passivo_circulante <- rtri(n, 800, 1200, 950)
estoques <- rtri(n, 200, 400, 280)



