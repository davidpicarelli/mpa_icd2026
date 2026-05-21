# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)
library(tidyverse)



# exercicio 6 -------------------------------------------------------------
#Bayes
#Um modelo classifica empresas como “risco elevado”.
#8% das empresas realmente entram em dificuldade financeira;
#entre as empresas que entram em dificuldade, 85% são classificadas como risco elevado;
#entre as empresas saudáveis, 25% também são classificadas como risco elevado.
#Calcule:
#  a probabilidade total de uma empresa ser classificada como risco elevado;
#  a probabilidade de dificuldade financeira dado que a empresa foi classificada como risco elevado:
#  P(dificuldade∣risco elevado)



# exercicio 7 -------------------------------------------------------------
#Simulação: frequência relativa
#Use R para simular lançamentos de uma moeda equilibrada.
#Faça o que se pede:
  
#  Use set.seed(2026).
#Simule lançamentos para três tamanhos: 1.000, 10.000 e 100.000.
#Em cada caso, use sample() com replace = TRUE e prob = c(0.5, 0.5).
#Calcule a frequência relativa de caras.
#Compare cada frequência simulada com P(cara)=0.5
#Interprete os resultados à luz da Lei dos Grandes Números.

# fixa o ponto inicial do gerador pseudoaleatório
set.seed(2026)

# número de lançamentos da moeda
n <- 1000

# tibble() organiza a simulação em formato de dados:
# cada linha é um lançamento; cada coluna guarda uma informação
simulacao_moeda <- tibble(
  # cria a sequência 1, 2, ..., n dos lançamentos simulados
  tentativa = seq_len(n),
  
  # sample() define o mecanismo aleatório:
  # resultados possíveis, número de sorteios, reposição e probabilidades
  resultado = sample(
    c("cara", "coroa"),
    size = n,
    replace = TRUE,
    prob = c(0.5, 0.5)
  ),
  
  # resultado == "cara" cria TRUE para cara e FALSE para coroa
  cara = resultado == "cara",
  
  # cumsum(cara) conta o total acumulado de caras
  freq_cara = cumsum(cara) / tentativa
)

# exibe as seis primeiras linhas da simulação
simulacao_moeda[1:6, ]

# fixa o ponto inicial do gerador pseudoaleatório
set.seed(2026)

# número de lançamentos da moeda
n <- 10000

# tibble() organiza a simulação em formato de dados:
# cada linha é um lançamento; cada coluna guarda uma informação
simulacao_moeda <- tibble(
  # cria a sequência 1, 2, ..., n dos lançamentos simulados
  tentativa = seq_len(n),
  
  # sample() define o mecanismo aleatório:
  # resultados possíveis, número de sorteios, reposição e probabilidades
  resultado = sample(
    c("cara", "coroa"),
    size = n,
    replace = TRUE,
    prob = c(0.5, 0.5)
  ),
  
  # resultado == "cara" cria TRUE para cara e FALSE para coroa
  cara = resultado == "cara",
  
  # cumsum(cara) conta o total acumulado de caras
  freq_cara = cumsum(cara) / tentativa
)

# exibe as seis primeiras linhas da simulação
simulacao_moeda[1:6, ]

# fixa o ponto inicial do gerador pseudoaleatório
set.seed(2026)

# número de lançamentos da moeda
n <- 100000

# tibble() organiza a simulação em formato de dados:
# cada linha é um lançamento; cada coluna guarda uma informação
simulacao_moeda <- tibble(
  # cria a sequência 1, 2, ..., n dos lançamentos simulados
  tentativa = seq_len(n),
  
  # sample() define o mecanismo aleatório:
  # resultados possíveis, número de sorteios, reposição e probabilidades
  resultado = sample(
    c("cara", "coroa"),
    size = n,
    replace = TRUE,
    prob = c(0.5, 0.5)
  ),
  
  # resultado == "cara" cria TRUE para cara e FALSE para coroa
  cara = resultado == "cara",
  
  # cumsum(cara) conta o total acumulado de caras
  freq_cara = cumsum(cara) / tentativa
)

# exibe as seis primeiras linhas da simulação
simulacao_moeda[1:6, ]


# exercicio 8 -------------------------------------------------------------

#Simulação: probabilidade total e Bayes

#Considere novamente o modelo do Exercício 6:
#8% das empresas entram em dificuldade financeira;
#entre as empresas em dificuldade, 85% são classificadas como risco elevado;
#entre as empresas saudáveis, 25% também são classificadas como risco elevado.
#Use R para simular 100.000 empresas.

#Faça o que se pede:
  
#  Use set.seed(2026).
#Simule quais empresas entram em dificuldade financeira.
#Simule a classificação de risco elevado de acordo com a situação real da empresa.
#Estime a proporção total de empresas classificadas como risco elevado.
#Estime P(dificuldade∣risco elevado)
#pela proporção simulada.
#Compare os resultados simulados com os valores teóricos obtidos no Exercício 6.
