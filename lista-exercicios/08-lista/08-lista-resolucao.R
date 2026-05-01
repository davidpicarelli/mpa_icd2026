# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)
library(tidyverse)



# exercicio 6 -------------------------------------------------------------




# exercicio 7 -------------------------------------------------------------

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