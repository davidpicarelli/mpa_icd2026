
# Simulação - Lançamento de moeda -----------------------------------------


# fixa o ponto inicial do gerador pseudoaleatório
set.seed(123)

# número de lançamentos da moeda
n <- 5000

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




# independencia de eventos-------------------------------------------------


#fixa a semente para reprodutibilidade
set.seed(456)

# número de repetições simuladas
n <- 100000

# simula as quedas separadamente; no modelo simulado, elas são independentes
simulacao <- tibble(
  queda_x = sample(
    c(TRUE, FALSE),
    size = n,
    replace = TRUE,
    prob = c(0.08, 0.92)
  ),
  queda_y = sample(
    c(TRUE, FALSE),
    size = n,
    replace = TRUE,
    prob = c(0.06, 0.94)
  )
)

# mean(TRUE/FALSE) calcula a proporção de TRUE
p_x <- mean(simulacao$queda_x)
p_y <- mean(simulacao$queda_y)

# & identifica as linhas em que as duas ações caíram ao mesmo tempo
p_xy <- mean(simulacao$queda_x & simulacao$queda_y)

# compara a interseção simulada com o produto das probabilidades
tibble(
  `P(X cai)` = p_x,
  `P(Y cai)` = p_y,
  `P(ambas caem)` = p_xy,
  `P(X cai) * P(Y cai)` = p_x * p_y
)



# teorema de bayes --------------------------------------------------------

# fixa a semente para reprodutibilidade
set.seed(789)

# número de clientes simulados
n_clientes <- 100000

# primeiro simulamos se cada cliente é inadimplente ou não
clientes_simulados <- tibble(
  inadimplente = sample(
    c(TRUE, FALSE),
    size = n_clientes,
    replace = TRUE,
    prob = c(0.10, 0.90)
  ),
  
  # case_when() aplica uma regra para cada condição lógica
  alto_risco = case_when(
    inadimplente ~ sample(
      c(TRUE, FALSE),
      size = n_clientes,
      replace = TRUE,
      prob = c(0.90, 0.10)
    ),
    # !inadimplente significa "não inadimplente"
    !inadimplente ~ sample(
      c(TRUE, FALSE),
      size = n_clientes,
      replace = TRUE,
      prob = c(0.20, 0.80)
    )
  )
)

# summarise() resume as principais proporções simuladas
clientes_simulados |>
  summarise(
    prop_inadimplente = mean(inadimplente),
    prop_alto_risco = mean(alto_risco),
    prop_inadimplente_dado_alto_risco = mean(inadimplente[alto_risco])
  )
