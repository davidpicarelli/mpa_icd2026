# Arquivo: Exercicio02.R
# Autor(a): David Picarelli Gonçalves
# Data: 21/06/2026
# Objetivo: Resolução dos exercicio 2 da aula 14

set.seed(2026)
# diferença observada
obs_diff <- dados_incentivos |>
  specify(produtividade ~ grupo) |>
  calculate(
    stat = "diff in means",
    order = ordem_incentivos
  )

# distribuição nula por permutação
perm_dist <- dados_incentivos |>
  specify(produtividade ~ grupo) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(
    stat = "diff in means",
    order = ordem_incentivos
  )

# valor-p
p_val <- perm_dist |>
  get_p_value(
    obs_stat = obs_diff,
    direction = "two-sided"
  )

obs_diff
p_val

# Interpretação:
# Diferença observada: 2,9 unidades a mais por semana do que o grupo com
# incentivo não monetário.
#
# Valor-p: 0,018
#
# Conclusão: Como o valor-p = 0,018 é menor que o nível de significância
# de 5% (0,05), rejeitamos a hipótese nula.
#
# Leitura causal: A leitura causal é possível porque os participantes 
# foram distribuídos aleatoriamente entre os grupos, tornando-os
# comparáveis.