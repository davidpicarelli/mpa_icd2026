# Arquivo: Exercicio01.R
# Autor(a): David Picarelli Gonçalves
# Data: 21/06/2026
# Objetivo: Resolução do exercicio 1 da aula 14

library(infer)

set.seed(2026)
ic_boot <- dados_incentivos |>
  specify(produtividade ~ grupo) |>
  generate(reps = 1000, type = "bootstrap") |>
  calculate(
    stat = "diff in means",
    order = ordem_incentivos
  ) |>
  get_confidence_interval(level = 0.95, type = "percentile")

ic_boot

# Interpretação:
# Com 95% de confiança, o incentivo monetário eleva a produtividade média entre
# aproximadamente 0,65 e 5,04.