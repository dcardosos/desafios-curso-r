library(dplyr)
library(tidyr)

dados <- readr::read_rds("aula1/dados/imdb.rds")


dados |>
  replace_na(list(receita = 0, orcamento = 0)) |> 
  pivot_longer(
    cols = ator_1:ator_3,
    names_to = "atores",
    values_to = "nome_ator") |> 
  group_by(nome_ator) |> 
  summarise(
      nota_media_imdb  = mean(nota_imdb, na.rm = TRUE),
      media_lucro = mean(receita - orcamento, na.rm = TRUE)
    )
  
  