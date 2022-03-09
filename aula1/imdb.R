library(dplyr)
library(tidyr)
library(purrr)
dados <- readr::read_rds("aula1/dados/imdb.rds")

pick_genero <- function(vec, position){
  
  table_split <- vec |> 
    stringr::str_split("[|]") |> 
    pluck(1) |> 
    table()
  
  if(length(names(table_split)) == 1){
    
    return(names(table_split))
    
  }
  
  valor <- table_split |> 
    sort(decreasing = TRUE) |>
    as_tibble(.name_repair = ~ c("value", "count")) |> 
    slice(position) |> 
    pull(value)
  
  if(tibble::is_tibble(valor)){
    
      return(NA)
    
  } else {
    
    return(valor)
    
  }
  
} 


dados |>
  replace_na(list(receita = 0, orcamento = 0, ano = 9999)) |> 
  pivot_longer(
    cols = ator_1:ator_3,
    names_to = "atores",
    values_to = "nome_ator") |> 
  select(-likes_facebook, -atores) |> 
  mutate(lucro = receita - orcamento) |> 
  group_by(nome_ator) |>
  tidyr::nest(filmes = -nome_ator) |>
  summarise(
    nota_media_imdb  = pluck(filmes, 1, "nota_imdb") |> mean(na.rm = TRUE),
    media_lucro = pluck(filmes, 1, "lucro") |> mean(na.rm = TRUE),
    generos_combined = pluck(filmes, 1, "generos") |> reduce(~ stringr::str_c(..., sep = "|")),
    top1_genero = generos_combined |> pick_genero(1),
    top2_genero = generos_combined |> pick_genero(2),
    top3_genero = generos_combined |> pick_genero(3),
    primeiro_registro = pluck(filmes, 1, "ano") |> keep(~ .x != 9999) |> min(),
    ultimo_registro = pluck(filmes, 1, "ano") |> keep(~ .x != 9999) |> max(),
    contracenou = ) |> 
 select(-generos_combined)


# dados |>
#   replace_na(list(receita = 0, orcamento = 0, ano = 9999)) |> 
#   pivot_longer(
#     cols = ator_1:ator_3,
#     names_to = "atores",
#     values_to = "nome_ator") |> 
#   select(-likes_facebook, -atores) |> 
#   mutate(lucro = receita - orcamento) |> 
#   group_by(nome_ator) |>
#   tidyr::nest(filmes = -nome_ator) |> 
#   ungroup() |> 
#   slice(1) |> 
#   pluck("filmes", 1)


# dados |>
#   pivot_longer(
#     cols = ator_1:ator_3,
#     names_to = "atores",
#     values_to = "nome_ator") |> 
#   group_by(nome_ator) |> 
#   summarise(
#     concat = titulo |> reduce(~ stringr::str_c(..., sep = "|")))
# 
# 
# 
# dados |>
#   pivot_longer(
#     cols = ator_1:ator_3,
#     names_to = "atores",
#     values_to = "nome_ator") |> 
#   group_by(filme, nome_ator) |> 
#   summarise(
#     concat = reduce(~ stringr::str_c(..., sep = "|"))
#   )
#   
# dados |>
#   pivot_longer(
#     cols = ator_1:ator_3,
#     names_to = "atores",
#     values_to = "nome_ator") |> 
#   group_by(titulo) |> 
#   summarise(
#     concat = nome_ator |> reduce(~ stringr::str_c(..., sep = "|")))
#   