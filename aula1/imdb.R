# Bibliotecas utilizadas 
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)

# Lendo os dados
dados <- read_rds("https://github.com/curso-r/livro-material/raw/master/assets/data/imdb.rds")

# Essa função vai ajudar a decidir os top1, top2, top3 gêneros de cada ator.
# A ideia dela é utilizar um vetor com todos os gêneros concatenanos (por ator)
# e calcular a frequência de cada termo no vetor e retornar uma posição, que é
# definida pelo argumento `position`

pick_genero <- function(vec, position){
  
  table_split <- vec |> 
    str_split("[|]") |> 
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

# Aqui é a manipulação da base em si.
# 1. replace de NA 
# 2. pivotar os ator_1:ator_3 em uma coluna só
# 3. tirar colunas desnecessárias
# 4. calcular o lucro
# 5/6. agrupar por ator e "aninhar" todo o resto do dado em uma coluna `filmes`

dados |>
  replace_na(list(receita = 0, orcamento = 0, ano = 9999)) |> 
  pivot_longer(
    cols = ator_1:ator_3,
    names_to = "atores",
    values_to = "nome_ator") |> 
  select(-likes_facebook, -atores) |> 
  mutate(lucro = receita - orcamento) |> 
  group_by(nome_ator) |>
  nest(filmes = -nome_ator) |>
  summarise(
    nota_media_imdb  = pluck(filmes, 1, "nota_imdb") |> mean(na.rm = TRUE),
    media_lucro = pluck(filmes, 1, "lucro") |> mean(na.rm = TRUE),
    generos_combined = pluck(filmes, 1, "generos") |> reduce(~ str_c(..., sep = "|")),
    top1_genero = generos_combined |> pick_genero(1),
    top2_genero = generos_combined |> pick_genero(2),
    top3_genero = generos_combined |> pick_genero(3),
    primeiro_registro = pluck(filmes, 1, "ano") |> keep(~ .x != 9999) |> min(),
    ultimo_registro = pluck(filmes, 1, "ano") |> keep(~ .x != 9999) |> max()) |> 
 select(-generos_combined)


# tentativa do contracenou ------------------------------------------------

dados |> 
  select(ator_2, ator_1) |> 
  table() |> 
  as.data.frame() |> 
  filter(ator_2 == "Al Pacino", Freq > 0) 

dados |> 
  select(ator_1, ator_2) |> 
  table() |> 
  as.data.frame() |> 
  filter(ator_2 == "Al Pacino", Freq > 0)
