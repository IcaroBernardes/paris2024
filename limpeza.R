# 0. Setup inicial ##########
## Carrega bibliotecas
library(dplyr)
library(tidyr)

## Carrega dados brutos
rawData <- read.csv("data/medals.csv")

# 1. Limpeza dos dados ##########
## Calcula a qtd. de medalhas ganhas por país a cada dia
workData <- rawData |> 
  dplyr::mutate(medalha = 1) |> 
  tidyr::pivot_wider(names_from = evento_medalha,
                     values_from = medalha,
                     values_fill = 0) |> 
  dplyr::relocate(Prata, .after = Ouro) |> 
  dplyr::summarise(across(
    .cols = where(is.numeric),
    .fns = sum
  ), .by = c(pais_codigo, pais_nome, evento_data))

## Preenche os dias em que países não ganharam medalha
## e calcula o cumulativo de medalhas a cada dia
workData <- workData |> 
  dplyr::arrange(pais_codigo, pais_nome, evento_data) |>
  tidyr::complete(nesting(pais_codigo, pais_nome), evento_data,
                  fill = list(Ouro = 0, Prata = 0, Bronze = 0)) |> 
  dplyr::mutate(across(
    .cols = where(is.numeric),
    .fns = cumsum
  ), .by = c(pais_codigo))

## Elimina períodos em que os países
## ainda não haviam ganho qualquer medalha
workData <- workData |> 
  dplyr::filter(if_any(
    .cols = where(is.numeric),
    .fns = \(x) x != 0
  ))

## Ordena os países a cada dia
workData <- workData |> 
  dplyr::arrange(
    desc(evento_data), desc(Ouro), desc(Prata), desc(Bronze),
    .by = evento_data
  ) |> 
  dplyr::mutate(
    pontos = Ouro*1E+5 + Prata*1E+3 + Bronze,
    rank = dense_rank(desc(pontos)),
    .by = evento_data
  ) |> 
  dplyr::select(-pontos)

## Salva o quadro de medalhas diário atualizado
write.csv(workData, "data/medalTable.csv", row.names = FALSE)
