# 0. Setup inicial ##########
## Carrega bibliotecas
library(dplyr)
library(httr2)
library(jsonlite)
library(purrr)
library(rvest)
library(tidyr)
library(xml2)

## Define URL a ser extraída e um user-agent que simula um navegador
url <- "https://olympics.com/pt/paris-2024/medalhas"
userAgent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"

# 1. Extração dos dados ##########
## Realiza a requisição com o user-agent
resp <- httr2::request(url) |> 
  httr2::req_user_agent(userAgent) |>
  httr2::req_perform()

## Obtém a página de resposta em formato da árvore HTML (xml_document)
page <- httr2::resp_body_html(resp)

## Extrai a seção da página que contém os resultados
jsonData <- page |> 
  rvest::html_element('script#__NEXT_DATA__') |> 
  rvest::html_text() |> 
  jsonlite::fromJSON()

## Extrai a tabela de medalhas
allData <- jsonData[["props"]][["pageProps"]][["initialMedals"]][["medalStandings"]][["medalsTable"]]

## Lista as medalhas obtidas por cada país
allMedals <- allData$disciplines |> 
  purrr::map(\(country) {
    
    countryMedals <- country |> 
      tidyr::unnest(cols = medalWinners) |> 
      dplyr::select(disciplineCode, name, eventCategory, medalType,
                    competitorDisplayName, date) |> 
      dplyr::rename(
        esporte_codigo = disciplineCode,
        esporte_nome = name,
        evento_genero = eventCategory,
        evento_medalha = medalType,
        atleta_nome = competitorDisplayName,
        data = date
      )
    
  })

## Inclui nome e sigla do país
tidyData <- allData |> 
  dplyr::select(organisation, description) |> 
  dplyr::rename(
    pais_abrev = organisation,
    pais_nome = description
  ) |> 
  dplyr::mutate(data = allMedals) |> 
  tidyr::unnest(cols = data)

## Traduz gênero do evento e medalha
tidyData <- tidyData |> 
  dplyr::mutate(
    evento_genero = case_match(
      evento_genero,
      "Men" ~ "Homens",
      "Women" ~ "Mulheres",
      "Mixed" ~ "Misto"
    ),
    evento_medalha = case_match(
      evento_medalha,
      "ME_GOLD" ~ "Ouro",
      "ME_SILVER" ~ "Prata",
      "ME_BRONZE" ~ "Bronze"
    )
  )

## Salva os dados
write.csv(tidyData, "data/medals.csv", row.names = FALSE)
