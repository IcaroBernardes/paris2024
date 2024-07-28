# 0. Setup inicial ##########
## Carrega bibliotecas
library(httr2)
library(xml2)
library(rvest)
library(dplyr)

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
divList <- page |> 
  rvest::html_element('div[data-test-id]')

## Extrai os resultados
abbrev <- divList |> 
  rvest::html_elements('.emotion-srm-5xu01z') |> 
  rvest::html_text()
name <- divList |> 
  rvest::html_elements('.emotion-srm-uu3d5n') |> 
  rvest::html_text()
first <- divList |> 
  rvest::html_elements('.emotion-srm-81g9w1:nth-child(2)') |> 
  rvest::html_text()
second <- divList |> 
  rvest::html_elements('.emotion-srm-81g9w1:nth-child(3)') |> 
  rvest::html_text()
third <- divList |> 
  rvest::html_elements('.emotion-srm-81g9w1:nth-child(4)') |> 
  rvest::html_text()

## Reconstrói a base
df <- dplyr::tibble(
  abrv = abbrev,
  pais = name,
  ouro = first,
  prata = second,
  bronze = third
)

## Inclui total de medalhas
df <- df |> 
  dplyr::mutate(across(
    .cols = -c(abrv, pais),
    .fns = as.numeric
  )) |> 
  dplyr::mutate(total = rowSums(across(where(is.numeric))))

## Inclui ordem e data
df <- df |> 
  dplyr::arrange(desc(ouro), desc(prata), desc(bronze), abrv) |> 
  dplyr::mutate(rank = 1:n(),
                data = as.character(Sys.Date()))

# 2. Incremento da série ##########
## Lê o arquivo histórico
old <- read.csv("data/medals.csv")

## Une os bancos
df <- dplyr::bind_rows(df, old)

## Salva os dados
write.csv(df, "data/medals.csv", row.names = FALSE)
