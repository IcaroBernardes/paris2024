# 0. Setup inicial ##########
## Carrega bibliotecas
library(dplyr)
library(ggplot2)
library(glue)
library(gt)
library(gtExtras)
library(htmltools)
library(stringr)
library(webshot2)

## Carrega dados brutos
rawData <- read.csv("data/medalTable.csv")

# 1. Processamento dos dados ##########
## Filtra dados do dia mais recente
workData <- rawData |> 
  dplyr::filter(evento_data == max(evento_data))

## Confirma se o Brasil está no top 10 ou é 11º
BRtop <- "BRA" %in% workData$pais_codigo[1:11]

## Filtra os dados do Brasil e top 10
workData <- workData |> 
  dplyr::filter(pais_codigo == "BRA" | row_number() <= 10)

## Define o texto do ranking
workData <- workData |> 
  dplyr::mutate(rank = glue::glue("{rank}º")) |> 
  dplyr::relocate(rank, .before = 1)

## Associa bandeiras aos nomes dos países usando estrutura HTML
workData <- workData|> 
  dplyr::rowwise() |>
  dplyr::mutate(pais_nome = as.character(div(
    img(
      style = "height:20px;border-radius:10px;border:solid 2px black;",
      src = glue::glue("https://gstatic.olympics.com/s1/t_original/static/noc/oly/3x2/180x120/{pais_codigo}.jpg")
    ),
    pais_nome,
    style = "display:flex;align-items:center;gap:10px;"
  ))) |> 
  dplyr::ungroup() |> 
  dplyr::select(-pais_codigo, -evento_data)

## Adiciona um espaço entre o Brasil
## e o resto se ele não estiver no top 11
if (!BRtop) {
  workData <- workData |> 
    dplyr::add_row(.before = nrow(workData)) |> 
    dplyr::mutate(across(
      .cols = everything(),
      .fns = \(x) ifelse(is.na(x), "", x)
    ))
}

## Identifica a linha com dados do Brasil
BRline <- stringr::str_detect(workData$pais_nome, "Brasil") |> which()

## Função que define os títulos das colunas de medalhas
labelMedal <- \(name, icon) {
  as.character(div(
    div(name), span(icon)
  ))
}

## Reescreve a data do dia de coleta do dado
fimDia <- unique(max(rawData$evento_data))
fimDia <- paste0(
  stringr::str_sub(fimDia, 9, 10),
  " de ",
  stringr::str_sub(fimDia, 5, 8)
)
meses <- c("-07-" = "Julho", "-08-" = "Agosto")
fimDia <- stringr::str_replace_all(fimDia, meses)

## Texto de informação do dia de coleta do dado
numDias <- n_distinct(rawData$evento_data)
diaOlimp <- glue::glue("<div>Resultados até {fimDia}</div>
                       Após <strong>{numDias}</strong> dias de Jogos Olimpícos") |> 
  HTML()

## Texto de atribuição dos dados e tabela
atrib <- div(
  div(
    id = "source",
    "Dados e bandeiras extraídos do site oficial dos Jogos Olímpicos Paris 2024"
  ),
  hr(),
  div("Tabela produzida por Ícaro Bernardes"),
  div(
    id = "atrib",
    HTML(local_image("fa-glyphs/x.svg")),
    HTML(local_image("fa-glyphs/instagram.svg")),
    HTML(local_image("fa-glyphs/linkedin.svg")),
    ' - @IcaroBSC',
    HTML(local_image("fa-glyphs/github.svg")),
    ' - @IcaroBernardes'
  )
) |> 
  as.character()

## Gera a tabela
gtTable <- workData |>
  gt() |> 
  tab_header("Quadro de medalhas", diaOlimp) |> 
  tab_footnote(html(atrib)) |> 
  fmt_markdown() |>
  cols_align(align = "center", columns = -pais_nome) |> 
  cols_align(align = "right", columns = rank) |> 
  cols_label(
    rank = "",
    pais_nome = "",
    Ouro = html(labelMedal("Ouro","🥇")),
    Prata = html(labelMedal("Prata","🥈")),
    Bronze = html(labelMedal("Bronze","🥉"))
  ) |> 
  cols_width(
    Ouro:Bronze ~ px(70),
    pais_nome ~ px(250)
  ) |> 
  opt_css(
    css = "
    .gt_title {
      font-size: 40px !important;
      line-height: 1;
      font-weight: bold !important;
    }
    .gt_subtitle {
      font-size: 15px !important;
      line-height: 1.1;
      padding-bottom: 10px !important;
    }
    .gt_col_heading {
      text-align: center !important;
    }
    .gt_col_heading div {
      font-weight: bold;
    }
    .gt_col_heading span {
      font-size: 30px;
    }
    .gt_footnote {
      padding-top: 10px !important;
      font-size: 15px !important;
    }
    #source {
      font-size: 10px !important;
    }
    .gt_footnote img {
      width: 17px;
    }
    #atrib {
      display: flex;
      gap: 5px;
      align-items: center;
    }
    "
  ) |> 
  gt_highlight_rows(rows = BRline, fill = "#91d1aacc")

## Salva a tabela
gtsave(gtTable, "medalTable.png")
