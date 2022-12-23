# 1. Carregamento de pacotes ########################
library(dplyr)
library(purrr)
library(polite)
library(httr)
library(rvest)
library(xml2)
library(glue)

# 2. Extração da lista de artigos ########################
## Cria versão polite do GET request
pGET <- polite::politely(httr::GET, robots = FALSE)

## Carrega os links já obtidos
links <- readRDS("dados/links.RDS")

## Busca os links dos artigos até não haver mais conteúdo
encontrado <- TRUE
pagina <- 0
while (encontrado) {
  
  ### Incrementa a página
  pagina <- pagina + 1
  print(glue::glue("Efetuando requisição da página {pagina}"))
  
  ### Faz a requisição GET
  resposta <- pGET(glue::glue("https://www.correio24horas.com.br/noticias/resultado-de-pesquisa/pagina/{pagina}/busca/enchente/ordem/latest/"))
  
  ### Extrai o conteúdo da página de resultados
  conteudo <- httr::content(resposta)
  
  ### Lista os elementos que contêm as notícias
  links_temp <- conteudo %>% 
    rvest::html_elements(".noticia-resultado-busca-responsivo__news__item a") 
  
  ### Verifica se algum resultado foi obtido.
  ### Caso sim, obtém os links e títulos e os adiciona à tibble
  encontrado <- (length(links_temp) > 0)
  if (encontrado) {
    
    links_temp <- links_temp %>% 
      purrr::map_dfr(~xml2::xml_attrs(.)) %>% 
      dplyr::select(-class)
    
    links <- bind_rows(links, links_temp)
    
  }
  
}

## Salva os links
saveRDS(links, "dados/links.RDS")

## Obtém corpo do texto, subtítulo e url da foto (quando disponíveis)
for (i in 1:dim(links)[1]) {
  
  ### Toma a url do artigo
  url <- links$href[i]
  print(glue::glue("Efetuando requisição do {i}o artigo"))
  
  ### Faz requisição para a página do artigo e obtém seu conteúdo
  artigo <- pGET(url) %>% httr::content()
  
  ### Extrai o corpo do texto
  corpo <- artigo %>% 
    rvest::html_elements(".js-mediator-article .bodytext") %>% 
    rvest::html_text() %>% 
    glue::glue_collapse(sep = "\n")
  
  ### Extrai o subtitulo do texto
  subtitulo <- artigo %>% 
    rvest::html_element(".noticias-single__description") %>% 
    rvest::html_text()
  
  ### Extrai a url da foto
  foto <- artigo %>% 
    rvest::html_element(".noticias-single__image-source") %>% 
    xml2::xml_attr("src")
  
  ### Une os resultados em uma tibble
  contents_temp <- tibble(
    href = url,
    subtitulo = subtitulo,
    corpo = corpo,
    foto = foto
  )
  contents <- bind_rows(contents, contents_temp)
  
}

## Salva os conteúdos
saveRDS(contents, "dados/contents.RDS")