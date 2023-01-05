### LIBRARY

library(tidyverse)
library(shiny)
library(scales)
library(reshape2)
library(stringr)
library(htmltools)
library(shinyWidgets)
library(rlang)
library(leaflet)
library(sf)
library(htmltools)
library(jsonlite)
library(echarts4r)
library(viridis)
library(shinyglide)
library(lubridate)
library(RColorBrewer)
library(colorspace)
library(shinycssloaders)
library(data.table)
library(shinyjs)
library(geobr)

### BANCO DE DADOS

tbl <- read_rds("dados_totais.rds") 

### REMOVENDO DANO MATERIAL NAO VALORADO
unselect_tbl_col <- tbl %>% 
  select(contains("DM")) %>% 
  select(!contains("Valor")) %>% 
  names()

### AJUSTANDO TBL

tbl_2 <- tbl %>% 
  select(-unselect_tbl_col) %>% 
  mutate(across(15:20, as.numeric))

tbl <- tbl_2

### LISTA PARA O SELECT:

select_list <- list(
  `Pessoas afetadas` = "DH", 
  `Dano material` = "DM"
)

### INTERFACE: 

ui <- fluidPage(
  useShinyjs(),
  ## Cria link com arquivo css externo
  # includeCSS("www/style.css"),
  
  ## Inclui fontes do Google na composição do site
  tags$head(
    tags$style(
      "@import url('https://fonts.googleapis.com/css2?family=Roboto:wght@300;700&display=swap');",
      "@import url('https://fonts.googleapis.com/css2?family=Roboto+Slab:wght@300;700&display=swap');"
    )
  ),
  tabsetPanel(
    tabPanel(
      title = "Chuvas na Bahia",
        # Show a plot of the generated distribution
        mainPanel(class = "chuvas",
                  uiOutput("filtro"),
                  uiOutput("select"),
                  echarts4rOutput("grafico_barra"), 
                  leafletOutput('mapa')
                  
        )
      )
    )
  )
  
# SERVIDOR
server <- function(input, output, session) {
    
  ### BOTOES: 
  
  
  # Filtro: 
  
  output$filtro <- renderUI({
    selectizeInput(
      'filtro',
      label = "Filtros",
      choices = unique(tbl$COBRADE),
      selected = unique(tbl$COBRADE), 
      multiple = TRUE
    )
    
  })
  
  updateSelectizeInput(session, 'filtro', 
                       choices = unique(tbl$COBRADE), server = TRUE, 
                       selected = unique(tbl$COBRADE))
  
  # Selecao
  
  output$select <- renderUI({
    selectizeInput(
      'select',
      label = "Variável para o mapa",
      choices = select_list,
      selected = select_list[[1]], 
      multiple = FALSE
    )
    
  })
  
  updateSelectizeInput(session, 'select', 
                       choices = select_list, server = TRUE)
  
  
  
  ### GRAFICO DE BARRAS: 
  
  output$grafico_barra <- renderEcharts4r({
    
    tbl %>% 
      filter(COBRADE %in% input$filtro) %>% 
      group_by(ano) %>% 
      summarise(n = n_distinct(`Município`)) %>% 
      mutate(ano = as.factor(ano)) %>% 
      e_charts(ano) %>% 
      e_bar(n) %>% 
      e_tooltip()
  
    })
  
  
  ### MAPA: 
    I 
  output$mapa <- renderLeaflet({
    

    ### READ STATES
    states <- geobr::read_municipality() %>% 
      filter(abbrev_state == "BA")
    
    # CRIA OS RESULTADOS. SEria interessante aparecer o ano acima do gráfico
    
    if (!is.null(input$grafico_barra_clicked_data[[1]][1])) {
     
       result <- tbl %>%
        filter(COBRADE %in% input$filtro) %>% 
        filter(ano == input$grafico_barra_clicked_data[[1]][1]) %>%
        group_by(`Município`) %>%
        summarise(value = sum(c_across(starts_with(input$select)),
                               na.rm = T)) %>%
        rename(name_muni = `Município`)
      
    } else {
      result <- tbl %>%
        filter(COBRADE %in% input$filtro) %>% 
        filter(ano == 2022) %>%
        group_by(`Município`) %>%
        summarise(value = sum(c_across(starts_with(input$select)),
                                         na.rm = T)) %>%
        rename(name_muni = `Município`)
    
    }
    
    # Juntando mapa e resultados: 
    
    mapa <- states %>% 
      left_join(result)
    
    ## cores: PROBLEMA PARA DANO MATERIAL
    
    qpal <- colorQuantile("Blues", mapa$value, n = 3)
    
    # Criando mapa
    mapa %>%
      leaflet(options = leafletOptions(minZoom = 3)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        smoothFactor = 0.5,
        fillOpacity = 0.6,
        weight = 0.5,
        color = ~ qpal(value),
        opacity = 0.8,
        highlightOptions = highlightOptions(
          color = "black",
          fillOpacity = 0.9,
          weight = 0.5,
          bringToFront = TRUE
        )
      )
    
  })
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)