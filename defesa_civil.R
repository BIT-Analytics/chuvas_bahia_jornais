### DADOS DA DEFESA CIVIL

### library

require(tidyverse)
require(lubridate)

### open: 

all_files <- list.files("dados/defesa/", full.names = TRUE)

db <- all_files %>%
  map_dfr(
    read_csv2,
    col_types = cols(
      "DM_Unidades Habitacionais Valor" =
        col_double(),
      "DM_Obras de infraestrutura pública Valor" =
        col_double(),
      "PEPL_Transportes locais, regionais e de longo curso (R$)" =
        col_double(), 
      "PEPL_Sistema de limpeza urbana e de recolhimento e destinação do lixo (R$)" =
        col_double()
    )
  )

### Ajustando

tbl <- db %>% 
  mutate(Registro = dmy(Registro)) %>% 
  mutate(ano = year(Registro))

