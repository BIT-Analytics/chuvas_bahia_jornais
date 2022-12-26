# Library

require(tidyverse)
require(geobr)
library(reticulate)
library(here)

###  instalacoes : 


reticulate::py_config()
use_python("/usr/bin/python3")

transformers <- reticulate::import("transformers")

## MODELOS: 

# https://huggingface.co/pierreguillou/ner-bert-large-cased-pt-lenerbr?text=Ao+Instituto+Médico+Legal+da+jurisdição+do+acidente+ou+da+residência+cumpre+fornecer%2C+no+prazo+de+90+dias%2C+laudo+à+v%C3%ADtima+%28art.+5%2C+§+5%2C+Lei+n.+6.194%2F74++de+19+de+dezembro+de+1974%29%2C+função+técnica+que+pode+ser+suprida+por+prova+pericial+realizada+por+ordem+do+ju%C3%ADzo+da+causa%2C+ou+por+prova+técnica+realizada+no+âmbito+administrativo+que+se+mostre+coerente+com+os+demais+elementos+de+prova+constante+dos+autos.

#https://huggingface.co/Davlan/bert-base-multilingual-cased-ner-hrl?text=إسمي+محمد+وأسكن+في+برلين

# Instantiate a pipeline
ner_tagger <- transformers$pipeline(task = "ner",  
                                    aggregation_strategy = "average",
                                    model = "pierreguillou/ner-bert-large-cased-pt-lenerbr")

### bancos de dado

df <- read_rds("dados/contents.RDS") %>% 
  left_join(read_rds("dados/links.RDS"))

#### emergencia

df_emergencia <- df %>% 
  filter(str_detect(corpo, "situação de emergência|Situação de emergência"))

## testando: 

text <- df_emergencia %>%
  tidytext::unnest_tokens(corpo, output =  paragrafo,  "sentences") %>%
  mutate(paragrafo = tm::removeWords(paragrafo, tm::stopwords("pt")))

result <- list()

for (i in 1:nrow(text)){ 
  
  
  # Make predictions
  outputs <- ner_tagger(text$paragrafo[[i]])
  
  # Convert predictions to tibble
  # This takes some bit of effort since some of the variables are numpy objects 
  
  # Function that takes a list element and converts
  # it to a character
  to_r <- function(idx){
    # Obtain a particular output from entire named list
    output_idx = outputs %>% 
      pluck(idx)
    
    # Convert score from numpy to integer
    output_idx$score = paste(output_idx$score) %>% 
      as.numeric()
    
    return(output_idx)
    
  }
  
  # Convert outputs to tibble
  result[[i]] <- map_dfr(1:length(outputs), ~to_r(.x)) %>% 
    mutate(url = text$href[i])
  
  gc()
  
  
}


tbl <- result %>% 
  plyr::ldply(data.frame)

write_rds(result, "Downloads/textos_jornal.RDS")

