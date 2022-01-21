setwd("~/Desktop/Santo Tomás/TSO_analisis/Análisis de texto")

load("~/Desktop/Santo Tomás/TSO_analisis/Bases/Nodos.rda")


############# Depuración Texto de contenidos ###################

library(tidytext)
library(tidyverse)
library(stringr)

l <-  length(nodos$Label)

texto_tb <- tibble(line=1:l,
                   text = nodos$Label)

stop_words <- tibble(read.csv("~/Desktop/Santo Tomás/TSO_analisis/Bases/stop_word.csv", header = FALSE)) %>%
  mutate(word = as.character(V1)) %>% 
  select(word)

letras <- tibble(word = letters)

palabras_vacias <- full_join(stop_words,letras)

aux <- texto_tb %>%
  unnest_tokens(word, text) %>% 
  anti_join(palabras_vacias)

List_palabras <- list(aux)

label_cont <- c()

for(i in 1:l){
  ad <- aux %>% 
    filter(line == i)
  
  label_cont[i] <- paste(ad$word, sep =, collapse = " ")
  
}

setwd("~/Desktop/Santo Tomás/TSO_analisis/Bases")
save(label_cont, file = "Etiquetas_Contenidos.rda")


library(tokenizers)

base_url <- "https://programminghistorian.org/assets/basic-text-processing-in-r"
url <- sprintf("%s/sotu_text/236.txt", base_url)
texto <- paste(readLines(url), collapse = "\n")

palabras <- tibble(word = as.character(unlist(tokenize_words(label_cont))))

palabras_frecuentes <- read_csv(sprintf("%s/%s", base_url, "word_frequency.csv"), show_col_types = FALSE)
palabras_frecuentes
table(palabras_frecuentes$language)

tabla0 <- inner_join(palabras, palabras_frecuentes)
tabla0 %>% arrange(frequency)

tabla1 <- tabla0 %>% 
  count(word) %>% 
  arrange(desc(n))

dim(tabla1)

tabla <- filter(tabla0, frequency<0.002) ### Aparecen más de dos veces en cada 100000 palabras

