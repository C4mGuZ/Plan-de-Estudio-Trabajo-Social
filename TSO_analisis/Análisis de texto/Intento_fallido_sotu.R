library(usethis)
library(devtools)

#install_github("cbail/textnets", force = TRUE)


library(textnets)
data("sotu")

class(sotu)


sotu %>% glimpse()

sotu_first_speeches <- sotu %>% group_by(president) %>% slice(1L)



prepped_sotu <- PrepText(sotu_first_speeches, 
                         groupvar = "president", 
                         textvar = "sotu_text", 
                         node_type = "groups", 
                         tokenizer = "words", 
                         pos = "nouns", 
                         remove_stop_words = TRUE, 
                         compound_nouns = TRUE)

udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")


sotu_text_network <- CreateTextnet(prepped_sotu)


VisTextNet(sotu_text_network, label_degree_cut = 0, )

setwd("~/Desktop/Santo Tomás/TSO_analisis/Bases")

load("Nodos.rda")

nodos %>% group_by(Linea_formativa)







