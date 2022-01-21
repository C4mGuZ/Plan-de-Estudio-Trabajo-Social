# Subredes ####
setwd("~/Desktop/Santo Tomás/TSO_analisis/Bases")
load("Red_inter3.rda")
load("Intersecciones.rda")
load("Nodos.rda")
names(nodos)

frec_inter <- aristas3 %>%
  filter(n_inter>=4) %>% 
          group_by(Interseccion) %>% 
          summarise(Frec = n()) %>% 
          arrange(desc(Frec)) %>% 
          mutate(n_inter = str_count(Interseccion, '\\w+'),
                 Interseccion = as.character(Interseccion)) 

library(openxlsx)
write.xlsx(frec_inter, "Frecuencias Intersección.xlsx")

frec_inter$Interseccion

get_nodos <- function(clave){
  sub <- aristas3 %>% 
    filter(Interseccion %>% str_detect(clave))
  
  ind <- unique(cbind(sub$Source,sub$Target))
  return(nodos %>% slice(ind))
}

get_nodos(frec_inter$Interseccion[4]) %>% 
  select(Label)

get_subred <- function(clave, color, forma){
  sub <- aristas3 %>% 
    filter(Interseccion %>% str_detect(clave))

    red <- network::as.network(sub, directed = FALSE, 
                             vertices = nodos,
                             verte.attrnames = colnames(nodos), 
                             matrix.type = "edgelist")
  plot <- ggnet2(network::get.inducedSubgraph(
    red,
    v=which(sna::degree(red)>0)),
    #       layout.par = "kamadakawai", 
    color = color,
    palette = "Set1",
    node.size = 12,
    edge.size = "weight", 
    #                    edge.color = "Interseccion",
    edge.alpha = 0.5,
    label = TRUE,
    shape = forma,
    label.size = 5) +
    labs(title = paste("Intersección por ", clave))
  
  return(plot)
}

get_subred("analisis politicas sociales","Asignatura","Tipo")
nrow(get_data("sistema proteccion social chile"))

df <- aristas3 %>%
  filter(n_inter>=4) %>% 
  group_by(Interseccion) %>% 
  summarise(Frec = n()) %>% 
  arrange(desc(Frec))

num <- length(df$Interseccion)

num_nodos <- numeric()

for(i in 1:num){
  df_aux <- get_data(df$Interseccion[i])
  num_nodos[i] <- nrow(df_aux)
}

nrow(get_data(df$Interseccion[1]))
get_subred(df$Interseccion[1], "Tipo", "Asignatura")


library(tidyverse)

gfrec3 <- aristas3 %>%
            filter(n_inter>=3) %>% 
            group_by(Interseccion) %>% 
            summarise(Frec = n()) %>% 
            arrange(desc(Frec)) %>% 
            mutate(Interseccion = as.character(Interseccion)) %>% 
            slice(1:60) %>% 
            ggplot(aes(x = reorder(Interseccion,Frec), y = Frec, fill = Frec)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = Frec, vjust = 0.4, hjust = -0.2), size = 3) +
            coord_flip() +
            theme_light() +
            labs(y = "",
                 x = "",
                 fill = "n inter",
                 title = "Más de 3 palabras")  +
            theme(legend.position = "none",
                  axis.text.x = element_blank(),
                  plot.title = element_text(size=20),
                  axis.text.y = element_text(size=16))

gfrec4 <- aristas3 %>%
  filter(n_inter>=4) %>% 
  group_by(Interseccion) %>% 
  summarise(Frec = n()) %>% 
  arrange(desc(Frec)) %>% 
  mutate(Interseccion = as.character(Interseccion)) %>% 
  slice(1:30) %>% 
  ggplot(aes(x = reorder(Interseccion,Frec), y = Frec, fill = Frec)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frec, vjust = 0.4, hjust = -0.2), size = 5) +
  coord_flip() +
  theme_light() +
  labs(y = "",
       x = "",
       fill = "n inter",
       title = "Frases en común con más de 4 palabras")  +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size=20))

gridExtra::grid.arrange(gfrec3,gfrec4, nrow = 1) 

get_subred("identificación sw spss aplicaciones", "Asignatura", "Unidad") +
  theme(plot.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

nodos %>% 
  filter(Label %>% str_detect("cuestion social"))

aristas3 %>%
  filter(n_inter>=2) %>% 
  group_by(Interseccion) %>% 
  summarise(Frec = n()) %>% 
  arrange(desc(Frec)) %>% 
  mutate(Interseccion = as.character(Interseccion)) %>% 
  filter(Interseccion %>% str_detect("cuestion social"))

edges_inter7 <- aristas3 %>%
  filter(n_inter>=8) 

red_inter7 <- network::as.network(edges_inter7, directed = FALSE, 
                           vertices = nodos,
                           verte.attrnames = colnames(nodos), 
                           matrix.type = "edgelist")
red_inter8 <- network::get.inducedSubgraph(
  red_inter7,
  v=which(sna::degree(red_inter7)>0))


ggnet2(red_inter8,
  #       layout.par = "kamadakawai", 
  color = "Linea_formativa",
  palette = "Set1",
  node.size = 15,
  edge.size = "weight", 
  #                    edge.color = "Interseccion",
  edge.alpha = 0.5,
  label = TRUE,
  shape = "Tipo",
  edge.label = "n_inter",
  edge.label.size = 5,
  edge.label.color = "darkred",
    label.size = 5) +
  labs(title = "") +
  guides(shape = "none") +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size=20)) 

comparar_enlaces(514,268)
nodos %>% 
  slice(514,268)


### Otra subred

edges_peso06 <- aristas3 %>%
  filter(n_inter<8, weight>0.8) 

red_peso06 <- network::as.network(edges_peso06, directed = FALSE, 
                                  vertices = nodos,
                                  verte.attrnames = colnames(nodos), 
                                  matrix.type = "edgelist")
red_peso06 <- network::get.inducedSubgraph(
  red_peso06,
  v=which(sna::degree(red_peso06)>0))


ggnet2(red_peso06,
       #       layout.par = "kamadakawai", 
       color = "Linea_formativa",
       palette = "Set1",
       node.size = 10,
       edge.size = "weight", 
       #                    edge.color = "Interseccion",
       edge.alpha = 0.5,
       label = TRUE,
       shape = "Tipo",
       edge.label = "n_inter",
       edge.label.size = 5,
       edge.label.color = "darkred",
       label.size = 5) +
  labs(title = "") +
  guides(shape = "none") +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size=20))

nodos %>% 
  slice(359,835) %>% 
  select(id,Label)
