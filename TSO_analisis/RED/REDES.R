### Crear RED ####

setwd("~/Desktop/Santo Tomás/TSO_analisis/Bases")

load("Nodos.rda")
load("Intersecciones.rda") ## aristas
load("Enlaces_RA.rda")


# statnet::update_statnet()
library(statnet)
library(network)
library(GGally)
library(RColorBrewer)


## Red por contenido y resultado de aprendizaje
red_ra <-  network::as.network(enlaces_ra, directed = TRUE, 
                                     vertices = nodos,
                                     verte.attrnames = colnames(nodos), 
                                     matrix.type = "edgelist")

ggnet2(red_ra,
#       layout.par = "kamadakawai", 
       color = "Linea_formativa",
       palette = "Set1",
       node.size = 5,
       edge.size = 3, 
       edge.color = "black",
       edge.alpha = 0.8,
      arrow.size = 2, arrow.gap = 0.01,
       label = TRUE,
       shape = "Tipo",
       label.size = 3)

grafico_ra <- function(stri){ 

red_aux <- get.inducedSubgraph(red_ra,
                                    v = which(red_ra %v% "Linea_formativa" == stri))

g <- ggnet2(red_aux,
            #       layout.par = "kamadakawai", 
            color = "Asignatura",
            palette = "Set2",
            node.size = 7,
            edge.size = 1.5, 
            edge.color = "black",
            edge.alpha = 0.8,
            arrow.size = 2, arrow.gap = 0.01,
            label = TRUE,
            shape = "Tipo",
            label.size = 3) +
  labs(title = stri) +
  theme(plot.title = element_text(size = 20)) 

return(g)}


g1_ra <- grafico_ra("Ciencias Sociales") +
   guides(shape = "none") 
g2_ra <- grafico_ra("Disciplinar") +
  guides(shape = "none") 
g3_ra <- grafico_ra("Investigación") +
  guides(shape = "none") 
g4_ra <- grafico_ra("Práctica") +
  guides(shape = "none") 
g5_ra <- grafico_ra("Politicas Sociales")

g1 <- gridExtra::grid.arrange(g1_ra,g2_ra, nrow = 1)
g2 <- gridExtra::grid.arrange(g3_ra,g4_ra, nrow = 1)

# ggsave("politicas_sociales.png", g5_ra, width = 4, height = 4)
# ggsave("ciencias_sociales.png", g1_ra, width = 4, height = 4)
# ggsave("disciplinar.png", g2_ra, width = 4, height = 4)
# ggsave("investigación.png", g3_ra, width = 4, height = 4)
# ggsave("práctica.png", g4_ra, width = 4, height = 4)

gridExtra::grid.arrange(g1_ra,g2_ra)


### Red Completa para intersecciones de más de tres palabras####
library(tidyverse)
aristas3 <- aristas %>%
  filter(n_inter>=3)

red_inter <-  network::as.network(aristas3, directed = FALSE, 
                                     vertices = nodos,
                                     verte.attrnames = colnames(nodos), 
                                     matrix.type = "edgelist")

red_inter <- get.inducedSubgraph(red_inter,
                                    v = which(degree(red_inter)>0))
summary(red_inter)

ggnet2(red_inter,
       #       layout.par = "kamadakawai", 
       color = "Linea_formativa",
       palette = "Set1",
       node.size = 8,
       edge.size = 1.5, 
       edge.color = "black",
       edge.alpha = 0.8,
       #       arrow.size = 2, arrow.gap = 0.01,
       label = TRUE,
       shape = "Tipo",
       label.size = 3) +
  labs(title = "Enlaces entre contenidos con más de tres palabras en común") +
  theme(plot.title = element_text(size = 20),
        legend.title = element_text(size = 16))

# save(red_inter, file = "Red_inter3.rda")

t1 <- mixingmatrix(red_inter, "Linea_formativa")

mosaicplot(t1,
          color = brewer.pal(5, "Set1"),
          las = 1,
          xlab = "",
          ylab = "",
          main = "")


### Subred Pesos 1
edges_sr1 <- aristas %>% 
  filter(weight == 1L, n_inter>2) %>% 
  arrange(desc(n_inter))

dim(edges_sr1)

edges_sr1 %>% 
  select(Interseccion) %>% 
  table()

subred_peso1 <-  network::as.network(edges_sr1, directed = FALSE, 
                                     vertices = nodos,
                                     verte.attrnames = colnames(nodos), 
                                     matrix.type = "edgelist")
subred_peso1 <- get.inducedSubgraph(subred_peso1, 
                                    v = which(degree(subred_peso1)>0))
# subred_peso1 <- get.inducedSubgraph(red_inter,
#                                     e = which(red_inter %e% "weight"== 1L))

subred_peso1

network::network.size(subred_peso1)
network::network.edgecount(subred_peso1)

ggnet2(subred_peso1,
       #       layout.par = "kamadakawai", 
       color = "Linea_formativa",
       palette = "Set1",
       node.size = 8,
       edge.size = 1.5, 
       edge.color = "black",
       edge.alpha = 0.8,
#       arrow.size = 2, arrow.gap = 0.01,
       label = TRUE,
       shape = "Tipo",
       label.size = 3) +
  labs(title = "Enlaces de contención")


comparar_enlaces(835,359)

t1 <- mixingmatrix(subred_peso1, "Linea_formativa") 

kableExtra::kable(t1, "pipe")

# mosaicplot(t1,
#            color = brewer.pal(5, "Set1"),
#            las = 1,
#            xlab = "",
#            ylab = "",
#            main = "Cantidad ")

ggnet2(subred_peso1,
       #       layout.par = "kamadakawai", 
       color = "Linea_formativa",
       palette = "Set1",
       node.size = 10,
       edge.size = 0.8, 
       edge.color = "black",
       edge.alpha = 0.5,
       edge.label = "n_inter",
       edge.label.size = 5,
       edge.label.color = "darkred",
       label = TRUE,
       shape = "Tipo",
       label.size = 4) +
  theme(plot.title = element_text(size = 20))

mixingmatrix(red_completa, "Linea_formativa")

mosaicplot(mixingmatrix(red_completa, "Linea_formativa"), 
           color = brewer.pal(5,"Set1"),
           main = "Enlaces entre Líneas Formativas",
           las = 1,
           xlab = "",
           ylab = "")
text(1.5, 1+0.1, labels=1.5, cex=1)

network::list.network.attributes(red_completa)
network::list.edge.attributes(red_completa)
network::list.vertex.attributes(red_completa)

names(aristas)
names(nodos)

ggnet2(red_completa,
       #       layout.par = "kamadakawai", 
       color = "Linea_formativa",
       palette = "Set1",
       node.size = 5,
       edge.size = "weight", 
       edge.color = "black",
       edge.alpha = 0.5,
       #                    arrow.size = 2, arrow.gap = 0.01,
       label = TRUE,
       shape = "TIPO",
       label.size = 3)

# save(red_completa, file = "Red_Completa.rda")

### Subred 1: n_inter mayor que 6 y peso mayor o igual al 0.5 ####

red_reducida <-  get.inducedSubgraph(red_completa,
                                     e = which(red_completa %e% "n_inter">=6,
                                                 red_completa %e% "weigth" >= 0.5))
red_reducida <- get.inducedSubgraph(red_reducida,
                                    v = which(degree(red_reducida)>0))

save(red_reducida, file = "Subred_nmayor6_weightmayor05.rda")

ggnet2(red_reducida,
       #       layout.par = "kamadakawai", 
       color = "Linea_formativa",
       palette = "Set1",
       node.size = 8,
       edge.size = "weight", 
       edge.color = "black",
       edge.alpha = 0.5,
       #                    arrow.size = 2, arrow.gap = 0.01,
       label = TRUE,
       shape = "Tipo",
       label.size = 3)

mixingmatrix(red_reducida, "Linea_formativa")

comparar_enlaces(514,268)

### Subred 2: weight mayor o igual 0.5

red_reducida1 <-  get.inducedSubgraph(red_completa,
                                     e = which(red_completa %e% "weight">=  0.5))
red_reducida1 <- get.inducedSubgraph(red_reducida1,
                                     v = which(degree(red_reducida1)>0))

mixingmatrix(red_reducida1, "Linea_formativa")

ggnet2(red_reducida1,
       #       layout.par = "kamadakawai", 
       color = "Linea_formativa",
       palette = "Set1",
       node.size = 8,
       edge.size = "weight", 
       edge.color = "black",
       edge.alpha = 0.5,
       #                    arrow.size = 2, arrow.gap = 0.01,
       label = TRUE,
       shape = "Tipo",
       label.size = 3)




