setwd("~/Desktop/Santo Tomás/TSO_analisis/Bases")

library(tidyverse)


load("Nodos.rda")
load("Etiquetas_Contenidos.rda")

nodos$Label[771]
label_cont[771]

interseccion <- function(i,j){
  inter <-  paste(Reduce(intersect,
                         strsplit(c(label_cont[i],
                                    label_cont[j]), 
                                  " ")), collapse = " ")
  return(inter)
}

l <- length(nodos$Label)

coincidencia <- matrix(rep(0,l*l), nrow = l)

for(i in 1:l){
  for(j in i:l)
    coincidencia[i,j] <- interseccion(i,j)
}

coincidencia[1,10]
dim(coincidencia)

setwd("~/Desktop/Santo Tomás/TSO_analisis/Bases")
save(coincidencia, file = "Matriz_intersecciones.rda")

lista_palabras_unicas <- sapply(strsplit(label_cont, " "), unique)

attr_edges_interseccion <- function(i,j){
  a <- length(lista_palabras_unicas[[i]])
  b <- length(lista_palabras_unicas[[j]])
  
  d <- length(Reduce(intersect, strsplit(c(label_cont[i],label_cont[j])," ")))
  return(d/min(a,b))
}

fct1 <- function(x1, x2){
  out <- numeric(length = length(x1))
  for(i in seq_along(x1)) {
    out[i] <- attr_edges_interseccion(x1[i],x2[i])
  }
  return(out)
}

x1 <- 1:l
x2 <- 1:l

matriz_attr_inter <- outer(x1,x2,fct1)
diag(matriz_attr_inter) <- 0

setwd("~/Desktop/Santo Tomás/TSO_analisis/Bases")
save(matriz_attr_inter, file = "Atributo1_matriz_inter.rda")

aristas <- which(matriz_attr_inter>0.6, arr.ind = TRUE) %>% 
  as_tibble() %>% 
  rename(Source = row, Target = col)

coincidencia[lower.tri(coincidencia)] <- ""
diag(coincidencia) <- ""

edges_1 <- which(coincidencia != "", arr.ind = TRUE)

n <- dim(edges_1)[1]
atributo_peso <- rep(0,n)
aux1 <- rep(0,n)

for(i in 1:n){
  input <- as.numeric(edges_1[i,])
  aux1[i] <- coincidencia[input[1],input[2]]
  atributo_peso[i] <-  matriz_attr_inter[input[1],input[2]]
  
}

atributo_ninter <- sapply(strsplit(aux1, " "), length)

aristas <- edges_1 %>% 
  as_tibble() %>% 
  rename(Source = row, Target = col) %>% 
  mutate(Interseccion = aux1,
         n_inter = atributo_ninter,
         Relacion = rep("C-C", n),
         weight = atributo_peso)

library(openxlsx)
setwd("~/Desktop/Santo Tomás/TSO_analisis/Bases")
write.xlsx(aristas, "Aristas.xlsx")


#  Exploración
aristas %>% 
  mutate(n_inter = as.factor(n_inter)) %>% 
  select(n_inter) %>% 
  table()

# Filtro Final

aristas1 <- aristas %>% 
  filter(n_inter>6) %>% 
  mutate(n_inter = as.factor(n_inter))

save(aristas1, file="Enlaces.rda")

aristas1[18,]

library(openxlsx)
write.xlsx(aristas1, "Enlaces.xlsx", overwrite = TRUE)
save(aristas, file = "Intersecciones.rda")

comparar_enlaces(10,953)

comparar_enlaces <- function(i,j){
  return(c(label_cont[i],
           label_cont[j],
           nodos$Label[i],
           as.character(nodos$Label[j]),
           #           matriz_attr_inter[i,j],
           interseccion(i,j),
           RecordLinkage::levenshteinSim(label_cont[i],label_cont[j])))
}

length(nodos$Label)
length(label_cont)

en_sr1 <- aristas %>% 
  filter(weight == 1L, n_inter>=3) %>% 
  arrange(desc(n_inter))

