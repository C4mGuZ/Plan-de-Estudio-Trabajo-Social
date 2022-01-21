
###### Análisis de Texto #########

library(tidyverse)

################ Cargar Base de nodos #################
setwd("~/Desktop/Santo Tomás/TSO_analisis/Bases")

nodos <- data.table::fread("TSO - Nodos.csv") %>% 
  tibble()

nodos <- nodos %>%
  mutate(id = as.integer(id),
         Label = as.character(Label),
         Tipo = as.factor(Tipo),
         Asignatura = as.factor(Asignatura),
         Unidad = factor(Unidad,
                         levels = c("1","2","3","4")),
         Semestre = factor(Semestre,
                           levels = c("1","2","3","4","5","6","7", "8","9")),
         Competencia = as.integer(Competencia),
         Ciclo_formativo = as.factor(Ciclo_formativo),
         Nivel_Dominio_formativo = factor(Nivel_Dominio_formativo,
                                          levels = c("Basico", "Intermedio", "Avanzado")),
         Linea_formativa = as.factor(Linea_formativa),
         Nivel_Logro_Aprendizaje = as.factor(Nivel_Logro_Aprendizaje)
         )

dim(nodos)

repeticiones_ra <- scan() ### Copiar de la hoja enlaces de TSO

ra_rep <- nodos %>%
              filter(Tipo == "R_A") %>%
              select(id) %>%
              mutate(repite = repeticiones_ra)


aux <-rep(ra_rep$id[1], ra_rep$repite[1])

num <- dim(ra_rep)[1]

for(i in 2:num){
  aux <- c(aux,rep(ra_rep$id[i],ra_rep$repite[i]))
}

n_ra_cont <- sum(length(aux),dim(ra_rep)[1])

aux2 <- c(1:n_ra_cont)[-ra_rep$id]

enlaces_ra <- tibble(Source = aux, 
                     Target = aux2)

library(openxlsx)
# write.xlsx(enlaces_ra, "Enlaces_Ra.xlsx", overwrite = T)

save(nodos, file = "Nodos.rda")
save(enlaces_ra, file = "Enlaces_RA.rda")










