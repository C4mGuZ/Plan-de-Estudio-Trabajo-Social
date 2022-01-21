### Gráfico para nodos ####

load("Bases/Nodos.rda")

cont<- nodos %>% 
  filter(Tipo == "Contenido") %>% 
  group_by(Linea_formativa) %>% 
  summarise(Contenidos = n())
cont <- cont$Contenidos


tabla1 <- nodos %>% 
  filter(Tipo=="R_A") %>% 
  group_by(Linea_formativa) %>%
  summarise(R_A = n()) %>% 
  mutate(Asignaturas = c(4,8,7,7,7), 
         Contenidos = cont,
         Total_Nodos = R_A + Contenidos) %>% 
  select(Linea_formativa, Asignaturas, R_A, Contenidos, Total_Nodos)

kableExtra::kable(tabla1, "pipe")

sum(tabla1$Total_Nodos)

tabla2 <- nodos %>% 
  slice(369, 1126) %>% 
  select(-c(Label,Tipo,Unidad))

kableExtra::kable(tabla2, "pipe")

tabla2_cont <- nodos %>% 
  slice(369, 1126) %>% 
  select(id,Label)

kableExtra::kable(tabla2_cont, "pipe")

tabla3 <- mixingmatrix(subred_peso1, "Linea_formativa") 

kableExtra::kable(tabla3, "pipe")

aristas3 %>%
  group_by(Interseccion) %>% 
  summarise(Frec = n()) %>% 
  arrange(desc(Frec)) %>% 
  mutate(n_inter = str_count(Interseccion, '\\w+')) %>% 
  arrange(desc(n_inter))

tabla4 <- nodos %>% 
  slice(514,268) %>% 
  select(-c(Label,Tipo,Unidad))

kableExtra::kable(tabla4, "pipe")

tabla4_cont <- nodos %>% 
  slice(514,268) %>% 
  select(id,Label)
kableExtra::kable(tabla4_cont, "pipe")

tabla5 <- nodos %>% 
  slice(359,835) %>% 
  select(id,Label)

kableExtra::kable(tabla5,"pipe")
tabla5 <- nodos %>% 
  slice(359,835) %>% 
  select(-c(Label,Tipo,Unidad))

tabla6 <- nodos %>% 
  slice(943,1038,955,1055,957,1057) %>% 
  select(id,Label)

kableExtra::kable(tabla6,"pipe")

tabla6 <- nodos %>% 
  slice(943,1038,955,1055,957,1057) %>% 
  select(-c(Label,Tipo,Unidad))

