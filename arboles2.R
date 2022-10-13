# Trabajar con el dataset de arboles
#ver la relacion altura/ancho de los arboles

# Hagan un modelo lineal simple entre las variables de su dataset. Pueden filtrar las unidades
# para obtener algo más específico y seguramente también tengan que transformar la variable
# independiente

library(tidyverse)
library(forcats)
library(modelr)
library(ggplot2)

#Leemos la tabla
tabla_inicial <- read.csv("C:/Users/annyp/Downloads/arbolado-publico-lineal-2017-2018.csv")%>%
    group_by(comuna)%>%
    mutate(cantidad_arboles=n())
    
#Busco las 5 especies que mas aparecen
especies_mas_recurrentes <- tabla_inicial%>%
    ungroup()%>%
    group_by(nombre_cientifico)%>%
    summarise(cantidad_especie=n())%>%
    arrange(cantidad_especie)%>%
    top_n(5)
    

# me quedo solo con las 5 especies con mas ejemplares en la tabla inicial
#solo la comuna 12 que es la que mas arboles tiene
tabla_inicial <-inner_join(tabla_inicial, especies_mas_recurrentes,by="nombre_cientifico")%>%
    filter(comuna==12)
   

# Hago el plot para todos los arboles
ggplot(tabla_inicial,aes(x=diametro_altura_pecho, y = altura_arbol)) +
    geom_point(aes(colour = nombre_cientifico))

#modelo para aproximacion lineal
#Existen dos posibles modelos que se pueden ajustar a estos datos:
mod <- lm(altura_arbol ~ diametro_altura_pecho, data = tabla_inicial)
mod1 <-lm(altura_arbol ~ diametro_altura_pecho + nombre_cientifico, data = tabla_inicial)
mod2 <-lm(altura_arbol ~ diametro_altura_pecho * nombre_cientifico, data = tabla_inicial)

grid2 <- tabla_inicial %>%
    add_predictions(mod)

grid <- tabla_inicial %>%
    data_grid(diametro_altura_pecho, nombre_cientifico) %>%
    gather_predictions(mod1, mod2)

ggplot(tabla_inicial,aes(diametro_altura_pecho, altura_arbol, colour = nombre_cientifico)) +
    geom_point() +
    geom_line(data = grid, aes(y = pred),size = 1) +
    geom_line(data = grid2, aes(y = pred), colour = "red", size = 1)+
    facet_wrap(~model)+
    labs(title='Aproximacion lineal con dos modelos distintos',
         subtitle='para cada una de las 5 especies con más ejemplares en la comuna 12',
         x='Diametro(cm)', y='Altura(m)')
#la recta roja esla que mejor aproxima todoslos datos


#cada graf por separado
ggplot(tabla_inicial,aes(diametro_altura_pecho, altura_arbol)) +
    geom_point(colour = nombre_cientifico) +
    geom_line(data = grid, aes(y = pred)) +
    facet_grid(model~nombre_cientifico)+
    labs(title='Aproximacion lineal con dos modelos distintos',
         subtitle='para cada una de las 5 especies con más ejemplares en la comuna 12',
         x='Diametro(cm)', y='Altura(m)')
