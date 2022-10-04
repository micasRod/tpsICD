# Trabajar con el dataset de arboles
#ver la relcion altura/ancho de los arboles

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

#solo la comuna 12 que es la que mas arboles tiene
tabla_inicial <-tabla_inicial%>%
    filter(comuna==12)

# Hago el plot para todos los arboles

mod <- lm(altura_arbol~diametro_altura_pecho, data=tabla_inicial)

pred <-add_predictions(tabla_inicial,mod)

grid <- tabla_inicial%>%
    add_predictions(mod)

ggplot(pred, aes(x=diametro_altura_pecho)) +
    geom_point(aes(y = altura_arbol),colour='Tomato') +
    geom_line(aes(y = pred), data = grid, colour = "Black", size = 1)

#PARA TODOS LOS ARBOLES LOS COEFICIENTES SON :
#(Intercept) diametro_altura_pecho 
#4.3085456             0.1414695 

mod2 <- lm(altura_arbol~diametro_altura_pecho + nombre_cientifico , data=tabla_inicial)

pred2 <-add_predictions(tabla_inicial,mod2)

grid <- mod2 %>%
    data_grid(diametro_altura_pecho, nombre_cientifico) %>%
    gather_predictions(mod2)


ggplot(pred2, aes(x=diametro_altura_pecho)) +
    geom_point(aes(y = altura_arbol),colour='Tomato') +
    geom_line(aes(y = pred), data = grid2, colour = "Black", size = 1)

