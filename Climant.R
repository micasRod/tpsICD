library(tidyverse)

library(modelr)
library(ggplot2)

options(na.action = na.warn)

clima <- read.csv("C:/Users/annyp/OneDrive/Escritorio/cosas/UNSAM/Cs de datos/Intro a la cs de datos/R/Guia5/Summary of Weather.csv")
clima2 <- read.csv("C:/Users/annyp/OneDrive/Escritorio/cosas/UNSAM/Cs de datos/Intro a la cs de datos/R/Guia5/Weather Station Locations.csv")

glimpse(clima)

clima %>% 
    summarise(distintos=n_distinct(clima), total=n())
#Hay 10 filas repetidas, las elimino con unique

clima <- unique(clima)


clima2 %>% 
    summarise(distintos=n_distinct(clima2), total=n())
#No hay filas repetidas


#UNO LAS DOS TABLAS Y ME QUEDO CON UNA SOLA ESTACION
estacionElegida <- inner_join(clima,clima2,by=c("STA"="WBAN"))%>% 
    filter(STA==10001)

#grafico temp max en funcion de temp min

ggplot(estacionElegida, aes(x=MaxTemp, y=MinTemp))+ 
    geom_point(color = "Tomato")

# la relación parece ser lineal, es decir: y = a_0 + a_1 * x
# Entonces podemos ajustar el modelo encontrando el valor de a_0 
#y a_1 que genera el modelo con la menor distancia a estos datos.

#transformamos nuestra familia de modelos en una función de R

mod <- lm(MinTemp~MaxTemp, data=estacionElegida)

pred <-add_predictions(estacionElegida,mod)

grid <- estacionElegida %>%
    add_predictions(mod)

ggplot(pred, aes(x=MaxTemp)) +
    geom_point(aes(y = MinTemp)) +
    geom_line(aes(y = pred), data = grid, colour = "Orange", size = 1)


#hacer lo mismo para todas las estaciones
#UNO LAS DOS TABLAS Y
estacionesTodas <- inner_join(clima,clima2,by=c("STA"="WBAN"))%>%
    filter(MaxTemp>=MinTemp)

#grafico temp max en funcion de temp min

ggplot(estacionesTodas, aes(x=MaxTemp, y=MinTemp, colour="STA"))+ 
    geom_point()


# la relación parece ser lineal, es decir: y = a_0 + a_1 * x
# Entonces podemos ajustar el modelo encontrando el valor de a_0 
#y a_1 que genera el modelo con la menor distancia a estos datos.

#transformamos nuestra familia de modelos en una función de R

mod2 <- lm(MinTemp~MaxTemp, data=estacionesTodas)

pred2 <-add_predictions(estacionesTodas,mod2)

grid2 <- estacionesTodas%>%
    add_predictions(mod2)

ggplot(pred2, aes(x=MaxTemp, colour=STA)) +
    geom_point(aes(y = MinTemp)) +
    geom_abline(intercept=0, slope=1,colour="Red")+
    geom_line(aes(y = pred), data = grid2, colour = "Purple", size = 1)

