library(tidyverse)
library(forcats)

#Leemos la tabla
tabla_inicial <- read.csv("C:/Users/annyp/Downloads/arbolado-publico-lineal-2017-2018.csv")%>%
    group_by(calle_nombre, calle_altura)%>%
    mutate(cantidad_jacarandas=sum(nombre_cientifico=='Jacaranda mimosifolia', na.rm=TRUE), cantidad_arboles=n())
    
#Tabla con los promedios de altura para los jacaranda de cada comuna
alto_promedio <- tabla_inicial%>%
    group_by(comuna)%>%
    filter(nombre_cientifico=='Jacaranda mimosifolia')%>%
    summarise(alto_promedio=mean(altura_arbol,na.rm=TRUE))

#Tabla filtrada para solo quedarnos son las cuadras que no tienen ningun jacaranda
# que ademas tengan menos de 15 arboles y una acera mas ancha que 5 metros
#de las comunas 2 y 3
tabla_filtrada <- tabla_inicial%>%
    filter(ancho_acera>5)%>%
    group_by(calle_nombre, calle_altura)%>%
    filter(cantidad_jacarandas==0, cantidad_arboles<15, comuna==2 || comuna==3) #me quedo con la comuna 2 y 3


#creamos una tabla con los nombres de las especies
tipo_arboles <- tibble(unique(tabla_filtrada$nombre_cientifico))
write.csv(tipo_arboles,"C:/Users/annyp/Downloads/tipo_arboles.csv")

#importamos una tabla con las especies de arboles que dice si son o no florales
florales <- read.csv("C:/Users/annyp/Downloads/florales.csv")

#Juntamos la tabla arboles que cree con la tabla filtrada
merged <- merge(tabla_filtrada,florales) 

#Voelvemos a filtrar
#las cuadras no tienen ningun arbol floral

tabla_ultima<- merged%>%
    group_by(calle_nombre, calle_altura)%>%
    mutate(tiene_floral=sum(es_floral=='si', na.rm=TRUE))%>%
    filter(tiene_floral==0)

#Vemos que cuadras quedan como posibilidad
cuadras_resultantes <-tabla_ultima%>%
    group_by(calle_nombre, calle_altura)%>%
    mutate(nombre_altura = paste(as.character(calle_nombre),as.character(calle_altura), sep=" "))%>%
    ungroup()%>%
    summarise(nombre_altura,comuna)%>%
    unique()

#Guardamos la tabla con las cuadras resultantes para ponerla en el informe
write.csv(cuadras_resultantes,"C:/Users/annyp/Downloads/cuadras_resultantes.csv")

#Graficos:
tabla_graficos <- read.csv("C:/Users/annyp/Downloads/arbolado-publico-lineal-2017-2018.csv")%>%
    group_by(calle_nombre, calle_altura)%>%
    mutate(cantidad_jacarandas=sum(nombre_cientifico=='Jacaranda mimosifolia', na.rm=TRUE), cantidad_arboles=n())
    

write.csv(tabla_graficos,"C:/Users/annyp/Downloads/tabla_graficos.csv")


x <- 1:15

jacarandas_por_comuna<- ggplot(data=tabla_inicial) + geom_bar(aes(x=factor(comuna), y=cantidad_jacarandas),
                                            fill= 'Tomato',stat='identity')

jacarandas_por_comuna <-jacarandas_por_comuna  + labs(title='Cantidad de Jacarand??s por comuna',
                          subtitle='En la via p??blica en 2017-2018',
                          x='Comuna', y='Ejemplares')

jacarandas_por_comuna


arboles_por_comuna <- ggplot(data=tabla_graficos) + geom_bar(aes(x=factor(comuna), y=cantidad_arboles),
                                            fill= 'cadetBlue3',stat='identity')

arboles_por_comuna <- arboles_por_comuna + labs(title='Cantidad de arboles por comuna',
              subtitle='En la via p??blica en 2017-2018',
              x='Comuna', y='Ejemplares')

arboles_por_comuna

