library(tidyverse)
library(data.table)
library(forcats)
library(ggridges)
library(ggplot2)

#Leemos las tablas
games <- read.csv("C:/Users/Micaela/Downloads/Guía 4 - Relational Data-20220919/games.csv")
leagues<- read.csv("C:/Users/Micaela/Downloads/Guía 4 - Relational Data-20220919/leagues.csv")
players <- read.csv(file="C:/Users/Micaela/Downloads/Guía 4 - Relational Data-20220919/players.csv",encoding = "latin1")
teams <- read.csv("C:/Users/Micaela/Downloads/Guía 4 - Relational Data-20220919/teams.csv")
teamstats <- read.csv("C:/Users/Micaela/Downloads/Guía 4 - Relational Data-20220919/teamstats.csv")
shots <- read.csv("C:/Users/Micaela/Downloads/Guía 4 - Relational Data-20220919/shots.csv")
appearances <- read.csv("C:/Users/Micaela/Downloads/Guía 4 - Relational Data-20220919/appearances.csv")



#Tabla con los numero de goles de cada equipo
equipos_mas_goles <- inner_join(teamstats, teams, by="teamID")%>% #para tener los nombres de los equipos
    group_by(name)%>% #agrupo por equipo
    summarise(goles=sum(goals))%>% #los goles totales de cada equipo son la suma de sus goles en cada partido
    arrange(desc(goles)) #ordeno en orden descendiente para que los mejores queden arriba(para el grafico de los 10 mejores)

#Tabla con los numero de tiro al arco de cada equipo
equipos_mas_tiros <- inner_join(teamstats, teams, by="teamID")%>%
    group_by(name)%>%#agrupo por equipo
    summarise(tiros=sum(shots))%>%
    arrange(desc(tiros))  #idem tabla anterior

#Graficos
goles<-ggplot(data=head(equipos_mas_goles,10)) + geom_bar(mapping=aes(x=fct_reorder(name,goles,.desc=TRUE),y=goles),
                                          fill= 'Blue',
                                          stat='identity')

goles + labs(title='Cantidad de goles que hicieron los quipos que mas los tienen',
                      subtitle='De 5 ligas europeas entre los años 2014 y 2020',
                      x='Equipo', y='Goles')

tiros<- ggplot(data=head(equipos_mas_tiros,10)) + geom_bar(aes(x=fct_reorder(name,tiros,.desc=TRUE),y=tiros),
                                                  fill= 'Blue',
                                                  stat='identity')

tiros + labs(title='10 equipos con más tiros al arco realizados',
                      subtitle='De las 5 "mejores" ligas europeas entre los años 2014 y 2020',
                      x='Equipo', y='Tiros al arco')


tiros_goles <-inner_join(equipos_mas_goles,equipos_mas_tiros, by="name")


tiros_goles <-ggplot(tiros_goles,aes(x=tiros,y=goles)) + geom_point(color = "Blue")

tiros_goles + labs(title='Numero de goles por número de tiros',
                    subtitle='Para cada equipo de las 5 "mejores" ligas europeas entre los años 2014 y 2020',
                    x='Tiros al arco', y='Goles')

#ELEGIR UNA LIGA: #elijo la liga
#Quédense con los 5 jugadores que más goles han metido en esa Liga

jugadores_mas_goles <- inner_join(appearances, players, by="playerID")%>%
    filter(leagueID==4)%>%
    group_by(name)%>%#agrupo por jugador
    summarise(goles=sum(goals))%>%#los goles de cada jugador son la suma de sus goles en cada partido 
    arrange(desc(goles))%>%#ordeno de mayor a menor
    head(5) #me quedo con los 5 mejores

#Analicen la distribución de tiempos (en minutos) en los cuales estos jugadores 
#realizaron los goles sin importar en qué club lo hicieron

goleadores <- players%>% #nombres e id de los jugadores
    inner_join(jugadores_mas_goles)%>% #la uno para que me queden solo los 5 con mas goles
    rename(shooterID=playerID)%>% #cambio el nombre de player a shooter para usar con shots
    inner_join(shots)%>% #uno la tabla que me dice los minutos y si fue o no gol
    filter(shotResult=='Goal')%>% #me quedo solo con los goles
    arrange(desc(goles))


goleadores_densidad <- ggplot(goleadores, aes(x = minute, y = fct_reorder(name, goles),fill=name,height = ..density..))+
    geom_density_ridges(alpha=0.5,stat = "density")+
    scale_y_discrete(expand = c(0.01, 0)) +
    scale_x_continuous(breaks = c(0,15,30,45,60,75,95)) +
    theme(legend.position = "none")

goleadores_densidad + labs(title = 'Goles por cada minuto del partido', 
        x = 'Minuto', 
        y = 'Goles')
       