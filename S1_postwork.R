# postwork
# beca santander\Fase 2\codigoR\S1\postwork
#Del siguiente enlace, descarga los datos de soccer de la temporada 2019/2020 de la primera división de la liga española: 
#https://www.football-data.co.uk/spainm.php

#Importa los datos a R como un Dataframe
library(tidyselect)
library(tidyverse)

datos1 <-  read.csv("~\\beca santander\\Fase 2\\codigoR\\S1\\postwork\\SP1laliga2019.csv")
View(datos1)

(dim.data.frame(datos1)) # 380 observaciones y 105 columnas

"Del dataframe que resulta de importar los datos a R, extrae las columnas que contienen los números de goles anotados
por los equipos que jugaron en casa (FTHG)
y los goles anotados por los equipos que jugaron como visitante (FTAG); guárdalos en vectores separados"



FTHG <- select(datos1, FTHG)
FTHG  
FTAG <- select(datos1, FTAG)
FTAG

# consulta cómo funciona la función table en R. Para ello, puedes ingresar los comandos help("table") 
#o ?table para leer la documentación.

?table

# Responde a las siguientes preguntas: 
#a) ¿Cuántos goles tuvo el partido con mayor empate? 

Empates <-  select(filter(datos1, FTR == "D"), HomeTeam, AwayTeam, FTHG, FTAG, Date)  #buscamos solo los empates FTR= D
view(Empates)

write.csv(Empates, "empates.csv")

EmpateMasGoles <- max(Empates$FTHG) * 2
EmpateMasGoles  # 8 goles

 
  
#b) ¿En cuántos partidos ambos equipos empataron 0 a 0? 

(sum(datos1$FTR == "D" & datos1$FTHG == 0))   #33 empates a cero

#c) ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar que el equipo visitante (AG) metiera un solo gol?

#buscamos victorias de local y visitante cero goles
VisitaCero <-  select(filter(datos1, FTR == "H" & FTAG==0), HomeTeam, AwayTeam, FTHG, FTAG, Date)  
view(VisitaCero)

write.csv(VisitaCero,"LocalGana_visitaCeroGoles.csv")

(MaximosgolesLocal <- max(VisitaCero$FTHG))  #mayor cantidad de goles en un partido del Local es 6

(sum(VisitaCero$FTHG == 6))    #cantidad de pártidos con el local metiendo 6 goles, es 1 partido

(sum(VisitaCero$FTHG == max(VisitaCero$FTHG))) #cantidad de pártidos con el local metiendo 6 goles, es 1 partido

