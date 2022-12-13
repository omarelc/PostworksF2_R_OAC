"Postwork Sesión 2.
Objetivo
•	Conocer algunas de las bases de datos disponibles en R
•	Observar algunas características y manipular los DataFrames con dplyr
•	Realizar visualizaciones con ggplot
Requisitos
1.	Tener instalado R y RStudio
2.	Haber realizado el prework y estudiado los ejemplos de la sesión.
Desarrollo
1.	Inspecciona el DataSet iris_meaniris` disponible directamente en R. Identifica 
las variables que contiene y su tipo, asegúrate de que no hayan datos faltantes 
y que los datos se encuentran listos para usarse.
se pasa data set a una variable para su análisis"
ds <- iris
"data set con los datos"
ds
"descripción de la estructura"
str(ds)
"primeros 6  filas"
head(ds)
"clase del objeto <- data frame"
class(ds)
"ver que dimensión tiene <- 150 5"
dim(ds)
"ver los datos"
View(ds)
"validar que no haya NA, todos son TRU <- no hay NA"
complete.cases(ds)

"2.	Crea una gráfica de puntos que contenga Sepal.Lenght en el eje horizontal, 
Sepal.Width en el eje vertical, que identifique Species por color 
y que el tamaño de la figura está representado por Petal.Width. 
Asegúrate de que la geometría contenga shape = 10 y alpha = 0.5.
se carga librería de gráficas"
library(ggplot2)
"se ejecuta la función"
ggplot(ds, aes(x = ds$Sepal.Length, y = ds$Sepal.Width, 
              color = ds$Species, size = ds$Petal.Width )) +
  geom_point(shape = 10, alpha = 0.5)

"3.	Crea una tabla llamada iris_mean que contenga el promedio de todas las variables 
agrupadas por Species.
cargamos la libreria"
library(dplyr)
"traemos los datos"
ds

iris_mean <- ds %>%
  group_by(Species) %>%
  summarize(Sepal.Length = mean(Sepal.Length),
            Sepal.Width = mean(Sepal.Width),
            Petal.Length = mean(Petal.Length),
            Petal.Width = mean(Petal.Width))



head(iris_mean)


"4.	Con esta tabla, agrega a tu gráfica anterior otra geometría de puntos 
para agregar los promedios en la visualización. 
Asegúrate que el primer argumento de la geometría sea el nombre de tu tabla y "
# que los parámetros sean shape = 23, size = 4, fill = "black" y stroke = 2. 
" También agrega etiquetas, temas y los cambios necesarios para mejorar tu visualización.

se carga librería de gráficas"
library(ggplot2)
"se ejecuta la función"

graph <- ggplot(ds, aes(x = Sepal.Length, y = Sepal.Width, 
              color = Species, size = Petal.Width )) +
              geom_point(shape = 10, alpha = 0.5)

graph

  
graph <- graph +  
      geom_point(data = iris_mean, shape = 23, size = 4, fill = "black", stroke = 2)
graph  



