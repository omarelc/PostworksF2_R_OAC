"Postwork Sesión 5
OBJETIVO
Realizar inferencia estadística para extraer información de la muestra que sea contrastable con la población
DESARROLLO
El data frame iris contiene información recolectada por Anderson sobre 50 flores de 3 especies distintas 
(setosa, versicolor y virginca), incluyendo medidas en centímetros del largo y ancho del sépalo así
como de los pétalos.

Estudios recientes sobre las mismas especies muestran que:
  "# Ho : <=, =, >=
#  Ha : >, =!, <
#  nivel de confianza:              90%, 95%,  99%
#  nivel de significacia : (1 -NC): 0.1, 0.05, 0.01"
library(dplyr)
df <- iris
View(df)

# Nivel de Confianza: 99% -> 0.01
nc <- 0.01

"1 En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual a 5.7 cm"
#  Planteamiento de hipótesis:
# Ho: prom_sepal_length_setosa == 5.7
# Ha: prom_sepal_length_setosa =! 5.7

t.test(iris[iris$Species == 'setosa', "Sepal.Length"], 
       alternative = 'two.sided', mu=5.7)

# A nivel de confianza estándar de 99%,  existe evidencia estadística 
# para rechazar Ho, es decir El promedio  es distinto a 5.7.

"E J E R C I C I O __ 2"
#En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es menor a 2.1 cm

"Planteamiento de hipótesis:"
# Ho: mu >= 2.1
# Ha: mu < 2.1  # colA INFERIOR

virginica <- filter(df,Species =='virginica')

t.test(x= virginica$Petal.Width, alternative = "less", mu = 2.1)

"data:  virginica$Petal.Width
t = -1.9052, df = 49, p-value = 0.03132
alternative hypothesis: true mean is less than 2.1
95 percent confidence interval:
    -Inf 2.09112
sample estimates:
mean of x 
    2.026 "

######
# A nivel de confianza del 99%, Existe evidencia estadística para rechazar Ho, es decir,
# El promedio NO es mayor o igual a 2.1
#####

"E J E R C I C I O __ 3"
#En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande que el promedio 
#del largo del pétalo de la especie versicolor.

"Planteamiento de hipótesis:
Ho: prom_largoPetalo_virginica <= prom_largoPetalo_versicolor 
Ha: prom_largoPetalo_virginica > prom_largoPetalo_versicolor"

versicolor <- filter(df,Species =='versicolor')
mean(virginica$Petal.Length)   #5.552
mean(versicolor$Petal.Length)  #4.26
sd(virginica$Petal.Length);sd(versicolor$Petal.Length)  #[1] 0.5518947 #[1] 0.469911

diferencias <-(virginica$Petal.Length - versicolor$Petal.Length)
mean(diferencias) #1.292

mean(virginica$Petal.Length)  - mean(versicolor$Petal.Length)  # 1.292

var.test(virginica$Petal.Length, versicolor$Petal.Length, ratio = 1,
         alternative = "two.sided",
         conf.level = 0.99)

t.test(x=virginica$Petal.Length,
       y= versicolor$Petal.Length,
      alternative = "greater", mu=1.1, var.equal = FALSE)

#  nivel de confianza:              90%, 95%,  99%
#  nivel de significacia : (1 -NC): 0.1, 0.05, 0.01
# A nivel de confdianza del 99% existe evidencia para rechazar la hipotesis Ho
# es decir que en promedio el petalo virginica no es 1.1 cm más largo que el versicolor



"E J E R C I C I O __ 4"
#En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies.
# Hipótesis:
# mu0 = mu1 = mu2
# mu0 != mu1 != mu2

setosa <- filter(df,Species =='setosa')
mean(setosa$Sepal.Width); mean(virginica$Sepal.Width); mean(versicolor$Sepal.Width)
# 3.428        2.974           2.77
sd(setosa$Sepal.Width); sd(virginica$Sepal.Width); sd(versicolor$Sepal.Width)
#0.379   0.322    0.314

x <- unlist(iris[iris$Species == 'virginica', 'Sepal.Width'], use.names = FALSE) 
y <- unlist(iris[iris$Species == 'versicolor', 'Sepal.Width'], use.names = FALSE)
z <- unlist(iris[iris$Species == 'setosa', 'Sepal.Width'], use.names = FALSE)

samples <- data.frame(y = c(x,y,z),
                      group = c('virginica', 'versicolor', 'setosa'))

m <- lm(y ~ group, data = samples)
anova(m)
summary(m)
p.value <- anova(m)$'Pr(>F)'[1]

if (p.value >= nc) { 
  cat('R: No Se Rechaza Ho:', p.value, '>=', nc)
} else {
  cat('R: Se Rechaza: Ho', p.value, '<', nc)
} 

par(mfrow=c(2, 2))
plot(m)

#No se rechaza Ho

"E J E R C I C I O __ 5"
#Utilizando pruebas de inferencia estadística, concluye si existe evidencia suficiente para concluir 
#que los datos recolectados por Anderson están en línea con los nuevos estudios.


#Utiliza 99% de confianza para toda las pruebas, en cada caso realiza el planteamiento de hipótesis adecuado y concluye.