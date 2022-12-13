"Instrucciones
Utilizando la variable total_intl_charge de la base de datos telecom_service.csv de la sesión 3, 
realiza un análisis probabilístico. Para ello, debes determinar la función de distribución de
probabilidad que más se acerque el comportamiento de los datos. Hint: Puedes apoyarte de medidas 
descriptivas o técnicas de visualización."

library(DescTools)
library(ggplot2)

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")

"medidas de tendencia central"
moda <- Mode(df$total_intl_charge)[1]
media <- mean(df$total_intl_charge)
mediana <- median(df$total_intl_charge)

ds <- sd(df$total_intl_charge)

# Una vez que hayas seleccionado el modelo, realiza lo siguiente:
#  Grafica la distribución teórica de la variable aleatoria total_intl_charge
hist(df$total_intl_charge, main="Histograma total cargos internacionales")

hist(df$total_intl_charge, prob=T, main="Histograma total cargos internacionales")

?hist

curve(dnorm(x, mean = media, sd = ds), from=0, to=5, 
      col='blue', main = "Densidad de Probabilidad Normal",
      ylab = "f(x)", xlab = "X")
abline(v = media, lwd = 0.5, lty = 2)



# ¿Cuál es la probabilidad de que el total de cargos internacionales sea menor a 1.85 usd?

pnorm(1.85, mean=media, sd=ds, lower.tail = T)   #0.1125002


#  ¿Cuál es la probabilidad de que el total de cargos internacionales sea mayor a 3 usd?

pnorm(3, mean=media, sd=ds, lower.tail=F)     #0.3773985


#  ¿Cuál es la probabilidad de que el total de cargos internacionales esté entre 2.35usd y 4.85 usd?

pnorm(4.85, mean=media, sd=ds) - pnorm(2.35, mean=media, sd=ds)    #0.7060114

#  Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales más alto que podría esperar?
qnorm(p=0.48, mean=media, sd=ds)    #2.726777


#  ¿Cuáles son los valores del total de cargos internacionales que dejan exactamente al centro 
# el 80% de probabilidad?
qnorm(p=0.10, mean=media, sd=ds)   #1.798583
qnorm(p=0.90, mean=media, sd=ds)    #3.73058
