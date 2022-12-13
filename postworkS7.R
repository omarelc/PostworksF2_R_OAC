"Postwork. Predicciones de la temperatura global
OBJETIVO
Estimar modelos ARIMA y realizar predicciones
DESARROLLO
Utilizando el siguiente vector numérico, realiza lo que se indica:"
  
url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"
Global <- scan(url, sep="")

head(Global)
#Crea una objeto de serie de tiempo con los datos de Global. La serie debe ser mensual comenzado en Enero de 1856
SerieT <- ts(Global, start = c(1856,1), frequency = 12)

(SerieT)
"      Jan    Feb    Mar    Apr    May    Jun    Jul    Aug    Sep    Oct    Nov    Dec
1856 -0.384 -0.457 -0.673 -0.344 -0.311 -0.071 -0.246 -0.235 -0.380 -0.418 -0.670 -0.386
1857 -0.437 -0.150 -0.528 -0.692 -0.629 -0.363 -0.375 -0.328 -0.495 -0.646 -0.754 -0.137
1858 -0.452 -1.031 -0.643 -0.328 -0.311 -0.263 -0.248 -0.274 -0.203 -0.121 -0.913 -0.197
1859 -0.249 -0.041 -0.082 -0.172 -0.085 -0.278 -0.220 -0.132 -0.436 -0.234 -0.288 -0.486
...."
class(SerieT)
# "ts"

mean(SerieT)  #-0.1382628
sd(SerieT)    #0.273536

#Realiza una gráfica de la serie de tiempo anterior de 2005
plot(SerieT, 
     main = "Gráfica de Serie de Tiempo", 
     xlab = "Tiempo",
     sub = "1856 a 2006")

# ALTERNATIVA
plot.ts(SerieT)

#Ahora realiza una gráfica de la serie de tiempo anterior, transformando a la primera diferencia:

x <- w <- SerieT

for(t in 2:1800) x[t] <- x[t-1] + w[t] 

plot(diff(x), type = "l", main = "Primera diferencia", 
     xlab = "t", ylab = expression(x[t]))



#¿Consideras que la serie es estacionaria en niveles o en primera diferencia?
#No es estacionaria

#Con base en tu respuesta anterior, obten las funciones de autocorrelación y autocorrelación parcial?
acf(diff(x))  
pacf(diff(x))


