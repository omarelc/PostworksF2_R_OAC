
"Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre como mejorar las ventas de un producto particular, 
y el conjunto de datos con el que disponemos son datos de publicidad que consisten en las ventas de aquel producto 
en 200 diferentes mercados, junto con presupuestos de publicidad para el producto en cada uno de aquellos mercados 
para tres medios de comunicación diferentes: TV, radio, y periódico. No es posible para nuestro cliente incrementar
directamente las ventas del producto. Por otro lado, ellos pueden controlar el gasto en publicidad para cada uno de 
los tres medios de comunicación. Por lo tanto, si determinamos que hay una asociación entre publicidad y ventas,
entonces podemos instruir a nuestro cliente para que ajuste los presupuestos de publicidad, y así indirectamente 
incrementar las ventas."

"En otras palabras, nuestro objetivo es desarrollar un modelo preciso que pueda ser usado para predecir las ventas sobre
la base de los tres presupuestos de medios de comunicación. Ajuste modelos de regresión lineal múltiple a los datos 
advertisement.csv y elija el modelo más adecuado siguiendo los procedimientos vistos"

# Considera:
  
"Y: Sales (Ventas de un producto)
X1: TV (Presupuesto de publicidad en TV para el producto)
X2: Radio (Presupuesto de publicidad en Radio para el producto)
X3: Newspaper (Presupuesto de publicidad en Periódico para el producto)"

adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")
summary(adv)
head(adv)
View(adv)

round(cor(adv),4)
"             TV  Radio Newspaper  Sales
TV        1.0000 0.0548    0.0566 0.9012
Radio     0.0548 1.0000    0.3541 0.3496
Newspaper 0.0566 0.3541    1.0000 0.1580
Sales     0.9012 0.3496    0.1580 1.0000"

pairs(~ Sales + TV + Radio + Newspaper, 
      data = adv, gap = 0.4, cex.labels = 1.5)

par(mfrow = c(2, 2))


# correlacion para TV
x1 <- adv$TV
y <-  adv$Sales

# Creating the plot
plot(x1, y, pch = 19, col = "lightblue", main = "Ventas - TV")

# Regression line
abline(lm(y ~ x1), col = "red", lwd = 3)


# correlacion para Radio
x2 <- adv$Radio
y <-  adv$Sales

# Creating the plot
plot(x2, y, pch = 19, col = "lightblue", main="Ventas- Radio")

# Regression line
abline(lm(y ~ x2), col = "red", lwd = 3)

# correlacion para Newspaper
x3 <- adv$Newspaper
y <-  adv$Sales

# Creating the plot
plot(x3, y, pch = 19, col = "lightblue", main = "Ventas -Newspaper ")

# Regression line
abline(lm(y ~ x3), col = "red", lwd = 3)
dev.off()

attach(adv)
m1 <- lm(Sales ~ TV)
summary(m1) #Multiple R-squared:  0.8122

m2 <- lm(Sales ~ Radio)
summary(m2)  #Multiple R-squared:  0.1222

m3 <- lm(Sales ~ Newspaper)
summary(m3) #Multiple R-squared:  0.02495

 # +++++++++++++++++++++++ #
m0 <-  lm(Sales ~ TV + Radio + Newspaper)

StanRes2 <- rstandard(m0)

par(mfrow = c(2, 2))

plot(TV, StanRes2, ylab = "Residuales Estandarizados")
plot(Radio, StanRes2, ylab = "Residuales Estandarizados")
plot(Newspaper, StanRes2, ylab = "Residuales Estandarizados")

qqnorm(StanRes2) #compara distribucion normal quantiles, contra la distribucion de los residuos
# la forma de linea de 45° indica que si se aproxima la distribucion de errores con la normal teorica
qqline(StanRes2)

dev.off()

shapiro.test(StanRes2)
"data:  StanRes2
W = 0.97528, p-value = 0.001339"

# significancia  0.1  rechazo Ho
# significancia  0.05 rechazo Ho
# significancia  0.01 rechazo Ho

data <- data.frame(
  TV = c(280, 300, 320, 340),
  Radio = c(60, 80, 100, 120),
  Newspaper = c(50, 70, 90, 100)
)

predict(m0, newdata = data, interval = "confidence", level = 0.95)
" fit      lwr      upr
1 26.30680 25.60519 27.00841
2 29.54245 28.54976 30.53514
3 32.77811 31.47402 34.08219
4 36.01040 34.40453 37.61628"

data2 <- data.frame(
  TV = c(280, 300, 320, 340),
  Radio = c(60, 80, 100, 120),
  Newspaper = c(0, 0, 0, 0)
)

predict(m0, newdata = data2, interval = "confidence", level = 0.95)
" fit      lwr      upr
1 26.29002 25.37994 27.20009
2 29.51896 28.30848 30.72943
3 32.74790 31.22156 34.27423
4 35.97684 34.12708 37.82659"

data3 <- data.frame(
  TV = c(280, 300, 320, 340),
  Radio = c(0, 0, 0, 0),
  Newspaper = c(0, 0, 0, 0)
)


predict(m0, newdata = data3, interval = "confidence", level = 0.95)
"   fit      lwr      upr
1 19.86994 19.25196 20.48792
2 20.95886 20.30538 21.61234
3 22.04777 21.35635 22.73919
4 23.13669 22.40527 23.86811"
