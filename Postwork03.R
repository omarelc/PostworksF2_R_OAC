# Postwork Sesión 3

#### Objetivo

#- Realizar un análisis descriptivo de las variables de un dataframe

#### Requisitos

#1. R, RStudio
#2. Haber realizado el prework y seguir el curso de los ejemplos de la sesión
#3. Curiosidad por investigar nuevos tópicos y funciones de R
library(dplyr)

library(DescTools)  #MODA 
library(ggplot2)

library(moments)
#### Desarrollo

"Utilizando el dataframe `boxp.csv` realiza el siguiente análisis descriptivo. No olvides excluir los missing values y transformar las variables a su
tipo y escala correspondiente."
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")
summary(df)

view(df)
complete.cases(df)

sum(complete.cases(df)) # 591 de 615

df.clean <- df[complete.cases(df),]

df.clean$Categoria <- factor(df.clean$Categoria)
df.clean$Grupo <-  factor(df.clean$Grupo, labels=c("No","Si"))

summary(df.clean)
" Categoria  Grupo      Mediciones    
 C1:202     No:444   Min.   :  2.80  
 C2:195     Si:147   1st Qu.: 23.45  
 C3:194              Median : 49.30  
                     Mean   : 62.88  
                     3rd Qu.: 82.85  
                     Max.   :290.60  "

#1) Calcula e interpreta las medidas de tendencia central de la variable `Mediciones`

mean(df.clean$Mediciones)   # 62.88494

mean(df.clean$Mediciones, trim = 0.125) #media truncada  52.5218

mediana <- median(df.clean$Mediciones)  # 49.3

moda <- Mode(df.clean$Mediciones)   # 23.3  repetido 6 veces



#2) Con base en tu resultado anterior, ¿qué se puede concluir respecto al sesgo de `Mediciones`?

"los datos presentaran un sesgo a la derecha"

#3) Calcula e interpreta la desviación estándar y los cuartiles de la distribución de `Mediciones`
sd(df.clean$Mediciones)  # 53.76972
cuartiles <-  quantile(df.clean$Mediciones, probs = c(0.25,0.5,0.75))
cuartiles
"25%     50%    75% 
 23.45  49.30  82.85 "


"4) Con ggplot, realiza un histograma separando la distribución de `Mediciones` por `Categoría`
¿Consideras que sólo una categoría está generando el sesgo?"
ggplot(df.clean, aes(Mediciones)) +
  geom_histogram(bins = 8) + 
  labs(title = "Histograma", 
       x = "Medición",
       y = "Frequency") + 
  theme_classic()

ggplot(df.clean, aes(x=Mediciones, color=Categoria)) +
  geom_histogram(bins=10,fill="white", alpha=0.99, position="dodge") +
  geom_vline(aes(xintercept=mean(Mediciones)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=moda),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mediana),
             color="orange", linetype="dashed", size=1)

"No parece que el sesogo a la derecha lo cause una sola categoría"

"5) Con ggplot, realiza un boxplot separando la distribución de `Mediciones` por `Categoría` 
y por `Grupo` dentro de cada categoría. ¿Consideras que hay diferencias entre categorías? ¿Los grupos al interior de cada categoría 
podrían estar generando el sesgo?"

boxplot(df.clean$Mediciones ~ df.clean$Categoria,  col = c("#FFE0B2", "#FFA726", "#F57C00"))
"todas las categorias aportan valores atipicamente altos"
boxplot(df.clean$Mediciones ~ df.clean$Grupo)
stripchart(df.clean$Mediciones ~ df.clean$Grupo,
           data = df.clean,
           method = "jitter",
           pch = 19,
           col = 2:4,
           vertical = TRUE,
           add = TRUE)
"hay muchos valores atipicamnete altos en el grupo del NO, en el del SI son muchos menos"
 
(boxplot.mediciones <- 
    ggplot(df.clean, aes(Mediciones, x = Mediciones, y = Categoria, fill = Grupo)) + 
    geom_boxplot(outlier.size = 3, outlier.colour = "#E55381", outlier.fill = "#E55381", outlier.alpha = 0.6, outlier.shape = 18) +
    labs(title = "Mediciones por Categoría",x = "Mediciones", y = "Categoría") + 
    coord_flip() + 
    theme_light())
