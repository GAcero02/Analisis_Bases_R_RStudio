#activamos la libreria tidyverse
library(tidyverse)

#cargamos los datos
datos <- read.csv("Salidas/base_limpia.csv")

#estructura de la base de datos
str(datos)
glimpse(datos)

#resumen de los datos
summary(datos)

#numero de filas y columnas
dim(datos)

#explorar variables categoricas
table(datos$sexo)

prop.table(
  table(datos$sexo)
)

#explorar variables numericas
mean(datos$edad)
median(datos$edad)
sd(datos$edad)
range(datos$edad)


#identificar valores faltantes
sum(is.na(datos))
colSums(is.na(datos))

#identificación de valores extremos
boxplot(datos$edad)
boxplot.stats(datos$edad)$out

#relaciones entre variables
datos %>%
  group_by(pertenencia_etnica) %>%
  summarise(
    media = mean(edad)
  )
