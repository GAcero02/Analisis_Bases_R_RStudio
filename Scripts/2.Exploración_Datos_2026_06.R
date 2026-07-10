#activamos la libreria tidyverse
library(tidyverse)
library(rio)

#cargamos los datos
datos <- read.csv("Salidas/base_limpia.csv")


# Estructura de la base ---------------------------------------------------
#estructura de la base de datos
dim(datos)
str(datos)
glimpse(datos)
summary(datos)


# Explorar variables categoricas (tabla exploratoria)  --------------------
Tabla_sexo <- datos %>%
  count(edades_agrupadas) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)
  )

Tabla_sexo

#export(Tabla_sexo, "Salidas/Tabla_Sexo.xlsx") #descomentar eto si se requiere una tabla sencilla

# Explorar variables numericas --------------------
summary(datos$edad_en_años)
sd(datos$edad_en_años)


#identificar valores faltantes
sum(is.na(datos))
colSums(is.na(datos))


#identificación de valores extremos
boxplot(datos$edad_en_años)
boxplot.stats(datos$edad_en_años)$out


#Relaciones entre variables (una categoricas y una numerica)
Tabla_etnia_edad <- datos %>%
  group_by(pertenencia_etnica) %>%
  summarise(
    media = mean(edad)
  )

Tabla_etnia_edad
export(Tabla_etnia_edad, "Salidas/Tabla_pertenecia_edad.xlsx") #descomentar eto si se requiere una tabla sencilla
