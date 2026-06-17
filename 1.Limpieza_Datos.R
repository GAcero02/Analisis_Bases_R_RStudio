#se activan los paquetes tidyverse, janitor, skimr
library(tidyverse)
library(janitor)
library(skimr)

#cargamos los datos usando la función read.csv de Rbase
datos <- read.csv("Entradas/Base_Prueba.csv")

#damos una revisada a los datos cargados con la función glimpse de tidyverse
glimpse(datos)

#vemos un resumen de las variables
summary(datos)

#miramos los nombres de las variables
names(datos)


# Limpiamos los nombres de las columnas -----------------------------------

#limpiamos los nombres de las variables
datos <- datos %>% clean_names()

#miramos como quedaron los nombres de las variables
names(datos)



# Eliminamos los duplicados -----------------------------------------------
sum(duplicated(datos))

#en caso de haber duplciados los eliminamos (revisar si es necesario o el caso esta duplicado por una razón de peso)
#si hay un identificador pasar a la siguiente linea
#distinc elimina si todas las variables son iguales
datos <- datos %>%  distinct()


# Revisar los faltantes por columna ---------------------------------------
colSums(is.na(datos))

colMeans(is.na(datos))*100


# limpiamos variables categoricas -----------------------------------------

table(datos$sexo)

datos <- datos %>%
  mutate(
    sexo = case_when(
      sexo %in% c("M", "m", "Masculino", "Hombre") ~ "Masculino",
      sexo %in% c("F", "f", "Femenino", "Mujer") ~ "Femenino",
      TRUE ~ sexo
    )
  )

table(datos$sexo)


# Limpiamos las variables cuantitativas -----------------------------------

summary(datos$edad)

datos <- datos %>%
  mutate(
    edad = ifelse(
      edad < 0 | edad > 120,
      NA,
      edad
    )
  )

summary(datos$edad)



# Limpiamos las variables de fecha ----------------------------------------

summary(datos$fecha_de_inicio_de_sintomas)

datos$fecha_de_inicio_de_sintomas <- as.Date(
  datos$fecha_de_inicio_de_sintomas,
  format = "%Y-%m-%d"
)

class(datos$fecha_de_inicio_de_sintomas)

summary(datos$fecha_de_inicio_de_sintomas)


datos$fecha_de_muerte <- as.Date(
  datos$fecha_de_muerte,
  format = "%Y-%m-%d"
)
class(datos$fecha_de_muerte)
summary(datos$fecha_de_muerte)

# Consistencia de fechas --------------------------------------------------

datos <- datos %>%
  mutate(
    inconsistencia =  fecha_de_muerte < fecha_de_inicio_de_sintomas
  )


# reporte sencillo --------------------------------------------------------


skim(datos)


# Exportar una versión limpia de la base ----------------------------------

write.csv(
  datos,
  "Salidas/base_limpia.csv",
  row.names = FALSE
)
