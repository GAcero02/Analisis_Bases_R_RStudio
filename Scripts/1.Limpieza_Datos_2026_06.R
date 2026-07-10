# Importamos las librerias necesarias -------------------------------------
#se activan los paquetes tidyverse, janitor, skimr
library(tidyverse)
library(janitor)
library(skimr)
library(rio)
library(stringr)

# Importamos la base de datos ---------------------------------------------
#cargamos los datos usando la función read.csv de Rbase
#datos <- read.csv("Entradas/Base_Prueba.csv") #carga archivos csv
Cod_Departamentos <- import("Bases/Cod_Departamentos.xlsx")
Cod_Municipios <- import("Bases/Cod_Municipios.xlsx")
datos <- import("Entradas/Base_Prueba.csv") # carga archivos xlsx , txt , xls , xlsx , csv
datos <- datos %>%  slice_sample(n = 200000) #sirve para acortar la base para pruebas

# Exploración inicial de la base ------------------------------------------
#damos una revisada a los datos cargados con la función glimpse de tidyverse
glimpse(datos)

#vemos un resumen de las variables
summary(datos)


# Limpiamos los nombres de las columnas -----------------------------------
#miramos los nombres de las variables
names(datos)

#limpiamos los nombres de las variables
datos <- datos %>% clean_names()
names(datos)


# Eliminamos los duplicados -----------------------------------------------
sum(duplicated(datos))

#en caso de haber duplicados los eliminamos (revisar si es necesario o el caso esta duplicado por una razón de peso)
#si hay un identificador pasar a la siguiente linea
#distinc elimina si todas las variables son iguales
datos <- datos %>%  distinct()


# Revisar los faltantes por columna ---------------------------------------
colSums(is.na(datos))
colMeans(is.na(datos))*100


# Hacer las variables categoricas en character ----------------------------
class(datos$codigo_divipola_municipio)

datos <- datos %>%
  mutate(
    across( #aplica una función a multiples columnas
      c(codigo_divipola_municipio, codigo_iso_del_pais),#columnas que se desean modificar
      as.character #función a aplicar a las multiples columnas
    )
  )
class(datos$codigo_divipola_municipio)



# Hacer las variables numercias en numeric ----------------------------
class(datos$edad)

datos <- datos %>%
  mutate(
    across( #aplica una función a multiples columnas
      c(edad),  #columnas que se desean modificar
      as.numeric #función a aplicar a las multiples columnas
    )
  )
class(datos$edad)

# Limpiamos variables categoricas (cambiamos la misma variable)-----------------------------------------
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


# Creamos variables categoricas nuevas dividiendo o multiplicando-----------------------------------------
table(datos$unidad_de_medida_de_edad)
table(datos$edad)

datos <- datos %>%
  mutate(
    edad_en_años = case_when(
      unidad_de_medida_de_edad == 1 ~ edad,
      unidad_de_medida_de_edad == 2 ~ edad/12,
      unidad_de_medida_de_edad == 3 ~ edad/365,
      unidad_de_medida_de_edad == 4 ~ 0.002,
      unidad_de_medida_de_edad == 5 ~ 0.002,
      TRUE ~ NA_real_
    )
  )

table(datos$edad_en_años)
# Creamos variables categoricas nuevas agrupando-----------------------------------------
table(datos$edad_en_años)

datos <- datos %>%
  mutate(
    edades_agrupadas = case_when(
      edad_en_años < 6 ~ "Primera infancia 0-5 años",
      edad_en_años >= 6 & edad_en_años < 12 ~ "Infancia 6-11 años",
      edad_en_años >= 12 & edad_en_años < 18 ~ "Adolescencia 12-17 años",
      edad_en_años >= 18 & edad_en_años < 29 ~ "Juventud 18-28 años",
      edad_en_años >= 29 & edad_en_años < 60 ~ "Adultez 29-59 años",
      edad_en_años >= 60 ~ "Persona mayor ≥60 años",
      TRUE ~ NA_character_
    )
  )

table(datos$edades_agrupadas)


# Ajustamos los números del divipola de los departamentos  ---------------------------------
table(datos$codigo_divipola_departamento)

datos <- datos %>%
  mutate(
    codigo_divipola_departamento = 
      str_pad(as.character(codigo_divipola_departamento), #str_pad del paquete stringr
                               width = 2, #longitud del texto deseado
                               side = "left", #donde agregar el texto deseado
                               pad = "0" #texto deseado a pegar
                               ) 
  )

datos <- datos %>%
  mutate(codigo_divipola_departamento =
           case_when(
             codigo_divipola_departamento == "8001" ~ "08001",
             TRUE ~ codigo_divipola_departamento
           ))

table(datos$codigo_divipola_departamento)

datos$codigo_divipola_departamento <- as.character(datos$codigo_divipola_departamento)

# Quitar comas, puntos, espacios u otros símbolos
datos$codigo_divipola_departamento = str_remove_all(datos$codigo_divipola_departamento, "[^0-9]")

table(datos$codigo_divipola_departamento)

# Agregar nombres de departamento según codigo ---------------------------------

datos <- datos %>%
  left_join(Cod_Departamentos, 
            by = c("codigo_divipola_departamento" = "DP"))

# Ajustamos los números del divipola de los municipios ---------------------------------
table(datos$codigo_divipola_municipio)

datos <- datos %>%
  mutate(
    cod_divipola = 
      str_pad(as.character(codigo_divipola_municipio), #str_pad del paquete stringr
                           width = 5, #longitud del texto deseado
                           side = "left", #donde agregar el texto deseado
                           pad = "0" #texto deseado a pegar
              ) 
  )
table(datos$codigo_divipola_municipio)


# Agregamos los nombres de los departamentos si hace falta ----------------

datos <- datos %>%
  left_join(
    Cod_Departamentos %>%
      select(DPNOM, DP),
    by = c(
      "codigo_divipola_departamento" = "DP"
    )
  )

# Limpiamos las variables cuantitativas -----------------------------------
summary(datos$edad)

datos <- datos %>%
  mutate(
    edad = ifelse(
      edad < 0 | edad > 120, #verificarmos edades por debajo de 0 o por encima de 120 para volverlos NA
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

# Consistencia de fechas --------------------------------------------------

datos <- datos %>%
  mutate(
    inconsistencia_fechas =  fecha_de_muerte < fecha_de_inicio_de_sintomas
  )
table(datos$inconsistencia_fechas)
# Reporte sencillo --------------------------------------------------------

skim(datos)

# Exportar una versión limpia de la base ----------------------------------

#write.csv(datos,"Salidas/base_limpia.csv",row.names = FALSE)
#export(datos, "Salidas/base_limpia.xlsx") #exporta en xlsx
export(datos, "Salidas/base_limpia.csv") #exporta en csv
