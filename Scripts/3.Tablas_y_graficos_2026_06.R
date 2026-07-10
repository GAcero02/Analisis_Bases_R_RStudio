library(tidyverse)
library(ggplot2)
library(gtsummary)
library(flextable)
library(officer)




# Grafica de fecha inicio sintomas ----------------------------------------
datos$fecha_de_inicio_de_sintomas <- as.Date(datos$fecha_de_inicio_de_sintomas)
class(datos$fecha_de_inicio_de_sintomas)

Graficas_inicio_sintomas <- ggplot(datos, aes(x = fecha_de_inicio_de_sintomas)) +
  geom_bar() +
 # scale_x_date( #esta función nos permite ver unicamente el años en el eje X
#    date_breaks = "1 year",
 #   date_labels = "%Y"
#  ) +
  labs(
    x = "Fecha de inicio de síntomas",
    y = "Número de casos",
    title = "Casos por fecha de inicio de síntomas"
  ) +
  theme_minimal()
Graficas_inicio_sintomas

ggsave(
  "Salidas/Graficas_inicio_sintomas.png",
  plot = Graficas_inicio_sintomas,
  width = 10,
  height = 6,
  dpi = 300
)

#Tabla completa para informe por sexo
tabla_por_sexo <- datos %>%
  select(sexo, edad_en_años, estado) %>%
  tbl_summary(
    by = sexo
  )

tabla_por_sexo

tabla_por_sexo %>%
  as_flex_table() %>%
  save_as_docx(
    path = "Salidas/tabla_por_sexo.docx"
  )
