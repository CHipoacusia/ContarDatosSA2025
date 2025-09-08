# =========================================================================
# ARCHIVO GLOBAL DE CONFIGURACIÓN Y DATOS
# =========================================================================
## 1. CONFIGURACIÓN Y LIBRERÍAS
# -------------------------------------------------------------------------
if (!require(shiny)) install.packages("shiny", dependencies = TRUE)
if (!require(shinyjs)) install.packages("shinyjs", dependencies = TRUE)
if (!require(shinyjqui)) install.packages("shinyjqui", dependencies = TRUE)
if (!require(shinythemes)) install.packages("shinythemes", dependencies = TRUE)
if (!require(shinyWidgets)) install.packages("shinyWidgets", dependencies = TRUE)
if (!require(shinyBS)) install.packages("shinyBS", dependencies = TRUE)
if (!require(bsplus)) install.packages("bsplus", dependencies = TRUE)
if (!require(htmltools)) install.packages("htmltools", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(readxl)) install.packages("readxl", dependencies = TRUE)
if (!require(stringr)) install.packages("stringr", dependencies = TRUE)
if (!require(stringi)) install.packages("stringi", dependencies = TRUE)
if (!require(sf)) install.packages("sf", dependencies = TRUE)
if (!require(leaflet)) install.packages("leaflet", dependencies = TRUE)
if (!require(tm)) install.packages("tm", dependencies = TRUE)
# if (!require(wordcloud)) install.packages("wordcloud", dependencies = TRUE)
if (!require(syuzhet)) install.packages("syuzhet", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(plotly)) install.packages("plotly", dependencies = TRUE)
if (!require(RColorBrewer)) install.packages("RColorBrewer", dependencies = TRUE)
if (!require(DT)) install.packages("DT", dependencies = TRUE)
if (!require(ggrepel)) install.packages("ggrepel", dependencies = TRUE)
if (!require(scales)) install.packages("scales", dependencies = TRUE)
if (!require(forcats)) install.packages("forcats", dependencies = TRUE)
if (!require(purrr)) install.packages("purrr", dependencies = TRUE)
if (!require(wordcloud2)) install.packages("wordcloud2", dependencies = TRUE)
if (!require(randomForest)) install.packages("randomForest", dependencies = TRUE)
if (!require(neuralnet)) install.packages("neuralnet", dependencies = TRUE)

library(shiny)
library(shinyjs)
library(shinyjqui)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(bsplus)
library(htmltools)
library(dplyr)
library(readxl)
library(stringr)
library(stringi)
library(sf)
library(leaflet)
library(tm)
# library(wordcloud)
library(syuzhet)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(DT)
library(ggrepel)
library(scales)
library(forcats)
library(purrr)
library(wordcloud2)
library(randomForest)
library(neuralnet)


### PALETAS DE COLORES
PALETA_COLORES <- c("#8cdaff", "#8ce1ff", "#90e0ef", "#adb5bd", "#969ca1")
PALETA_DESIERTO <- c("#E4C29B", "#D19B68", "#FEF2BF", "#DDA15E", "#A06B4C", "#C7A68E", "#E4B07B", "#B18D6F")
PALETA_OCEANO <- c("#83a3c2", "#86b3dc", "#8cb1d4", "#8cbbd6", "#8ec4df", "#c5e1eb", "#d6f0f6", "#e9f8fb")
PALETA_MONTANA <- c("#84999d", "#9aabac", "#aabcc0", "#b3c0c3", "#e3dccf", "#f7f5f0", "#d1c9bd", "#b5aab1")
PALETA_BOSQUE <- c("#879b80", "#95a88c", "#a8b5a3", "#92b888", "#c3d6bb", "#bdafa1", "#b1a497", "#978c81")
PALETA_PRADERA <- c("#a4b7a9", "#a1c6a9", "#d1e2c4", "#f7ffc5", "#ffe04d", "#ffc7cc", "#d8bfd8", "#a4d4a4")
PALETA_URBANO_DIA <- c("#8e8e8e", "#b0b0b0", "#c1c1c1", "#bdbdbd", "#dedede", "#f0f0f0", "#9bbccf", "#a8c9dc")
PALETA_URBANO_NOCTURNO <- c("#7d7d7d", "#939393", "#a8a8a8", "#8e959b", "#98a3b0", "#ffe04d", "#ff9b8a", "#8cd3ff")
PALETA_CIRCO_MULTICOLOR <- c("#ff949b", "#f4a261", "#f6d860", "#2a9d8f", "#8a9699", "#d18cff", "#ffc107", "#4caf50")
PALETA_CARNAVAL_ROJO <- c("#d46a6a", "#ff9b85", "#ff6347", "#ffb6c1", "#ffd700", "#ffc485", "#ff9b85", "#cd5c5c")
PALETA_PASTEL_ROMANTICO <- c("#ffb6c1", "#ffdab9", "#e6e6fa", "#b0e0e6", "#add8e6", "#98fb98", "#f08080", "#e0bbe4")
colores_panel <- c("#E6F7AA", "#FFF9E6", "#F0FFF0", "#F9E6FF", "#FFE6E6", "#FAEBD7")


### FUNCIONES AUXILIARES
# Normaliza nombres de provincias para uniones consistentes.
normalizar_provincia <- function(nombres) {
  nombres <- toupper(stri_trans_general(nombres, "Latin-ASCII"))
  nombres <- str_replace_all(nombres, "CABA", "CIUDAD AUTONOMA DE BUENOS AIRES")
  nombres <- str_replace_all(nombres, "CAPITAL FEDERAL", "CIUDAD AUTONOMA DE BUENOS AIRES")
  nombres <- str_replace_all(nombres, "TIERRA DEL FUEGO, ANTARTIDA E ISLAS DEL ATLANTICO SUR", "TIERRA DEL FUEGO")
  return(nombres)
}
# Limpia una cadena de texto para análisis.
limpiar_texto <- function(texto) {
  texto_limpio <- tolower(texto)
  texto_limpio <- stri_trans_general(texto_limpio, "Latin-ASCII")
  texto_limpio <- removePunctuation(texto_limpio)
  texto_limpio <- removeNumbers(texto_limpio)
  texto_limpio <- stripWhitespace(texto_limpio)
  
  stopwords_es <- stopwords("spanish")
  palabras_irrelevantes <- c(
    "persona", "personas", "años", "año", "vez", "hipoacusia", "tecnologia",
    "puede", "puedo", "asi", "aunque", "ser", "soy", "era", "fui", "igual",
    "hacia", "tener", "tenerlo", "tenerla", "siempre", "tambien", "mas", "aun", "embargo", "dia"
  )
  texto_limpio <- removeWords(texto_limpio, c(stopwords_es, palabras_irrelevantes))
  return(texto_limpio)
}


### CARGA Y PROCESAMIENTO DE DATOS

# --- Datos Geoespaciales para el Mapa ---
prov_sf <- tryCatch({
  st_read("https://apis.datos.gob.ar/georef/api/provincias.geojson") %>%
    mutate(Provincia_norm = normalizar_provincia(nombre))
},
error = function(e) {
  stop("ERROR CRÍTICO: No se pudo conectar a la API de Georef para descargar el mapa. Verifica tu conexión a internet.")
})

# --- Datos de Narrativas (para Análisis de Texto) ---
archivo_narrativas <- "data/NARRATIVAS BREVES DE EXPERIENCIAS LABORALES.xlsx"
if (!file.exists(archivo_narrativas)) {
  stop(paste("ERROR CRÍTICO: No se encuentra el archivo de narrativas en la ruta:", archivo_narrativas))
}
datos_narrativas <- read_excel(archivo_narrativas)
if (!"Experiencia" %in% names(datos_narrativas)) {
  stop("ERROR CRÍTICO: El archivo de narrativas debe contener una columna llamada 'Experiencia'.")
}
textos_originales <- datos_narrativas$Experiencia[!is.na(datos_narrativas$Experiencia)]
textos_limpios <- sapply(textos_originales, limpiar_texto, USE.NAMES = FALSE)
narrativas_data <- list(originales = textos_originales, limpios = textos_limpios)

# --- Datos de la Encuesta ---
# ARCHIVO DE ENCUESTAS MODIFICADO POR JORGE PARA MEJORAR LAS ETIQUETAS DE LOS GRÁFICOS
archivo_poblacion_modificado <- "data/Base_Datos_Form_Modificada.xlsx"
if (!file.exists(archivo_poblacion_modificado)) {
  stop(paste("ERROR CRÍTICO: No se encuentra el archivo de la encuesta en la ruta:", archivo_poblacion))
}

# ARCHIVO DE ENCUESTAS ORIGINAL PARA LA GENERACIÓN DE LOS MODELOS
archivo_poblacion <- "data/Base_Datos_Form.xlsx"
if (!file.exists(archivo_poblacion)) {
  stop(paste("ERROR CRÍTICO: No se encuentra el archivo de la encuesta en la ruta:", archivo_poblacion_original))
}


### CARGA DE LOS DATOS DE LOS ENCUESTADOS EN LA INVESTIGACIÓN - VERSIÓN DE JORGE
datos_poblacion_modificado <- read_excel(archivo_poblacion_modificado, col_types = "text") %>%
  rename(
    Edad = 'Edad',
    Genero = `Genero`,
    Provincia = `Provincia`,
    Educacion = `Nivel educativo`,
    Trabaja = `¿Actualmente tiene un trabajo?`,
    Sector = `Si su respuesta fue sí, ¿en qué sector trabaja?`,
    Tipo_Empleo = `¿Si esta trabajando que tipo de empleo tiene ud?`,
    Ayuda = `Ha tenido ayuda de ONGs y/o entidades de apoyo para lograr insertarse laboralmente?`,
    Cant_Empleos = `¿Cuántos empleos ha tenido en los últimos 5 años?`,
    Permanencia = `¿Qué barreras ha encontrado en el acceso o permanencia en el mundo laboral?`,
    
    ##### INCLUSION #####
    Experiencia_Inclusiva =`En una escala del 1 al 5, ¿cómo calificaría su experiencia general desde una perspectiva inclusiva en los lugares donde ha trabajado ?`,
    Recibio_Adaptacion = `Ha recibido algún tipo de adaptación o apoyo específico en su vida laboral debido a su hipoacusia?`,
    Tipo_Adaptacion =`Si su respuesta es sí, ¿qué tipo de adaptación ha sido más útil para usted?`,
    Adaptacion_Satisfactoria = `En una escala del 1 al 5, ¿qué tan satisfecho está con las adaptaciones que ha recibido en sus trabajos anteriores o actual?`,
    
    #### TECNOLOGIA #####
    Uso_Tecnologia = `¿Utiliza actualmente o ha utilizado alguna tecnología que le ayude o lo haya ayudado en su vida laboral?`,
    Tipo_Tecnologia = `Si su respuesta es sí, ¿qué tipo de tecnología utiliza o ha utilizado? (Seleccione todas las que apliquen)`,
    Tecnologia_Beneficiosa = `¿Qué tipo de tecnología o apoyo adicional cree que sería más beneficioso para su inclusión laboral? (Seleccione todas las que apliquen)`,
    Conocimiento_IA = `En una escala del 1 al 5, ¿cómo calificaría su conocimiento sobre inteligencia artificial aplicada en los lugares donde ha trabajado?`,
    
    ### INTELIGENCIA ARTIFICIAL #####
    IA_Mejora = `¿Cree que la inteligencia artificial podría mejorar significativamente su experiencia laboral?`,
    Importancia_Capacitacion = `En una escala del 1 al 5, ¿cómo calificaría la importancia de recibir capacitación en el uso de tecnologías de Inteligencia artificial para mejorar la inclusión laboral?`,
    Prioridad_IA = `¿Qué nivel de prioridad daría usted a la implementación de soluciones basadas en inteligencia artificial en su lugar de trabajo?`,
    Privacidad_Datos = `En una escala del 1 al 5, ¿qué tan importante considera la privacidad de sus datos en el uso de IA en los entornos laborales?`
  ) %>%
  mutate(
    # Agregamos la normalización de la provincia
    #Provincia_norm = normalizar_provincia(Provincia),
    
    Trabaja = case_when(
      tolower(Trabaja) %in% c("sí", "si") ~ "Sí",
      tolower(Trabaja) == "no" ~ "No",
      TRUE ~ "No especificado"
    ),
    
    Cant_Empleos = case_when(
      Cant_Empleos %in% c("45323", "1 a 2") ~ "1-2",
      Cant_Empleos %in% c("45385", "3 a 4") ~ "3-4",
      Cant_Empleos == "Más de 4" ~ ">4",
      Cant_Empleos == "Ninguno" ~ "0",
      TRUE ~ Cant_Empleos
    ),
    
    Ayuda = if_else(tolower(Ayuda) %in% c("sí", "si"), "Sí", "No"),
    
    Tipo_Empleo = case_when(
      Tipo_Empleo == "Empleado medio tiempo" ~ "Part Time",
      Tipo_Empleo == "Empleado tiempo completo" ~ "Full Time",
      TRUE ~ Tipo_Empleo
    ),
    
    Sector = if_else(Sector == "No aplica - SI LA RESPUESTA ANTERIOR FUE NO", "No aplica", Sector),
    
    Permanencia = case_when(
      Permanencia == "Falta de adaptaciones tecnológicas" ~ "Tecnológicas",
      Permanencia == "Falta de comprensión por parte de colegas" ~ "Comprensión",
      Permanencia == "Otras similares a las anteriores" ~ "Otras",
      Permanencia == "Actitudes discriminatorias" ~ "Discriminación",
      TRUE ~ Permanencia
    ),
    # Edad = factor(Edad, levels = opciones_edad)
  )

### FIN MODIFICACIÓN JORGE



### CARGA DE LOS DATOS DE LOS ENCUESTADOS EN LA INVESTIGACIÓN - VERSIÓN DE YAEL

# Forzamos a leer TODAS las columnas como TEXTO para tener control total
datos_poblacion <- read_excel(archivo_poblacion, col_types = "text") %>%
  rename(
    ##### POBLACION & FILTROS GRAL #####
    Edad_Grupo = `Edad`, # Se lee la columna de rangos directamente
    Genero = `Genero`,
    Provincia_original = `Provincia`,
    Educacion = `Nivel educativo`,
    
    ##### TRABAJO #####
    Trabaja = `¿Actualmente tiene un trabajo?`,
    Sector = `Si su respuesta fue sí, ¿en qué sector trabaja?`,
    Tipo_Empleo = `¿Si esta trabajando que tipo de empleo tiene ud?`,
    Ayuda = `Ha tenido ayuda de ONGs y/o entidades de apoyo para lograr insertarse laboralmente?`,
    Cant_Empleos = `¿Cuántos empleos ha tenido en los últimos 5 años?`,
    Permanencia = `¿Qué barreras ha encontrado en el acceso o permanencia en el mundo laboral?`,
    
    ##### INCLUSION #####
    Experiencia_Inclusiva =`En una escala del 1 al 5, ¿cómo calificaría su experiencia general desde una perspectiva inclusiva en los lugares donde ha trabajado ?`,
    Recibio_Adaptacion = `Ha recibido algún tipo de adaptación o apoyo específico en su vida laboral debido a su hipoacusia?`,
    Tipo_Adaptacion =`Si su respuesta es sí, ¿qué tipo de adaptación ha sido más útil para usted?`,
    Adaptacion_Satisfactoria = `En una escala del 1 al 5, ¿qué tan satisfecho está con las adaptaciones que ha recibido en sus trabajos anteriores o actual?`,
    
    #### TECNOLOGIA #####
    Uso_Tecnologia = `¿Utiliza actualmente o ha utilizado alguna tecnología que le ayude o lo haya ayudado en su vida laboral?`,
    Tipo_Tecnologia = `Si su respuesta es sí, ¿qué tipo de tecnología utiliza o ha utilizado? (Seleccione todas las que apliquen)`,
    Conocimiento_IA = `En una escala del 1 al 5, ¿cómo calificaría su conocimiento sobre inteligencia artificial aplicada en los lugares donde ha trabajado?`,
    IA_Mejora = `¿Cree que la inteligencia artificial podría mejorar significativamente su experiencia laboral?`,
    Tecnologia_Beneficiosa = `¿Qué tipo de tecnología o apoyo adicional cree que sería más beneficioso para su inclusión laboral? (Seleccione todas las que apliquen)`,
    Importancia_Capacitacion = `En una escala del 1 al 5, ¿cómo calificaría la importancia de recibir capacitación en el uso de tecnologías de Inteligencia artificial para mejorar la inclusión laboral?`,
    Prioridad_IA = `¿Qué nivel de prioridad daría usted a la implementación de soluciones basadas en inteligencia artificial en su lugar de trabajo?`,
    Privacidad_Datos = `En una escala del 1 al 5, ¿qué tan importante considera la privacidad de sus datos en el uso de IA en los entornos laborales?`
  ) %>%
  filter(!is.na(Edad_Grupo), !is.na(Provincia_original)) %>%
  mutate(
    # Se elimina la lógica de conversión de edad, ya no es necesaria.
    
    Provincia_norm = normalizar_provincia(Provincia_original),
    Trabaja = case_when(
      tolower(Trabaja) %in% c("sí", "si") ~ "Sí",
      tolower(Trabaja) == "no" ~ "No",
      TRUE ~ "No especificado"
    ),
    Cant_Empleos = case_when(
      Cant_Empleos %in% c("45323", "1 a 2") ~ "1-2",
      Cant_Empleos %in% c("45385", "3 a 4") ~ "3-4",
      Cant_Empleos == "Más de 4"           ~ ">4",
      Cant_Empleos == "Ninguno"            ~ "0",
      TRUE                                 ~ Cant_Empleos
    ),
    Ayuda = if_else(tolower(Ayuda) %in% c("sí", "si"), "Sí", "No"),
    Tipo_Empleo = case_when(
      Tipo_Empleo == "Empleado medio tiempo"   ~ "Part Time",
      Tipo_Empleo == "Empleado tiempo completo" ~ "Full Time",
      TRUE                                   ~ Tipo_Empleo
    ),
    Sector = if_else(Sector == "No aplica - SI LA RESPUESTA ANTERIOR FUE NO", "No aplica", Sector),
    Permanencia = case_when(
      Permanencia == "Falta de adaptaciones tecnológicas"       ~ "Tecnológicas",
      Permanencia == "Falta de comprensión por parte de colegas" ~ "Comprensión",
      Permanencia == "Otras similares a las anteriores"          ~ "Otras",
      Permanencia == "Actitudes discriminatorias"               ~ "Discriminación",
      TRUE                                                     ~ Permanencia
    ),
    Experiencia_Inclusiva = case_when(
      Experiencia_Inclusiva %in% c("1", "1.0") ~ "Mala",
      Experiencia_Inclusiva %in% c("2", "2.0") ~ "Regular",
      Experiencia_Inclusiva %in% c("3", "3.0") ~ "Buena",
      Experiencia_Inclusiva %in% c("4", "4.0") ~ "Muy Buena",
      Experiencia_Inclusiva %in% c("5", "5.0") ~ "Excelente",
      TRUE ~ "No especificado"
    ),
    Tipo_Adaptacion = case_when(
      Tipo_Adaptacion == "No aplica - SI LA RESPUESTA ANTERIOR FUE NO" ~ "No aplica",
      Tipo_Adaptacion == "Intérprete de lenguaje de señas" ~ "Intérprete de lenguaje",
      Tipo_Adaptacion == "Software de reconocimiento de voz" ~ "Reconocimiento de voz",
      Tipo_Adaptacion == "Subtítulos en reuniones" ~ "Subtítulos en reuniones",
      TRUE ~ "No especificado"
    ),
    Adaptacion_Satisfactoria = case_when(
      Adaptacion_Satisfactoria %in% c("1", "1.0") ~ "Mala",
      Adaptacion_Satisfactoria %in% c("2", "2.0") ~ "Regular",
      Adaptacion_Satisfactoria %in% c("3", "3.0") ~ "Buena",
      Adaptacion_Satisfactoria %in% c("4", "4.0") ~ "Muy Buena",
      Adaptacion_Satisfactoria %in% c("5", "5.0") ~ "Satisfactoria",
      TRUE ~ "No especificado"
    ),
    Uso_Tecnologia = if_else(tolower(Uso_Tecnologia) %in% c("sí", "si"), 1, 0),
    IA_Mejora = if_else(tolower(IA_Mejora) %in% c("sí", "si"), 1, 0)
    # Las conversiones a `as.numeric` para las otras escalas se pueden hacer aquí si es necesario
  ) %>%
  rename(Provincia = Provincia_original)

# --- 4.4. Opciones para los Filtros de la UI ---

# Se definen las opciones de edad en el orden cronológico deseado
opciones_edad_ordenadas  <- c("Menos de 20 años", "20-30 años", "31-40 años", "41-50 años", "Más de 50 años", "No especificado")

# Se asegura que solo las opciones de edad presentes en los datos se usen para los filtros
opciones_edad <- intersect(opciones_edad_ordenadas, unique(datos_poblacion$Edad_Grupo))

# Opciones para los otros filtros
prov_nombres_map <- datos_poblacion %>%
  distinct(Provincia, Provincia_norm) %>%
  arrange(Provincia)
opciones_prov <- setNames(prov_nombres_map$Provincia_norm, prov_nombres_map$Provincia)

opciones_gen   <- sort(unique(datos_poblacion$Genero))
opciones_edu   <- sort(unique(datos_poblacion$Educacion))


# Finalmente, se convierte la columna a un FACTOR ORDENADO para que los gráficos respeten el orden
datos_poblacion <- datos_poblacion %>%
  mutate(Edad_Grupo = factor(Edad_Grupo, levels = opciones_edad))

equivalencias <-data.frame(
  # columna en datos_poblacion
  original = c("Provincia_norm", "Edad_Grupo", "Genero","Educacion","Trabaja", "Sector", "Tipo_Empleo",
               "Ayuda","Experiencia_Inclusiva", "Recibio_Adaptacion","Tipo_Adaptacion","Cant_Empleos",
               "Adaptacion_Satisfactoria","Permanencia", "Uso_Tecnologia","Tipo_Tecnologia",
               "Conocimiento_IA","IA_Mejora","Tecnologia_Beneficiosa","Importancia_Capacitacion",
               "Prioridad_IA","Privacidad_Datos","¿Estaría dispuesto(a) a participar en programas de capacitación sobre inteligencia artificial para mejorar la inclusión laboral?",
               "¿Considera que las instituciones educativas deberían incluir capacitación sobre inteligencia artificial en sus programas?", "¿Cuál de los siguientes aspectos cree que mejorará con la implementación de inteligencia artificial para ayudar a las personas con hipoacusia para generar un entorno laboral sin barreras?"),   
  # lo que usa el modelo dsNumeric
  infoModelo = c(
    "Provincia", "Edad_Grupo", "Genero", "Educacion", "Trabaja", "Sector", "Tipo_Empleo", 
    "Recibio_Ayuda", "Target", "Recibio_Adaptacion", "Tipo_Adaptacion", "Cant_Empleos", 
    "Adaptacion_Satisfactoria", "Permanencia", "Uso_Tecnologia", "Tipo_Tecnologia", 
    "Conocimiento_IA", "IA_Mejora", "Tecnología_Beneficiosa", "Importancia_Capacitacion",
    "Prioridad_IA" ,"Privacidad_Datos", "Participa_EnCapacitacionIA","InstEducativas_CapacitarEnIA", "Implementacion_MejorasIA"
  ),
  # # texto que verá el usuario
  titulo   = c("Provincia de residencia", "Edad del encuestado", "Genero del encuestado", "Nivel Educativo", "¿Actualmente tiene un trabajo?", "¿En qué sector trabaja?", "¿Qué tipo de empleo tiene ud?",
               "Ha tenido ayuda/apoyo para lograr insertarse laboralmente?", " ¿Cómo calificaría su experiencia desde una perspectiva inclusiva en los lugares donde ha trabajado ?", 
               "Ha recibido algún tipo de apoyo en su vida laboral debido a su hipoacusia?", "¿Qué tipo de adaptación ha sido más útil para usted?", "¿Cuántos empleos ha tenido en los últimos 5 años?",
               "¿Qué tan satisfecho está con las adaptaciones que ha recibido en sus trabajos?", "¿Qué barreras ha encontrado en el acceso o permanencia en el mundo laboral?", "¿Utiliza actualmente o ha utilizado tecnología que le ayude en su vida laboral?", "¿Qué tipo de tecnología utiliza o ha utilizado?",
               "¿Cómo calificaría su conocimiento sobre IA aplicada en los lugares donde ha trabajado?", "¿Cree que la IA podría mejorar su experiencia laboral?", "¿Qué tipo de tecnología/apoyo cree que sería más beneficioso para su inclusión laboral?", "¿Cómo calificaría recibir capacitación en el uso de tecnologías de IA para mejorar la inclusión laboral?",
               "¿Qué nivel de prioridad daría a la implementación de soluciones basadas en IA en su lugar de trabajo?","¿Qué tan importante considera la privacidad de sus datos en el uso de IA en los entornos laborales?",
               "¿Participaría en programas de capacitación sobre IA para mejorar la inclusión laboral?", "¿Considera que las instituciones educativas deberían incluir capacitación sobre IA en sus programas?",
               "¿Cuál aspecto cree que mejoraría con la implementación de IA para ayudar a las personas con hipoacusia para un entorno laboral sin barreras?"),     
  stringsAsFactors = FALSE
  
)

### FIN MODIFICACIÓN YAEL



### SE CREA UN VECTOR CON NOMBRES MÁS DESCRIPTIVOS Y BREVES DE LOS CAMPOS
### PARA EL GRÁFICO DE IMPORTANCIA DE VARIABLES EN EL MODELO BOSQUE ALEATORIO
etiquetas_plus <-c(
  "Provincia", "Grupo Etario", "Género", "Nivel Educativo", "Trabaja Actualmente", "Sector Laboral",
  "Tipo de Empleo", "Ayuda de ONGs", "Grado de Inclusión Laboral", "Recibió Adaptación/Apoyo",
  "Tipo de Adaptación", "Empleos Últimos 5 Años", "Satisfacción por Adaptaciones",
  "Barreras en el Trabajo", "Utiliza Tecnología", "Tipo de Tecnología Usada",
  "Nivel de Conocimiento sobre IA", "La IA Mejorará el Trabajo", 
  "Tecnología Beneficiosa p/Inclusión", "Importancia de Capacitación en IA",
  "Se capacitaría en IA", "Las Escuelas deben Enseñar IA", "Prioridad de la IA en el Trabajo",
  "Importancia de Privacidad de Datos en IA", "Aspectos que Mejorrán con IA")



opciones_prov_1 <- sort(unique(datos_poblacion_modificado$Provincia))
opciones_gen_1 <- sort(unique(datos_poblacion_modificado$Genero))
opciones_edad_1 <- c("< 20", "20-30", "31-40", "41-50", "> 50")
opciones_edu_1 <- sort(unique(datos_poblacion_modificado$Educacion))

# Mensaje confirmando que todo cargó correctamente.
message("global.R ejecutado con éxito. Librerías y datos cargados.")