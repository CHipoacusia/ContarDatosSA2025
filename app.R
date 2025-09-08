#####################################################################################
# Argentina Inclusiva 
#
# Versión: v6.7
# Descripción:
#   - Código optimizado y comentado siguiendo mejores prácticas.
#   - v2.2 Se agrego la tab trabajo
#   - v5.0 Se agrego Inclusion
#   - v6.0 Se agrego tecnologia
#   - v6.7 se arreglo tema de filtros y grupo etario
#   - v7.0 principales modificaciones:
#          - Base_Datos_Form_.xlsx (Marco, Yael)
#          - Base_Datos_Form_Modificada.xlsx (Jorge)
#          - Pantallas más compactas, pretendiendo aspecto de Dashboard
#          - Se agregaron botones de acción (tipos de gráficos, ayudas, etc.)
#          - Se modificó la disposición del Panel de Inicio original
#          - Se agregó un Panel de Inicio para portada
#          - Se incorporó el Panel de Importancia de Variables y Red Neuronal (Yael)
#          - Se emplearon dos archivos de encuestas: Base_Datos_Form.xlsx (Yael y Marco)
#                                                    Base_Datos_Form_Modificada.xlsx (Jorge)
#   - v7.1 pequeñas correcciones 
#   - v7.2 imagen
#   - v7.3 fondos de inicio, reflexión, provincias y gráficos, algunos formatos
#   - v7.4 resaltar Prov 
#   - v7.5 revision de objetos
#   - V7.6 revision de graficos de analisis 
#
# Autores: Yael Moretti, Marco A. Ferrara, Jorge Perera
# 
# Ultima modificación: 03/09/2025
#
#####################################################################################

# Define la versión de los archivos a utilizar
version <- "v7.6"

# Carga los componentes de la aplicación
source(paste0("global.", version, ".R"))
source(paste0("ui.", version, ".R"))
source(paste0("server.", version, ".R"))

# Lanza la aplicación Shiny
shinyApp(ui = ui, server = server)
