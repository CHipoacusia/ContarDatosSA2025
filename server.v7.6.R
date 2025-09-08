server <- function(input, output, session) {
  
  
  ### SECCIÓN PARA LA GENERACIÓN DE MODELOS - YAEL
  
  dsNumeric <- reactive({
    datos_poblacion %>%
      mutate(
        Provincia = as.numeric(factor(Provincia_norm, levels = unique(datos_poblacion$Provincia_norm))),
        Edad_Grupo = as.numeric(factor(Edad_Grupo, levels = unique(datos_poblacion$Edad_Grupo))),
        Genero = as.numeric(factor(Genero, levels = unique(datos_poblacion$Genero))),
        Educacion = as.numeric(factor(Educacion, levels = unique(datos_poblacion$Educacion))),
        Trabaja = as.numeric(factor(Trabaja, levels = unique(datos_poblacion$Trabaja))),
        Sector = as.numeric(factor(Sector, levels = unique(datos_poblacion$Sector))),
        Tipo_Empleo = as.numeric(factor(Tipo_Empleo, levels = unique(datos_poblacion$Tipo_Empleo))),
        Recibio_Ayuda = as.numeric(factor(Ayuda, levels = unique(datos_poblacion$Ayuda))),
        Target = factor(Experiencia_Inclusiva),
        Recibio_Adaptacion = as.numeric(factor(Recibio_Adaptacion, levels = unique(datos_poblacion$Recibio_Adaptacion))),
        Tipo_Adaptacion = as.numeric(factor(Tipo_Adaptacion, levels = unique(datos_poblacion$Tipo_Adaptacion))),
        Cant_Empleos = as.numeric(factor(Cant_Empleos, levels = unique(datos_poblacion$Cant_Empleos))),
        Adaptacion_Satisfactoria =  as.numeric(factor(Adaptacion_Satisfactoria, levels = unique(datos_poblacion$Adaptacion_Satisfactoria))),
        Permanencia = as.numeric(factor(Permanencia, levels = unique(datos_poblacion$Permanencia))),
        Uso_Tecnologia   = as.numeric(factor(Uso_Tecnologia, levels = unique(datos_poblacion$Uso_Tecnologia))),
        Tipo_Tecnologia = as.numeric(factor(Tipo_Tecnologia, levels = unique(datos_poblacion$Tipo_Tecnologia))),
        Conocimiento_IA  = as.numeric(factor(Conocimiento_IA, levels = unique(datos_poblacion$Conocimiento_IA))), 
        IA_Mejora = as.numeric(factor(IA_Mejora, levels = unique(datos_poblacion$IA_Mejora))),
        Tecnología_Beneficiosa = as.numeric(factor(Tecnologia_Beneficiosa, levels = unique(datos_poblacion$Tecnologia_Beneficiosa))),
        Importancia_Capacitacion = as.numeric(factor(Importancia_Capacitacion, levels = unique(datos_poblacion$Importancia_Capacitacion))),
        Prioridad_IA = as.numeric(factor(Prioridad_IA, levels = unique(datos_poblacion$Prioridad_IA))),
        Privacidad_Datos = as.numeric(factor(Privacidad_Datos, levels = unique(datos_poblacion$Privacidad_Datos))),
        Participa_EnCapacitacionIA =  as.numeric(factor('¿Estaría dispuesto(a) a participar en programas de capacitación sobre inteligencia artificial para mejorar la inclusión laboral?')),
        InstEducativas_CapacitarEnIA = as.numeric(factor('¿Considera que las instituciones educativas deberían incluir capacitación sobre inteligencia artificial en sus programas?')),
        Implementacion_MejorasIA = as.numeric(factor('¿Cuál de los siguientes aspectos cree que mejorará con la implementación de inteligencia artificial para ayudar a las personas con hipoacusia para generar un entorno laboral sin barreras?'))
      )%>%
      mutate(Target = case_when(
        Target %in% c("Mala") ~ "MAL",
        Target %in% c("Regular") ~ "REG",
        Target %in% c("Buena") ~ "BUE",
        Target %in% c("Muy Buena") ~ "MBU",
        Target %in% c("Excelente") ~ "EXC",
        Target %in% c("No especificado") ~ "NES")
      )%>%
      select(Provincia, Edad_Grupo, Genero, Educacion, Trabaja, Sector, Tipo_Empleo, 
             Recibio_Ayuda, Target, Recibio_Adaptacion, Tipo_Adaptacion, Cant_Empleos, 
             Adaptacion_Satisfactoria, Permanencia, Uso_Tecnologia, Tipo_Tecnologia, 
             Conocimiento_IA, IA_Mejora, Tecnología_Beneficiosa, Importancia_Capacitacion,
             Prioridad_IA ,Privacidad_Datos, Participa_EnCapacitacionIA,InstEducativas_CapacitarEnIA, Implementacion_MejorasIA)
  })
  

  
  # --- Función de entrenamiento ---
  # Crear contenedor reactivo para guardar resultados
  valores <- reactiveValues()
  entrenarModelo <- function(df, target, split, ntree) {
    df <- na.omit(df)
    df[[target]] <- as.factor(df[[target]])
    
    # >>> SOLUCIÓN: Reordenar los niveles del factor Target aquí <<<
    df[[target]] <- factor(df[[target]],
                           levels = c("MAL", "REG", "BUE", "MBU", "EXC", "NES"))
    
    
    set.seed(123)
    train_index <- sample(seq_len(nrow(df)), size = floor(split/100 * nrow(df)))
    train_data <- df[train_index, ]
    test_data  <- df[-train_index, ]
    
    modelo <- randomForest(
      as.formula(paste(target, "~ .")),
      data = train_data,
      importance = TRUE,
      ntree = ntree
    )
    
    pred <- predict(modelo, test_data)
    cm <- table(Real = test_data[[target]], Predicho = pred)
    acc <- sum(diag(cm)) / sum(cm)
    
    list(
      df = df,
      modelo = modelo,
      cm = cm,
      acc = acc
    )
  } ##FIN FUNCION
  
  
  ### ENTRENAR EL MODELO DE BOSQUE ALEATORIO AUTOMÁTICAMENTE AL INICIO
  ### PARA DETECTAR VARIABLES IMPORTANTES
  observe({
    req(dsNumeric())
    
    # La función entrenarModelo() crea el modelo
    valores$resultado <- entrenarModelo(
      df = dsNumeric(),
      target = "Target",
      split = input$split,
      ntree = input$ntree
    )
  })
  
  ### FIN SECCIÓN PARA GENERACIÓN DE RED NEURONAL - YAEL
  
  ### Objeto reactivo para la importancia de las variables
  ### Para usarlo para la tabla y para el gráfico y que no haya diferencias entre ellos
  importancia_modelo <- reactive({
    req(valores$resultado$modelo)
    
    # Extraer la importancia completa del modelo
    imp <- importance(valores$resultado$modelo)
    
    # Seleccionar solo las columnas de los niveles del target y la métrica de precisión
    # 'MeanDecreaseAccuracy' y los nombres de las columnas que corresponden a los niveles
    columnas_seleccionadas <- c("MeanDecreaseAccuracy", "MAL", "REG", "BUE", "MBU", "EXC", "NES")
    
    # Crear un data.frame con las columnas deseadas
    imp_df <- data.frame(
      Variables = rownames(imp),
      imp[, columnas_seleccionadas]
    )
    
    return(imp_df)
  })
  
  model_state <- reactiveVal(FALSE)
  valores <- reactiveValues()
  
  
  ### BOTONES DE LOS FILTROS PRINCIPALES (POR PROVINCIA, GÉNERO, EDAD Y EDUCACIÓN)
  observeEvent(input$prov_todos, { updateCheckboxGroupInput(session, "filtro_prov", selected = opciones_prov_1) })
  observeEvent(input$gen_todos, { updateCheckboxGroupInput(session, "filtro_gen", selected = opciones_gen_1) })
  observeEvent(input$edad_todos, { updateCheckboxGroupInput(session, "filtro_edad", selected = opciones_edad_1) })
  observeEvent(input$edu_todos, { updateCheckboxGroupInput(session, "filtro_edu", selected = opciones_edu_1) })
  
  
  ### VARIABLES REACTIVAS PARA TIPOS DE GRÁFICOS, ORDENAMIENTO Y PALETA DE COLORES
  opcion_grafico <- reactiveVal(1)  # Tipo de gráfico (1: columnas; 2: barras; 3: circular; 4: anillo)
  opcion_orden <- reactiveVal(1)    # Ordenamiento de etiquetas (1: mayor frecuencia; 2: menor freecuencia; 3: orden original)
  opcion_paleta <- reactiveVal(1)   # Paleta de colores (Paletas del 1 al 10)
  opcion_grafico1 = reactiveVal(1)
  opcion_grafico2 = reactiveVal(2)
  opcion_grafico3 = reactiveVal(3)
  opcion_grafico4 = reactiveVal(4)
  opcion_grafico5 = reactiveVal(1)
  opcion_grafico6 = reactiveVal(2)
  
  ### VARIABLE PARA EL TIMER DE LA NUBE DE PALABRAS
  timer_activo <- reactiveVal(TRUE)
  
  ### VARIABLES GENERALES
  paleta_actual <- reactiveVal(PALETA_DESIERTO)
  paletas <- c("PALETA_DESIERTO", "PALETA_OCEANO", "PALETA_MONTANA", "PALETA_BOSQUE", "PALETA_PRADERA",
               "PALETA_URBANO_DIA", "PALETA_URBANO_NOCTURNO", "PALETA_CIRCO_MULTICOLOR",
               "PALETA_CARNAVAL_ROJO", "PALETA_PASTEL_ROMANTICO")
  paleta_aleatoria <- reactiveVal(TRUE)
  PROVINCIAS_DESTACADAS <- c("Buenos Aires", "Ciudad Autónoma de Buenos Aires", "Córdoba", "Tucumán", "Chubut", "Río Negro")
  
  
  ### BOTÓN PARA GRÁFICO DE COLUMNAS
  observeEvent(input$btn_cols, {
    opcion_grafico(1)
    # Gráficos 1 (columnas) y 2 (barras): se habilita el botón ordenamiento
    shinyjs::toggleState("btn_order", TRUE) # Habilita el botón
    opcion_grafico1(1)
    opcion_grafico2(1)
    opcion_grafico3(1)
    opcion_grafico4(1)
    opcion_grafico5(1)
    opcion_grafico6(1)
  })
  

  ### BOTÓN PARA GRÁFICO DE barras
  observeEvent(input$btn_bars, {
    opcion_grafico(2)
    # Gráficos 1 (columnas) y 2 (barras): se habilita el botón ordenamiento
    shinyjs::toggleState("btn_order", TRUE) # Habilita el botón
    opcion_grafico1(2)
    opcion_grafico2(2)
    opcion_grafico3(2)
    opcion_grafico4(2)
    opcion_grafico5(2)
    opcion_grafico6(2)
  })
  
  
  ### BOTÓN PARA GRÁFICO CIRCULAR
  observeEvent(input$btn_circ, {
    opcion_grafico(3)
    # Se deshabilita el botón ordenamiento porque no funciona bien y es confuso
    shinyjs::toggleState("btn_order", FALSE) # Deshabilita el botón
    opcion_grafico1(3)
    opcion_grafico2(3)
    opcion_grafico3(3)
    opcion_grafico4(3)
    opcion_grafico5(3)
    opcion_grafico6(3)
  })

  
  ### BOTÓN PARA GRÁFICO DE ANILLO  
  observeEvent(input$btn_ring, {
    opcion_grafico(4)
    # GSe deshabilita el botón ordenamiento porque no funciona bien y es confuso
    shinyjs::toggleState("btn_order", FALSE) # Deshabilita el botón
    opcion_grafico1(4)
    opcion_grafico2(4)
    opcion_grafico3(4)
    opcion_grafico4(4)
    opcion_grafico5(4)
    opcion_grafico6(4)
  })
  
  
  ### BOTÓN PARA GRÁFICOS DE TODOS LOS TIPOS
  observeEvent(input$btn_graphs, {
    valor_base <- (opcion_grafico1() %% 4) + 1
    opcion_grafico1(valor_base)
    
    lista_opciones <- list(opcion_grafico2, opcion_grafico3, 
                           opcion_grafico4, opcion_grafico5, opcion_grafico6)
    valor_actual <- valor_base
    
    for (opcion in lista_opciones) {
      valor_siguiente <- (valor_actual %% 4) + 1
      opcion(valor_siguiente)
      valor_actual <- valor_siguiente
    }
  })
  
  
  ### BOTÓN PARA TIPO DE ORDENAMIENTO
  observeEvent(input$btn_order, { 
    valor_actual <- opcion_orden()
    if (valor_actual < 3) { opcion_orden(valor_actual + 1) }
    else { opcion_orden(1) }
  })
  
  
  ### BOTÓN PARA LA PALETA DE COLORES
  observeEvent(input$btn_pale, {
    valor_actual <- opcion_paleta()
    if (valor_actual < length(paletas)) { opcion_paleta(valor_actual + 1)
    } else { opcion_paleta(1) }
    nombre_paleta <- paletas[opcion_paleta()]
    paleta_actual(get(nombre_paleta, envir = .GlobalEnv))
    paleta_aleatoria(FALSE)
  })
  
  
  ### BOTÓN PARA LA PALETA ALEATORIA DE COLORES
  observeEvent(input$btn_pal_aleat, {
    # Alterna entre TRUE y FALSE
    paleta_aleatoria(!paleta_aleatoria())
    paleta_aleatoria(TRUE)
  })
  
  
  ### DIMENSIONES DE LOS GRÁFICOS
  w2_graph <- 400  # Ancho para 2 gráficos por fila
  w3_graph <- 260  # Ancho para 3 gráficos por fila
  h_graph <- 240
  
  
  ### EXPRESIÓN REACTIVA PARA LOS DATOS FILTRADOS POR PROVINCIA, GÉNERO, EDAD Y EDUCACIÓN
  datos_filtrados <- reactive({
    req(input$filtro_prov, input$filtro_gen, input$filtro_edad, input$filtro_edu)
    datos_poblacion_modificado %>%
      filter(
        Provincia %in% input$filtro_prov,
        Genero %in% input$filtro_gen,
        Edad %in% input$filtro_edad,
        Educacion %in% input$filtro_edu
      )
  })
  
  
  ### ETIQUETAS ABREVIADAS
  etiquetas_prov <- c(
    "Buenos Aires" = "Bs.As.",
    "CABA" = "CABA",
    "Chubut" = "Chu",
    "Córdoba" = "Cór",
    "Río Negro" = "RNe",
    "Tucumán" = "Tuc"
  )
  
  etiquetas_gen <- c(
    "Femenino" = "Fem.",
    "Masculino" = "Masc."
  )
  
  etiquetas_educ <- c(
    "Primario" = "Prim.",
    "Secundario completo" = "Sec.",
    "Técnico o terciario" = "Téc./Terc.",
    "Universitario completo" = "Un.Comp.",
    "Universitario incompleto" = "Un.Inc."
  )
  
  etiquetas_edad <- c(
    "< 20" = "< 20",
    "20-30" = "20-30",
    "31-40" = "31-40",
    "41-50" = "41-50",
    "> 50" = "> 50"
  )
  
  etiquetas_calificacion <- c(
    "A" = "Mala",
    "B" = "Regular",
    "C" = "Buena",
    "D" = "Muy Buena",
    "E" = "Excelente"
  )
  
  etiquetas_satisfaccion <- c(
    "A" = "Ninguna",
    "B" = "Regular",
    "C" = "Buena",
    "D" = "Alta",
    "E" = "Muy Alta"
  )
  
  etiquetas_importancia <- c(
    "A" = "Ninguna",
    "B" = "Regular",
    "C" = "Aceptable",
    "D" = "Alta",
    "E" = "Muy Alta"
  )
  
  etiquetas_prioridad <- c(
    "A" = "Ninguna",
    "B" = "Regular",
    "C" = "Normal",
    "D" = "Alta",
    "E" = "Muy Alta"
  )
  
  etiquetas_mejoras <- c(
    "Mejora en software de reconocimiento de voz" = "Soft.Rec",
    "Implementación de inteligencia artificial para accesibilidad" = "IA",
    "Capacitación en uso de tecnologías de IA" = "Cap.IA",
    "Desarrollo de aplicaciones de comunicación específicas" = "Aplic",
    "Otras similares" = "Otras"
  )
  
  
  
  ### FUNCIÓN PARA GENERAR UN GRAFICO INTERACTIVO
  crear_grafico <- function(df, var_string, titulo_grafico = NULL, etiquetas = NULL, tipo_grafico = 1,
                            tipo_ordenamiento = 1, paleta_colores = NULL, alto_grafico = 300,
                            ancho_grafico = 300, mostrar_porcentajes = TRUE, max_chars_etiquetas = NULL) {
    
    req(nrow(df) > 0)
    
    # Filtrar los NA y las cadenas vacías
    df_limpio <- df %>% filter(!is.na(.data[[var_string]]) & .data[[var_string]] != "")
    
    # Verificar que el data.frame no esté vacío después del filtrado
    req(nrow(df_limpio) > 0)
    
    # Conteo y cálculo de porcentajes en los datos limpios
    if (tipo_ordenamiento == 1 && (tipo_grafico == 1 || tipo_grafico == 2)) { # Mayor a menor frecuencia
      df_sum <- df_limpio %>%
        count(.data[[var_string]], name = "n") %>%
        mutate(porcentaje = scales::percent(n / sum(n), accuracy = 1)) %>%
        arrange(desc(n))
    } else if (tipo_ordenamiento == 2 && (tipo_grafico == 1 || tipo_grafico == 2)) { # Menor a mayor frecuencia
      df_sum <- df_limpio %>%
        count(.data[[var_string]], name = "n") %>%
        mutate(porcentaje = scales::percent(n / sum(n), accuracy = 1)) %>%
        arrange(n)
    } else { # Orden original (alfabético o predefinido)
      df_sum <- df_limpio %>%
        count(.data[[var_string]], name = "n") %>%
        mutate(porcentaje = scales::percent(n / sum(n), accuracy = 1))
    }
    
    # Convertir la variable a factor
    df_sum[[var_string]] <- factor(df_sum[[var_string]], levels = unique(df_sum[[var_string]]))
    
    
    # Si la variable paleta_aleatoria() es TRUE, se elige una paleta al azar.
    # Si es FALSE, se usa la paleta_actual() que se le pasa como argumento
    if (paleta_aleatoria()) {
      # Usamos la lista de paletas para elegir una al azar
      nombre_paleta_aleatoria <- sample(paletas, 1)
      paleta_elegida <- get(nombre_paleta_aleatoria, envir = .GlobalEnv)
    } else {
      # Usamos la paleta_actual() que se le pasa como argumento
      paleta_elegida <- paleta_actual()
    }
    
    
    # Mapeo de etiquetas y gestión de texto
    etiquetas_finales <- levels(df_sum[[var_string]])
    if (!is.null(etiquetas)) {
      etiquetas_finales <- etiquetas[levels(df_sum[[var_string]])]
    }
    if (!is.null(max_chars_etiquetas)) {
      etiquetas_finales <- ifelse(nchar(etiquetas_finales) > max_chars_etiquetas,
                                  paste0(substr(etiquetas_finales, 1, max_chars_etiquetas), ""),
                                  etiquetas_finales)
    }
    if (tipo_grafico == 2) {
      etiquetas_finales <- paste0(etiquetas_finales, " ")
    }

    plot <- NULL
    
    # Tipo de gráfico
    if (tipo_grafico == 1) { # Gráfico de Columnas
      plot <- plot_ly(df_sum, x = ~df_sum[[var_string]], y = ~n, type = 'bar',
                      marker = list(color = paleta_elegida))
      if (mostrar_porcentajes) {
        plot <- plot %>% add_text(x = ~df_sum[[var_string]], y = ~n, text = ~lapply(porcentaje, function(p) paste0("<b>", p, "</b>")),
                    textposition = 'top', textfont = list(color = '#000000', size = 12), cliponaxis = FALSE)
      }
      plot <- plot %>%
        layout(xaxis = list(title = "", tickangle = 0, tickmode = "array", tickvals = levels(df_sum[[var_string]]),
                            ticktext = paste0("<b>", etiquetas_finales, "</b>")),
               yaxis = list(title = ""),
               showlegend = FALSE)
    } else if (tipo_grafico == 2) { # Gráfico de Barras
      # Para barras, el orden se invierte
      df_sum[[var_string]] <- factor(df_sum[[var_string]], levels = rev(levels(df_sum[[var_string]])))
      etiquetas_finales <- rev(etiquetas_finales)
      
      plot <- plot_ly(df_sum, x = ~n, y = ~df_sum[[var_string]], type = 'bar',
                      marker = list(color = paleta_elegida))
      if (mostrar_porcentajes) {
        
        # plot <- plot %>% add_text(x = ~n, y = ~df_sum[[var_string]], text = ~lapply(porcentaje, function(p) paste0("<b>", p, "</b>")),
        #                           textposition = 'outside', 
        #                           textfont = list(color = '#000000', size = 12),
        #                           marker = list(color = 'rgba(0,0,0,0)'),
        #                           cliponaxis = FALSE)
        
        plot <- plot %>%
          add_annotations(
            x = ~n,
            y = ~df_sum[[var_string]],
            text = ~lapply(porcentaje, function(p) paste0("<b>", p, "</b>")),
            xref = "x",
            yref = "y",
            xanchor = "left", # Alinea el texto a la izquierda del punto de anclaje
            showarrow = FALSE, # No muestra la flecha de la anotación
            xshift = 3, # Desplazamiento fijo en píxeles
            font = list(color = '#000000', size = 12)
          )
      }
      plot <- plot %>%
        layout(xaxis = list(title = ""),
               yaxis = list(title = "", tickmode = "array", tickvals = levels(df_sum[[var_string]]),
                            ticktext = paste0("<b>", etiquetas_finales, "</b>")),
               showlegend = FALSE)
    } else if (tipo_grafico == 3) { # Gráfico Circular
      labels_pie <- paste0(etiquetas_finales, " (", df_sum$porcentaje, ")")
      plot <- plot_ly(df_sum, labels = ~labels_pie, values = ~n, type = 'pie',
                      textinfo = 'text', text = ~porcentaje,
                      textposition = 'inside',
                      insidetextorientation = 'radial', marker = list(colors = paleta_elegida)) %>%
        layout(showlegend = TRUE,
               legend = list(x = 1.05, y = 0.5,
                             traceorder = "normal",
                             orientation = 'v',
                             title = list(text = "")))
    } else if (tipo_grafico == 4) { # Gráfico de Anillos
      labels_donut <- paste0(etiquetas_finales, " (", df_sum$porcentaje, ")")
      plot <- plot_ly(df_sum, labels = ~labels_donut, values = ~n, type = 'pie',
                      hole = 0.5, textinfo = 'text', text = ~porcentaje,
                      textposition = 'inside',
                      insidetextorientation = 'radial',
                      marker = list(colors = paleta_elegida)) %>%
        layout(showlegend = TRUE,
               legend = list(x = 1.05, y = 0.5,
                             traceorder = "normal",
                             orientation = 'v',
                             title = list(text = "")))
    }
    
    # Configuración de layout común
    plot <- plot %>%
      layout(
        title = list(text = paste0("<b>", titulo_grafico, "</b>"),
                     x = 0,
                     font = list(size = 14, family = "Arial", color = "#000000")),
        autosize = TRUE,
        #margin = list(l = 10, r = 10, b = 20, t = 20),
        margin = list(l = 40, r = 40, b = 40, t = 40),
        plot_bgcolor = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        width = ancho_grafico, # ancho_grafico,
        height = alto_grafico #alto_grafico
      ) %>%
      config(responsive = TRUE)
    
    return(plot)
  }

  
  ### PESTAÑA "ANÁLISIS" - "MAPA INTERACTIVO"
  # --- Centroide de CABA disponible globalmente en el server ---
  caba_centroid <- st_centroid(
    prov_sf %>% filter(nombre == "Ciudad Autónoma de Buenos Aires")
  )
  
  output$mapa <- renderLeaflet({
    provincias_mapa <- prov_sf %>% filter(nombre %in% PROVINCIAS_DESTACADAS)
    req(nrow(provincias_mapa) > 0)
    
    provincias_con_id <- provincias_mapa %>%
      mutate(id_casilla = case_when(
        nombre == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
        nombre == "Buenos Aires" ~ "Buenos Aires",
        nombre == "Chubut" ~ "Chubut",
        nombre == "Córdoba" ~ "Córdoba",
        nombre == "Río Negro" ~ "Río Negro",
        nombre == "Tucumán" ~ "Tucumán",
        TRUE ~ NA_character_
      ))
    
    leaflet(provincias_con_id) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom = 3, maxZoom = 7)) %>%
      addPolygons(
        fillColor = ~case_when(
          nombre == "Ciudad Autónoma de Buenos Aires" ~ "blue",
          nombre %in% PROVINCIAS_DESTACADAS ~ "#49E0FF",
          TRUE ~ "lightblue"
        ),
        color = "white",
        weight = 1,
        fillOpacity = 0.7,
        label = ~nombre,
        layerId = ~id_casilla,
        highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)
      ) %>%
      # marcador para CABA
      addMarkers(data = caba_centroid, popup = "CABA", layerId = "CABA") %>%
      fitBounds(
        lng1 = min(st_bbox(provincias_con_id)[1]),
        lat1 = min(st_bbox(provincias_con_id)[2]),
        lng2 = max(st_bbox(provincias_con_id)[3]),
        lat2 = max(st_bbox(provincias_con_id)[4])
      )
  })
  
  # Resaltar provincia cuando se pasa por el div
  observeEvent(input$provincia_card_hover, {
    prov <- input$provincia_card_hover
    proxy <- leafletProxy("mapa")
    
    proxy %>% clearGroup("hover")
    
    if (!is.null(prov)) {
      if (prov == "CABA") {
        proxy %>% addCircleMarkers(
          data = caba_centroid,
          radius = 15,
          color = "black",
          fill = FALSE,
          weight = 3,
          group = "hover"
        )
      } else {
        geom <- prov_sf %>% filter(nombre == prov)
        proxy %>% addPolygons(
          data = geom,
          color = "black",
          weight = 4,
          fill = FALSE,
          group = "hover"
        )
      }
    }
  }, ignoreNULL = FALSE)
  
  # output$mapa <- renderLeaflet({
  #   
  #   provincias_mapa <- prov_sf %>% filter(nombre %in% PROVINCIAS_DESTACADAS)
  #   req(nrow(provincias_mapa) > 0)
  #   
  #   # Normalización de los nombres de las provincias para que coincidan
  #   # con los de las casillas de verificación
  #   provincias_con_id <- provincias_mapa %>%
  #     dplyr::mutate(
  #       id_casilla = dplyr::case_when(
  #         nombre == "Ciudad Autónoma de Buenos Aires" ~ "CABA",
  #         nombre == "Buenos Aires" ~ "Buenos Aires",
  #         nombre == "Chubut" ~ "Chubut",
  #         nombre == "Córdoba" ~ "Córdoba",
  #         nombre == "Río Negro" ~ "Río Negro",
  #         nombre == "Tucumán" ~ "Tucumán",
  #         TRUE ~ NA_character_ # Asigna NA a las que no coincidan
  #       )
  #     )
  #   
  #   # Centroide de CABA para el marcador
  #   caba_centroid <- st_centroid(provincias_con_id[provincias_con_id$id_casilla == "CABA", ])
  #   
  #   leaflet(provincias_con_id) %>%
  #     # Mapa base
  #     addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom = 3, maxZoom = 7)) %>%
  #     
  #     # Capa de polígonos
  #     addPolygons(
  #       fillColor = ~dplyr::case_when(
  #         nombre == "Ciudad Autónoma de Buenos Aires" ~ "blue", # Color para CABA
  #         nombre %in% PROVINCIAS_DESTACADAS ~ "#49E0FF", # Color para el resto
  #         TRUE ~ "lightblue" # Color por defecto para las que no están destacadas
  #       ),
  #       color = "white",
  #       weight = 1,
  #       fillOpacity = 0.7,
  #       label = ~nombre,
  #       layerId = ~id_casilla, # Usa el ID normalizado para la selección
  #       highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)
  #     ) %>%
  #     
  #     # Se agrega un marcador para CABA porque es muy pequeño el polígono
  #     # que le corresponde, para seleccionarla sin hacer zoom --- NO ESTÁ FUNCIONANDO!!
  #     addMarkers(
  #       data = caba_centroid,
  #       popup = "CABA",
  #       layerId = "CABA" # El ID del marcador también es "CABA"
  #     ) %>%
  #     
  #     # Ajusta el zoom del mapa a las provincias destacadas
  #     fitBounds(
  #       lng1 = min(st_bbox(provincias_con_id)[1]),
  #       lat1 = min(st_bbox(provincias_con_id)[2]),
  #       lng2 = max(st_bbox(provincias_con_id)[3]),
  #       lat2 = max(st_bbox(provincias_con_id)[4])
  #     )
  # })
  
  
  ### CAMPO INPUT QUE SE CREA AUTOMÁTICAMENTE PARA DETECTAR EL POLÍGONO ELEGIDO
  observeEvent(input$mapa_shape_click, {
    req(input$mapa_shape_click$id)
    provincia_clickeada <- input$mapa_shape_click$id
    updateCheckboxGroupInput(session, "filtro_prov", selected = provincia_clickeada)
    updateNavbarPage(session, "main_nav", selected = "Investigacion")
  }, priority = 1) # Prioridad baja (por defecto)
  
  ### CAMPO INPUT PARA DETECTAR EL CLIC SOBRE EL MARCADOR DE CABA --- NO FUNCIONA!!
  observeEvent(input$mapa_marker_click, {
    req(input$mapa_marker_click$id) # Asegura que haya un ID asociado al marcador
    provincia_clickeada <- input$mapa_marker_click$id
    updateCheckboxGroupInput(session, "filtro_prov", selected = provincia_clickeada)
    updateNavbarPage(session, "main_nav", selected = "Población")
  }, priority = 10) # Prioridad alta para que se ejecute primero
  
  
  
  ### PESTAÑA "ANÁLISIS" - SUBPESTAÑA "NUBE DE PALABRAS"
  
  output$nube <- renderWordcloud2({
    
    # Si el timer está activo, se genera la nube cada 2 segundos.
    if (timer_activo() == TRUE)
      invalidateLater(2000, session)
    
    req(narrativas_data$limpios)
    
    corpus <- Corpus(VectorSource(narrativas_data$limpios))
    tdm <- TermDocumentMatrix(corpus)
    matriz <- as.matrix(tdm)
    freq <- sort(rowSums(matriz), decreasing = TRUE)
    df_nube <- data.frame(palabra = names(freq), freq = freq)
    
    # Genera la nube de palabras con forma de elipse/circl
    wordcloud2(
      data = df_nube, 
      size = 0.5, 
      shape = 'circle', # O 'ellipse' si estuviera disponible, pero 'circle' es la opción más cercana
      color = 'random-light',
      backgroundColor = "white"
    )
  })
  
  ### BOTÓN PARA INICIAR/DETENER LA NUBE DE PALABRAS
  observeEvent(input$btn_nu1, {
    timer_activo(!timer_activo())
  })
  
  
  ### PESTAÑA "ANÁLISIS" - SUBPESTAÑA "ANÁLISIS DE SENTIMIENTOS"
  
  output$sentimientos <- renderPlotly({
    sentimientos <- get_nrc_sentiment(narrativas_data$limpios)[, 1:8]
    df_sent <- data.frame(
      sentimiento_en = names(colSums(sentimientos)),
      cantidad = colSums(sentimientos)
    ) %>%
      mutate(
        sentimiento_es = case_when(
          sentimiento_en == "anger" ~ "Ira",
          sentimiento_en == "anticipation" ~ "Anticipación",
          sentimiento_en == "disgust" ~ "Repulsión",
          sentimiento_en == "fear" ~ "Miedo",
          sentimiento_en == "joy" ~ "Alegría",
          sentimiento_en == "sadness" ~ "Tristeza",
          sentimiento_en == "surprise" ~ "Sorpresa",
          sentimiento_en == "trust" ~ "Confianza"
        )
      )
    
    # Calcular los porcentajes
    total_sentimientos <- sum(df_sent$cantidad)
    df_sent$porcentaje <- round((df_sent$cantidad / total_sentimientos) * 100, 2)
    
    # Crear el gráfico de barras
    fig <- plot_ly(df_sent, x = ~reorder(sentimiento_es, -cantidad), y = ~cantidad, type = "bar",
                   marker = list(
                     color = PALETA_PRADERA,
                     line = list(color = 'rgb(8,48,107)', width = 1.5),
                     opacity = 0.8
                   )) %>%
      layout(
        xaxis = list(
          title = "Sentimientos",
          titlefont = list(family = "sans-serif", size = 18, color = 'black'),
          tickfont = list(family = "sans-serif", size = 14, color = 'black', weight = "bold")
        ),
        yaxis = list(
          title = "Cantidad de Respuestas",
          titlefont = list(family = "sans-serif", size = 18, color = 'black', weight = "bold"),
          tickfont = list(family = "sans-serif", size = 14, color = 'black', weight = "bold")
        ),
        plot_bgcolor = 'transparent',
        paper_bgcolor = 'transparent',
        #margin = list(b = 40, t = 40)
        margin = list(l = 80, r = 80, b = 40, t = 40)
      )
    
    # Agregar los porcentajes como anotaciones
    fig <- fig %>%
      add_annotations(
        x = df_sent$sentimiento_es,
        y = df_sent$cantidad,
        text = paste0("<b>", df_sent$porcentaje, "%</b>"),
        xref = "x",
        yref = "y",
        showarrow = FALSE,
        yshift = 10 # Desplazar el texto un poco hacia arriba de la columna
      )
    
    return(fig)
  })
  
  
  ### PESTAÑA "ANÁLISIS" - SUBPESTAÑA "DIAGRAMA DE SANKEY"
  output$sankey <- renderPlotly({
    req(narrativas_data, narrativas_data$limpios)
    
    Negativo_color <- "rgba(239,83,80,0.6)"
    Positivo_color <- "rgba(102,187,106,0.6)"
    Mixto_color    <- "rgba(255,167,38,0.6)"
    Neutro_color   <- "rgba(144,164,174,0.6)"
    
    sents_simple <- get_nrc_sentiment(narrativas_data$limpios) %>%
      dplyr::mutate(
        sentimiento_general = dplyr::case_when(
          positive > 0 & negative == 0 ~ "Positivo",
          negative > 0 & positive == 0 ~ "Negativo",
          positive > 0 & negative > 0  ~ "Mixto",
          TRUE                         ~ "Neutro"
        )
      )
    
    categ_tema <- function(tx) {
      tx <- tolower(tx)
      if (grepl("trabajo|laboral|inclusion|entrevista", tx)) return("Trabajo")
      if (grepl("familia|padres|madre|hermano", tx))        return("Familia")
      if (grepl("tecnolog", tx))                          return("Tecnología")
      if (grepl("escuela|estudio|colegio", tx))          return("Educación")
      "Otro"
    }
    
    temas <- vapply(narrativas_data$limpios, categ_tema, character(1))
    
    df_sankey <- tibble::tibble(origen = "Narrativas", sentimiento = sents_simple$sentimiento_general, destino = temas)
    
    nodos <- unique(c(df_sankey$origen, df_sankey$sentimiento, df_sankey$destino))
    idx   <- function(x) match(x, nodos) - 1
    
    # Define the node colors, including the previous colors for themes
    node_colors <- c(
      "Narrativas" = "rgba(49, 130, 189, 0.8)",
      "Neutro" = Neutro_color, 
      "Negativo" = Negativo_color, 
      "Mixto" = Mixto_color, 
      "Positivo" = Positivo_color,
      "Trabajo" = "rgba(102, 187, 106, 0.8)", # Color Verde para Trabajo
      "Familia" = "rgba(135, 206, 235, 0.5)", # Color Celeste para Familia
      "Tecnología" = "rgba(239, 83, 80, 0.8)", # Color Rojo para Tecnología
      "Educación" = "rgba(255, 167, 38, 0.8)", # Color Naranja para Educación
      "Otro" = "rgba(144, 164, 174, 0.8)" # Color Gris para Otro
    )
    
    node_color_vector <- node_colors[nodos]
    
    # Links 1: Narrativas -> Sentimiento
    links1 <- df_sankey %>%
      dplyr::count(origen, sentimiento, name = "value") %>%
      dplyr::mutate(
        color = dplyr::case_when(
          sentimiento == "Negativo" ~ gsub("0.6", "0.4", Negativo_color),
          sentimiento == "Positivo" ~ gsub("0.6", "0.4", Positivo_color),
          sentimiento == "Mixto"    ~ gsub("0.6", "0.4", Mixto_color),
          TRUE                      ~ gsub("0.6", "0.4", Neutro_color)
        ),
        source = idx(origen), target = idx(sentimiento)
      )
    
    # Links 2: Sentimiento -> Destino
    links2 <- df_sankey %>%
      dplyr::count(sentimiento, destino, name = "value") %>%
      dplyr::mutate(
        color = dplyr::case_when(
          sentimiento == "Negativo" ~ gsub("0.6", "0.4", Negativo_color),
          sentimiento == "Positivo" ~ gsub("0.6", "0.4", Positivo_color),
          sentimiento == "Mixto"    ~ gsub("0.6", "0.4", Mixto_color),
          TRUE                      ~ gsub("0.6", "0.4", Neutro_color)
        ),
        source = idx(sentimiento), target = idx(destino)
      )
    
    links <- dplyr::bind_rows(links1, links2)
    
    plot_ly(
      type = "sankey",
      node = list(
        label = nodos,
        color = node_color_vector,
        pad = 15,
        thickness = 20,
        hovertemplate = '<b>%{label}</b><br>Value: %{value}<extra></extra>',
        label.font = list(
          family = "sans-serif",
          size = 18,
          color = "black"
        )
      ),
      link = list(
        source = links$source,
        target = links$target,
        value  = links$value,
        color  = links$color
      )
    ) %>% layout(
      title = "",
      margin = list(l = 80, r = 80, b = 160, t = 40, pad = 10),
      plot_bgcolor = 'transparent',
      paper_bgcolor = 'transparent',
      
      annotations = list(
        list(x = 0.05, y = -0.15, text = "Neutro → Narrativas sin carga emocional clara.",
             showarrow = FALSE, xref = "paper", yref = "paper",
             font = list(size = 14, color = "black"), align = "left"),
        list(x = 0.05, y = -0.25, text = "Positivo → Narrativas con connotación favorable.",
             showarrow = FALSE, xref = "paper", yref = "paper",
             font = list(size = 14, color = "black"), align = "left"),
        list(x = 0.05, y = -0.35, text = "Negativo → Narrativas con connotación desfavorable.",
             showarrow = FALSE, xref = "paper", yref = "paper",
             font = list(size = 14, color = "black"), align = "left"),
        list(x = 0.05, y = -0.45, text = "Mixto → Narrativas con percepciones encontradas.",
             showarrow = FALSE, xref = "paper", yref = "paper",
             font = list(size = 14, color = "black"), align = "left")
      ),
      
      shapes = list(
        list(type = "rect", xref = "paper", yref = "paper",
             x0 = 0.00, x1 = 0.04, y0 = -0.17, y1 = -0.13,
             fillcolor = Neutro_color, line = list(width = 0)),
        list(type = "rect", xref = "paper", yref = "paper",
             x0 = 0.00, x1 = 0.04, y0 = -0.27, y1 = -0.23,
             fillcolor = Positivo_color, line = list(width = 0)),
        list(type = "rect", xref = "paper", yref = "paper",
             x0 = 0.00, x1 = 0.04, y0 = -0.37, y1 = -0.33,
             fillcolor = Negativo_color, line = list(width = 0)),
        list(type = "rect", xref = "paper", yref = "paper",
             x0 = 0.00, x1 = 0.04, y0 = -0.47, y1 = -0.43,
             fillcolor = Mixto_color, line = list(width = 0))
      )
    )
  })
  
  
  ### PESTAÑA "INVESTIGACIÓN" - SUBPESTAÑA "DEMOGRAFÍA"
  
  output$plot_dem1 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Demografia")
      crear_grafico(df = datos_filtrados(), var_string = "Provincia", titulo_grafico = "Encuestados por Provincias",
        etiquetas = etiquetas_prov, tipo_grafico = opcion_grafico1(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
        alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  
  output$plot_dem2 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Demografia")
      crear_grafico(df = datos_filtrados(), var_string = "Genero", titulo_grafico = "Encuestados por Géneros",
                  etiquetas = etiquetas_gen, tipo_grafico = opcion_grafico2(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  
  output$plot_dem3 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Demografia")
      crear_grafico(df = datos_filtrados(), var_string = "Edad", titulo_grafico = "Encuestados por Grupos Etarios",
                  etiquetas = etiquetas_edad, tipo_grafico = opcion_grafico3(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  
  output$plot_dem4 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Demografia")
      crear_grafico(df = datos_filtrados(), var_string = "Educacion", titulo_grafico = "Encuestados por Nivel Educativo",
                  etiquetas = etiquetas_educ, tipo_grafico = opcion_grafico4(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  
  
  ### PESTAÑA "INVESTIGACIÓN" - SUBPESTAÑA "TABLA"
  
  output$tabla_poblacion <- DT::renderDT({
    df_tabla <- datos_filtrados() %>%
      select(Provincia, Genero, Edad, Educacion)
    
    datatable(
      df_tabla,
      options = list(pageLength = 10,
                     # lengthMenu = c(5, 10, 20, 50),
                     lengthChange = FALSE,
                     searching = FALSE,
                     language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json')),
                     rownames = FALSE,
      colnames = c("Provincia", "Género", "Edad", "Educación")
    )
  })
  
  
  ### PESTAÑA "INVESTIGACIÓN" - SUBPESTAÑA "TRABAJO"
  
  output$plot_work1 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Trabajo")
      crear_grafico(df = datos_filtrados(), var_string = "Trabaja", titulo_grafico = "Con Empleo",
                  etiquetas = NULL, tipo_grafico = opcion_grafico1(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w3_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  
  output$plot_work2 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Trabajo")
      crear_grafico(df = datos_filtrados(), var_string = "Sector", titulo_grafico = "Sector Laboral",
                  etiquetas = NULL, tipo_grafico = opcion_grafico2(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w3_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 4)
  })
  
  output$plot_work3 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Trabajo")
      crear_grafico(df = datos_filtrados(), var_string = "Tipo_Empleo", titulo_grafico = "Tipo de Empleo",
                  etiquetas = NULL, tipo_grafico = opcion_grafico3(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w3_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 4)
  })
  
  output$plot_work4 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Trabajo")
      crear_grafico(df = datos_filtrados(), var_string = "Cant_Empleos", titulo_grafico = "Empleos en los Últimos 5 Años",
                  etiquetas = NULL, tipo_grafico = opcion_grafico4(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w3_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  
  output$plot_work5 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Trabajo")
      crear_grafico(df = datos_filtrados(), var_string = "Ayuda", titulo_grafico = "Ayuda de ONGs/Instituciones",
                  etiquetas = NULL, tipo_grafico = opcion_grafico5(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w3_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 4)
  })
  
  output$plot_work6 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Trabajo")
      crear_grafico(df = datos_filtrados(), var_string = "Permanencia", titulo_grafico = "Dificultades en el Trabajo",
                  etiquetas = NULL, tipo_grafico = opcion_grafico6(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w3_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 4)
  })
  
  
  ### PESTAÑA "INVESTIGACIÓN" - SUBPESTAÑA "INCLUSIÓN"
  
  output$plot_inc1 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Inclusion")
      crear_grafico(df = datos_filtrados(), var_string = "Experiencia_Inclusiva", titulo_grafico = "Experiencia Inclusiva",
                  etiquetas = etiquetas_calificacion, tipo_grafico = opcion_grafico1(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  
  output$plot_inc2 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Inclusion")
      crear_grafico(df = datos_filtrados(), var_string = "Recibio_Adaptacion", titulo_grafico = "Adaptación a la Hipoacusia en el Ambiente Laboral",
                  etiquetas = NULL, tipo_grafico = opcion_grafico2(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 4)
  })
  
  output$plot_inc3 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Inclusion")
      crear_grafico(df = datos_filtrados(), var_string = "Tipo_Adaptacion", titulo_grafico = "Tipo de Adaptación a la Hipoacusia",
                  etiquetas = NULL, tipo_grafico = opcion_grafico3(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 4)
  })
  
  output$plot_inc4 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Inclusion")
      crear_grafico(df = datos_filtrados(), var_string = "Adaptacion_Satisfactoria", titulo_grafico = "Satisfacción por la Adaptación a la Hipoacusia",
                  etiquetas = etiquetas_satisfaccion, tipo_grafico = opcion_grafico4(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  
  
  
  ### PESTAÑA "INVESTIGACIÓN" - SUBPESTAÑA "TECNOLOGÍA"
  
  output$plot_tec1 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Tecnologia")
      crear_grafico(df = datos_filtrados(), var_string = "Uso_Tecnologia", titulo_grafico = "Empleo de Tecnología para la Hipoacusia",
                  etiquetas = NULL, tipo_grafico = opcion_grafico1(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  
  output$plot_tec2 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Tecnologia")
      crear_grafico(df = datos_filtrados(), var_string = "Tipo_Tecnologia", titulo_grafico = "Tipo de Tecnología Utilizada",
                  etiquetas = NULL, tipo_grafico = opcion_grafico2(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 4)
  })
  
  output$plot_tec3 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Tecnologia")
      crear_grafico(df = datos_filtrados(), var_string = "Tecnologia_Beneficiosa", titulo_grafico = "Tecnologías que podrían ser beneficiosas",
                  etiquetas = etiquetas_mejoras, tipo_grafico = opcion_grafico3(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  
  output$plot_tec4 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "Tecnologia")
      crear_grafico(df = datos_filtrados(), var_string = "Conocimiento_IA", titulo_grafico = "Nivel de conocimiento sobre IA",
                  etiquetas = etiquetas_satisfaccion, tipo_grafico = opcion_grafico4(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  
  
  
  ### PESTAÑA "INVESTIGACIÓN" - SUBPESTAÑA "INTELIGENCIA ARTIFICIAL"
  
  output$plot_ia1 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "IA")
      crear_grafico(df = datos_filtrados(), var_string = "IA_Mejora", titulo_grafico = "La IA mejorará la Experiencia Laboral",
                  etiquetas = NULL, tipo_grafico = opcion_grafico1(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  
  output$plot_ia2 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "IA")
      crear_grafico(df = datos_filtrados(), var_string = "Importancia_Capacitacion", titulo_grafico = "Importancia de la Capacitación en IA",
                  etiquetas = etiquetas_importancia, tipo_grafico = opcion_grafico2(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  
  output$plot_ia3 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "IA")
      crear_grafico(df = datos_filtrados(), var_string = "Prioridad_IA", titulo_grafico = "Prioridad de Aplicar la IA en el Trabajo",
                  etiquetas = etiquetas_prioridad, tipo_grafico = opcion_grafico3(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  
  output$plot_ia4 <- renderPlotly({
    req(nrow(datos_filtrados()) > 0)
    if (input$panel_poblacion == "IA")
      crear_grafico(df = datos_filtrados(), var_string = "Privacidad_Datos", titulo_grafico = "Importancia de la Privacidad de Datos en la IA",
                  etiquetas = etiquetas_importancia, tipo_grafico = opcion_grafico4(), tipo_ordenamiento = opcion_orden(), paleta_colores = paleta_actual(),
                  alto_grafico = h_graph, ancho_grafico = w2_graph, mostrar_porcentajes = TRUE, max_chars_etiquetas = 10)
  })
  

  
  
  # ### PESTAÑA "MODELOS" - SUBPESTAÑA "BOSQUE ALEATORIO" - SUBPESTAÑA "ESTRUCTURA DE DATOS"
  # 
  # output$estructura <- renderPrint({
  #   str(valores$resultado$df)
  # })  
  # 
  # 
  # ### PESTAÑA "MODELOS" - SUBPESTAÑA "BOSQUE ALEATORIO" - SUBPESTAÑA "RESULTADOS" 
  # 
  # output$matriz_calor <- renderPlot({
  #   # Asegúrate de que 'valores$resultado$cm' es una matriz de confusión
  #   # Si es un objeto de tipo 'table', conviértelo a un data frame
  #   if (inherits(valores$resultado$cm, "table")) {
  #     cm_df <- as.data.frame(valores$resultado$cm)
  #     colnames(cm_df) <- c("Predicho", "Real", "Frecuencia")
  #   } else {
  #     # Si ya es un data frame, úsalo directamente
  #     cm_df <- valores$resultado$cm
  #   }
  #   
  #   # Crea el gráfico de matriz de calor
  #   ggplot(cm_df, aes(x = Predicho, y = Real, fill = Frecuencia)) +
  #     geom_tile(color = "white") +
  #     geom_text(aes(label = Frecuencia), vjust = 1) +
  #     scale_fill_gradient(low = "lightblue", high = "darkblue") +
  #     labs(
  #       title = "Matriz de Confusión",
  #       # subtitle = paste("Accuracy:", round(valores$resultado$acc, 3)),
  #       x = "Predicho",
  #       y = "Real"
  #     ) +
  #     theme_minimal() +
  #     theme(
  #       plot.title = element_text(size = 20, face = "bold"),
  #       # plot.subtitle = element_text(size = 14, face = "italic"),
  #       axis.title = element_text(size = 16, face = "bold"),
  #       axis.text = element_text(size = 16, face = "bold"),
  #       legend.title = element_text(size = 14, face = "bold")
  #     )
  # })
  # # output$accuracy_text <- renderPrint({
  # #   list(
  # #     "Accuracy" = round(valores$resultado$acc, 3)
  # #   )
  # # })
  # 
 
  
   
  ### PESTAÑA "CONCLUSIÓN" - SUBPESTAÑA "VARIABLES IMPORTANTES" - SUBPESTAÑA "TABLA"

  # output$tabla_importancia <- renderTable({
  # 
  #   importancia_df <- as.data.frame(importance(valores$resultado$modelo))
  # 
  #   # Se ordena el dataframe en orden descendente, si es de clasificación, en base
  #   # a la columna 'MeanDecreaseGini' y si es de regresión, en base a 'MeanDecreaseAccuracy')
  #   importancia_ordenada <- importancia_df[order(importancia_df$MeanDecreaseAccuracy, decreasing = TRUE), ]
  # 
  #   # Se quita la columna de la métrica Gini, para facilitar el entendimiento del ordenamiento de las variables
  #   importancia_ordenada <- importancia_ordenada[, -which(names(importancia_ordenada) == "MeanDecreaseGini")]
  # 
  #   # Se cambia el nombre de la columna de la métrica por uno más simple
  #   importancia_ordenada <- importancia_ordenada %>% rename(Importancia = MeanDecreaseAccuracy)
  # 
  #   importancia_ordenada
  # }, rownames = TRUE)
  
  
  output$tabla_importancia <- renderTable({
    # Usar el objeto reactivo modificado
    importancia_df <- importancia_modelo()
    
    # Ordenar el data.frame por la métrica de precisión de mayor a menor
    importancia_ordenada <- importancia_df %>% 
      arrange(desc(MeanDecreaseAccuracy))
    
    # Opcional: renombrar la columna principal para mayor claridad
    importancia_ordenada <- importancia_ordenada %>%
      rename(Importancia = MeanDecreaseAccuracy,
             Variable = Variables)
    
    importancia_ordenada
  }, rownames = FALSE)
  
  
  
  ### PESTAÑA "CONCLUSIÓN" - SUBPESTAÑA "VARIABLES IMPORTANTES" - SUBPESTAÑA "GRÁFICO"
  
  # output$grafico_importancia <- renderPlot({
  #   # 1. Extraer importancia
  #   imp <- importance(valores$resultado$modelo)
  #   imp_df <- data.frame(
  #     Variables = rownames(imp),
  #     Importancia = imp[, 1]  # Se toma la métrica MeanDecreaseAccuracy
  #   )
  #   
  #   # 2. Ordenar por importancia
  #   imp_df <- imp_df[order(imp_df$Importancia, decreasing = FALSE), ]
  #   imp_df$Variables <- factor(imp_df$Variables, levels = imp_df$Variables)
  #   
  #   ggplot(imp_df, aes(x = Variables, y = Importancia, color = Importancia, size = Importancia)) +
  #     geom_point() +
  #     scale_color_gradient(low = "skyblue", high = "darkblue") +  # degradado azul
  #     theme_minimal(base_size = 14) +
  #     coord_flip() +
  #     labs(title = "IMPORTANCIA DE LAS VARIABLES EN EL MODELO",
  #          x = "",
  #          y = "",
  #          color = "Nivel",
  #          size = "Peso") +
  #     theme(
  #       plot.title = element_text(face = "bold", hjust = 0), # Título en negrita y alineado a la izquierda
  #       axis.text = element_text(face = "bold"),
  #       axis.text.y = element_text(hjust = 0), # Etiquetas alineadas a la izquierda
  #     )
  # })

  output$grafico_importancia <- renderPlot({
    # Usar el objeto reactivo
    imp_df <- importancia_modelo()
    
    # Ordenar el data.frame por importancia de MAYOR a MENOR
    imp_df <- imp_df %>%
      arrange(desc(MeanDecreaseAccuracy))
    
    # Convertir Variables a factor e INVERTIR el orden
    imp_df$Variables <- factor(imp_df$Variables, levels = rev(imp_df$Variables))
    
    ggplot(imp_df, aes(x = Variables, y = MeanDecreaseAccuracy, color = MeanDecreaseAccuracy, size = MeanDecreaseAccuracy)) +
      geom_point() +
      scale_color_gradient(low = "skyblue", high = "darkblue") +
      theme_minimal(base_size = 14) +
      coord_flip() +
      labs(title = "IMPORTANCIA DE LAS VARIABLES EN EL MODELO",
           x = "",
           y = "",
           color = "Nivel",
           size = "Peso") +
      theme(
        plot.title = element_text(face = "bold", hjust = 0),
        axis.text = element_text(face = "bold"),
        axis.text.y = element_text(hjust = 0),
      )
  })
  
  
  ### PESTAÑA "CONCLUSIÓN" - BOTÓN "ENTRENAR MODELO"
  
  # Random Forest -> Reactive con las top N variables 
  topVars <- reactive({
    req(valores$resultado$modelo)  # Nos aseguramos que el RF ya se entrenó
    imp <- importance(valores$resultado$modelo)
    imp_df <- data.frame(Variables = rownames(imp), Importancia = imp[, 1])
    imp_df <- imp_df[order(imp_df$Importancia, decreasing = TRUE), ]
    topN <- input$cantVar
    imp_df$Variables[1:topN]
  })
  
  
  ################~~~~Redes Neuronales~~~~########################
  modeloRN <- reactiveVal(NULL)  # Para guardar la red neuronal
  means_scaled <- reactiveVal(NULL)   # Para guardar Medias de features
  sds_scaled   <- reactiveVal(NULL)   # Para guardar Desvios de features
  targetColsRV <- reactiveVal(NULL)   # Para guardar Columnas de salida (clases)
  
  
  ### PARA INDICAR QUE LA RED NEURONAL NO FUE ENTRENADA
  ### CUANDO SE MODIFICA LA CANTIDAD DE VARIABLES O EL NÚMERO DE CAPAS
  observeEvent(input$cantVar, {
    model_state(FALSE)
  })
  observeEvent(input$capa, {
    model_state(FALSE)
  })

    
  ### BOTÓN ENTRENAR RED NEURONAL  
  observeEvent(input$BtnTrain, {
    data <- dsNumeric()
    data <- na.omit(data)   # quitar filas NA
    
    target <- data$Target
    features <- data %>% select(-Target)
    
    # Eliminar columnas inválidas
    features <- features %>%
      select(where(~ !all(is.na(.)))) %>%
      select(where(~ n_distinct(.) > 1)) %>%
      select(where(~ sum(.) > 0))
    
    # ---> Seleccionar solo las variables más importantes <---
    top_vars <- topVars()   # usa el reactive definido arriba
    features <- features %>% select(all_of(top_vars))
    
    # Escalar con medias y sd de entrenamiento
    m <- apply(features, 2, mean) # calcula la media
    s <- apply(features, 2, sd) #calcula la desviacion stantard
    features_scaled <- scale(features, center = m, scale = s) %>% as.data.frame()
    
    # Redondear las variables escaladas a 4 decimales
    features_scaled <- round(features_scaled, digits = 4)
    
    valores$features_scaled <- features_scaled
    
    # Dataset final
    ##data_scaled <- cbind(features_scaled, Target = target)
    
    # Dataset final: Convertir Target a dummies
    target <- as.factor(target)  # asegurar que sea factor
    target_dummies <- model.matrix(~ target - 1)  # dummies multiclase
    data_nn <- cbind(features_scaled, target_dummies)
    
    
    # Partición train/test
    set.seed(123)
    trainIndex <- sample(1:nrow(data_nn), 0.7*nrow(data_nn))
    trainData <- data_nn[trainIndex, ]
    testData  <- data_nn[-trainIndex, ]
    
    # Fórmula dinámica: todas las dummies de Target como salida
    targetCols <- colnames(target_dummies)
    inputCols  <- setdiff(colnames(data_nn), targetCols)
    f <- as.formula(
      paste(paste(targetCols, collapse = " + "), "~", paste(inputCols, collapse = " + "))
    )
    
    # ---> Usar el valor del input 'capa' <---
    nn <- neuralnet(f, 
                    data = trainData, 
                    hidden = c(input$capa),  # capas ocultas dinámicas
                    linear.output = FALSE)
    
    # Guardo el modelo entrenado en la reactiveVal
    modeloRN(nn)
    means_scaled(m)
    sds_scaled(s)
    targetColsRV(targetCols)
    # 👉 Guardar en reactive para usarlo en la UI
    valores$data_nn <- data_nn
    
    # Evaluar en test
    pred <- compute(nn, testData[, inputCols])$net.result
    pred_class <- apply(pred, 1, which.max)
    true_class <- apply(testData[, targetCols], 1, which.max)
    acc <- mean(pred_class == true_class)
    
    valores$acc <- acc
    
    model_state(TRUE)
    
  })
  
  
  output$info_modelo <- renderUI({
    
    if (model_state()) { ### Si se entrenó el modelo de Red Neuronal
      req(valores$features_scaled)
      req(valores$acc)
      
      info_html <- paste(
        "<strong>MODELO ENTRENADO CON ÉXITO ✅</strong><br><br>",
        "<strong>Variables usadas:</strong><br>",
        "<ul>",
        paste("<li>", colnames(valores$features_scaled), "</li>", collapse = ""), 
        "</ul><br>",
        "<strong>Neuronas en la capa oculta:</strong>", input$capa, "<br><br>",
        "<strong>Precisión en el conjunto de prueba:</strong>", round(valores$acc*100, 2), "%" 
      )
      div(style = "margin: 20px;", HTML(info_html))
    } else {
      div(
        style = "margin: 20px; color: black; font-size: 1.2em;",
        HTML("<strong>Debe entrenarse la red neuronal para ver los datos.</strong><br><br>
            Haga clic en el botón <b>Entrenar Red Neuronal</b>.")
      )
    }
  })
  

  ### PESTAÑA "CONCLUSIÓN" - SUBPESTAÑA "MODELO DE RED NEURONAL" - SUBPESTAÑA "DATOS ESCALADOS"  
  
  output$tabla_datos_escalados <- renderUI({
    
      if (model_state()) { ### Si se entrenó el modelo de Red Neuronal
        req(valores$data_nn)
        req(valores$features_scaled)
        req(valores$acc)
        
        ### El siguiente código es para mejorar el nombre de las columnas "target" y
        ### ordenarlas desde la menor categoría hacia la mayor
        datos_renombrados <- valores$data_nn
        colnames(datos_renombrados) <- sub("^target", "", colnames(datos_renombrados)) # Se quita la palabra "target"
        orden_deseado_target <- c("MAL", "REG", "BUE", "MBU", "EXC", "NES") # Se define el nuevo ordenamiento
        columnas_predictoras <- colnames(datos_renombrados)[!colnames(datos_renombrados) %in% orden_deseado_target]
        orden_final <- c(columnas_predictoras, orden_deseado_target)
        datos_ordenados <- datos_renombrados[, orden_final] # Se ordena el dataframe
        
        
        # Renderiza la tabla de datos
        DT::renderDataTable({
          datatable(
            datos_ordenados,
            options = list(pageLength = 9,
                           scrollX = TRUE,
                           lengthChange = FALSE,
                           searching = FALSE,
                           language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json')),
            rownames = FALSE
          )
        })
      } else{ 
        div(
          style = "margin: 20px; color: black; font-size: 1.2em;",
          HTML("<strong>Debe entrenarse la red neuronal para ver los datos.</strong><br><br>
            Haga clic en el botón <b>Entrenar Red Neuronal</b>.")
        )
      }
    })

  
  ### PESTAÑA "CONCLUSIÓN" - SUBPESTAÑA "PREDICCIÓN"
  
  output$dynamicInputs <- renderUI({
    req(topVars())
    
    lapply(topVars(), function(varname) {
      fila <- equivalencias[equivalencias$infoModelo == varname, ]
      req(nrow(fila) == 1)  # asegurar que encontró equivalencia
      
      valores_originales <- sort(unique(datos_poblacion[[fila$original]]))
      
      selectInput(
        inputId = paste0("input_", varname),  # ID = topVar (modelo)
        label   = fila$titulo,                # Título amigable
        choices = c("Seleccione..." = "", valores_originales),
        selected = NULL,##valores_originales[1]
        selectize = TRUE  # Asegura comportamiento moderno
      )
    })
  })
  
  
  ### PESTAÑA "CONCLUSIÓN" - SUBPESTAÑA "PREDICCIÓN"
  
  observeEvent(input$BtnPredict, {

    if (model_state()) { ### Si se entrenó el modelo e Red Neuronal

      req(modeloRN(), topVars(), means_scaled(), sds_scaled(), targetColsRV())

      nn_Modelo <- modeloRN()
      targetCols <- targetColsRV()

      # Crear el vector con los valores ingresados por el usuario (mapeados con equivalencias)
      newdata <- sapply(topVars(), function(varname) {
        fila <- equivalencias[equivalencias$infoModelo == varname, ]
        req(nrow(fila) == 1)

        valor_original <- input[[paste0("input_", varname)]]
        if (is.null(valor_original) || valor_original == "") return(NA)

        # Buscar cómo se codificó en dsNumeric()
        col_equiv <- fila$original
        niveles <- unique(datos_poblacion[[col_equiv]])
        codificado <- as.numeric(factor(valor_original, levels = niveles))

        codificado
      })

      newdata <- as.data.frame(t(newdata))
      colnames(newdata) <- topVars()

      # Escalar con medias y sds del entrenamiento
      newdata_scaled <- as.data.frame(
        scale(newdata, center = means_scaled(), scale = sds_scaled())
      )

      # --- Tabla comparativa (original, codificado, escalado) ---
      comparativa <- data.frame(
        Variable   = colnames(newdata),
        Seleccion  = sapply(topVars(), function(var) input[[paste0("input_", var)]]),
        Codificado = as.numeric(newdata[1, ]),
        Escalado   = round(as.numeric(newdata_scaled[1, ]), 3)
      )

      output$tabla_comparativa <- renderUI({

        div(
          class = "table-responsive",
          tags$h4(
            style = "color: black; font-size: 1.2em; font-weight: bold;",
            "VARIABLES PREDICTORAS"
          ),
          tags$table(
            class = "table table-striped",
            tags$thead(
              tags$tr(
                lapply(colnames(comparativa), function(col) tags$th(col))
              )
            ),
            tags$tbody(
              lapply(1:nrow(comparativa), function(i) {
                tags$tr(
                  lapply(comparativa[i, ], function(cell) tags$td(cell))
                )
              })
            )
          )
        )
      })

      # Predecir con RN
      pred <- compute(nn_Modelo, newdata_scaled)$net.result
      pred_class <- which.max(pred)

      # Probabilidades
      probas <- round(pred[1, ] * 100, 2)

      # Diccionario de equivalencias
      equivalenciaTarget <- c(
        "1" = "MALO",
        "target1" = "MALO",
        "targetMAL" = "MALO",
        "2" = "REGULAR",
        "target2" = "REGULAR",
        "targetREG" = "REGULAR",
        "3" = "BUENO",
        "target3" = "BUENO",
        "targetBUE" = "BUENO",
        "4" = "MUY BUENO",
        "target4" = "MUY BUENO",
        "targetMBU" = "MUY BUENO",
        "5" = "EXCELENTE",
        "target5" = "EXCELENTE",
        "targetEXC" = "EXCELENTE",
        "6 " = "NO ESPECIFICADO",
        "target6" = "NO ESPECIFICADO",
        "targetNES" = "NO ESPECIFICADO"
      )

      # Clase predicha en texto
      clase_predicha_num <- targetCols[pred_class]   # el número (ej: "3")
      clase_predicha <- equivalenciaTarget[clase_predicha_num]  # la etiqueta (ej: "Buena")


      output$prediction <- renderUI({

        # Renombrar probas con las etiquetas
        names(probas) <- equivalenciaTarget[targetCols]

        # Preparar los datos para la tabla de probabilidades
        probabilidades_df <- data.frame(
          Clase = names(probas),
          Probabilidad = paste0(probas, "%")
        )

        # --- Pasos para ordenar las filas ---
        # 1. Definir el orden deseado
        orden_deseado <- c("MALO", "REGULAR", "BUENO", "MUY BUENO", "EXCELENTE", "NO ESPECIFICADO")

        # 2. Convertir la columna 'Clase' en un factor con el orden deseado
        probabilidades_df$Clase <- factor(probabilidades_df$Clase, levels = orden_deseado)

        # 3. Ordenar el data.frame por la columna 'Clase'
        probabilidades_df <- probabilidades_df[order(probabilidades_df$Clase), ]
        # --- Fin de los pasos para ordenar ---

        tagList(
          p(HTML("<b>¿CÓMO CALIFICARÍA SU LUGAR DE TRABAJO DESDE UNA PERSPECTIVA INCLUSIVA?</b>"),
            style = "font-size: 1.5em; color: black; font-weight: bold;"),
          div(
            style = "color: black; font-size: 2em; font-weight: bold; margin-bottom: 20px;",
            HTML(paste0("👉 ", clase_predicha))
          )
          # ,
          # hr(),
          # p(HTML("<b>PROBABILIDAD DE CLASES</b>"),
          #   style = "font-size: 1.2em; color: black; font-weight: bold;"),
          # div(
          #   class = "table-responsive",
          #   tags$table(
          #     class = "table table-striped table-bordered",
          #     tags$thead(
          #       tags$tr(
          #         tags$th("Clase"),
          #         tags$th("Probabilidad (%)")
          #       )
          #     ),
          #     tags$tbody(
          #       apply(probabilidades_df, 1, function(row) {
          #         tags$tr(
          #           tags$td(row["Clase"]),
          #           tags$td(row["Probabilidad"])
          #         )
          #       })
          #     )
          #   )
          # ) #div prediccion Prob
        )
      })

    } else {

      output$prediction <- renderUI({
        div(
          style = "margin: 20px; color: black; font-size: 1.2em;",
          HTML("<strong>Debe entrenarse la red neuronal para realizar la predicción.</strong><br><br>
              Haga clic en el botón <b>Entrenar Red Neuronal</b>.")
        )
      })

      output$tabla_comparativa <- renderUI({ NULL })

    }

  })

    
  
 ### BOTONES DE AYUDA EN EL PANEL DEMOGRAFÍA
  
  observeEvent(input$btn_dem1, {
    showModal(modalDialog(
        title = HTML("<b style='color: black;'>Encuestados por Provincia</b>"),
        HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas
            según provincias, correspondientes a la/s provincia/s 
            y a los grupos demográficos seleccionados
            (género, edad y educación).</p>
            <p>La participación de los encuestados fue
            voluntaria y el formulario se distribuyó por todo el país
            vía redes sociales.</p>
        "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_dem2, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Encuestados por Géneros</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
            según géneros, correspondientes a la/s provincia/s 
            y a los grupos demográficos seleccionados
            (género, edad y educación).</p>
            <p>La participación de los encuestados fue
            voluntaria y el formulario se distribuyó por todo el país
            vía redes sociales.</p>
        "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_dem3, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Encuestados por Grupos Etarios</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
            según grupos etarios, correspondientes a la/s provincia/s 
            y a los grupos demográficos seleccionados
            (género, edad y educación).</p>
            <p>La participación de los encuestados fue
            voluntaria y el formulario se distribuyó por todo el país
            vía redes sociales.</p>
        "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_dem4, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Encuestados por Nivel Educativo</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
            según el nivel de educación alcanzado,correspondientes a la/s provincia/s 
            y a los grupos demográficos seleccionados
            (género, edad y educación).</p>
            <p>La participación de los encuestados fue
            voluntaria y el formulario se distribuyó por todo el país
            vía redes sociales.</p>
        "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  
  
  ### BOTONES DE AYUDA EN EL PANEL TRABAJO
  
  observeEvent(input$btn_work1, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Con Empleo</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
            que poseían un empleo al momento de completar el formulario,
            correspondientes a la/s provincia/s 
            y a los grupos demográficos seleccionados
            (género, edad y educación).</p>
            <p>La participación de los encuestados fue voluntaria y
            el formulario se distribuyó por todo el país vía redes sociales.</p>
        "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_work2, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Sector Laboral</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              según el sector laboral donde se desempeñaban al momento
              de completar el formulario, correspondientes a la/s provincia/s 
              y a los grupos demográficos seleccionados
              (género, edad y educación).</p>
              <p>Los sectores laborales son:</p>
              <p><b>Públ:</b> Empleado en el Sector Público.</p>
              <p><b>Priv:</b> Empleado en el Sector Privado.</p>
              <p><b>Autó:</b> Trabajador Autónomo.</p>
              <p><b>ONG:</b>  Miembro de una ONG.</p>
              <p><b>No A:</b> No Aplica, por no trabajar.</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })

  observeEvent(input$btn_work3, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Tipo de Empleo</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              según el tipo de empleo donde se desempeñaban al momento
              de completar el formulario, correspondientes a la/s provincia/s 
              y a los grupos demográficos seleccionados
              (género, edad y educación).</p>
              <p>Los tipos de empleo son:</p>
              <p><b>Free:</b> Trabajador Freelance.</p>
              <p><b>Part:</b> Trabajador Part Time.</p>
              <p><b>Full:</b> Trabajador Full Time.</p>
              <p><b>No A:</b> No Aplica, por no trabajar.</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_work4, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Cantidad de Empleos en los Últimos Cinco Años</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              según la cantidad de trabajos que desempeñaron en los últimos
              cinco años, correspondientes a la/s provincia/s 
              y a los grupos demográficos seleccionados
              (género, edad y educación).</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_work5, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Ayuda de ONGs u otros organismos</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              que recibieron o no apoyo de ONGs u otros organismos
              para su inserción en el mundo laboral, 
              correspondientes a la/s provincia/s 
              y a los grupos demográficos seleccionados
              (género, edad y educación).</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })

    observeEvent(input$btn_work6, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Dificultades en el Mundo del Trabajo</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              según el tipo de barreras o dificultades encontradas para el acceso o
              la permanencia en el lugar de trabajo, surgidas de su limitación física, 
              correspondientes a la/s provincia/s y a los grupos demográficos seleccionados
              (género, edad y educación).</p>
              <p>Las dificultades son:</p>
              <p><b>Comp:</b> Falta de comprensión por parte de los compañeros.</p>
              <p><b>Disc:</b> Actitudes discriminatorias.</p>
              <p><b>Tecn:</b> Falta de adaptaciones tecnológicas.</p>
              <p><b>Otra:</b> Otras dificultades sufridas.</p>
              <p><b>No A:</b> No Aplica, por no trabajar.</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  
    
  ### BOTONES DE AYUDA EN EL PANEL INCLUSION
    
  observeEvent(input$btn_inc1, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Experiencia Inclusiva</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              según su apreciación personal sobre la experiencia laboral desde
              una perspectiva inclusiva (desde mala hasta excelente), 
              correspondientes a la/s provincia/s y a los grupos demográficos seleccionados
              (género, edad y educación).</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_inc2, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Adaptación a la Hipoacusia en el Ambiente Laboral</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              que consideran haber recibido o no algún tipo de adaptación o
              apoyo específico en su vida laboral debido a su hipoacusia, 
              correspondientes a la/s provincia/s y a los grupos demográficos seleccionados
              (género, edad y educación).</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_inc3, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Tipo de Adaptación a la Hipoacusia</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              según el tipo de adaptación más útil recibida en su trabajo,
              correspondientes a la/s provincia/s y a los grupos demográficos seleccionados
              (género, edad y educación).</p>
              <p>Las adaptaciones son:</p>
              <p><b>Inté:</b> Intérprete de lenguaje de señas.</p>
              <p><b>Soft:</b> Software de reconocimiento de voz.</p>
              <p><b>Subt:</b> Subtítulos en reuniones.</p>
              <p><b>No A:</b> No Aplica, por no trabajar.</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_inc4, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Satisfacción por la Adaptación a la Hipoacusia en el Trabajo</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              según el grado de satisfacción por las adaptaciones recibidas
              en los trabajos anteriores o actual (desde ninguna hasta muy alta),
              correspondientes a la/s provincia/s 
              y a los grupos demográficos seleccionados (género, edad y educación).</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })

  
  
  ### BOTONES DE AYUDA EN EL PANEL TECNOLOGÍA
  
  observeEvent(input$btn_tec1, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Empleo de Tecnología para la Hipoacusia</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              que utilizan o han utilizado alguna tecnología de apoyo para la hipoacusia, 
              correspondientes a la/s provincia/s y a los grupos demográficos seleccionados
              (género, edad y educación).</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_tec2, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Tipo de Tecnología Utilizada</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              según la tecnología empleada para la hipoacusia, 
              correspondientes a la/s provincia/s y a los grupos demográficos seleccionados
              (género, edad y educación).</p>
              <p>Las tecnologías son:</p>
              <p><b>Audí:</b> IAudífonos.</p>
              <p><b>Impl:</b> Implantes cocleares.</p>
              <p><b>Apli:</b> Aplicaciones de transcripción.</p>
              <p><b>Soft:</b> Software de reconocimiento de voz/señas.</p>              
              <p><b>Disp:</b> La tecnología disponible que se entregue o facilite.</p>
              <p><b>No A:</b> No Aplica, por no trabajar.</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_tec3, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Tecnologías que podrían ser beneficiosas</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              según la opinión sobre cuáles tecnologías podrían resultar más adecuadas
              para superar la barrera de la hipoacusia en el trabajo,
              correspondientes a la/s provincia/s y a los grupos demográficos seleccionados
              (género, edad y educación).</p>
              <p>Las tecnologías son:</p>
              <p><b>Cap.IA:</b> Capacitación en uso de tecnologías de IA.</p>
              <p><b>Aplic:</b> Aplicaciones de comunicación específicas.</p>
              <p><b>Soft.Rec:</b> Mejora en software de reconocimiento de voz/señas.</p>
              <p><b>IA:</b> Implementación de inteligencia artificial para accesibilidad.</p>
              <p><b>Otras:</b> Otras tecnologías.</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_tec4, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Nivel de Conocimiento sobre la IA</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              según el nivel de conocimiento que manifiestan poseer sobre Inteligencia Artificial,
              correspondientes a la/s provincia/s 
              y a los grupos demográficos seleccionados (género, edad y educación).</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  
  
  ### BOTONES DE AYUDA EN EL PANEL INTELIGENCIA ARTIFICIAL
  
  observeEvent(input$btn_ia1, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>La IA mejorará la Experiencia Laboral</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              según su opinion con respecto a la posibilidad que representa la IA
              para mejorar la experiencia laboral, 
              correspondientes a la/s provincia/s y a los grupos demográficos seleccionados
              (género, edad y educación).</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_ia2, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Importancia de la Capacitación en IA</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              según la importancia que le asignan a la formación en herramientas de IA, 
              correspondientes a la/s provincia/s y a los grupos demográficos seleccionados
              (género, edad y educación).</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_ia3, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Prioridad de aplicar la IA en el Trabajo</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              según la opinión sobre el nivel de prioridad de aplicar la IA en el trabajo,
              correspondientes a la/s provincia/s y a los grupos demográficos seleccionados
              (género, edad y educación).</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  observeEvent(input$btn_ia4, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Importancia de la Privacidad de los Datos en la IA</b>"),
      HTML("<p>Se presenta la cantidad de personas hipoacúsicas encuestadas,
              según su opinión con respecto a respetar la privacidad de los datos en el ámbito de la IA,
              correspondientes a la/s provincia/s 
              y a los grupos demográficos seleccionados (género, edad y educación).</p>
              <p>La participación de los encuestados fue voluntaria y
              el formulario se distribuyó por todo el país vía redes sociales.</p>
          "), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })  
  
  
  
  ### BOTÓN DE AYUDA EN EL GRUPO DE BOTONES PARA GRÁFICOS
  
  observeEvent(input$btn_help, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Personalización de Gráficos</b>"),
        HTML(
          paste0("<p>", icon("chart-simple"), " - Se presentan los datos en un gráfico de columnas.</p>"),
          paste0("<p>", icon("chart-bar"),    " - Se presentan los datos en un gráfico de barras horizontales.</p>"),
          paste0("<p>", icon("chart-pie"),    " - Se presentan los datos en un gráfico circular.</p>"),
          paste0("<p>", icon("circle"),       " - Se presentan los datos en un gráfico de anillo.</p>"),
          paste0("<p>", icon("chart-line"),   " - Se presentan los datos en gráficos diferentes.</p>"),
          paste0("<p>", icon("sort"),         " - Se modifica el ordenamiento de las etiquetas alternado entre tres formas:
                 1. de mayor a menor frecuencia; 2. de menor a mayor frecuencia;
                 3. manteniendo el orden original (sin ordenamiento).
                 Este ordenamiento no se aplica en el caso de gráficos circulares y de anillos.</p>"),
          paste0("<p>", icon("palette"),      " - Se alterna entre diferentes paletas de colores: desierto, océano, montaña, bosque, pradera, urbano día, urbano nocturno, circo multicolor, rojo carnaval, pastel romántico</p>"),
          paste0("<p>", icon("paint-brush"),  " - Se establecen paletas aleatorias para los gráficos.</p>")
          ), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  
  
  
  ### BOTÓN DE AYUDA EN EL MAPA INTERACTIVO
  observeEvent(input$btn_ma1, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Provincias de origen de los participantes en la investigación</b>"),
      HTML("<p>En el mapa interactivo se muestran las provincias 
           a las que pertenecen las personas hipoacúsicas que participaron
           voluntariamente de la investigación, mediante encuestas
           difundidas por las redes sociales.</p>
           <p>Al seleccionar una provincia se presentan en los paneles de la solapa \"Investigación\"
           los gráficos que describen la realidad de las personas hipoacúsicas participantes 
           provenientes de esa provincia.</p>
           <p>Nota: para observar el área correspondiente a CABA (indicada por la chincheta)
           es necesario ampliar el zoom del mapa.</p>"
      ), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) }) 
  
  
  ### BOTÓN DE AYUDA EN NUBE DE PALABRAS
  observeEvent(input$btn_nu2, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Nube de Palabras Significativas</b>"),
      HTML("<p>Se presentan en una nube de palabas aquellas que resultaron más frecuentes
          en las narrativas del grupo de personas hipoacúsicas que formaron
          parte del estudio,  referidas a las barreras o dificutades que afrontan 
          diariamente en su lugar de trabajo.</p>",
          paste0("<p>", icon("cloud"),   " - inicia/detiene la generación automática de las nubes de palabras.</p>")
          ), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })  
  
  
  ### BOTÓN DE AYUDA EN ANÁLISIS DE SENTIMIENTOS
  observeEvent(input$btn_se1, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Análisis de sentimientos</b>"),
      HTML("<p>Se presentan las frecuencias de los sentimientos mejor detectados
           en los textos correspodientes a las narrativas del grupo de personas
           hipoacúsicas que formaron parte del estudio,  referidas a las barreras 
           o dificutades que afrontan diariamente en su lugar de trabajo.</p>",
      ), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })  
  
  
  ### BOTÓN DE AYUDA EN DIAGRAMA SANKEY
  observeEvent(input$btn_sa1, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Diagramde de Flujo Sankey</b>"),
      HTML("<p>El diagrama de Sankey muestra el flujo de los elementos o 
           cantidades de un estado a otro. Sirve en este caso para visualizar 
           flujos de datos en el análisis de las narrativas de las personas hipoacúsicas. 
           Se muestra cómo se distribuyen los sentimientos (positivo, negativo, mixto o neutro) 
           y cómo se relacionan con los temas detectados en las narrativas 
           (familia, trabajo, tecnología, educación y otros).</p>
           <p>El diagrama se puede modificar arrastrando las barras de colores
           correspondientes a los estados.</p>",
      ), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })  
  
  
  ### BOTÓN DE AYUDA EN CONCLUSIÓN - VARIABLES IMPORTANTES
  observeEvent(input$btn_bo1, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>VARIABLES IMPORTANTES</b>"),
      HTML("<p>En este apartado se identifican las variables más significactivas para
            la determinación de la variable objetivo: <b>Experiencia Laboral de las Personas 
            Hipoacúsicas desde una Perspectiva Inclusiva</b>. El modelo aplicado para obtener
            la importancia de las variables es el de <i>Bosque Aleatorio</i>.</p>
            <p>Para este cálculo deben definirse dos parámetros:</p>
            <p><b>Conjunto de Entrenamiento</b>: es el tamaño del conjunto de entrenamiento del modelo
            como un porcentaje de todos los registros de la encuesta. El porcentaje restante
            corresponde al conjunto de prueba con el que se evalúa el ajuste del modelo.</p>
            <p><b>Árboles de Decisión</b>: es la cantidad de árboles de decisión en el bosque
            aleatorio. Una mayor cantidad de árboles puede mejorar el ajuste del modelo,
            pero incrementa los tiempos de entrenamiento. Valores altos
            no significan necesariamente un mejor modelo.</p>",
      ), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  
  ### BOTÓN DE AYUDA EN CONCLUSIÓN - RED NEURONAL
  observeEvent(input$btn_rn1, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>RED NEURONAL</b>"),
      HTML("<p>En este apartado se entrena el modelo de <i>Red Neuronal</i> para la predicción de la 
            variable objetivo: <b>Experiencia Laboral de las Personas Hipoacúsicas desde una
            Perspectiva Inclusiva</b>.</p>
            <p>Para este cálculo deben definirse dos parámetros:</p>
            <p><b>Cantidad de Variables</b>: es el número de variables predictoras que
            deben considerarse en la red neuronal. Las variables se toman del ordenamiento
            obtenido en el punto anterior, comenzando por la de mayor importancia.</p>
            <p><b>Capas Ocultas</b>: es el número de capas que debe contener la red neuronal,
            además de las capas de entrada y de salida. A mayor número de capas, mayor es la 
            <i>profundidad</i> de la red; el ajuste puede ser mejor pero los tiempos de entrenamiento
            pueden incrementarse exponencialmente. <i>Nota: dependiendo de las características
            del computador, el navegador puede llegar a bloquearse con un número alto de capas.</i></p>",
      ), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })

  
  ### BOTÓN DE AYUDA EN CONCLUSIÓN - VARIABLES IMPORTANTES: TABLA
  observeEvent(input$btn_bo2, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Importancia de las Variables Predictoras</b>"),
      HTML("<p>Se muestran en la tabla, ordenadas de mayor a menor importancia, las variables predictoras
           en base a su influencia sobre el valor final de la variable objetivo:
           <b>Experiencia Laboral de las Personas Hipoacúsicas desde una Perspectiva Inclusiva</b>. 
           Se indica su importancia global y también para cada uno de los seis niveles de la variable objetivo:
           MAL - mala; REG - regular; BUE - buena; MBU - muy buena; EXC - excelente; NES; no especificado.</p>
           <p>La importancia de una variable predictora es una medida de cuánto contribuye a la precisión
           de un modelo de predicción. Un valor elevado de importancia indica que la variable es crucial 
           para la precisión del modelo, ya que su alteración provoca una degradación significativa 
           en el rendimiento predictivo.</p>",
      ), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })
  
  
  ### BOTÓN DE AYUDA EN CONCLUSIÓN - VARIABLES IMPORTANTES: GRÁFICO
  observeEvent(input$btn_bo3, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Importancia de las Variables Predictoras</b>"),
      HTML("<p>Se muestran en el gráfico, ordenadas de mayor a menor importancia, las variables predictoras
           en base a su significancia sobre el valor final de la variable objetivo:
           <b>Experiencia Laboral de las Personas Hipoacúsicas desde una Perspectiva Inclusiva</b>.</p>
           <p>La importancia de una variable predictora es una medida de cuánto contribuye a la precisión
           de un modelo de predicción.</p>",
      ), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })  
  
  
  ### BOTÓN DE AYUDA EN CONCLUSIÓN - MODELO DE RED NEURONAL: INFORMACIÓN
  observeEvent(input$btn_rn2, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Información del Modelo</b>"),
      HTML("<p>Se presentan las variables consideradas en el entrenamiento de la red neuronal
           y las capas definidas para el modelo. La precisión en el conjunto de prueba indica
           el porcentaje de aciertos, es decir, la cantidad de veces que el modelo predijo
           correctamente los valores de la variable objetivo a partir de las variables predictoras,
           en el conjunto de prueba.</p>",
      ), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) })  
  
  
  ### BOTÓN DE AYUDA EN CONCLUSIÓN - MODELO DE RED NEURONAL: DATOS ESCALADOS
  observeEvent(input$btn_rn3, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Datos Escalados</b>"),
      HTML("<p>En la tabla se muestran las variables predictoras empleadas en el modelo de
           red neuronal con sus valores escalados, es decir, tranformados a una escala uniforme
           que permita compararlas en igualdad de condiciones, sin que se produzca un sesgo 
           hacia las que poseen valores originales mucho más altos o más bajos que el resto.</p>
           <p>Los niveles de la variable objetivo: <b>Experiencia Laboral de las Personas Hipoacúsicas
           desde una Perspectiva Inclusiva</b> están codificados en formato <i>one hot</i> o de 
           <i>variables dummy</i>. Es un método que permite convertir variables categóricas, 
           como los niveles de la variable objetivo (MAL, REG, BUE, MBU, EXC y NES), en un formato numérico
           que los modelos pueden procesar. Por cada categoría, se crea una nueva columna;
           a las observaciones que pertenecen a esa categoría se les asigna un 1 en la nueva columna,
           mientras que las demás categorías reciben un 0. Esto le permite al modelo entender los niveles
           sin asumir un orden jerárquico que no existe.</p>
           <p>Nota: MAL - Malo, REG - Regular, BUE - Bueno, MBU - Muy Bueno, EXC - Excelente, NES - No Especificado.</p>"
      ), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) }) 
  
  ### BOTÓN DE AYUDA EN CONCLUSIÓN - PREDICCIÓN
  observeEvent(input$btn_rn4, {
    showModal(modalDialog(
      title = HTML("<b style='color: black;'>Predicción con el modelo de Red Neuronal</b>"),
      HTML("<p>Se realiza la predicción del valor que tendría la variable objetivo: 
           <b>Experiencia Laboral de las Personas Hipoacúsicas desde una Perspectiva Inclusiva</b>
           en base a los valores que se ingresan para cada una de las variables predictoras.</b>
           <p>La respuesta para calificar el lugar de trabajo en base a la experiencia laboral de las
           personas hipoacúsicas desde una perspectiva inclusiva puede ser: MALO, REGULAR, BUENO,
           MUY BUENO, EXCELENTE o NO ESPECIFICADO (cuando no se disponen de datos suficientes).</p>
           <p>Se presenta además la tabla de clases con la probabilidad de todas las respuestas y la tabla
           con los valores descriptivos, numéricos originales y escalados de las variables predictoras consideradas.</p>"
      ), footer = modalButton("Cerrar"), easyClose = TRUE, size = "m", class = "custom-modal-bg" ) ) }) 
  
  
  
  ### CÁLCULO DE REGISTROS Y PORCENTAJES POR PROVINCIA
  conteo_provincias <- reactive({
    datos_poblacion_modificado %>%
      count(Provincia, sort = TRUE) %>%
      mutate(Porcentaje = (n / sum(n)) * 100)
  })
  
  buscar_provincia <- function(df, provincia_nombre) {
    df %>% 
      filter(Provincia == provincia_nombre) %>% 
      as.list()
  }
  
  ### PANEL INICIO - SUBPANEL MUESTRA - CAMPOS DE REGISTROS Y PORCENTAJES
  # Renderizar los datos para Buenos Aires
  output$bsas_personas <- renderText({
    prov_data <- buscar_provincia(conteo_provincias(), "Buenos Aires")
    if (length(prov_data$n) > 0) {
      prov_data$n
    } else {
      "0"
    }
  })
  output$bsas_porcentaje <- renderText({
    prov_data <- buscar_provincia(conteo_provincias(), "Buenos Aires")
    if (length(prov_data$Porcentaje) > 0) {
      paste0(round(prov_data$Porcentaje, 2), "%")
    } else {
      "0.00%"
    }
  })
  
  # Renderizar los datos para CABA
  output$caba_personas <- renderText({
    prov_data <- buscar_provincia(conteo_provincias(), "CABA")
    if (length(prov_data$n) > 0) {
      prov_data$n
    } else {
      "0"
    }
  })
  output$caba_porcentaje <- renderText({
    prov_data <- buscar_provincia(conteo_provincias(), "CABA")
    if (length(prov_data$Porcentaje) > 0) {
      paste0(round(prov_data$Porcentaje, 2), "%")
    } else {
      "0.00%"
    }
  })
  
  # Renderizar los datos para Chubut
  output$chubut_personas <- renderText({
    prov_data <- buscar_provincia(conteo_provincias(), "Chubut")
    if (length(prov_data$n) > 0) {
      prov_data$n
    } else {
      "0"
    }
  })
  output$chubut_porcentaje <- renderText({
    prov_data <- buscar_provincia(conteo_provincias(), "Chubut")
    if (length(prov_data$Porcentaje) > 0) {
      paste0(round(prov_data$Porcentaje, 2), "%")
    } else {
      "0.00%"
    }
  })
  
  # Renderizar los datos para Córdoba
  output$cordoba_personas <- renderText({
    prov_data <- buscar_provincia(conteo_provincias(), "Córdoba")
    if (length(prov_data$n) > 0) {
      prov_data$n
    } else {
      "0"
    }
  })
  output$cordoba_porcentaje <- renderText({
    prov_data <- buscar_provincia(conteo_provincias(), "Córdoba")
    if (length(prov_data$Porcentaje) > 0) {
      paste0(round(prov_data$Porcentaje, 2), "%")
    } else {
      "0.00%"
    }
  })
  
  # Renderizar los datos para Río Negro  
  output$rionegro_personas <- renderText({
    prov_data <- buscar_provincia(conteo_provincias(), "Río Negro")
    if (length(prov_data$n) > 0) {
      prov_data$n
    } else {
      "0"
    }
  })
  output$rionegro_porcentaje <- renderText({
    prov_data <- buscar_provincia(conteo_provincias(), "Río Negro")
    if (length(prov_data$Porcentaje) > 0) {
      paste0(round(prov_data$Porcentaje, 2), "%")
    } else {
      "0.00%"
    }
  })
  
  # Renderizar los datos para Tucumán
  output$tucuman_personas <- renderText({
    prov_data <- buscar_provincia(conteo_provincias(), "Tucumán")
    if (length(prov_data$n) > 0) {
      prov_data$n
    } else {
      "0"
    }
  })
  output$tucuman_porcentaje <- renderText({
    prov_data <- buscar_provincia(conteo_provincias(), "Tucumán")
    if (length(prov_data$Porcentaje) > 0) {
      paste0(round(prov_data$Porcentaje, 2), "%")
    } else {
      "0.00%"
    }
  })
  
} # Fin del server