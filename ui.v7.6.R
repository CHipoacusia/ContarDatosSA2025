# --- Funciones modificadas ---

### Función para crear y colocar un botón de acción
crear_btn <- function(btn_id, icon_name = "question", tooltip_text) {
  div(
    style = "position: absolute; top: 2px; right: 2px;
             z-index: 1000; background-color: rgba(255, 255, 255, 0); padding: 5px; border: 0px solid #ccc;
             border-radius: 0px; display: block;",
    actionButton(btn_id, NULL, icon = icon(icon_name, class = "fa-fw"),
                 style = "width: 25px; height: 25px; margin-bottom: 2px; box-shadow: 1px 1px 3px rgba(0,0,0,0.2); padding: 0px;"
    ) %>% bs_embed_tooltip(title = tooltip_text, placement = "left", data_class = "tooltip_style", delay = 0)
  ) # div
} # crear_btn

crear_btn_2 <- function(btn_id, icon_name = "question", tooltip_text) {
  div(
    style = "position: absolute; top: -7px; right: 2px;
             z-index: 1000; background-color: rgba(255, 255, 255, 0); padding: 5px; border: 0px solid #ccc;
             border-radius: 0px; display: block;",
    actionButton(btn_id, NULL, icon = icon(icon_name, class = "fa-fw"),
                 style = "width: 25px; height: 25px; margin-bottom: 2px; box-shadow: 1px 1px 3px rgba(0,0,0,0.2); padding: 0px;"
    ) %>% bs_embed_tooltip(title = tooltip_text, placement = "left", data_class = "tooltip_style", delay = 0)
  ) # div
} # crear_btn


### Función para crear y colocar un conjunto vertical de dos botones
### para la nube de palabras
crear_btn_nube <- function() {
  div(
    style = "position: absolute; top: 2px; right: 2px;
             z-index: 1000; background-color: rgba(255, 255, 255, 0);
             padding: 5px; border: 0px solid #ccc; border-radius: 0px;
             display: flex; flex-direction: column;",
    actionButton("btn_nu1", NULL, icon = icon("cloud", class = "fa-fw"),
                 style = "width: 25px; height: 25px; margin-bottom: 2px;
               box-shadow: 1px 1px 3px rgba(0,0,0,0.2); padding: 0px;") %>%
      bs_embed_tooltip(title = "Regenerar Nube", placement = "left", data_class = "tooltip_style", delay = 0),
    actionButton("btn_nu2", NULL, icon = icon("question", class = "fa-fw"),
                 style = "width: 25px; height: 25px; margin-bottom: 2px;
               box-shadow: 1px 1px 3px rgba(0,0,0,0.2); padding: 0px;") %>%
      bs_embed_tooltip(title = "Ayuda", placement = "left", data_class = "tooltip_style", delay = 0),
  ) # div
} # crear_btn_nube

### Función para crear y colocar un conjunto vertical de ocho botones
### para personalización de gráficos
crear_btn_graf <- function() {
  div(
    style = "position: absolute; top: 2px; right: -40px;
             z-index: 1000; background-color: rgba(255, 255, 255, 0);
             padding: 5px; border: 0px solid #ccc; border-radius: 0px;
             display: flex; flex-direction: column;",
    actionButton("btn_cols", NULL, icon = icon("chart-simple", class = "fa-fw"),
                 style = "width: 25px; height: 25px; margin-bottom: 2px;
               box-shadow: 1px 1px 3px rgba(0,0,0,0.2); padding: 0px;") %>%
      bs_embed_tooltip(title = "Gráfico de Columnas", placement = "left", data_class = "tooltip_style", delay = 0),
    actionButton("btn_bars", NULL, icon = icon("chart-bar", class = "fa-fw"),
                 style = "width: 25px; height: 25px; margin-bottom: 2px;
               box-shadow: 1px 1px 3px rgba(0,0,0,0.2); padding: 0px;") %>%
      bs_embed_tooltip(title = "Gráfico de Barras", placement = "left", data_class = "tooltip_style", delay = 0),
    actionButton("btn_circ", NULL, icon = icon("chart-pie", class = "fa-fw"),
                 style = "width: 25px; height: 25px; margin-bottom: 2px;
               box-shadow: 1px 1px 3px rgba(0,0,0,0.2); padding: 0px;") %>%
      bs_embed_tooltip(title = "Gráfico Circular", placement = "left", data_class = "tooltip_style", delay = 0),
    actionButton("btn_ring", NULL, icon = icon("circle", class = "fa-fw"),
                 style = "width: 25px; height: 25px; margin-bottom: 2px;
               box-shadow: 1px 1px 3px rgba(0,0,0,0.2); padding: 0px;") %>%
      bs_embed_tooltip(title = "Gráfico de Anillo", placement = "left", data_class = "tooltip_style", delay = 0),
    actionButton("btn_graphs", NULL, icon = icon("chart-line", class = "fa-fw"),
                 style = "width: 25px; height: 25px; margin-bottom: 2px;
               box-shadow: 1px 1px 3px rgba(0,0,0,0.2); padding: 0px;") %>%
      bs_embed_tooltip(title = "Gráficos Diferentes", placement = "left", data_class = "tooltip_style", delay = 0),
    actionButton("btn_order", NULL, icon = icon("sort", class = "fa-fw"),
                 style = "width: 25px; height: 25px; margin-bottom: 2px;
               box-shadow: 1px 1px 3px rgba(0,0,0,0.2); padding: 0px;") %>%
      bs_embed_tooltip(title = "Orden de Etiquetas", placement = "left", data_class = "tooltip_style", delay = 0),
    actionButton("btn_pale", NULL, icon = icon("palette", class = "fa-fw"),
                 style = "width: 25px; height: 25px; margin-bottom: 2px;
               box-shadow: 1px 1px 3px rgba(0,0,0,0.2); padding: 0px;") %>%
      bs_embed_tooltip(title = "Cambiar paleta", placement = "left", data_class = "tooltip_style", delay = 0),
    actionButton("btn_pal_aleat", NULL, icon = icon("paint-brush", class = "fa-fw"),
                 style = "width: 25px; height: 25px; margin-bottom: 2px;
               box-shadow: 1px 1px 3px rgba(0,0,0,0.2); padding: 0px;") %>%
      bs_embed_tooltip(title = "Paleta aleatoria", placement = "left", data_class = "tooltip_style", delay = 0),
    actionButton("btn_help", NULL, icon = icon("question", class = "fa-fw"),
                 style = "width: 25px; height: 25px; margin-bottom: 2px;
               box-shadow: 1px 1px 3px rgba(0,0,0,0.2); padding: 0px;") %>%
      bs_embed_tooltip(title = "Ayuda", placement = "left", data_class = "tooltip_style", delay = 0)
  ) # div
} # crear_btn_graf

# --- Interfaz de usuario (UI) ---
ui <- fluidPage(
  useShinyjs(), 
  navbarPage(
    id = "main_nav",
    title = "Argentina Inclusiva",
    theme = shinytheme("cerulean"),
    header = tagList(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      ) # tags$head
    ), # tagList
    # === PESTAÑA 0: INICIO
    tabPanel(
      title = "INICIO",
      icon = icon("home"),
      value = "inicio",
      fluidRow(class = "left-container",
        style = "background-image: url('fondo_port.png');
                 background-size: cover;
                 background-position: center;
                 display: flex;
                 height: 90vh;
                 align-items: top;
                 border-radius: 20px;",
        column(
          width = 7,
          # offset = 0,
          tags$div(
            style = "background-color: rgba(0, 0, 140, 0.5);
                     padding: 3%; border-radius: 20px;",
            h4(tags$img(src = "banderaARG.PNG", style = "height: 60px; margin-right:10px;"),
               strong("ARGENTINA INCLUSIVA"),
               style = "color: #FFFFFF; font-size: 2.5vw; font-weight: bold;"),
            br(),
            h2(HTML("ROMPIENDO BARRERAS AUDITIVAS EN EL TRABAJO"),
               style = "font-size: 4vw; font-weight: bold; color: #FFFFFF;"),
            br(),
            h1(HTML("Tesis Doctoral del Mgte. Julio Paredes sobre las dificultades
                    que se les presentan a las personas hipoacúsicas en su entorno laboral."),
               style = "font-size: 2vw; font-weight: bold; color: #FFFFFF;")
          ) # tags$div
        ) # column
      ) # fluidRow
    ), # tabPanel INICIO
    # === PESTAÑA 1: ANÁLISIS
    tabPanel(
      title = "ANÁLISIS",
      icon = icon("search"),
      value = "Analisis",
      sidebarLayout(
        div(style = "position: relative;",
            sidebarPanel(class = "left-container", width = 4, style = "height: 90vh",
                         h4(HTML("<b>Provincias de origen de las personas hipoacúsicas encuestadas.</b>"),
                            style = "line-height: 1.2; color: black; margin-bottom: 15px;", class = "subtitle-container"),
                         div(class = "chart-container", style = "height: 71vh; position: relative;",
                             leafletOutput("mapa", height = "100%"),
                             crear_btn("btn_ma1", "question", "Ayuda")
                         ) # div chart-container
            ) # sidebarPanel
        ), # div
        
        
        
        mainPanel(class = "left-container", style = "height: 90vh; padding-top: 5px;",
          width = 8,
          h4(HTML("Investigación doctoral del Mgte. Julio Paredes sobre las dificultades que las personas
                   hipoacúsicas deben superar en el mundo del trabajo.
                   La indagación se fundamentó en datos brindados por voluntarios de Buenos Aires,
                   CABA, Chubut, Córdoba, Río Negro y Tucumán."),
             style = "line-height: 1.2; color: black;", class = "text-container"),
          tabsetPanel(
            id = "analisis_tabs",
            type = "pills",
            tabPanel(value = "Muestra", "Muestra", class = "right-container", style = "height: 63vh;", # 390px",
                     icon = icon("list-alt"),
                     # div(class = "right-container",
                         tags$head(
                           tags$script("
                                        $(document).on('mouseenter', '.prov-container', function() {
                                          var prov = $(this).attr('data-provincia');
                                          Shiny.setInputValue('provincia_card_hover', prov, {priority: 'event'});
                                        });
                                        $(document).on('mouseleave', '.prov-container', function() {
                                          Shiny.setInputValue('provincia_card_hover', null, {priority: 'event'});
                                        });
                                      ")
                         ),
                         fluidRow(
                           column(4,
                                  div(class = "prov-container",  `data-provincia`= "Buenos Aires",
                                      p("BUENOS AIRES", style = "color: black; margin-top: 0px; margin-bottom: 3px;
                                                    font-weight: bold; font-size: 1.6em;"),
                                      div(style = "display: flex; align-items: center; gap: 20px;",
                                          div(style = "font-size: 2.5em; flex-shrink: 0;", icon("users")),
                                          div(
                                            p("Participantes:", style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                    font-weight: bold; font-size: 1.4em;"),
                                            p(span(textOutput("bsas_personas", inline = TRUE),
                                                   style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                        font-weight: bold; font-size: 1.4em;")),
                                            p(span(textOutput("bsas_porcentaje", inline = TRUE),
                                                   style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                        font-weight: bold; font-size: 1.4em;"))
                                          ) # div
                                      ) # div style
                                  ) # div prov-container
                           ), # column BUENOS AIRES
                           column(4,
                                  div(class = "prov-container", `data-provincia`= "CABA",
                                      p("CABA", style = "color: black; margin-top: 0px; margin-bottom: 3px;
                                                    font-weight: bold; font-size: 1.6em;"),
                                      div(style = "display: flex; align-items: center; gap: 20px;",
                                          div(style = "font-size: 2.5em; flex-shrink: 0;", icon("users")),
                                          div(
                                            p("Participantes:", style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                    font-weight: bold; font-size: 1.4em;"),
                                            p(span(textOutput("caba_personas", inline = TRUE),
                                                   style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                        font-weight: bold; font-size: 1.4em;")),
                                            p(span(textOutput("caba_porcentaje", inline = TRUE),
                                                   style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                        font-weight: bold; font-size: 1.4em;"))
                                          ) # div
                                      ) # div style
                                  ) # div prov-container
                           ), # column CABA
                           column(4,
                                  div(class = "prov-container", `data-provincia`=  "Chubut",
                                      p("CHUBUT", style = "color: black; margin-top: 0px; margin-bottom: 3px;
                                                    font-weight: bold; font-size: 1.6em;"),
                                      div(style = "display: flex; align-items: center; gap: 20px;",
                                          div(style = "font-size: 2.5em; flex-shrink: 0;", icon("users")),
                                          div(
                                            p("Participantes:", style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                    font-weight: bold; font-size: 1.4em;"),
                                            p(span(textOutput("chubut_personas", inline = TRUE),
                                                   style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                        font-weight: bold; font-size: 1.4em;")),
                                            p(span(textOutput("chubut_porcentaje", inline = TRUE),
                                                   style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                        font-weight: bold; font-size: 1.4em;"))
                                          ) # div
                                      ) # div style
                                  ) # div prov-container
                           ) # column CHUBUT
                         ), # fluidRow
                         fluidRow(
                           column(4, style = "margin-top: 10px;", 
                                  div(class = "prov-container", `data-provincia`=  "Córdoba",
                                      p("CÓRDOBA", style = "color: black; margin-top: 0px; margin-bottom: 3px;
                                                    font-weight: bold; font-size: 1.6em;"),
                                      div(style = "display: flex; align-items: center; gap: 20px;",
                                          div(style = "font-size: 2.5em; flex-shrink: 0;", icon("users")),
                                          div(
                                            p("Participantes:", style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                    font-weight: bold; font-size: 1.4em;"),
                                            p(span(textOutput("cordoba_personas", inline = TRUE),
                                                   style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                        font-weight: bold; font-size: 1.4em;")),
                                            p(span(textOutput("cordoba_porcentaje", inline = TRUE),
                                                   style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                        font-weight: bold; font-size: 1.4em;"))
                                          ) # div
                                      ) # div style
                                  ) # div prov-container
                           ), # column CÓRDOBA
                           column(4, style = "margin-top: 10px;", 
                                  div(class = "prov-container", `data-provincia`=  "Río Negro",
                                      p("RÍO NEGRO", style = "color: black; margin-top: 0px; margin-bottom: 3px;
                                                    font-weight: bold; font-size: 1.6em;"),
                                      div(style = "display: flex; align-items: center; gap: 20px;",
                                          div(style = "font-size: 2.5em; flex-shrink: 0;", icon("users")),
                                          div(
                                            p("Participantes:", style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                    font-weight: bold; font-size: 1.4em;"),
                                            p(span(textOutput("rionegro_personas", inline = TRUE),
                                                   style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                        font-weight: bold; font-size: 1.4em;")),
                                            p(span(textOutput("rionegro_porcentaje", inline = TRUE),
                                                   style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                        font-weight: bold; font-size: 1.4em;"))
                                          ) # div
                                      ) # div style
                                  ) # div prov-container
                           ), # column RÍO NEGRO
                           column(4, style = "margin-top: 10px;", 
                                  div(class = "prov-container", `data-provincia`= "Tucumán",
                                      p("TUCUMÁN", style = "color: black; margin-top: 0px; margin-bottom: 3px;
                                                    font-weight: bold; font-size: 1.6em;"),
                                      div(style = "display: flex; align-items: center; gap: 20px;",
                                          div(style = "font-size: 2.5em; flex-shrink: 0;", icon("users")),
                                          div(
                                            p("Participantes:", style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                    font-weight: bold; font-size: 1.4em;"),
                                            p(span(textOutput("tucuman_personas", inline = TRUE),
                                                   style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                        font-weight: bold; font-size: 1.4em;")),
                                            p(span(textOutput("tucuman_porcentaje", inline = TRUE),
                                                   style = "color: black; margin-top: 0px; margin-bottom: 0px;
                                                        font-weight: bold; font-size: 1.4em;"))
                                          ) # div
                                      ) # div style
                                  ) # div prov-container
                           ) # column TUCUMÁN
                         ) # fluidRow
                     # ) # div "right-container"
            ), # tabPanel MUESTRA
            
            tabPanel(value = "Nube", "Nube de Palabras", 
                     icon = icon("cloud"),
                     div(style = "position: relative;", 
                         div(class = "right-container", style = "height: 63vh;",
                             wordcloud2Output("nube", width = "100%", height = "100%"),
                             # div(style = "position: absolute; top: 0px; right: 0px;",
                             crear_btn_nube()
                         )
                     )
            ), # tabPanel NUBE
            
            tabPanel(value = "Sentimientos", "Análisis de Sentimientos",
                     icon = icon("heart"),
                     div(style = "position: relative;",
                         div(class = "right-container",  
                             style = "height: 63vh;
                               border-radius: 20px;",
                             #"background-image: url('fondo_graf.png'); 
                              # background-size: cover;
                              # background-position: center;
                             # display: flex;
                             # align-items: top;
                             plotlyOutput("sentimientos", width = "100%", height = "100%")),
                         crear_btn("btn_se1", "question", "Ayuda")
                     ) # div style
            ), # tabPanel SENTIMIENTOS
            
            tabPanel(value = "Sankey", "Diagrama de Sankey",
                     icon = icon("share-alt"),
                     div(style = "position: relative;",
                         div(class = "right-container",
                             style = "height: 63vh;
                               border-radius: 20px;",
                             #"background-image: url('fondo_graf.png');
                             #  background-size: cover;
                              # background-position: center;
                               # display: flex;
                              # align-items: top;
                             # Leyenda Sankey
                           #  tags$div(
                            #   style = "display:flex; gap:10px; margin-bottom:10px;",
                           #    tags$div(style="display:flex; align-items:center; gap:5px;",
                            #            tags$div(style="width:15px; height:15px; background-color:orange;"), "Neutro → Narrativas sin carga emocional clara."),
                            #   tags$div(style="display:flex; align-items:center; gap:5px;",
                            #            tags$div(style="width:15px; height:15px; background-color:red;"), "Positivo → Narrativas con connotación favorable."),
                            #   tags$div(style="display:flex; align-items:center; gap:5px;",
                            #            tags$div(style="width:15px; height:15px; background-color:green;"), "Negativo → Narrativas con connotación desfavorable."),
                            #   tags$div(style="display:flex; align-items:center; gap:5px;",
                            #            tags$div(style="width:15px; height:15px; background-color:purple;"), "Mixto → Narrativas con percepciones encontradas.")
                           #  ),
                             plotlyOutput("sankey", width = "100%", height = "100%")),
                         crear_btn("btn_sa1", "question", "Ayuda")
                         
                     ) # div style
            ) # tabPanel SANKEY
          ) # tabsetPanel ANALISIS
        ) # mainPanel
      ) # sidebarLayout
    ), # tabPanel ANÁLISIS
    
    
    # === PESTAÑA 2: INVESTIGACIÓN
    tabPanel(
      title = "INVESTIGACIÓN",
      icon = icon("clipboard"),
      value = "Investigacion",
      sidebarLayout(
        sidebarPanel(
          class = "filter-container", style = "height: 90vh;",
          width = 2,
          fluidRow(column(7, h5("Provincia", class = "title-2")), column(5, actionButton("prov_todos", "Todas", class="btn-xs"))),
          checkboxGroupInput("filtro_prov", NULL, choices = opciones_prov_1, selected = opciones_prov_1),
          br(),
          fluidRow(column(7, h5("Género", class = "title-1")), column(5, actionButton("gen_todos", "Todos", class="btn-xs"))),
          checkboxGroupInput("filtro_gen", NULL, choices = opciones_gen_1, selected = opciones_gen_1),
          br(),
          fluidRow(column(7, h5("Grupo Etario", class = "title-1")), column(5, actionButton("edad_todos", "Todos", class="btn-xs"))),
          checkboxGroupInput("filtro_edad", NULL, choices = opciones_edad_1, selected = opciones_edad_1),
          br(),
          fluidRow(column(7, h5("Educación", class = "title-1")), column(5, actionButton("edu_todos", "Todos", class="btn-xs"))),
          checkboxGroupInput("filtro_edu", NULL, choices = opciones_edu_1, selected = opciones_edu_1)
        ), # sidebarPanel FILTROS
        mainPanel(
          width = 9,
          tabsetPanel(id = "panel_poblacion",
                      type = "pills",

                      # tabPanel DEMOGRAFÍA
                      tabPanel(id = "panel_demografia",
                               "Demografía",
                               value = "Demografia",
                               icon = icon("users"),
                               br(),
                               div(style = "position: relative;",
                                   crear_btn_graf()
                               ), # div style
                               fluidRow(
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_dem1")),
                                               crear_btn("btn_dem1", "question", "Ayuda")
                                 )), # column
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_dem2")),
                                               crear_btn("btn_dem2", "question", "Ayuda")
                                 )) # column
                               ), # fluidRow
                               fluidRow(
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_dem3")),
                                               crear_btn("btn_dem3", "question", "Ayuda")
                                 )), # column
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_dem4")),
                                               crear_btn("btn_dem4", "question", "Ayuda")
                                 )) # column
                               ) # fluidRow
                      ), # tabPanel DEMOGRAFÍA
                      
                      # tabPanel TRABAJO
                      tabPanel(id = "panel_trabajo",
                               "Trabajo",
                               icon = icon("briefcase"),
                               value = "Trabajo",
                               width = 9,
                               br(),
                               div(style = "position: relative;",
                                   crear_btn_graf()
                               ), # div style
                               fluidRow(
                                 column(4, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_work1")),
                                               crear_btn("btn_work1", "question", "Ayuda")
                                 )), # column
                                 column(4, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_work2")),
                                               crear_btn("btn_work2", "question", "Ayuda")
                                 )), # column
                                 column(4, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_work3")),
                                               crear_btn("btn_work3", "question", "Ayuda")
                                 )) # column
                               ), # fluidRow
                               fluidRow(
                                 column(4, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_work4")),
                                               crear_btn("btn_work4", "question", "Ayuda")
                                 )), # column
                                 column(4, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_work5")),
                                               crear_btn("btn_work5", "question", "Ayuda")
                                 )), # column
                                 column(4, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_work6")),
                                               crear_btn("btn_work6", "question", "Ayuda")
                                 )) # column
                               ) # fluidRow
                      ), # tabPanel TRABAJO
                      
                      # tabPanel INCLUSION
                      tabPanel(id = "panel_inclusion",
                               "Inclusión",
                               icon = icon("handshake-angle"),
                               value = "Inclusion",
                               width = 9,
                               br(),
                               div(style = "position: relative;",
                                   crear_btn_graf()
                               ), # div style
                               fluidRow(
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_inc1")),
                                               crear_btn("btn_inc1", "question", "Ayuda")
                                 )), # column
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_inc2")),
                                               crear_btn("btn_inc2", "question", "Ayuda")
                                 )) # column
                               ), # fluidRow
                               fluidRow(
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_inc3")),
                                               crear_btn("btn_inc3", "question", "Ayuda")
                                 )), # column
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_inc4")),
                                               crear_btn("btn_inc4", "question", "Ayuda")
                                 )) # column
                               ) # fluidRow
                      ), # tabPanel INCLUSION
                      
                      # tabPanel TECNOLOGÍA
                      tabPanel(id = "panel_tecnologia",
                               "Tecnologia",
                               icon = icon("microchip"),
                               value = "Tecnologia",
                               width = 9,
                               br(),
                               div(style = "position: relative;",
                                   crear_btn_graf()
                               ), # div style
                               fluidRow(
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_tec1")),
                                               crear_btn("btn_tec1", "question", "Ayuda")
                                 )), # column
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_tec2")),
                                               crear_btn("btn_tec2", "question", "Ayuda")
                                 )) # column
                               ), # fluidRow
                               fluidRow(
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_tec3")),
                                               crear_btn("btn_tec3", "question", "Ayuda")
                                 )), # column
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_tec4")),
                                               crear_btn("btn_tec4", "question", "Ayuda")
                                 )) # column
                               ) # fluidRow
                      ), # tabPanel TECNOLOGÍA
                      
                      # tabPanel INTELIGENCIA ARTIFICIAL
                      tabPanel(id = "panel_ia",
                               "IA",
                               icon = icon("robot"),
                               value = "IA",
                               width = 9,
                               br(),
                               div(style = "position: relative;",
                                   crear_btn_graf()
                               ), # div style
                               fluidRow(
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_ia1")),
                                               crear_btn("btn_ia1", "question", "Ayuda")
                                 )), # column
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_ia2")),
                                               crear_btn("btn_ia2", "question", "Ayuda")
                                 )) # column
                               ), # fluidRow
                               fluidRow(
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_ia3")),
                                               crear_btn("btn_ia3", "question", "Ayuda")
                                 )), # column
                                 column(6, div(style = "position: relative;",
                                               div(class="chart-container", plotlyOutput("plot_ia4")),
                                               crear_btn("btn_ia4", "question", "Ayuda")
                                 )) # column
                               ) # fluidRow
                      ), # tabPanel INTELIGENCIA ARTIFICIAL
                      
                      # tabPanel TABLA
                      tabPanel(id = "panel_tabla",
                               "Tabla",
                               value = "Tabla",
                               icon = icon("table"),
                               br(),
                               div(class="chart-container-table",
                                   DTOutput("tabla_poblacion")
                               ) # div
                      ) # tabPanel TABLA
          ) # tabsetPanel
        ) # mainPanel
      ) # sidebarLayout
    ), # tabPanel INVESTIGACIÓN
    
    
    # === PESTAÑA 3: CONCLUSIÓN
    tabPanel(
      title = "CONCLUSIÓN",
      icon = icon("flag-checkered"),
      value = "conclusion",
      
          sidebarLayout(
            sidebarPanel(width = 3, class = "filter-container", style = "height: 90vh; margin-top: 0px;",
                          div(class = "text-container", style = "font-size: 0.8em; color: black; margin-top: 5px;",
                            fluidRow(style = "margin-top: 3px;",
                              column(10, div(style = "font-size: 1.2em; color: black;",
                                              strong(HTML("VARIABLES IMPORTANTES")))),
                              column(2, crear_btn_2("btn_bo1", "question", "Ayuda")),
                            ),
                            div(numericInput("split", "Conjunto de Entrenamiento", 70, min = 50, step = 5, max = 90),
                                style = "margin-top: 15px;"),
                            div(numericInput("ntree", "Árboles de Decisión", 500, min = 100, step = 50, max = 2000),
                                style = "margin-top: 3px;"),
                          ), # div
                         
                          div(class = "text-container", style = "font-size: 0.8em; color: black; margin-top: 20px;",
                            fluidRow(style = "margin-top: 3px;",
                              column(10, div(style = "font-size: 1.2em; color: black;",
                                              strong(HTML("RED NEURONAL")))),
                              column(2, crear_btn_2("btn_rn1", "question", "Ayuda")),
                            ),
                            div(sliderInput("cantVar", "Cantidad de Variables", 5, min = 2, max = 24),
                                style = "margin-top: 15px;"),
                            div(sliderInput("capa", "Capas Ocultas", 3, min = 1, max = 10),
                                style = "margin-top: 3px;"),
                            actionButton("BtnTrain", "Entrenar Red Neuronal", style = "width:100%; margin-top: 5px;")
                          ), # div
                         
            ), # sidebarPanel
           
            mainPanel(width = 9,
                    tabsetPanel(
                      id = "vars_red_neuronal",
                      type = "pills",
                      
                      
                      tabPanel("Variables Importantes",
                        id = "vars_signif",
                        icon = icon("cogs"),
                        div(style = "margin-top: 10px;",
                        
                        tabsetPanel(
                            id = "vars_significativas",
                            type = "pills",

                            tabPanel("Gráfico",
                                   id = "grafico_variables",
                                   icon = icon("line-chart"),
                                   div(style = "position: relative; ",
                                      div(class="chart-container", style = "height: 74vh; margin-top: 9px;",
                                         plotOutput("grafico_importancia", height = "71vh"),
                                         crear_btn("btn_bo3", "question", "Ayuda")
                                      )
                                   )
                            ), # tabPanel GRAFICO

                            tabPanel("Tabla",
                                     id = "tabla_variables",
                                     icon = icon("table"),
                                     div(style = "position: relative; margin-top: 9px;",
                                         div(class="chart-container",
                                             style = "height: 74vh; margin-top: 9px;",
                                             div(style = "height: 70vh; overflow-y: scroll; ",
                                                 tableOutput("tabla_importancia"),
                                                 crear_btn("btn_bo2", "question", "Ayuda")
                                             )
                                         )
                                     )
                            ) # tabPanel TABLA
                            
                          ), # tabsetPanel VARS_SIGNIFICATIVAS
                        ), # div
                      ), # tabPanel VARIABLES IMPORTANTES
                      
                      tabPanel("Modelo de Red Neuronal",
                        id = "red_neuronal",
                        icon = icon("brain"),
                        div(style = "margin-top: 10px;",
                                   
                        tabsetPanel(
                         id = "mod_red_neuronal",
                         type = "pills",
                         
                         tabPanel("Información",
                            id = "informacion",
                            icon = icon("list-alt"),
                            div(style = "position: relative; margin-top: 9px;",
                                div(class="chart-container",
                                    style = "height: 74vh; margin-top: 9px;",
                                    htmlOutput("info_modelo"),
                                    crear_btn("btn_rn2", "question", "Ayuda")
                                )
                            )
                         ), # tabPanel INFORMACIÓN
                         
                         tabPanel("Datos Escalados",
                            id = "datos_escalados",
                            icon = icon("table"),
                            div(style = "position: relative; ",
                              div(class="chart-container", style = "height: 74vh; margin-top: 9px;",
                                  uiOutput("tabla_datos_escalados"),
                                  crear_btn("btn_rn3", "question", "Ayuda")
                              )
                            )                              
                          ), # tabPanel DATOS
                        ), # tabsetPanel MOD_RED_NEURONAL
                        ), # div
                      ), # tabPanel MODELO DE RED NEURONAL
                    
                      
                      tabPanel("Predicción",
                         id = "prediccion",
                         icon = icon("magic"),
                         wellPanel(
                           
                           #style = paste0("background-color: white", "!important; height: 82vh; margin-top: 10px;"),
                           class = "chart-container",
                           style = "height: 82vh; margin-top: 10px;",
                           column(4, 
                              class = "text-container", 
                              style = "margin-right: 10px;",
                              h4(strong("VARIABLES PREDICTORAS"), style = "color: black; font-size: 1.0em; 
                                          font-weight: bold; margin-top: 5px;"),
                              wellPanel(
                                style = "height: 59vh; overflow-y: scroll; background-color: white; 
                                          font-size: 0.8em; font-weight: normal !important;",
                                uiOutput("dynamicInputs"),
                              ),
                              actionButton("BtnPredict", "Predecir Variable Objetivo", style = "width: 100%")
                            ), # column
                           
                            column(7, class = "chart-container", style = "overflow-y: scroll; height: 78vh;",
                               uiOutput("prediction"),
                               uiOutput("tabla_comparativa"),
                               crear_btn("btn_rn4", "question", "Ayuda"),
                            ), # column
                         ) # wellPanel
                         
                      ), # tabPanel PREDICCIÓN
                      
                    ), # tabsetPanel VARS RED NEURONAL
                    
           ) # mainPanel
         ) # sidebarLayout
                 
    ), # tabPanel CONCLUSIÓN
    
    tabPanel("REFLEXIÓN FINAL",
       id = "reflexion",
       icon = icon("lightbulb"),
       fluidRow(class = "left-container",
                style = "background-image: url('fondo_ref.png');
                 background-size: cover;
                 background-position: center;
                 display: flex;
                 height: 90vh;
                 align-items: top;
                 border-radius: 20px;",
                column(
                  width = 7,
                  # offset = 0,
                  tags$div(
                    style = "background-color: rgba(0, 0, 140, 0.5);
                     padding: 3%; border-radius: 20px;",
                     h3(HTML("<b>Los resultados, recogidos desde la voz de la comunidad hipoacúsica,
                        confirman que la inteligencia artificial, aplicada con enfoque inclusivo,
                        reduce barreras y mejora la inserción laboral.
                        Sin embargo, su impacto pleno depende de políticas sostenidas,
                        financiamiento y cultura organizacional inclusiva.
                        La participación activa de las personas con hipoacusia profunda
                        en el diseño de estas tecnologías garantiza mayor equidad y transforma
                        la empleabilidad de manera sostenible.</b>"),
                      style = "line-height: 1.3; font-size: 2.0em; color: white;
                       margin-top: 5px; margin-bottom: 5px; height: 70vh;"),
                  ) # tags$div
                ) # column
       ) # fluidRow
       
       
       
       
       
    ), # tabPanel REFLEXION
    
    
    footer = tags$footer(
      p("Datos de Paredes, Julio; © 2024 - Diseño de la presentación: Moretti, Yael; Ferrara, Marco Aurelio y Perera, Jorge.",
        br(), "Resolución recomendada para el navegador: 1920 x 1200 px",
        style = "font-family: Arial; font-size: 12px; text-align: center; margin-top: 10px")
    ) # tags$footer
    
  ) # navbarPage
  
) # fluidPage

