# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# LOADING LIBS ------------------------------------------------------------

if(!require("pacman")) install.packages("pacman")
p_load(shiny, shinydashboard, shinyjs, shinyWidgets, leaflet,leaflet.extras, data.table, here, reticulate, httr,plotly)
#install.packages("here", "reticulate")

# DICCIONARIOS PARA LOS FILTROS -------------------------------------------

DT <- data.table(
  ANDALUCÍA = c('ALMERÍA', 'CÁDIZ', 'CÓRDOBA', 'GRANADA', 'HUELVA', 'JAÉN', 'MÁLAGA', 'SEVILLA', ''),
  ARAGÓN = c('HUESCA', 'TERUEL', 'ZARAGOZA', '', '', '', '', '', ''),
  ASTURIAS = c('ASTURIAS','', '', '', '', '', '', '', ''),
  ISLAS_BALEARES = c('BALEARS (ILLES)','', '', '', '', '', '', '', ''),
  CANARIAS = c('PALMAS (LAS)', 'SANTA CRUZ DE TENERIFE','', '', '', '', '', '', ''),
  CANTABRIA = c('CANTABRIA','', '', '', '', '', '', '', ''),
  CASTILLA_Y_LEÓN = c('ÁVILA', 'BURGOS', 'LEÓN', 'PALENCIA', 'SALAMANCA', 'SEGOVIA', 'SORIA', 'VALLADOLID', 'ZAMORA'),
  CASTILLA_LA_MANCHA = c('ALBACETE', 'CIUDAD REAL', 'CUENCA', 'GUADALAJARA', 'TOLEDO', '', '', '', ''),
  CATALUÑA = c('BARCELONA', 'GIRONA', 'LLEIDA', 'TARRAGONA','', '', '', '', ''),
  COMUNIDAD_VALENCIANA = c('ALICANTE', 'CASTELLÓN / CASTELLÓ', 'VALENCIA / VALÈNCIA','', '', '', '', '', ''),
  EXTREMADURA = c('BADAJOZ', 'CÁCERES','', '', '', '', '', '', ''),
  GALICIA = c('CORUÑA (A)', 'LUGO', 'OURENSE', 'PONTEVEDRA', '', '', '', '', ''),
  COMUNIDAD_DE_MADRID = c('MADRID','', '', '', '', '', '', '', ''),
  MURCIA = c('MURCIA','', '', '', '', '', '', '', ''),
  NAVARRA = c('NAVARRA','', '', '', '', '', '', '', ''),
  PAÍS_VASCO = c('ARABA/ÁLAVA', 'BIZKAIA', 'GIPUZKOA', '', '', '', '', '', ''),
  LA_RIOJA = c('RIOJA (LA)','', '', '', '', '', '', '', ''),
  CEUTA = c('CEUTA','', '', '', '', '', '', '', ''),
  MELILLA = c('MELILLA','', '', '', '', '', '', '', '')
)
combustible <- data.table(
  precio_gasoleo_a = c(''),
  precio_gasoleo_premium = c(''),
  precio_gasolina_95_e5 = c(''),
  precio_gasolina_98_e5 = c('')
)

# COMIENZA SHINY UI -------------------------------------------

ui = dashboardPage(
  skin = 'blue',
  
  # CABECERA ----------------------------------------------------------------
  
  dashboardHeader(
    title = shiny::span(img(src="https://cdn-icons-png.flaticon.com/512/8391/8391498.png", width = 25, height = 25), "GasoLite"),
    # titleWidth = 300,
    tags$li(class = "dropdown",
            tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; color: #fff;"),
            tags$li(class = "dropdown", actionLink("login", textOutput("logintext"))))
  ),
  

  # BARRA LATERAL -----------------------------------------------------------

  dashboardSidebar(
    useShinyjs(),
    
    #Filtro CCAA
    column(12,
           fluidRow(
             selectizeInput(
               inputId = "CCAA",
               label = "Escoja la CCAA",
               choices = names(DT),
               selected = 1
               )
             )
    ),
    #Filtro provincia
    column(12,
           fluidRow(
             selectizeInput(
               inputId = "PROVINCIA",
               label = "Escoja la provincia",
               choices = DT[['ANDALUCÍA']],
               selected = 2
             )
           )
    ),
    #Filtro carburante
    column(12,
           fluidRow(
             selectInput ('tipogasoleo',
                          label = 'Seleccione un carburante',
                          choices = names(combustible),
                          selected = 'precio_gasoleo_a'
             )
           )
    ),
    #Filtro precio
    column(12, 
           fluidRow(
             sliderInput('precio',
                         label = 'Seleccione el precio máximo del carburante',
                         min = 1.5,
                         max = 3,
                         value = 1.9,
                         round = FALSE,
                         step = 0.1
             )
             # uiOutput("slider")
           )
    ),
    
    #Filtro lowcost
    column(12,
           fluidRow(
             radioButtons('lowcost',
                          label = '¿Quiere una gasolinera LowCost?',
                          choices = list(
                            'Si' = 1,
                            'No' = 2,
                            'Indiferente' = 3
                          ),
                          selected = 3
             )
           )
    ),
    #Filtro 24h
    column(12,
           fluidRow(
             radioButtons('si_24_h',
                          label = '¿Quiere una gasolinera 24H?',
                          choices = list(
                            'Si' = 1,
                            'No' = 2,
                            'Indiferente' = 3
                          ),
                          selected = 3
             )
           )
    ),
    #Filtro autoservicio
    column(12,
           fluidRow(
             radioButtons('si_autoservicio',
                          label = '¿Quiere una gasolinera autoservicio?',
                          choices = list(
                            'Si' = 1,
                            'No' = 2,
                            'Indiferente' = 3
                          ),
                          selected = 3
             )
           )
    ),
    #Botón
    column(12,
           fluidRow(
             column(
               width = 10,
               actionBttn(
                 'boton', 
                 label = 'Buscar',
                 size = 'md',
                 color = 'primary',
                 style = 'minimal'
               ),
               align = 'center'
             )
           )
           )
    ),
  

# CUERPO ------------------------------------------------------------------

  dashboardBody(

    # PESTAÑA MAPA 1 ----------------------------------------------------------

    #Tabs
    tabsetPanel(id="tabs",
      #Tab principal: Mapa
      tabPanel(
        'Mapa', 
        fluidRow(
          box(width=12,
              title = "Seleccione los filtros deseados y pulse 'Buscar' para obtener las gasolineras determinadas",
              status = 'primary',
              #imageOutput("myImage"),
              leafletOutput(outputId = "mymap", height = 600)),
          )
        ),
      
      # PESTAÑA MAPA 2 ----------------------------------------------------------
      
      #Tab mapa específico
      tabPanel(
        'Mapa por ubicación',
        tags$script('
        $(document).ready(function () {
          navigator.geolocation.getCurrentPosition(onSuccess, onError);
                
          function onError (err) {
            Shiny.onInputChange("geolocation", false);
          }
                
          function onSuccess (position) {
            setTimeout(function () {
              var coords = position.coords;
              console.log(coords.latitude + ", " + coords.longitude);
              Shiny.onInputChange("geolocation", true);
              Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude);
            }, 1100)
          }
        });
              '),
        
        fluidRow(
          box(width = 12,
              title = 'Elija la ubicación y el rango máximo de km para visualizar el mapa',
              status = 'primary',
              column(6,
                     textInput(inputId = 'loc', label = NULL , value = "", width = NULL, placeholder = "Escriba una dirección. Ej:'Calle Goya, 88, Madrid' "),
                     checkboxInput("use_location", "Utilizar su ubicación actual"),
                     sliderInput('rango_km',
                                 label = 'Seleccione los kms',
                                 min = 0, 
                                 max = 20, 
                                 value = 5, 
                                 round = FALSE,
                                 step = 1
                     )
                     ),
              
              #Botón
              column(6,
                     fluidRow(
                       column(
                         width = 10,
                         actionBttn(
                           'buscar', 
                           label = 'Buscar',
                           size = 'md',
                           color = 'primary',
                           style = 'jelly'
                         ),
                         align = 'center'
                       )
                     ),
                     p(style="white-space: pre-wrap"),
                     infoBoxOutput("num_gasolineras", width = 12),
              ),
              leafletOutput(outputId = "mymap2", height = 600)
          )
        )
      ),
      
      # PESTAÑA DASHBOARD ----------------------------------------------------------
      
      #Tab Dashboard
      tabPanel(
        'Dashboard', 
        fluidRow(
          box(width = 12, class = "text-left",
              title = 'Dashboard: Gráficos de interés conforme a los filtros seleccionados',
              p('Los gráficos aparecrán una vez realizada la selección y búsqueda'),
              status = 'primary',
              column(width = 12,
                     infoBoxOutput("comunidad", width = 6),
                     infoBoxOutput("provincia", width = 6)
                     ),
              column(width = 12,
                      infoBoxOutput("min_price_mean_infobox",width = 6),
                      infoBoxOutput("max_price_mean_infobox",width = 6)
                      ),
              column(width = 12,
                     plotOutput("low_cost")
                     ),
              column(width = 12,
                     column(2, tableOutput('precio_medio_tabla')),
                     column(10, plotOutput('barPlot'))
                     ),
              column(width = 12,
                     column(9, plotlyOutput("graph")),
                     column(3, valueBoxOutput('valuebox', width = 12)),
                     column(3, valueBoxOutput('valuebox2', width = 12)),
                     
                     )
              
          )
        )
      ),
      
      # PESTAÑA EMPRESARIO ----------------------------------------------------------
      
      #Tab estadísticas
      tabPanel(
        'Empresario', 
        fluidRow(
          box(width = 12,
              title = 'Graficos de interés para el empresario',
              status = 'primary',
              plotlyOutput("histogram"),
              textOutput("text1"),
              p(style="white-space: pre-wrap"),
              downloadButton("descargar", "Reporte")
          )
          )
        ),
      
      # PESTAÑA SUGERENCIAS ----------------------------------------------------------
      
      #Tab de sugerencias
      tabPanel(
        'Sugerencias', 
        fluidRow(
          box(width = 12,
              title = 'Escribanos para saber como podemos mejorar la aplicación',
              status = 'primary',
              textInput("tema", "Tema a tratar:", "",placeholder = "Breve resumen de la sugerencia"),
              textInput("sugerencia", "Sugerencia:", "",placeholder = "Escriba aqui su texto"),
              actionButton("enviar", "Enviar")
          )
        )
      ),
      
      # PESTAÑA LOGIN ----------------------------------------------------------
      
      #Tab de login
      tabPanel(
        'Login', 
        fluidRow(
          box(width = 12,
              title = 'Login',
              status = 'primary',
              
              radioButtons('conexion',
                           label = '¿Qué desea realizar?',
                           choices = list(
                             'Conectarse' = 1,
                             'Registrarse' = 2,
                             'Actualizar' = 3
                           ),
                           selected = 1
              ),
              
              
              #Registro
              conditionalPanel(
                condition = "input.conexion == 2",
                
                textInput("Nemail", "Correo electrónico:", "",placeholder = "ejemplo@correo.es"),
                passwordInput("Npassword", "Contraseña:", "",placeholder = "Mínimo 6 carácteres"),
                textInput("Nnombre", "Nombre de usuario:", "",placeholder = "Nombre Propio/Nick"),
                #CCAA
                selectizeInput(
                  inputId = "user_CCAA",
                  label = "Escoja la CCAA",
                  choices = names(DT),
                  selected = 1
                ),
                #Provincia
                selectizeInput(
                  inputId = "user_PROVINCIA",
                  label = "Escoja la provincia",
                  choices = DT[['ANDALUCÍA']],
                  selected = 2
                ),
                #Carburante
                selectInput ('user_tipogasoleo',
                             label = 'Seleccione un carburante',
                             choices = names(combustible),
                             selected = 'precio_gasoleo_a'
                ),
                
                textInput("dir_fav", "Insertar dirección favorita:", "",placeholder = "Ej: Paseo Ermita del Santo, 40, Madrid"),
                
                actionButton("registerButton", "Registrarse")
                ),
              
              #Conexión
              conditionalPanel(
                condition = "input.conexion == 1",
                textInput("email", "Correo electrónico:", "",placeholder = "ejemplo@correo.es"),
                passwordInput("password", "Contraseña:", "",placeholder = "Mínimo 6 carácteres"),
                actionButton("loginButton", "Conectarse"),
              ),
              #Actualizar
              conditionalPanel(
                condition = "input.conexion == 3",
                
                textInput("Aemail", "Correo electrónico:", "",placeholder = "ejemplo@correo.es"),
                passwordInput("Apassword", "Contraseña:", "",placeholder = "Mínimo 6 carácteres"),
                textInput("Anombre", "Nombre de usuario:", "",placeholder = "Nombre Propio/Nick"),
                #CCAA
                selectizeInput(
                  inputId = "Auser_CCAA",
                  label = "Escoja la CCAA",
                  choices = names(DT),
                  selected = 1
                ),
                #Provincia
                selectizeInput(
                  inputId = "Auser_PROVINCIA",
                  label = "Escoja la provincia",
                  choices = DT[['ANDALUCÍA']],
                  selected = 2
                ),
                #Carburante
                selectInput ('Auser_tipogasoleo',
                             label = 'Seleccione un carburante',
                             choices = names(combustible),
                             selected = 'precio_gasoleo_a'
                ),
                textInput("Adir_fav", "Insertar dirección favorita:", "",placeholder = "Ej: Paseo Ermita del Santo, 40, Madrid"),
                
                actionButton("updateButton", "Actualizar")
              )
        )
      ),
      )
    )
  )
)
