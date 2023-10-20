# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# CARGA DE LIBRERÍAS ------------------------------------------------------------

if(!require("pacman")) install.packages("pacman")
p_load(readxl, shiny, shinydashboard, shinydashboardPlus, shinyjs, shinyWidgets, 
       shinybusy, tidyverse, magrittr, janitor, lubridate, tidyr, httr, jsonlite, 
       leaflet, geosphere, readxl, data.table, geosphere, mapsapi,xml2,mapsapi, httr)


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



# COMIENZO SHINY SERVER ----------------------------------------------------------

shinyServer(function(input, output, session){
  

  # LOG IN Firebase ------------------------------------------------------------------
  
  urlDB <- "https://grupo4-lpe-941e0-default-rtdb.europe-west1.firebasedatabase.app/users/tokenusuario.json"
  urlDB_sug <- "https://grupo4-lpe-941e0-default-rtdb.europe-west1.firebasedatabase.app/sugerencias/numerosug.json"
  user_sug <- "Desconocido"
  
  # CARGA DE DATOS ------------------------------------------------------------
  
  url_='https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/' 
  data <- fromJSON(url_)
  ds_raw <- data$ListaEESSPrecio
  ds_f <- ds_raw %>% clean_names() %>% type_convert(locale = locale(decimal_mark = ",")) %>% as_tibble()
  ds_f %>% count(rotulo) 
  ds_f %>% distinct(rotulo) 
  

  # LIMPIEZA DE DATOS -------------------------------------------------------

  #Eliminamos columnas innecesarias (carburantes poco comunes)
  borrar <- c('precio_biodiesel','precio_bioetanol','precio_gas_natural_comprimido','precio_gas_natural_licuado',
              'precio_gases_licuados_del_petroleo','precio_gasoleo_b','precio_gasolina_95_e10','precio_gasolina_95_e5_premium',
              'precio_gasolina_98_e10','precio_hidrogeno')
  datos <- ds_f[ , !(names(ds_f) %in% borrar)]
  
  #Diferenciamos por lowcost
  no_low_cost <- c('REPSOL','CEPSA', 'GALP','SHELL','BP','PETRONOR','AVIA','Q8', 'CAMPSA','BONAREA')
  ds_low_cost <- datos %>% mutate(low_cost = !rotulo %in% no_low_cost)
  
  #Creamos columna con el nombre de las CCAA
  ds_low_cost2 <- ds_low_cost%>% mutate(ds_low_cost,ccaa = ifelse (idccaa=="01","ANDALUCÍA",ifelse (idccaa=="02","ARAGÓN", ifelse (idccaa=="03","ASTURIAS", ifelse (idccaa=="04","ISLAS_BALEARES", 
                                                           ifelse (idccaa=="05","CANARIAS",ifelse (idccaa=="06","CANTABRIA", ifelse (idccaa=="07","CASTILLA_LA_MANCHA", 
                                                           ifelse (idccaa=="08","CASTILLA_Y_LEÓN", ifelse (idccaa=="09","CATALUÑA", ifelse (idccaa=="10","COMUNIDAD_VALENCIANA",
                                                           ifelse (idccaa=="11","EXTREMADURA", ifelse (idccaa=="12","GALICIA", ifelse (idccaa=="13","COMUNIDAD_DE_MADRID", ifelse (idccaa=="14","MURCIA", ifelse (idccaa=="15","NAVARRA",
                                                           ifelse (idccaa=="16","PAÍS_VASCO", ifelse (idccaa=="17","LA_RIOJA",ifelse (idccaa=="18","CEUTA",ifelse (idccaa=="19","MELILLA","NA"))))))))))))))))))))
  
  #Convertimos los valores nulos en 0
  ds_low_cost2[is.na(ds_low_cost2)] <- 0
  
  #Diferenciamos por 24h
  no_24h <- ds_low_cost2  %>% mutate('si_24H' = horario %in% 'L-D: 24H')
  
  #Diferenciamos por autoservicio
  tipo_servicio <- read_excel("preciosEESS_es.xls", skip = 3)
  columnaserv <- select(tipo_servicio, direccion, Tipo_servicio)
  df_servicio <- merge(x = no_24h, y = columnaserv, by=c("direccion"), all.x = TRUE)
  df_autoserv <- df_servicio %>% mutate(autoservicio = str_detect(Tipo_servicio,pattern = "(A)"))
  
  sample <- sample_n(df_autoserv, 200)
  
  # FRONT ------------------------------------------------------------
  
  output$mymap <- renderLeaflet({
    leaflet(sample) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addMarkers(~longitud_wgs84, ~latitud, 
                 icon = icon('person'),)
  })
  
  #Imponemos las provinicas en base a la CCAA elegida
  observeEvent(input$CCAA, {
    freezeReactiveValue(input, "PROVINCIA")
    updateSelectizeInput(
      session,
      inputId = "PROVINCIA",
      choices = DT[[input$CCAA]],
      selected = 1
    )}, ignoreInit = TRUE)
  
  #Botón cargando
  observeEvent(input$boton, {
    show_modal_spinner(
      spin = 'radar',
      color = 'blue',
      text = 'Cargando...'
    )
    Sys.sleep(2)
    remove_modal_spinner()
    
   
    
    # FILTROS DE DATOS ------------------------------------------------------------
    
    #Filtro provincia
    df_provincia <- filter(df_autoserv,provincia==input$PROVINCIA)
    
    #Filtro de low_cost
    if (input$lowcost==1){
      df_lo <- filter(df_provincia,low_cost == TRUE)
    }else if (input$lowcost==2){
      df_lo <- filter(df_provincia,low_cost == FALSE)
    }else if (input$lowcost==3){
      df_lo <- df_provincia
    }
    
    
    #Filtro de 24_h
    if (input$si_24_h==1){
      df_24 <- filter(df_lo,si_24H == TRUE)
    }else if (input$si_24_h==2){
      df_24 <- filter(df_lo,si_24H == FALSE)
    }else if (input$si_24_h==3){
      df_24 <- df_lo
    }
    
    #Filtro de autoservicio
    if (input$si_autoservicio==1){
      df_auto <- filter(df_24,autoservicio == TRUE)
    }else if (input$si_autoservicio==2){
      df_auto <- filter(df_24,autoservicio == FALSE)
    }else if (input$si_autoservicio==3){
      df_auto <- df_24
    }
    
    #Precio
    
    # print(input$precio)
    # print(min(df_autoserv[input$tipogasoleo]))
    # 
    # if (input$precio < min(df_autoserv[input$tipogasoleo])){
    #   precio <- min(df_autoserv[input$tipogasoleo])
    #   prit('hola')
    # }else if (input$precio >= min(df_autoserv[input$tipogasoleo])){
    #   precio <- input$precio
    #   prit('adios')
    # }
  
    # output$slider <- renderUI({
    #   sliderInput("inSlider", "Slider", min=min(df_auto[input$tipogasoleo]), max=max(df_auto[input$tipogasoleo]), value=mean(df_auto[input$tipogasoleo]))
    # })
    
    # observeEvent(input$tipogasoleo, {
    #   df_precio <- filter(df_autoserv,provincia==input$PROVINCIA)
    #   print(min(df_precio[input$tipogasoleo]))
    #   #freezeReactiveValue(input, "precio")
    #   updateSliderInput(
    #     session, "precio",
    #     value = mean(df_precio[input$tipogasoleo]),
    #     min = min(df_precio[input$tipogasoleo]),
    #     max = max(df_precio[input$tipogasoleo])
    #   )}
    #   )
    
    
    
    observeEvent(input$tipogasoleo, {
      freezeReactiveValue(input, "precio")
      updateSelectizeInput(
        session,
        inputId = "precio",
        value = 200,
        max = 300,
        min = 0
      )}, ignoreInit = TRUE)
    
    #Filtro por precio
    df_precio <- subset(df_auto, df_auto[input$tipogasoleo] < input$precio & df_auto[input$tipogasoleo] > 0 )
    
    tabla_final <- df_precio %>% select(ccaa, provincia,rotulo, municipio, input$tipogasoleo, low_cost, si_24H, horario, autoservicio, latitud, longitud_wgs84, direccion)

    # REPORT ------------------------------------------------------------------

    # output$descargar <- downloadHandler(
    #   filename = function(){"gasolineras.csv"}, 
    #   content = function(fname){
    #     write.csv(df_precio, fname)
    #   }
    # )
    
    output$descargar <- downloadHandler(
      # Para la salida en PDF, usa "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copia el reporte a un directorio temporal antes de porcesarlo, en
        #caso de que no tengamos permiso de escritura en el directorio actual
        #puede ocurrir un error
        tempReport <- file.path(tempdir(), "informe.Rmd")
        file.copy("informe.Rmd", tempReport, overwrite = TRUE)

        # configurar los parametros para pasar al documento .Rmd
        params <- list(n = tabla_final)

        #Copilar el documento con la lista de parametros, de tal manera que se
        #evalue de la misma manera que el entorno de la palicacipon.
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    
    # MAPA 1 ------------------------------------------------------------
    
    tabla_final$low_cost[tabla_final$low_cost == TRUE] <- 'Low_cost'
    tabla_final$low_cost[tabla_final$low_cost == FALSE] <- 'NO_Low_cost'
    
    tabla_final <- mutate(tabla_final, cntnt=paste0('<strong>CCAA: </strong>',ccaa,
                                            '<br><strong>Provincia:</strong> ', provincia,
                                            '<br><strong>Carburante:</strong> ', names(tabla_final[input$tipogasoleo]),
                                            '<br><strong>Precio:</strong> ',input$tipogasoleo)) 
    
    calc_mode <- function(x){
      distinct_values <- unique(x)
      distinct_tabulate <- tabulate(match(x, distinct_values))
      distinct_values[which.max(distinct_tabulate)]
    }
    tabla_final<-tabla_final %>% mutate(autoservicio = if_else(is.na(autoservicio), 
                                                               calc_mode(autoservicio), 
                                                               autoservicio))
    tabla_final<-tabla_final %>% mutate(low_cost = if_else(is.na(low_cost), 
                                                           calc_mode(low_cost), 
                                                           low_cost))
    tabla_final<-tabla_final %>% mutate(si_24H = if_else(is.na(si_24H), 
                                                         calc_mode(si_24H), 
                                                         si_24H))
    
    pal <- colorFactor(pal = c("#2DF34E", "#F3392D"), domain = c("TRUE", "FALSE"))
    pal <- colorFactor(palette = c("blue", "red"),tabla_final$low_cost)
    
    output$mymap <- renderLeaflet({
      leaflet(tabla_final) %>%
        addProviderTiles(providers$OpenStreetMap)%>%
        addCircles(~longitud_wgs84, ~latitud) %>% 
        addCircleMarkers(data = tabla_final, lng = ~longitud_wgs84, lat =~latitud, 
                         radius = 5, popup = ~tabla_final$rotulo, label = ~tabla_final$direccion,
                         color = ~pal(tabla_final$low_cost),
                         stroke = FALSE, fillOpacity = 0.8) %>% 
        addLegend("bottomright", pal = pal, values = ~tabla_final$low_cost,
                  title = "Tipo de gasolinera:",
                  opacity = 1
        )
      
    })
    
    
    # PESTAÑA DASHBOARD ------------------------------------------------------------
    
    #CCAA
    output$comunidad = renderInfoBox({
      valueBox (
        value = input$CCAA,
        subtitle = 'ccaa',
        color = 'blue',
        width = 12,
        icon = icon("earth-americas")
      )
    })
    
    #Provincia
    output$provincia = renderInfoBox({
      valueBox (
        value = input$PROVINCIA,
        subtitle = 'provincia',
        color = 'blue',
        width = 12,
        icon = icon("location-dot")
      )
    })
    
    tabla_final_grouped <- aggregate(tabla_final[,input$tipogasoleo], by = list(tabla_final$municipio),FUN = mean)
    colnames(tabla_final_grouped) <- c("municipio", "precio_medio")
    tabla_final_grouped <- tabla_final_grouped[order(tabla_final_grouped$precio_medio),]
    tabla_final_grouped <- head(tabla_final_grouped, 10)
    min_price_mean <- min(tabla_final_grouped$precio_medio)
    min_price_mean_index <- which.min(tabla_final_grouped$precio_medio)
    min_price_mean_municipio <- tabla_final_grouped$municipio[min_price_mean_index]
    output$min_price_mean_infobox <- renderInfoBox({
      infoBox("Municipio_precio_med_min", paste(min_price_mean_municipio,":",round(min_price_mean, 2)), icon = icon("euro"), color = "aqua")
    })
    max_price_mean <- max(tabla_final_grouped$precio_medio)
    max_price_mean_index <- which.max(tabla_final_grouped$precio_medio)
    max_price_mean_municipio <- tabla_final_grouped$municipio[max_price_mean_index]
    output$max_price_mean_infobox <- renderInfoBox({
      infoBox("Municipio_precio_med_max", paste(max_price_mean_municipio,":",round(max_price_mean, 2)), icon = icon("euro"), color = "aqua")
    })
    
    # GRÁFICOS ----------------------------------------------------------------
  
    tabla_final$low_cost <- as.factor(tabla_final$low_cost)
    
    gas_agg <- aggregate(tabla_final$low_cost, by = list(tabla_final$municipio),FUN = length)
    colnames(gas_agg) <- c("municipio", "low_cost_count")
    
    output$low_cost <- renderPlot({
      ggplot(gas_agg, aes(x=reorder(municipio,-low_cost_count), y=low_cost_count, fill=municipio)) +
        geom_bar(stat = "identity", position="dodge") +
        ggtitle("Gasolineras según los filtros elegidos") +
        xlab("Municipio") +
        ylab("Número de gasolineras") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(legend.position = "none")
    }, height = 400)
    
    output$precio_medio_tabla <- renderTable({
      tabla_final_grouped
    }, width = 0.2)
    
    output$barPlot <- renderPlot({
      ggplot(tabla_final_grouped, aes(x=reorder(municipio,precio_medio), y=precio_medio)) +
        geom_bar(stat = "identity", width = 0.2, fill = "blue") +
        ggtitle("Precio medio de gasoleo según municipio") +
        xlab("Municipio") +
        ylab("Precio medio") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    output$text1 <- renderText({
      "Si desea obtener más información descargue el siguiente reporte : "
    })
   
    output$histogram <- renderPlotly({
      precios <- as.numeric(unlist(tabla_final$precio))
      
      
      fig_h<- plot_ly(x = precios, type = "histogram")
      
      fig_h
    })
    
    datoscomparativa19<- read_csv('datos_19-01_final.csv')
    datoscomparativa12<-read_csv('datos_12-01_final.csv')
    datoscomparativa20<-read_csv('datos_20-01_final.csv')
    datoscomparativa21<-read_csv('datos_21-01_final.csv')
    datoscomparativa22<-read_csv('datos_22-01_final.csv')
    datoscomparativa23<-read_csv('datos_23-01_final.csv')
    
    
    #dia 12 octubre
    datosfinal <- subset(datoscomparativa12, datoscomparativa12[input$tipogasoleo] < input$precio & datoscomparativa12[input$tipogasoleo] > 0 & datoscomparativa12$provincia == input$PROVINCIA)
    #print(unique(datosfinal$municipio))
    datosfinal12_grouped <- aggregate(datosfinal[,input$tipogasoleo], by = list(datosfinal$municipio),FUN = mean)
    
    colnames(datosfinal12_grouped) <- c("municipio", "precio_medio")
    datosfinal12_grouped<-datosfinal12_grouped%>%arrange(desc(precio_medio))
    datosfinal12_grouped <- head(datosfinal12_grouped, 10)
    
    #dia 19 enero
    
    datosfinal2 <- subset(datoscomparativa19, datoscomparativa19[input$tipogasoleo] < input$precio & datoscomparativa19[input$tipogasoleo] > 0 & datoscomparativa19$provincia == input$PROVINCIA)
    #print(unique(datosfinal2$municipio))
    datosfinal2_grouped <- aggregate(datosfinal2[,input$tipogasoleo], by = list(datosfinal2$municipio),FUN = mean)
    
    colnames(datosfinal2_grouped) <- c("municipio", "precio_medio")
    datosfinal2_grouped<-datosfinal2_grouped%>%arrange(desc(precio_medio))
    
    datosfinal2_grouped <- head(datosfinal2_grouped, 10)
    
    #dia 20 enero
    
    datosfinal20 <- subset(datoscomparativa20, datoscomparativa20[input$tipogasoleo] < input$precio & datoscomparativa20[input$tipogasoleo] > 0 & datoscomparativa20$provincia == input$PROVINCIA)
    #print(unique(datosfinal20$municipio))
    datosfinal20_grouped <- aggregate(datosfinal20[,input$tipogasoleo], by = list(datosfinal20$municipio),FUN = mean)
    
    colnames(datosfinal20_grouped) <- c("municipio", "precio_medio")
    datosfinal20_grouped<-datosfinal20_grouped%>%arrange(desc(precio_medio))
    
    datosfinal20_grouped <- head(datosfinal20_grouped, 10)
    
    #dia 21 enero
    
    datosfinal21 <- subset(datoscomparativa21, datoscomparativa21[input$tipogasoleo] < input$precio & datoscomparativa21[input$tipogasoleo] > 0 & datoscomparativa21$provincia == input$PROVINCIA)
    #print(unique(datosfinal21$municipio))
    datosfinal21_grouped <- aggregate(datosfinal21[,input$tipogasoleo], by = list(datosfinal21$municipio),FUN = mean)
    
    colnames(datosfinal21_grouped) <- c("municipio", "precio_medio")
    datosfinal21_grouped<-datosfinal21_grouped%>%arrange(desc(precio_medio))
    
    datosfinal21_grouped <- head(datosfinal21_grouped, 10)
    #dia 22 enero
    
    datosfinal22 <- subset(datoscomparativa22, datoscomparativa22[input$tipogasoleo] < input$precio & datoscomparativa22[input$tipogasoleo] > 0 & datoscomparativa22$provincia == input$PROVINCIA)
    #print(unique(datosfinal21$municipio))
    datosfinal22_grouped <- aggregate(datosfinal22[,input$tipogasoleo], by = list(datosfinal22$municipio),FUN = mean)
    
    colnames(datosfinal22_grouped) <- c("municipio", "precio_medio")
    datosfinal22_grouped<-datosfinal22_grouped%>%arrange(desc(precio_medio))
    
    datosfinal22_grouped <- head(datosfinal22_grouped, 10)
    #dia 23 enero
    
    datosfinal23 <- subset(datoscomparativa23, datoscomparativa23[input$tipogasoleo] < input$precio & datoscomparativa23[input$tipogasoleo] > 0 & datoscomparativa23$provincia == input$PROVINCIA)
    #print(unique(datosfinal21$municipio))
    datosfinal23_grouped <- aggregate(datosfinal23[,input$tipogasoleo], by = list(datosfinal23$municipio),FUN = mean)
    
    colnames(datosfinal23_grouped) <- c("municipio", "precio_medio")
    datosfinal23_grouped<-datosfinal23_grouped%>%arrange(desc(precio_medio))
    
    datosfinal23_grouped <- head(datosfinal23_grouped, 10)
    
    #Order
    
    datosfinal2_grouped$municipio <- trimws(datosfinal2_grouped$municipio)
    datosfinal12_grouped$municipio <- trimws(datosfinal12_grouped$municipio)
    datosfinal20_grouped$municipio <- trimws(datosfinal20_grouped$municipio)
    datosfinal21_grouped$municipio <- trimws(datosfinal21_grouped$municipio)
    datosfinal22_grouped$municipio <- trimws(datosfinal22_grouped$municipio)
    datosfinal23_grouped$municipio <- trimws(datosfinal23_grouped$municipio)
    
    
    datosfinal2_grouped <- datosfinal2_grouped %>% arrange(municipio)
    datosfinal12_grouped <- datosfinal12_grouped %>% arrange(municipio)
    datosfinal20_grouped <- datosfinal20_grouped %>% arrange(municipio)
    datosfinal21_grouped <- datosfinal21_grouped %>% arrange(municipio)
    datosfinal22_grouped <- datosfinal22_grouped %>% arrange(municipio)
    datosfinal23_grouped <- datosfinal23_grouped %>% arrange(municipio)
    
    
    library(gridExtra)
    
    
    library(plotly)
    library(shiny)
    output$graph <- renderPlotly({
      plot_ly() %>%
        add_trace(x = datosfinal12_grouped$municipio, y = datosfinal12_grouped$precio_medio, type = 'scatter', mode = 'lines+markers', name = "12-10") %>%
        add_trace(x = datosfinal2_grouped$municipio, y = datosfinal2_grouped$precio_medio, type = 'scatter', mode = 'lines+markers',name = "19-01") %>%
        add_trace(x = datosfinal20_grouped$municipio, y = datosfinal20_grouped$precio_medio, type = 'scatter', mode = 'lines+markers',name = "20-01") %>%
        add_trace(x = datosfinal21_grouped$municipio, y = datosfinal21_grouped$precio_medio, type = 'scatter', mode = 'lines+markers',name = "21-01")%>%
        add_trace(x = datosfinal22_grouped$municipio, y = datosfinal22_grouped$precio_medio, type = 'scatter', mode = 'lines+markers',name = "22-01")%>%
        add_trace(x = datosfinal23_grouped$municipio, y = datosfinal23_grouped$precio_medio, type = 'scatter', mode = 'lines+markers',name = "23-01")%>%
        layout(title ="Evolución precio gasolina")
    })
    
    
    #porcentaje variación 
    #Para datosfinal2_grouped
    #Para datosfinal2_grouped
    #Para datosfinal12_grouped
    
    
    total_variation <-((mean(datosfinal12_grouped$precio_medio)-(mean(datosfinal23_grouped$precio_medio)))*100)
    total_var <-(total_variation/(datosfinal12_grouped$precio_medio))
    total_vari<-round(total_variation, 2)
    
    output$valuebox <- renderValueBox({
      valueBox(
        paste0(total_vari, "%"), "Variación 12 octubre a 23 enero", icon = icon("euro"),
        color = "blue"
      )
    })
    total_variation2 <-((mean(datosfinal23_grouped$precio_medio)-(mean(datosfinal2_grouped$precio_medio)))*100)
    total_var2 <-(total_variation2/(datosfinal23_grouped$precio_medio))
    total_vari2<-round(total_variation2, 2)
    
    output$valuebox2 <- renderValueBox({
      valueBox(
        paste0(total_vari2, "%"), "Variación 19 enero a 23 enero", icon = icon("euro"),
        color = "blue"
      )
    })
 
    
  # MAPA 2 ------------------------------------------------------------
    
  })
  observeEvent(input$buscar, {
    show_modal_spinner(
      spin = 'semipolar',
      color = 'blue',
      text = 'Cargando...'
    )
    Sys.sleep(2)
    remove_modal_spinner()
    
    key= "AIzaSyBRMg-WgkdJSg30zDdtALjHapL1Z4v06As"
    
    if (input$use_location == TRUE){
      lat <- input$lat
      long <- input$long

      current_location <- c(long, lat)
      long_lat <- df_autoserv %>% select(longitud_wgs84,latitud)
      distancias_cualquiera <- distGeo(current_location, long_lat)
      df_distancias_total <- df_autoserv %>% mutate(distancias = round(distancias_cualquiera/1000, digits = 2))
      df_distancias <- subset(df_distancias_total, df_distancias_total$distancias < input$rango_km)
    }else{
      direccion <- input$loc
      
      res<-mp_geocode(direccion,key=key)
      bar<-mp_get_points(res)%>%select(pnt)
      bar2<-as.numeric(unlist(bar))
      
      distancias_cualquiera<-df_autoserv %>% select(longitud_wgs84,latitud) %>% distGeo(bar2)
      df_distancias_total<-df_autoserv%>%  mutate(distancias = round(distancias_cualquiera/1000, digits = 2))
      df_distancias <- subset(df_distancias_total, df_distancias_total$distancias < input$rango_km)
    }
    
    df_distancias$low_cost[df_distancias$low_cost == TRUE] <- 'Low_cost'
    df_distancias$low_cost[df_distancias$low_cost == FALSE] <- 'NO_Low_cost'
    
    pal <- colorFactor(pal = c("#2DF34E", "#F3392D"), domain = c("TRUE", "FALSE"))
    pal <- colorFactor(palette = c("blue", "red"),df_distancias$low_cost)
    
    output$mymap2 <- renderLeaflet({
      leaflet(df_distancias) %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        #addProviderTiles(providers$CartoDB.Positron)
        addCircles(~longitud_wgs84, ~latitud) %>% 
        addCircleMarkers(data = df_distancias, lng = ~longitud_wgs84, lat =~latitud, 
                         radius = 8, 
                         #popup = ~df_distancias$rotulo,
                         popup = paste0( df_distancias$rotulo 
                                         , "<br>"
                                         , "Distancia: "
                                         , df_distancias$distancias
                                         , "km"
                         ),
                         label = ~df_distancias$direccion,
                         color = ~pal(df_distancias$low_cost),
                         stroke = FALSE, fillOpacity = 0.8) %>% 
        addLegend("bottomright", pal = pal, values = ~df_distancias$low_cost,
                  title = "Tipo de gasolinera:",
                  opacity = 1
        )
        # addProviderTiles(providers$CartoDB.Positron) %>%
        # addMarkers(~longitud_wgs84, ~latitud,
        #            icon = icon('person'),
        #            label = 'hola',
        #            labelOptions = labelOptions(textsize = "12px"),
        #            popup = '~popup_text')
    })
    
    output$num_gasolineras = renderInfoBox({
      valueBox (
        value = nrow(df_distancias),
        subtitle = 'Nº de gasolineras',
        color = 'blue',
        width = NULL,
        icon = icon("gas-pump")
      )
    })
  })
    
  # SUGERENCIAS ----------------------------------------------------------------
  
  observeEvent(input$enviar, {
    tema <- input$tema
    sugerencia <- input$sugerencia
    random_number <- sample(0:9, 10, replace = TRUE)
    random_number <- paste0(random_number, collapse = "")
    urlDB_sug2 <- sub("numerosug", random_number, urlDB_sug)
    resUser<-PUT(urlDB_sug2,
                 body=list(usuario=user_sug,tema=tema,sugerencia=sugerencia), encode = "json")
    showModal(modalDialog(
      title = "Respuesta enviada",
      "Gracias por su colaboración"
    ))
  })
  
  # LOGIN ----------------------------------------------------------------
  
  #Imponemos las provinicas en base a la CCAA elegida
  observeEvent(input$user_CCAA, {
    freezeReactiveValue(input, "user_PROVINCIA")
    updateSelectizeInput(
      session,
      inputId = "user_PROVINCIA",
      choices = DT[[input$user_CCAA]],
      selected = 1
    )}, ignoreInit = TRUE)
  
  observeEvent(input$loginButton, {
    authEmail <- input$email
    authPassword <- input$password
    response <- POST(
      "https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=AIzaSyDZeu_FAc1gl7xwZiOn_TgeCyGwQZbbtpQ",
      body = list(email=authEmail,password=authPassword, returnSecureToken = TRUE), encode = "json")
    status <- http_status(response)$category
    body <- content(response)
    idtoKen <-body$idToken
    localId<-body$localId
    
    if (status == 'Success'){
      urlDBUser <- sub("tokenusuario", localId, urlDB)
      isAuth <- TRUE
      getUserDB<-jsonlite::fromJSON(urlDBUser)
      
      updateTabsetPanel(session, "tabs", selected = "Mapa")
      updateSelectizeInput(session, "CCAA", selected = getUserDB$ccaa)
      updateSelectizeInput(session, "PROVINCIA", selected = getUserDB$provincia)
      updateSelectInput(session, "tipogasoleo", selected = getUserDB$combustible)
      updateTextInput(session, "loc", value = getUserDB$dir)
      
      user_sug <- getUserDB$email
      
      showModal(modalDialog(
        title = "Login completado con éxito",
        "Has sido devuelto a la página inicial"
      ))
      
      
    }else{
      isAuth <- FALSE
      print(message<-body$error$message)
      showModal(modalDialog(
        title = "Error en el login",
        "Por favor, asegúrese de que los datos introducidos son correctos"
      ))
      
    }  
  })
  observeEvent(input$registerButton, {
    Nemail <- input$Nemail
    Npassword <- input$Npassword
    Nnombre <- input$Nnombre
    ccaa_reg<- input$user_CCAA
    provincia_reg<- input$user_PROVINCIA
    combustible_reg <- input$user_tipogasoleo
    dir_fav <- input$dir_fav
    response <-
      POST("https://identitytoolkit.googleapis.com/v1/accounts:signUp?key=AIzaSyDZeu_FAc1gl7xwZiOn_TgeCyGwQZbbtpQ",
           body = list(email=Nemail,password=Npassword, returnSecureToken = TRUE), encode = "json")
    status <- http_status(response)$category
    body <- content(response)
    idtoKen <-body$idToken
    localId<-body$localId
    if (status == 'Success'){
      urlDBUser <- sub("tokenusuario", localId, urlDB)
      isAuth <- TRUE
      resUser<-PUT(urlDBUser,
                   body=list(email=Nemail,nombre=Nnombre,ccaa=ccaa_reg,provincia=provincia_reg,combustible=combustible_reg,dir=dir_fav), encode = "json")
      
      getUserDB<-jsonlite::fromJSON(urlDBUser)
      
      
      updateTabsetPanel(session, "tabs", selected = "Mapa")
      updateSelectizeInput(session, "CCAA", selected = getUserDB$ccaa)
      updateSelectizeInput(session, "PROVINCIA", selected = getUserDB$provincia)
      updateSelectInput(session, "tipogasoleo", selected = getUserDB$combustible)
      updateTextInput(session, "loc", value = getUserDB$dir)
      
      user_sug <- getUserDB$email
      
      showModal(modalDialog(
        title = "Registro completado",
        "Has sido devuelto a la página inicial"
      ))
      
      
    }else{
      isAuth <- FALSE
      print(message<-body$error$message)
      showModal(modalDialog(
        title = "Error en el registro",
        "Por favor, asegúrese de rellenar los parámetros correctamente"
      ))
      
    }  
    
  })
  
  observeEvent(input$Auser_CCAA, {
    freezeReactiveValue(input, "Auser_PROVINCIA")
    updateSelectizeInput(
      session,
      inputId = "Auser_PROVINCIA",
      choices = DT[[input$Auser_CCAA]],
      selected = 1
    )}, ignoreInit = TRUE)
  
  
  observeEvent(input$updateButton, {
    Aemail <- input$Aemail
    Apassword <- input$Apassword
    Anombre <- input$Anombre
    Accaa_reg<- input$Auser_CCAA
    Aprovincia_reg<- input$Auser_PROVINCIA
    Acombustible_reg <- input$Auser_tipogasoleo
    Adir_fav <- input$Adir_fav
    response <- POST(
      "https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=AIzaSyDZeu_FAc1gl7xwZiOn_TgeCyGwQZbbtpQ",
      body = list(email=Aemail,password=Apassword, returnSecureToken = TRUE), encode = "json")
    status <- http_status(response)$category
    body <- content(response)
    idtoKen <-body$idToken
    localId<-body$localId
    if (status == 'Success'){
      urlDBUser <- sub("tokenusuario", localId, urlDB)
      isAuth <- TRUE
      resUser<-PATCH(urlDBUser,
                     body=list(email=Aemail,nombre=Anombre,ccaa=Accaa_reg,provincia=Aprovincia_reg,combustible=Acombustible_reg,dir=Adir_fav), encode = "json")
      
      getUserDB<-jsonlite::fromJSON(urlDBUser)
      
      
      updateTabsetPanel(session, "tabs", selected = "Mapa")
      updateSelectizeInput(session, "CCAA", selected = getUserDB$ccaa)
      updateSelectizeInput(session, "PROVINCIA", selected = getUserDB$provincia)
      updateSelectInput(session, "tipogasoleo", selected = getUserDB$combustible)
      updateTextInput(session, "loc", value = getUserDB$dir)
      
      user_sug <- getUserDB$email
      
      showModal(modalDialog(
        title = "Actualización completada",
        "Has sido devuelto a la página inicial"
      ))
      
      
    }else{
      isAuth <- FALSE
      print(message<-body$error$message)
      showModal(modalDialog(
        title = "Error al actualizar",
        "Por favor, asegúrese de que los datos introducidos son correctos"
      ))
      
    }  
    
  })
})
  
