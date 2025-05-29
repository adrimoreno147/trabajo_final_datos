  library(tidyverse)
  library(geojsonsf)
  library(leaflet)
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(readxl)
  library(readr)
  library(leaflet.extras)
  
  #################################################### gráfico de renta media por persona ################################
  
  barrios <- read_xlsx('./barrios.xlsx')
  
  renta <- read.csv('./datos_renta.csv', sep = ';')


  
  
  barrios_sf <- barrios %>%
    mutate(geometry = geojson_sf(geo_shape)$geometry) %>%  # extrae solo la geometría
    st_sf(crs = 4326)

  
  
  rentas_2022 <- renta %>%
    rename(Codigo_distrito = Distritos,
           renta_media = Total)
  
  # Unión de tablas
  barrios_sf2 <- barrios_sf %>%
    group_by(Codigo_distrito) %>%
    summarise(geometry = st_union(geometry)) %>%
    ungroup()
  
  
  
  # En barrios_sf2 (o donde proceda), transforma a integer
  barrios_sf2 <- barrios_sf2 %>%
    mutate(Codigo_distrito = as.integer(Codigo_distrito))
  
  # En rentas_2022 (si fuera character)
  rentas_2022 <- rentas_2022 %>%
    mutate(Codigo_distrito = as.integer(Codigo_distrito))
  
  # Ahora el join funciona correctamente
  mapa_sf <- barrios_sf2 %>%
    left_join(rentas_2022, by = "Codigo_distrito")
  
  

  
  mapa_sf <- barrios_sf2 %>%
    left_join(rentas_2022, by = "Codigo_distrito")
  


mapa_sf <- mapa_sf %>%
  # primero quitar todos los puntos de miles
  mutate(renta_media = gsub("\\.", "", renta_media),
         # luego convertir a numérico
         renta_media = as.numeric(renta_media))

  
  
  
  
  
  pal_roja <- colorBin(
    palette = "Reds",         # tonos de rojo
    domain  = mapa_sf$renta_media,
    bins    = 5,              # ajusta nº de clases
    na.color = "transparent"
  )
  
  # 2. Construye el mapa interactivo
  leaflet(mapa_sf) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor   = ~pal_roja(renta_media),
      weight      = 1,
      opacity     = 1,
      color       = "white",
      dashArray   = "3",
      fillOpacity = 0.8,
      highlightOptions = highlightOptions(
        weight        = 2,
        color         = "#444",
        fillOpacity   = 1,
        bringToFront  = TRUE)) %>%
        
    # 3. Leyenda mejorada
    addLegend(
      pal        = pal_roja,
      values     = ~renta_media,
      position   = "bottomright",
      title      = htmltools::HTML("<strong>Renta media<br/>(€/persona)</strong>"),
      labFormat  = labelFormat(prefix = "€", big.mark = "."),
      opacity    = 0.9,
      na.label   = "Sin dato"
    )
  
  
  
  ##################### gráfico puntos de casinos (básico) ##########################3
  datos_salones <- read_xlsx('./ubi_salones.xlsx')
  
  datos_salones <- datos_salones %>%
    separate(coord, into = c("latitud", "longitud"), sep = ",") %>%
    mutate(
      latitud = as.numeric(latitud),
      longitud = as.numeric(longitud)
    )
  
  
  # Agrupar barrios por distrito
  barrios_sf2 <- barrios_sf %>%
    group_by(Codigo_distrito) %>%
    summarise(geometry = st_union(geometry)) %>%
    ungroup()
  
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      data = barrios_sf2,
      color = "#333333",
      weight = 1,
      fill = FALSE,
      popup = ~Codigo_distrito
    ) %>%
    addCircleMarkers(
      data = datos_salones,
      lng = ~longitud,
      lat = ~latitud,
      radius = 6,
      color = "red",
      fillOpacity = 0.9,
      popup = ~nombre
    ) %>%
    setView(lng = -0.376, lat = 39.47, zoom = 12)
  
  
  
  
  
  ###################################### gráfico de densidad ################################
  # 1. Leer tu base de salones
  salones <- read_xlsx("ubi_salones.xlsx")
  
  # 2. Convertir a objeto sf con CRS compatible (WGS84)
  salones_sf <- st_as_sf(datos_salones, coords = c("latitud", "longitud"), crs = 4326)

  # 4. Unir cada salón al barrio donde se encuentra
  salones_con_barrios <- st_join(salones_sf, barrios_sf, left = FALSE)
  
  
  
  barrios_sf <- st_transform(barrios_sf, crs = 4326)
  salones_sf <- st_transform(salones_sf, crs = 4326)
  
  # Obtener coordenadas de los salones en formato data.frame
  coords <- salones_con_barrios %>% 
    st_coordinates() %>% 
    as.data.frame()
  
  
  
  # Extraer coordenadas de los salones
  coords_salones <- st_coordinates(salones_sf) %>% as.data.frame()
  salones_sf$lon <- coords_salones$Y
  salones_sf$lat <- coords_salones$X
  
  # Crear el mapa interactivo
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    # Añadir polígonos de los barrios
    addPolygons(
      data = barrios_sf2,
      fillColor = "transparent",
      color = "black",
      weight = 1,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.1,
        bringToFront = TRUE
      )
    ) %>%
    
    # Añadir heatmap con los salones
    addHeatmap(
      data = salones_sf,
      lng = ~lon, lat = ~lat,
      blur = 5,
      max = 0.4,
      radius = 80
    )
  
  
  ################################### mapa deportes ######################
  bbox_valencia <- osmdata::getbb("Valencia")
  deportes_vlc <- bbox_valencia %>%
    osmdata::opq() %>%
    osmdata::add_osm_feature(key = "leisure", value = "pitch")
  
  
  deportes_vlc <- osmdata::osmdata_sf(deportes_vlc)
  deportes_vlc_points <- deportes_vlc$osm_points
  
  # Asegúrate de que ambos datasets estén en el mismo CRS
  barrios_sf <- st_transform(barrios_sf, 4326)  # Si no lo están ya
  deportes_vlc_points <- st_transform(deportes_vlc_points, 4326)
  
  # 2. Reconstrucción del sf usando coords = c("longitud", "latitud")
  salones_sf <- st_as_sf(
    datos_salones,
    coords = c("longitud", "latitud"),  # ¡orden correcto: lon, lat!
    crs    = 4326,                      # WGS84 (long-lat)
    remove = FALSE                      # conserva columnas originales
  )
  
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    # Capa: Barrios
    addPolygons(data = barrios_sf2,
                color = "#000000", weight = 1,
                fillColor = "#FFFFFF", fillOpacity = 0.1) %>%
    
    # Capa: Instalaciones deportivas
    addCircleMarkers(data = deportes_vlc_points,
                     radius = .5,
                     color = "#238443",  # verde
                     fillOpacity = 0.6,
                     label = ~leisure) %>%
    
    # Capa: Salones de juego
    addCircleMarkers(
      data        = salones_sf,
      radius      = 5,
      color       = "#e41a1c",
      fillOpacity = 0.8,
      label       = ~nombre)%>%
    
    # Vista centrada en Valencia
    setView(lng = mean(st_coordinates(st_centroid(st_union(barrios_sf)))[,1]),
            lat = mean(st_coordinates(st_centroid(st_union(barrios_sf)))[,2]),
            zoom = 13)

  
  
  ####################################### mapa institutos #####################################
  edu <- read_xls('./centros_educativos.xls')
  head(edu)
            
  
  
  table(edu$Denominacion_Generica_ES)
  edu_secundaria <- edu %>%
    filter(grepl("SECUNDARIA", Denominacion_Generica_ES),
           Localidad == "VALÈNCIA")
  

  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    # Polígonos: barrios
    addPolygons(data = barrios_sf2,
                color = "#000000", weight = 1,
                fillColor = "#FFFFFF", fillOpacity = 0.1) %>%
    
    # Puntos: centros educativos
    addCircleMarkers(data = edu_secundaria,
                     radius = .5,
                     color = "#08519c",  # azul oscuro
                     fillOpacity = 0.8) %>%
    
    # Puntos: salones de juego
    addCircleMarkers(data = salones_sf,
                     radius = 3,
                     color = "#e41a1c", fillColor = "#e41a1c",
                     fillOpacity = 0.8,
                     label = ~nombre,
                     group = "Salones de juego") %>%
    
    # Centrar vista en València
    setView(lng = mean(st_coordinates(st_centroid(st_union(barrios_sf)))[,1]),
            lat = mean(st_coordinates(st_centroid(st_union(barrios_sf)))[,2]),
            zoom = 13)
  
