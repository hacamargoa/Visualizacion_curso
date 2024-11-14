library(sf)
library(sp)
library(httr)
library(jsonlite)
library(leaflet)

#https://www.argentina.gob.ar/misiones-satelitales/acceso-los-productos
#https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG
#https://catalogos2.conae.gov.ar/smn_modis_va/find.aspx?tipo=6&id=NDVI+2020-12-10+a+2020-12-25+Aqua+Argentina+GeoTIFF+5Km.tif 
#https://catalog.saocom.conae.gov.ar/catalog/#/ 
  
library(rnaturalearth)
Argentina_ <- ne_countries(scale = "medium", country = "Argentina", returnclass = "sf")


##Fuentes externas
#API Application programming interface

url <- "https://nominatim.openstreetmap.org/search"

# Verificar la respuesta y parsear el JSON a un dataframe
  data <- fromJSON(content(GET(url, query = list(q = "Buenos Aires, Argentina", format = "json")), "text"))
  print(data)
  
  # Paso 2: Crear el mapa con leaflet
  B_As<-leaflet() 
  B_As<-addTiles(B_As) 
  B_As<-setView(B_As,lng = as.numeric(data$lon[1]), lat = as.numeric(data$lat[1]), zoom = 12 ) 
  B_As<-addMarkers(B_As,lng = as.numeric(data$lon[1]), lat = as.numeric(data$lat[1]), popup = "Buenos Aires")

  B_As

  #Montañas
  
  Montañas<-data.frame(lon = c(-70.0101, -68.5404, -67.8499, -67.8517, -67.6643),
                       lat = c(-32.6532, -27.0978, -24.6353, -28.1738, -25.8794),
                       nombre=c("Aconcagua","Ojos de Salado","Monte Piss","Nevado Tres Cruces", "Cerro Bonete"),
                       Altura=c(6961,6893,6793,6629,6759))
  Montañas_<-Montañas
  coordinates(Montañas) <- ~ lon + lat
  proj4string(Montañas) <- CRS("+proj=longlat +datum=WGS84")
  Mont<-st_as_sf(Montañas)  
  
  #Obtener información
  data_ <- fromJSON(content(GET(url, query = list(q = "Argentina", format = "json")), "text"))
  print(data)
  
  # Crear el mapa con leaflet
  Arg_<-leaflet() 
  Arg_<-addTiles(Arg_) 
  Arg_<-setView(Arg_,lng = as.numeric(data_$lon[1]), lat = as.numeric(data_$lat[1]), zoom = 4 ) 
  #Arg_<-addMarkers(Arg_,lng = Montañas$lon, lat = Montañas$lat, popup = Montañas$nombre)
  Arg_<-addCircleMarkers(Arg_,data=Mont,radius = 5, color ="red",label= ~nombre,
                         labelOptions = labelOptions(noHide = TRUE, offset=c(0.1,-12), textOnly = TRUE,
                                                     style = list("font-size" = "12px","font-weight" = "bold")))
  
  Arg_
  
  
  #otras url
  #http://dev.virtualearth.net/REST/v1/Locations?query= 
  #https://api.mapbox.com/geocoding/v5/mapbox.places/
  #http://api.positionstack.com/v1/forward?access_key=
  #https://geocode.search.hereapi.com/v1/geocode
  
  
  #Google maps
  #https://maps.googleapis.com/maps/api/geocode/json
  
  library(ggmap)
  
  register_google("AIzaSyBT2Fgc8kt1gAmZ-8DldEo2i2E2C3_P5WU")
  BAs_Map <- get_map("Buenos Aires", source="google", api_key = apiKey, zoom=12)
  ggmap(BAs_Map)
  
  As_Map <- get_map("Cordoba, Argentina", source="google", maptype="terrain",api_key = apiKey, zoom=5)
  ggmap(As_Map)+
    geom_point(data = Montañas_, aes(x = lon, y = lat), color = "red", size = 3) +
    geom_text(data = Montañas_, aes(x = lon, y = lat, label = nombre), vjust = -1, color = "blue") +
    labs(title = "Montañas mas altas de Argentina", x = "Longitude", y = "Latitude")
 
  #Google_Earth
  library(googledrive)
  library(rgee)
  rgee::ee_install()
  ee_Initialize()
  ee_Authenticate()
  ee_check()
  region <- ee$Geometry$Point(c(-58.3816, -34.6037))$buffer(50000)
  ee_Initialize(user = "hector.camargoa@gmail.com")
  
  
  #Ejercicio
  #Crear un vector con 10 lugares de Argentina o de su provincia 
  #Obtener las coordenadas con Nominatim y crear un dataframe con ellos 
  #visualizar esos puntos con la capa base de OpenStreetMap
  library(httr)
  library(jsonlite)
  library(leaflet)
  
  # Crear un vector con los lugares de Argentina
  lugares <- c("Buenos Aires", "Mendoza", "Córdoba", "Ushuaia", "Cataratas del Iguazú")
  
  # Crear una lista vacía para almacenar coordenadas
  coordenadas <- list()
  
  # Obtener las coordenadas de cada lugar usando Nominatim
  for (lugar in lugares) {
    url <- paste0("https://nominatim.openstreetmap.org/search?city=", 
                  URLencode(lugar), "&country=Argentina&format=json")
    response <- GET(url)
    data <- fromJSON(content(response, "text"))
    
    if (length(data) > 0) {
      lat <- data[[1]]$lat
      lon <- data[[1]]$lon
      coordenadas[[lugar]] <- c(lat = lat, lon = lon)
    } else {
      coordenadas[[lugar]] <- c(lat = NA, lon = NA)
    }
  }
  
  # Crear un mapa interactivo con leaflet
  mapa <- leaflet() %>%
    addTiles() %>%
    setView(lng = -64, lat = -38, zoom = 4)  # Centrado en Argentina
  
  # Agregar marcadores para cada lugar
  for (lugar in names(coordenadas)) {
    if (!is.na(coordenadas[[lugar]]["lat"])) {
      mapa <- mapa %>% addMarkers(
        lng = as.numeric(coordenadas[[lugar]]["lon"]),
        lat = as.numeric(coordenadas[[lugar]]["lat"]),
        popup = lugar
      )
    }
  }
  
  # Mostrar el mapa
  mapa