library(sf)
library(sp)
library(raster)
library(rnaturalearth)
getwd()


#Definir directorio de trabajo
#SHAPEFILES
Argentina <- st_read("Mapa_Argentina_Bicontinental_ArcGIS/datos_shp/limites.shp")
plot(Argentina$geometry, axes=T)
plot(st_geometry(Argentina), axes=T)
class(Argentina)



Argentina_ <- ne_countries(scale = "medium", country = "Argentina", returnclass = "sf")
plot(Argentina_$geometry, axes=T)
class(Argentina_)

Montañas<-data.frame(lon = c(-70.0101, -68.5404, -67.8499, -67.8517, -67.6643),
                     lat = c(-32.6532, -27.0978, -24.6353, -28.1738, -25.8794),
                     nombre=c("Aconcagua","Ojos de Salado","Monte Piss","Nevado Tres Cruces", "Cerro Bonete"),
                     Altura=c(6961,6893,6793,6629,6759))
Montañas_<-Montañas #mantener el dataframe original
coordinates(Montañas) <- ~ lon + lat
proj4string(Montañas) <- CRS("+proj=longlat +datum=WGS84")
class(Montañas)
Mont<-st_as_sf(Montañas)
class(Mont)
st_write(Mont, "Montañas_shapefile.shp",delete_dsn = TRUE)

#Graficar con R básico
plot(Argentina_$geometry, main = "", axes=T,grid=T)
grid(col = "gray", lty = "dotted")
plot(Montañas,add=T,pch=19,col ="red")
text(x = coordinates(Montañas)[,1], y = coordinates(Montañas)[,2], 
     labels = Montañas$Altura, pos = 3, cex = 0.8, col = "blue")

#otra forma
library(ggplot2)
ggplot() +
  geom_sf(data = Argentina_, fill = "lightblue", color = "black")+  
  geom_sf(data = Mont, aes(geometry = geometry), color = "red", size = 3)+  
  geom_text(data = Mont, aes(x = Montañas_$lon, y = Montañas$lat, label = nombre), vjust = -1, color = "black")+
  theme_minimal() +
  labs(title = "Mapa de Argentina con las 5 montañas más altas",
       x = "Longitud",
       y = "Latitud")

#GEOTIFF
NDVI<-raster("NDVI 2020-12-10 a 2020-12-25 Aqua Argentina GeoTIFF 5Km.tif")*
plot(NDVI)
plot(Argentina_$geometry, add=T,axes=T)
plot(Montañas,add=T,pch=19,col ="red")
text(coordinates(Montañas), labels = Montañas$Altura, pos = 3, cex = 0.8, col = "black")

#csv o txt
Arg_fert<-read.table("C:/Users/camargo-h/Documents/Projects/Personal/Argentina/Curso/Clase 3/Datos_fert.txt", h=T)
ArgFRas<-rasterFromXYZ(Arg_fert[,c(1,2,3)])
proj4string(ArgFRas) <- CRS("+proj=longlat +datum=WGS84")
st_crs(ArgFRas)
plot(ArgFRas)
plot(Argentina_$geometry, add=T,axes=T)


#asc
Fert_2000<-raster("../pfery2000.asc")
plot(Fert_2000)

#netcdf
library(ncdf4)
trigo<-raster("../Clase 3/winter_and_spring_wheat_areas_phase3.nc4")
trigo
plot(trigo)
world <- ne_countries(scale = "medium", returnclass = "sf")
plot(world$geometry, add=T,axes=T)

#loop
Wheat_list<-list()

lista1<-list(trigo_wwh_area_rf,trigo_wwh_area_rf,trigo_swh_area_rf,trigo_swh_area_ir)

#traer todas las variables
rasters<-list()
nombres<-c("wwh_area", "wwh_area_rf", "wwh_area_ir", "wwh_mask", 
            "swh_area", "swh_area_rf", "swh_area_ir", "swh_mask")
for (i in 1:8){
  rasters[[i]] <- raster("../Clase 3/winter_and_spring_wheat_areas_phase3.nc4", varname = nombres[i])
} 
 



 
trigo_wwh_area_rf <- raster("../Clase 3/winter_and_spring_wheat_areas_phase3.nc4", varname = "wwh_area_rf")

trigo_wwh_area_rf <- raster("../Clase 3/winter_and_spring_wheat_areas_phase3.nc4", varname = "wwh_area_ir")
trigo_swh_area_rf <- raster("../Clase 3/winter_and_spring_wheat_areas_phase3.nc4", varname = "swh_area_rf")
trigo_swh_area_ir <- raster("../Clase 3/winter_and_spring_wheat_areas_phase3.nc4", varname = "swh_area_ir")


#calculos con raster
trigo_stack <- stack(trigo_wwh_area_rf, trigo_wwh_area_ir, trigo_wwh_area_rf, trigo_swh_area_ir)
trigo_stack[[3]]
trigo_stack[[c(1,2)]]
plot(trigo_stack[[3]]+trigo_stack[[4]])

plot(trigo_stack[[4]])
plot(world$geometry, add=T,axes=T)

trigo_irr <- calc(trigo_stack[[c(3,4)]], fun = function(x) sum(x, na.rm = TRUE))
plot(trigo_irr)

##Ejercicio
##Obtener un mapa que se enfoque area sembrada en trigo (de invierno y primavera) irrigado y secano en Estados unidos
## y adicionar los limites de pais



