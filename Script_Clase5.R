library(sf)
library(sp)
library(raster)
library(rnaturalearth)

Argentina_ <- ne_countries(scale = "medium", country = "Argentina", returnclass = "sf")
Montañas<-data.frame(lon = c(-70.0101, -68.5404, -67.8499, -67.8517, -67.6643),
                     lat = c(-32.6532, -27.0978, -24.6353, -28.1738, -25.8794),
                     nombre=c("Aconcagua","Ojos de Salado","Monte Piss","Nevado Tres Cruces", "Cerro Bonete"),
                     Altura=c(6961,6893,6793,6629,6759))
coordinates(Montañas) <- ~ lon + lat
proj4string(Montañas) <- CRS("+proj=longlat +datum=WGS84")
Mont<-st_as_sf(Montañas)

#Graficar
plot(Argentina_$geometry, axes=T,grid=T)
grid(col = "gray", lty = "dotted")
plot(Montañas,add=T,pch=19,col ="red")
text(x = coordinates(Montañas)[,1]+2, y = coordinates(Montañas)[,2]-1, 
     labels = Montañas$Altura, pos = 3, cex = 0.8, col = "blue")
text(x = coordinates(Montañas)[,1]-6.3, y = coordinates(Montañas)[,2]-1, 
    labels = Montañas$nombre, pos = 3, cex = 0.8, col = "blue")

#Graficar 2
plot(Argentina_$geometry, axes=T,grid=T,col="gray70",main = "Montañas mas altas de Argentina",
     xlab="Longitude",ylab="Latitude",font.main = 3,cex.main = 1.5,font.lab =3,cex.lab=1.2)
grid(col = "gray", lty = "dotted")
plot(Montañas,add=T,pch=19,col ="red")
text(x = coordinates(Montañas)[,1]+2, y = coordinates(Montañas)[,2]-1, 
     labels = Montañas$Altura, pos = 3, cex = 0.8, col = "blue")
text(x = coordinates(Montañas)[,1]-6.3, y = coordinates(Montañas)[,2]-1, 
     labels = Montañas$nombre, pos = 3, cex = 0.8, col = "blue")

#Graficar 3
library(datasets)
View(airquality)
air<-airquality
air$Dia<-seq(1,153)
plot(air$Dia,air$Temp) #1

plot(air$Dia,air$Temp,type="l",col="red",lwd=2,ylim=c(0,170),xlab="Dia",ylab="") #2
lines(air$Dia,air$Wind,type="l",col="green4",lwd=2)
lines(air$Dia,air$Ozone,type="l",col="blue",lwd=2)

#add a legend
legend("top",legend = c("Temperature", "Wind", "Ozone"), 
       col = c("red", "green4", "blue"),lwd = 2,bty = "n",ncol=3)                               

#add a legend 2
par(xpd=TRUE)
legend("topright",inset=c(0.1,-0.15),legend = c("Temperature", "Wind", "Ozone"), 
       col = c("red", "green4", "blue"),lwd = 2,bty = "y",ncol=3)    

#Graficar 4
trigo<-raster("../Clase 3/winter_and_spring_wheat_areas_phase3.nc4")
plot(trigo,main="Areas de trigo mundiales (ha)")
countries <- ne_countries(scale = "medium", returnclass = "sf")
plot(countries$geometry, add=T,axes=T)

plot(trigo,main="Areas de trigo mundiales (ha)",horizontal=T,
     legend.only=F,legend.args = list(text='Hectareas (ha)',side = 3, line = 1))
plot(countries$geometry, add=T,axes=T,side = 0, line = 1)

library(ggplot2)
#Montañas de Argentina
ggplot() +
  geom_sf(data = Argentina_, fill = "lightblue", color = "black")+  
  geom_sf(data = Mont, aes(geometry = geometry), color = "red", size = 3)+  
  geom_text(data = Mont, aes(x = coordinates(Montañas)[,1], y = coordinates(Montañas)[,2], label = nombre), vjust = -1, color = "black")+
  theme_minimal() +
  labs(title = "Mapa de Argentina con las 5 montañas más altas",
       x = "Longitud",
       y = "Latitud")

#series de tiempo
ggplot(data = air,aes(x=Dia))+ 
  geom_line(aes(y = Temp, color = "Temperature"), size = 1.2) + 
  geom_line(aes(y = Wind, color = "Wind"), size = 1.2) + 
  geom_line(aes(y = Ozone, color = "Ozone"), size = 1.2) + 
  scale_color_manual(values = c("Temperature" = "red", "Wind" = "green4", "Ozone" = "blue")) + 
  labs(title = "Análisis de Variables Ambientales",y = "Valores", x = "Dia") + 
  #theme_minimal() +  
  theme(legend.title = element_blank(),  
        legend.position = "top",
        plot.title = element_text(hjust = 0.5,size = 15),#family = "Arial"),
        axis.title.x = element_text(size = 14),          
        axis.title.y = element_text(size = 14),          
        axis.text.x = element_text(size = 12),           
        axis.text.y = element_text(size = 12))  
        
#Raster
trigo_df <- as.data.frame(trigo, xy = TRUE)
colnames(trigo_df)[3] <- "Area"

my_fill_scale <- scale_fill_gradient2(low = "gray80", mid = "yellow2", high = "darkgreen",
                                      midpoint = 70000,na.value = NA, name = "",
                                      breaks = c(0, 40000, 80000, 120000,160000),
                                      labels = c("0","40000", "80000","120000","160000"),
                                      limits = c(0, 130000))

ggplot() +
  geom_sf(data = countries, color = "black", fill = "gray50")+
  geom_raster(data = trigo_df, aes(x, y, fill = Area)) +
  my_fill_scale +  
  labs(title = "Áreas de Trigo Mundiales (ha)",
       x = " ", y = " ")+
  theme(panel.background = element_rect(fill = "lightblue"),
        plot.title = element_text(hjust = 0.5, size = 25),
        axis.text = element_text(size = 20),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.56, 0.06),
        legend.key.width = unit(0.8, "inch"),
        legend.key.height = unit(0.1, "inch"),
        legend.text = element_text(size = 20),
        legend.box.background = element_rect(color = "white"),
        legend.box.margin = margin(0, 30, 0, 0),
        legend.key.size = unit(1, "line"),
        legend.direction = "horizontal")
  
ggplot() +
  geom_sf(data = countries, color = "black", fill = "gray40")+
  geom_raster(data = trigo_df, aes(x, y, fill = Area)) +
  coord_sf(crs = "+proj=robin", xlim = c(-12.3e6, 13e6),
           ylim=c(-6.8e6,7.9e6))+
  my_fill_scale +  
  labs(title = "Áreas de Trigo Mundiales (ha)",
       x = " ", y = " ")+
  theme(panel.background = element_rect(fill = "lightblue"),
        plot.title = element_text(hjust = 0.5, size = 25),
        axis.text = element_text(size = 20),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.56, 0.06),
        legend.key.width = unit(0.8, "inch"),
        legend.key.height = unit(0.1, "inch"),
        legend.text = element_text(size = 20),
        legend.box.background = element_rect(color = "white"),
        legend.box.margin = margin(0, 5, 0, 0),
        legend.key.size = unit(1, "line"),
        legend.direction = "horizontal")



#Proyeccion 2
trigo2 <- projectRaster(trigo, crs = CRS("+proj=robin"))
plot(trigo2)
plot(trigo)
trigo2_df <- as.data.frame(trigo2, xy = TRUE)
colnames(trigo2_df)[3] <- "Area"

ggplot() +
  geom_sf(data = countries, color = "black", fill = "gray50")+
  geom_raster(data = trigo2_df, aes(x, y, fill = Area)) +
  geom_sf(data = countries, color = "black", fill = "NA")+
  coord_sf(crs = "+proj=robin", xlim = c(-12.3e6, 13e6),
           ylim=c(-6.8e6,7.9e6))+
  my_fill_scale +  
  labs(title = "Áreas de Trigo Mundiales (ha)",
       x = " ", y = " ")+
  theme(panel.background = element_rect(fill = "lightblue"),
        plot.title = element_text(hjust = 0.5, size = 25),
        axis.text = element_text(size = 20),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.56, 0.06),
        legend.key.width = unit(0.8, "inch"),
        legend.key.height = unit(0.1, "inch"),
        legend.text = element_text(size = 12),
        legend.box.background = element_rect(color = "white"),
        legend.box.margin = margin(0, 5, 0, 0),
        legend.key.size = unit(1, "line"),
        legend.direction = "horizontal")

library(rworldmap)

trigo_sdf<-as(trigo/1000,"SpatialGridDataFrame") #miles de hectareas
mapNpool<-mapGriddedData(trigo_sdf,catMethod = seq(0,140,20),
                         colourPalette = c("white","yellow","darkgoldenrod2",
                                           "orangered","red","red2","red4"),
                         borderCol = "black",
                         oceanCol="azure2",xlim=c(-180,180),ylim=c(-35,90),landCol="gray",
                         addLegend=F)
do.call(addMapLegend,c(mapNpool, legendLabels="all",legendIntervals="page",
                       legendWidth=0.5,digits=2,legendShrink=0.9,
                       legendMar=5,labelFontSize=0.8))
mtext("Áreas de Trigo Mundiales (ha)", side = 3, line = -3, cex = 2)











#Ejercicio

#1. Usar datos Datos_fert_Arg.txt para crear un mapa presentable de
#la Fertilización en trigo en Argentina a gusto con ggplot o plot

#2. Utilizar Rendim_mundial.txt (rendimiento (kg/m2)) para crear un mapa 
#usando el paquete rworldmap de rendimiento de alguno de los cultivos

#primero leerlos>raster>

world<-read.table("../Clase 2/Datos_clase2.txt",h=T,sep =",")
world<-subset(world,Year==2012)
worldr<-rasterFromXYZ(world[c(1,2,4)])
plot(worldr)


getwd()
