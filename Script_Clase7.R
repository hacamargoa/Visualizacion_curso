library(raster)
library(rnaturalearth)
library(sf)
install.packages("fields")
library(fields)

Alemania <- ne_countries(country = "Germany", returnclass = "sf")
trigo<-raster("../Clase 3/winter_and_spring_wheat_areas_phase3.nc4")
plot(trigo,main="Areas de trigo mundiales (ha)")

#Zoom a USA
Trigo_Usa<-crop(trigo,extent(-126, -61, 25, 50))
plot(Trigo_Usa)
plot(USA$geometry,add=T)

#Enmascaramiento
Trigo_USA<-mask(Trigo_Usa,USA)
plot(Trigo_USA)
plot(USA$geometry,add=T)

#Correlación espacial
Fert<-read.table("C:/Users/camargo-h/Documents/Projects/Personal/Argentina/Curso/Clase 7/Datos_clase7.txt", h=T)
Fertr<-rasterFromXYZ(Fert)
plot(Fertr)

Fert_Usa<-crop(Fertr,extent(-126, -61, 25, 50))
plot(Fert_Usa,main="Kg/m2 de Nitrogeno")
plot(USA$geometry,add=T)

Fert_USA<-mask(Fert_Usa,USA)
plot(Fert_USA)
plot(USA$geometry,add=T)

USA_rast<-brick(Trigo_USA,Fert_USA)
USA_rast<-stack(Trigo_USA,Fert_USA)
Corr_sp<-cor.test(getValues(Trigo_USA),getValues(Fert_USA),use = "complete.obs")
Corr_sp$estimate
Corr_sp$p.value

#reduccion de escala
Muestra<-raster(extent(-126, -61, 25, 50),res=0.05)
plot(Muestra)
trigo05<-resample(Trigo_Usa, Muestra, method = "bilinear")
plot(trigo05)
plot(USA$geometry,add=T)

Trigo05<-mask(trigo05, USA)
plot(Trigo05)
plot(USA$geometry,add=T)

fert05<-resample(Fert_USA, Muestra, method = "bilinear")
plot(fert05)
plot(USA$geometry,add=T)

#Interpolación bilinear
Datos<-read.table("C:/Users/camargo-h/Documents/Projects/Personal/Argentina/Curso/Clase 7/Datos_clase7_reg.txt",h = T)
Interp_r<-rasterFromXYZ(Datos)
Datos_<-Datos
coordinates(Datos) <- ~Lon + Lat
colores <- colorRampPalette(c("blue", "green", "yellow", "red"))(100)
plot(Datos, col=colores[cut(Datos$value, breaks=100)], pch=20,main=" ")
axis(3, col.axis="blue", col.lab="black", cex.axis=1)  
axis(2, col.axis="blue", col.lab="black", cex.axis=1)  
image.plot(legend.only=TRUE,zlim=range(Datos$value),col=colores,legend.lab="Valores",legend.line=-1,legend.width=1,
           legend.shrink=0.8,horizontal = T)   

library(akima)
Datos2 <- with(Datos_, interp(Lon, Lat, value, duplicate = "mean"))
interp_df <- as.data.frame(expand.grid(Lon = Datos2$x, Lat = Datos2$y))
interp_df$Valores <- as.vector(Datos2$z)
Interp_r<-rasterFromXYZ(interp_df)
plot(Interp_r)

#Otras opciones Krigging y Regresions lineales y no lineales

#Ejercicio

Temp<-raster("../Clase 7/CRU_mean_temperature_mon_0.5x0.5_global_2019_v4.03.nc")
Temp<-brick("../Clase 7/CRU_mean_temperature_mon_0.5x0.5_global_2019_v4.03.nc")
Temp<-stack("../Clase 7/CRU_mean_temperature_mon_0.5x0.5_global_2019_v4.03.nc")

Temp[[10]]+Temp[[11]]

#Obtener un mapa para Junio y otro para Diciembre que 
#solo muestre la temperatura de Argentina a una resolución 
#de 0.05x0.05

