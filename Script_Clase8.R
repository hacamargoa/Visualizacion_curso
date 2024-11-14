library(geoAr) #https://cran.r-project.org/web/packages/geoAr/geoAr.pdf
library(ggplot2)
library(plyr)
library(dplyr)

arg_prov<- get_geo(geo = "ARGENTINA", level = "provincia")
arg_dep<- get_geo(geo = "ARGENTINA", level = "departamento")

plot(arg_dep)

ggplot(data = arg_prov) +
  geom_sf(fill = "lightblue", color = "darkblue", size = 0.3) +
  labs(title = "Map of Argentina by Provinces") +
  theme_minimal()

#Pipes tuberias
Ejemplo<-read.table("C:/Users/camargo-h/Documents/Projects/Personal/Argentina/Curso/Clase 7/Datos_clase7_reg.txt",h = T)
Ejemplo<-subset(Ejemplo,Lat >30)
Ejemplo<-select(Ejemplo,c(Lon,Lat))
rm(Ejemplo)

Ejemplo<-read.table("C:/Users/camargo-h/Documents/Projects/Personal/Argentina/Curso/Clase 7/Datos_clase7_reg.txt",h = T)%>%
subset(Lat >30)%>%
select(Lon,Lat)


get_geo("CATAMARCA")%>%
  add_geo_codes()%>%
  plot()

p<-get_geo("CATAMARCA")%>%
  add_geo_codes()

plot(p$geometry)

get_geo("CATAMARCA") %>%
  leaflet::leaflet() %>%
  leaflet::addPolygons() %>%
  addArgTiles()

###GITHUB

get_geo("LA RIOJA") %>%
  leaflet::leaflet() %>%
  leaflet::addPolygons() %>%
  addArgTiles()

p<-get_geo("LA RIOJA")%>%
  add_geo_codes()

plot(p)




