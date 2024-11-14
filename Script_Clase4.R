#Hacer data frame 
data <- data.frame(
  altura = c(150, 160, 170, 180, 190),
  peso = c(55, 65, 75, 85, 95),
  edad = c(23, 34, 45, 56, 67)
)

#Manipular data frame
data[,2]
data[3,2]
data[3,2]<-50


#loops

i=0
while (i <= 10){
  print(i)
  i=i+1
}

#loops

for (i in 0:10){
  print(i)
  i=i+1
}

promedios<-c()
for (i in colnames(data)) {
  # Calcular el promedio de la columna actual y añadirlo al vector
  promedios <- c(promedios, mean(data[[i]]))
}


for (i in 1:5) {
  for (j in 1:5) {
    print(paste(i, "x", j, "=", i * j))
  }
}

df<-data.frame()
for (i in 1:5) {
  for (j in 1:5) {
    df[i,j]=i * j
  }
}

#Ejercicio
#Crear un proyecto del curso

#Crear una lista que contenga los 8 rasters de las 8 
#variables almacenadas en winter_and_spring_wheat_areas_phase3.nc4
#(wwh_area, wwh_area_rf, wwh_area_ir, wwh_mask, 
#swh_area, swh_area_rf, swh_area_ir, swh_mask)
#Obligatorio: que sea con un bucle (loop)



