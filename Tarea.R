#en la salita de 2 años NO se usan variables, tampoco sentencia if, loops ni funciones, ni parametros


#R para Kindergarten, sala de 2 años (pero con conceptos no triviales de analisis de datos)
#entorno de ejecucion : PC local

#set working directory
#setwd( "M:\\datasets\\" )
setwd( "C:\\Users\\Pablo Oliva\\Google Drive\\MaestriaDataScience\\LaboratorioDeImplementacion\\Clase01\\datasets" )



#opcion nativa en R para cargar un dataset

#opcion con  libreria  data.table

#instalo el paquete, se hace una sola vez
#install.packages( "data.table" )
#invoco a la libreria
library( "data.table" )
library(robustbase)

#cargo los datos, con la funcion File Read de la libreria data.table
t0       <-  Sys.time()
dataset  <-  data.table::fread("201904.txt")
t1       <-  Sys.time()
tcorrida <-  as.numeric(t1 - t0, units = "secs")
cat("tiempo carga dataset con fread ",  tcorrida, "\n" )


#se observa una enorme diferencia en el tiempo de carga de un archivo
# A partir de ahora usamos la libreria  data.table para manejar los datasets

#Notar como se llamó a la funcion data.table::fread() especificando la libreria
#tambien se la podria haber llamado  directamente con fread()


#------------------------------------------------------------------------------
#operaciones basicas a un dataset usando  data.table

# Ejercicio 1

#cantidad de columnas
ncol( dataset )

#nombres de las columnas
colnames( dataset )

#cantidad de registros
row_amount <- nrow( dataset )

# Ejercicio 2
baja_2_amount <- nrow(  dataset[ clase_ternaria=="BAJA+2", ] )
# En el  dataset anterior era 945 / 186266
baja_2_rate <- baja_2_amount / row_amount

# Ejercicio 3
# Crear ganancia
dataset[  , ganancia := -500]
dataset[ clase_ternaria=="BAJA+2",  ganancia := 19500 ]

sum(dataset[ clase_ternaria=="BAJA+2", ]$ganancia )
# Ganancia con predicado perfecto: 17433000

# Ejercicio 4
median_mplan_sueldo <- median(dataset$mplan_sueldo)
summary(dataset$mplan_sueldo)
less_than_median <- dataset[(dataset$mplan_sueldo < median_mplan_sueldo), ]
summary(less_than_median$mplan_sueldo)
sum(less_than_median$ganancia)

#Ejercicio 5

# Median for all variables
numeric_subset <- dplyr::select_if(dataset, is.numeric)
#medianas <- c()
#for(column_name in colnames(numeric_subset)) {
#  print(median(dataset[, ..column_name], na.rm = TRUE))
  #mediana_nueva <- median(dataset[, ..column_name], na.rm = TRUE)
  #medianas <- c(medianas, mediana_nueva)
#}
datamatrix <- as.matrix(numeric_subset)
colMedians(datamatrix, na.rm = FALSE, hasNA = TRUE, keep.names=TRUE)

