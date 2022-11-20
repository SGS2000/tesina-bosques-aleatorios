#Se realiza el tuneo de los hiperparámetros del algoritmo

####Librerías###
library(randomForest)
library(caret)
library(tidyverse)
library(doParallel)

####Funciones###

  #Reducir dimensiones de la matriz y convertirla a dataframe
etiquetas = as.data.frame(c(rep("aventura",125), rep("ciencia ficcion",72),
                            rep("fantastico",100), rep("ficcion historica",139),
                            rep("policial",82), rep("romance",136),
                            rep("terror",92)) )
colnames(etiquetas) = "etiqueta_genero"

convertir_matriz = function(matriz, conjunto, atributo="palabra", cota=10000){
  conjunto=conjunto[1:cota,]
  matriz = matriz[, (colnames(matriz) %in% conjunto[[atributo]])]
  datos=convert(matriz, to = "data.frame")
  datos=cbind(select(datos,-doc_id),etiquetas)
  return(datos)
}
  #Crea un objeto train a partir de un dataframe específicado
entrenar_algoritmo = function(datos, arboles=500){
  cl <- makePSOCKcluster(detectCores())
  registerDoParallel(cl)
  
  entrenamiento <- train(etiqueta_genero~.,
                   data = datos,
                   method = "rf",
                   metric = "Accuracy",
                   tuneGrid = data.frame(.mtry = c(50,100,500,1000,1500,2000,3000)),
                   trControl = trainControl(method = "repeatedcv",
                                            number = 5,
                                            repeats = 3,
                                            search="grid"),
                   ntree = arboles
  )
  
  stopCluster(cl)
  return(entrenamiento)
}

###Carga de archivos###
load(".../palabras_ord.Rdata")
load(".../bigramas_ord.Rdata")
load(".../palabras_tfidf_ord.Rdata")
load(".../bigramas_tfidf_ord.Rdata")
load(".../palabras_filt_ord.Rdata")
load(".../bigramas_filt_ord.Rdata")
load(".../palabras_tfidf_filt_ord.Rdata")
load(".../bigramas_tfidf_filt_ord.Rdata")

load(".../matriz_terminos.Rdata")
load(".../matriz_bigramas.Rdata")
load(".../matriz_tfidf.Rdata")
load(".../matriz_bg_tfidf.Rdata")
load(".../matriz_filt.Rdata")
load(".../matriz_bg_filt.Rdata")
load(".../matriz_tfidf_filt.Rdata")
load(".../matriz_bg_tfidf_filt.Rdata")

####Elegir directorio####
setwd("...")

###Establecer semilla###
semilla = 30062000

###Separar en conjunto de entrenamiento y de pruebas###

  ##Escenario A##
datosA = convertir_matriz(matriz_terminos, palabras_ord)
set.seed(semilla)
muestra <- sample(nrow(datosA),size=522,replace=FALSE)
datos_entrenamientoA <- datosA [muestra,]
datos_testeoA <- datosA[-muestra,]

  ##Escenario B##
datosB = convertir_matriz(matriz_bigramas, bigramas_ord, "bigrama")
set.seed(semilla)
muestra <- sample(nrow(datosB),size=522,replace=FALSE)
datos_entrenamientoB <- datosB[muestra,]
datos_testeoB <- datosB[-muestra,]

  ##Escenario C##
datosC = convertir_matriz(matriz_tfidf, palabras_tfidf_ord)
set.seed(semilla)
muestra <- sample(nrow(datosC),size=522,replace=FALSE)
datos_entrenamientoC <- datosC[muestra,]
datos_testeoC <- datosC[-muestra,]

  ##Escenario D##
datosD = convertir_matriz(matriz_bg_tfidf, bigramas_tfidf_ord, "bigrama")
set.seed(semilla)
muestra <- sample(nrow(datosD),size=522,replace=FALSE)
datos_entrenamientoD <- datosD[muestra,]
datos_testeoD <- datosD[-muestra,]

  ##Escenario E##
datosE = convertir_matriz(matriz_filt, palabras_filt_ord)
set.seed(semilla)
muestra <- sample(nrow(datosE),size=522,replace=FALSE)
datos_entrenamientoE <- datosE[muestra,]
datos_testeoE <- datosE[-muestra,]

  ##Escenario F##
datosF = convertir_matriz(matriz_bg_filt, bigramas_filt_ord, "bigrama")
set.seed(semilla)
muestra <- sample(nrow(datosF),size=522,replace=FALSE)
datos_entrenamientoF <- datosF[muestra,]
datos_testeoF <- datosF[-muestra,]

  ##Escenario G##
datosG = convertir_matriz(matriz_tfidf_filt.Rdata, palabras_tfidf_filt_ord)
set.seed(semilla)
muestra <- sample(nrow(datosG),size=522,replace=FALSE)
datos_entrenamientoG <- datosG[muestra,]
datos_testeoG <- datosG[-muestra,]

  ##Escenario H##
datosH = convertir_matriz(matriz_bg_tfidf_filt, bigramas_tfidf_filt_ord, "bigrama")
set.seed(semilla)
muestra <- sample(nrow(datosH),size=522,replace=FALSE)
datos_entrenamientoH <- datosH[muestra,]
datos_testeoH <- datosH[-muestra,]

###Tuneo de los hiperparámetros###
  ##Escenario A##
set.seed(semilla)
trainA = entrenar_algoritmo(datos_entrenamientoA)
trainA_1000 = entrenar_algoritmo(datos_entrenamientoA, arboles=1000)

  ##Escenario B##
set.seed(semilla)
trainB = entrenar_algoritmo(datos_entrenamientoB)
trainB_1000 = entrenar_algoritmo(datos_entrenamientoB, arboles=1000)

  ##Escenario C##
set.seed(semilla)
trainC = entrenar_algoritmo(datos_entrenamientoC)
trainC_1000 = entrenar_algoritmo(datos_entrenamientoC, arboles=1000)

  ##Escenario D##
set.seed(semilla)
trainD = entrenar_algoritmo(datos_entrenamientoD)
trainD_1000 = entrenar_algoritmo(datos_entrenamientoD, arboles=1000)

  ##Escenario E##
set.seed(semilla)
trainE = entrenar_algoritmo(datos_entrenamientoE)
trainE_1000 = entrenar_algoritmo(datos_entrenamientoE, arboles=1000)

  ##Escenario F##
set.seed(semilla)
trainF = entrenar_algoritmo(datos_entrenamientoF)
trainF_1000 = entrenar_algoritmo(datos_entrenamientoF, arboles=1000)

  ##Escenario G##
set.seed(semilla)
trainG = entrenar_algoritmo(datos_entrenamientoG)
trainG_1000 = entrenar_algoritmo(datos_entrenamientoG, arboles=1000)

  ##Escenario H##
set.seed(semilla)
trainH = entrenar_algoritmo(datos_entrenamientoH)
trainH_1000 = entrenar_algoritmo(datos_entrenamientoH, arboles=1000)