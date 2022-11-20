#Se evalúan los bosques aleatorios anteriores utilizando enfoques alternativos
  #y se aplica boosting. 

####Librerías####
library(randomForest)
library(tidyverse)
library(caret)
library(xgboost)

####Funciones####

  #Calcular la precisión obtenida al utilizar 
    #dos etiquetas de Goodreads

    #Segunda etiqueta de Goodreads
nueva_etiqueta <- c("aventura", "aventura", "aventura", "aventura", "aventura", 
  "ciencia ficcion", "ficcion historica", "aventura", "aventura", 
  "ficcion historica", "aventura", "aventura", "policial", "ficcion historica", 
  "ficcion historica", "ficcion historica", "aventura", "aventura", 
  "aventura", "ficcion historica", "policial", "aventura", "ciencia ficcion", 
  "aventura", "ficcion historica", "aventura", "ficcion historica", 
  "aventura", "ficcion historica", "aventura", "ficcion historica", 
  "ciencia ficcion", "terror", "aventura", "fantastico", "ciencia ficcion", 
  "terror", "ciencia ficcion", "fantastico", "terror", "ciencia ficcion", 
  "fantastico", "ciencia ficcion", "terror", "fantastico", "fantastico", 
  "ciencia ficcion", "fantastico", "fantastico", "terror", "fantastico", 
  "fantastico", "fantastico", "aventura", "ciencia ficcion", "aventura", 
  "aventura", "ciencia ficcion", "fantastico", "aventura", "terror", 
  "aventura", "fantastico", "fantastico", "fantastico", "policial", 
  "fantastico", "fantastico", "aventura", "terror", "aventura", 
  "terror", "aventura", "romance", "fantastico", "fantastico", 
  "fantastico", "fantastico", "terror", "aventura", "fantastico", 
  "ciencia ficcion", "fantastico", "ficcion historica", "ficcion historica", 
  "aventura", "aventura", "ficcion historica", "ficcion historica", 
  "ficcion historica", "ficcion historica", "policial", "romance", 
  "ficcion historica", "ficcion historica", "ficcion historica", 
  "aventura", "ficcion historica", "ficcion historica", "ficcion historica", 
  "aventura", "aventura", "ficcion historica", "ficcion historica", 
  "ficcion historica", "aventura", "ficcion historica", "aventura", 
  "ficcion historica", "ficcion historica", "aventura", "aventura", 
  "ficcion historica", "ficcion historica", "ficcion historica", 
  "ficcion historica", "ficcion historica", "romance", "ficcion historica", 
  "ficcion historica", "ficcion historica", "fantastico", "romance", 
  "ficcion historica", "ficcion historica", "ficcion historica", 
  "aventura", "ficcion historica", "ficcion historica", "aventura", 
  "ficcion historica", "terror", "terror", "policial", "policial", 
  "terror", "policial", "policial", "ficcion historica", "policial", 
  "policial", "policial", "policial", "romance", "ficcion historica", 
  "policial", "ficcion historica", "policial", "ficcion historica", 
  "policial", "terror", "policial", "policial", "policial", "policial", 
  "ficcion historica", "romance", "romance", "romance", "romance", 
  "ficcion historica", "romance", "ficcion historica", "romance", 
  "romance", "romance", "romance", "ficcion historica", "romance", 
  "romance", "romance", "romance", "romance", "romance", "romance", 
  "romance", "romance", "romance", "romance", "ficcion historica", 
  "romance", "ficcion historica", "ficcion historica", "romance", 
  "romance", "romance", "romance", "ficcion historica", "romance", 
  "ficcion historica", "romance", "romance", "ficcion historica", 
  "romance", "fantastico", "fantastico", "terror", "terror", "fantastico", 
  "ciencia ficcion", "policial", "terror", "fantastico", "policial", 
  "fantastico", "policial", "fantastico", "fantastico", "fantastico", 
  "fantastico", "policial", "terror", "ciencia ficcion", "policial", 
  "fantastico", "policial", "romance", "fantastico", "policial", 
  "fantastico", "terror", "terror", "policial", "terror")

comparar_precision = function(obj_entren, conjunto_prueba){
  test <- predict(obj_entren, newdata = conjunto_prueba, type="prob") #Predicciones como probabilidad
  
  etiqueta_real <- conjunto_prueba$etiqueta_genero #Etiquetas reales de los libros del conjunto de pruebas
  clase_predicha <- apply(test,1,function(row) colnames(test)[which.max(row)]) #Género con mayor probabilidad según el modelo
  datos <- as.data.frame(cbind(etiqueta_real,nueva_etiqueta,clase_predicha)) #Se construye el conjunto de datos
  
  #Precisión obtenida considerando solo la primer etiqueta
  datos$clasificacion1 <- ifelse(datos$clase_predicha==datos$etiqueta_real,
                                 "Bien clasificado",
                                 "Mal clasificado")
  criterio1 = round(nrow(filter(datos,datos$clasificacion1=="Bien clasificado")) / nrow(datos), 3)
  
  #Precisión obtenida considerando las dos primeras etiquetas
  datos$clasificacion2 <- ifelse(datos$clase_predicha==datos$etiqueta_real|datos$clase_predicha==datos$nueva_etiqueta,
                                 "Bien clasificado",
                                 "Mal clasificado")
  criterio2 = round(nrow(filter(datos,datos$clasificacion2=="Bien clasificado")) / nrow(datos), 3)
  
  print(paste("La precisión anterior fue: ", criterio1, sep=""))
  print(paste("La precisión con el nuevo criterio es: ", criterio2, sep=""))
}

  #Calcular la precisión obtenida al utilizar 
    #las dos primeras etiquetas obtenidas
comparar_precision2 = function(obj_entren, conjunto_prueba){
  test <- predict(obj_entren, newdata = conjunto_prueba, type="prob") #Predicciones como probabilidad
  
  etiqueta_real <- conjunto_prueba$etiqueta_genero #Etiquetas reales de los libros del conjunto de pruebas
  
    #Se obtienen los dos géneros con mayor probabilidad según el modelo:
  primer_clase <- apply(test,1,function(row) colnames(test)[which.max(row)])
  segunda_clase <- apply(test,1,function(row) colnames(test)[which.max(row[row!=max(row)])])
  datos <- as.data.frame(cbind(etiqueta_real,primer_clase,segunda_clase))
  
  #Precisión obtenida considerando solo la clase con mayor probabilidad
  datos$clasificacion1 <- ifelse(datos$etiqueta_real==datos$primer_clase,
                                 "Bien clasificado",
                                 "Mal clasificado")
  criterio1 = round(nrow(filter(datos,datos$clasificacion1=="Bien clasificado")) / nrow(datos),3)
  
  #Precisión obtenida considerando las dos clases con mayor probabilidad
  datos$clasificacion2 <- ifelse(datos$etiqueta_real==datos$primer_clase|datos$etiqueta_real==datos$segunda_clase,
                                 "Bien clasificado",
                                 "Mal clasificado")
  criterio2 = round(nrow(filter(datos,datos$clasificacion2=="Bien clasificado")) / nrow(datos),3)
  
  print(paste("La precisión anterior fue: ", criterio1, sep=""))
  print(paste("La precisión con el nuevo criterio es: ", criterio2, sep=""))
}

####Carga de archivos####
load(".../datos_entrenamientoA.Rdata")
load(".../datos_testeoA.Rdata")
load(".../trainA.Rdata")
load(".../datos_entrenamientoB.Rdata")
load(".../datos_testeoB.Rdata")
load(".../trainB.Rdata")
load(".../datos_entrenamientoC.Rdata")
load(".../datos_testeoC.Rdata")
load(".../trainC.Rdata")
load(".../datos_entrenamientoD.Rdata")
load(".../datos_testeoD.Rdata")
load(".../trainD.Rdata")
load(".../datos_entrenamientoE.Rdata")
load(".../datos_testeoE.Rdata")
load(".../trainE.Rdata")
load(".../datos_entrenamientoF.Rdata")
load(".../datos_testeoF.Rdata")
load(".../trainF.Rdata")
load(".../datos_entrenamientoG.Rdata")
load(".../datos_testeoG.Rdata")
load(".../trainG.Rdata")
load(".../datos_entrenamientoH.Rdata")
load(".../datos_testeoH.Rdata")
load(".../trainH.Rdata")

####Elegir directorio####
setwd("...")

####Establecer semilla####
semilla = 30062000

##############################
####Enfoque multi-etiqueta####
##############################

  #Escenario A
comparar_precision(trainA,data_testeoA)
  #Escenario B
comparar_precision(trainB,data_testeoB)
  #Escenario C
comparar_precision(trainC,data_testeoC)
  #Escenario D
comparar_precision(trainD,data_testeoD)
  #Escenario E
comparar_precision(trainE,data_testeoE)
  #Escenario F
comparar_precision(trainF,data_testeoF)
  #Escenario G
comparar_precision(trainG,data_testeoG)
  #Escenario H
comparar_precision(trainH,data_testeoH)

##################################
######Múltiples predicciones######
##################################

  #Escenario A
comparar_precision2(trainA,data_testeoA)
  #Escenario B
comparar_precision2(trainB,data_testeoB)
  #Escenario C
comparar_precision2(trainC,data_testeoC)
  #Escenario D
comparar_precision2(trainD,data_testeoD)
  #Escenario E
comparar_precision2(trainE,data_testeoE)
  #Escenario F
comparar_precision2(trainF,data_testeoF)
  #Escenario G
comparar_precision2(trainG,data_testeoG)
  #Escenario H
comparar_precision2(trainH,data_testeoH)

#####################################################
####Boosting - Solo se aplica para el escenario C####
#####################################################

set.seed(semilla)

  #Hiperparámetros a ajustar
grilla = expand.grid(
  nrounds = c(50,100,200,500,800,1000),
  max_depth = c(1,3,6),
  eta = c(0.01, 0.4),
  gamma = c(0.01, 0, 5),
  colsample_bytree = c(0.6,0.8),
  subsample = c(0.5, 0.75, 1),
  min_child_weight = c(0,1)
)

  #Hiperparámetros a ajustar
train_boost <- train(etiqueta_genero~ ., 
                method = "xgbTree", 
                data = datos_entrenamientoC,
                trControl = trainControl(method = "cv", 
                                         number = 5),
                tuneGrid = grilla
)

  #Evaluación del modelo
confusionMatrix(predict(train_boost, datos_testeoC), as.factor(datos_testeoC$etiqueta_genero))
