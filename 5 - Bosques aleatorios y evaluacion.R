#Se entrenan los modelos, se los evalúa en el conjunto de pruebas, se crean las
  #matrices de confusión y se calcula la importancia de las variables

####Librerías####
library(randomForest)
library(rpart)
library(caret)
library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(doParallel)

####Funciones####

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
entrenar_algoritmo = function(datos, arboles, num_var){
  cl <- makePSOCKcluster(detectCores())
  registerDoParallel(cl)
  
  entrenamiento <- train(etiqueta_genero~.,
                         data = datos,
                         method = "rf",
                         metric = "Accuracy",
                         tuneGrid = data.frame(.mtry = num_var),
                         trControl = trainControl(method = "repeatedcv",
                                                  number = 5,
                                                  repeats = 3,
                                                  search="grid"),
                         ntree = arboles
  )
  
  stopCluster(cl)
  return(entrenamiento)
}

  #Construir matriz de confusión
obtener_matriz_conf = function(predicciones, conjunto_prueba){
  mat_test = confusionMatrix(predicciones,
                              as.factor(conjunto_prueba$etiqueta_genero))
  mat_test = mat_test$table
  escala = prop.table(as.matrix(mat_test),2)
  mat_test = melt(mat_test)
  mat_test = cbind(mat_test, as.numeric(escala))
  colnames(mat_test) = c("Pred","Obs","Frec","escala")
  
  graf = ggplot(data =  mat_test, mapping = aes(x = Obs, y = Pred )) +
    scale_y_discrete(limits = rev(levels(mat_test$Pred))) +
    geom_tile(aes(fill = escala), colour = "white") +
    geom_text(aes(label=paste(Frec,"\n"," (",round(escala,2)*100,"%",")",sep="") ), size=4.25) +
    scale_fill_gradient(low = "orangered1", high = "green3") +
    xlab("Clase observada") + ylab("Clase predicha") +
    theme_classic() + theme(legend.position = "none")
  return(graf)
}

  #Calcular la importancia de las variables y obtener un gráfico
colores <- colorRampPalette(brewer.pal(8, "Set2"))(10)

importancia_var = function(obj_entren, cantidad=10){
  impor_train = varImp(obj_entren, scale = F)
  
  impor_train <- impor_train$importance %>% 
    mutate(termino=row.names(.)) %>%
    arrange(-Overall)
  colnames(impor_train) = c("importancia","termino")
  impor_train <- impor_train[1:cantidad,]
  
  graf = impor_train[order(impor_train$importancia,decreasing = T),] %>%
    ggplot(aes(x = reorder(termino, -importancia),y = importancia, fill=termino)) + 
    geom_bar(stat="identity") +
    scale_fill_manual(values = colores) +
    theme_minimal() +
    xlab("Término") + ylab("Importancia") +
    theme(axis.ticks.x = element_blank(),
          legend.position ='none')
  
  return(graf)
}

####Carga de archivos####
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

####Establecer semilla####
semilla = 30062000

########################################
####Aplicación de bosques aleatorios####
########################################

  ###Escenario A###

    #Separar en conjuntos
datosA = convertir_matriz(matriz_terminos, palabras_ord)
set.seed(semilla)
muestra <- sample(nrow(datosA),size=522,replace=FALSE)
datos_entrenamientoA <- datosA [muestra,]
datos_testeoA <- datosA[-muestra,]

    #Entrenamiento del modelo
set.seed(semilla)
trainA = entrenar_algoritmo(datos_entrenamientoA, 1000, 1500)

    #Evaluación en el conjunto de pruebas
set.seed(semilla)
testA <- predict(trainA,newdata=datos_testeoA)

    #Matriz de confusión
obtener_matriz_conf(testA, datos_testeoA)

    #Importancia de las variables
importancia_var(trainA)

  ###Escenario B###

    #Separar en conjuntos
datosB = convertir_matriz(matriz_bigramas, bigramas_ord, "bigrama", 5000)
set.seed(semilla)
muestra <- sample(nrow(datosB),size=522,replace=FALSE)
datos_entrenamientoB <- datosB[muestra,]
datos_testeoB <- datosB[-muestra,]

    #Entrenamiento del modelo
set.seed(semilla)
trainB = entrenar_algoritmo(datos_entrenamientoB, 1000, 500)

    #Evaluación en el conjunto de pruebas
set.seed(semilla)
testB <- predict(trainB,newdata=datos_testeoB)

    #Matriz de confusión
obtener_matriz_conf(testB, datos_testeoB)

    #Importancia de las variables
importancia_var(trainB)

  ###Escenario C###

    #Separar en conjuntos
datosC = convertir_matriz(matriz_tfidf, palabras_tfidf_ord)
set.seed(semilla)
muestra <- sample(nrow(datosC),size=522,replace=FALSE)
datos_entrenamientoC <- datosC[muestra,]
datos_testeoC <- datosC[-muestra,]

    #Entrenamiento del modelo
set.seed(semilla)
trainC = entrenar_algoritmo(datos_entrenamientoC, 1000, 1500)

    #Evaluación en el conjunto de pruebas
set.seed(semilla)
testC <- predict(trainC,newdata=datos_testeoC)

    #Matriz de confusión
obtener_matriz_conf(testC, datos_testeoC)

    #Importancia de las variables
importancia_var(trainC)

  ###Escenario D###

    #Separar en conjuntos
datosD = convertir_matriz(matriz_bg_tfidf, bigramas_tfidf_ord, "bigrama")
set.seed(semilla)
muestra <- sample(nrow(datosD),size=522,replace=FALSE)
datos_entrenamientoD <- datosD[muestra,]
datos_testeoD <- datosD[-muestra,]

    #Entrenamiento del modelo
set.seed(semilla)
trainD = entrenar_algoritmo(datos_entrenamientoD, 1000, 500)

    #Evaluación en el conjunto de pruebas
set.seed(semilla)
testD <- predict(trainD,newdata=datos_testeoD)

    #Matriz de confusión
obtener_matriz_conf(testD, datos_testeoD)

    #Importancia de las variables
importancia_var(trainD)

  ###Escenario E###

    #Separar en conjuntos
datosE = convertir_matriz(matriz_filt, palabras_filt_ord)
set.seed(semilla)
muestra <- sample(nrow(datosE),size=522,replace=FALSE)
datos_entrenamientoE <- datosE[muestra,]
datos_testeoE <- datosE[-muestra,]

    #Entrenamiento del modelo
set.seed(semilla)
trainE = entrenar_algoritmo(datos_entrenamientoE, 500, 3000)

    #Evaluación en el conjunto de pruebas
set.seed(semilla)
testE <- predict(trainE,newdata=datos_testeoE)

    #Matriz de confusión
obtener_matriz_conf(testE, datos_testeoE)

    #Importancia de las variables
importancia_var(trainE)

  ###Escenario F###

    #Separar en conjuntos
datosF = convertir_matriz(matriz_bg_filt, bigramas_filt_ord, "bigrama")
set.seed(semilla)
muestra <- sample(nrow(datosF),size=522,replace=FALSE)
datos_entrenamientoF <- datosF[muestra,]
datos_testeoF <- datosF[-muestra,]

    #Entrenamiento del modelo
set.seed(semilla)
trainF = entrenar_algoritmo(datos_entrenamientoF, 500, 500)

    #Evaluación en el conjunto de pruebas
set.seed(semilla)
testF <- predict(trainF,newdata=datos_testeoF)

    #Matriz de confusión
obtener_matriz_conf(testF, datos_testeoF)

    #Importancia de las variables
importancia_var(trainF)

  ###Escenario G###

    #Separar en conjuntos
datosG = convertir_matriz(matriz_tfidf_filt.Rdata, palabras_tfidf_filt_ord)
set.seed(semilla)
muestra <- sample(nrow(datosG),size=522,replace=FALSE)
datos_entrenamientoG <- datosG[muestra,]
datos_testeoG <- datosG[-muestra,]

    #Entrenamiento del modelo
set.seed(semilla)
trainG = entrenar_algoritmo(datos_entrenamientoG, 500, 500)

    #Evaluación en el conjunto de pruebas
set.seed(semilla)
testG <- predict(trainG,newdata=datos_testeoG)

    #Matriz de confusión
obtener_matriz_conf(testG, datos_testeoG)

    #Importancia de las variables
importancia_var(trainG)

  ###Escenario H###

    #Separar en conjuntos
datosH = convertir_matriz(matriz_bg_tfidf_filt, bigramas_tfidf_filt_ord, "bigrama", 5000)
set.seed(semilla)
muestra <- sample(nrow(datosH),size=522,replace=FALSE)
datos_entrenamientoH <- datosH[muestra,]
datos_testeoH <- datosH[-muestra,]

    #Entrenamiento del modelo
set.seed(semilla)
trainH = entrenar_algoritmo(datos_entrenamientoH, 500, 3500)

    #Evaluación en el conjunto de pruebas
set.seed(semilla)
testH <- predict(trainH,newdata=datos_testeoH)

    #Matriz de confusión
obtener_matriz_conf(testH, datos_testeoH)

    #Importancia de las variables
importancia_var(trainH)

#########################################
####Aplicación de árboles de decisión####
#########################################

  ###Escenario A###
arbolA <- rpart(etiqueta_genero ~ ., data = trainA)
pred_A = predict(arbolA, testA, type="class")
confusionMatrix(pred_A,as.factor(testA$etiqueta_genero))

  ###Escenario B###
arbolB <- rpart(etiqueta_genero ~ ., data = trainB)
pred_B = predict(arbolB, testB, type="class")
confusionMatrix(pred_B,as.factor(testB$etiqueta_genero))

  ###Escenario C###
arbolC <- rpart(etiqueta_genero ~ ., data = trainC)
pred_C = predict(arbolC, testC, type="class")
confusionMatrix(pred_C,as.factor(testC$etiqueta_genero))

  ###Escenario D###
arbolD <- rpart(etiqueta_genero ~ ., data = trainD)
pred_D = predict(arbolD, testD, type="class")
confusionMatrix(pred_D,as.factor(testD$etiqueta_genero))

  ###Escenario E###
arbolE <- rpart(etiqueta_genero ~ ., data = trainE)
pred_E = predict(arbolE, testE, type="class")
confusionMatrix(pred_E,as.factor(testE$etiqueta_genero))

  ###Escenario F###
arbolF <- rpart(etiqueta_genero ~ ., data = trainF)
pred_F = predict(arbolF, testF, type="class")
confusionMatrix(pred_F,as.factor(testF$etiqueta_genero))

  ###Escenario G###
arbolG <- rpart(etiqueta_genero ~ ., data = trainG)
pred_G = predict(arbolG, testG, type="class")
confusionMatrix(pred_G,as.factor(testG$etiqueta_genero))

  ###Escenario H###
arbolH <- rpart(etiqueta_genero ~ ., data = trainH)
pred_H = predict(arbolH, testH, type="class")
confusionMatrix(pred_H,as.factor(testH$etiqueta_genero))