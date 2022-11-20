#Se construyen las matrices DTM y se evalúa la dimensión de reducción de sus dimensiones

####Librerias####
library(pdftools) 
library(tidytext) 
library(stopwords)
library(tidyverse)
library(stringi)
library(corpus) 
library(quanteda)
library(plotly)

####Carga de archivos####
load(".../palabras.Rdata")
load(".../bigramas.Rdata")
load(".../palabras_tfidf.Rdata")
load(".../bigramas_tfidf.Rdata")
load(".../palabras_filt.Rdata")
load(".../bigramas_filt.Rdata")
load(".../palabras_tfidf_filt.Rdata")
load(".../bigramas_tfidf_filt.Rdata")

####Funciones####

  #Lematizador#
lematizacion <- read_table2(".../lematizacion.txt",locale = locale(encoding = "ISO-8859-1"), 
                            col_names = FALSE, col_types = cols(`1` = col_character(), 
                                                                X1 = col_character()))
names(lematizacion) <- c("stem", "term")

stem_list <- function(term) {
  i <- match(term, lematizacion$term)
  if (is.na(i)) {
    stem <- term
  } else {
    stem <- lematizacion$stem[[i]]
  }
  stem}

  #Importar pdfs y guardarlo como lista
importar_pdf= function(ruta){
  setwd(ruta)
  pdf_archivos <- list.files(pattern = "pdf$")
  lista = lapply(pdf_archivos, pdf_text)
  names(lista) = pdf_archivos
  return(lista)
}

  #Obtener palabras que coincidan con las de un archivo
buscar_palabras = function(texto,titulo,archivo){
  texto = stri_c_list(texto, sep = "")
  nuevo_texto = text_tokens(texto, stemmer = stem_list)%>% #Lematizar
    unlist()%>% 
    paste(collapse=' ') %>%
    #Se corrigen errores comunes/de codificación y se elimina texto ajeno a la obra.
    gsub("-\n\n", "", .) %>%
    gsub("-\n", "", .) %>%
    gsub("_", " ", .) %>%
    gsub("a'", "a ", .) %>% 
    gsub(" 'm", " m", .) %>% gsub("\\.", " ", .) %>% 
    gsub("tanto'que", "tanto que", .) %>% gsub("sus'estrados", "sus estrados", .) %>% 
    gsub("no'habría", "no habría", .) %>% gsub("ocasiones'no", "ocasiones no", .) %>% 
    gsub("	com'o", "como", .) %>%
    gsub("dominio publicar", " ", .) %>% gsub("dominio público", " ", .) %>% gsub("biblioteca org ar", " ", .) %>%
    gsub("orgullo y prejuicio", " ", .) %>% gsub("biblioteca d2g", " ", .) %>% gsub("daisy org", " ", .) %>%
    gsub("biblioteca virtual", " ", .) %>% gsub("La leyenda de Sleepy Hollow Washington Irving", " ", .) %>% 
    gsub("sandokán", "sandokan", .) %>% gsub("des pués", "después", .)
  
  tibble(nuevo_texto)  %>%
    unnest_tokens(output = termino, input = nuevo_texto) %>%
    filter(termino %in% archivo)%>%
    count(termino) %>% 
    mutate(documento = titulo, pje =  100*n/sum(n)) 
  
}

  #Obtener bigramas que coincidan con los de un archivo
buscar_bigramas = function(texto,titulo,archivo){
  texto = stri_c_list(texto, sep = "")
  nuevo_texto = texto %>% #Misma limpieza
    gsub("-\n\n", "", .) %>%
    gsub("-\n", "", .) %>%
    gsub("_", " ", .) %>% 
    gsub("a'", "a ", .) %>% 
    gsub("\\.", " ", .) %>% 
    gsub("tanto'que", "tanto que", .) %>% gsub("sus'estrados", "sus estrados", .) %>% 
    gsub("no'habría", "no habría", .) %>% gsub("ocasiones'no", "ocasiones no", .) %>% 
    gsub("com'o", "como", .) %>%
    gsub("dominio publico", " ", .) %>% gsub("dominio público", " ", .) %>% gsub("biblioteca org ar", " ", .) %>%
    gsub("orgullo y prejuicio", " ", .) %>% gsub("biblioteca d2g", " ", .) %>% gsub("daisy org", " ", .) %>%
    gsub("biblioteca virtual", " ", .) %>% gsub("La leyenda de Sleepy Hollow Washington Irving", " ", .) %>% 
    gsub("sandokán", "sandokan", .) %>% gsub("des pués", "después", .)
  datos = as.data.frame(nuevo_texto) #Pasar a dataframe
  names(datos) = "texto"
  datos %>%
    unnest_tokens(bigrama, texto, token = "ngrams", n = 2) %>% #Separar en tokens
    filter(bigrama %in% archivo) %>%
    count(bigrama, sort = TRUE) %>% 
    mutate(documento = titulo)
}

  #Obtener terminos de los textos de una lista
obtener_terminos = function(lista, archivopalabras){
  terminos = buscar_palabras(texto=lista[1],titulo=as.character(names(lista[1])),archivopalabras)
  for (i in 2:length(lista)) {
    nuevo = buscar_palabras(texto=lista[i],titulo=as.character(names(lista[i])),archivopalabras)
    terminos = rbind(terminos,nuevo)
  }
  return(terminos)
}

  #Obtener bigramas de los textos de una lista
obtener_bigramas = function(lista, archivobigramas){
  bigramas = buscar_bigramas(texto=lista[1],titulo=as.character(names(lista[1])),archivobigramas)
  for (i in 2:length(lista)) {
    nuevo = buscar_bigramas(texto=lista[i],titulo=as.character(names(lista[i])),archivobigramas)
    bigramas = rbind(bigramas,nuevo)
  }
  return(bigramas)
}

####Elegir directorio####
setwd("...")

####Importar textos####
lista_aventura = importar_pdf(".../aventura")
lista_cienciafic = importar_pdf(".../cienciaficcion")
lista_fantastico = importar_pdf(".../fantastico")
lista_fichist = importar_pdf(".../fichist")
lista_policial = importar_pdf(".../policial")
lista_romance = importar_pdf(".../romance")
lista_terror = importar_pdf(".../terror")

####Obtener palabras y bigramas por documento####
base_aventura = obtener_terminos(lista_aventura,palabras$palabra)
base_cienciafic = obtener_terminos(lista_cienciafic,palabras$palabra)
base_fantastico = obtener_terminos(lista_fantastico,palabras$palabra)
base_fichist = obtener_terminos(lista_fichist,palabras$palabra)
base_policial = obtener_terminos(lista_policial,palabras$palabra)
base_terror = obtener_terminos(lista_terror,palabras$palabra)
base_romance = obtener_terminos(lista_romance,palabras$palabra)

base_total = rbind(base_aventura,base_cienciafic,base_fantastico,base_fichist,base_policial,
                   base_romance,base_terror)

base_bg_aventura = obtener_bigramas(lista_aventura,bigramas$bigrama)
base_bg_cienciafic = obtener_bigramas(lista_cienciafic,bigramas$bigrama)
base_bg_fantastico = obtener_bigramas(lista_fantastico,bigramas$bigrama)
base_bg_fichist = obtener_bigramas(lista_fichist,bigramas$bigrama)
base_bg_policial = obtener_bigramas(lista_policial,bigramas$bigrama)
base_bg_romance = obtener_bigramas(lista_romance,bigramas$bigrama)
base_bg_terror = obtener_bigramas(lista_terror,bigramas$bigrama)

base_bg_total = rbind(base_bg_aventura,base_bg_cienciafic,base_bg_fantastico,base_bg_fichist,base_bg_policial,
                      base_bg_romance,base_bg_terror)

####Construcción de las matrices####

  #Escenario A
matriz_terminos = cast_dfm(base_total, document=documento, term=termino, value=n)

  #Escenario B
matriz_bigramas = cast_dfm(base_bg_total, document=documento, term=bigrama, value=n)

  #Escenario C
base_tfidf = base_total %>%
  bind_tf_idf(termino, documento, n)
matriz_tfidf = cast_dfm(base_tfidf, document=documento, term=termino, value=tf_idf)

  #Escenario D
base_bg_tfidf = base_bg_total %>%
  bind_tf_idf(bigrama, documento, n)
matriz_bg_tfidf = cast_dfm(base_bg_tfidf, document=documento, term=bigrama, value=tf_idf)

  #Escenario E
base_filt = base_total %>%
  filter(termino %in% palabras_filt$palabra)
matriz_filt = cast_dfm(base_filt, document=documento, term=termino, value=n)

  #Escenario F
base_bg_filt = base_bg_total %>%
  filter(bigrama %in% bigramas_filt$bigrama)
matriz_bg_filt = cast_dfm(base_bg_filt, document=documento, term=bigrama, value=n)

  #Escenario G
base_tfidf_filt = base_total %>%
  filter(termino %in% palabras_tfidf_filt$palabra) %>%
  bind_tf_idf(termino, documento, n)
matriz_tfidf_filt = cast_dfm(base_tfidf_filt, document=documento, term=termino, value=tf_idf)

  #Escenario H
base_bg_tfidf_filt = base_bg_total %>%
  filter(bigrama %in% bigramas_tfidf_filt$bigrama) %>%
  bind_tf_idf(bigrama, documento, n)
matriz_bg_tfidf_filt = cast_dfm(base_bg_tfidf_filt, document=documento, term=bigrama, value=tf_idf)


####Reducción de la dimensionalidad####

  #Escenario A
palabras_ord = subset(palabras, select = c(palabra,n) )

palabras_ord = palabras_ord %>% #Se fusionan celdas
  group_by(palabra)%>% 
  summarise(across(everything(), sum))
palabras_ord = palabras_ord[order(-palabras_ord$n),]

palabras_ord$orden = 1:nrow(palabras_ord)
ggplotly(ggplot(palabras_ord, aes(x=orden, y = n))
         +geom_line()
)

  #Escenario B
bigramas_ord = subset(bigramas, select = c(bigrama,n) )

bigramas_ord = bigramas_ord %>%
  group_by(bigrama)%>% 
  summarise(across(everything(), sum))
bigramas_ord = bigramas_ord[order(-bigramas$n),]

bigramas_ord$orden = 1:nrow(bigramas_ord)
ggplotly(ggplot(bigramas_ord, aes(x=orden, y = n))
         +geom_line()
) 

  #Escenario C
palabras_tfidf_ord = subset(palabras_tfidf, select = c(palabra,n,tf_idf) )

palabras_tfidf_ord = palabras_tfidf_ord %>%
  group_by(palabra)%>% 
  summarise(across(everything(), sum))
palabras_tfidf_ord = palabras_tfidf[order(-palabras_tfidf$tf_idf),]

palabras_tfidf_ord$orden = 1:nrow(palabras_tfidf_ord)
ggplotly(ggplot(palabras_tfidf_ord, aes(x=orden, y = tf_idf))
         +geom_line()
)

  #Escenario D
bigramas_tfidf_ord = subset(bigramas_tfidf, select = c(bigrama,n,tf_idf) )

bigramas_tfidf_ord  = bigramas_tfidf_ord  %>% #Se fusionan celdas
  group_by(bigrama)%>% 
  summarise(across(everything(), sum))
bigramas_tfidf_ord  = bigramas_tfidf_ord[order(-bigramas_tfidf_ord$tf_idf),]

bigramas_tfidf_ord$orden = 1:nrow(bigramas_tfidf_ord)
ggplotly(ggplot(bigramas_tfidf_ord, aes(x=orden, y = tf_idf))
         +geom_line()
)

  #Escenario E
palabras_filt_ord = subset(palabras_filt, select = c(palabra,n) )

palabras_filt_ord = palabras_filt_ord %>%
  group_by(palabra)%>% 
  summarise(across(everything(), sum))
palabras_filt_ord = palabras_filt_ord[order(-palabras_filt_ord$n),]

palabras_filt_ord$orden = 1:nrow(palabras_filt_ord)
ggplotly(ggplot(palabras_filt_ord, aes(x=orden, y = n))
         +geom_line()
)

  #Escenario F
bigramas_filt_ord = subset(bigramas_filt, select = c(bigrama,n) )

bigramas_filt_ord  = bigramas_filt_ord  %>% #Se fusionan celdas
  group_by(bigrama)%>% 
  summarise(across(everything(), sum))
bigramas_filt_ord  = bigramas_filt_ord[order(-bigramas_filt_ord$n),]

bigramas_filt_ord$orden = 1:nrow(bigramas_filt_ord)
ggplotly(ggplot(bigramas_filt_ord, aes(x=orden, y = n))
         +geom_line()
)

  #Escenario G
palabras_tfidf_filt_ord = subset(palabras_tfidf_filt, select = c(palabra,n,tf_idf) )

palabras_tfidf_filt_ord = palabras_tfidf_filt_ord %>%
  group_by(palabra)%>% 
  summarise(across(everything(), sum))
palabras_tfidf_filt_ord = palabras_tfidf_filt_ord[order(-palabras_tfidf_filt_ord$tf_idf),]

palabras_tfidf_filt_ord$orden = 1:nrow(palabras_tfidf_filt_ord)
ggplotly(ggplot(palabras_tfidf_filt_ord, aes(x=orden, y = tf_idf))
         +geom_line()
)
  #Escenario H
bigramas_tfidf_filt_ord = subset(bigramas_tfidf_filt, select = c(bigrama,n,tf_idf) )

bigramas_tfidf_filt_ord  = bigramas_tfidf_filt_ord  %>% #Se fusionan celdas
  group_by(bigrama)%>% 
  summarise(across(everything(), sum))
bigramas_tfidf_filt_ord  = bigramas_tfidf_filt_ord[order(-bigramas_tfidf_filt_ord$tf_idf),]

bigramas_tfidf_filt_ord$orden = 1:nrow(bigramas_tfidf_filt_ord)
ggplotly(ggplot(bigramas_tfidf_filt_ord, aes(x=orden, y = tf_idf))
         +geom_line()
)