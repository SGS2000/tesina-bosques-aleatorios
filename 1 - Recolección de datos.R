#Se buscan enlaces de distintos libros y se descargan.

####Librerías####
library(xml2)
library(rvest)
library(tidyverse)
library(gutenbergr)

####Elegir directorio####
setwd("...")

###################################
######Descargas de Elejandria######
###################################


#Elegir género en https://www.elejandria.com/categorias/literatura-y-ficcion/13#
dir1 =  read_html("https://www.elejandria.com/categorias/literatura-y-ficcion/policiaca-negra-y-suspense/131") #Ejemplo

dirlibros = dir1 %>% 
  html_nodes(".primary-text-color") %>% 
  html_attr('href')

dir2 = na.omit(dirlibros) %>%
  lapply(. %>%
           read_html() %>%
           html_nodes(".btn.btn-info.mt-3.download-link.col") %>% 
           html_attr('href') %>%
           head(1)
  )

dir2 = dir2[lapply(dir2,length)>0]

descarga = as.character(dir2) %>%
  lapply(. %>%
           read_html() %>%
           html_nodes(".btn.btn-info.offset-top.btn-lg.download-link") %>% 
           html_attr('href')%>%
           head(10)
  )

#Descarga
for (i in 1:length(descarga)) {
  download.file(as.character(descarga[i]),paste("libro",i,".pdf",sep=""), mode="wb")
}

#####################################
######Descargas de freeditorial######
#####################################

#Elegir género en https://freeditorial.com/es#
dir1 =  read_html("https://freeditorial.com/es/books/collection/novela-de-misterio-y-suspense?page=2") #Ejemplo

dirlibros = dir1 %>% 
  html_nodes(".more") %>% 
  html_attr('href')

dir2 = paste("https://freeditorial.com",dirlibros,"/downloadbookepub?format2=pdf",sep="") 

#Descarga
for (i in 1:length(dir2)) {
  download.file(dir2[i],paste("libro",i,".pdf",sep=""), mode="wb")
}

############################################
######Descargas de Project Gutenberg########
############################################

#Buscar libros en español
libros <- gutenberg_metadata %>% 
  filter(language == "es") %>% 
  left_join(gutenberg_subjects)  

#Filtrar libros de ficción
libros2 = libros %>% 
  filter(grepl("fiction", subject, ignore.case = TRUE))
  
#Descarga
  #Escribir ID del libro deseado
ID = 9980 #Ejemplo
nombre = libros2 %>%
  filter(gutenberg_id == ID) %>%
  .[1,2] %>%
  as.character()

dir = paste("https://www.gutenberg.org/ebooks/",ID,".epub.noimages",sep="") 

  #Descargar (como EPUB)
download.file(dir,paste(nombre,".epub",sep=""), mode="wb")


#############################################
#########Descargas de Ganso y Pulpo##########
#############################################

#Lista de géneros
dir = read_html("https://gansoypulpo.com/autor")
                      
generos = dir %>% 
  html_nodes(".sf-input-select")%>%
  .[3]%>%
  html_nodes("option")%>%
  html_attr('value')

generos #Buscar género deseado

#Buscar enlaces
  #Colocar posición del género deseado en el objeto "generos"
posicion = 8 #Ejemplo 
link = paste("https://gansoypulpo.com/autor/?_sft_genero=",generos[posicion],"&sf_paged=1", sep="")
link2 = link

  #Número de páginas
contador = 0
while (!identical(link2, character(0))) {
  link2 = read_html(link2) %>%
    html_nodes(".nextpostslink") %>% 
    html_attr('href')
  contador = contador + 1
}

  #Obtener enlaces de todas las páginas
enlaces = c()
for (i in 1:contador) {
  link = paste("https://gansoypulpo.com/autor/?_sft_genero=",generos[posicion],"&sf_paged=",i, sep="")
  libros = read_html(link) %>%
    html_nodes(".title") %>% 
    html_nodes("a") %>% 
    html_attr('href')
  enlaces = c(enlaces,libros)
}

  #Descargar (como EPUB)
for (i in 1:length(enlaces)) {
  descarga = read_html(enlaces[i])%>%
    html_nodes(".download") %>%
    html_nodes("a") %>% 
    html_attr('href')
  titulo = read_html(enlaces[i])%>%
    html_nodes(".download") %>%
    html_nodes("a") %>% 
    html_attr('title')
  download.file(descarga,paste(titulo,".epub",sep=""), mode="wb")
}


  