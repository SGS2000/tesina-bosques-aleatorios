#Se importan los PDFs al programa, se realiza el preprocesamiento y se construyen los atributos

####Librerías####
library(pdftools) 
library(tidytext) 
library(stopwords)
library(tidyverse)
library(stringi)
library(corpus) 
library(tm)

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

  #Importar pdf y guardarlo como caracter
importar_pdf= function(ruta){
  setwd(ruta)
  pdf_archivos <- list.files(pattern = "pdf$") 
  lista = lapply(pdf_archivos, pdf_text)
  names(lista) = pdf_archivos
  conjunto = stri_c_list(lista, sep = "") #Combinar archivos
  return(conjunto)
}

  #Obtener las palabras de un grupo de documentos
    #Lista de palabras vacías
borrar = c(get_stopwords(language = "es",source = "stopwords-iso")$word,"http","https","www",
           "net","com","org","edu","textos info","creativecommons","elejandria","freeditorial",
           "dominiopublico","lectulandia","elaleph","librodot","feedbooks","wikipedia",
           "wikisource","standardebooks","thevirtuallibrary","cinefantastico","blogspot",
           "bibliotecadigitaldumas","luarna","ciudadseva","planetalibro","kamparina","books",
           "gutenberg","webmaster","irc","web","fdl","idpf","nom","gnu","tcpdf","pgdp","d2g",
           "descargando","digitalizado","digitalizar","windows","linux","pdftohtml","xhtml","ueb",
           "verne","jverne","austen","lovecraft","carroll","irving","bierce",
           "usted","tan","haber","estar","ser","tener","tenés","sos","estás","vos",
           "decir","decis","decí","tené","sabés","sabé")


obtener_palabras = function(libro,genero){
  
  nuevo_texto = text_tokens(libro, stemmer = stem_list)%>% #Lematizar
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
  
  tibble(nuevo_texto) %>% #Contar palabras
    unnest_tokens(output = palabra, input = nuevo_texto) %>%
    #Se eliminan stopwords, simbolos no correspondientes, números, guiones y palabras de una o dos letras
    filter(!palabra %in% borrar, nchar(palabra) > 2, 
           grepl(pattern = "[\U0020-\U007F\U00A0-\U00FF]",palabra),
           !grepl(pattern = "_",palabra),
           !grepl(pattern = "www",palabra),
           !grepl(pattern = "[0-9]",palabra)) %>% 
    count(palabra) %>% 
    mutate(genero = genero, pje =  100*n/sum(n))
  
}

  #Obtener los pares de palabras (bigramas)
obtener_bigramas = function(documento, genero){
  nuevo_texto = documento %>% #Misma limpieza
    gsub("-\n\n", "", .)  %>% 
    gsub("-\n", "", .)  %>% 
    gsub("_", " ", .) %>% 
    gsub("a'", "a ", .) %>% 
    gsub("\\.", " ", .) %>% 
    gsub("tanto'que", "tanto que", .) %>% gsub("sus'estrados", "sus estrados", .) %>% 
    gsub("no'habría", "no habría", .) %>% gsub("ocasiones'no", "ocasiones no", .) %>% 
    gsub("com'o", "como", .) %>%
    gsub("dominio publico", " ", .) %>% gsub("dominio público", " ", .) %>% gsub("biblioteca org ar", " ", .) %>%
    gsub("orgullo y prejuicio", " ", .) %>% gsub("biblioteca d2g", " ", .) %>% gsub("daisy org", " ", .) %>%
    gsub("biblioteca virtual", " ", .) %>% gsub("La leyenda de Sleepy Hollow Washington Irving", " ", .) %>% 
    gsub("sandokán", "sandokan", .)  %>% gsub("des pués", "después", .)
  datos = as.data.frame(nuevo_texto) #Pasar a dataframe
  names(datos) = "texto"
  datos %>%
    unnest_tokens(bigrama, texto, token = "ngrams", n = 2) %>% #Separar en tokens
    separate(bigrama, c("p1", "p2"), sep = " ") %>% #Separa en dos palabras
    filter(nchar(p1) > 2,
           nchar(p2) > 2,
           !p1 %in% borrar, #Stopwords
           !p2 %in% borrar,
           grepl(pattern = "[\U0020-\U007F\U00A0-\U00FF]",p1), #Simbolos extranjeros
           grepl(pattern = "[\U0020-\U007F\U00A0-\U00FF]",p2),
           !grepl(pattern = "[0-9]",p1), #Números
           !grepl(pattern = "[0-9]",p2),
           !grepl(pattern = "_",p1),
           !grepl(pattern = "_",p2),
           !grepl(pattern = "www",p1), #URLs
           !grepl(pattern = "www",p2)) %>% #Se eliminan stopwords, simbolos no pertencientes, guiones y palabras de una o dos letras
    filter(!grepl(pattern = "dominio",p1)&!grepl(pattern = "público",p2)) %>%
    unite(bigrama, p1,p2, sep = " ") %>% #Junta el bigrama
    count(bigrama, sort = TRUE) %>% 
    mutate(genero = genero) %>% 
    filter(n>1) #Se eliminan los bigramas de frecuencia igual a 1
}


####Elegir directorio####
setwd("...")

####Filtro de palabras####
  #Se importa un diccionario y se lo usa para filtrar nombres propios
diccionario <- read_csv(".../diccionarioRAE.txt",col_names = FALSE)
names(diccionario) <- "palabra"

####Importar textos####
  #Colocar los textos en carpetas según género
corpus_aventura = importar_pdf(".../Libros/aventura")
corpus_cienciafic = importar_pdf(".../Libros/cienciaficcion")
corpus_fantastico = importar_pdf(".../Libros/fantastico")
corpus_fichist = importar_pdf(".../Libros/fichist")
corpus_policial = importar_pdf(".../Libros/policial")
corpus_romance = importar_pdf(".../Libros/romance")
corpus_terror = importar_pdf(".../Libros/terror")

####Construcción de escenarios####

  ##Escenario A##
palabras_aventura = obtener_palabras(corpus_aventura,"aventura")
palabras_cienciafic = obtener_palabras(corpus_cienciafic,"ciencia ficción")
palabras_fantastico = obtener_palabras(corpus_fantastico,"fantastico")
palabras_fichist = obtener_palabras(corpus_fichist,"ficcion historica")
palabras_policial = obtener_palabras(corpus_policial,"policial")
palabras_romance = obtener_palabras(corpus_romance,"romance")
palabras_terror = obtener_palabras(corpus_terror,"terror")

palabras = bind_rows(palabras_aventura,palabras_cienciafic,palabras_fantastico,
                     palabras_fichist,palabras_policial,palabras_romance,palabras_terror)

  ##Escenario B##
bigramas_aventura = obtener_bigramas(corpus_aventura, "aventura")
bigramas_cienciafic = obtener_bigramas(corpus_cienciafic, "ciencia ficción")
bigramas_fan = obtener_bigramas(corpus_fantastico, "fantastico")
bigramas_fichist = obtener_bigramas(corpus_fichist,"ficcion historica")
bigramas_policial = obtener_bigramas(corpus_policial, "policial")
bigramas_romance = obtener_bigramas(corpus_romance, "romance")
bigramas_terror = obtener_bigramas(corpus_terror, "terror")

bigramas =  bind_rows(bigramas_aventura,bigramas_cienciafic,bigramas_fan,bigramas_fichist,
                      bigramas_policial,bigramas_romance,bigramas_terror)

  ##Escenario C##
palabras_tfidf <- palabras %>% 
  bind_tf_idf(palabra, genero, n) %>% 
  arrange(desc(tf_idf))

  ##Escenario D##
bigramas_tfidf <- bigramas%>% 
  bind_tf_idf(bigrama, genero, n) %>% 
  arrange(desc(tf_idf)) 

  ##Escenario E##
palabras_filt = filter(palabras, palabras$palabra %in% diccionario$palabra)

  ##Escenario F##
bigramas_filt = bigramas %>%
  separate(bigrama, c("p1", "p2"), sep = " ") %>% #Separa en dos palabras
  filter(p1 %in% diccionario$palabra,
         p2 %in% diccionario$palabra) %>% #Se aplica filtro
  unite(bigrama, p1,p2, sep = " ") #Se vuelve a unir el bigrama

  ##Escenario G##
palabras_tfidf_filt = filter(palabras_tfidf, palabras_tfidf$palabra %in% diccionario$palabra)

  ##Escenario H##
bigramas_tfidf_filt = bigramas_tfidf %>%
  separate(bigrama, c("p1", "p2"), sep = " ") %>%
  filter(p1 %in% diccionario$palabra,
         p2 %in% diccionario$palabra) %>% 
  unite(bigrama, p1,p2, sep = " ")
