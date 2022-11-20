# Código utilizado en la tesina
Se divide el código en seis partes para una mejor organización. A continuación se describe el contenido de cada una:
1. Contiene el código para descargar la mayoría de los libros utilizados en la tesina (ver detalles en la sección 3.1)
2. Código para la importación de los PDFs, el preprocesamiento y la construcción de atributos (secciones 3.2.1 a 3.2.3)
3. Código correspondiente a la construcción de matrices DTM y el procedimiento de reducción de dimensionalidad (secciones 3.2.4 y 3.2.5)
4. Contiene el código para realizar el ajuste de los hiperparámetros (sección 3.3.3)
5. Contiene el código para aplicar los algoritmos de árboles de decisión y bosques aleatorios (secciones 3.3.1 y 3.3.2) y para evaluar sus resultados (secciones 3.3.4 y 3.3.5)
6. Contiene los códigos para los enfoques alternativos abordados en la tesina (sección 3.3.6) y para la aplicación de boosting para el escenario C (sección 3.3.7)

Se incluyen dos archivos necesarios para la ejecución del código:
* *diccionarioRAE.txt*: Palabras presentes en el *Diccionario de la lengua española (23ª ed.)* de la Real Academia Española. Se utiliza para crear los escenarios con filtro en el archivo 2.
* *lematizacion.txt*: Lista de lemas en español, creada por Michal Měchura ([ver aquí](https://github.com/michmech/lemmatization-lists "GitHub")). Utilizada para lematizar utilizando la función *stem_list()* en los archivos 2 y 3.
