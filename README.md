---
title: "Asociación e independencia entre variables"
author: "Jenny Barrera / Antonio Caicedo"
date: "18/10/2020"
output: html_document
---

## Punto 1. CARGA DE DATOS Y CONTEXTO

## Importar datos a R

Ubicamos nuestro archivo de datos en una carpeta denominada data, dentro del nuevo proyecto creado.

## Información sobre el conjunto de datos

Los datos corresponden a información de biodiversidad marina relacionada con eventos de muestreo realizados anualmente en varias estaciones ubicadas en la costa oeste de Estados Unidos, que buscan cuantificar las condiciones ambientales y la biota encontrada a lo largo de la corriente de California. Los individuos capturados e identificados fueron contablizados. También se presentan datos de variables físicas como la temperatura del mar, profundidad y distancia a la costa. 

## Fuente: 
Auth, Toby; 03/20/2017. NOAA Fisheries Northwest Fisheries Science Center. Juvenile Fish Data: Prerecruit Survey Trawl Data Catch (https://www.webapps.nwfsc.noaa.gov/apex/parrdata/inventory/tables/table/prerecruit_survey_trawl_data_catch).

## Variables:

- cruise: Nombre del crucero
- year: Año del crucero
- transect: Transecto
- station: Número de la estación
- distance_from_shore: Distancia más cercana a la costa en kilómetros
- offshore_distance: Distancia aproximada desde la costa hacia mar adentro. La categoría alta significa que el punto de toma de muestra está más alejado de la costa en        comparación con los demás.
- start_depth: Profundidad en metros del fondo al inicio de la toma de muestra. Puede dar una idea de la profundidad de la estación. 
- depth: Variable profundidad inicial transformada en categórica ordinal. Da una idea de la profundidad de la estación. 
- surface_temperature: Temperatura superficial del mar en grados centígrados medida al inicio de la colecta.
- taxon: Nombre científico de los organismos capturados a nivel de especie, género o familia. 
- maturity: Representa la edad de los individuos o su estado de madurez.
- individuals: Representa el número de individuos capturados 
- comments: Comentarios realizados sobre los organismos capturados 


## Cargar librerías

```{r}
library("tidyverse")
library("readxl")
library("ggplot2")
library('GGally')
library("lubridate")
```

## Importar datos

```{r}
read_xlsx(
  path = "data/data_taller.xlsx", 
  sheet="data"
) -> vivos

## Revisar la estructura de los datos
str(vivos)
## Le decimos a R que vamos a trabajar con esos datos y se los ponemos "en primer plano"
attach(vivos)
```

## Punto 2. LIMPIEZA Y DETECCIÓN DE DATOS ATÍPICOS

## 2.1 Limpieza de los datos

En la práctica la calidad de los datos puede estar afectada por los procesos de captura, sistematización y distribución. **Siempre** hay que verificar la calidad de nuestros datos.

- Nombrado adecuado de las variables
- Datos faltantes
- Valores duplicados

### Nombrado adecuado de las variables
- [Cómo nombrar las cosas](http://www2.stat.duke.edu/~rcs46/lectures_2015/01-markdown-git/slides/naming-slides/naming-slides.pdf)

#Validamos los nombres de las variables

```{r}
names(vivos)
```

## Dimensionalidad de los datos

```{r}
dim(vivos)
```


### Datos faltantes

```{r}
sapply(vivos, function(x) sum(is.na(x)))
```

La variable surface_temperatura presenta 22 datos perdidos. Por tal motivo procedemos a probar si la pérdida de datos es aleatoria.

### Prueba para probar si la pérdida de datos es aleatoria 

Asignamos un nuevo objeto con las variables numéricas:

```{r}
select(vivos, distance_from_shore,start_depth,surface_temperature,individuals)->variables_numericas

```

```{r}
# Cargamos el paquete MissMech
library("MissMech")

# Testeamos si nuestros datos están perdidos de forma MCAR
# Hipótesis nula: no existen diferencias en la forma en que se pierden datos entre las variables
# Dicho de otra manera, los datos están perdidos de forma aleatoria
# Hipótesis alterna: sí existen diferencias (hay patrones de pérdida de datos)
TestMCARNormality(variables_numericas)
```
De la prueba anterior se concluye que los datos no fueron perdidos de forma aleatotia.

### Obtenemos el nuevo dataset luego de haber eliminado los 22 datos faltantes de la temperatura.

Esto se hizo teniendo en cuenta que los datos no fueron perdidos de forma aleatoria.

```{r}
vivos[-(1765:1786),]-> data_dep1
```

### Valores duplicados

Se eliminarán duplicados en toda la fila, pues no se espera dos registros iguales.

Podemos buscar y eliminar duplicados basados en toda la fila.

```{r}
distinct(data_dep1)
```
Se observan que se mantienen los mismos registros, es decir no hay valores duplicados.


## 2.2 Datos atípicos.

```{r}
# Estadístico de resumen para la variable objetivo
summary(data_dep1$individuals)
```

Se observa un valor máximo extremo, validaremos posibles valores outliners

#
```{r}
# Boxplot de una variable
boxplot(data_dep1$individuals)

# Valores de potenciales outliers
boxplot.stats(data_dep1$individuals)$out

```
Se observa una gran varianción entre la variable objetivo "individuals" sin embargo los valores los interpretamos como lógicos ya que esta variable hace referencia al número de individuos capturados por grupo biológico. Sabemos que contamos con grupos muy diversos por ejemplo, peces, medusas, camarones, krill entre otros, que podrían llegar a ser poco o muy abundantes.

Otro método para evaluar los datos atípicos es la distancia de cook. En ese sentido, diremos que tendremos una fila que representa una observación atípica si el valor observado es superior a 4 veces el promedio (no es un límite estricto).

```{r}
modelo.regresion <- lm(individuals ~ ., data=data_dep1)
cooksd <- cooks.distance(modelo.regresion)
```
```{r}
# Gráfica de la distancia de Cook
plot(cooksd, pch="*", cex=2, main="Observaciones Atípicas por distancia de Cook") 
# Superponemos el límite definido
abline(h = 4*mean(cooksd, na.rm=T), col="red")  
# Agregamos etiquetas de identificación para observaciones atípicas
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red") 
```
```{r}
## Número de filas a revisar
atipicas <- which((cooksd>4*mean(cooksd, na.rm=T)) == "TRUE")
atipicas
```
Con relación a este análisis se obtuvieron 9 filas para revisar, las cuales correspondieron al grupo Euphausiidae conocido comúnmente como Krill, el cual presentó abundancias superiores a los 121000 individuos y alcanzó un máximo de 1017133. Para un mejor análisis se sugiere verificar el método de muestreo y las unidades que se reportan, ya que se registran como #número de individuos capturados, pero no se sabe si estos son datos brutos o estandarizados. Para el objetivo del presente taller, procederemos a trabajar con los datos obtenidos, sin depurar éstos valores. Supondremos que son número de individuos y se encuentran en la misma unidad.

## Punto 3: ANÁLISIS DESCRIPTIVO 

### Cargamos paquetes y revisamos estructura

```{r}
library("tidyverse")
library("skimr")
library("summarytools")
library("modeest")
library("ggpubr")
```

```{r}

## Revisar la estructura de los datos
str(data_dep1)
```


```{r}
## Convertimos variables respectivas a factores
factores <- c("year", "station")
data_dep1 %>% mutate_at(factores,factor) -> data_factor
```

### Resumen numérico para todo el conjunto de datos 

El método **summary()** que trae por defecto R nos brinda estadísticas de resumen para cada una de las variables de nuestro conjunto de datos.

```{r}
## Resumen básico de datos
summary(data_factor)
```

Un método que resume la estadística descriptiva de las variables numéricas:

```{r}
summarytools::descr(data_factor)
```
En total se obtuvieron 2430 registros, 890 para el año 2011, 317 para el 2013, 617 para el 2014 y 606 para el año 2015. Con base en estos análisis, podemos decir que nuestra variable de interes "individuals" presentó un promedio de 3025 individuos capturados con un rango de variación entre 1 como el valor mínimo y 1017133 como el valor máximo con una desviación estándard de 34459.99. El valor central o la mediana fué de 4 individuos capturados. Por su parte la temperatura superficial del mar tuvo un promedio de 13.98 °C con una variación entre 2.4 y 17.4, la desviación estándard fue de 1.70. La profundidad al inicio del muestreo tuvo un promedio de 708.15 metros con st de 799.58 y un rango de variación entre 57 y 3100 metros. La distancia a la costa tuvo un promedio de 46.91 kilómetros con un rango de variación entre 6.7 y 176.3 kilómetros. La desviación estándard fue de 24.8.



También podemos sacar estadísticos descriptivos para la variable cualitativa "taxon" que se refiere a los diferentes grupos de organismos.

```{r}
# Moda
mfv(data_factor$taxon)

# Frecuencias
table(data_factor$taxon)

# Proporciones
prop.table(table(data_factor$taxon))

```

Con base en éste análisis, podemos decir que el taxón con mayor frecuencia de aparición fue el grupo "Euphausiidae" con 156 ocurrencias que representaron una proporción de 0.06 (6%).  



### Resumen gráfico

#Análisis univariado

A. Exploramos nuestra variable de interés "individuals" de tipo continua con un diagrama de puntos, un histograma y un boxplot.

Diagrama de puntos

```{r}
plot(data_factor$individuals)
```

Histograma


```{r}
hist(data_factor$individuals)

Boxplot

```{r}
boxplot(data_factor$individuals)
```
Redimensionando los datos,

```{r}
data_factor[data_factor$individuals < 20, ]-> data_factor_red
boxplot(data_factor_red$individuals)
```

Con base en las anteriores gráficas, se evidencia que la mayoría de observaciones registraron un conteo menor a los 200000 individuos capturados, mientras que unos pocos datos fueron mayores a 200000 y solo un registro superó el valor de 1000000 individuos capturados. El histograma nos muestra el agrupamiento de la mayoría de los datos con valores bajos menores a los 200000 individuos capturados. Al redimensionar el boxplot se evidencia que el valor de la mediana es muy pequeño (4) y el 50% de la información está agrupado en valores de conteo bajos menores a 5 individuos capturados.


B. Variable surface_temperature

```{r}
boxplot(data_factor$surface_temperature)
```

Al construir un boxplot con esta variable el 50% de la información se agrupó entre valores de 13°C y 15°C, además de evidencian valores muy bajos menores a los 10°C y un dato menor a 5°C, los cuales se consideran atípicos para la zona de estudio, por lo que se sugiere hacer una revisión de la captura de estos datos.

C. Variable start_depth

```{r}
boxplot(data_factor$start_depth)
```
El comportamiento de esta variable muestra que los datos se agruparon entre profundidades inciales de 200 y 1000 metros aproximadamente, con algunos valores atípicos que superaron los 2500 metros.

D. Distancia a la costa

```{r}
boxplot(data_factor$distance_from_shore)
```

Por último, la mayoría de los puntos de muestreo se ubicaron entre los 25 y 70 kilómetros de la costa. Sólo dos registros superaron los 100 km de distancia.

#Análisis entre variables numéricas.

Definimos un vector con solo las variables númericas

```{r}
select(vivos, distance_from_shore,start_depth,surface_temperature,individuals)->variables_numericas

```


```{r}
## Creamos la visualización usando el método ggpairs()
ggpairs(
  variables_numericas, 
  title="Correlograma"
  ) 
```


```{r}
ggcorr(
  variables_numericas, 
  method = c("everything", "pearson"),
  size = 3
  )
```

#Analisis entre variables numericas y variables categoricas.


```{r}
p <- ggplot(vivos, aes(x=taxon, y=individuals))
p + geom_boxplot()
```

Se evidencia un organismo (Variable taxon) que contiene la mayor cantidad de individuos.


```{r}
p <- ggplot(vivos, aes(x=depth, y=individuals))
p + geom_boxplot()
```


```{r}
p <- ggplot(vivos, aes(x=offshore_distance, y=individuals))
p + geom_boxplot()
```




```{r}
# Discretizamos los organismos (Individuals) de los datos del censo
# mutate() anexa/crea variables a la base de datos
vivos %>% mutate(
  individuals2 = vivos$individuals,
   rangos = cut(individuals2, c(0, 1000, 10000, 100000, Inf))
) -> datos_completos_fechas

head(datos_completos_fechas[(ncol(datos_completos_fechas)-1):ncol(datos_completos_fechas)])
```

## Punto 4: Tabla dinámica

Instalación de paquetes y carga de librerías:

```{r}
install_github("ramnathv/htmlwidgets")
library(devtools)
install_github("smartinsightsfromdata/rpivotTable")
library(rpivotTable)
```

Construcción de la tabla dinámica: la salida nos muestra una tabla dinámica como se trabaja en excel, podemos hacer la combinación deseada de las variables de interés.

```{r}
rpivotTable(data_factor, rows="year", col="offshore_distance", aggregatorName="Sum", vals="individuals")
```




