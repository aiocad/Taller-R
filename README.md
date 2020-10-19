---
title: "Asociación e independencia entre variables"
author: "Jenny Barrera / Antonio Caicedo"
date: "18/10/2020"
output: html_document
---


## Importar datos a R

Ubicamos nuestro archivo de datos en una carpeta denominada data, dentro del nuevo proyecto creado.

## Carga de paquetes para importar datos

```{r}
library("tidyverse")
library("readxl")
```

## Importar archivo de excel a R

```{r}
read_xlsx(
  path = "data/data_taller.xlsx", 
  sheet="data"
) -> data_taller_xlsx
str(data_taller_xlsx)
```

```{r}
## Le decimos a R que vamos a trabajar con esos datos y se los ponemos "en primer plano"
attach(data_taller_xlsx)
```

## Información sobre el conjunto de datos

Los datos corresponden a información de biodiversidad marina relacionada con eventos de muestreo realizados anualmente en varias estaciones ubicadas en la costa oeste de Estados Unidos, que buscan cuantificar las condiciones ambientales y la biota encontrada a lo largo de la corriente de California . Los individuos capturados e identificados fueron contablizados. También se presentan datos de variables físicas como la temperatura del mar, profundidad y distancia a la costa. 

## Fuente: 
Auth, Toby; 03/20/2017. NOAA Fisheries Northwest Fisheries Science Center. Juvenile Fish Data: Prerecruit Survey Trawl Data Catch (https://www.webapps.nwfsc.noaa.gov/apex/parrdata/inventory/tables/table/prerecruit_survey_trawl_data_catch).

## Variables:

- cruise: Nombre del crucero
- year: Año del crucero
- station:Número de la estación
- distance_from_shore:Distancia más cercana a la costa en kilómetros
- offshore_distance: Distancia aproximada desde la costa hacia mar adentro. La categoría alta significa que el punto de toma de muestra está más alejado de la costa en comparación con los demás.
- start_depth: Profundidad en metros del fondo al inicio de la toma de muestra. Puede dar una idea de la profundidad de la estación. 
- depth: Variable profundidad inicial transformada en categórica ordinal. Da una idea de la profundidad de la estación. 
- surface_temperature: Temperatura superficial del mar en grados centígrados medida al inicio de la colecta.
- taxon: Nombre científico de los organismos capturados a nivel de especie, género o familia. 
- maturity: Representa la edad de los individuos o su estado de madurez.
- individuals: Representa el número de individuos capturados 
- comments: Comentarios realizados sobre los organismos capturados 

## Dimensionalidad de los datos

```{r}
dim(data_taller_xlsx)
```

Tenemos 2451 filas o individuos y 13 variables ó columnas.

## Limpieza de los datos

Verificamos si tenemos datos faltantes

```{r}
sapply(data_taller_xlsx, function(x) sum(is.na(x)))
```

La variable surface_temperatura presenta 22 datos perdidos. Por tal motivo procedemos a probar si la pérdida de datos es aleatoria.

### Prueba para probar la pérdida aleatoria de datos

```{r}
# Instalamos paquetes 
# install.packages("mvnmle")
# install.packages("BaylorEdPsych")

# A mediados de 2020 estos paquetes fueron desatendidos del CRAN
# Sin embargo, podríamos instalarlos de forma autónoma (standalone)
# [EN] Tools > Install packages > Install from: Package Archive File
# [ES] Herramientas > Instalar paquetes > Instalar desde: Archivo de paquete
```
ERROR!!!! NO PUDE INSTALAR "mvnmle" TENGO LA VERSION 4 DE R!!

```{r}
# Cargamos paquetes
library("mvnmle")
library("BaylorEdPsych")

# Hacemos el test de Little
# Hipótesis nula: los datos están perdidos de forma aleatoria
# Asumiendo que siguen una distribución normal multivariada
# Hipótesis alterna: hay patrones de pérdida de datos
LittleMCAR(data_taller_xlsx) -> little_test
little_test$p.value
```


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

## Limpieza de los datos

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

En este caso no se tienen datos faltantes.


### Valores duplicados

Se eliminaran duplicados en toda la fila, pues no se espera dos registros iguales.

Podemos buscar y eliminar duplicados basados en toda la fila.

```{r}
distinct(vivos)
```
Se observan que se mantienen los mismos registros, es decir no hay valores duplicados.


## Datos atipicos.

```{r}
# Estadístico de resumen para la variable objetivo
summary(vivos$individuals)
```

Se observa un valor maximo extremos, validaremos posibles valores outliners

#
```{r}
# Boxplot de una variable
boxplot(vivos$individuals)

# Valores de potenciales outliers
boxplot.stats(vivos$individuals)$out

```


Se observa una gran varianción entre la variable objetivo "vivos" sin embargo los valores los interpretamos como logicos pues se puede tener muestreo de un solo organismo vivo hasta miles como se evidencio.



## Análisis descriptivo

### Resumen numérico

El método **summary()** que trae por defecto R nos brinda estadísticas de resumen para cada una de las variables de nuestro conjunto de datos.

```{r}
## Resumen básico de datos
summary(vivos)
```

### Resumen gráfico

```{r, out.width = '100%'}
## Cargamos la librería ggplot2
library(ggplot2)
## Usamos el método para graficar histogramas
## Seleccionamos como objetivo la variable quality
ggplot(vivos, aes(individuals)) +
    geom_histogram()
```



Se evidencia que existe una gran desproporción en las variable individuos, seguramente dependiendo del organismos


#Analisis entre variables numericas.

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






