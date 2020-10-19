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

###cruise: Nombre del crucero
year: Año del crucero
station:Número de la estación
distance_from_shore:Distancia más cercana a la costa en kilómetros
offshore_distance: Distancia aproximada desde la costa hacia mar adentro. La categoría alta significa que el punto de toma de muestra está más alejado de la costa en comparación con los demás.
start_depth: Profundidad en metros del fondo al inicio de la toma de muestra. Puede dar una idea de la profundidad de la estación. 
depth: Variable profundidad inicial transformada en categórica ordinal. Da una idea de la profundidad de la estación. 
surface_temperature: Temperatura superficial del mar en grados centígrados medida al inicio de la colecta.
taxon: Nombre científico de los organismos capturados a nivel de especie, género o familia. 
maturity: Representa la edad de los individuos o su estado de madurez.
individuals: Representa el número de individuos capturados 
comments: Comentarios realizados sobre los organismos capturados 

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


### Datos faltantes

```{r}
sapply(wine_raw, function(x) sum(is.na(x)))
```

En caso que se encuentren datos faltantes se pueden utilizar procesos de imputación de datos, entre otros:
- Usando la media de la variable
- HotDeck
- MICE

### Valores duplicados

Podemos buscar y eliminar duplicados basados en un columna, por ejemplo, cuando esperamos tener datos únicos de un individuo y tenemos una columna para identificarlo.

```{r}
## Ejemplo: acá dejamos valores únicos en la columna pH
distinct(wine_raw, pH, .keep_all = TRUE)
## Ejemplo: acá dejamos valores únicos en la columna density
distinct(wine_raw, density, .keep_all = TRUE)
```

Podemos buscar y eliminar duplicados basados en toda la fila.

```{r}
distinct(wine_raw)
```

## Análisis descriptivo

### Resumen numérico

El método **summary()** que trae por defecto R nos brinda estadísticas de resumen para cada una de las variables de nuestro conjunto de datos.

```{r}
## Resumen básico de datos
summary(wine_raw)
```

### Resumen gráfico

```{r, out.width = '100%'}
## Cargamos la librería ggplot2
library(ggplot2)
## Usamos el método para graficar histogramas
## Seleccionamos como objetivo la variable quality
ggplot(wine_raw, aes(quality)) +
    geom_histogram()
```

## Efectos de una variable sobre otra

En una investigación o estudio podemos sospechar de la influencia o efecto de un conjunto de variables sobre una variable particular de interés (**CTQ: Critical to quality**). Una parte esencial de la fase de análisis es reunir evidencia estadística suficiente para seleccionar las variables que sí tienen un efecto real sobre nuestra variable de interés.

![Influencia](../images/R_00.png)

Existen distintas herramientas estadísticas para tener una idea bien formada de cómo se relacionan dos o más variables entre sí.

Antes de explorar dichas herramientas, conviene hacer una revisión sobre algunos conceptos.

### Asociación entre dos variables contínuas

La **covarianza** es una medida numérica que nos permite cuantificar la relación (lineal) entre dos variables contínuas.

$$ Cov(X, Y) = E[(X-E(X))(Y-E(Y))]$$

Su estimador es la covarianza muestral:

$$ s_{XY}=\frac{1}{n}\sum_{i=1}^{n}(x_i - \bar{x})(y_i - \bar{y}) $$

- Si dos variables son **independientes** su covarianza es nula. El reciproco no es cierto en general, si dos variables tienen covarianza nula se dice que son incorreladas (no hay relación lineal, aunque puede haber una relación no lineal).
- Si la covarianza es positiva indica que a valores grandes de $X$ le corresponden valores grandes de $Y$ (i.e. al incrementar $X$ se incrementa $Y$) y se dice que hay una relación lineal positiva.
- Si la covarianza es negativa indica que a valores grandes de $X$ le corresponden valores pequeños de $Y$ (i.e. al incrementar X, Y disminuye) y se dice que hay una relación lineal negativa.

Cuanto mayor es el valor (absoluto) de la covarianza, mayor es el grado de relación lineal entre las variables. Sin embargo, su valor depende de las escala de las variables por lo que es difícil determinar cuando es grande o pequeña. Para medir el grado de relación lineal puede ser preferible reescalarla, i.e. emplear el **coeficiente de correlación:**

$$ \rho(X,Y) = \frac{Cov(X,Y)}{\sigma(X)\sigma(Y)} $$

Su estimador es el coeficiente de correlación muestral:

$$ r_{XY}= \frac{\sum_{i = 1}^{n}(x_i - \bar{x})(y_i - \bar{y})}{\sqrt{\sum_{i=1}^{n}(x_i - \bar{x})^2}\sqrt{\sum_{i=1}^{n}(x_i - \bar{x})^2}} $$


- Una correlación positiva entre dos variables indica que a medida que los valores de una variable crecen los valores de la otra variable también crecen. Y viceversa. El máximo valor de una correlación positiva es 1.
- Una correlación negativa entre dos variables indica que a medida que los valores de una variable crecen los valores de la otra variable decrecen. El máximo valor de una correlación negativa es -1.
- Una correlación de cero entre dos variables indica que no existe una asociación lineal entre ellas.

Dado que el trabajo estadístico de datos es principalmente matricial y tenemos un número finito de **variables aleatorias**, en vez de calcular la covarianza entre dos variables podemos construir una **matriz de covarianzas** y calcular las covarianzas entre todas las variables.

#### Covarianzas en R

```{r}
## Covarianzas entre dos variables
cov(wine_raw$fixed_acidity, wine_raw$quality)
```

```{r}
cov(wine_raw$chlorides, wine_raw$quality)
```

```{r}
## Correlaciones entre dos variables
cor(wine_raw$fixed_acidity, wine_raw$quality, method = 'pearson')
```
```{r}
cor(wine_raw$chlorides, wine_raw$quality, method = 'pearson')
```


```{r}
## Matriz de correlaciones
cor(wine_raw, method = 'pearson')
```
Por defecto el método **cor()** calcula una correlación de Pearson, por tanto, el resultado numerico asume que la relación entre las variables es lineal. Dado que en la practica hay muchas relaciones no lineales, una forma más robusta de calcular la asociación es calculando una **correlación de Spearman** (method = ‘spearman’) o el estadístico **Tau de Kendall** (method = ‘kendall’).

Que exista una asociación fuerte entre dos variables no implica una relación causal. Para testear la causalidad veremos otras herramientas más adelante.

#### Visualización de la relación entre variables

Hemos visto que podemos crear gráficos univariados para tener una fotografía del comportamiento de una variable. De igual manera, es posible construir gráficos que muestren la asociación entre dos o más variables.

| Variable 1 | Variable 2 | Visualización |
| :---: | :---: | :---: |
| Categórica | Categórica | Tablas de contingencia |
| Categórica | Contínua | Boxplot por grupos |
| Contínua | Contínua | Diagrama de dispersón |

Para nuestro ejemplo del vino rojo, siguiendo las recomendaciones de la tabla, conviene crear diagramas de dispersión.

#### Diagrama de dispersión

Son útiles porque al cruzar los valores de un par de variables podemos encontrar posibles relaciones matemáticas entre ellas.

![Relación de variables continuas](../images/R_01.png)

```{r}
## Una relación lineal inexistente
plot(wine_raw$residual_sugar, wine_raw$quality)
```

```{r}
## Una relación lineal positiva
plot(wine_raw$alcohol, wine_raw$quality)
```

```{r}
## Una relación lineal negativa
plot(wine_raw$volatile_acidity, wine_raw$quality)
```

#### Correlogramas

Podemos crear una visualización donde se muestren todos los posibles diagramas de dispersión entre parejas de variables con sus respectivos coeficientes de correlación.

```{r, out.width = '100%'}
## Instalamos la librería GGally
#install.packages('GGally')
## Cargamos la librería
library('GGally')
## Creamos la visualización usando el método ggpairs()
ggpairs(
  wine_raw, 
  title="Correlograma"
  ) 
```

Podemos graficar filtrando ciertas variables de interés. En este caso, vamos a remover aquellas que tengan un coeficiente de correlación menor a 0.2 con nuestra variable CTQ (quality).

```{r, out.width = '100%'}
## Declaramos un vector con nuestras variables de interés
var_interes = c('volatile_acidity','citric_acid','sulphates','alcohol','quality')
## Creamos la visualización usando el método ggpairs() agregando el parámetro columns
ggpairs(
  wine_raw, 
  title="Correlograma",
  columns = var_interes
  ) 
```

Otra forma de visualizar la correlación entre variables.

```{r, out.width = '100%'}
ggcorr(
  wine_raw, 
  method = c("everything", "pearson"),
  size = 3
  )
```

De las anteriores matrices y gráficas podemos observar algunas nuevas correlaciones de interés, por ejemplo, entre el pH y la acidéz. Podemos observar además que para la **CTQ** aproximadamente la mitad de las variables independientes correlacionan positivamente y la otra mitad negativamente.

En la práctica, se seleccionan las variables independientes que tienen las medidas de asociación más altas en la medida que nos aportan más información. Una regla de oro sencilla es excluir variables que tengan una correlación menor (en valor absoluto) a 0.2.

### Examen detallado de variables de interés

De nuestro conjunto de datos iniciales hemos detectado ciertas variables independientes o explicativas que nos pueden aportar mayor información para explicar la calidad del vino.

- volatile_acidity
- citric_acid
- sulphates
- alcohol

Vamos ahora a examinarlas individualmente por medio de un **boxplot** en el que cruzaremos los valores de cada una con la puntuación obtenida en el vino.

Se puede utilizar la representación con boxplot porque los valores de tienen una relación ordinal ($1 < 2$, $2 < 3$...), los valores van incrementando, se mide una misma cosa, tiene una única dirección (sin puntos medios).

#### volatile_acidity

```{r}
p <- ggplot(wine_raw, aes(as.factor(quality), volatile_acidity))
p + geom_boxplot()
```

#### citric_acid

```{r}
p <- ggplot(wine_raw, aes(as.factor(quality), citric_acid))
p + geom_boxplot()
```

#### sulphates

```{r}
p <- ggplot(wine_raw, aes(as.factor(quality), sulphates))
p + geom_boxplot()
```

#### alcohol

```{r}
p <- ggplot(wine_raw, aes(as.factor(quality), alcohol))
p + geom_boxplot()
```

En nuestro conjunto de datos todas las variables son contínuas. El cálculo de medidas de resumen bivariadas como las covarianzas o coeficientes de correlación, así como los resúmenes gráficos vistos, nos permiten tener una idea bien formada de si existen relaciones entre las variables y el sentido de dichas relaciones.

Vamos ahora a examinar cómo podemos hacer el mismo proceso de análisis cuando tenemos variables **categóricas**.

```{r}
## Partimos de la base de datos 'wine_raw'
## y la ontroducimos a un algoritmo de operaciones
wine_raw %>%
  ## mutate() crea una nueva variable llamada 'calidad'
  ## basada en los rangos ya conocidos de la variable quality
  mutate(
    calidad = ifelse(
      quality == '3' | quality == '4','baja',
      ifelse(
        quality == '5' | quality == '6','media',
        'alta'))
  ) %>% 
  ## mutate_at() recibe la columna 'calidad' y la convierte en un factor
  mutate_at('calidad', factor) %>%
  
  ## mutate() crea una nueva variable llamada 'acetico'
  ## basada en rangos conocidos de la variable 'volatile_acidity'
  mutate(
    acetico = ifelse(volatile_acidity < 0.7, 'bajo', 'alto')
  ) %>% 
  ## mutate_at() recibe la columna 'acetico' y la convierte en un factor
  ## el resultado de todas las operaciones se guarda en 'wine_processed'
  mutate_at('acetico', factor) -> wine_processed
```

Dado que transformamos nuestro conjunto de datos para agregar dos nuevas columnas categóricas, podemos explorar algunas medidas y gráficas relevantes para asociar 1) una variable categórica con una contínua, 2) dos variables categóricas.

### Asociación entre una variable categórica y una variable contínua

En este caso no podemos calcular covarianzas ni correlaciones de Pearson, luego debemos disponer de otro conjunto de herramientas.

- Correlación biserial-puntual. Sirve para cruzar una columna numérica con una variable categórica **binaria**.
- Regresión logística. Se trata de predecir una variable categórica en función de una continua.
- Prueba de Kruskall-Wallis. Es un test que sirve para mirar si existe una relación entre una variable categórica y una continua.

Por facilidad, haremos una **prueba de Kruskall-Wallis** cuyo p-valor nos indicará si existe una relación significante entre las variables.

- **Hipótesis nula:** las variables son independientes.
- **Hipótesis alternativa:** las variables son dependientes.

```{r}
# Hipótesis -> nula
# Resultado: Cercano a cero -> se rechaza la hipótesis
# Conclusión: el ácido volátil afecta la percepción de calidad
kruskal.test(wine_processed$volatile_acidity, wine_processed$calidad)
```

```{r}
kruskal.test(wine_processed$citric_acid, wine_processed$calidad)
```

```{r}
kruskal.test(wine_processed$sulphates, wine_processed$calidad)
```

```{r}
kruskal.test(wine_processed$alcohol, wine_processed$calidad)
```

#### Visualización

En este caso podemos construir un boxplot para cruzar cada grupo de calidad con las variables de interés.

```{r}
ggplot(data = wine_processed) +
  aes(x = calidad, y = volatile_acidity) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
```

```{r}
ggplot(data = wine_processed) +
  aes(x = calidad, y = citric_acid) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
```

```{r}
ggplot(data = wine_processed) +
  aes(x = calidad, y = sulphates) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
```

```{r}
ggplot(data = wine_processed) +
  aes(x = calidad, y = alcohol) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
```

### Asociación entre dos variables categóricas

En este caso conviene hacer análisis mediante tablas de contingencia, las cuales cuentan las frecuencias observadas en cada categoría.

```{r}
tbl = table(wine_processed$acetico, wine_processed$calidad) 
tbl
```

```{r}
# Damos nombre a las columnas y las filas 
colnames(tbl) <- c("Calidad alta", "Calidad baja", "Calidad media")
rownames(tbl) <- c("Ácido acético alto","Ácido acético bajo")
tbl
```

Al tener conformada la tabla de contingencia, la forma de revisar si existe una asociación entre las variables es por medio de una **prueba de independencia $X^2$** (Chi-Cuadrado).

La prueba indicará si dos características son independientes o tienen una asociación, de manera que las frecuencias elevadas en una de ellas suele ser acompañado con frecuencias altas en la otra.

- **Hipótesis nula:** las columnas y las filas de la tabla son independientes
- **Hipótesis alternativa:** las columnas y las filas son dependientes

```{r}
## Prueba Chi-Cuadrado
chisq.test(tbl)
```

## Observaciones de cierre

Identificar las relaciones existentes entre dos o más variables es parte arte y parte ciencia, por lo que se recomienda ganar experiencia leyendo articulos cientificos y viendo soluciones a diversos problemas.

Además,

1. Hay que procurar trabajar con **variables informativas**, es decir, que guarden una relación con la variable objetivo.
2. Hay que evitar las redundancias, luego lo ideal es que nuestras variables explicativas/independientes/features sean **independientes** entre sí.
3. Nuestra intuición puede fallar en dimensiones superiores a 3. En la mayoría de los casos aumentar la cantidad de variables afecta negativamente el entendimiento de un problema si no contamos con una gran cantidad de datos. Por ultimo, una **cantidad controlada de variables** asegura una mejor interpretabilidad de los análisis y modelos.
© 20
