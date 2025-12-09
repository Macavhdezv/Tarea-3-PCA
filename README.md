# Tarea-3-PCA

¡Hola! En este repositorio realizaré la tarea número 3 sobre Análisis de Componentes Principales. No sabía que realizar y luego de pensar bastante este último tiempo me dio un hiperfoco en ver cosas de crimen. Cuando pensé que hacer, recordé que mi serie de crime favorita es Criminal Minds, por lo que el tema de este trabajo es sobre esta serie.

Ahora comenzaremos la ultima tarea del curso. En esta tarea, se deberá realizar un análisis de complementos principales. Para esto empezamos con el llamado de paquetes

```{r}
library(tidyverse)
library(readxl)
library(janitor)
library(ggplot2)
library(dplyr)
library(corrr)
library(corrplot)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(naniar)
library(zoo)
library(ggrepel)

```

Como no encontré una base de datos con los episodios de la serie Criminal Minds, tuve que extraer los datos desde Wikipedia ya que en este caso estaba la descripción de los episodios, a través web scraping. Primero extraje los episodios y ocupe todos los capítulos hasta la temporada 10 debido a dos situaciones: en Wikipedia la temporada 11 no tenía descripciones y además, en esta temporada se va mi personaje preferido Aaron Hotchner.

```{r}
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)

url <- "https://es.wikipedia.org/wiki/Anexo:Episodios_de_Mentes_criminales"
page <- read_html(url)

tables <- page %>% html_nodes("table.wikitable")

procesar_tabla <- function(tbl) {
  df <- tbl %>% html_table(fill = TRUE)

  colnames(df) <- colnames(df) %>%
    str_replace_all("\\s+", "_") %>%
    str_replace_all("[^A-Za-z0-9_]", "") %>%
    tolower()

  posibles <- c("n_en_serie","n_en_temp","titulo",
                "dirigido_por","escrito_por",
                "fecha_de_emision_original","emision_original")

  cp <- intersect(posibles, colnames(df))
  if (length(cp) < 3) return(NULL)

  df <- df %>% select(any_of(cp))


  df <- df %>% mutate(across(everything(), as.character))


  if ("fecha_de_emision_original" %in% colnames(df)) {
    df <- df %>% rename(fecha_emision = fecha_de_emision_original)
  } else if ("emision_original" %in% colnames(df)) {
    df <- df %>% rename(fecha_emision = emision_original)
  } else {
    df <- df %>% mutate(fecha_emision = NA_character_)
  }

  return(df)
}

episodios_lista <- tables %>% map(procesar_tabla) %>% discard(is.null)

episodes_all <- bind_rows(episodios_lista, .id = "temporada") %>%
  mutate(temporada = as.integer(temporada))

criminalminds_episodes <- episodes_all %>% 
  filter(temporada <= 15) %>% 
  rowid_to_column("id")

criminalminds_episodes

criminalminds_episodes <- criminalminds_episodes %>%
  filter(id %% 2 != 0) %>%    
  mutate(id = row_number()) %>%
  filter(id < 234) %>%   
  select(-fecha_emision)
  

```

Era muy complicado realizar el web scraping de las descripciones de los episodios, ya que estas se encuentran en nodos diferentes. Por lo tanto, tuve que extraerlas por separado y luego unirlas a la base de datos completa.

```{r}
library(rvest)
library(dplyr)
library(stringr)
library(tibble)

url <- "https://es.wikipedia.org/wiki/Anexo:Episodios_de_Mentes_criminales"
page <- read_html(url)


desc_nodes <- page %>% html_nodes("td[colspan]")


descs <- desc_nodes %>% html_text(trim = TRUE)


descs <- descs[nchar(descs) > 50]


criminalminds_descriptions <- tibble(
  id = seq_along(descs),
  descripcion = descs
)

criminalminds_descriptions

criminalminds_descriptions <- criminalminds_descriptions%>%
  filter(id < 234) %>%      
  mutate(id = row_number())  

```

Unimos la base de datos completa de episodios con las descripciones uniendose a través de la columna "id"

```{r}
criminal_minds <- left_join(criminalminds_episodes, criminalminds_descriptions, by = "id")

```

Sentía que me faltaba información sobre las series, por lo que decidí buscar en IMDb la información de calificaciones y votos de cada episodio. Para esto, descargué la base de datos desde IMDb y limpié los datos innecesarios.

```{r}
imdb_criminalminds <- read_xlsx("C:/Users/macav/OneDrive - Universidad Católica de Chile/Documentos/Tarea-3-PCA/criminal_minds_clean.xlsx") |>
  clean_names()

imdb_criminalminds <- imdb_criminalminds %>% 
  select(position, title, year, im_db_rating, num_votes)

imdb_criminalminds <- imdb_criminalminds %>%
  filter(position < 234) %>% 
  rename(id = position)

```

Volvemos unir las bases de datos a través de la columna "id" nuevamente. Tambien debemos colocar todas las variables en numeros por lo que las transformaremos

```{r}

criminal_minds_full <- left_join(criminal_minds, imdb_criminalminds, by = "id")

```

Crear variables binarias con respecto a la descripción. 

```{r}

criminal_minds_full <- criminal_minds_full %>%
  mutate(
    mujer = as.numeric(str_detect(descripcion, regex("mujer(es)?", ignore_case = TRUE))),
    hombre = as.numeric(str_detect(descripcion, regex("hombre(s)?", ignore_case = TRUE))),
    adolescente = as.numeric(str_detect(descripcion, regex("adolescente(s)?", ignore_case = TRUE))),
    nino = as.numeric(str_detect(descripcion, regex("niñ(a|o|as|os)", ignore_case = TRUE))),
    asesino = as.numeric(str_detect(descripcion, regex("asesin(a|o|as|os|atos)", ignore_case = TRUE))),
    cuerpo = as.numeric(str_detect(descripcion, regex("cuerpo(s)?", ignore_case = TRUE))),
    victima = as.numeric(str_detect(descripcion, regex("víctima(s)?", ignore_case = TRUE))),
    piromano = as.numeric(str_detect(descripcion, regex("pirómano", ignore_case = TRUE))),
    familia = as.numeric(str_detect(descripcion, regex("familia(s)?", ignore_case = TRUE)))
  )

write_csv(criminal_minds_full, "criminal_minds_full.csv")

```

# Creamos matriz final para PCA

```{r}

pca_matrix <- criminal_minds_full %>%
  select(
    temporada,  n_en_serie, n_en_temp,
    im_db_rating, num_votes,
    mujer, hombre, adolescente, nino,
    asesino, cuerpo, victima, piromano, familia
  ) %>%
  mutate(across(everything(), as.numeric))
```

## Estandarización

Ahora si podemos hacer el análisis de componentes principales, en esta parte solo tomaremos las variables numéricas de la base de datos completa de Criminal Minds.

```{r}

pca_matrix_scaled <- scale(pca_matrix)

write.csv(pca_matrix_scaled, "pca_matrix_scaled.csv")
view 
```

Ejecutamos el PCA

```{r}
pca_model <- prcomp(pca_matrix_scaled, center = TRUE, scale. = TRUE)
summary(pca_model)

Importance of components:
                         PC1    PC2     PC3     PC4     PC5     PC6     PC7     PC8    PC9
Standard deviation     1.639 1.1838 1.14068 1.08729 1.06124 1.04082 0.98550 0.97074 0.9142
Proportion of Variance 0.192 0.1001 0.09294 0.08444 0.08045 0.07738 0.06937 0.06731 0.0597
Cumulative Proportion  0.192 0.2920 0.38499 0.46943 0.54988 0.62726 0.69663 0.76394 0.8236
                          PC10   PC11    PC12    PC13    PC14
Standard deviation     0.86923 0.8557 0.79683 0.58838 0.01386
Proportion of Variance 0.05397 0.0523 0.04535 0.02473 0.00001
Cumulative Proportion  0.87760 0.9299 0.97526 0.99999 1.00000
```

Ahora comencé a graficar

```{r}
fviz_eig(pca_model, addlabels = TRUE, 
         ylim = c(0, 50),
         barfill = "#7AC5CD",  barcolor = "#2E8B57")
```
<img width="750" height="516" alt="fviz_eig" src="https://github.com/user-attachments/assets/3147e68e-3f9f-4ea5-8747-f1d2fdd691ac" />

Podemos observar que las dos primeras componentes principales explican un 40.6% de la varianza total, por lo que nos quedaremos con estas dos componentes para analizar.

```{r}
fviz_pca_biplot(
  pca_model,
  repel = TRUE,
  col.ind = "#00CDCD",
  col.var = "#2F4F4F",
  labelsize = 2,
  arrowsize = 0.7
)



```
<img width="750" height="516" alt="fviz_pca_biplot" src="https://github.com/user-attachments/assets/012467d7-5de7-4029-a2ea-d339f13ed326" />

Podemos observar que las variables "im_db_rating" y "num_votes" están muy correlacionadas, lo que tiene sentido ya que a mayor número de votos, mayor es la calificación promedio. Además, las variables binarias relacionadas con la presencia de mujeres, hombres, adolescentes y niños en los episodios también muestran cierta correlación entre sí.

```{r}
fviz_pca_ind(
  pca_model,
  geom.ind = "point",
  pointshape = 21,
  pointsize = 3,
  fill.ind = factor(criminal_minds_full$temporada),
  palette = "Paired",
  addEllipses = TRUE,
  legend.title = "Temporada"
)



```
<img width="875" height="516" alt="fviz_pca_ind" src="https://github.com/user-attachments/assets/c5cf0ad6-6c0f-44af-b944-7ac0a322843b" />

En este gráfico podemos observar cómo se agrupan los episodios según las temporadas. Parece que hay cierta diferenciación entre las temporadas, lo que podría indicar que los episodios de diferentes temporadas tienen características distintas en términos de las variables analizadas.

```{r}
fviz_pca_ind(
  pca_model,
  geom.ind = "point",
  pointshape = 21,
  pointsize = 3,
  fill.ind = factor(criminal_minds_full$mujer),
  palette = c("#009ACD", "#79CDCD"),
  legend.title = "Mujer"
)



```
<img width="875" height="516" alt="fviz_pca_ind2" src="https://github.com/user-attachments/assets/0c3af9bd-b105-4cb3-a098-1ec785030b88" />
En este gráfico podemos observar cómo se agrupan los episodios según la presencia de mujeres en la descripción. Parece que los episodios con presencia de mujeres tienden a agruparse en ciertas áreas del espacio PCA, lo que podría indicar que estos episodios tienen características distintas en términos de las variables analizadas.

```{r}
fviz_pca_var(
  pca_model,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)



```
<img width="875" height="516" alt="fviz_pca_var" src="https://github.com/user-attachments/assets/06feccf5-ea46-417e-80bc-7e6847e1b3e2" />
En este gráfico podemos observar las contribuciones de cada variable a las componentes principales. Las variables "im_db_rating" y "num_votes" tienen una alta contribución a la primera componente principal, mientras que las variables binarias relacionadas con la presencia de mujeres, hombres, adolescentes y niños tienen una mayor contribución a la segunda componente principal.

#Conclusiones

Fue bastante entretendo realizar este análisis ya que dejó mostrar temas relevantes. Los episodios de *Criminal Minds* presentan patrones claros tanto en su evolución como en su contenido. Por un lado, las primeras temporadas tienden a agruparse debido a una mayor presencia de elementos violentos como: asesinatos, víctimas y cuerpos, mientras que las temporadas siguientes se distribuyen de manera más dispersa, reflejando cambios en la temática que presenta el estilo narrativo de la serie. Por otro lado, las variables de popularidad (rating y votos) forman un eje independiente y esto también puede entenderse al ser tal vez otra base de datos, indicando que la recepción del público no depende directamente del tipo de crimen mostrado, una idea que creía posible. Además, elementos como la presencia de mujeres, hombres o familias aparecen en muchos episodios y no separan claramente los grupos, mientras que temas más específicos como son los casos que involucran niños, adolescentes o violencia explícita, sí generan patrones definidos dentro del PCA. En conjunto, estos resultados permiten observar cómo la serie evoluciona y cómo ciertos temas narrativos se agrupan de manera consistente, ofreciendo una visión general de la estructura y cambios en sus episodios.

