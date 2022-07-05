# Ánalisis generales (urgencias)

Este repo contiene códigos de algunos análisis on-the-run

## Tips

### 1. Extraer bases de BigQuery

``` r
library(bigrquery)

Base <- bq_table_download(bq_project_query(
  ## A. Nombre del proyecto
  "insights-295219",
  
  ## B. Consulta SQL
  ("SELECT * FROM `insights-295219.landing_prod.entities`"))) 
```

### 2. Extraer bases de CloudStorage

### 3. Simplificaicón de textos en español

``` r
simplyTEXT <- function(texto) {
  texto <- tolower(texto)
  #texto <- gsub(pattern = "\ ", replacement = "",texto)
  texto <- gsub(pattern = "á", replacement = "a",texto)
  texto <- gsub(pattern = "é", replacement = "e",texto)
  texto <- gsub(pattern = "í", replacement = "i",texto)
  texto <- gsub(pattern = "ó", replacement = "o",texto)
  texto <- gsub(pattern = "ú", replacement = "u",texto)
  texto <- gsub(pattern = "ü", replacement = "u",texto)
  texto <- gsub(pattern = "ñ", replacement = "n",texto)
  return(texto)
}
```

### 4. Otros insumos

#### 4.1. Matriz de confusión

<p align="center">
<img src="0. insumos/matriz_confusion.png" width="484"/>
</p>
