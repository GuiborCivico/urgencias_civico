# Ánalisis generales (urgencias)
Este repo contiene códigos de algunos análisis on-the-run


## Tips
### 1. Extraer bases de BigQuery

```r
library(bigrquery)

Base <- bq_table_download(bq_project_query(
  ## A. Nombre del proyecto
  "insights-295219",
  
  ## B. Consulta SQL
  (SELECT * FROM `insights-295219.landing_prod.entities`)) 

```