# Ánalisis generales (urgencias)
Este repo contiene códigos de algunos análisis on-the-run


## Tips
### 1. Extraer bases de BigQuery

```r

Base <- bq_table_download(bq_project_query(
  ## 1. Nombre del proyecto
  "insights-295219",
  
  ## 2. Consulta SQL
  (paste0("SELECT DISTINCT _id,category_ids as ID_cat,",
          " latitude, longitude, name,   FROM",
          " `insights-295219.landing_prod.entities` ",
          "WHERE  _type = 'Place' AND Visible = true")))) 

```