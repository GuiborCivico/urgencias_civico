# Pruebas Civico
library(dplyr)
library(ggplot2)
library(sf)
library(sp)
library(geojsonsf)

a1_raw <- read.csv("3. Datos/Tesselation.cvs", sep = ";") %>% 
  mutate(across(TOTAL_DENSIDAD:POINT_Y,
                ~as.numeric(gsub(",",".",.)))) %>% 
  select(-X,-Y) %>% 
  mutate(lon = POINT_X, lat = POINT_Y) %>% 
  st_as_sf(coords = c("POINT_X","POINT_Y")) %>% 
  st_set_crs(4326) %>% 
  st_join(
    (geojson_sf("3. Datos/A1_Barrios_Bogota.json") %>% st_set_crs(4326))) %>% 
  as.data.frame()
  

a2_resumen <- 
  rbind(
    # Resumen Barrial
    (a1_raw %>% 
       group_by(Localidad = Nombre_Loc, Barrio = Nombre_Bar) %>% 
       summarise(Riesgo_Primera_cuota = mean(PAR30, na.rm = T),
                 Riesgo_Segunda_cuota = mean(PAR60, na.rm = T),
                 Riesgo_Tercera_cuota = mean(PAR90, na.rm = T),
                 Riesgo_General = mean(PONDERADO_TOTAL, na.rm = T)) %>% 
       reshape2::melt(id.vars = c("Localidad", "Barrio"),
                      variable.name = "Tipo", value.name = "Riesgo") %>%
       mutate(Tipo = gsub("_","\ ",substr(Tipo,8,20))) %>% 
       arrange(Localidad, Barrio, Tipo)),
    # Resumen General de ciduad
    (a1_raw %>% 
       group_by() %>%
       summarise(Riesgo_Primera_cuota = mean(PAR30, na.rm = T),
                 Riesgo_Segunda_cuota = mean(PAR60, na.rm = T),
                 Riesgo_Tercera_cuota = mean(PAR90, na.rm = T),
                 Riesgo_General = mean(PONDERADO_TOTAL, na.rm = T)) %>% 
       as.data.frame() %>% 
       mutate(Localidad = "A. Todas las localidades", 
              Barrio ="A. Todos los barrios") %>% 
       reshape2::melt(id.vars = c("Localidad", "Barrio"),
                      variable.name = "Tipo", value.name = "Riesgo") %>%
       mutate(Tipo = gsub("_","\ ",substr(Tipo,8,20))) %>% 
       arrange(Localidad, Barrio, Tipo)))
  


write.csv(a2_resumen, "3. Datos/01_Pruebas_GeoIDEAL.csv", row.names = F)





a2_resumen %>% filter(Barrio ==c("METROPOLIS","TINTALITO", "ZONA FRANCA","CIUDAD JARDIN SUR")) %>% 
  ggplot(aes(Barrio,Riesgo, fill = Tipo))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_y_continuous(labels = scales::percent_format())+
  theme_minimal()
