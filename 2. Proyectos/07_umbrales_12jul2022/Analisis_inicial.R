# Simulacion de umbrales 22 de ju de 2022


# librerias
library(bigrquery)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)


# Hiper parámetros 
max_aprobacion = 60
min_aporbacion = 50

# 1. Descarga de datos  -----
a0_raw <- bq_table_download(bq_project_query(
  ## A. Nombre del proyecto
  "atlas-323415",
  
  ## B. Consulta SQL
  ("SELECT * FROM `atlas-323415.z00_scores_creditos.sc03_base_line_thresholds_par10_30_60_90`"))) 



# 2. Filtro por hiperparámetros ----
a1_prefiltros <-  a0_raw %>% 
  filter(between(porcentaje_aprobacion,min_aporbacion,max_aprobacion)) %>% 
  mutate(iteracion = row_number())



# 3. Análisis general  ----


## 3.1. Presisión ----
### 3.1.1. Base de datos ----
a2_presision <- a1_prefiltros %>% 
  mutate(iteracion = row_number()) %>% 
  pivot_longer(!iteracion, names_to = "parametros", values_to = "valor") %>% 
  # Específico
  filter(str_detect(parametros, "pre")) %>% 
  mutate(PAR = paste0("PAR ", str_extract(parametros,"[:digit:]+")),
         tipo = ifelse(str_detect(parametros,"g$"),
                       "II. Final","I. Inicial"),
         valor = valor/100) 

### 3.1.2. Plot ----

pt1 <- a2_presision %>% ggplot(aes(PAR,valor, color = tipo))+
  geom_boxplot(outlier.shape = NA) +
  scale_color_manual(values = c("brown3","cyan4"))+
  
  stat_summary(aes(PAR,valor, group = tipo), fun = mean, 
               position = position_dodge(width = .75))+
  
  stat_summary(aes(PAR,valor, group = tipo), fun = mean,
               geom = "line", lty = 2,
               position = position_dodge(width = .75))+
  coord_cartesian(ylim = c(0,1))+
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 5))+
  labs(title = "Porcentaje de deudores detectados (Sensitivity)",
       subtitle = paste0("Aprobación entre el ",min_aporbacion,"% y ",
                         max_aprobacion,"%"),
       x = "",y = "", color = "Momento")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


## 3.2. Umbrales

a3_umbrales <- a1_prefiltros %>% 
  select(iteracion, starts_with("PAR")) %>% 
  pivot_longer(!iteracion, names_to = "PAR",values_to = "valor") %>% 
  mutate(valor = valor/100)


a3_umbrales %>% ggplot(aes(valor, PAR))+
  geom_boxplot(color = "cyan4", alpha = 0.7)+
  stat_summary(fun.y = mean, color ="red")+
  geom_vline(xintercept = mean(a3_umbrales$valor, na.rm = T),
             lty = 2, color = "red")+
  labs(title = paste0(
    "Umbrales aproximados para una aprobación \nde entre el ",
    min_aporbacion,"% y el ",max_aprobacion,"%" ))+
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10))+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
  


a3_umbrales %>% ggplot(aes(valor, color = PAR, fill = PAR))+
  geom_density(alpha = 0.3)+
  scale_x_continuous(labels = scales::percent_format())



# Nequi ----

a2_presision <- a0_raw %>% 
  # # # # # # 
  filter(between(porcentaje_aprobacion,min_aporbacion/2,max_aprobacion/2) ) %>% 
  # # # # # # 
  mutate(iteracion = row_number()) %>% 
  pivot_longer(!iteracion, names_to = "parametros", values_to = "valor") %>% 
  # Específico
  filter(str_detect(parametros, "pre")) %>% 
  mutate(PAR = paste0("PAR ", str_extract(parametros,"[:digit:]+")),
         tipo = ifelse(str_detect(parametros,"g$"),
                       "II. Final","I. Inicial"),
         valor = valor/100) 


pt2 <- a2_presision %>% ggplot(aes(PAR,valor, color = tipo))+
  geom_boxplot(outlier.shape = NA) +
  scale_color_manual(values = c("hotpink1","cyan3"))+
  
  stat_summary(aes(PAR,valor, group = tipo), fun = mean, 
               position = position_dodge(width = .75))+
  
  stat_summary(aes(PAR,valor, group = tipo), fun = mean,
               geom = "line", lty = 2,
               position = position_dodge(width = .75))+
  coord_cartesian(ylim = c(0,1))+
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 5))+
  labs(title = "NEQUI (sensitivity)",
       subtitle = paste0("Aprobación entre el ",min_aporbacion/2,"% y ",
                         max_aprobacion/2,"%"),
       x = "",y = "", color = "Momento")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



ggpubr::ggarrange(
  pt1, pt2, nrow = 2)



pr <- a0_raw %>% 
  # # # # # # 
  filter(between(porcentaje_aprobacion,min_aporbacion,max_aprobacion)) %>% 
  group_by() %>% 
  summarise(across(PAR10:PAR90, ~mean(., na.rm = T))/2) %>% 
  as.vector()


pr[1]
