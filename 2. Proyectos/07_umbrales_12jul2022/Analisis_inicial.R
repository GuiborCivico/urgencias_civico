# Simulacion de umbrales 22 de ju de 2022


# librerias
library(bigrquery)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

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
### 3.1.1. Base ----
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

pt1_presision <- a2_presision %>% ggplot(aes(PAR,valor, color = tipo))+
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
  theme(text = element_text(family = "serif", size =6),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


## 3.2. Umbrales ----
### 3.2.1. Bases ----
a3_umbrales <- a1_prefiltros %>% 
  select(iteracion, starts_with("PAR")) %>% 
  pivot_longer(!iteracion, names_to = "PAR",values_to = "valor") %>% 
  mutate(valor = valor/100)

### 3.2.2. Plot ----
n_fun <- function(x){
  return(data.frame(
    y = mean(x), label = paste0(round(mean(x)*100, 2),"%")  
    ))
}


pt2_umbrales<- a3_umbrales %>% ggplot(aes(valor, PAR))+
  geom_boxplot(color = "cyan4", alpha = 0.7, outlier.shape = NA, lwd = 0.3)+
  stat_summary(fun.y = mean, color ="firebrick", size = 0.1)+
  stat_summary(fun.data = n_fun, geom = "text",
               family = "serif", size =2, color = "firebrick", 
               position = position_nudge(y = -0.06,x = 0.04))+
  
  geom_vline(xintercept = mean(a3_umbrales$valor, na.rm = T),
             lty = 2, color = "grey45")+
  
  labs(title = paste0(
    "Umbrales aproximados para una aprobación \nde entre el ",
    min_aporbacion,"% y el ",max_aprobacion,"%" ))+
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10))+
  theme_minimal()+
  theme(text = element_text(family = "serif", size =6),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
  

ggen <- a1_prefiltros %>% group_by() %>% 
  summarise(across(PAR10:PAR90,~(mean(.,na.rm  =T)))) %>% 
  as.data.frame() %>% unlist() %>% as.vector() %>% 
  round(.,1)

# Nequi ----


neq <- a1_prefiltros %>% group_by() %>% 
  summarise(across(PAR10:PAR90,~(mean(.,na.rm  =T)))*umbral_nequi) %>% 
  as.data.frame() %>% unlist() %>% as.vector() %>% 
  round(.,1)


b1_nequi <- a0_raw %>% 
  filter(PAR10<=neq[1],PAR30<=neq[2],PAR60<=neq[3],PAR90<=neq[4])


b2_presision <- b1_nequi %>% 
  mutate(iteracion = row_number()) %>% 
  pivot_longer(!iteracion, names_to = "parametros", values_to = "valor") %>% 
  # Específico
  filter(str_detect(parametros, "pre")) %>% 
  mutate(PAR = paste0("PAR ", str_extract(parametros,"[:digit:]+")),
         tipo = ifelse(str_detect(parametros,"g$"),
                       "II. Final","I. Inicial"),
         valor = valor/100) 

### 3.1.2. Plot ----

pt3_nequi_pre <- b2_presision %>% ggplot(aes(PAR,valor, color = tipo))+
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
  labs(title = "NEQUI: Sensibilidad esperada",
       subtitle = paste0("Aprobación entre el ",
                         min(b1_nequi$porcentaje_aprobacion),
                         "% (min) y ",
                         max(b1_nequi$porcentaje_aprobacion),
                         "% (max)"),
       x = "",y = "", color = "Momento")+
  theme_minimal()+
  theme(text = element_text(family = "serif", size =6),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))




pt3_nequi_pre

pt4_nequi_apr <- b1_nequi %>% ggplot(aes(porcentaje_aprobacion/100))+
  geom_density(fill = "cyan4",color = "cyan4", alpha =.4)+
  geom_vline(xintercept = 
               c(mean(b1_nequi$porcentaje_aprobacion)/100,
                 median(b1_nequi$porcentaje_aprobacion)/100),
             lty = c(1,2), color = "brown3") +
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10))+
  labs(title = "NEQUI: Distribución de la aprobación esperada",
       subtitle = 
         paste0("¿Cuantos solicitudes aprobaria para NEQUI ",
                "si apruebase entre el ",min_aporbacion,"% y el ",
                max_aprobacion, "% de todas las soliciutdes?"),
       x = "Aprobación sobre el total de solicitudes",y = "",
       caption = paste0("Línea continua: Promedio (", 
                        round(mean(b1_nequi$porcentaje_aprobacion),2),
                        "%)\nLínea discontinua: Mediana (",
                        round(median(b1_nequi$porcentaje_aprobacion),2),
                        "%)"))+
  theme_minimal()+
  theme(text = element_text(family = "serif", size =6),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "cyan4"),
        plot.caption = element_text(color = "brown3"))





rm(a0_raw,a1_prefiltros,a2_presision,a3_umbrales,b1_nequi,b2_presision)



