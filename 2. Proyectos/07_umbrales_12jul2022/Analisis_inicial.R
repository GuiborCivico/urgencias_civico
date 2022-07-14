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



max_PAR30 = 10
max_PAR60 = 5

# 2. Filtro por hiperparámetros ----
a1_prefiltros <-  a0_raw %>% 
  filter(precision_nopar30_g <= max_PAR30 &
         precision_nopar60_g <= max_PAR60) %>% 
  mutate(across(PAR10:acc_par90,~./100),
         iteracion = row_number())



# 3. Análisis general  ----
# 3.1. Niveles de aprobación

a1_prefiltros %>% ggplot(aes(porcentaje_aprobacion))+
  geom_density(fill = "cyan4",color ="cyan4", alpha = 0.35)+
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 5))+
  geom_vline(xintercept = c(mean(a1_prefiltros$porcentaje_aprobacion),
                            median(a1_prefiltros$porcentaje_aprobacion)),
             lty = c(1,2), color = "brown3")+
  labs(title = "Aprobación General Esperada",
       subtitle = paste0(
         "Con las restricciónes:\nCaretera en PAR 30 = ", max_PAR30,"% (max)\n",
         "Catera en PAR 60 = ",max_PAR60,"% (max)"),
       y ="",x  = "Nivel de aprobación", 
       caption = paste0(
         "Línea continua: Promedio (", 
         round(mean(a1_prefiltros$porcentaje_aprobacion*100),2),
         "%)  |  Línea discontinua: Mediana (",
         round(median(a1_prefiltros$porcentaje_aprobacion*100),2),
         "%)\n\nMáxima Aaprobación Estimada = ",
         round(max(a1_prefiltros$porcentaje_aprobacion*100),2),"%"))+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))








a0_raw %>% mutate(across(PAR10:acc_par90,~./100),
                 iteracion = row_number()) %>% 
  ggplot(aes(precision_nopar10_g, porcentaje_aprobacion, color = PAR10))+
  geom_point() +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))+
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10))+
  geom_abline(intercept = 0, slope = 1, size = 0.5, lty = 2, color = "brown3") +
  labs(title = "Cartera en PAR10", y = "Aprobación General",
       x = 'Caretera en PAR 10\n"Con riesgo de moras iguales o mayores a 10 días"',
       color ="Umbral en PAR10", caption = "[---] Línea de 45° grados (referncia)")+
  theme_minimal()+ 
  theme(text = element_text(family = "serif"),
                         plot.title = element_text(hjust = 0.5),
                         plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(color = "brown3"))



a0_raw %>% mutate(across(PAR10:acc_par90,~./100),
                  iteracion = row_number()) %>% 
  ggplot(aes(precision_nopar30_g, porcentaje_aprobacion, color = PAR30))+
  geom_point() +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))+
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10))+
  geom_abline(intercept = 0, slope = 1, size = 0.5, lty = 2, color = "brown3") +
  labs(title = "Cartera en PAR30", y = "Aprobación General",
       x = 'Caretera en PAR 30\n"Con riesgo de moras iguales o mayores a 30 días"',
       color ="Umbral en PAR30", caption = "[---] Línea de 45° grados (referncia)")+
  theme_minimal()+ 
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(color = "brown3"))



a0_raw %>% mutate(across(PAR10:acc_par90,~./100),
                  iteracion = row_number()) %>% 
  ggplot(aes(precision_nopar60_g, porcentaje_aprobacion, color = PAR60))+
  geom_point() +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))+
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10))+
  geom_abline(intercept = 0, slope = 1, size = 0.5, lty = 2, color = "brown3") +
  labs(title = "Cartera en PAR60", y = "Aprobación General",
       x = 'Caretera en PAR 60\n"Con riesgo de moras iguales o mayores a 60 días"',
       color ="Umbral en PAR60", caption = "[---] Línea de 45° grados (referncia)")+
  theme_minimal()+ 
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(color = "brown3"))



a0_raw %>% mutate(across(PAR10:acc_par90,~./100),
                  iteracion = row_number()) %>% 
  ggplot(aes(precision_nopar90_g, porcentaje_aprobacion, color = PAR90))+
  geom_point() +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))+
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10))+
  geom_abline(intercept = 0, slope = 1, size = 0.5, lty = 2, color = "brown3") +
  labs(title = "Cartera en PAR90", y = "Aprobación General",
       x = 'Caretera en PAR 90\n"Con riesgo de moras iguales o mayores a 90 días"',
       color ="Umbral en PAR90", caption = "[---] Línea de 45° grados (referncia)")+
  theme_minimal()+ 
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(color = "brown3"))


stargazer::stargazer(
lm(porcentaje_aprobacion~precision_nopar10_g+
     precision_nopar30_g+precision_nopar60_g+precision_nopar90_g,
   a0_raw), type = "text")





## 3.1. Presision INVERSA
### 3.2.1. Base ----
a2_inv_presision <- a1_prefiltros %>% 
  mutate(iteracion = row_number()) %>% 
  pivot_longer(!iteracion, names_to = "parametros", values_to = "valor") %>% 
  # Específico
  filter(str_detect(parametros, "nopar")) %>% 
  mutate(PAR = paste0("PAR ", str_extract(parametros,"[:digit:]+")),
         tipo = ifelse(str_detect(parametros,"g$"),
                       "II. Final","I. Inicial"),
         valor = valor/100) 

### 3.2.2. Plot ----

pt1_presision <- a2_inv_presision %>% ggplot(aes(PAR,valor, color = tipo))+
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



pt1_presision




## 3.2. Presisión ----
### 3.2.1. Base ----
a3_seneitivity <- a1_prefiltros %>% 
  mutate(iteracion = row_number()) %>% 
  pivot_longer(!iteracion, names_to = "parametros", values_to = "valor") %>% 
  # Específico
  filter(str_detect(parametros, "pre")) %>% 
  mutate(PAR = paste0("PAR ", str_extract(parametros,"[:digit:]+")),
         tipo = ifelse(str_detect(parametros,"g$"),
                       "II. Final","I. Inicial"),
         valor = valor/100) 

### 3.2.2. Plot ----

pt1_presision <- a3_seneitivity %>% ggplot(aes(PAR,valor, color = tipo))+
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







## 3.3. Umbrales ----
### 3.3.1. Bases ----
a4_umbrales <- a1_prefiltros %>% 
  select(iteracion, starts_with("PAR")) %>% 
  pivot_longer(!iteracion, names_to = "PAR",values_to = "valor") %>% 
  mutate(valor = valor/100)

### 3.2.2. Plot ----
n_fun <- function(x){
  return(data.frame(
    y = mean(x), label = paste0(round(mean(x)*100, 2),"%")  
    ))
}


pt2_umbrales<- a4_umbrales %>% ggplot(aes(valor, PAR))+
  geom_boxplot(color = "cyan4", alpha = 0.7, outlier.shape = NA, lwd = 0.3)+
  stat_summary(fun.y = mean, color ="firebrick", size = 0.1)+
  stat_summary(fun.data = n_fun, geom = "text",
               family = "serif", size =2, color = "firebrick", 
               position = position_nudge(y = -0.06,x = 0.04))+
  
  geom_vline(xintercept = mean(a4_umbrales$valor, na.rm = T),
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



