library(bigrquery)
library(dplyr)
library(ggplot2)

# 1. Cargar información
## 1.1. Caragar base de imputaciones de rechazados
a0_raw <- bq_table_download(bq_project_query(
  ## A. Nombre del proyecto
  "atlas-323415",
  
  ## B. Consulta SQL
  ("SELECT  * FROM `atlas-323415.z00_scores_creditos.sc01_loan_form_nequi_leads`")))

## 1.2. Base actuald de créditos IMIX
a0_IMIX_credits <-  read.csv("reportExport 08 de julio.csv")
a0_IMIX_credits 






# 2. Agrupación por ID custumer ----
a1_cooked <- a0_raw %>% group_by(customer_id) %>% 
  summarise(solicitudes = n(),
            first.date = min(as.Date(substr(created_at,1,10))),
            last.date = max(as.Date(substr(created_at,1,10))),
            procesadas = sum(!is.na(Prob_PAR10_2)),
            min_par10 = min(Prob_PAR10_2, na.rm = T),
            max_par10 = max(Prob_PAR10_2, na.rm = T),
            min_par30 = min(Prob_PAR30, na.rm = T),
            max_par30 = max(Prob_PAR30, na.rm = T),
            min_par60 = min(Prob_PAR60, na.rm = T),
            max_par60 = max(Prob_PAR60, na.rm = T)) %>% 
  as.data.frame() %>%  
  mutate(across(min_par10:max_par60,~ifelse(is.infinite(.),NA,.))) %>% 
  arrange(-procesadas) %>% 
  # Cruce con bae de IMIX 
  #   (dectar rechazados con desembolsos posteirores)
  mutate(Esta.IMIX = ifelse(customer_id %in% a0_IMIX_credits$Id.Cliente,T,F),
         antiguedad = as.numeric(Sys.Date()-last.date)) %>%
  # Detectando "Jineteo"
  mutate(Jineteando = ifelse(solicitudes >= 4,T,F)) 




pr <- a1_cooked %>% 
  mutate(intentos.jinete = ifelse(
    Esta.IMIX ==F &
      min_par10 != max_par10,T,F)) %>% 
  mutate(intentos.jinete = ifelse(is.na(intentos.jinete),F,intentos.jinete))



# 3. Filtros básicos ----
a3_Pre_final <- a1_cooked %>%
  ## 3.1. Sin imputación de riesgo
  filter(procesadas != 0) %>% 
  ## 3.2. Sin desembolsos históricos
  filter(Esta.IMIX == F) %>% 
  ## 3.3. Umbral de "Jineteo"
  filter(Jineteando == F)

sum(a1_cooked$procesadas ==0) # (604) Cuantos no tiene riesgo
sum(a1_cooked$Esta.IMIX ==1)  # (1703) Cuantos tuvieron/tiene desmbolos Cïvico 
sum(a1_cooked$Jineteando ==1) # (191) Cuantos estn jineteando

mean(a1_cooked$procesadas ==0) # (14.4%) Cuantos no tiene riesgo
mean(a1_cooked$Esta.IMIX ==1)  # (40.8%) Cuantos tuvieron/tiene desmbolos Cïvico 
mean(a1_cooked$Jineteando ==1) # (4.5%) Cuantos estn jineteando




# 4. Base con filtor de riesgo
a4_Final <- a3_Pre_final %>%
  mutate(Jinetando = ifelse(solicitudes >= 4,T,F)) %>% 
  mutate(Grupo = 
           case_when(
             between(max_par10,0,0.2)&between(max_par30,0,0.2)&
               between(max_par60,0,0.2)~"A1. Muy buenos",
             
             between(max_par10,0,0.3)&between(max_par30,0,0.3)&
               between(max_par60,0,0.3)~"A2. Buenos",
             
             between(max_par10,0,0.4)&between(max_par30,0,0.4)&
               between(max_par60,0,0.4)~"A3. Regulares",
             
             between(max_par10,0,0.6)&between(max_par30,0,0.6)&
               between(max_par60,0,0.6)~"A4. Malos",
             T ~"A5. Perversos"))


table(a4_Final$Grupo)
table(a4_Final$Grupo)/nrow(a4_Final)

write.csv(a4_Final %>% 
        filter(Grupo %in% c("A1. Muy buenos","A2. Buenos")) %>% 
          select(customer_id, Grupo, solicitudes, antiguedad, last.date),
      "Barrido_Inicial_A1_A2.csv", row.names = F)




# Agluans gráficas



a4_Final %>% ggplot(aes(Grupo, antiguedad))+
  geom_boxplot(outlier.shape = NA)+
  coord_cartesian(ylim = c(0,300))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  stat_summary(fun.y = mean, geom = "point", color = "orange",size = 3)+
  labs(title = "Distribución de antigüedad por grupos",
       subtitle = "Base histórica de rechazados",y = "Antigüedad (días)",
       caption = "[----] Promedio general (94.4 días de antigüedad)")+
  geom_hline(yintercept = mean(a4_Final$antiguedad),lty = 2, color = "cyan4")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5))
  
ggsave("Antiguedades_por_grupo_de_calidad.png")

  


