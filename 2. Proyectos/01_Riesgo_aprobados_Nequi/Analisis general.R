  library(dplyr)
  library(ggplot2)
  library(bigQueryR)
  library(bigrquery)

setwd("C:\\Users\\guibo\\Desktop\\urgencia NEQUI")

# 1. Extracción de solicitudes cruzadas Nequi-Cívico ----

a1_nequi <- read.csv("clientes_civico_nequi_preaprobados_2022_06.csv")


a2_credit_civico <- bq_table_download(
  bq_project_query(
    "atlas-323415",
    "SELECT  * FROM `atlas-323415.z01_atlas_operacion.ao01_solicitudes_caracterizadas`")) %>% 
  as.data.frame() %>% 
  rename(id = customer_id) 

sum(a1_nequi$id %in% a2_credit_civico$customer_id)


names(a1_nequi)
pr <- names(a2_credit_civico)
pr[c(1,40,41,87,91,131)]

# Clientes de Nequi en Base créditos (con solicitud)
a3_nequi_civico <- a1_nequi %>% 
  filter(id %in% a2_credit_civico$customer_id)

# Solicitudes con ID en
a4_solicitudes_nequi <- a2_credit_civico %>% filter(customer_id %in% a1_nequi$id) %>% 
  filter(!is.na(LN05_number_of_employees)) %>% 
  group_by(customer_id) %>% 
  mutate(repeticiones = n()) %>% 
  as.data.frame()

# Enviados a Nequi con perfil nuevo
a5_nequi_nuevos <- a1_nequi %>% 
  filter(id %in% a4_solicitudes_nequi$customer_id)


write.csv(a5_nequi_nuevos,"Solicitudes_NEQUI_urgencia.csv", row.names = T)


table(a5_nequi_nuevos$preaprobado)/nrow(a5_nequi_nuevos)

table(a1_nequi$preaprobado)/nrow(a1_nequi)
table(a3_nequi_civico$preaprobado)/nrow(a3_nequi_civico)



# PAR 30 Civico-Nequi: 2.6%
# PAR 30 Civico: 8.1%
# PAR 30 NEqui: 4.5%


# 2. Análisis de resultados:


a1_base <- read.csv("prediction_nequi_estadistico.csv")

a1_base <- a1_base %>% 
  select(id, cliente_nequi, preaprobado, monto_preaprobado,
         Prob_PAR30,Prob_PAR60,Prob_PAR90, Aprobado_Final) %>% 
  filter(!is.na(Prob_PAR30)) %>% 
  arrange(Prob_PAR30) %>% 
  mutate(inde_30 = row_number())%>% 
  arrange(Prob_PAR60) %>% 
  mutate(inde_60 = row_number())%>% 
  arrange(Prob_PAR90) %>% 
  mutate(inde_90 = row_number())

## 2.1. Valores de referencia

# min
a1_base %>% group_by(preaprobado) %>% 
  summarise(across(Prob_PAR30:Prob_PAR90,~min(.,na.rm = T)))
# preaprobado                    Prob_PAR30 Prob_PAR60 Prob_PAR90
# <chr>                               <dbl>      <dbl>      <dbl>
#   1 No tiene preaprobado Propulsor     0.0933     0.0467     0.0233
# 2 Tiene preaprobado Propulsor        0.207      0.117      0.07  

# mean
a1_base %>% group_by(preaprobado) %>% 
  summarise(across(Prob_PAR30:Prob_PAR90,~mean(.,na.rm = T)))
# preaprobado                    Prob_PAR30 Prob_PAR60 Prob_PAR90
# <chr>                               <dbl>      <dbl>      <dbl>
#   1 No tiene preaprobado Propulsor      0.354      0.253     0.145 
# 2 Tiene preaprobado Propulsor         0.331      0.178     0.0800

# Max
a1_base %>% group_by(preaprobado) %>% 
  summarise(across(Prob_PAR30:Prob_PAR90,~max(.,na.rm = T)))
# preaprobado                    Prob_PAR30 Prob_PAR60 Prob_PAR90
# <chr>                               <dbl>      <dbl>      <dbl>
#   1 No tiene preaprobado Propulsor      0.880      0.867     0.437 
# 2 Tiene preaprobado Propulsor         0.417      0.283     0.0933



## 2.2. Gráficas
a1_base %>% ggplot(aes(Prob_PAR30, inde_30, color = preaprobado))+
  geom_point() +
  geom_vline(xintercept = c(0.0933, 0.331,0.880), lty = c(2,1,2), color= "red")+
  scale_x_continuous(labels = scales::percent_format()) +
  labs(title = "Distribución de riesgo PAR 30")+
  theme_minimal()

pr <- data.frame(
  aumento_riesgo = numeric(),
  aprobacion_par30 = numeric(),
  aprobacion_par60 = numeric(),
  aprobacion_par60 = numeric())

for (i in (seq(-30,100)/100)) {
  
  x <- data.frame(
    aumento_riesgo = i*-1, 
    aprobacion_par30 = mean(a1_base$Prob_PAR30 < 0.331-(0.331*i)),
    aprobacion_par60 = mean(a1_base$Prob_PAR60 < 0.178-(0.178*i)),
    aprobacion_par90 = mean(a1_base$Prob_PAR90 < 0.0800-(0.0800*i))
    )
  
  pr <- rbind(pr, x)
    
}


pr %>% ggplot(aes(aumento_riesgo,aprobacion_par30))+
  geom_path(color = "cyan4", lwd = 1)+
  geom_vline(xintercept = c(0),lty = 2)+
  geom_hline(yintercept = c(mean(a1_base$Prob_PAR30 <  0.331),
                            0.02),lty = 2,
             color = c("black","red"))+
  geom_hline(yintercept = c(0.1,0.05),lty = 1, color = "green")+
  geom_vline(xintercept = c(-0.4,-0.28),lty = 1, color = "green")+
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 13))+
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10))+
  labs(x = "Aumento porcentual del umbral de riesgo \n(actualmente del 33.1%)",
       y = "Aprobación\n(actualmente del 2.1% aprox)",
       title = "Análisis de los niveles de aprobación de Nequi (PAR 30)",
       subtitle = "Evaluación de reisgo de aprobaciones Nequi desde el score Cívico") +
  theme_minimal()+
  theme(text = element_text(size = 8))

ggsave("Anslisis_Par30.png")

pr %>% ggplot(aes(aumento_riesgo,aprobacion_par60))+
  geom_path(color = "cyan4", lwd = 1)+
  geom_vline(xintercept = c(0),lty = 2)+
  geom_hline(yintercept = c(0.208,
                            0.02),lty = 2,
             color = c("black","red"))+
  geom_hline(yintercept = c(0.1,0.05),lty = 1, color = "green")+
  geom_vline(xintercept = c(-0.4,-0.25),lty = 1, color = "green")+
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 13))+
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10))+
  labs(x = "Aumento porcentual del umbral de riesgo \n(actualmente del 17.8%)",
       y = "Aprobación\n(actualmente del 2.1% aprox)",
       title = "Análisis de los niveles de aprobación de Nequi (PAR 60)",
       subtitle = "Evaluación de reisgo de aprobaciones Nequi desde el score Cívico") +
  theme_minimal()+
  theme(text = element_text(size = 8))

ggsave("Anslisis_Par60.png")

pr %>% ggplot(aes(aumento_riesgo,aprobacion_par90))+
  geom_path(color = "cyan4", lwd = 1)+
  geom_vline(xintercept = c(0),lty = 2)+
  geom_hline(yintercept = c(0.203,
                            0.02),lty = 2,
             color = c("black","red"))+
  geom_hline(yintercept = c(0.1,0.05),lty = 1, color = "green")+
  geom_vline(xintercept = c(-0.33,-0.16),lty = 1, color = "green")+
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 13))+
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10))+
  labs(x = "Aumento porcentual del umbral de riesgo \n(actualmente del 8%)",
       y = "Aprobación\n(actualmente del 2.1% aprox)",
       title = "Análisis de los niveles de aprobación de Nequi (PAR 90)",
       subtitle = "Evaluación de reisgo de aprobaciones Nequi desde el score Cívico") +
  theme_minimal()+
  theme(text = element_text(size = 8))
0.0800
ggsave("Anslisis_Par90.png")
