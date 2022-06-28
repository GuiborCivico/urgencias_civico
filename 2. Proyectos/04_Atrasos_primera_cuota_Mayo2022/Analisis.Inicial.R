library(googlesheets4)
library(dplyr)
library(ggplot2)



a0_raw <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1p5-h2Vx_EpM3iCof1HZycuvhp79JgfPVYtb4dtNISTg/edit?usp=sharing") %>% 
  mutate(cuota = as.numeric(gsub("\\.|\\$|\\ ","",`Valor quota`)),
         corte = 
           case_when(
             as.numeric(substr(a0_raw$`Fecha desembolso`,9,10)) <=5~".A. primeros 5 dias del mes",
             as.numeric(substr(a0_raw$`Fecha desembolso`,9,10)) >5 & 
               as.numeric(substr(a0_raw$`Fecha desembolso`,9,10)) <=10~".B. del 5to al 10mo dia",
             
             as.numeric(substr(a0_raw$`Fecha desembolso`,9,10)) >10 & 
               as.numeric(substr(a0_raw$`Fecha desembolso`,9,10)) <=15~".C. del 10mo al 15vo dia",
             
             as.numeric(substr(a0_raw$`Fecha desembolso`,9,10)) >15 & 
               as.numeric(substr(a0_raw$`Fecha desembolso`,9,10)) <=20~".D. del 15vo al 20vo dia",
             
             as.numeric(substr(a0_raw$`Fecha desembolso`,9,10)) >20 & 
               as.numeric(substr(a0_raw$`Fecha desembolso`,9,10)) <=25~".E. del 20vo al 25vo dia",
             
             T ~".F. ultimos 5 a 6 dias del mes")) %>% 
  mutate(Capital.inicial.cien.MIL = `Capital inical`/(10^5),
         cuota.mil = cuota/1000,
         antiguedad = as.numeric(Sys.Date()-as.Date(a0_raw$`Fecha desembolso`)))


a0_raw %>% ggplot(aes(`Días de vencimiento`))+
  geom_density(color = "grey40", fill = "grey35", alpha = 0.4) +
  geom_vline(xintercept = c(mean(a0_raw$`Días de vencimiento`, na.rm = T),
                            median(a0_raw$`Días de vencimiento`, na.rm = T)),
             lty = c(1,2), color = "red")+
  labs(title = "Concetración de días de mora",
       subtitle = "Pagos primera cuota", x = "Días de vencimineto",
       y = "Densidad")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))



a0_raw %>%arrange(`Fecha desembolso`) %>% 
  ggplot(aes(`Fecha desembolso`)) +
  stat_ecdf(geom = "step", color = "cyan4")+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Acumulación por fechas",
       subtitle = "Créditos sin pago de primer cuota",
       y ="Acumulación (%)")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


a0_raw %>% ggplot(aes(`Capital inical`, `Días de vencimiento`))+
  geom_point() +
  scale_x_continuous(labels = scales::dollar_format()) +
  geom_smooth(color = "brown3") +
  geom_smooth(method = "lm", color = "cyan4")+
  labs(title = "Realación entre capital inicial y días de vencimiento",
       subtitle = "R2 = 0.009")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


a0_raw %>% ggplot(aes(cuota, `Días de vencimiento`))+
  geom_point() +
  scale_x_continuous(labels = scales::dollar_format()) +
  geom_smooth(color = "brown3") +
  geom_smooth(method = "lm", color = "cyan4")+
  labs(title = "Realación cuota inicial y días de vencimiento",
       subtitle = "")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))




stargazer::stargazer(
lm(`Días de vencimiento`~Capital.inicial.cien.MIL+cuota.mil+Plazo+antiguedad+
     `canal de venta`+corte,a0_raw), 
lm(`Días de vencimiento`~Capital.inicial.cien.MIL+cuota.mil+Plazo+
     `canal de venta`+corte,a0_raw),
type = "text", column.labels =  c("Control por antigüedad","SIN control por antigüedad"))



## 
Base <- bq_table_download(bq_project_query(
  ## A. Nombre del proyecto
  "insights-295219",
  
  ## B. Consulta SQL
  ("SELECT * FROM `insights-295219.landing_prod.entities`"))) 