library(googlesheets4)
library(dplyr)
library(ggplot2)
library(bigrquery)


a0_raw <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1p5-h2Vx_EpM3iCof1HZycuvhp79JgfPVYtb4dtNISTg/edit?usp=sharing") %>% 
  mutate(cuota = as.numeric(gsub("\\.|\\$|\\ ","",`Valor quota`)),
         corte = 
           case_when(
             as.numeric(substr(`Fecha desembolso`,9,10)) <=5~".A. primeros 5 dias del mes",
             as.numeric(substr(`Fecha desembolso`,9,10)) >5 & 
               as.numeric(substr(`Fecha desembolso`,9,10)) <=10~".B. del 5to al 10mo dia",
             
             as.numeric(substr(`Fecha desembolso`,9,10)) >10 & 
               as.numeric(substr(`Fecha desembolso`,9,10)) <=15~".C. del 10mo al 15vo dia",
             
             as.numeric(substr(`Fecha desembolso`,9,10)) >15 & 
               as.numeric(substr(`Fecha desembolso`,9,10)) <=20~".D. del 15vo al 20vo dia",
             
             as.numeric(substr(`Fecha desembolso`,9,10)) >20 & 
               as.numeric(substr(`Fecha desembolso`,9,10)) <=25~".E. del 20vo al 25vo dia",
             
             T ~".F. ultimos 5 a 6 dias del mes")) %>% 
  mutate(Capital.inicial.cien.MIL = `Capital inical`/(10^5),
         cuota.mil = cuota/1000,
         antiguedad = as.numeric(Sys.Date()-as.Date(`Fecha desembolso`)),
         dia.semana. = weekdays(as.Date(`Fecha desembolso`))) 


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
     `canal de venta`+corte+dia.semana.,a0_raw), 
lm(`Días de vencimiento`~Capital.inicial.cien.MIL+cuota.mil+Plazo+
     `canal de venta`+corte+dia.semana.,a0_raw),
type = "text", column.labels =  c("Control por antigüedad","SIN control por antigüedad"))



## 
a1_cli.ideal <- bq_table_download(bq_project_query(
  ## A. Nombre del proyecto
  "atlas-323415",
  
  ## B. Consulta SQL
  ("SELECT  * FROM `atlas-323415.z01_atlas_operacion.ao02_comecial_cliente_ideal`"))) 



a1_cli.ideal <- a1_cli.ideal %>%
  rename(Titular = customer_id) %>% 
  select(Titular, LF16_credit_bag, SA00_Prob_PAR30, SA01_Prob_PAR60, SA02_Prob_PAR90) %>% 
  distinct(Titular, .keep_all = T)





pr <- merge(a0_raw, a1_cli.ideal, by = "Titular", all.x = T) %>% 
  mutate(LF16_credit_bag = ifelse(is.na(LF16_credit_bag), "Sin bolsa", LF16_credit_bag))




round(table(pr$LF16_credit_bag)/nrow(pr),4)*100

pr %>% ggplot(aes(LF16_credit_bag, `Días de vencimiento`))+
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point",
               shape = 20, size = 4, color = "red")+
  labs(title = "Distribución de dias de atraso por bolsa",
       subtitle = "Sub-muestra de atrasados primera cuota de Mayo 2022",
       x = "Bolsa")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))






