# Análisis fondo de garantias
library(dplyr)
library(bigrquery)
library(ggplot2)



# 1. Cargar ingormación ----
a0_raw <- bq_table_download(bq_project_query(
  ## A. Nombre del proyecto
  "atlas-323415",
  
  ## B. Consulta SQL
  ("SELECT  * FROM `atlas-323415.z01_atlas_operacion.ao02_comecial_cliente_ideal`"))) 


# 2. Arreglo inicial de la base ----
a1_clietes <- a0_raw %>% 
  select(PAR30 = SA00_Prob_PAR30, 
         PAR60 =SA01_Prob_PAR60,
         PAR90 = SA02_Prob_PAR90, date =LF00_created_at) %>% 
  arrange(date) %>% 
  mutate(date.index = row_number()) %>% 
  arrange(PAR30) %>% 
  mutate(index.30 = row_number()) %>% 
  arrange(PAR60) %>% 
  mutate(index.60 = row_number()) %>% 
  arrange(PAR90) %>% 
  mutate(index.90 = row_number(),
         month = substr(date,1,7),
         trimestre = 
           ifelse(lubridate::month(date) <= 3,"Trimestre 1",
                  "Trimestre 2"))

# 3. Gráficas iniciales ----
## 3.1. Acumulación de créditos por fecha ----
a1_clietes %>% ggplot(aes(date, date.index)) +
  stat_ecdf(geom = "step", color = "red")+
  theme_minimal()

## 3.2. Distribució de riesgos por tipo de PAR ----
pt <- ggpubr::ggarrange(
  a1_clietes %>% ggplot(aes(PAR30)) +
    stat_ecdf(geom = "step", color = "cyan3", lwd = 1)+
    geom_vline(xintercept = c(mean(a1_clietes$PAR30, na.rm = T),
                              median(a1_clietes$PAR30, na.rm = T)),
               lty = c(1,2), color = "cyan3", alpha = 0.5)+
    scale_x_continuous(labels = scales::percent_format(),
                       breaks = scales::pretty_breaks(n = 10))+
    scale_y_continuous(labels = scales::percent_format())+
    labs(title = "PAR 30", x = "", y ="Población")+
    theme_minimal()+theme(text = element_text(family = "serif"),
                          plot.title = element_text(hjust = 0.5)),
  
  a1_clietes %>% ggplot(aes(PAR60)) +
    stat_ecdf(geom = "step", color = "brown3", lwd = 1)+
    geom_vline(xintercept = c(mean(a1_clietes$PAR60, na.rm = T),
                              median(a1_clietes$PAR60, na.rm = T)),
               lty = c(1,2), color = "brown3", alpha = 0.5)+
    scale_x_continuous(labels = scales::percent_format(),
                       breaks = scales::pretty_breaks(n = 10))+
    scale_y_continuous(labels = scales::percent_format())+
    labs(title = "PAR 60", x = "Riesgo de NO PAGO", y ="")+
    theme_minimal()+theme(text = element_text(family = "serif"),
                          plot.title = element_text(hjust = 0.5)),
  a1_clietes %>% ggplot(aes(PAR90)) +
    stat_ecdf(geom = "step", color = "purple", lwd = 1)+
    geom_vline(xintercept = c(mean(a1_clietes$PAR90, na.rm = T),
                              median(a1_clietes$PAR90, na.rm = T)),
               lty = c(1,2), color = "purple", alpha= 0.5)+
    scale_x_continuous(labels = scales::percent_format(),
                       breaks = scales::pretty_breaks(n = 10))+
    scale_y_continuous(labels = scales::percent_format())+
    labs(title = "PAR 90", x = "", y ="")+
    theme_minimal()+theme(text = element_text(family = "serif"),
                          plot.title = element_text(hjust = 0.5)),
  nrow = 1)

pt
ggsave("2. Proyectos/03_Fondo_de_garantias/g01_Riesgos_acumulados.png",
       pt, h = 10, w = 20)


# 4. Análisis de distribución del riesgo ----

## 4.1. Análisis Mensual ----

### 4.1.1. Panel mensual ----
a2_panel_mensual <- a1_clietes %>% 
  select(month, trimestre, PAR30, PAR60, PAR90) %>% 
  reshape2::melt(id.vars = c("month","trimestre"), variable.name = "PAR",
                 value.name = "riesgo") %>% 
  mutate(rango = case_when(
    riesgo <= 0.1 ~ "de 0% a 10% (Muy Bajo)",
    riesgo > 0.2 & riesgo <= 0.3 ~ "de 10% a 20%",
    riesgo > 0.3 & riesgo <= 0.4 ~ "de 20% a 30%",
    riesgo > 0.4 & riesgo <= 0.5 ~ "de 30% a 40%",
    riesgo > 0.5 & riesgo <= 0.6 ~ "de 40% a 50%",
    riesgo > 0.6 & riesgo <= 0.7 ~ "de 50% a 60%",
    riesgo > 0.7 & riesgo <= 0.8 ~ "de 60% a 70%",
    riesgo > 0.8 & riesgo <= 0.9 ~ "de 70% a 80%",
    T ~ "de 90% a 100% (Muy Alto)")) %>%
  # Agrupación
  group_by(PAR, month) %>%
  mutate(tamano.rango = n()) %>% 
  as.data.frame() %>% 
  group_by(PAR, month, rango) %>% 
  summarise(n = n(),
            tamano.rango = mean(tamano.rango)) %>% 
  mutate(p = n/tamano.rango) %>% as.data.frame()


### 4.1.2. Gráficación mensual ----
pt <- ggpubr::ggarrange(
a2_panel_riesgo %>% filter(PAR == "PAR30") %>% 
  ggplot(aes(month, rango, fill = p))+
  geom_tile() +
  scale_fill_gradient(high = c("red","red2","red4"),
                      low = c("cyan4","cyan3")) +
  geom_text(aes(label =  paste0(round(p*100,1),"%"),
    family = "serif",), color = "white") +
  labs(title = "", subtitle = "PAR 30", x = "Mes", y = "Nivel de riesgo")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "none",plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color ="grey50")),

a2_panel_riesgo %>% filter(PAR == "PAR60") %>% 
  ggplot(aes(month, rango, fill = p))+
  geom_tile() +
  scale_fill_gradient(high = c("red","red2","red4"),
                      low = c("cyan4","cyan3")) +
  geom_text(aes(label =  paste0(round(p*100,1),"%"),
                family = "serif",), color = "white") +
  labs(title = "Distribución mensual de créditos por niveles de riesgo",
       subtitle =   "PAR 60", x = "Mes", y = " ")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "none",plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color ="grey50")),

a2_panel_riesgo %>% filter(PAR == "PAR90") %>% 
  ggplot(aes(month, rango, fill = p))+
  geom_tile() +
  scale_fill_gradient(high = c("red","red2","red4"),
                      low = c("cyan4","cyan3")) +
  geom_text(aes(label =  paste0(round(p*100,1),"%"),
                family = "serif",), color = "white") +
  labs(title = "", subtitle = "PAR 90", x = "Mes", y = " ")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "none",plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color ="grey50")), nrow = 1)
pt
ggsave("2. Proyectos/03_Fondo_de_garantias/g02_Riesgos_mensualisados.png",
       pt, h = 10, w = 20)


## 4.2. Análisis Trimestral ----

### 4.2.1. Panel trimestral ----
a3_panel_trimestral <- a1_clietes %>% 
  select(month, trimestre, PAR30, PAR60, PAR90) %>% 
  reshape2::melt(id.vars = c("month","trimestre"), variable.name = "PAR",
                 value.name = "riesgo") %>% 
  mutate(rango = case_when(
    riesgo <= 0.1 ~ "de 0% a 10% (Muy Bajo)",
    riesgo > 0.2 & riesgo <= 0.3 ~ "de 10% a 20%",
    riesgo > 0.3 & riesgo <= 0.4 ~ "de 20% a 30%",
    riesgo > 0.4 & riesgo <= 0.5 ~ "de 30% a 40%",
    riesgo > 0.5 & riesgo <= 0.6 ~ "de 40% a 50%",
    riesgo > 0.6 & riesgo <= 0.7 ~ "de 50% a 60%",
    riesgo > 0.7 & riesgo <= 0.8 ~ "de 60% a 70%",
    riesgo > 0.8 & riesgo <= 0.9 ~ "de 70% a 80%",
    T ~ "de 90% a 100% (Muy Alto)")) %>%
  # Agrupación
  group_by(PAR, trimestre) %>%
  mutate(tamano.rango = n()) %>% 
  as.data.frame() %>% 
  group_by(PAR, trimestre, rango) %>% 
  summarise(n = n(),
            tamano.rango = mean(tamano.rango)) %>% 
  mutate(p = n/tamano.rango) %>% as.data.frame()


### 4.2.2. Gráficación mensual ----
pt <- ggpubr::ggarrange(
  a3_panel_trimestral %>% filter(PAR == "PAR30") %>% 
    ggplot(aes(trimestre, rango, fill = p))+
    geom_tile() +
    scale_fill_gradient(high = c("red","red2","red4"),
                        low = c("cyan4","cyan3")) +
    geom_text(aes(label =  paste0(round(p*100,1),"%"),
                  family = "serif",), color = "white") +
    labs(title = "", subtitle = "PAR 30", x = "Trimestre", y = "Nivel de riesgo")+
    theme_minimal()+
    theme(text = element_text(family = "serif"),
          legend.position = "none",plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color ="grey50")),
  
  a3_panel_trimestral %>% filter(PAR == "PAR60") %>% 
    ggplot(aes(trimestre, rango, fill = p))+
    geom_tile() +
    scale_fill_gradient(high = c("red","red2","red4"),
                        low = c("cyan4","cyan3")) +
    geom_text(aes(label =  paste0(round(p*100,1),"%"),
                  family = "serif",), color = "white") +
    labs(title = "Distribución trimestral de créditos por niveles de riesgo",
         subtitle =   "PAR 60", x = "Trimestre", y = " ")+
    theme_minimal()+
    theme(text = element_text(family = "serif"),
          legend.position = "none",plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color ="grey50")),
  
  a3_panel_trimestral %>% filter(PAR == "PAR90") %>% 
    ggplot(aes(trimestre, rango, fill = p))+
    geom_tile() +
    scale_fill_gradient(high = c("red","red2","red4"),
                        low = c("cyan4","cyan3")) +
    geom_text(aes(label =  paste0(round(p*100,1),"%"),
                  family = "serif",), color = "white") +
    labs(title = "", subtitle = "PAR 90", x = "Trimestre", y = " ")+
    theme_minimal()+
    theme(text = element_text(family = "serif"),
          legend.position = "none",plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color ="grey50")), nrow = 1)
pt
ggsave("2. Proyectos/03_Fondo_de_garantias/g04_Riesgos_trimestrales.png",
       pt, h = 10, w = 20)
















# 5. Exportacion de tablas resultado ----

f1_riesgos_mes <- 
  a2_panel_riesgo %>% 
  reshape2::dcast(PAR+month~rango, value.var = "p", fill = 0) %>% 
  arrange(month,PAR)
write.csv(f1_riesgos_mes, "3. Datos/T01_resultados_mensuales.csv", row.names = F)

f2_riesgos_trimestre <-
  a3_panel_trimestral %>% 
  reshape2::dcast(PAR+trimestre~rango, value.var = "p", fill = 0) %>% 
  arrange(trimestre,PAR)
write.csv(f2_riesgos_trimestre, "3. Datos/T02_resultados_trimestre.csv", row.names = F)





