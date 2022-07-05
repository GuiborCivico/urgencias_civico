#Tips comercial
library(dplyr)
library(ggplot2)
library(stargazer)
library(bigrquery)
rrport <- function(df, variable) {
  variable <- substitute(variable)
  if(is.symbol(variable)) variable <- deparse(variable)
  
  message(
    paste0(
      as.character(variable),"\n\n",
      "Tipo: ",
      class(df[,variable]), "\n",
      
      "Completitud: ",
      round((mean(!is.na(df[,variable]))*100),1), "%")
  )
  
  if (is.character(df[,variable]) == T) {
    message("\n--------\n","Número de niveles: ",
            nlevels(as.factor(df[,variable])),
            "\nTres priemros niveles: ",
            "\n   1: ",
            levels(as.factor(df[,variable]))[1],
            "\n   2: ",
            levels(as.factor(df[,variable]))[2],
            "\n   3: ",
            levels(as.factor(df[,variable]))[3])
  }
  
  if (is.numeric(df[,variable])) {
    x <- df[,variable]
    x <- data.frame(
      val = x)
    message("Promedio = ", 
            round(mean(df[,variable], na.rm = T),1), "\n",
            "Mediana = ", 
            round(median(df[,variable], na.rm = T),1),"\n",
            "Std = ", 
            round(sd(df[,variable], na.rm = T)),1)
    
    ggpubr::ggarrange(
      
      x %>% ggplot(aes(val))+
        geom_histogram(alpha = 0.7)+
        geom_vline(xintercept = mean(x$val, na.rm = T),lty=2,color ="red")+
        labs(title = paste0(as.character(variable),"\nReporte general"))+
        theme_minimal(),
      
      x %>% ggplot(aes(val))+
        geom_boxplot()+
        theme_minimal(), nrow  = 1)
  }
  
}



# 1. Cargar datos
a0_ts_raw <- read.csv("3. Datos/02_Training_set_2022-6-16 (1).csv") %>% 
  mutate(PAR15 = ifelse(IM08_PAR15 >= 1, 1,0),
         PAR30 = ifelse(IM09_PAR30 >= 1, 1,0),
         PAR60 = ifelse(IM10_PAR60 >= 1, 1,0),
         PAR90 = ifelse(IM11_PAR90 >= 1, 1,0)) %>% 
  rename(customer_id = IM00_id_cliente) %>% 
  select(customer_id, starts_with("PAR"),CU07_contact_gender,CU06_birth_date) %>% 
  mutate(across(customer_id:CU06_birth_date, ~ifelse(. == "",NA,.))) %>% 
  mutate(edad = as.numeric((Sys.Date()-as.Date(CU06_birth_date))/365))




b0_new_form <- bq_table_download(bq_project_query(
  ## A. Nombre del proyecto
  "atlas-323415",
  
  ## B. Consulta SQL
  ("SELECT * FROM `atlas-323415.raw_data.loan_form_new`"))) 


pr <- merge(a0_ts_raw, b0_new_form, by = "customer_id", all.y = T) %>% 
  distinct(customer_id, .keep_all = T) %>% 
  filter(!is.na(PAR15)) %>% 
  mutate(antiguedad = as.numeric(Sys.Date()-as.Date(created_at))) %>% 
  # FILTRO POR ANTIGUEDAD
  filter(antiguedad >= 60)  %>% 
  ## ## ## ## ## ## #
  mutate(nequi_sales_percentage =
           as.numeric(gsub("%","",nequi_sales_percentage)))  
  
  
pr <- pr[complete.cases(pr), ]



pr <- pr %>% 
  mutate(nequi_sales_percentage = as.numeric(gsub("%","",nequi_sales_percentage)))







#####
mmtest <- function(base, model, umbral) {
  base$riesgo <- predict(model, base, type = "response")
  base <- base %>%
  mutate(prediccion = ifelse(riesgo >= umbral,"P.1","P.0"))
  base
  
}

# 1. Modelación
m1 <- #pr %>% 
  glm(PAR15~antiguedad+
      edad+CU07_contact_gender+nequi_sales_percentage+operation_type, pr, family = "binomial")

# 2. Revisión
pr %>% 
  stargazer(m1, type = "text")



# 3. Imputación y validación
ppar_15(0.2)



ppar_15 <- function(umbral_) {
  pr <- mmtest(pr, m1, umbral_)
  message(
    "Aprobación esperada = ", 100-round(100*mean(pr$PAR15),2),"%\n",
    "Aprobación final = ", round(100*mean(pr$riesgo < umbral_),2),"%\n",
  paste0(
    "Acuarcy = ",
    round(
      100*(table(pr$PAR15,pr$prediccion)[1]+
             table(pr$PAR15,pr$prediccion)[4])/nrow(pr),2),"%  ||  ",
    
    "Sensitivity = ",
    round(
      100*(table(pr$PAR15,pr$prediccion)[4]/
             (table(pr$PAR15,pr$prediccion)[4]+
                table(pr$PAR15,pr$prediccion)[2])),2),"%  ||  ", 
    
    "Specificity = ",
    round(
      100*(table(pr$PAR15,pr$prediccion)[1]/
             (table(pr$PAR15,pr$prediccion)[1]+
                table(pr$PAR15,pr$prediccion)[3])),2),"%"))
}


round((table(pr$PAR15)/nrow(pr))*100,2)
round((table(pr$PAR30)/nrow(pr))*100,2)
round((table(pr$PAR60)/nrow(pr))*100,2)




















rrport(a0_ts_raw, IM01_plazo)

# Categóricas
rrport(a0_ts_raw, CU05_categories) # 562 niveles

rrport(a0_ts_raw, CU07_contact_gender) # 2 niveles

rrport(a0_ts_raw, SL11_inventory_method)
table(a0_ts_raw$SL11_inventory_method)


