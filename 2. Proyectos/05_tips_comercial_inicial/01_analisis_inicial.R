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



# 1. Cargar datos ----

## 1.1. Traing set con PAR's ----
a0a_ts_raw <- read.csv("3. Datos/02_Training_set_2022-6-16 (1).csv") %>% 
  # calcular riesgo binario 
  mutate(PAR15 = ifelse(IM08_PAR15 >= 1, 1,0),
         PAR30 = ifelse(IM09_PAR30 >= 1, 1,0),
         PAR60 = ifelse(IM10_PAR60 >= 1, 1,0),
         PAR90 = ifelse(IM11_PAR90 >= 1, 1,0)) %>% 
  ### Preparar base para el merge
  rename(customer_id = IM00_id_cliente) %>% 
  select(customer_id, starts_with("PAR"),CU07_contact_gender,CU06_birth_date) %>% 
  mutate(across(customer_id:CU06_birth_date, ~ifelse(. == "",NA,.))) %>% 
  mutate(edad = as.numeric((Sys.Date()-as.Date(CU06_birth_date))/365))

## 1.2. Nueva solicitud (loan_form_new) ----
a0b_new_form <- bq_table_download(bq_project_query(
  ## A. Nombre del proyecto
  "atlas-323415",
  
  ## B. Consulta SQL
  ("SELECT * FROM `atlas-323415.raw_data.loan_form_new`"))) 



# 2. Creación de base final ----

## 2.1. Merge y filtros iniciales ----
# Condiciones: 
  # - Que no seaan "custumer_id" repetidos
  # - Que no tengan "NA" en los PAR's
  # - Más de 60 días de antiguedad
  # - Con todos los datos completos

a1_base <- merge(a0a_ts_raw, a0b_new_form, by = "customer_id", all.y = T) %>% 
  # Repetidos por ID
  distinct(customer_id, .keep_all = T) %>% 
  # Con NA en algun PAR
  filter(!is.na(PAR15)) %>%
  # Con antiguedad mayor a 60 días
  mutate(antiguedad = as.numeric(Sys.Date()-as.Date(created_at))) %>% 
  filter(antiguedad >= 60) %>% 
  # Con todos los datos completos
  .[complete.cases(.),]


## 2.2. Ediciones generales a la base ----



a1_base <- a1_base %>% 
  mutate(
    # Dueño de negocio
    is_business_owner = ifelse(is_business_owner == "true",T,F), 
    # Camara de comercio
    is_registered_in_chamber_commerce = ifelse(is_registered_in_chamber_commerce=="true",T,F),
    # Cambio de actividad último año
    change_business_activity_last_year = ifelse(change_business_activity_last_year=="true",T,F),
    # % de ventas Nequi
    nequi_sales_percentage =
           as.numeric(gsub("%","",nequi_sales_percentage)),
    #
    )

class(a1_base$business_works_since_year)
table(as.numeric(a1_base$business_works_since_year))

  mutate(nequi_sales_percentage =
           as.numeric(gsub("%","",nequi_sales_percentage)),
         household_income = as.numeric(gsub("\\.||Más de ", "",household_income))/100000,
         monthly_sales = as.numeric(gsub("\\.||Más de ", "",monthly_sales))/100000)  
  
  



pr %>%  mutate(number_of_family_employees = 
                 as.numeric(number_of_employees),
               number_of_employees = ifelse(number_of_employees >10,10,
                                            number_of_family_employees))





#####
mmtest <- function(base, model, umbral) {
  base$riesgo <- predict(model, base, type = "response")
  base <- base %>%
  mutate(prediccion = ifelse(riesgo >= umbral,"P.1","P.0"))
  base
  
}

# 1. Modelación

# 2. Revisión
pr %>% 
  stargazer(m1, type = "text")



################
# 1. Modelación
m1 <- #pr %>% 
  glm(PAR15~antiguedad+
        edad+CU07_contact_gender+civil_status+nequi_sales_percentage+operation_type+
        is_health_taxpayer+is_arl_taxpayer+is_health_taxpayer+is_pension_taxpayer+
        is_reported_in_credit_bureaus+has_current_loans+household_income+monthly_sales+
        preferred_investment,
      pr, family = "binomial")

pr %>% 
  stargazer(m1, type = "text")

# 3. Imputación y validación
ppar_15(0.3)



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





pr %>%  select(is_health_taxpayer, is_arl_taxpayer, is_health_taxpayer, 
               is_pension_taxpayer,has_current_loans,is_reported_in_credit_bureaus,
               ,civil_status, household_income, debtors_register, )


table(pr$is_reported_in_credit_bureaus)











rrport(a0_ts_raw, IM01_plazo)

# Categóricas
rrport(a0_ts_raw, CU05_categories) # 562 niveles

rrport(a0_ts_raw, CU07_contact_gender) # 2 niveles

rrport(a0_ts_raw, SL11_inventory_method)
table(a0_ts_raw$SL11_inventory_method)


