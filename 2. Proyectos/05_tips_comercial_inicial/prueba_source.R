#Tips comercial
library(dplyr)
library(ggplot2)
library(stargazer)
library(bigrquery)



# 1. Cargar datos ----

## 1.1. Traing set con PAR's ----
a0a_ts_raw <- read.csv("02_Training_set_2022-6-16 (1).csv") %>% 
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
  ### 2.2.1. Arreglo de variables ----
mutate(
  # Dueño de negocio
  is_business_owner = ifelse(is_business_owner == "true",T,F), 
  # Camara de comercio
  is_registered_in_chamber_commerce = ifelse(is_registered_in_chamber_commerce=="true",T,F),
  # Años de experiiencia
  business_works_since_year = as.numeric(business_works_since_year)) %>% 
  filter(between(business_works_since_year,1950,2022)) %>% 
  mutate(business_works_since_year = 2022-business_works_since_year,
         # Cambio de actividad último año
         change_business_activity_last_year = ifelse(change_business_activity_last_year=="true",T,F),
         # % de ventas Nequi
         nequi_sales_percentage =
           as.numeric(gsub("%","",nequi_sales_percentage)),
         # Empleados y empleados de la familia
         across(number_of_employees:number_of_family_employees,~as.numeric(.))) %>% 
  filter(number_of_employees <= 50, number_of_family_employees<= 50) %>%
  mutate(
    # Ventas mensuales
    monthly_sales = as.numeric(gsub("\\.||Más de ", "",monthly_sales))/10^6,
    # Clientes conocidos por nombre
    number_of_customers_known_by_nickname = as.numeric(number_of_customers_known_by_nickname),
    # Colados en TM
    fictitious_situation_02 = as.numeric(fictitious_situation_02),
    
    # Prioridades para crecer
    growth_actions_priority = stringr::str_replace_all(
      stringr::str_extract(growth_actions_priority,
                           '"(.*?)"'), "[^[:alnum:]]", ""),
    # Capacidad autopercibida para recuperase
    difficulty_achieving_current_progress = as.numeric(difficulty_achieving_current_progress),
    # AUtocalificaicón como comerciante
    qualification_as_merchant = as.numeric(qualification_as_merchant),
    # Pioriradad en metas
    goals_priority = stringr::str_replace_all(
      stringr::str_extract(goals_priority,
                           '"(.*?)"'), "[^[:alnum:]]", ""),
    # Ingresos del hogar
    household_income = as.numeric(gsub("\\.||Más de ", "",household_income))/10^6,
    # Personas en el hogar
    across(number_of_household_members:number_of_household_contributors,
           ~as.numeric(.)),
    # Variables de Seguridad Social y Prestamos
    across(is_health_taxpayer:had_other_loans, 
           ~ifelse(.=="SI",T,F)),
    # Creditos vigente y reporte en centrales (autoreportado)
    across(has_current_loans:is_reported_in_credit_bureaus,
           ~ifelse(.=="true",T,F)),
    # Probabilidad de atrasao autoreportada
    probability_of_late_payment = as.numeric(probability_of_late_payment)) %>% 
  ### 2.2.2. Adición de nuevas variables ----
# Semana 
mutate(
  semana = case_when(
    as.numeric(substr(created_at,9,10)) <= 7~"Semana_1",
    as.numeric(substr(created_at,9,10)) > 7 &
      as.numeric(substr(created_at,9,10)) <= 14~"Semana_2",
    as.numeric(substr(created_at,9,10)) > 14 &
      as.numeric(substr(created_at,9,10)) <= 21~"Semana_3",
    T~"Semana_4"),
  # Dependencia económica
  dependencia = number_of_household_contributors/number_of_household_members) %>% 
  filter(!is.infinite(dependencia), !is.nan(dependencia)) %>% 
  .[complete.cases(.),]


rm(a0a_ts_raw,a0b_new_form)



#####



