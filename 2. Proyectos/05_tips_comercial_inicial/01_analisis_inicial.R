#Tips comercial
library(dplyr)
library(ggplot2)
library(stargazer)



# 1. Cargar datos
a0_ts_raw <- read.csv("3. Datos/02_Training_set_2022-6-16 (1).csv") %>% 
  mutate(PAR15 = ifelse(IM08_PAR15 >= 1, 1,0),
         PAR30 = ifelse(IM09_PAR30 >= 1, 1,0),
         PAR60 = ifelse(IM10_PAR60 >= 1, 1,0),
         PAR90 = ifelse(IM11_PAR90 >= 1, 1,0)) %>% 
  # Cambios general en las varaibles
  mutate(across(X:PAR90,~ifelse(. == "",NA,.))) %>% 
  # Cambios puntuales
  mutate(
    ## Cálculo de edades
    edad = as.numeric(((Sys.Date()-as.Date(CU06_birth_date)))/356))



a0_ts_raw %>% select(CU07_contact_gender + edad + CL02_request_term)

# a0_ts_raw %>% 
stargazer(glm(PAR15~CU07_contact_gender +edad, a0_ts_raw, family = "binomial"),
          glm(PAR30~CU07_contact_gender, a0_ts_raw, family = "binomial"),
          glm(PAR60~CU07_contact_gender, a0_ts_raw, family = "binomial"),
          glm(PAR90~CU07_contact_gender, a0_ts_raw, family = "binomial"),
          type = "text")






head(a0_ts_raw$CL02_request_term,5)
class(a0_ts_raw$CL02_request_term)
table(a0_ts_raw$CL02_request_term)
paste0("El ",
round((mean(is.na(
  a0_ts_raw$CL02_request_term # Cambio
  ))*100),2),"% de son NA")

hist(a0_ts_raw$edad)




