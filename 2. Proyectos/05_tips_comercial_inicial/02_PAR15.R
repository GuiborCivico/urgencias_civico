# Modealción par 15

source("2. Proyectos/05_tips_comercial_inicial/01_analisis_inicial.R")

# 1. Funciones de evaluación ----

mmtest <- function(base, model, umbral) {
  base$riesgo <- predict(model, base, type = "response")
  base <- base %>%
    mutate(prediccion = ifelse(riesgo >= umbral,"P.1","P.0"))
  base
} # Imputacion y definición
ppar_15 <- function(fuente,umbral_, model,model.name) {
  pr <- fuente
  pr <- mmtest(fuente, model, umbral_)
  
  data.frame(
    Modelo = model.name,
    Umbral = umbral_,
    Aprobacion.real = 1-mean(pr$PAR15),
    Aprobacion.final = mean(pr$riesgo < umbral_),
    Acuarcy = (table(pr$PAR15,pr$prediccion)[1]+
                 table(pr$PAR15,pr$prediccion)[4])/nrow(pr),
    Sensitivity =(table(pr$PAR15,pr$prediccion)[4]/
                    (table(pr$PAR15,pr$prediccion)[4]+
                       table(pr$PAR15,pr$prediccion)[2])),
    Specificity = (table(pr$PAR15,pr$prediccion)[1]/
                     (table(pr$PAR15,pr$prediccion)[1]+
                        table(pr$PAR15,pr$prediccion)[3])))
} # Evaluación PAR15


# 2. Modelación ----


## 2.1. Modelo básico ---- 
m1 <- glm(PAR15~antiguedad+semana, 
          family = "binomial", a1_base)
stargazer(m1, type = "text")
z1_ev_PAR15 <- ppar_15(a1_base, 0.2, m1, "01_basico")


## 2.2. Modelo persona ----
m2 <- glm(PAR15~antiguedad+semana+
            # Persona
            CU07_contact_gender+edad+civil_status+education_level+
            number_of_household_members+number_of_household_contributors+
            dependencia, 
          family = "binomial", a1_base)
stargazer(m1,m2, type = "text")
z1_ev_PAR15 <- rbind(z1_ev_PAR15,ppar_15(a1_base,0.2,m2,"02_persona"))

## 2.3. Modelo ingresos ----
m3 <- glm(PAR15~antiguedad+semana+
            # Persona
            CU07_contact_gender+edad+civil_status+education_level+
            number_of_household_members+number_of_household_contributors+
            dependencia+
            # Ingresos
            monthly_sales+nequi_sales_percentage+household_income, 
          family = "binomial", a1_base)
stargazer(m1,m2,m3, type = "text")
z1_ev_PAR15 <- rbind(z1_ev_PAR15,ppar_15(a1_base,0.2,m3,"03_ingresos"))


## 2.4. Modelo bancario ----
m4 <- glm(PAR15~antiguedad+semana+
            # Persona
            CU07_contact_gender+edad+civil_status+education_level+
            number_of_household_members+number_of_household_contributors+
            dependencia+
            # Ingresos
            monthly_sales+nequi_sales_percentage+household_income+
            # Bacario
            had_loans_with_family_or_friends+had_loans_with_lenders+
            had_loans_with_banks+had_other_loans+has_current_loans+
            is_reported_in_credit_bureaus, 
          family = "binomial", a1_base)
stargazer(m1,m2,m3,m4, type = "text")
z1_ev_PAR15 <- rbind(z1_ev_PAR15,ppar_15(a1_base,0.2,m4,"04_bancario"))

## 2.5. Modelo gestión ----
m5 <- glm(PAR15~antiguedad+semana+
            # Persona
            CU07_contact_gender+edad+civil_status+education_level+
            number_of_household_members+number_of_household_contributors+
            dependencia+
            # Ingresos
            monthly_sales+nequi_sales_percentage+household_income+
            # Bacario
            had_loans_with_family_or_friends+had_loans_with_lenders+
            had_loans_with_banks+had_other_loans+has_current_loans+
            is_reported_in_credit_bureaus+
            # Gestión
            inventory_method+accounting_method+debtors_register,
          family = "binomial", a1_base)
stargazer(m1,m2,m3,m4,m5, type = "text")
z1_ev_PAR15 <- rbind(z1_ev_PAR15,ppar_15(a1_base,0.2,m5,"05_gestion"))

## 2.6. Modelo formalidad ----
m6 <- glm(PAR15~antiguedad+semana+
            # Persona
            CU07_contact_gender+edad+civil_status+education_level+
            number_of_household_members+number_of_household_contributors+
            dependencia+
            # Ingresos
            monthly_sales+nequi_sales_percentage+household_income+
            # Bacario
            had_loans_with_family_or_friends+had_loans_with_lenders+
            had_loans_with_banks+had_other_loans+has_current_loans+
            is_reported_in_credit_bureaus+
            # Gestión
            inventory_method+accounting_method+debtors_register+
            # Fomalidad
            business_legal_type+is_registered_in_chamber_commerce+
            is_health_taxpayer+is_pension_taxpayer+is_arl_taxpayer+
            is_family_compensation_fund_taxpayer,
          
          family = "binomial", a1_base)
stargazer(m1,m2,m3,m4,m5,m6, type = "text")
z1_ev_PAR15 <- rbind(z1_ev_PAR15,ppar_15(a1_base,0.2,m6,"06_formalidad"))

## 2.7. Modelo comunidad ----
m7 <- glm(PAR15~antiguedad+semana+
            # Persona
            CU07_contact_gender+edad+civil_status+education_level+
            number_of_household_members+number_of_household_contributors+
            dependencia+
            # Ingresos
            monthly_sales+nequi_sales_percentage+household_income+
            # Bacario
            had_loans_with_family_or_friends+had_loans_with_lenders+
            had_loans_with_banks+had_other_loans+has_current_loans+
            is_reported_in_credit_bureaus+
            # Gestión
            inventory_method+accounting_method+debtors_register+
            # Fomalidad
            business_legal_type+is_registered_in_chamber_commerce+
            is_health_taxpayer+is_pension_taxpayer+is_arl_taxpayer+
            is_family_compensation_fund_taxpayer+
            # Comunidad
            frequency_of_preferential_treatment_from_suppliers+
            number_of_customers_known_by_nickname,
          family = "binomial", a1_base)
stargazer(m1,m2,m3,m4,m5,m6,m7, type = "text")
z1_ev_PAR15 <- rbind(z1_ev_PAR15,ppar_15(a1_base,0.2,m7,"07_comunidad"))


## 2.8. Modelo autopercepción ----
m8 <- glm(PAR15~antiguedad+semana+
            # Persona
            CU07_contact_gender+edad+civil_status+education_level+
            number_of_household_members+number_of_household_contributors+
            dependencia+
            # Ingresos
            monthly_sales+nequi_sales_percentage+household_income+
            # Bacario
            had_loans_with_family_or_friends+had_loans_with_lenders+
            had_loans_with_banks+had_other_loans+has_current_loans+
            is_reported_in_credit_bureaus+
            # Gestión
            inventory_method+accounting_method+debtors_register+
            # Fomalidad
            business_legal_type+is_registered_in_chamber_commerce+
            is_health_taxpayer+is_pension_taxpayer+is_arl_taxpayer+
            is_family_compensation_fund_taxpayer+
            # Comunidad
            frequency_of_preferential_treatment_from_suppliers+
            number_of_customers_known_by_nickname+
            # Autopercepcion
            business_progress_compared_last_year+business_progress_over_next_year+
            business_progress_compared_others+difficulty_achieving_current_progress+
            qualification_as_merchant+probability_of_late_payment,
          family = "binomial", a1_base)
stargazer(m1,m2,m3,m4,m5,m6,m7,m8, type = "text")
z1_ev_PAR15 <- rbind(z1_ev_PAR15,ppar_15(a1_base,0.2,m8,"08_autopercepcion"))


## 2.9. Modelo carácter ----
m9 <- glm(PAR15~antiguedad+semana+
            # Persona
            CU07_contact_gender+edad+civil_status+education_level+
            number_of_household_members+number_of_household_contributors+
            dependencia+
            # Ingresos
            monthly_sales+nequi_sales_percentage+household_income+
            # Bacario
            had_loans_with_family_or_friends+had_loans_with_lenders+
            had_loans_with_banks+had_other_loans+has_current_loans+
            is_reported_in_credit_bureaus+
            # Gestión
            inventory_method+accounting_method+debtors_register+
            # Fomalidad
            business_legal_type+is_registered_in_chamber_commerce+
            is_health_taxpayer+is_pension_taxpayer+is_arl_taxpayer+
            is_family_compensation_fund_taxpayer+
            # Comunidad
            frequency_of_preferential_treatment_from_suppliers+
            number_of_customers_known_by_nickname+
            # Autopercepcion
            business_progress_compared_last_year+business_progress_over_next_year+
            business_progress_compared_others+difficulty_achieving_current_progress+
            qualification_as_merchant+probability_of_late_payment+
            # Caracter
            preferred_investment+fictitious_situation_01+fictitious_situation_02+
            growth_actions_priority+goals_priority,
          family = "binomial", a1_base)
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,m9, type = "text",
          keep = c("CU07","Semana","has_current_loans"),
          digits = 2,
          covariate.labels = 
            c("Semana 2","Semana 3",
              "Semana 4","Genero (Hombre)",
              "Créditos vigentes-Sí"))


z1_ev_PAR15 <- rbind(z1_ev_PAR15,ppar_15(a1_base,0.2,m9,"09_caracter"))



# 3. Grafica evolutiva -----


z1_ev_PAR15 %>% reshape2::melt(id.vars = c("Modelo","Umbral"), 
                               variable.name ="parametro",
                               value.name = "valor") %>% 
  ggplot(aes(as.factor(Modelo), valor, group = parametro, color = parametro))+
  geom_line()+
  geom_point()+
  scale_y_continuous(labels = scales::percent_format())+
  labs(title = "Simulaciones simples PAR15",
       subtitle = "Modelos aditivos",
       x = "Adición de variables",
       y = "%", color = "Parámetro")+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        axis.text.x = element_text(hjust = 1, angle = 45))
