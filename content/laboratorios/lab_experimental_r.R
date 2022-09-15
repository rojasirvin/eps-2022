# Espacio de trabajo ----
rm(list = ls())
options(scipen=999) # Prevenir notación científica

library(tidyverse)
library(janitor)
library(sandwich)
library(clubSandwich)
library(modelsummary)
library(stargazer)

#Usamos los datos de Angrist et al. en la Tarea 1----
#Noten que aquí solo analizaremos UN tratamiento
#Por tanto, los números que generemos no serán iguales
#a los reportados en el artículo

data.angrist <- read_csv("./content/laboratorios/STAR_public_use.csv",
                         locale = locale(encoding = "latin1"))   %>% 
  clean_names()


#Usemos por ahora un solo tratamiento, sfsp
data.angrist.table1 <- data.angrist %>% 
  select(noshow,age,female, mtongue, gpa0, sfsp) %>%
  mutate(mtongue=ifelse(mtongue=="English",1,0)) %>% 
  mutate(sfsp=factor(sfsp,levels=c(0,1),
                     labels=c("Control","SFSP")))

#Pedimos estadísticas por grupo
data.angrist.table1 %>% 
  filter(noshow==0) %>% 
  group_by(sfsp) %>% 
  summarize(mean=mean(age),
            std=sd(age), n=n()) %>% 
  ungroup()


#Prueba t
t.test(data=filter(data.angrist.table1,noshow==0),age ~ sfsp)

#Prueba t como una regresión
dif_age <- lm(age ~ sfsp, data=filter(data.angrist.table1,noshow==0))
summary(dif_age)

#¿Las x predicen tratamiento?
predict_t <- lm(as.numeric(sfsp) ~ age + female + mtongue + gpa0, data=filter(data.angrist.table1,noshow==0) )
summary(predict_t)


#Una forma muy rápida con modelsummary ----
datasummary(noshow+age+female~ sfsp*(mean + sd)*Arguments(na.rm=TRUE),
            fmt = "%.2f",
            data = data.angrist.table1,
            title = "Pruebas de balance",
            notes = "Fuente: Angrist, Lang & Oreopoulos (2009)")

datasummary_balance(~sfsp,
                    fmt = "%.2f",
                    data = data.angrist.table1,
                    title = "Pruebas de balance",
                    notes = "Fuente: Angrist, Lang & Oreopoulos (2009)")





#Errores estándar
reg <- lm(gpa_year1 ~ sfsp +
            factor(sex)+
            factor(mtongue)+
            factor(hsgroup)+
            factor(mom_edn)+
            factor(dad_edn),
          data=filter(data.angrist, noshow==0))

summary(reg)$coef[1:2,]




#Uso stargazer para presentar resultados (hay otras maneras)
stargazer(reg,
          type = "text",
          se = list(NULL),
          column.labels = "Clásicos",
          keep = c("ssp", "sfp", "sfsp"))


#Comparamos errores con robustos
cov0       <- vcovHC(reg, type = "HC0")
robse0    <- sqrt(diag(cov0))


stargazer(reg, reg,
          type = "text",
          se = list(NULL, robse0),
          column.labels =c("Clásicos", "White"),
          keep = c("ssp", "sfp", "sfsp"))



#Ahora errores HC3 (corrección por influencia), el default

cov3      <- vcovHC(reg)
robse3    <- sqrt(diag(cov3))

stargazer(reg, reg, reg,
          type = "text",
          se = list(NULL, robse0, robse3),
          column.labels =c("Clásicos", "White", "HC3"),
          keep = c("ssp", "sfp", "sfsp"))

