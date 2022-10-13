#DID

rm(list = ls()) 
options(scipen=999) 

library(tidyverse)
library(janitor)
library(clubSandwich)
library(ExPanDaR) #for describing panel data
library(bacondecomp)
library(lfe)
library(fixest) #incluye el estimador de SA y el correspondiente gráfico de evento

#Datos de:
#Stevenson, B., & Wolfers, J. (2006). Bargaining in the shadow of the law: Divorce laws and family distress.
#The Quarterly Journal of Economics, 121(1), 267-288.


#Datos divorce contenidos en el paquete bacondecomp
wd <- divorce %>% 
  filter(year>1964 & year<1996 & sex==2) %>% 
  mutate(suicide_rate=suicide*1000000/(stpop*fshare),
         year=as.numeric(year))

#Explorar datos en panel----

#Esta es una de las funciones de ExPanDaR para explorar datos faltantes
prepare_missing_values_graph(wd, ts_id = "year")

#Esto genera una aplicación interactiva
ExPanD(df = wd,
       ts_id="year",
       cs_id="st",
       title = "Wow, mis datos",
       abstract = "Datos tomados de Stevenson & Wolfers (2006)")




#DID hasta antes de Goodman-Bacon----
wd <- divorce %>% 
  filter(year>1964 & year<1996 & sex==2) %>% 
  mutate(suicide_rate=suicide*1000000/(stpop*fshare),
         year=as.numeric(year))

#Asumiendo tendencias paralelas
modelo1 <- felm(suicide_rate ~ unilateral | factor(st) + factor(year),
                data = wd)

summary(modelo1)

#Frecuentemente se usaban "tendencias específicas

modelo2 <- felm(suicide_rate ~ unilateral | factor(st) + factor(year) + factor(st):year,
                data = wd)

summary(modelo2)

#Recordemos agrupar los errores
modelo1a <- felm(suicide_rate ~ unilateral | factor(st) + factor(year) | 0 | st,
                 data = wd)

summary(modelo1a)

modelo2a <- felm(suicide_rate ~ unilateral | factor(st) + factor(year) + factor(st):year | 0 | st,
                 data = wd)

summary(modelo2a)



#Descomposición de Goodman-Bacon----

df_bacon <- bacon(suicide_rate ~ unilateral,
                  data = wd,
                  id_var = "st",
                  time_var = "year")

coef_bacon <- sum(df_bacon$estimate * df_bacon$weight)
print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))

fit_tw <- lm(suicide_rate ~ unilateral + factor(st) + factor(year), 
             data = wd)
print(paste("Two-way FE estimate =", round(fit_tw$coefficients[2], 4)))


#Replicamos la figura 6 en Goodman-Bacon (2021)
df_bacon %>% 
  ggplot(aes(x=weight,
             y=estimate,
             shape=type)) +
  geom_point() +
  geom_hline(yintercept = round(fit_tw$coefficients[2], 4))




#Estimador de Sun y Abraham (2020)----
#Definimos: periodos_desde_t: periodos desde el tratamiento (negativo antes del tratamiento)

wd <- wd %>% 
  mutate(periodos_desde_t = year-divyear) 
  

modeloSA <-  feols(suicide_rate ~ sunab(divyear, periodos_desde_t) | st + year,
                   data = wd,
                   vcov = ~st)

summary(modeloSA)



#Gráfico (iplot está incluido en fixest) 
iplot(modeloSA)

modeloSA %>% 
  iplot(main = "fixest::sunab",
        xlab = "Periodos desde el tratamiento",
        ref.line = 1)


#Efecto de tratamiento
summary(modeloSA, agg = "ATT")

#Agregado por cohortes
summary(modeloSA, agg = "cohort")
