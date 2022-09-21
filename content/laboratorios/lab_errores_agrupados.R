# Espacio de trabajo ----
rm(list = ls())
options(scipen=999) # Prevenir notación científica

library(tidyverse)
library(janitor)
library(sandwich)
library(clubSandwich)
library(stargazer)
library(lmtest)

#Experimento con cumplimiento imperfecto ----

data.morocco<-read_csv("./content/laboratorios/crepon_morocco.csv",
                       locale = locale(encoding = "latin1"))   %>% 
  clean_names() %>% 
  filter(merge_indicator!=1)   # 2 y 3 incluyen la línea base


#Asignación: treatment
#Cumplimiento: client

#Estadística por grupo de treatment de head_age_bl
data.morocco %>% 
  group_by(treatment) %>%
  summarize(mean=mean(head_age_bl),
            std=sd(head_age_bl), n=n()) %>% 
  ungroup()

#Con una regresión:
[FALTA: estimar con regresión]


#Pongamos atención al valor p
round(summary(dreg)$coef[1:7,], digits=4)
nobs(dreg)


#Errores estándar agrupados----

#Ignorando la agrupación
coeftest(dreg,
         vcov = vcovHC(dreg, type="HC1"))[1:2,]


#CR0 implementa la corrección de sándwich para errores agrupados
[FALTA: obtener los errores agrupados CR0]

#Errores CR1S, que significan clustered roubst "Stata" :3
coef_test(dreg,
          vcov = "CR1S", 
          cluster = data.morocco$demi_paire)[1:2,]


#Coleccionamos con stargazer
stargazer(dreg, dreg, dreg,
          type = "text",
          se = list(sqrt(diag(vcovHC(dreg, type = "HC0"))),
                    sqrt(diag(vcovCR (dreg, type = "CR0", cluster= data.morocco$demi_paire))),
                    sqrt(diag(vcovCR (dreg, type = "CR1S", cluster= data.morocco$demi_paire)))),
          column.labels =c("HC0", "CR0", "CR1S"),
          report=("vc*p"),
          keep = c("treatment"))




#Remuestreo----

set.seed(927)
data.morocco<-read_csv("./content/laboratorios/crepon_morocco.csv",
                       locale = locale(encoding = "latin1"))   

obs <- nrow(data.morocco)
obs

#Veamos la media del gasto total
mean(data.morocco$expense_total, na.rm=T)

#Una muestra bootstrap
data.b <- data.morocco[sample(nrow(data.morocco),
                              obs,
                              replace = TRUE), ]

mean(data.b$expense_total, na.rm=T)


# Otra muestra bootstrap
data.b <- data.morocco[sample(nrow(data.morocco),
                              obs,
                              replace = TRUE), ]
mean(data.b$expense_total, na.rm=T)



#Ciclos
#Si quiero hacer lo mismo, digamos 10 veces
reps=10
resultados <- data.frame(medias=matrix(ncol = 1, nrow = reps))

for (i in 1:reps)
{
  data.b <- data.morocco[sample(nrow(data.morocco),
                                obs,
                                replace = TRUE), ]

  
  #Guardamos en cada entrada la media estimada
  resultados[i,1] <- mean(data.b$expense_total, na.rm=T)
}
