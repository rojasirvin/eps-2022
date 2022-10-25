# Espacio de trabajo ----
rm(list = ls())
options(scipen=999) # Prevenir notación científica

library(tidyverse)
library(janitor)
library(clubSandwich)
library(modelsummary)
library(MatchIt)
library(Zelig)
library(cobalt)

#Datos

data.smoking <- read_csv(
  "./cattaneo_smoking.csv",
  locale = locale(encoding = "latin1")) %>% 
  clean_names() %>% 
  mutate(smoke=ifelse(mbsmoke=="smoker",1,0)) %>% 
  mutate(married=ifelse(mmarried=="married",1,0)) %>% 
  mutate(firstbaby=ifelse(fbaby=="Yes",1,0))

#Asegurarse que no hay NA, MatchIt no corre con NA
data.smoking <- data.smoking[complete.cases(data.smoking), ] 

#Semilla
set.seed(1021)


#Comparaciones simples----


#[FALTA: Comparaciones simples]










#Nuestro modelo para el PS----
binaria <- "smoke"
variables <- c("married", "firstbaby", "medu", "nprenatal", "foreign", "mhisp", "fage")

ps <- as.formula(paste(binaria,
                         paste(variables,
                               collapse ="+"),
                         sep= " ~ "))
print(ps)



#Estimación del PS
ps.est <- glm(formula = ps,
              family = binomial(link = "logit"),
              data = data.smoking)


#El PS estimado es la probabilidad ajustada
data.smoking <- data.smoking %>% 
  mutate(ps_hat = predict(ps.est, type = "response"))


data.smoking %>% 
  ggplot(aes(x = ps_hat)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  facet_wrap(~smoke) +
  xlab("Probabilidad de fumar") +
  theme_bw()













#Usaremos la función matchit para hacer el apareamiento----

#[FALTA: usar matchit para construir la muestra emparejada]

summary(m.out)


#Un par de visualizaciones
#[FALTA: Plots para ver traslape]


#Con match.data obtenemos la muestra apareada
m.data <- match.data(m.out)
dim(m.data)


#Esta matriz nos dice quién es el match de quién
head(m.out$match.matrix)



#Loveplot----
love.plot(bal.tab(m.out),
          threshold = .1)



#Efectos del tratamiento----
z.out <- zelig(bweight~smoke,
               data = m.data,
               model = "ls")

z.out



#Inferencia----

#Simularemos el valor esperado de las diferencias cuando t==0
x.out <- setx(z.out, smoke=0)

#Con respecto a cuando t==1
x1.out <- setx1(z.out, smoke=1)

#Corremos la simulación
sim.out <- sim(z.out, x=x.out, x1=x1.out)

#Vemos los resultados
summary(sim.out)






#Caliper----
#ratio: indica cuántos emparejados del grupo no tratado queremos
#caliper: distancia máxima en términos de PS

m.out <- matchit(formula=ps,
                 method = "nearest",
                 distance= "logit",
                 replace = FALSE,
                 ratio = 2,
                 caliper = .1,
                 data = data.smoking)

zelig(bweight~smoke,
               data = match.data(m.out),
               model = "ls")

#Recuerden hacer inferencia apropiadamente









#Subclasificación---

m.out <- matchit(formula=ps,
                 method = "subclass",
                 subclass = 5,
                 distance= "logit",
                 data = data.smoking)

zelig(bweight~smoke,
               data = match.data(m.out),
               model = "ls")

#Recuerden hacer inferencia apropiadamente
