#Diseeños con discontinuidades

rm(list = ls()) 
options(scipen=999) 

library(tidyverse)
library(janitor)
library(sandwich)
library(clubSandwich)
library(modelsummary)
library(stargazer)
library(rdrobust)
library(gtsummary)
library(lfe)
library(rdd)


#Datos del estudio The Runner Up Effect----
data.brasil<-read_csv(
  "./brazil_runner_up.csv",
  locale = locale(encoding = "latin1"))

#Descriptiva
data.brasil %>%
  select(run, cand_ran_again, cand_winner) %>% 
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} / {N} ({p}%)"),
              digits = all_continuous() ~ 2,
              label = run ~ "Diferencia de votos",
              missing_text = "(Missing)")


#Gráficos de discontinuidades
[FALTA: ARGUMENTOS QUE DEFINEN LA ESTÉTICA]

data.brasil %>% 
  ggplot(aes(       ))+
  geom_point()+
  geom_vline(xintercept=0,
             linetype="dashed",
             color = "red",
             size=1)



#Polinomios locales de orden 2
m1 <- lm(cand_ran_again ~ run+I(run^2),
         data=subset(data.brasil,run>-48 & run<0))

[FALTA: POLINOMO DEL LADO DERECHO]



#Valores ajustados:
data.brasil <- data.brasil %>% 
  mutate(cand_ran_again_hat_left=ifelse(run>-48 & run<0,
                                        predict(m1,.),NA)) %>% 
  mutate(cand_ran_again_hat_right=ifelse(run>=0 & run<48,
                                         predict(m2,.),NA))

#Gráfico con valores ajustados
data.brasil %>% 
  ggplot(aes(x=bin_run,y=bin_cand_ran_again))+
  geom_point()+
  geom_vline(xintercept=0,
             linetype="dashed",
             color = "red",
             size=1)+
  geom_line(aes(x=run, y=cand_ran_again_hat_left))



#Con los dos segmentos
[FALTA: GRÁFICO CON LOS DOS SEGMENTOS]



#smooth en ggplot: lineal
data.brasil %>% 
  ggplot(aes(x=bin_run,y=bin_cand_ran_again))+
  geom_point()+
  geom_smooth(data = filter(data.brasil, run>-48 & run<0),
              method = "lm",
              formula = y ~ poly(x, 2))+
  geom_smooth(data = filter(data.brasil, run>=0 & run<48),
              method = "lm",
              formula = y ~ poly(x, 2))


#Ahora loess
data.brasil %>% 
  ggplot(aes(x=bin_run,y=bin_cand_ran_again))+
  geom_point()+
  geom_smooth(data = filter(data.brasil, run>-48 & run<0),
              method = "loess") +
  geom_smooth(data = filter(data.brasil, run>=0 & run<48),
              method = "loess")


#Mismo procedimiento para la probabilidad de ganar:
w1 <- lm(bin_cand_winner ~ run+I(run^2), data=subset(data.brasil,run>-48 & run<0))
w2 <- lm(bin_cand_winner ~ run+I(run^2), data=subset(data.brasil,run>=0 & run<48))

data.brasil <- data.brasil %>% 
  mutate(cand_win_hat_left=ifelse(run>-48 & run<0,predict(w1,.),NA)) %>% 
  mutate(cand_win_hat_right=ifelse(run>=0 & run<48,predict(w2,.),NA))

data.brasil %>%
  filter(bin_cand_ran_again<.55 , bin_cand_winner <.55) %>% 
  ggplot()+
  geom_point(aes(x=bin_run,y=bin_cand_ran_again),shape=17,fill="black")+
  geom_point(aes(x=bin_run,y=bin_cand_winner))+
  geom_line(aes(x=run, y=cand_win_hat_left))+
  geom_line(aes(x=run, y=cand_win_hat_right))+
  geom_line(aes(x=run, y=cand_ran_again_hat_left))+
  geom_line(aes(x=run, y=cand_ran_again_hat_right))+
  geom_vline(xintercept=0, color = "black", size=1)+
  xlab("Vote Share Difference Between 2nd and 3rd; t (%)")+
  ylab("")+
  scale_x_continuous(breaks = c(-50,0,50))+
  scale_y_continuous(breaks=seq(0, 0.5, 0.05))



##Análisis paramétrico----

#Redefinir variables que son porcentajes
perc.vars <- c( "cand_ran_again", "cand_winner", "cand_ran_lag", "cand_winner_lag", "cand_maj_party", "party_winner", "party_ran_again")

data.brasil[perc.vars] <- lapply(data.brasil[perc.vars],
                                 function(x) x*100)


#D = elegibles
data.brasil <- data.brasil %>% 
  mutate(D=ifelse(run>0,1,0))

#Usaremos efectos fijos municipales con felm
#felm(y ~ x1 + x2 | EFECTOS FIJOS | INSTRUMENTOS | CLUSTER
     ```

summary(rd1 <- felm(cand_winner ~ D  + run |0 | 0 | id_munic, data=data.brasil))

summary(rd2 <- felm(cand_winner ~ D + run+I(run^2) |0 | 0 | id_munic, data=data.brasil))
     
summary(rd3 <- felm(cand_winner ~ D  + run + run*D |0 | 0 | id_munic,
                         data=data.brasil))
     
summary(rd4 <- felm(cand_winner ~ D  + run + I(run^2) + run*D + I(run^2)*D |0 | 0 | id_munic,
                         data=data.brasil))
     
     
#Resumimos
stargazer(rd1, rd2, rd3, rd4,
         title="Comparación de especificaciones de RD",
         type="text", 
         df=FALSE, digits=2)

     
##rdrobust----
     
#rdplot para gráficos
rdres <- rdplot(y = data.brasil$cand_winner,
               x = data.brasil$run,
               p = 2, #default p = 4
               binselect = 'esmv',
               title = "Efecto seguidor",
               y.label = "Probabilidad de ganar en t+1",
               x.label = "Distancia con respecto al 3er lugar en t")

summary(rdres)

#rdbwselect para seleccionar anchos de banda
     
summary(rdbwselect(y = data.brasil$cand_winner,
                  x = data.brasil$run,
                  p = 2,
                  kernel = 'triangular',
                  cluster=data.brasil$id_munic,
                  bwselect = "mserd"))

     
#El procedimiento de Imbens y Kalyanaraman (2012) está en el paquete rdd
       
rdd::IKbandwidth(X=data.brasil$run,
           Y=data.brasil$cand_winner,
           cutpoint = NULL,
           verbose = TRUE,
           kernel = "rectangular")


#Si especificamos el ancho de ventana reportado de 12.57, obtenemos el resultado preferido por los autores en el artículo:
rd5 <- felm(cand_winner ~ D  + run |0 | 0 | id_munic, data=subset(data.brasil,bw<12.57))
rd6 <- felm(cand_winner ~ D  + run |0 | 0 | id_munic, data=subset(data.brasil,bw<12.57/2))
rd7 <- felm(cand_winner ~ D  + run |0 | 0 | id_munic, data=subset(data.brasil,bw<12.57*2))

stargazer(rd5, rd6, rd7,
          title="Comparación de especificaciones de RD (2)",
          type="text",
          df=FALSE, digits=2)


#rdrobust estima los efectos realizando inferencia de manera correcta
summary(rdrobust(y = data.brasil$cand_winner,
                x = data.brasil$run,
                p = 2, #default p = 1
                bwselect='mserd',
                kernel = 'triangular',
                cluster=data.brasil$id_munic)) 


[FALTA: CON rdrobust REPLICAR EL RESULTADO DEL PAPER HACIENDO h=12.57, kernel = 'uniform' Y p = 1]
summary(rdrobust(y = data.brasil$cand_winner,
                x = data.brasil$run,

                
                cluster=data.brasil$id_munic)) 






     
#Diseño difuso----
     
#Debemos haber llamado la librería *causaldata*
       
vet <- causaldata::mortgages

#Indicadora de estar por encima del corte de 
vet <- vet %>%
  mutate(D = ifelse(qob_minus_kw <= 0, 1, 0)) %>% 
  filter(abs(qob_minus_kw) < 12)
     
#Primera etapa
summary(pe <- lm(vet_wwko ~ D + qob_minus_kw + factor(nonwhite) + factor(bpl) + factor(qob),
                data = vet))$coef[1:3,]

vet <- vet %>% 
 mutate(vet_wwko_hat = predict(pe))
     
#Modelo estructural
summary(se <- lm(home_ownership ~ vet_wwko_hat + qob_minus_kw + factor(nonwhite) + factor(bpl) + factor(qob),
                data = vet))$coef[1:3,]
     
#Recordemos cómo usar felm:
#felm(y ~ x1 + x2 | EFECTOS FIJOS | INSTRUMENTOS | CLUSTER

[FALTA: MODELO DE MC2E EN FELM]
summary(felm(  |
               |
               ,
             data = vet))




#Usando rdrobust
summary(rdrobust(y = vet$home_ownership,
                 x = vet$qob_minus_kw,
                 fuzzy = vet$vet_wwko,
                 c = 0,
                 p = 1,
                 h = 12,
                 kernel = 'uniform'))