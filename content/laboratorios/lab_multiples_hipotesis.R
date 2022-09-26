# Espacio de trabajo ----
rm(list = ls())
options(scipen=999) # Prevenir notación científica

library(tidyverse)

#Datos
data.pvalues<-read_csv("./data_benjamini_hochberg.csv",
                       locale = locale(encoding = "latin1"))  
n <- 15
alpha <- 0.05

data.pvalues


#Bonferroni----
data.bonferroni <- data.pvalues %>% 
  mutate(bonferroni_alpha=alpha/n) %>% 
  mutate(bonferrini_rechazar=ifelse(poriginal<=bonferroni_alpha,1,0))

#Benjamini-Hochberg----
data.bh <- data.pvalues %>% 
  mutate(bh_alpha=alpha*hipotesis/n) %>% 
  mutate(bh_rechazar=ifelse(poriginal<=bh_alpha,1,0))