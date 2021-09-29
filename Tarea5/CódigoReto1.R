library(ggplot2)
library(tidyverse)
library(ggpubr)
library(car)
library(rstatix)
library(rapportools)
library(readr)
library(gridExtra)

bueno = 3.141592 #número pi
n = seq(1, 6, 1)
muestra = c(10^3, 10^4, 10^5, 10^6, 10^7) # puntitos en el cuadro
df = data.frame()

for (muchos in muestra) {
  for (replica in 1:30){
    interior = 0
    for (r in 1:muchos) {
      x = runif(1, -1, 1)
      y = runif(1, -1, 1)
      d = sqrt(x*x + y*y)
      if (d < 1) {
        interior = interior + 1
      }
    }
    
    tasa = interior / muchos
    pi = 4 * tasa
    #print(pi)
    for (i in n) {
      b = trunc(bueno*10^i)/10^i
      r = trunc(pi*10^i)/10^i
      
      if (r == b) {
        deci = i
      } else {
        break
      }
    }
    datos <- c(muchos, replica, pi, deci)
    df = rbind(df, datos)
    #cat(muchos, replica, pi, deci,'\n')
  }
}

names(df) <- c("Muestra", "Replica", "Resultado", "Decimales")
df$Muestra = as.factor(df$Muestra)
ggplot(df, aes(x= Muestra, y= Decimales, fill= Muestra)) + 
  geom_boxplot()+
  labs(x = "Muestra", y = "Decimales correctos") + #nombres
  scale_x_discrete(labels = c("1K", "10K", "100K", "1M", "10M"))+
  scale_fill_discrete(labels = c("10^3", "10^4", "10^5", "10^6", "10^7"))

#Estadísticas descriptivas
df %>%
  group_by(Muestra) %>%
  get_summary_stats(Decimales, type = "mean_sd")

#SUPUESTOS PARA ANOVA
#1:Outliers
df %>%
  group_by(Muestra) %>%
  identify_outliers(Decimales)

#2:Normalidad por Shapiro
df %>%
  group_by(Muestra) %>%
  shapiro_test(Decimales)

#3:Homogeneidad de varianza con prueba Levene
df %>%
  levene_test(Decimales~Muestra)

#PRUEBA ESTADÍSTICA KRUSKAL WALLIS
kruskal.test(Decimales ~ Muestra, data = df)

#PRUEBA WILCOXON
pairwise.wilcox.test(df$Decimales, df$Muestra)