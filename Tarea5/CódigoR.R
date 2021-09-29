library(ggplot2)
library(tidyverse)
library(ggpubr)
library(car)
library(rstatix)
library(rapportools)
library(readr)
library(gridExtra)

desde = 3
hasta = 7
bueno = 0.048834 #número de Wolfram Alpha
n = seq(1, 6, 1)
muestra = c(10^3, 10^4, 10^5, 10^6, 10^7) # puntitos en el cuadro
df = data.frame()

for (m in muestra){
  for (replica in 1:30){
    f <- function(x) { return(1 / (exp(x) + exp(-x))) } # funcion que piden
    g <- function(x) { return((2 / pi) * f(x)) } # normalizado a distr
    
    suppressMessages(library(distr)) # paquete
    generador  <- r(AbscontDistribution(d = g)) # creamos un generador
    valores <- generador(m) # generamos valores
    montecarlo = sum(valores >= desde & valores <= hasta) # checamos
    integral <- sum(montecarlo) / m # tasa: integral para g(x)
    resultado <- (pi / 2) * integral # integral para f(x) (renorm), antes print((pi / 2) * integral)
    for (i in n) {
      b = trunc(bueno*10^i)/10^i
      r = trunc(resultado*10^i)/10^i
      
      if (r == b) {
        deci = i
      } else {
        break
      }
    }
    datos <- c(m, replica, resultado, deci)
    df = rbind(df, datos)
    #cat(m, replica, resultado, deci,'\n')
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