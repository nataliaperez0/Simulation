#PRUEBA ESTADÍSTICA
library(readxl)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(car)
library(rstatix)
library(rapportools)
library(readr)
library(gridExtra)

ruta_excel = "C:\\Users\\beren\\OneDrive\\Escritorio\\datos.xlsx"
datos = read_excel(ruta_excel)
datos

datos$Muestra= as.factor(datos$Muestra) #crear vector a partir del dataframe

diagrama = ggplot(data = datos, aes(x = Muestra, y = Decimales, fill = Muestra)) +
  geom_boxplot() + theme_bw() + labs(x = "Cantidad de semillas", y = "Distancia Manhattan")

diagrama

#Estadísticas descriptivas
datos %>%
  group_by(Muestra) %>%
  get_summary_stats(Decimales, type = "mean_sd")

#SUPUESTOS PARA ANOVA
#1:Outliers
datos %>%
  group_by(Muestra) %>%
  identify_outliers(Decimales)

#2:Normalidad por Shapiro
datos %>%
  group_by(Muestra) %>%
  shapiro_test(Decimales)

#3:Homogeneidad de varianza con prueba Levene
datos %>%
  levene_test(Decimales~Muestra)

#PRUEBA ESTADÍSTICA KRUSKAL WALLIS
kruskal.test(Decimales ~ Muestra, data = datos)

#PRUEBA WILCOXON
pairwise.wilcox.test(datos$Decimales, datos$Muestra)