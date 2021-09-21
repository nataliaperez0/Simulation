#PRUEBA ESTADÍSTICA
library(readxl)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(car)

ruta_excel = "C:\\Users\\beren\\OneDrive\\Escritorio\\Maestría\\2do semestre\\5. Simulación computacional de nanomateriales\\Tareas\\Tarea_4\\datos.xlsx"
datos = read_excel(ruta_excel)
datos

datos$Semillas= as.factor(datos$Semillas) #crear vector a partir del dataframe

diagrama = ggplot(data = datos, aes(x = Semillas, y = Distancia, fill = Semillas)) +
  geom_boxplot() + theme_bw() + labs(x = "Cantidad de semillas", y = "Distancia Manhattan")

diagrama

#Estadísticas descriptivas
datos = datos %>%
  rstatix::reorder_levels(Semillas, order = c("6", "12", "24"))

datos %>%
  group_by(Semillas) %>%
  get_summary_stats(Distancia, type = "mean_sd")

#SUPUESTOS PARA ANOVA
#1:Outliers
datos %>%
  group_by(Semillas) %>%
  identify_outliers(Distancia)

#2:Normalidad con residuales
normalidad = lm(Distancia ~ Semillas, data = datos)
head(datos)
head(normalidad$fitted.values)
head(normalidad$residuals)

grafica1 = ggqqplot(residuals(normalidad))+
  labs(title = 'Normalidad con residuales')
grafica1

shapiro_test(residuals(normalidad))

datos %>%
  group_by(Semillas) %>%
  shapiro_test(Distancia)

#3:Homogeneidad de varianza con prueba Levene
datos %>%
  levene_test(Distancia~Semillas)


#PRUEBA ESTADÍSTICA KRUSKAL WALLIS
kruskal.test(Distancia ~ Semillas, data = datos)

pairwise.wilcox.test(datos$Distancia, datos$Semillas)

grafica2 = ggline(data = datos, x = "Semillas", y = "Distancia", add = c("mean_se", "jitter"))
grafica2
  