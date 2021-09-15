#PRUEBA ESTADÍSTICA ANOVA
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("rapportools")

library(tidyverse)
library(ggpubr)
library(rstatix)
library(rapportools)
library(readr)
library(ggplot2)
library(gridExtra)

ruta_csv = "C:\\Users\\beren\\OneDrive\\Escritorio\\Maestría\\2do semestre\\5. Simulación computacional de nanomateriales\\Tareas\\Tarea_3\\misdatos.csv"

datos = read.csv(ruta_csv)

names(datos) <- c("Replica", "Trabajador", "Orden", "Tiempo")

datos$Trabajador= as.factor(datos$Trabajador) #crear vector a partir del dataframe
datos$Orden = as.factor(datos$Orden) #crear vector a partir del dataframe

ggplot(data = datos, aes(x = Trabajador, y = Tiempo, fill = Orden)) +
  geom_boxplot() + theme_bw()


library(readxl)
ruta_excel = "C:/Users/beren/OneDrive/Escritorio/Maestría/2do semestre/5. Simulación computacional de nanomateriales/Tareas/Tarea_3/misdatos.xlsx"
misdatos <- read_excel(ruta_excel)
excel_sheets(ruta_excel)

trabajador1 = read_excel(ruta_excel, sheet = "Trabajador1")
trabajador2 = read_excel(ruta_excel, sheet = "Trabajador2")
trabajador3 = read_excel(ruta_excel, sheet = "Trabajador3")
ordenfp = read_excel(ruta_excel, sheet = "FP")
ordendp = read_excel(ruta_excel, sheet = "DP")
ordenoa = read_excel(ruta_excel, sheet = "OA")


#ANALISIS DESCRIPTIVO
#Un trabajador
trabajador1 = trabajador1 %>%
  rstatix::reorder_levels(Orden, order = c("FP", "DP", "OA"))

trabajador1 %>%
  group_by(Orden) %>%
  get_summary_stats(Tiempo, type = "mean_sd")
#Dos trabajadores
trabajador2 = trabajador2 %>%
  rstatix::reorder_levels(Orden, order = c("FP", "DP", "OA"))

trabajador2 %>%
  group_by(Orden) %>%
  get_summary_stats(Tiempo, type = "mean_sd")
#Tres trabajadores
trabajador3 = trabajador3 %>%
  rstatix::reorder_levels(Orden, order = c("FP", "DP", "OA"))

trabajador3 %>%
  group_by(Orden) %>%
  get_summary_stats(Tiempo, type = "mean_sd")
#Orden FP
ordenfp = ordenfp %>%
  rstatix::reorder_levels(Trabajadores, order = c("1", "2", "3"))

ordenfp %>%
  group_by(Trabajadores) %>%
  get_summary_stats(Tiempo, type = "mean_sd")
#Orden DP
ordendp = ordendp %>%
  rstatix::reorder_levels(Trabajadores, order = c("1", "2", "3"))

ordendp %>%
  group_by(Trabajadores) %>%
  get_summary_stats(Tiempo, type = "mean_sd")
#Orden OA
ordenoa = ordenoa %>%
  rstatix::reorder_levels(Trabajadores, order = c("1", "2", "3"))

ordenoa %>%
  group_by(Trabajadores) %>%
  get_summary_stats(Tiempo, type = "mean_sd")

#VISUALIZAR DATOS
#Un trabajador
boxplot = ggboxplot(data = trabajador1, x = "Orden", y = "Tiempo",
                    fill = "Orden")
boxplot

#Dos trabajadores
boxplot = ggboxplot(data = trabajador2, x = "Orden", y = "Tiempo",
                    fill = "Orden")
boxplot

#Tres trabajadores
boxplot = ggboxplot(data = trabajador3, x = "Orden", y = "Tiempo",
                    fill = "Orden")
boxplot

#Orden FP
boxplot = ggboxplot(data = ordenfp, x = "Trabajadores", y = "Tiempo",
                    fill = "Trabajadores")
boxplot

#Orden DP
boxplot = ggboxplot(data = ordendp, x = "Trabajadores", y = "Tiempo",
                    fill = "Trabajadores")
boxplot

#Orden OA
boxplot = ggboxplot(data = ordenoa, x = "Trabajadores", y = "Tiempo",
                    fill = "Trabajadores")
boxplot

#SUPUESTOS PARA ANOVA

#1:OUTLIERS
#Un trabajador
trabajador1 %>%
  group_by(Orden) %>%
  identify_outliers(Tiempo)

#Dos trabajadores
trabajador2 %>%
  group_by(Orden) %>%
  identify_outliers(Tiempo)

#Tres trabajadores
trabajador3 %>%
  group_by(Orden) %>%
  identify_outliers(Tiempo)

#Orden FP
ordenfp %>%
  group_by(Trabajadores) %>%
  identify_outliers(Tiempo)

#Orden DP
ordendp %>%
  group_by(Trabajadores) %>%
  identify_outliers(Tiempo)

#Orden OA
ordenoa %>%
  group_by(Trabajadores) %>%
  identify_outliers(Tiempo)

#2:NORMALIDAD CON RESIDUALES
#Un trabajador
normalidad1 = lm(Tiempo ~ Orden, data = trabajador1)
head(trabajador1)
head(normalidad1$fitted.values)
head(normalidad1$residuals)

ggqqplot(residuals(normalidad1))
shapiro_test(residuals(normalidad1))

trabajador1 %>%
  group_by(Orden) %>%
  shapiro_test(Tiempo)

#Dos trabajadores
normalidad2 = lm(Tiempo ~ Orden, data = trabajador2)
head(trabajador2)
head(normalidad2$fitted.values)
head(normalidad2$residuals)

ggqqplot(residuals(normalidad2))
shapiro_test(residuals(normalidad2))

trabajador2 %>%
  group_by(Orden) %>%
  shapiro_test(Tiempo)

#Tres trabajadores
normalidad3 = lm(Tiempo ~ Orden, data = trabajador3)
head(trabajador3)
head(normalidad3$fitted.values)
head(normalidad3$residuals)

ggqqplot(residuals(normalidad3))
shapiro_test(residuals(normalidad3))

trabajador3 %>%
  group_by(Orden) %>%
  shapiro_test(Tiempo)

#Orden FP
normalidad4 = lm(Tiempo ~ Trabajadores, data = ordenfp)
head(ordenfp)
head(normalidad4$fitted.values)
head(normalidad4$residuals)

ggqqplot(residuals(normalidad4))
shapiro_test(residuals(normalidad4))

ordenfp %>%
  group_by(Trabajadores) %>%
  shapiro_test(Tiempo)

#Orden DP
normalidad5 = lm(Tiempo ~ Trabajadores, data = ordendp)
head(ordendp)
head(normalidad5$fitted.values)
head(normalidad5$residuals)

ggqqplot(residuals(normalidad5))
shapiro_test(residuals(normalidad5))

ordendp %>%
  group_by(Trabajadores) %>%
  shapiro_test(Tiempo)

#Orden OA
normalidad6 = lm(Tiempo ~ Trabajadores, data = ordendp)
head(ordenoa)
head(normalidad6$fitted.values)
head(normalidad6$residuals)

ggqqplot(residuals(normalidad6))
shapiro_test(residuals(normalidad6))

ordenoa %>%
  group_by(Trabajadores) %>%
  shapiro_test(Tiempo)

#NORMALIDAD PARA CADA GRUPO POR SEPARADO
#Un trabajador
ggqqplot(data = trabajador1, x = "Tiempo", facet.by = "Orden")
trabajador1 %>%
  group_by(Orden) %>%
  shapiro_test(Tiempo)

#Dos trabajadores
ggqqplot(data = trabajador2, x = "Tiempo", facet.by = "Orden")
trabajador2 %>%
  group_by(Orden) %>%
  shapiro_test(Tiempo)

#Tres trabajadores
ggqqplot(data = trabajador3, x = "Tiempo", facet.by = "Orden")
trabajador3 %>%
  group_by(Orden) %>%
  shapiro_test(Tiempo)

#Orden FP
ggqqplot(data = ordenfp, x = "Tiempo", facet.by = "Trabajadores")
ordenfp %>%
  group_by(Trabajadores) %>%
  shapiro_test(Tiempo)

#Orden DP
ggqqplot(data = ordendp, x = "Tiempo", facet.by = "Trabajadores")
ordendp %>%
  group_by(Trabajadores) %>%
  shapiro_test(Tiempo)

#Orden OA
ggqqplot(data = ordenoa, x = "Tiempo", facet.by = "Trabajadores")
ordenoa %>%
  group_by(Trabajadores) %>%
  shapiro_test(Tiempo)

#HOMOGENEIDAD DE VARIANZAS CON PRUEBA DE LEVENE
#Un trabajador
trabajador1 %>%
  levene_test(Tiempo~Orden)

#Dos trabajadores
trabajador2 %>%
  levene_test(Tiempo~Orden)

#Tres trabajadores
trabajador3 %>%
  levene_test(Tiempo~Orden)

#Orden FP
ordenfp %>%
  levene_test(Tiempo~Trabajadores)

#Orden DP
ordendp %>%
  levene_test(Tiempo~Trabajadores)

#Orden OA
ordenoa %>%
  levene_test(Tiempo~Trabajadores)

#ANOVA
#Un trabajador
view(trabajador1)
trabajador1
attach(trabajador1)
names(trabajador1)
class(Orden)
class(Tiempo)
summary(trabajador1)
boxplot(Tiempo~Orden)
aov(Tiempo~Orden)
anova1 = aov(Tiempo~Orden)
summary(anova1)
TukeyHSD(anova1)

#Dos trabajadores
view(trabajador2)
trabajador2
attach(trabajador2)
names(trabajador2)
class(Orden)
class(Tiempo)
summary(trabajador2)
boxplot(Tiempo~Orden)
aov(Tiempo~Orden)
anova2 = aov(Tiempo~Orden)
summary(anova2)
TukeyHSD(anova2)

#Tres trabajadores
view(trabajador3)
trabajador3
attach(trabajador3)
names(trabajador3)
class(Orden)
class(Tiempo)
summary(trabajador3)
boxplot(Tiempo~Orden)
aov(Tiempo~Orden)
anova3 = aov(Tiempo~Orden)
summary(anova3)
TukeyHSD(anova3)

#Orden FP
view(ordenfp)
ordenfp
attach(ordenfp)
names(ordenfp)
class(Trabajadores)
class(Tiempo)
summary(ordenfp)
boxplot(Tiempo~Trabajadores)
aov(Tiempo~Trabajadores)
anova4 = aov(Tiempo~Trabajadores)
summary(anova4)
TukeyHSD(anova4)

#Orden DP
view(ordendp)
ordendp
attach(ordendp)
names(ordendp)
class(Trabajadores)
class(Tiempo)
summary(ordendp)
boxplot(Tiempo~Trabajadores)
aov(Tiempo~Trabajadores)
anova5 = aov(Tiempo~Trabajadores)
summary(anova5)
TukeyHSD(anova5)

#ANOVA DE WELCH
#Orden OA
anova6 = ordenoa %>% welch_anova_test(Tiempo~Trabajadores)
anova6
comp_mult = ordenoa %>% games_howell_test(Tiempo~Trabajadores)
comp_mult