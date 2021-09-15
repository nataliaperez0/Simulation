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
  geom_boxplot() + theme_bw() + labs(y = "Tiempo (ms)")


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
g1 = ggplot(data = trabajador1, aes(x = Orden, y = Tiempo, fill = Orden)) + 
  geom_boxplot() + theme_bw() + labs(x = "Orden de trabajo", y = "Tiempo (ms)", title = 'Tiempos para 1 trabajador')

#Dos trabajadores
g2 = ggplot(data = trabajador2, aes(x = Orden, y = Tiempo, fill = Orden)) + 
  geom_boxplot() + theme_bw() + labs(x = "Orden de trabajo", y = "Tiempo (ms)", title = 'Tiempos para 2 trabajadores')

#Tres trabajadores
g3 = ggplot(data = trabajador3, aes(x = Orden, y = Tiempo, fill = Orden)) + 
  geom_boxplot() + theme_bw() + labs(x = "Orden de trabajo", y = "Tiempo (ms)", title = 'Tiempos para 3 trabajadores')

#Orden FP
g4 = ggplot(data = ordenfp, aes(x = Trabajadores, y = Tiempo, fill = Trabajadores)) + 
  geom_boxplot() + theme_bw() + labs(x = "Trabajadores", y = "Tiempo (ms)", title = 'Tiempos para el orden FP')

#Orden DP
g5 = ggplot(data = ordendp, aes(x = Trabajadores, y = Tiempo, fill = Trabajadores)) + 
  geom_boxplot() + theme_bw() + labs(x = "Trabajadores", y = "Tiempo (ms)", title = 'Tiempos para el orden DP')

#Orden OA
g6 = ggplot(data = ordenoa, aes(x = Trabajadores, y = Tiempo, fill = Trabajadores)) + 
  geom_boxplot() + theme_bw() + labs(x = "Trabajadores", y = "Tiempo (ms)", title = 'Tiempos para el orden OA')

grid.arrange(g1, g4, g2, g5, g3, g6, ncol = 2)

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

n1 = ggqqplot(residuals(normalidad1))+
  labs(title = 'Normalidad con residuales para 1 trabajador')
shapiro_test(residuals(normalidad1))

trabajador1 %>%
  group_by(Orden) %>%
  shapiro_test(Tiempo)

#Dos trabajadores
normalidad2 = lm(Tiempo ~ Orden, data = trabajador2)
head(trabajador2)
head(normalidad2$fitted.values)
head(normalidad2$residuals)

n2 = ggqqplot(residuals(normalidad2))+
  labs(title = 'Normalidad con residuales para 2 trabajadores')
shapiro_test(residuals(normalidad2))

trabajador2 %>%
  group_by(Orden) %>%
  shapiro_test(Tiempo)

#Tres trabajadores
normalidad3 = lm(Tiempo ~ Orden, data = trabajador3)
head(trabajador3)
head(normalidad3$fitted.values)
head(normalidad3$residuals)

n3 = ggqqplot(residuals(normalidad3))+
  labs(title = 'Normalidad con residuales para 3 trabajadores')
shapiro_test(residuals(normalidad3))

trabajador3 %>%
  group_by(Orden) %>%
  shapiro_test(Tiempo)

#Orden FP
normalidad4 = lm(Tiempo ~ Trabajadores, data = ordenfp)
head(ordenfp)
head(normalidad4$fitted.values)
head(normalidad4$residuals)

n4 = ggqqplot(residuals(normalidad4))+
  labs(title = 'Normalidad con residuales para el orden FP')
shapiro_test(residuals(normalidad4))

ordenfp %>%
  group_by(Trabajadores) %>%
  shapiro_test(Tiempo)

#Orden DP
normalidad5 = lm(Tiempo ~ Trabajadores, data = ordendp)
head(ordendp)
head(normalidad5$fitted.values)
head(normalidad5$residuals)

n5 = ggqqplot(residuals(normalidad5))+
  labs(title = 'Normalidad con residuales para el orden DP')
shapiro_test(residuals(normalidad5))

ordendp %>%
  group_by(Trabajadores) %>%
  shapiro_test(Tiempo)

#Orden OA
normalidad6 = lm(Tiempo ~ Trabajadores, data = ordenoa)
head(ordenoa)
head(normalidad6$fitted.values)
head(normalidad6$residuals)

n6 = ggqqplot(residuals(normalidad6))+
  labs(title = 'Normalidad con residuales para el orden OA')
shapiro_test(residuals(normalidad6))

ordenoa %>%
  group_by(Trabajadores) %>%
  shapiro_test(Tiempo)

grid.arrange(n1, n4, n2, n5, n3, n6, ncol = 2)

#NORMALIDAD PARA CADA GRUPO POR SEPARADO
#Un trabajador
m1 = ggqqplot(data = trabajador1, x = "Tiempo", facet.by = "Orden")+
  labs(title = 'Normalidad por grupos para 1 trabajador')
trabajador1 %>%
  group_by(Orden) %>%
  shapiro_test(Tiempo)

#Dos trabajadores
m2 = ggqqplot(data = trabajador2, x = "Tiempo", facet.by = "Orden")+
  labs(title = 'Normalidad por grupos para 2 trabajadores')
trabajador2 %>%
  group_by(Orden) %>%
  shapiro_test(Tiempo)

#Tres trabajadores
m3 = ggqqplot(data = trabajador3, x = "Tiempo", facet.by = "Orden")+
  labs(title = 'Normalidad por grupos para 3 trabajadores')
trabajador3 %>%
  group_by(Orden) %>%
  shapiro_test(Tiempo)

#Orden FP
m4 = ggqqplot(data = ordenfp, x = "Tiempo", facet.by = "Trabajadores")+
  labs(title = 'Normalidad por grupos para el orden FP')
ordenfp %>%
  group_by(Trabajadores) %>%
  shapiro_test(Tiempo)

#Orden DP
m5 = ggqqplot(data = ordendp, x = "Tiempo", facet.by = "Trabajadores")+
  labs(title = 'Normalidad por grupos para el orden DP')
ordendp %>%
  group_by(Trabajadores) %>%
  shapiro_test(Tiempo)

#Orden OA
m6 = ggqqplot(data = ordenoa, x = "Tiempo", facet.by = "Trabajadores")+
  labs(title = 'Normalidad por grupos para el orden OA')
ordenoa %>%
  group_by(Trabajadores) %>%
  shapiro_test(Tiempo)

grid.arrange(m1, m4, m2, m5, m3, m6, ncol = 2)

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