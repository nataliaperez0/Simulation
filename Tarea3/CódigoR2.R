install.packages("car")
install.packages("reshape")
install.packages("multcomp")
install.packages("pastecs")
install.packages("WRS")

library(ggplot2)
library(car)
library(reshape)
library(multcomp)
library(pastecs)
library(WRS)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(rapportools)
library(readr)
library(ggplot2)
library(gridExtra)

names(misdatos2)

misdatos2$Trabajador= as.character(misdatos2$Trabajador) #crear vector a partir del dataframe
misdatos2$Orden = as.character(misdatos2$Orden) #crear vector a partir del dataframe
misdatos2$Tiempo = as.numeric(misdatos2$Tiempo) #crear vector a partir del dataframe

attach(misdatos2)
names(misdatos2)

class(Trabajador)
class(Orden)
class(Tiempo)
factor(Trabajador)
factor(Orden)

leveneTest(misdatos2$Tiempo, interaction(misdatos2$Orden, misdatos2$Trabajador), center = median)

Model1 = aov(Tiempo~Trabajador+Orden, data = misdatos2) #Modelo con efectos principales
anova(Model1)

Model2 = aov(Tiempo~Orden*Trabajador, data = misdatos2) #Modelo con efectos principales con interacción
anova(Model2)

plot(Model1)

aov_residuals = residuals(object = Model1)
shapiro.test(x=aov_residuals)

Anova(Model2, Type = "III")

