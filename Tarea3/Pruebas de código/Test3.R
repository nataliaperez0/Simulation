library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

data = Trabajador_1
Trabajador_1 <- filter(.data = Trabajador_1, orden %in% c("facil_primero", "dificil_primero", "aleatorio"))

ggplot(data = Trabajador_1, aes(x = orden, y = tiempo, colour = orden)) +
  geom_boxplot() +
  geom_point() +
  theme_bw() 

aggregate(tiempo~orden, data = Trabajador_1, FUN = var)

library(car)
data = Trabajador_1
Trabajador_1 <- filter(.data = Trabajador_1, orden %in% c("facil_primero", "dificil_primero", "aleatorio"))
leveneTest(y = Trabajador_1$tiempo, group = Trabajador_1$orden, center = "median")

tapply(Trabajador_1$tiempo, Trabajador_1$orden, var, na.rm=TRUE)
bartlett.test(tiempo ~ orden, data=Trabajador_1)