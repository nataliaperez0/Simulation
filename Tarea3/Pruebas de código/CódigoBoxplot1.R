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
