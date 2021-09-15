library(readr)

ruta_csv = "C:\\Users\\beren\\OneDrive\\Escritorio\\Maestría\\2do semestre\\5. Simulación computacional de nanomateriales\\Tareas\\Tarea_3\\Intentos\\datanew2.csv"

datos = read.csv(ruta_csv)

names(datos) <- c("replica", "trabajador", "orden", "tiempo")

library(ggplot2)
library(gridExtra)

datos$trabajador= as.factor(datos$trabajador) #crear vector a partir del dataframe
datos$orden = as.factor(datos$orden) #crear vector a partir del dataframe

p1 <- ggplot(data = datos, aes(x = trabajador, y = tiempo)) + 
  geom_boxplot() + theme_bw()
p2 <- ggplot(data = datos, aes(x = orden, y = tiempo)) +
  geom_boxplot() + theme_bw()
p3 <- ggplot(data = datos, aes(x = trabajador, y = tiempo, fill = orden)) +
  geom_boxplot() + theme_bw()

grid.arrange(p1, p2, p3,ncol = 2)