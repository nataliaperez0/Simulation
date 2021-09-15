library(readr)
library(ggplot2)

ruta_csv = "C:\\Users\\beren\\OneDrive\\Escritorio\\Maestría\\2do semestre\\5. Simulación computacional de nanomateriales\\Tareas\\Tarea_3\\Intentos\\datanew.csv"

datos = read.csv(ruta_csv, col.names = c('Replica1', 'Trabajador1', 'Ordenamiento1', 'Tiempo1',
                                         'Replica2', 'Trabajador2', 'Ordenamiento2', 'Tiempo2',
                                         'Replica3', 'Trabajador3', 'Ordenamiento3', 'Tiempo3'))


datos$Trabajador1 = as.factor(datos$Trabajador1) #crear vector a partir del dataframe
datos$Ordenamiento1 = as.factor(datos$Ordenamiento1) #crear vector a partir del dataframe

datos$Trabajador2 = as.factor(datos$Trabajador2) #crear vector a partir del dataframe
datos$Ordenamiento2 = as.factor(datos$Ordenamiento2) #crear vector a partir del dataframe

datos$Trabajador3 = as.factor(datos$Trabajador3) #crear vector a partir del dataframe
datos$Ordenamiento3 = as.factor(datos$Ordenamiento3) #crear vector a partir del dataframe

#un tipo de gráfica
ggplot(data = datos)+
  geom_boxplot(aes(x= Trabajador1, y= Tiempo1, fill= Ordenamiento1))+
  geom_boxplot(aes(x= Trabajador2, y= Tiempo2, fill= Ordenamiento2))+
  geom_boxplot(aes(x= Trabajador3, y= Tiempo3, fill= Ordenamiento3))+
  labs(x = "Trabajadores", y = "Tiempo (ms)", title = 'Distancia Manhattan')

#otro tipo de gráfica
ggplot(data = datos)+
  geom_boxplot(aes(x= Ordenamiento1, y= Tiempo1, fill= Trabajador1))+
  geom_boxplot(aes(x= Ordenamiento2, y= Tiempo2, fill= Trabajador2))+
  geom_boxplot(aes(x= Ordenamiento3, y= Tiempo3, fill= Trabajador3))+
  labs(x = "Trabajadores", y = "Tiempo (ms)", title = 'Distancia Manhattan') #nombres