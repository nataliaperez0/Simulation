# library
library(ggplot2)
library(scales)

ggplot(data = DATOS_T1)+
  geom_boxplot(mapping = aes(x = dimension, y = n20000), fill = "orange", alpha = 0.8)+
  geom_boxplot(mapping = aes(x = dimension, y = n2000), fill = "purple", alpha = 0.8)+
  geom_boxplot(mapping = aes(x = dimension, y = n200), fill = "green", alpha = 0.8)+
  geom_boxplot(mapping = aes(x = dimension, y = n20), fill = "blue")+
  labs(x = "Dimensión", y = "Distancia", title = 'Distancia Manhattan')

levels(DATOS_T1$dimension)
DATOS_T1$dimension = factor(DATOS_T1$dimension, levels=c("D2", "D4", "D8", "D16", "D32", "D64", "D128"))
levels(DATOS_T1$dimension)