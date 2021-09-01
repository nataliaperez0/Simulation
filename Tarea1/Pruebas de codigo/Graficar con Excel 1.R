# library
library(ggplot2)
library(scales)

ggplot(data = DATOS_T1)+
  geom_boxplot(mapping = aes(x = dimension, y = n20000, fill = "n20000"))+
  geom_boxplot(mapping = aes(x = dimension, y = n2000, fill = "n2000"))+
  geom_boxplot(mapping = aes(x = dimension, y = n200, fill = "n200"))+
  geom_boxplot(mapping = aes(x = dimension, y = n20, fill = "n20"))+
  labs(x = "Dimensión", y = "Distancia", title = 'Distancia Manhattan')+
  scale_y_log10()

levels(DATOS_T1$dimension)
DATOS_T1$dimension = factor(DATOS_T1$dimension, levels=c("D2", "D4", "D8", "D16", "D32", "D64", "D128"))
levels(DATOS_T1$dimension)