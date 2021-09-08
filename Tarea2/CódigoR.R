library(parallel) #librería para paralelizar
library(ggplot2) #librería para gráficar
library(dplyr) 
dim = 18 #dimensión de la malla
num = dim^2
dur = 50 #número de iteraciones
p = c(0.25, 0.50, 0.75) #probabilidades iniciales
datos1 = data.frame() #almacena los 0 y 1 de cada replica por prob inicial
datos2 = data.frame() #almacena la prob inicial y la prob de vida infinita

for (inicial in p) { #para ejecutar las prob iniciales
  for (replica in 1:40) { #para hacer replicas
    actual = matrix(round(runif(num) < p), nrow=dim, ncol=dim, byrow=TRUE)
    paso = function(pos) { #regla de superviviencia de las celdas
      fila = floor((pos - 1) / dim) + 1
      columna = ((pos - 1) %% dim) + 1
      vecindad =  actual[max(fila - 1, 1) : min(fila + 1, dim), 
                         max(columna - 1, 1): min(columna + 1, dim)]
      return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
    }
    cluster <- makeCluster(detectCores(logical = FALSE) - 2)
    clusterExport(cluster, "dim")
    clusterExport(cluster, "paso")
    
    for (iteracion in 1:dur) { #para ejecutar 50 iteraciones
      clusterExport(cluster, "actual")
      siguiente <- parSapply(cluster, 1:num, paso)
      vivos = sum(siguiente)
      cat(inicial, replica, iteracion, vivos, '\n')
      actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    }
    if (vivos > 0) {
      vivos = 1 #cuenta como 1 cuando la replica sobrevive 50 iteraciones
      print(vivos)
    } else {
      vivos = 0 #cuenta como 0 cuando la replica no sobrevive
      print(vivos)
    }
    vector = c(vivos) #vector que almacena todos los 0 y 1
    resultado = c(inicial, vivos) #almancena los 0 y 1 y su prob inicial
    datos1 = rbind(datos1, resultado) #dataframe para 0, 1 y prob inicial
    stopCluster(cluster)
  }
}

names(datos1) <- c("Prob", "Decision") #nombra las columnas del dataframe
datos2 = aggregate(Decision~Prob, datos1, mean) #calcula el promedio de prob inicial
ggplot(datos2, aes(x= Prob, y= Decision)) + 
  geom_smooth(color="blue", size=1)+ #se puede cambiar a geom_line
  geom_point(shape=21, color="black", fill="#69b3a2", size=6)+
  theme_bw()+
  labs(x = "Probabilidad de población inicial", y = "Probabilidad de vida infinita", 
       title = "Probabilidad de creación de vida")