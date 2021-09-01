library(dplyr) 
library(ggplot2)
library(scales)
library(parallel)
cluster = makeCluster(detectCores(logical = FALSE) - 2)
datos <-  data.frame()
largos <- c(20, 200, 2000, 20000)

for (dimension in c(2**1, 2**2, 2**3, 2**4, 2**5, 2**6, 2**7)) {
  parSapply(cluster, largos, {
    clusterExport(cluster, "dimension")
    clusterExport(cluster, "duracion")
    clusterExport(cluster, "datos")
    for (replica in 1:12) {
      pos <- rep(0, dimension)
      mayor <- 0
      for (t in 1:duracion) {
        cambiar <- sample(1:dimension, 1)
        cambio <- 1
        if (runif(1) < 0.5) {
          cambio <- -1
        }
        pos[cambiar] <- pos[cambiar] + cambio
        d <- sum(abs(pos))
        if (d > mayor) {
          mayor <- d
        }
      }
      resultado <- c(dimension, duracion, replica, mayor)
      datos <- rbind(datos, resultado)
    }
  })
}
names(datos) <- c("dim", "Caminata", "rep", "dist")
stopCluster(cluster)

datos$dim <- as.factor(datos$dim)
datos$Caminata <- as.factor(datos$Caminata)
ggplot(datos, aes(x= dim, y= dist, fill= Caminata)) + 
  geom_boxplot()+
  labs(x = "Dimensión", y = "Distancia", title = 'Distancia Manhattan')+
  scale_y_log10()