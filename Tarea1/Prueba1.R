library(parallel)
library(ggplot2)
library(scales)
cluster = makeCluster(detectCores(logical = FALSE) - 2)
clusterExport(cluster, "t")
datos = data.frame()

repetir = 12 #número de repeticiones del programa
#número de dimensiones=256 
caminata1 = 20 #largo de la caminata
caminata2 = 200
caminata3 = 2000
#posición en el origen

for (dim in c(2**1, 2**2, 2**3, 2**4, 2**5, 2**6, 2**7)){
  clusterExport(cluster, "dim")
  clusterExport(cluster, "caminata1")
  resultado1 = parSapply(cluster, 1:repetir,
                        function(r){
                          pos = rep(0, dim)
                          mayor = 0
                          for (t in 1:caminata1){
                            cual = sample(dim, 1)
                            if (runif(1) <0.5){
                              pos[cual] = pos[cual] + 1
                            } else{
                              pos[cual] = pos[cual] -1
                            }
                            d = max(mayor, sum(abs(pos)))
                          }
                          if (d > mayor){
                            mayor = d
                          }
                          return(mayor)
                        })
  datos = rbind(datos, resultado1)
}

for (dim in c(2**1, 2**2, 2**3, 2**4, 2**5, 2**6, 2**7)){
  clusterExport(cluster, "dim")
  clusterExport(cluster, "caminata2")
  resultado2 = parSapply(cluster, 1:repetir,
                        function(r){
                          pos = rep(0, dim)
                          mayor = 0
                          for (t in 1:caminata2){
                            cual = sample(dim, 1)
                            if (runif(1) <0.5){
                              pos[cual] = pos[cual] + 1
                            } else{
                              pos[cual] = pos[cual] -1
                            }
                            d = max(mayor, sum(abs(pos)))
                          }
                          if (d > mayor){
                            mayor = d
                          }
                          return(mayor)
                        })
  datos = rbind(datos, resultado2)
}

for (dim in c(2**1, 2**2, 2**3, 2**4, 2**5, 2**6, 2**7)){
  clusterExport(cluster, "dim")
  clusterExport(cluster, "caminata3")
  resultado3 = parSapply(cluster, 1:repetir,
                        function(r){
                          pos = rep(0, dim)
                          mayor = 0
                          for (t in 1:caminata3){
                            cual = sample(dim, 1)
                            if (runif(1) <0.5){
                              pos[cual] = pos[cual] + 1
                            } else{
                              pos[cual] = pos[cual] -1
                            }
                            d = max(mayor, sum(abs(pos)))
                          }
                          if (d > mayor){
                            mayor = d
                          }
                          return(mayor)
                        })
  datos = rbind(datos, resultado3)
}

stopCluster(cluster)
print(datos)
boxplot(data.matrix(datos), use.cols=FALSE, 
        xlab="Dimensi\u{F3}n",names = c("2", "4", "8", "16", "32", "64", "128"), ylab="Distancia m\u{E1}xima",
        main="Manhattan", col=rainbow(12))