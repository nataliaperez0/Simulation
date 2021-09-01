library(parallel)
cluster = makeCluster(detectCores(logical = FALSE) - 2)
clusterExport(cluster, "t")
datos1 = data.frame()
datos2 = data.frame()
datos3 = data.frame()

repetir = 12 #número de repeticiones del programa
#número de dimensiones=256 
caminata1 = 100 #largo de la caminata
caminata2 = 1000
caminata3 = 10000
#posición en el origen

for (dim in 1:5){
  clusterExport(cluster, "dim")
  clusterExport(cluster, "caminata1")
  resultado1 = parSapply(cluster, 1:repetir,
                        function(r){
                          pos = rep(0, dim)
                          mayor = 0
                          for (t in 1:caminata1){
                            cual = sample(1:dim, 1)
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
  datos1 = rbind(datos1, resultado1)
}

for (dim in 1:5){
  clusterExport(cluster, "dim")
  clusterExport(cluster, "caminata2")
  resultado2 = parSapply(cluster, 1:repetir,
                        function(r){
                          pos = rep(0, dim)
                          mayor = 0
                          for (t in 1:caminata2){
                            cual = sample(1:dim, 1)
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
  datos2 = rbind(datos2, resultado2)
}

for (dim in 1:5){
  clusterExport(cluster, "dim")
  clusterExport(cluster, "caminata3")
  resultado3 = parSapply(cluster, 1:repetir,
                        function(r){
                          pos = rep(0, dim)
                          mayor = 0
                          for (t in 1:caminata3){
                            cual = sample(1:dim, 1)
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
  datos3 = rbind(datos3, resultado3)
}

stopCluster(cluster)
print(datos1)
print(datos2)
print(datos3)
dataf = data.frame(datos1, datos2, datos3)
print(dataf)
boxplot(data.matrix(dataf), use.cols=FALSE, 
        xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima",
        main="Manhattan", col=rainbow(12))