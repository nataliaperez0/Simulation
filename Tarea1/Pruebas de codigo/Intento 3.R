library(parallel)
cluster = makeCluster(detectCores(logical = FALSE) - 2)
clusterExport(cluster, "t")
datos = data.frame()

repetir = 12 #número de repeticiones del programa
#número de dimensiones=256 
caminata = 1000 #largo de la caminata
#posición en el origen

for (dim in 1:5){
  clusterExport(cluster, "dim")
  clusterExport(cluster, "caminata")
  resultado = parSapply(cluster, 1:repetir,
                        function(r){
                          pos = rep(0, dim)
                          mayor = 0
                          for (t in 1:caminata){
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
  datos = rbind(datos, resultado)
}
stopCluster(cluster)
print(datos)
boxplot(data.matrix(datos), use.cols=FALSE, 
        xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima",
        main="Manhattan", col=rainbow(12))