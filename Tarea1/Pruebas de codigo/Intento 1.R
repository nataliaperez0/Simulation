library(parallel)
cluster = makeCluster(detectCores(logical = FALSE) - 2)
clusterExport(cluster, "n")
clusterExport(cluster, "t")

repetir = 20 #número de repeticiones del programa
#número de dimensiones 
caminata = 100 #largo de la caminata
#posición en el origen

for (dim in 1:256){
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
}
stopCluster(cluster)
print(resultado)