library(parallel)
library(ggplot2)
library(scales)
cluster = makeCluster(detectCores(logical = FALSE) - 2)
clusterExport(cluster, "t")
datos = data.frame()

repetir = 12 #número de repeticiones del programa
nivel1 = 20 #largo de la caminata
nivel2 = 200
nivel3 = 2000
nivel4 = 20000

for (dim in c(2**1, 2**2, 2**3, 2**4, 2**5, 2**6, 2**7)){
  clusterExport(cluster, "dim")
  clusterExport(cluster, "nivel1")
  resultado1 = parSapply(cluster, 1:repetir,
                        function(r){
                          pos = rep(0, dim)
                          mayor = 0
                          for (t in 1:nivel1){
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
  clusterExport(cluster, "nivel2")
  resultado2 = parSapply(cluster, 1:repetir,
                        function(r){
                          pos = rep(0, dim)
                          mayor = 0
                          for (t in 1:nivel2){
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
  clusterExport(cluster, "nivel3")
  resultado3 = parSapply(cluster, 1:repetir,
                        function(r){
                          pos = rep(0, dim)
                          mayor = 0
                          for (t in 1:nivel3){
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

for (dim in c(2**1, 2**2, 2**3, 2**4, 2**5, 2**6, 2**7)){
  clusterExport(cluster, "dim")
  clusterExport(cluster, "nivel4")
  resultado4 = parSapply(cluster, 1:repetir,
                         function(r){
                           pos = rep(0, dim)
                           mayor = 0
                           for (t in 1:nivel4){
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
  datos = rbind(datos, resultado4)
}

stopCluster(cluster)
print(datos)
names(datos)=c("r1","r2","r3", "r4", "r5", "r6", "r7","r8", "r9","r10", "r11", "r12")

boxplot(data.matrix(datos), use.cols=FALSE, 
        xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima",
        main="Distancia Manhattan", col=rainbow(12))