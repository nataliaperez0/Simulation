library(parallel)
library(ggplot2)
library(scales)
cluster = makeCluster(detectCores(logical = FALSE) - 2)
clusterExport(cluster, "t")
datos = data.frame()

repetir = 12 #número de repeticiones del programa
dimension = c(2**1, 2**2, 2**3, 2**4, 2**5, 2**6, 2**7)
caminata = c(20, 200, 2000, 20000)

for (i in caminata) {
  for (dim in dimension) {
    clusterExport(cluster, "dim")
    clusterExport(cluster, "caminata")
    resultado = parSapply(cluster, 1:repetir,
                           function(r){
                             pos = rep(0, dim)
                             mayor = 0
                             for (t in caminata){
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
    datos = rbind(datos, resultado)
  }
}
stopCluster(cluster)
print(datos)
boxplot(data.matrix(datos), use.cols=FALSE, 
        xlab="Dimensi\u{F3}n",names = c("2", "4", "8", "16", "32", "64", "128"), ylab="Distancia m\u{E1}xima",
        main="Manhattan", col=rainbow(12))