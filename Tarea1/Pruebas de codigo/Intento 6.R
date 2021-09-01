library(parallel)
library(ggplot2)
library(scales)
cluster = makeCluster(detectCores(logical = FALSE) - 2)
clusterExport(cluster, "t")
datos = data.frame()
caminata1 = data.frame()

repetir = 12 #número de repeticiones del programa
#número de dimensiones=256 
largo1 = 20 #largo de la caminata
largo2 = 200
largo3 = 2000
#posición en el origen
dimension= c(rep(2, repetir), rep(4, repetir), rep(8, repetir), rep(16, repetir), rep(32, repetir), rep(64, repetir), rep(128, repetir))

for (dim in c(2**1, 2**2, 2**3, 2**4, 2**5, 2**6, 2**7)){
  clusterExport(cluster, "dim")
  clusterExport(cluster, "largo1")
  resultado1 = parSapply(cluster, 1:repetir,
                         function(r){
                           pos = rep(0, dim)
                           mayor = 0
                           for (t in 1:largo1){
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
  caminata1=rbind(caminata1, resultado1)
}

for (dim in c(2**1, 2**2, 2**3, 2**4, 2**5, 2**6, 2**7)){
  clusterExport(cluster, "dim")
  clusterExport(cluster, "largo2")
  resultado2 = parSapply(cluster, 1:repetir,
                         function(r){
                           pos = rep(0, dim)
                           mayor = 0
                           for (t in 1:largo2){
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
  clusterExport(cluster, "largo3")
  resultado3 = parSapply(cluster, 1:repetir,
                         function(r){
                           pos = rep(0, dim)
                           mayor = 0
                           for (t in 1:largo3){
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

tabla1 = data_frame(dimension)
stopCluster(cluster)
print(datos)
names(datos)=c("r1","r2","r3", "r4", "r5", "r6", "r7","r8", "r9","r10", "r11", "r12")
rownames(datos)=paste("Dimension", 1:21, sep="_")

ggplot(datos)+
  geom_boxplot(mapping = aes(x = datos[, 3:4], y= datos[1:7,]))+
  geom_boxplot(mapping = aes(x = datos[, 3:4], y= datos[8:14,]))+
  geom_boxplot(mapping = aes(x = datos[, 3:4], y= datos[15:21,]))+
  labs(x = "Dimensión", y = "Distancia", title = 'Distancia Manhattan')