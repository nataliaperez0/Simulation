library(parallel)
dim = 18
num = dim^2
dur = 8
p = c(0.25, 0.50, 0.75)
sobrevive = NULL
datos = data.frame()

for (inicial in p) {
  for (replica in 1:2) {
    actual = matrix(round(runif(num) < p), nrow=dim, ncol=dim, byrow=TRUE)
    paso = function(pos) {
      fila = floor((pos - 1) / dim) + 1
      columna = ((pos - 1) %% dim) + 1
      vecindad =  actual[max(fila - 1, 1) : min(fila + 1, dim), 
                         max(columna - 1, 1): min(columna + 1, dim)]
      return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
    }
    cluster <- makeCluster(detectCores() - 1)
    clusterExport(cluster, "dim")
    clusterExport(cluster, "paso")
    
    for (iteracion in 1:dur) {
      clusterExport(cluster, "actual")
      siguiente <- parSapply(cluster, 1:num, paso)
      vivos = sum(siguiente)
      cat(inicial, replica, iteracion, vivos, '\n')
      actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    }
    if (vivos > 0) {
      vivos = 1
      print(vivos)
    } else {
      vivos = 0
      print("no hay nadie")
    }
    stopCluster(cluster)
  }
  vector = c(vivos)
  print(vector)
  proba = (sum(vector))/2
  resultado = c(inicial, proba)
  datos = rbind(datos, resultado)
}

names(datos) <- c("Prob", "Probinfinito")
with(datos,plot(Prob,Probinfinito, main="Probabilidad de creación de vida",
                ylab="Propabilidad de vida infinita",
                xlab="Probabilidad de población inicial",
                type= "p",
                col="blue"))