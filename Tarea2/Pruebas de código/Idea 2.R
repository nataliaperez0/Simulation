library(parallel)
dim = 12
num = dim^2
dur = 9
p = c(0.25, 0.50, 0.75)

for (inicial in p) {
  for (replica in 1:2) {
    actual = matrix(round(runif(num) < p), nrow=dim, ncol=dim, byrow=TRUE)
    suppressMessages(library("sna"))
    png("p2_t0_r.png")
    plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
    graphics.off()
    print(actual)
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
      if (vivos == 0) { # todos murieron
        print("Ya no queda nadie vivo.")
        break;
      }
      actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
      print(actual)
      salida = paste("p2_t", iteracion, "_r.png", sep="")
      tiempo = paste("Paso", iteracion)
      png(salida)
      plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
      graphics.off()
    }
    stopCluster(cluster)
  }
}