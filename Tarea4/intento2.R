n <-  25
numsem <- seq(1, 25, 4)

for (k in numsem) {
  zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
  x <- rep(0, k) # ocupamos almacenar las coordenadas x de las semillas
  y <- rep(0, k) # igual como las coordenadas y de las semillas
  
  for (semilla in 1:k) {
    while (TRUE) { #hasta que hallamos una posicion vacia para la semilla
      fila <- sample(1:n, 1)
      columna <- sample(1:n, 1)
      if (zona[fila, columna] == 0){
        zona[fila, columna] = semilla
        x[semilla] <- columna
        y[semilla] <- fila
        break
      }
    }
  }
  
  celda <- function(pos){
    fila <- floor((pos - 1) / n) + 1
    columna <- ((pos - 1) %% n) + 1
    if (zona[fila, columna] > 0){ #es una semilla
      return(zona[fila, columna])
    } else{
      cercano <- NULL #sin valor por el momento
      menor <- n * sqrt(2) #mayor posible para comenzar la busqueda
      for (semilla in 1:k){
        dx <- columna - x[semilla]
        dy <- fila - y[semilla]
        dist <- sqrt(dx^2 + dy^2)
        if (dist < menor){
          cercano <- semilla
          menor <- dist
        }
      }
      return(cercano)
    }
  }
  
  celdas <- sapply(1:(n * n), function(p) celda(p))
  voronoi <- matrix(celdas, nrow = n, ncol = n, byrow=TRUE)
  rotate <- function(x) t(apply(x, 2, rev))
  sem=paste("p4s", "-", k, ".png", sep="")
  png(sem)
  par(mar = c(0,0,0,0))
  image(rotate(zona), col=rainbow(k+1), xaxt='n', yaxt='n')
  graphics.off()
  cel=paste("p4c", "-", k, ".png", sep="")
  png(cel)
  par(mar = c(0,0,0,0))
  image(rotate(voronoi), col=rainbow(k+1), xaxt='n', yaxt='n')
  graphics.off()
}