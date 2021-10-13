library(rasterVis)
library(latticeExtra)
library(lattice)
library(viridisLite)
library(reshape2)
library(raster)

g <- function(x, y) {
  return((sin(x*2) * ((x + 1)^4 - 30 * x^2 -50 * x) + (y + 1)^4 - 30 * y^2 - 50 * y))
}
x <- seq(-5, 5, 0.5) 
y <-  x
z <- outer(x, y, g)
tmax=40

low <- -5
high <- 5
step <- 0.7
replicas <- 20
replica <- function(t){
  curr <- c(runif(1, low, high), runif(1, low, high))
  best <- curr
  for (tiempo in 1:t) {
    delta <- runif(1, 0, step)
    izq <- curr + c(-delta,0)  
    der <- curr + c(delta,0)   
    arr <- curr + c(0,-delta)  
    aba <- curr + c(0,delta)   
    
    puntos <- c(izq, der, arr, aba)
    for(k in 1:8){
      if(puntos[k] < (-5)){
        puntos[k] <- puntos[k]+2 
      }
      if(puntos[k] > 5){
        puntos[k] <- puntos[k]-2
      }
    }
    vx <- c()
    vy <- c()
    for(p in 1:8){
      if(p %% 2 == 0){
        vy <- c(vy,puntos[p])
      }else{
        vx <- c(vx,puntos[p])
      }
    }
    vg <- c()
    for(q in 1:4){
      vg <- c(vg, g(vx[q], vy[q]) )
    }
    dm <- which.max(vg)
    curr <- c(vx[dm], vy[dm])
    if(g(curr[1],curr[2]) > g(best[1],best[2])){
      best <- curr
    }
  }
  return(best)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (pot in 1:100) { 
  tmax <- pot
  resultados <- foreach(i = 1:replicas, .combine=c) %dopar% replica(tmax)
  
  vx <- c()
  vy <- c()
  aux <- (2*replicas)
  for(p in 1:aux){
    if(p %% 2 == 0){
      vy <- c(vy,resultados[p])
    }else{
      vx <- c(vx,resultados[p])
    }
  }
  
  valores <- c()
  for(q in 1:replicas){
    valores <- c(valores, g(vx[q], vy[q]))
  }
  mejor <- which.max(valores)
  x <- seq(-5, 5,length.out=500)
  y <-  x
  z <- outer(x, y, g)
  dimnames(z) <- list(x, y)
  d <- melt(z)
  names(d) <- c("x", "y", "z")
  
  png(paste0("t7_", tmax, ".png", sep=""), width=500, height=500)
  plot(levelplot(z ~ x * y, data = d, col.regions = terrain.colors(100)))
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(vx, vy, pch=20, col="black", cex=2)
  trellis.unfocus()
  trellis.focus("panel"[1], 1, 1, highlight=FALSE)
  lpoints(vx[mejor], vy[mejor], pch=20, col="red",cex=3)
  trellis.unfocus()
  
  graphics.off() 
}
stopImplicitCluster()