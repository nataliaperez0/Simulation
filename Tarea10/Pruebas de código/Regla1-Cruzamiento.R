#Regla 1: el peso y el valor de cada objeto se generan independientemente con una distribución uniforme.

library(testit)
library(tcltk)

knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso)
  assert(n == length(valor))
  vt <- sum(valor)
  if (pt < cap) {
    return(vt)
  } else {
    filas <- cap + 1
    cols <- n + 1
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols)
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0
    }
    rownames(tabla) <- 0:cap
    colnames(tabla) <- c(0, valor)
    for (objeto in 1:n) {
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        tabla[acum, objeto + 1] <- tabla[acum, objeto]
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}
#pesos independientes con distribucion uniforme
generador.pesos <- function(cuantos, min, max) {
  return(sort(round((runif(cuantos)) * (max - min) + min)))
}
#valores independientes con distribucion uniforme
generador.valores <- function(cuantos, min, max) {
  return(round((runif(cuantos)) * (max - min) + min))
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(round(runif(tam * n)), nrow = tam, ncol = n)
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

df = data.frame()
cruz = c(10, 15, 20) #variar la cantidad de cruzamientos
pm = 0.1 #probabilidad de mutacion

for (rep in cruz){
  for (replica in 1:3){
    n <- 60 #cuantos objetos
    pesos <- generador.pesos(n, 15, 80)
    valores <- generador.valores(pesos, 10, 500)
    capacidad <- round(sum(pesos) * 0.65)
    optimo <- knapsack(capacidad, pesos, valores)
    init <- 30  #cuantas soluciones
    p <- poblacion.inicial(n, init)
    tam <- dim(p)[1]
    assert(tam == init)
    mejores <- double()
    
    tiempo = 10 #segundos
    start = Sys.time()
    
    while(TRUE) {
      elapsed = as.numeric(difftime(Sys.time(), start, units = 'secs'))
      remaining = tiempo - round(elapsed) 
      Sys.sleep(0.1)
      print(remaining)
      
      p$obj <- NULL
      p$fact <- NULL
      for (i in 1:tam) { # cada objeto puede mutarse con probabilidad pm
        if (runif(1) < pm) {
          p <- rbind(p, mutacion(p[i,], n))
        }
      }
      for (i in 1:rep) { #cantidad de reproducciones
        padres <- sample(1:tam, 2, replace=FALSE)
        hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
        p <- rbind(p, hijos[1:n]) # primer hijo
        p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
      }
      tam <- dim(p)[1]
      obj <- double()
      fact <- integer()
      for (i in 1:tam) {
        obj <- c(obj, objetivo(p[i,], valores))
        fact <- c(fact, factible(p[i,], pesos, capacidad))
      }
      p <- cbind(p, obj)
      p <- cbind(p, fact)
      mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
      p <- p[mantener,]
      tam <- dim(p)[1]
      assert(tam == init)
      factibles <- p[p$fact == TRUE,]
      mejor <- max(factibles$obj)
      mejores <- c(mejores, mejor)
      
      print(paste(mejor, (optimo - mejor) / optimo))
      opt <- ((optimo - mejor) / optimo)*100
      segundos <-round(elapsed)
      
      if (remaining <= 0) break
      
      resultado = c(rep, replica, segundos, mejor, opt, optimo)
      df = rbind(df, resultado)
      names(df) = c("Cruzamiento", "Replica", "Segundo", "Mejor", "%Optimo", "Optimo")
    }
  }
}

library(ggplot2)
df$Segundo = as.factor(df$Segundo)
dfs = split.data.frame(df, f = df$Cruzamiento)

ggplot(dfs$`10`, aes(x= Segundo, y= Mejor)) + 
  geom_boxplot(fill = "#F8766D")+
  labs(x = "Tiempo (segundos)", y = "Mayor valor", title = 'Cruzamientos = 10')+
  geom_hline(aes(yintercept=Optimo), colour="green", size= 1)

ggplot(dfs$`15`, aes(x= Segundo, y= Mejor)) + 
  geom_boxplot(fill = "#F8766D")+
  labs(x = "Tiempo (segundos)", y = "Mayor valor", title = 'Cruzamientos = 15')+
  geom_hline(aes(yintercept=Optimo), colour="green", size= 1)

ggplot(dfs$`20`, aes(x= Segundo, y= Mejor)) + 
  geom_boxplot(fill = "#F8766D")+
  labs(x = "Tiempo (segundos)", y = "Mayor valor", title = 'Cruzamientos = 20')+
  geom_hline(aes(yintercept=Optimo), colour="green", size= 1)

png("p10.png", width=600, height=300)
plot(1:tiempo, mejores, xlab="Tiempo (segundos)", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
points(1:tiempo, mejores, pch=15)
abline(h=optimo, col="green", lwd=3)
graphics.off()
print(paste(mejor, (optimo - mejor) / optimo))