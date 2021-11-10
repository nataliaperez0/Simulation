poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- sample(1:varcount, 1)
    deg <- sample(0:maxdeg, 1)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars) {
  value <- 0.0
  terms = dim(pol)[1]
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

## NUEVO!!!
domin.by <- function(target, challenger) {
  # sum sobre los TRUE/FALSE
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora (en maximizar)
  } # si no hay empeora, vemos si hay mejora
  # sum sobre los TRUE/FALSE
  return(sum(challenger > target) > 0)
}

vc <- 4
md <- 3
tc <- 5
k <- 2 # cuantas funciones objetivo
obj <- list()
for (i in 1:k) {
  obj[[i]] <- poli(md, vc, tc)
}
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim) # neg -> min, pos -> max
n <- 200 # cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
for (i in 1:n) { # evaluamos las soluciones
  for (j in 1:k) { # para todos los objetivos
    val[i, j] <- eval(obj[[j]], sol[i,])
  }
}
mejor1 <- which.max(sign[1] * val[,1])
mejor2 <- which.max(sign[2] * val[,2])
cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
png("p11_init.png")
plot(val[,1], val[,2], xlab=xl, ylab=yl, main="Ejemplo bidimensional")
graphics.off()
png("p11_mejores.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(val[mejor1, 1], val[mejor1, 2], col="blue", pch=15, cex=1.5)
points(val[mejor2, 1], val[mejor2, 2], col="orange", pch=16, cex=1.5)
graphics.off()
no.dom <- logical() # TRUE/FALSE segun si nadie lo domina
dominadores <- integer()
for (i in 1:n) { # para cada asignacion
  d <- logical() # quienes le dominan (si / no)
  for (j in 1:n) { # para todos los demas
    # i es a quien le retan, j es quien esta retando
    # lo comparamos como si todo fuese max (min f = max -f)
    d <- c(d, domin.by(sign * val[i,], sign * val[j,]))
  }
  cuantos <- sum(d)
  dominadores <- c(dominadores, cuantos)
  no.dom <- c(no.dom, sum(d) == 0) # nadie le domina (puros FALSE)
}
# agarra solo los que tienen TRUE en no.dom
frente <- subset(val, no.dom) # solamente las no dominadas
png("p11_frente.png", width=15, height=15, units="cm", res=1200)
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(frente[,1], frente[,2], col="green", pch=16, cex=0.9)
mejor1 <- which.max((1 + (-2 * minim[1])) * val[,1])
mejor2 <- which.max((1 + (-2 * minim[2])) * val[,2])
points(val[mejor1, 1], val[mejor1, 2], col="blue", pch=15, cex=0.5)
points(val[mejor2, 1], val[mejor2, 2], col="orange", pch=16, cex=0.5)
graphics.off()

library(ggplot2) # recordar instalar si hace falta
data <- data.frame(pos=rep(0, n), dom=dominadores)
png("p11_violin.png")
gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
gr + geom_boxplot(width=0.2, fill="blue", color="green", lwd=2) +
  xlab("") +
  ylab("Frecuencia") +
  ggtitle("Cantidad de soluciones dominantes")
graphics.off()

porcentaje=50
dispersos = kmeans(frente, round(dim(frente)[1]*porcentaje/100), iter.max = 1000, nstart = 50, algorithm = "Lloyd")
dispersos$cluster
dispersos$centers

png("p11_fdispersos.png", width=15, height=15, units="cm", res=1200)
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(frente[,1], frente[,2], col="green", pch=16, cex=0.9)
points(dispersos$centers[,1], dispersos$centers[,2], col="red", pch=16, cex=0.9)
mejor1 <- which.max((1 + (-2 * minim[1])) * val[,1])
mejor2 <- which.max((1 + (-2 * minim[2])) * val[,2])
points(val[mejor1, 1], val[mejor1, 2], col="blue", pch=15, cex=0.5)
points(val[mejor2, 1], val[mejor2, 2], col="orange", pch=16, cex=0.5)
graphics.off()