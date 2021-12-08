#OTRO
l <- 5
n <- 20
v <- l / 20
nanoparticulas <- data.frame(x = double(), y = double(),
                             dx = double(), dy = double(), d = double())
for (i in 1:n) {
  nanoparticulas <- rbind(nanoparticulas, data.frame(x = runif(1, 0, l),
                                                     y = runif(1, 0, l),
                                                     dx = runif(1, -v, v),
                                                     dy = runif(1, -v, v),
                                                     d = round(runif(1, 1, 5))))
}

tmax <- 20
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}
png(paste("fig_00", tl, ".png", sep=""))
plot(nanoparticulas$x, nanoparticulas$y, col=rainbow(20), pch=16, cex=nanoparticulas$d, xlim=c(-0.1, 5.1), ylim=c(-0.1, 5.1),
     main="Estado inicial", xlab="X", ylab="Y")
graphics.off()

png("Histograma.png")
plot(hist(nanoparticulas$d), main="Estado inicial",
     xlab="Tamaño de partícula (nm)", ylab="Número de partículas")
graphics.off()

r <- 0.5
for (tiempo in 1:tmax) {
  aglomeracion <- rep(FALSE, n)
  for (i in 1:n) { # aglomeración
    p1 <- nanoparticulas[i, ]
    for (j in 1:n) {
      if (!aglomeracion[j]) { # aun se aglomera
        p2 <- nanoparticulas[j, ]
        dx <- p1$x - p2$x
        dy <- p1$y - p2$y
        dis <- sqrt(dx^2 + dy^2)
        if (dis < r) { # umbral
          aglomeracion[j] <- TRUE
          p1$d <- p1$d + p2$d
        }
      }
    }
  }
  for (i in 1:n) { # movimientos
    p <- nanoparticulas[i, ]
    p$x <- p$x + p$dx
    p$y <- p$y + p$dy
    if (p$x > l) {
      p$x <- p$x - l
    }
    if (p$y > l) {
      p$y <- p$y - l
    }
    if (p$x < 0) {
      p$x <- p$x + l
    }
    if (p$y < 0) {
      p$y <- p$y + l
    }
    nanoparticulas[i, ] <- p
  }
  tl <- paste(tiempo, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  salida <- paste("fig_", tl, ".png", sep="")
  tiempo <- paste("Paso", tiempo)
  png(salida)
  plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
  plot(nanoparticulas$x, nanoparticulas$y, col=rainbow(20), pch=16, cex=nanoparticulas$d, xlim=c(-0.1, 5.1), 
       ylim=c(-0.1, 5.1),  main=paste(tiempo), xlab="X", ylab="Y")
  graphics.off()
}
