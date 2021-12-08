l <- 10
n <- 30
v <- 1 / 10
r <- 0.5
concentracion = c(0.05, 0.25, 0.50, 0.75, 0.98)
df = data.frame()

for (solvente in concentracion){
  nanoparticulas <- data.frame(x = double(), y = double(),
                               dx = double(), dy = double(), d = double())
  for (i in 1:n) {
    nanoparticulas <- rbind(nanoparticulas, data.frame(x = runif(1, 0, l),
                                                       y = runif(1, 0, l),
                                                       dx = runif(1, -v, v),
                                                       dy = runif(1, -v, v),
                                                       d = round(runif(1, 1, 5))))
  }
  
  tmax <- 100
  digitos <- floor(log(tmax, 10)) + 1
  tl <- "0"
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  png(paste("fig_00", tl, ".png", sep=""))
  plot(nanoparticulas$x, nanoparticulas$y, col=rainbow(20), pch=16, cex=nanoparticulas$d, xlim=c(-0.1, 10.1), ylim=c(-0.1, 10.1),
       main="Estado inicial", xlab="X", ylab="Y")
  graphics.off()
  
  png("Histograma.png")
  plot(hist(nanoparticulas$d), main="Estado inicial",
       xlab="Tamaño de partícula (nm)", ylab="Número de partículas")
  graphics.off()
  
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
            aglomeracion[i] <- TRUE
            if (runif(1) > solvente){
              p1$d <- p1$d + p2$d
              p1 = p2
              nanoparticulas[i, ] <- p1 
            }
            nanoparticulas1 <- unique(nanoparticulas)
          }
        }
      }
    }
    parfinal = nrow(nanoparticulas1[nanoparticulas1$d,])
    result = c(solvente, tiempo, parfinal)
    df = rbind(df, result)
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
    plot(nanoparticulas$x, nanoparticulas$y, col=rainbow(20), pch=16, cex=nanoparticulas$d, xlim=c(-0.1, 10.1), 
         ylim=c(-0.1, 10.1),  main=paste(tiempo), xlab="X", ylab="Y")
    graphics.off()
  }
}


names(df) = c("Solvente", "Iteracion", "NPs")
df$Solvente = df$Solvente * 100
df$Solvente= as.factor(df$Solvente)
library(ggplot2)
ggplot(df, aes(x = Iteracion, y = NPs, group = Solvente, colour = Solvente))+
  geom_line()+
  geom_point(size = 2, shape = 21, fill = "white")+
  geom_smooth(method = "loess", se=FALSE, formula =y ~ x)+
  guides(colour = guide_legend(title = "Solvente (%)"))+
  scale_x_continuous(name = "Iteración")+
  scale_y_continuous(name = "Cantidad de nanopartículas")+
  theme_bw()