#IDEA 2
library(ggplot2)
l <- 1
n <- 50
pi <- 0.05
pv = seq(0, 1, 0.1)
v <- l / 20
pr <- 0.02

for (va in pv){
  agentes <- data.frame(x = double(), y = double(),
                        dx = double(), dy = double(),
                        Estado  = character(), probvacu = double())
  for (i in 1:n) {
    e <- "S"
    if (runif(1) < pi) {
      e <- "I"
    } else {
      if(runif(1) < va){
        e = "R"
      }
    }
    agentes <- rbind(agentes, data.frame(x = runif(1, 0, l),
                                         y = runif(1, 0, l),
                                         dx = runif(1, -v, v),
                                         dy = runif(1, -v, v),
                                         Estado = e,
                                         probvacu = va))
  }
  levels(agentes$Estado) <- c("S", "I", "R")
  epidemia <- integer()
  r <- 0.1
  tmax <- 2
  digitos <- floor(log(tmax, 10)) + 1
  for (tiempo in 1:tmax) {
    infectados <- dim(agentes[agentes$Estado == "I",])[1]
    epidemia <- c(epidemia, infectados)
    if (infectados == 0) {
      break
    }
    contagios <- rep(FALSE, n)
    for (i in 1:n) { # posibles contagios
      a1 <- agentes[i, ]
      if (a1$Estado == "I") { # desde los infectados
        for (j in 1:n) {
          if (!contagios[j]) { # aun sin contagio
            a2 <- agentes[j, ]
            if (a2$Estado == "S") { # hacia los susceptibles
              dx <- a1$x - a2$x
              dy <- a1$y - a2$y
              d <- sqrt(dx^2 + dy^2)
              if (d < r) { # umbral
                p <- (r - d) / r
                if (runif(1) < p) {
                  contagios[j] <- TRUE
                }
              }
            }
          }
        }
      }
    }
    for (i in 1:n) { # movimientos y actualizaciones
      a <- agentes[i, ]
      if (contagios[i]) {
        a$Estado <- "I"
      } else if (a$Estado == "I") { # ya estaba infectado
        if (runif(1) < pr) {
          a$Estado <- "R" # recupera
        }
      }
      a$x <- a$x + a$dx
      a$y <- a$y + a$dy
      if (a$x > l) {
        a$x <- a$x - l
      }
      if (a$y > l) {
        a$y <- a$y - l
      }
      if (a$x < 0) {
        a$x <- a$x + l
      }
      if (a$y < 0) {
        a$y <- a$y + l
      }
      agentes[i, ] <- a
    }
    aS <- agentes[agentes$Estado == "S",]
    aI <- agentes[agentes$Estado == "I",]
    aR <- agentes[agentes$Estado == "R",]
    tl <- paste(tiempo, "", sep="")
    vacuna = paste(va, "", sep="")
    while (nchar(tl) < digitos) {
      tl <- paste("0", tl, sep="")
      vacuna = paste("0", vacuna, sep="")
    }
    salida <- paste("p6_t", tl, vacuna, ".png", sep="")
    tiempo <- paste("Paso", tiempo)

    png(salida)
    plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
    if (dim(aS)[1] > 0) {
      points(aS$x, aS$y, pch=15, col="chartreuse3", bg="chartreuse3")
    }
    if (dim(aI)[1] > 0) {
      points(aI$x, aI$y, pch=16, col="firebrick2", bg="firebrick2")
    }
    if (dim(aR)[1] > 0) {
      points(aR$x, aR$y, pch=17, col="goldenrod", bg="goldenrod")
    }
    graphics.off()
  }
}

agentes$estado= as.factor(agentes$Estado) #crear vector a partir del dataframe
ggplot(data = agentes, aes(x = probvacu, y = Estado, fill = Estado)) +
  geom_boxplot() + theme_bw() + 
  labs(x = "Probabilidad de vacunación", y = "Estado del agente") 