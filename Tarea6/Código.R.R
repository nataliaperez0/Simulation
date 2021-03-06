library(dplyr)
l <- 1
n <- 50
pi <- 0.05
pr <- 0.02
v <- l / 20
pv = seq(0, 1, 0.1)
replicas = 12
datos = data.frame()
infect <- c()

for (va in pv){
  for (rep in 1:replicas){
    agentes <- data.frame(x = double(), y = double(),
                          dx = double(), dy = double(),
                          estado  = character())
    for (i in 1:n) {
      e <- "S"
      if (runif(1) < pi) {
        e <- "I"
      }else {
        if(runif(1) < va){
          e = "R"
        }
      }
      agentes <- rbind(agentes, data.frame(x = runif(1, 0, l),
                                           y = runif(1, 0, l),
                                           dx = runif(1, -v, v),
                                           dy = runif(1, -v, v),
                                           estado = e, vacuna = va))
    }
    print(agentes)
    levels(agentes$estado) <- c("S", "I", "R")
    epidemia <- integer()
    r <- 0.1
    tmax <- 100
    digitos <- floor(log(tmax, 10)) + 1
    for (tiempo in 1:tmax) {
      infectados <- dim(agentes[agentes$estado == "I",])[1]
      epidemia <- c(epidemia, infectados)
      if (infectados == 0) {
        break
      }
      contagios <- rep(FALSE, n)
      for (i in 1:n) { # posibles contagios
        a1 <- agentes[i, ]
        if (a1$estado == "I") { # desde los infectados
          for (j in 1:n) {
            if (!contagios[j]) { # aun sin contagio
              a2 <- agentes[j, ]
              if (a2$estado == "S") { # hacia los susceptibles
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
          a$estado <- "I"
        } else if (a$estado == "I") { # ya estaba infectado
          if (runif(1) < pr) {
            a$estado <- "R" # recupera
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
      aS <- agentes[agentes$estado == "S",]
      aI <- agentes[agentes$estado == "I",]
      aR <- agentes[agentes$estado == "R",]
      resultado = c(va, rep, tiempo)
      datos = rbind(datos, resultado)
      names(datos) = c("Vacuna", "Replica", "Iteracion")
      tl <- paste(tiempo, "", sep="")
      vl = paste(va, "", sep="")
      while (nchar(tl) < digitos) {
        tl <- paste("0", tl, sep="")
        vl = paste("0", vl, sep="")
      }
      if (rep == 1){
        salida <- paste("p6_t", vl, tl, ".png", sep="")
        tiempo <- paste("Paso", tiempo)
        png(salida)
        plot(l, type="n", main=c(tiempo, va), xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
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
    #salida <- paste("Vacuna=", va, ".png", sep="")
    #png(salida)
    #plot(1:length(epidemia), 100 * epidemia / n, xlab="Tiempo", ylab="Porcentaje de infectados",
    #main=paste("Vacunados:",va * 100, "%"))
    #graphics.off()
    infect <- c(infect, epidemia)
  }
}

datos2 <- data.frame(Cantidad_infectados = c(infect))
datos2 <- filter(datos2, Cantidad_infectados > 0)
New = cbind(datos, datos2)
MaxInfectados <- New %>% 
  group_by(Vacuna, Replica) %>%
  filter(Cantidad_infectados == max(Cantidad_infectados))

MaxInfectados2 <- MaxInfectados %>% 
  group_by(Vacuna, Replica) %>%
  filter(Iteracion == min(Iteracion))


library(ggplot2)
MaxInfectados2$Vacuna = as.factor(MaxInfectados2$Vacuna)
ggplot(MaxInfectados2, aes(x= Vacuna, y= Iteracion, fill = Vacuna)) + 
  geom_boxplot()+
  labs(x = "Probabilidad de vacuna", y = "Iteración")

ggplot(MaxInfectados2, aes(x= Vacuna, fill = Vacuna, y = ((Cantidad_infectados/50) * 100))) + 
  geom_boxplot()+
  labs(x = "Probabilidad de vacuna", y = "Mayor porcentaje de infectados")

library(tidyverse)
library(ggpubr)
library(rstatix)
library(rapportools)
library(readr)

#PRUEBA ESTADISTICA PARA CANTIDAD DE INFECTADOS
#Estadísticas descriptivas
MaxInfectados2 %>%
  group_by(Vacuna) %>%
  get_summary_stats(Cantidad_infectados, type = "mean_sd")

#SUPUESTOS PARA ANOVA
#1:Outliers
MaxInfectados2 %>%
  group_by(Vacuna) %>%
  identify_outliers(Cantidad_infectados)

#2:Normalidad por Shapiro
MaxInfectados2 %>%
  group_by(Vacuna) %>%
  shapiro_test(Cantidad_infectados)

MaxInfectados2
str(MaxInfectados2)
names(MaxInfectados2)
shapiro.test(MaxInfectados2$Cantidad_infectados)

#PRUEBA ESTADÍSTICA KRUSKAL WALLIS
kruskal.test(Cantidad_infectados ~ Vacuna, data = MaxInfectados2)

#PRUEBA WILCOXON
pairwise.wilcox.test(MaxInfectados2$Cantidad_infectados, MaxInfectados2$Vacuna)

#PRUEBA ESTADISTICA PARA ITERACION
#Estadísticas descriptivas
MaxInfectados2 %>%
  group_by(Vacuna) %>%
  get_summary_stats(Iteracion, type = "mean_sd")

#SUPUESTOS PARA ANOVA
#1:Outliers
MaxInfectados2 %>%
  group_by(Vacuna) %>%
  identify_outliers(Iteracion)

#2:Normalidad por Shapiro
MaxInfectados2 %>%
  group_by(Vacuna) %>%
  shapiro_test(Iteracion)

MaxInfectados2
str(MaxInfectados2)
names(MaxInfectados2)
shapiro.test(MaxInfectados2$Iteracion)

#PRUEBA ESTADÍSTICA KRUSKAL WALLIS
kruskal.test(Iteracion ~ Vacuna, data = MaxInfectados2)

#PRUEBA WILCOXON
pairwise.wilcox.test(MaxInfectados2$Iteracion, MaxInfectados2$Vacuna)