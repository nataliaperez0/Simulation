#Simulacion del comportamiento de las nanoparticulas ante concentraciones variables de solvente
library(ggplot2)
l <- 10 #Dimension del cuadro bidimensional
n <- 30 #Cantidad de nanoparticulas
v <- 1 / 10 #Velocidad de desplazamiento
r <- 0.5 #Radio critico
concentracion = c(0.05, 0.25, 0.50, 0.75, 0.98) #Concentraciones del solvente
replicas = 10 #Cantidad de replicas
df = data.frame() 

for (solvente in concentracion){
  for (rep in 1:replicas){
    nanoparticulas <- data.frame(x = double(), y = double(),
                                 dx = double(), dy = double(), d = double())
    for (i in 1:n) {
      nanoparticulas <- rbind(nanoparticulas, data.frame(x = runif(1, 0, l),
                                                         y = runif(1, 0, l),
                                                         dx = runif(1, -v, v),
                                                         dy = runif(1, -v, v),
                                                         d = round(runif(1, 1, 5))))
    }
    
    tmax <- 100 #Cantidad de iteraciones
    digitos <- floor(log(tmax, 10)) + 1
    tl <- "0"
    while (nchar(tl) < digitos) {
      tl <- paste("0", tl, sep="")
    }
    png(paste("fig_00", tl, ".png", sep=""))
    plot(nanoparticulas$x, nanoparticulas$y, col=rainbow(20), pch=16, cex=nanoparticulas$d, xlim=c(-0.1, 10.1), ylim=c(-0.1, 10.1),
         main="Estado inicial", xlab="X", ylab="Y")
    graphics.off()
    
    png("Figura_1.png", width=15, height=15, units="cm", res=1200)
    ggplot(nanoparticulas, aes(x=d)) + geom_histogram(binwidth = 1, col='black', fill='blue')+
      labs(x = "Tamaño de partícula (nm)", y = "Número de partículas")+
      theme_bw()
    graphics.off()
    
    for (tiempo in 1:tmax) {
      aglomeracion <- rep(FALSE, n)
      for (i in 1:n) { #Aglomeración
        p1 <- nanoparticulas[i, ]
        for (j in 1:n) {
          if (!aglomeracion[j]) { #Aun se aglomera
            p2 <- nanoparticulas[j, ]
            dx <- p1$x - p2$x
            dy <- p1$y - p2$y
            dis <- sqrt(dx^2 + dy^2)
            if (dis < r) { #Radio critico
              aglomeracion[i] <- TRUE
              if (runif(1) > solvente){ #Solvente
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
      result = c(solvente, rep, tiempo, parfinal)
      df = rbind(df, result)
      for (i in 1:n) { #Movimientos
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
      if (rep == 1) {
        salida <- paste("fig_", tl, ".png", sep="")
        tiempo <- paste("Paso", tiempo)
        png(salida)
        plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
        plot(nanoparticulas$x, nanoparticulas$y, col=rainbow(20), pch=16, cex=nanoparticulas$d, xlim=c(-0.1, 10.1), 
             ylim=c(-0.1, 10.1),  main=paste(tiempo), xlab="X", ylab="Y")
        graphics.off()
      }
    }
  }
}

names(df) = c("Solvente", "Replica", "Iteracion", "NPs")
df2 = aggregate(df[,c("NPs")],by = list(Sol = df$Solvente, Iter = df$Iteracion),FUN = mean)
names(df2)[3] = "NPs"

df2$Sol = df2$Sol * 100
df2$Sol= as.factor(df2$Sol)

png("Figura_2.png", width=15, height=15, units="cm", res=1200)
ggplot(df2, aes(x = Iter, y = NPs, group = Sol, colour = Sol))+
  geom_line()+
  geom_point(size = 2, shape = 21, fill = "white")+
  geom_smooth(method = "loess", se=FALSE, formula =y ~ x)+
  guides(colour = guide_legend(title = "Solvente (%)"))+
  scale_x_continuous(name = "Iteración")+
  scale_y_continuous(name = "Cantidad de nanopartículas")+
  theme_bw()
graphics.off()

png("Figura_3.png", width=15, height=15, units="cm", res=1200)
gr <- ggplot(df2, aes(x=Sol, y=NPs)) + geom_violin(fill="pink", color="purple")
gr + geom_boxplot(width=0.5, fill="cyan", color="black", lwd=0.5) +
  labs(x = "Concentración del solvente (%)", y = "Cantidad de nanopartículas")+
  theme_bw()
graphics.off()

library(tidyverse)
library(ggpubr)
library(car)
library(rstatix)
library(rapportools)
library(readr)
library(gridExtra)

#PRUEBA ESTADISTICA...
#Estadísticas descriptivas
df2 %>%
  group_by(Sol) %>%
  get_summary_stats(NPs, type = "mean_sd")

#SUPUESTOS PARA ANOVA
#1:Outliers
df2 %>%
  group_by(Sol) %>%
  identify_outliers(NPs)

#2:Normalidad por Shapiro
tapply(df2$NPs, df2$Sol, shapiro.test)

#3:Homogeneidad de varianza con prueba Levene
df2 %>%
  levene_test(NPs~Sol)

#PRUEBA ESTADÍSTICA KRUSKAL WALLIS
kruskal.test(NPs ~ Sol, data = df2)

#PRUEBA WILCOXON
pairwise.wilcox.test(df2$NPs, df2$Sol)