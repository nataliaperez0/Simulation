library(lattice)
library(sp)
library(viridisLite)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(car)
library(rstatix)
library(rapportools)
library(readr)
library(gridExtra)

g <- function(x,y) {
  return((sin(x*2) * ((x + 1)^4 - 30 * x^2 -50 * x) + (y + 1)^4 - 30 * y^2 - 50 * y))
}

x <- seq(-2, 5, 0.25) 
y <- x
z <- outer(x, y, g)

low <- -2
high <- 5
pasos <- seq(0.25, 2, 0.25) #variar el paso
replicas <- 50 #cantidad de puntos rojos
df<- data.frame()

for (step in pasos) {
  for (repetir in 1:30){ #hacer repeticiones del experimento
    replica <- function(t) {
      curr <- c(runif(1, low, high), runif(1, low, high))
      best <- curr
      for (tiempo in 1:t) {
        delta <- runif(1, 0, step)
        izq <- curr +c(-delta,0)
        der <- curr + c(delta,0)
        arr <- curr + c(0,-delta)
        aba <- curr + c(0,delta)
        
        coord <- c(izq, der, arr, aba)
        for(p in 1:8){
          if(coord[p] < (-2)){
            coord[p] <- coord[p]+5
          }
          if(coord[p] > 5){
            coord[p] <- coord[p]-2
          }
        }
        
        vx<-c()
        vy<-c()
        for(q in 1:8){
          if(q %% 2 == 0){
            vy <- c(vy,coord[q])
          }else{
            vx <- c(vx,coord[q])
          }
        }
        
        vg<- c()
        for(k in 1:4){
          vg <- c(vg, g(vx[k], vy[k]) )
        }
        
        pmax <- which.max(vg)
        curr <- c(vx[pmax], vy[pmax])
        if(g(curr[1],curr[2]) > g(best[1],best[2])){
          best <- curr
        }
      }
      return(best)
    }
    
    suppressMessages(library(doParallel))
    registerDoParallel(makeCluster(detectCores(logical = FALSE) - 2))
    
    for (pot in 1:40) { #iteraciones
      tmax <- pot
      resultados <- foreach(i = 1:replicas, .combine=c) %dopar% replica(tmax)
      
      vx<- c()
      vy<- c()
      aux<-(2*replicas)
      for(q in 1:aux){
        if(q %% 2 == 0){
          vy <- c(vy,resultados[q])
        }else{
          vx <- c(vx,resultados[q])
        }
      }
      
      
      val <- c()
      for(k in 1:replicas){
        val <- c(val, g(vx[k], vy[k]))
      }
      
      maximo <- which.max(val)
      x <- seq(-2, 5, 0.25) 
      y <-  x
      z <- outer(x, y, g)
      dimnames(z) <- list(x, y)
      d <- melt(z)
      names(d) <- c("x", "y", "z")
      
      if (repetir == 1 & step == 0.25){
        png(paste0("t7_", tmax, ".png", sep=""), width=500, height=500)
        plot(levelplot(z ~ x * y, data = d, col.regions = terrain.colors(100)))
        trellis.focus("panel", 1, 1, highlight=FALSE)
        lpoints(vx, vy, pch=20, col="red", cex=2) #puntos rojos
        trellis.unfocus()
        trellis.focus("panel"[1], 1, 1, highlight=FALSE) 
        lpoints(vx[maximo], vy[maximo], pch=20, col="blue",cex=3) #raya azul
        trellis.unfocus()
        
        graphics.off()
      }
    }
    ultimo<-min(val)
    df<- rbind(df,c(step,repetir,ultimo))
  }
}
stopImplicitCluster()

#GRAFICAR
names(df) <- c("Paso", "Repeticion", "Minimo")
df$Paso = as.factor(df$Paso)
ggplot(df, aes(x= Paso, y= Minimo, fill= Paso)) + 
  geom_boxplot()+
  labs(x = "Paso", y = "Valor mínimo")

#PRUEBA ESTADISTICA...
#Estadísticas descriptivas
df %>%
  group_by(Paso) %>%
  get_summary_stats(Minimo, type = "mean_sd")

#SUPUESTOS PARA ANOVA
#1:Outliers
df %>%
  group_by(Paso) %>%
  identify_outliers(Minimo)

#2:Normalidad por Shapiro
df %>%
  group_by(Paso) %>%
  shapiro_test(Minimo)

#3:Homogeneidad de varianza con prueba Levene
df %>%
  levene_test(Minimo~Paso)

#PRUEBA ESTADÍSTICA KRUSKAL WALLIS
kruskal.test(Minimo ~ Paso, data = df)

#PRUEBA WILCOXON
pairwise.wilcox.test(df$Minimo, df$Paso)