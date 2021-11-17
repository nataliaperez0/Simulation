binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

df = data.frame()
probn = c(0.98,0.65,0.35) #probabilidades negro
probg = c(0.95,0.75,0.55) #probabilidades gris
probb = c(0.45,0.10,0.005) #probabilidades blanco

for (ne in probn){
  for (g in probg){
    for (b in probb){
      for (replica in 1:12){
        modelos <- read.csv("digits.txt", sep=" ", header=FALSE, stringsAsFactors=F)
        modelos[modelos=='n'] <- ne
        modelos[modelos=='g'] <- g
        modelos[modelos=='b'] <- b
        
        r <- 5
        c <- 3
        dim <- r * c
        
        tasa <- 0.15
        tranqui <- 0.99
        
        tope <- 9
        digitos <- 0:tope
        k <- length(digitos)
        contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
        rownames(contadores) <- 0:tope
        colnames(contadores) <- c(0:tope, NA)
        
        n <- floor(log(k-1, 2)) + 1
        neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones
        
        for (t in 1:5000) { # entrenamiento
          d <- sample(0:tope, 1)
          pixeles <- runif(dim) < modelos[d + 1,]
          correcto <- binario(d, n)
          for (i in 1:n) {
            w <- neuronas[i,]
            deseada <- correcto[i]
            resultado <- sum(w * pixeles) >= 0
            if (deseada != resultado) {
              ajuste <- tasa * (deseada - resultado)
              tasa <- tranqui * tasa
              neuronas[i,] <- w + ajuste * pixeles
            }
          }
        }
        
        for (t in 1:300) { # prueba
          d <- sample(0:tope, 1)
          pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
          correcto <- binario(d, n)
          salida <- rep(FALSE, n)
          for (i in 1:n) {
            w <- neuronas[i,]
            deseada <- correcto[i]
            resultado <- sum(w * pixeles) >= 0
            salida[i] <- resultado
          }
          r <- min(decimal(salida, n), k) # todos los no-existentes van al final
          contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
        }
        print(contadores)
        #Calculo puntaje F-score
        precision = diag(contadores) / colSums(contadores[,1:10])
        recall = diag(contadores) / rowSums(contadores)
        fscore = (2 * precision * recall) / (precision + recall)
        result = c(ne, g, b, replica, fscore)
        df = rbind(df, result)
      }
    }
  }
}

names(df) <- c("Negro", "Gris","Blanco","Replica","0", "1","2","3","4","5","6","7","8","9")

library(ggplot2)
library(tidyr)

etiq=rep(c("0.98,0.95,0.45","0.98,0.95,0.10","0.98,0.95,0.005","0.98,0.75,0.45","0.98,0.75,0.10","0.98,0.75,0.005","0.98,0.55,0.45","0.98,0.55,0.10","0.98,0.55,0.005",
              "0.65,0.95,0.45","0.65,0.95,0.10","0.65,0.95,0.005","0.65,0.75,0.45","0.65,0.75,0.10","0.65,0.75,0.005","0.65,0.55,0.45","0.65,0.55,0.10","0.65,0.55,0.005",
              "0.35,0.95,0.45","0.35,0.95,0.10","0.35,0.95,0.005","0.35,0.75,0.45","0.35,0.75,0.10","0.35,0.75,0.005","0.35,0.55,0.45","0.35,0.55,0.10","0.35,0.55,0.005"),
            times=c(12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12))
df$Etiquetas <- etiq 

library(reshape2)
df1 <- melt(df,id.vars='Etiquetas', measure.vars=c('0','1','2','3','4','5','6','7','8','9'))

png("Figura1.png", width=15, height=15, units="cm", res=1200)
gr = ggplot(df1, aes(x= Etiquetas, y= value))+ geom_violin(fill="pink", color="purple", lwd=0.2)
gr + geom_boxplot(width=0.4, fill="cyan", color="black", lwd=0.2, outlier.size = 0.6)+ labs(x="Combinación de probabilidades (n,g,b)", y= "F-score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
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
df1 %>%
  group_by(Etiquetas) %>%
  get_summary_stats(value, type = "mean_sd")

#SUPUESTOS PARA ANOVA
#1:Outliers
df1 %>%
  group_by(Etiquetas) %>%
  identify_outliers(value)

#2:Normalidad por Shapiro
tapply(df1$value, df1$Etiquetas, shapiro.test)

#3:Homogeneidad de varianza con prueba Levene
df1 %>%
  levene_test(value~Etiquetas)

#PRUEBA ESTADÍSTICA KRUSKAL WALLIS
kruskal.test(value ~ Etiquetas, data = df1)

#PRUEBA WILCOXON
pairwise.wilcox.test(df1$value, df1$Etiquetas)