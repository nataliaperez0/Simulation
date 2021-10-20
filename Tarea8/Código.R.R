library(testit) # para pruebas, recuerda instalar antes de usar
tamas <- c(100, 200, 400)
n <- 100000
df = data.frame()

for (k in tamas){
  for (replica in 1:30){
    originales <- rnorm(k)
    cumulos <- originales - min(originales) + 1
    cumulos <- round(n * cumulos / sum(cumulos))
    assert(min(cumulos) > 0)
    diferencia <- n - sum(cumulos)
    if (diferencia > 0) {
      for (i in 1:diferencia) {
        p <- sample(1:k, 1)
        cumulos[p] <- cumulos[p] + 1
      }
    } else if (diferencia < 0) {
      for (i in 1:-diferencia) {
        p <- sample(1:k, 1)
        if (cumulos[p] > 1) {
          cumulos[p] <- cumulos[p] - 1
        }
      }
    }
    
    png("p8_init.png")
    plot(hist(cumulos), main="Estado inicial",
         xlab="Tama\u{00f1}o de c\u{00fa}mulos", ylab="Frecuencia absoluta")
    graphics.off()
    
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
    assert(sum(cumulos) == n)
    c <- median(cumulos) # tamaño critico de cumulos
    d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva
    
    primero <- as.data.frame(table(cumulos))
    names(primero) <- c("tam", "num")
    primero$tam <- as.numeric(levels(primero$tam))[primero$tam]
    assert(sum(primero$num * primero$tam) == n)
    
    filtrados1 = primero[primero$tam >= c,]
    filtrados1$cont = filtrados1$tam * filtrados1$num
    f1 = sum(filtrados1$cont) # particulas removidas
    porcentaje1 = 100 * f1/n # porcentaje exitosamente filtrado
    paso1 = 0
    resultado1 = c(k, replica, paso1, porcentaje1, c)
    df = rbind(df, resultado1)
    names(df) = c("k", "Replica", "Iteracion", "filtrado", "c")
    assert(sum(abs(cumulos)) == n)
    
    rotura <- function(x) {
      return (1 / (1 + exp((c - x) / d)))
    }
    union <- function(x) {
      return (exp(-x / c))
    }
    romperse <- function(tam, cuantos) {
      romper <- round(rotura(tam) * cuantos) # independientes
      resultado <- rep(tam, cuantos - romper) # los demas
      if (romper > 0) {
        for (cumulo in 1:romper) { # agregar las rotas
          t <- 1
          if (tam > 2) { # sample no jala con un solo valor
            t <- sample(1:(tam-1), 1)
          }
          resultado <- c(resultado, t, tam - t)
        }
      }
      assert(sum(resultado) == tam * cuantos) # no hubo perdidas
      return(resultado)
    }
    unirse <- function(tam, cuantos) {
      unir <- round(union(tam) * cuantos) # independientes
      if (unir > 0) {
        division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
        assert(sum(abs(division)) == tam * cuantos)
        return(division)
      } else {
        return(rep(tam, cuantos))
      }
    }
    freq <- as.data.frame(table(cumulos))
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    duracion <- 50
    digitos <- floor(log(duracion, 10)) + 1
    for (paso in 1:duracion) {
      assert(sum(cumulos) == n)
      cumulos <- integer()
      for (i in 1:dim(freq)[1]) { # fase de rotura
        urna <- freq[i,]
        if (urna$tam > 1) { # no tiene caso romper si no se puede
          cumulos <- c(cumulos, romperse(urna$tam, urna$num))
        } else {
          cumulos <- c(cumulos, rep(1, urna$num))
        }
      }
      assert(sum(cumulos) == n)
      assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
      freq <- as.data.frame(table(cumulos)) # actualizar urnas
      names(freq) <- c("tam", "num")
      freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
      assert(sum(freq$num * freq$tam) == n)
      cumulos <- integer()
      for (i in 1:dim(freq)[1]) { # fase de union
        urna <- freq[i,]
        cumulos <- c(cumulos, unirse(urna$tam, urna$num))
      }
      assert(sum(abs(cumulos)) == n)
      assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
      juntarse <- -cumulos[cumulos < 0]
      cumulos <- cumulos[cumulos > 0]
      assert(sum(cumulos) + sum(juntarse) == n)
      nt <- length(juntarse)
      if (nt > 0) {
        if (nt > 1) {
          juntarse <- sample(juntarse)
          for (i in 1:floor(nt / 2) ) {
            cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
          }
        }
        if (nt %% 2 == 1) {
          cumulos <- c(cumulos, juntarse[nt])
        }
      }
      assert(sum(cumulos) == n)
      freq <- as.data.frame(table(cumulos))
      names(freq) <- c("tam", "num")
      freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
      assert(sum(freq$num * freq$tam) == n)
      tl <- paste(paso, "", sep="")
      while (nchar(tl) < digitos) {
        tl <- paste("0", tl, sep="")
      }
      if (replica == 1){
        png(paste("p8_ct", tl, "k=", k, "rep=", replica, ".png", sep=""), width=300, height=300)
        tope <- 50 * ceiling(max(cumulos) / 50)
        hist(cumulos, breaks=seq(0, tope, 50), 
             main=paste("Paso", paso, "con ambos fen\u{00f3}menos"), freq=FALSE,
             ylim=c(0, 0.02), xlab="Tama\u{00f1}o", ylab="Frecuencia relativa")
        graphics.off() 
      }
      freq
      filtrados = freq[freq$tam >= c,]
      filtrados$cont = filtrados$tam * filtrados$num
      f = sum(filtrados$cont) # particulas removidas
      porcentaje = 100 * f/n # porcentaje exitosamente filtrado
      resultado = c(k, replica, paso, porcentaje, c)
      df = rbind(df, resultado)
  
      assert(sum(abs(cumulos)) == n)
    }  
  }
}

library(ggplot2)
df$Iteracion = as.factor(df$Iteracion)
dfs = split.data.frame(df, f = df$k)
ggplot(dfs$`100`, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#F8766D")+
  labs(x = "Iteracion", y = "% filtrado", title = 'k = 100')

ggplot(dfs$`200`, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#8800FF")+
  labs(x = "Iteracion", y = "% filtrado", title = 'k = 200')

ggplot(dfs$`400`, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#FF8800")+
  labs(x = "Iteracion", y = "% filtrado", title = 'k = 400')


library(tidyverse)
library(ggpubr)
library(car)
library(rstatix)
library(rapportools)
library(readr)
library(gridExtra)

#PRUEBA ESTADISTICA...
#Estadísticas descriptivas

#k = 100
dfs$`100` %>%
  group_by(k) %>%
  get_summary_stats(filtrado, type = "mean_sd")

#k = 200
dfs$`200` %>%
  group_by(k) %>%
  get_summary_stats(filtrado, type = "mean_sd")

#k = 400
dfs$`400` %>%
  group_by(k) %>%
  get_summary_stats(filtrado, type = "mean_sd")

#SUPUESTOS PARA ANOVA
#1:Outliers

#k = 100
dfs$`100` %>%
  group_by(k) %>%
  identify_outliers(filtrado)

#k = 200
dfs$`200` %>%
  group_by(k) %>%
  identify_outliers(filtrado)

#k = 400
dfs$`400` %>%
  group_by(k) %>%
  identify_outliers(filtrado)

#2:Normalidad por Shapiro

#k = 100
dfs$`100` %>%
  group_by(k) %>%
  shapiro_test(filtrado)

#k = 200
dfs$`200` %>%
  group_by(k) %>%
  shapiro_test(filtrado)

#k = 400
dfs$`400` %>%
  group_by(k) %>%
  shapiro_test(filtrado)

#3:Homogeneidad de varianza con prueba Levene

#k = 100
dfs$`100` %>%
  levene_test(filtrado$k)

#k = 200
dfs$`200` %>%
  levene_test(filtrado~k)

#k = 400
dfs$`400` %>%
  levene_test(filtrado~k)

#PRUEBA ESTADÍSTICA KRUSKAL WALLIS

#k = 100
dfs$`100` %>%
kruskal.test(filtrado ~ k)

#k = 200
dfs$`200` %>%
  kruskal.test(filtrado ~ k)

#k = 400
dfs$`400` %>%
  kruskal.test(filtrado ~ k)

#PRUEBA WILCOXON
pairwise.wilcox.test((dfs$`100`)$filtrado, (dfs$`100`)$Iteracion)
pairwise.wilcox.test((dfs$`200`)$filtrado, (dfs$`200`)$Iteracion)
pairwise.wilcox.test((dfs$`400`)$filtrado, (dfs$`200`)$Iteracion)