library(testit) # para pruebas, recuerda instalar antes de usar
tamas <- c(100, 200, 400)
n <- 100000
df = data.frame()
se = c(300, 600, 1200, 1800)

for (k in tamas){
  for (c in se){
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
      
      assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
      assert(sum(cumulos) == n)
      
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
}


a1 = df[df$k == 100 & df$c == 300, ]
a2 = df[df$k == 100 & df$c == 600, ]
a3 = df[df$k == 100 & df$c == 1200, ]
a4 = df[df$k == 100 & df$c == 1800, ]

b1 = df[df$k == 200 & df$c == 300, ]
b2 = df[df$k == 200 & df$c == 600, ]
b3 = df[df$k == 200 & df$c == 1200, ]
b4 = df[df$k == 200 & df$c == 1800, ]

c1 = df[df$k == 400 & df$c == 300, ]
c2 = df[df$k == 400 & df$c == 600, ]
c3 = df[df$k == 400 & df$c == 1200, ]
c4 = df[df$k == 400 & df$c == 1800, ]

library(ggplot2)
a1$Iteracion = as.factor(a1$Iteracion)
ggplot(a1, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#F8766D")+
  labs(x = "Iteracion", y = "% filtrado", title = 'k = 100 y c = 300')

a2$Iteracion = as.factor(a2$Iteracion)
ggplot(a2, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#F8766D")+
  labs(x = "Iteracion", y = "% filtrado", title = 'k = 100 y c = 600')

a3$Iteracion = as.factor(a3$Iteracion)
ggplot(a3, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#F8766D")+
  labs(x = "Iteracion", y = "% filtrado", title = 'k = 100 y c = 1200')

a4$Iteracion = as.factor(a4$Iteracion)
ggplot(a4, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#F8766D")+
  labs(x = "Iteracion", y = "% filtrado", title = 'k = 100 y c = 1800')

b1$Iteracion = as.factor(b1$Iteracion)
ggplot(b1, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#8800FF")+
  labs(x = "Iteracion", y = "% filtrado", title = 'k = 200 y c = 300')

b2$Iteracion = as.factor(b2$Iteracion)
ggplot(b2, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#8800FF")+
  labs(x = "Iteracion", y = "% filtrado", title = 'k = 200 y c = 600')

b3$Iteracion = as.factor(b3$Iteracion)
ggplot(b3, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#8800FF")+
  labs(x = "Iteracion", y = "% filtrado", title = 'k = 200 y c = 1200')

b4$Iteracion = as.factor(b4$Iteracion)
ggplot(b4, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#8800FF")+
  labs(x = "Iteracion", y = "% filtrado", title = 'k = 200 y c = 1800')

c1$Iteracion = as.factor(c1$Iteracion)
ggplot(c1, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#FF8800")+
  labs(x = "Iteracion", y = "% filtrado", title = 'k = 400 y c = 300')

c2$Iteracion = as.factor(c2$Iteracion)
ggplot(c2, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#FF8800")+
  labs(x = "Iteracion", y = "% filtrado", title = 'k = 400 y c = 600')

c3$Iteracion = as.factor(c3$Iteracion)
ggplot(c3, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#FF8800")+
  labs(x = "Iteracion", y = "% filtrado", title = 'k = 400 y c = 1200')

c4$Iteracion = as.factor(c4$Iteracion)
ggplot(c4, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#FF8800")+
  labs(x = "Iteracion", y = "% filtrado")

a1[a1$Iteracion == 0,] %>%
  get_summary_stats(filtrado, type = "mean_sd")

a2[a2$Iteracion == 0,] %>%
  get_summary_stats(filtrado, type = "mean_sd")

a3[a3$Iteracion == 0,] %>%
  get_summary_stats(filtrado, type = "mean_sd")

a4[a4$Iteracion == 1,] %>%
  get_summary_stats(filtrado, type = "mean_sd")

b1[b1$Iteracion == 0,] %>%
  get_summary_stats(filtrado, type = "mean_sd")

b2[b2$Iteracion == 1,] %>%
  get_summary_stats(filtrado, type = "mean_sd")

b3[b3$Iteracion == 2,] %>%
  get_summary_stats(filtrado, type = "mean_sd")

b4[b4$Iteracion == 2,] %>%
  get_summary_stats(filtrado, type = "mean_sd")

c1[c1$Iteracion == 0,] %>%
  get_summary_stats(filtrado, type = "mean_sd")

g1 =c1 %>%
  group_by(Iteracion) %>%
  get_summary_stats(filtrado, type = "mean_sd")

g1 %>%
  get_summary_stats(mean, type = "max")

c2[c2$Iteracion == 3,] %>%
  get_summary_stats(filtrado, type = "mean_sd")

c3[c3$Iteracion == 3,] %>%
  get_summary_stats(filtrado, type = "mean_sd")

c4[c4$Iteracion == 3,] %>%
  get_summary_stats(filtrado, type = "mean_sd")