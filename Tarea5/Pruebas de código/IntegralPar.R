library(ggplot2)
desde = 3
hasta = 7
bueno = 0.048834 #número de Wolfram Alpha
n = seq(1, 6, 1)
muestra = c(500, 5000, 50000, 500000, 5000000, 50000000) # puntitos en el cuadro
cuantos <- 2
df = data.frame()

for (m in muestra){
  for (replica in 1:30){
    f <- function(x) { return(1 / (exp(x) + exp(-x))) } # funcion que piden
    g <- function(x) { return((2 / pi) * f(x)) } # normalizado a distr
    
    suppressMessages(library(distr)) # paquete
    generador  <- r(AbscontDistribution(d = g)) # creamos un generador
    parte <- function() {
      valores <- generador(m)
      return(sum(valores >= desde & valores <= hasta))
    }
    suppressMessages(library(doParallel))
    registerDoParallel(makeCluster(detectCores(logical = FALSE) - 2))
    montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
    stopImplicitCluster()
    integral <- sum(montecarlo) / (cuantos * m)
    resultado <- (pi / 2) * integral
    
    for (i in n) {
      b = trunc(bueno*10^i)/10^i
      r = trunc(resultado*10^i)/10^i
      
      if (r == b) {
        deci = i
      } else {
        break
      }
    }
    datos <- c(m, replica, resultado, deci)
    df = rbind(df, datos)
    #cat(m, replica, resultado, deci,'\n')
  }
}

names(df) <- c("Muestra", "Replica", "Resultado", "Decimales")
df$Muestra = as.factor(df$Muestra)
ggplot(df, aes(x= Muestra, y= Decimales, fill= Muestra)) + 
  geom_boxplot()+
  labs(x = "Muestra", y = "Decimales correctos", title = 'Gráfica') #nombres