bueno = 3.141592 #número pi
n = seq(1, 6, 1)
muestra = c(100, 2000, 50000) # puntitos en el cuadro
df = data.frame()

for (muchos in muestra) {
  for (replica in 1:15){
    interior = 0
    for (r in 1:muchos) {
      x = runif(1, -1, 1)
      y = runif(1, -1, 1)
      d = sqrt(x*x + y*y)
      if (d < 1) {
        interior = interior + 1
      }
    }
    
    tasa = interior / muchos
    pi = 4 * tasa
    #print(pi)
    for (i in n) {
      b = trunc(bueno*10^i)/10^i
      r = trunc(pi*10^i)/10^i
      
      if (r == b) {
        deci = i
      } else {
        break
      }
    }
    datos <- c(muchos, replica, pi, deci)
    df = rbind(df, datos)
  }
}