g = function(x) {
  return((sin(x*2) * ((x + 1)^4 - 30 * x^2 -50 * x) + (y + 1)^4 - 30 * y^2 - 50 * y))
}
low = -5
high = -low
x = seq(low, high, 0.25)
y = x
z = outer(x, y, g)

tmax <- 50
step <- 0.25
replicas = 20
digitos <- floor(log(tmax, 10)) + 1

replica <- function(t) {
  currx <- runif(1, low, high)
  curry <- runif(1, low, high)
  bestx <- currx
  besty <- curry
  for (tiempo in 1:t) {
    deltax <- runif(1, 0, step)
    deltay <- runif(1, 0, step)
    left <- currx - deltax
    right <- currx + deltax
    arriba <- curry + deltay
    abajo <- curry - deltay
    if (f(left) > f(right)) {
      curr <- left
    } else {
      curr <- right
    }
    if (f(arriba) > f(abajo)){
      curr <- arriba
    } else{
      curr <- abajo
    }
    if (f(currx, curry) > f(bestx, besty)) {
      bestx <- currx
      besty <- curry
    }
  }
  return(best)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
x <- seq(low, high, length.out=500)
y <- foreach(i = x, .combine=c) %dopar% f(i)
