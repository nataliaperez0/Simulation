#IDEA 1
l <- 1
n <- 50
pi <- 0.05
pv = 0.1
v <- l / 20
agentes <- data.frame(x = double(), y = double(),
                      dx = double(), dy = double(),
                      estado  = character())
for (i in 1:n) {
  e <- "S"
  if (runif(1) < pi) {
    e <- "I"
  }
  if(runif(1) < pv){
    e = "V"
  }
  agentes <- rbind(agentes, data.frame(x = runif(1, 0, l),
                                       y = runif(1, 0, l),
                                       dx = runif(1, -v, v),
                                       dy = runif(1, -v, v),
                                       estado = e))
}

#IDEA 2
l <- 1
n <- 50
pi <- 0.05
pv = seq(0, 1, 0.1)
v <- l / 20
agentes <- data.frame(x = double(), y = double(),
                      dx = double(), dy = double(),
                      estado  = character())
for (va in pv){
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
                                         estado = e))
  }
}

#IDEA 3
l <- 1
n <- 50
pi <- 0.05
pv = seq(0, 1, 0.1)
v <- l / 20
replicas = 30
agentes <- data.frame(x = double(), y = double(),
                      dx = double(), dy = double(),
                      estado  = character())
for (va in pv){
  for (rep in 1:replicas)
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
                                         estado = e))
  }
}