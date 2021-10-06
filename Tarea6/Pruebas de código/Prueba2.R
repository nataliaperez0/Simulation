#IDEA 3
library(ggplot2)
l <- 1
n <- 50
pi <- 0.05
pv = seq(0, 1, 0.1)
v <- l / 20
replicas = 30
agentes <- data.frame(x = double(), y = double(),
                      dx = double(), dy = double(),
                      estado  = character(), vacuna = double())
for (va in pv){
  for (rep in 1:replicas){
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
                                           estado = e, vacuna = va))
    }
  }
}

ggplot(data = agentes, aes(x = vacuna, y = estado, fill = estado)) +
  geom_boxplot() + theme_bw() + 
  labs(x = "Probabilidad de vacunación", y = "Estado del agente") 