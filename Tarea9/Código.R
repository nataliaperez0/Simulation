n <- 50
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), m=rnorm(n), vel = numeric(n))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien son de 0 a 1
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
mmax = max(p$m) 
mmin = min(p$m)
p$m = (2 * (p$m - mmin) / (mmax - mmin) + 1) #masa entre 1 y 3
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
eps <- 0.001
G <- 0.6674 #valor supuesto para la constante gravitacional(6.674*10^(-11))

fuerza <- function(i) { 
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  mi = p [i,]$m
  fx1 = 0
  fy1 = 0
  fx2 = 0
  fy2 = 0
  for (j in 1:n) {
    cj <- p[j,]$c
    mj = p[j,]$m
    dir <- (-1)^(1 + 1 * (ci * cj < 0)) #es atraccion o repulsion
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps) 
    fg <- G * ((mi * mj) / ((sqrt(dx^2 + dy^2) + eps)^2)) #fuerza gravitatoria
    fx1 = fx1 - dx * factor
    fy1 = fy1 - dy * factor
    fx2 = fx2 - dx * fg
    fy2 = fy2 - dy * fg
    #sumatoria de fuerzas
    fx = fx1 + fx2
    fy = fy1 + fy2
  }
  return(c(fx, fy))
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}
png(paste("p9_t", tl, ".png", sep=""))
plot(p$x, p$y, col=colores[p$g+6], pch=16, cex=p$m, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
     main="Estado inicial", xlab="X", ylab="Y")
graphics.off()
for (iter in 1:tmax) {
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  delta <- 0.02 / max(abs(f)) # que nadie desplace un paso muy largo
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
  v = foreach(i = 1:n, .combine=c)%dopar% sqrt((delta * f[c(TRUE, FALSE)][i])^2 + (delta * f[c(FALSE, TRUE)][i])^2)
  p$vel = p$vel+v
  tl <- paste(iter, "", sep="")
  
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  png(paste("p9_t", tl, ".png", sep=""))
  plot(p$x, p$y, col=colores[p$g+6], pch=16, cex=p$m, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
       main=paste("Paso", iter), xlab="X", ylab="Y")
  graphics.off()
}
stopImplicitCluster()

library(ggplot2)
ggplot(data = p, aes(x= m, y= vel, color= c))+
  geom_point(size=2)+
  geom_smooth(method = "loess", se=FALSE, formula =y ~ x)+
  stat_summary(fun = mean, geom = "point",
               size = 0.7, fill = "black")+
  guides(scale = "none", color=guide_legend(title = "Carga"))+
  scale_x_continuous(name = "Masa")+
  scale_y_continuous(name = "Velocidad")+
  theme(plot.title = element_text(hjust = 0.5))