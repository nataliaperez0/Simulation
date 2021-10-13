#1
g = function(x, y){
  return((sin(x^2) * (x + 1)^4 - 30 * x^2 -50 * x + (y + 1)^4 - 30 * y^2 - 50 * y))
}
x = seq(-6, 5, 0.25)
y = x
z = outer(x, y, g)
png("p7_2d.png", width = 700, height = 700)
persp(x, y, z, shade = 0.2, col = 'orange', theta = 40, phi = 30)
graphics.off()

#2
g = function(x, y){
  return((sin(x*2) * (x + 1)^4 - 30 * x^2 -50 * x + (y + 1)^4 - 30 * y^2 - 50 * y))
}
x = seq(-6, 5, 0.25)
y = x
z = outer(x, y, g)
png("p7_2d.png", width = 700, height = 700)
persp(x, y, z, shade = 0.2, col = 'orange', theta = 40, phi = 30)
graphics.off()

#3
g <- function(x,y) {
  return((sin(x*2) * ((x + 1)^4 - 30 * x^2 -50 * x) + (y + 1)^4 - 30 * y^2 - 50 * y))
}
x <- seq(-2, 5, 0.25) 
y <- x
z <- outer(x, y, g)
png("p7_2d.png", width = 700, height = 700)
persp(x, y, z, shade = 0.2, col = 'pink', theta = 40, phi = 30)
graphics.off()

#4
g = function(x, y){
  return((sin(x*2) * ((x + 1)^4 - 30 * x^2 -50 * x) + (y + 1)^4 - 30 * y^2 - 50 * y))
}
x = seq(-5, 5, 0.25)
y = x
z = outer(x, y, g)
png("p7_flat_1.png", width = 500, height = 500)
image(z)
graphics.off()

#5
g <- function(x,y) {
  return((sin(x*2) * ((x + 1)^4 - 30 * x^2 -50 * x) + (y + 1)^4 - 30 * y^2 - 50 * y))
}
x <- seq(-2, 5, 0.25) 
y <- x
z <- outer(x, y, g)
dimnames(z) = list(x, y)
library(reshape2)
d = melt(z)
names(d) = c("x", "y", "z")
library(lattice)
png("p7_flat_2.png", width = 500, height = 500)
levelplot(z ~ x * y, data = d, col.regions = terrain.colors(100))
graphics.off()