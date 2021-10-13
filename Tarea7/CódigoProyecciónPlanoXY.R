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