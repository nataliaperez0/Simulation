g <- function(x,y) {
  return((sin(x*2) * ((x + 1)^4 - 30 * x^2 -50 * x) + (y + 1)^4 - 30 * y^2 - 50 * y))
}
x <- seq(-2, 5, 0.25) 
y <- x
z <- outer(x, y, g)
png("p7_2d.png", width = 700, height = 700)
persp(x, y, z, shade = 0.2, col = 'pink', theta = 40, phi = 30)
graphics.off()