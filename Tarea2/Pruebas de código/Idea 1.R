dim = 5
num = dim**2
p = 0.5
valores = round(runif(num) < p)
actual <- matrix(valores, nrow=dim, ncol=dim, byrow=TRUE)