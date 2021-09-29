https://cran.r-project.org/web/packages/countcolors/vignettes/Introduction.html

library(jpeg)
library(countcolors)
library(scatterplot3d)

nasa=readJPEG("C:\\Users\\beren\\OneDrive\\Escritorio\\nasa.jpg")
print(nasa)
colordistance::plotPixels("nasa.jpg", lower = NULL, upper = NULL, n = 5000)