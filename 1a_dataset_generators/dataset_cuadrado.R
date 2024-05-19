library(tidyverse)

# Setear directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Determinar si un punto pertenece o no al cuadrado
es_cuadrado <- function(x1, x2) {
  if ((x1 >= -1 & x1 <= 1) & (x2 >= -1 & x2 <= 1)) return(1.0) # cuadrado
  return(0.0) # exterior
}

# Crear dataset
set.seed(505)
n <- 5000
cuadrado <- data.frame(
  x1 = runif(n, min = -2, max = 2),
  x2 = runif(n, min = -2, max = 2)
)
cuadrado$y <- as.numeric(map2(cuadrado$x1, cuadrado$x2, es_cuadrado))

# Visualizar dataset
cuadrado %>% ggplot(aes(x = x1, y = x2, color = y)) + geom_point(size = 5)

# Guardar dataset
cuadrado %>% write.csv("../1b_datasets/cuadrado.csv", row.names = FALSE)
