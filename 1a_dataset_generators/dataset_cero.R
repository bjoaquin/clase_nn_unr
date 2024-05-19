library(tidyverse)

# Setear directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Determinar si un punto pertenece o no al cero
es_cero <- function(x1, x2) {
  if ((x1 >=1 & x1 <= 4) & (x2 >= 1 & x2 <= 6)) {
    if ((x1 >=2 & x1 <= 3) & (x2 >= 2 & x2 <= 5)) return(0.0) # rectángulo interno
    return(1.0) # rectángulo externo
  }
  return(0.0) # exterior
}

# Crear dataset
set.seed(505)
n <- 5000
cero <- data.frame(
  x1 = runif(n, min = 0, max = 5),
  x2 = runif(n, min = 0, max = 7)
)
cero$y <- as.numeric(map2(cero$x1, cero$x2, es_cero))

# Visualizar dataset
cero %>% ggplot(aes(x = x1, y = x2, color = y)) + geom_point(size = 5)

# Guardar dataset
cero %>% write.csv("../1b_datasets/cero.csv", row.names = FALSE)
