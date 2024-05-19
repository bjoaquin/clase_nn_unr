# Fuente: https://www.quora.com/How-do-I-plot-a-5-point-star-on-a-graph

library(tidyverse)

# Setear directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Funciones para hallar los vértices de la estrella
sine_fn   <- function(n) sin(4*n*pi/5)
cosine_fn <- function(n) cos(4*n*pi/5)

# Vértices de la estrella
Px <- 0:4 %>% sine_fn()
Py <- 0:4 %>% cosine_fn()

# Pendientes de las rectas que definen la estrella
slope_fn <- function(n) {
  next_n <- ifelse(n == 5, 1, n+1)
  (Py[next_n] - Py[n]) / (Px[next_n] - Px[n])
}
slopes <- 1:5 %>% slope_fn()

# Interceptos de las rectas que definen la estrella
intercept_fn <- function(n) Py[n] - slopes[n] * Px[n]
intercepts <- 1:5 %>% intercept_fn()

# Función para evaluar la n-ésima recta en un valor de x
lines <- function(n, x) slopes[n] * x + intercepts[n]

# Condiciones cuya unión define el conjunto estrella
cond1 <- function(x, y) y > lines(2,x) & y < lines(3,x) & y > lines(4,x)
cond2 <- function(x, y) y < lines(1,x) & y > lines(2,x) & y < lines(5,x)
cond3 <- function(x, y) y < lines(1,x) & y > lines(4,x) & y < lines(5,x)

# Determinar si un punto pertenece o no a la estrella
es_estrella <- function(x, y) {
  ifelse(cond1(x,y) | cond2(x,y) | cond3(x,y), 1.0, 0.0)
}

# Crear dataset
set.seed(505)
n <- 5000
estrella <- data.frame(
  x1 = runif(n, min = -1.5, max = 1.5),
  x2 = runif(n, min = -1.5, max = 1.5)
)
estrella$y <- as.numeric(map2(estrella$x1, estrella$x2, es_estrella))

# Visualizar dataset
estrella %>% ggplot(aes(x = x1, y = x2, color = y)) + geom_point(size = 5)

# Guardar dataset
estrella %>% write.csv("../1b_datasets/estrella.csv", row.names = FALSE)
