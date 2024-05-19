library(tidyverse)
library(keras)

# Setear directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Setear estilo de gráficos
theme_set(theme_bw(base_size = 16))



# (1) Manipulación de datos
# =======================================

# Leer dataset
cuadrado <- read.csv("../1b_datasets/cuadrado.csv")

# Visualizar dataset
cuadrado %>% ggplot(aes(x = x1, y = x2, color = y)) + geom_point(size = 5)

x <- cuadrado %>% select(x1, x2) %>% scale()
y <- cuadrado$y





# (2) Red neuronal
# =======================================

tensorflow::set_random_seed(341)

# Definir modelo
nn <- keras_model_sequential() %>%
  layer_dense(units = 4, activation = "relu", input_shape = ncol(x)) %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compilar modelo (i.e. definir aprendizaje)
nn %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = list("accuracy")
)

# Entrenar modelo
history <- nn %>% fit(x, y, epochs = 300, batch_size = 50)

# Predecir respuesta
cuadrado$yhat <- predict(nn, x)

# Visualizar predicción
cuadrado %>% ggplot(aes(x = x1, y = x2, color = yhat)) + geom_point(size = 5)
