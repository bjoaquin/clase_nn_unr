library(tidyverse)
library(keras)

# Setear directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Setear estilo de gráficos
theme_set(theme_bw(base_size = 16))



# (1) Manipulación de datos
# =======================================

# Leer dataset
estrella <- read.csv("../1b_datasets/estrella.csv")

# Visualizar dataset
estrella %>% ggplot(aes(x = x1, y = x2, color = y)) + geom_point(size = 5)

x <- estrella %>% select(x1, x2) %>% scale()
y <- estrella$y





# (2) Red neuronal
# =======================================

tensorflow::set_random_seed(911)

# Definir modelo
nn <- keras_model_sequential() %>%
  layer_dense(units = 5, activation = "relu", input_shape = ncol(x)) %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compilar modelo (i.e. definir aprendizaje)
nn %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = list("accuracy")
)

# Entrenar modelo
history <- nn %>% fit(x, y, epochs = 200, batch_size = 50)

# Predecir respuesta
estrella$yhat <- predict(nn, x)

# Visualizar predicción
estrella %>% ggplot(aes(x = x1, y = x2, color = yhat)) + geom_point(size = 5)
