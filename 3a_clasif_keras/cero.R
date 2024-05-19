library(tidyverse)
library(keras)

# Setear directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Setear estilo de gráficos
theme_set(theme_bw(base_size = 16))



# (1) Manipulación de datos
# =======================================

# Leer dataset
cero <- read.csv("../1b_datasets/cero.csv")

# Visualizar dataset
cero %>% ggplot(aes(x = x1, y = x2, color = y)) + geom_point(size = 5)

x <- cero %>% select(x1, x2) %>% scale()
y <- cero$y





# (2) Red neuronal
# =======================================

tensorflow::set_random_seed(341)

# Definir modelo
nn <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu", input_shape = ncol(x)) %>%
  layer_dense(units = 2, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compilar modelo (i.e. definir aprendizaje)
nn %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = list("accuracy")
)

# Entrenar modelo
history <- nn %>% fit(x, y, epochs = 100, batch_size = 50)

# Predecir respuesta
cero$yhat <- predict(nn, x)

# Visualizar predicción
cero %>% ggplot(aes(x = x1, y = x2, color = yhat)) + geom_point(size = 5)
