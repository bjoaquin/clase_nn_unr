library(tidyverse)
library(keras)

# Setear directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Setear estilo de gráficos
theme_set(theme_bw(base_size = 16))



# (1) Manipulación de datos
# =======================================

# Cargar dataset
# (Fuente: https://www.kaggle.com/datasets/abrambeyer/openintro-possum)
df <- read.csv("../1b_datasets/possum.csv")
df <- df %>% drop_na()

# Eliminar variable "case" (no sirve)
df <- df %>% select(!c(case))

# Estandarizar variables numéricas
x <- df %>% mutate_if(is.numeric, scale)
# Codificar variables categóricas (dummies)
x <- model.matrix(age ~ . -1 , data = x)

y <- df$age





# (2) Red neuronal
# =======================================

tensorflow::set_random_seed(341)

# Definir modelo
nn <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu", input_shape = ncol(x)) %>%
  layer_dense(units = 4, activation = "relu") %>%
  layer_dense(units = 1, activation = "relu")

# Compilar modelo (i.e. definir aprendizaje)
nn %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = list("mse")
)

# Entrenar modelo
history <- nn %>% fit(x, y, epochs = 300, batch_size = 10)

# Predecir respuesta
performance <- data.frame(y = y, yhat_nn = predict(nn, x))

# Visualizar predicción
performance %>% 
  ggplot(aes(x = y, y = yhat_nn)) + 
  geom_segment(
    x = min(y), y = min(y), xend = max(y), yend = max(y),
    linewidth = 1, color = "red"
  ) +
  geom_point(size = 2) +
  xlab("Respuesta observada") + ylab("Respuesta predicha")





# (3) Comparación con modelo clásico
# =======================================

# Aplicar modelo de regresión lineal
reg <- lm(age ~ ., data = df)
summary(reg)
mean(summary(reg)$residuals^2) # MSE
performance$yhat_reg <- reg$fitted.values

# Comparar valores predichos por c/ modelo
performance <- performance %>% 
  pivot_longer(!y, names_to = "modelo", values_to = "yhat") %>% 
  mutate(modelo = if_else(modelo == "yhat_nn", "Red Neuronal", "Regresión Lineal"))

# Graficar resultados
performance %>% 
  ggplot(aes(x = y, y = yhat)) + 
  geom_segment(
    x = min(y), y = min(y), xend = max(y), yend = max(y),
    linewidth = 1, color = "red"
  ) +
  geom_point(size = 2) + facet_wrap(vars(modelo)) + 
  xlab("Respuesta observada") + ylab("Respuesta predicha")
