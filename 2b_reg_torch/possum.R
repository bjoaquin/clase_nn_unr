library(tidyverse)
library(torch)
library(luz)

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

set.seed(341)
torch_manual_seed(341)

# Definir dataset y dataloader
ds <- tensor_dataset(
  torch_tensor(x),
  torch_tensor(y)$to(torch_float())
)
dl <- dataloader(ds, batch_size = 10, shuffle = TRUE)

# Definir arquitectura de red
d_in <- ncol(x)
d_hidden1 <- 8
d_hidden2 <- 4
d_out <- 1

# Definir modelo
net <- nn_module(
  initialize = function(d_in, d_hidden1, d_hidden2, d_out) {
    self$net <- nn_sequential(
      nn_linear(d_in, d_hidden1),
      nn_relu(),
      nn_linear(d_hidden1, d_hidden2),
      nn_relu(),
      nn_linear(d_hidden2, d_out),
      nn_relu()
    )
  },
  forward = function(x) {
    self$net(x)
  }
)

# Compilar y entrenar modelo
fitted <- net %>%
  setup(loss = nn_mse_loss(), optimizer = optim_adam) %>%
  set_hparams(
    d_in = d_in,
    d_hidden1 = d_hidden1, d_hidden2 = d_hidden2, 
    d_out = d_out
  ) %>%
  fit(dl, epochs = 300)

# Predecir respuesta
performance <- data.frame(y = y, yhat_nn = as.numeric(fitted$model(x)))

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
