library(tidyverse)
library(torch)
library(luz)

# Setear directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# (1) Manipulación de datos
# =======================================

# Leer dataset
estrella <- read.csv("../1b_datasets/estrella.csv")

# Visualizar dataset
estrella %>% ggplot(aes(x = x1, y = x2, color = y)) + geom_point(size = 5) + theme_minimal()

x <- estrella %>% select(x1, x2) %>% scale()
y <- estrella$y





# (2) Red neuronal
# =======================================

set.seed(341)
torch_manual_seed(341)

# Definir dataset (tensores) y dataloader
ds <- tensor_dataset(
  torch_tensor(x),
  torch_tensor(y)$to(torch_float())
)
dl <- dataloader(ds, batch_size = 50, shuffle = TRUE)

# Definir arquitectura de red
d_in <- ncol(x)
d_hidden <- 5
d_out <- 1

# Definir modelo
net <- nn_module(
  initialize = function(d_in, d_hidden, d_out) {
    self$net <- nn_sequential(
      nn_linear(d_in, d_hidden),
      nn_relu(),
      nn_linear(d_hidden, d_out),
      nn_sigmoid()
    )
  },
  forward = function(x) {
    self$net(x)
  }
)

# Compilar y entrenar modelo
fitted <- net %>%
  setup(loss = nn_bce_loss(), optimizer = optim_rmsprop) %>%
  set_hparams(
    d_in = d_in,
    d_hidden = d_hidden, 
    d_out = d_out
  ) %>%
  fit(dl, epochs = 100)

# Extraer predicciones
estrella$yhat <- fitted$model(x) %>% as.numeric()

# Visualizar predicción
estrella %>% ggplot(aes(x = x1, y = x2, color = yhat)) + geom_point(size = 5) + theme_minimal()
