# -------------------------------------------------------------------------
# SCRIPT DE FUNCIONES DE DATOS (Contiene la lógica de limpieza y variables)
# Objetivo: Limpiar, seleccionar y transformar variables para cada modelo.
# -------------------------------------------------------------------------

# Cargar paquetes necesarios
library(dplyr)
library(MASS)    # Para Boston
library(mlbench) # Para Pima

# --- FUNCIÓN 1: Datos para RIDGE (Necesita Matrices y Escalado) ---
get_data_ridge <- function() {
  data("Boston", package = "MASS")
  df <- na.omit(Boston)

  # Ridge necesita que las variables estén en la misma escala (Estandarización)
  x_matrix <- as.matrix(scale(df %>% dplyr::select(-medv)))
  y_vector <- df$medv

  # Retornamos una lista con las matrices escaladas y el vector Y
  list(x = x_matrix, y = y_vector, data_original = df)
}

# --- FUNCIÓN 2: Datos para ROBUSTA (Necesita Dataframe limpio) ---
get_data_robusta <- function() {
  data("Boston", package = "MASS")
  df <- na.omit(Boston)
  # Robusta trabaja directo con el dataframe.
  df
}

# --- FUNCIÓN 3: Datos para CLASIFICACIÓN (LOGIT) ---
get_data_logit <- function() {
  data("PimaIndiansDiabetes2", package = "mlbench")
  # Logit necesita datos completos (sin NAs)
  df <- na.omit(PimaIndiansDiabetes2)
  df
}

# --- FUNCIÓN 4: Datos para PROBIT (Mismos que Logit) ---
get_data_probit <- function() {
  get_data_logit()
}