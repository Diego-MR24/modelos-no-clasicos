# Script: 03_robusta_analysis.R
# Objetivo: Comparar Regresión Clásica (OLS) vs Regresión Robusta (RLM)
#           para mitigar el efecto de outliers en 'medv'.

library(MASS)       
library(caret)
library(tidyverse)

# Cargar funciones de preparación de datos
source("data/data.R")

# 1. Carga y División de Datos
# get_data_robusta() retorna el dataframe Boston limpio
df_rob <- get_data_robusta()

set.seed(123)
train_idx <- createDataPartition(df_rob$medv, p = 0.7, list = FALSE)

train_data <- df_rob[train_idx, ]
test_data  <- df_rob[-train_idx, ]

# 2. Entrenamiento de Modelos
# Modelo A: OLS Clásico (Mínimos Cuadrados Ordinarios) - Muy sensible a outliers
model_ols <- lm(medv ~ ., data = train_data)

# Modelo B: Regresión Robusta (Estimador-M de Huber)
# Reduce el peso de las observaciones con residuos grandes
model_rob <- rlm(medv ~ ., data = train_data)

# 3. Predicción y Métricas
# Predicciones
preds_ols <- predict(model_ols, newdata = test_data)
preds_rob <- predict(model_rob, newdata = test_data)

# Cálculo de RMSE (Raíz del Error Cuadrático Medio)
rmse_ols <- sqrt(mean((test_data$medv - preds_ols)^2))
rmse_rob <- sqrt(mean((test_data$medv - preds_rob)^2))

# 4. Gráficos de Diagnóstico (Comparación de Residuos)
# Este gráfico es clave para mostrar cómo la Robusta maneja los errores extremos
png("plots/02_robusta_vs_ols_residuos.png", width=800, height=400)
par(mfrow=c(1,2)) # Dividir pantalla en 2 paneles

# Panel 1: Residuos OLS
plot(test_data$medv, test_data$medv - preds_ols,
     main = "Residuos OLS (Sensible)",
     xlab = "Valor Real", ylab = "Residuos",
     col = "red", pch = 19)
abline(h = 0, lwd = 2)

# Panel 2: Residuos Robusta
plot(test_data$medv, test_data$medv - preds_rob,
     main = "Residuos Robusta (Estable)",
     xlab = "Valor Real", ylab = "Residuos",
     col = "blue", pch = 19)
abline(h = 0, lwd = 2)

dev.off()

# 5. Salida en Consola
cat("--- Análisis de Regresión Robusta ---\n")
cat("Comparación de Error (RMSE) en Test Set:\n")
cat("RMSE OLS (Clásico):", rmse_ols, "\n")
cat("RMSE Robusta:      ", rmse_rob, "\n")

cat("\n--- Diferencia en Coeficientes (Ejemplo) ---\n")
cat("Observa cómo cambian los coeficientes al ignorar outliers:\n")
vars_select <- c("crim", "rm", "tax")
print(data.frame(
  OLS = coef(model_ols)[vars_select],
  Robusta = coef(model_rob)[vars_select]
))