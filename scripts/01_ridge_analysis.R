# Objetivo: Implementación de Regresión Ridge (Regularización L2)
#           prediciendo 'medv' en el dataset Boston Housing.

library(glmnet)
library(caret)

source("data/data.R")

datos <- get_data_ridge()

set.seed(123)
train_idx <- createDataPartition(datos$y, p = 0.7, list = FALSE)

x_train <- datos$x[train_idx, ]
y_train <- datos$y[train_idx]
x_test  <- datos$x[-train_idx, ]
y_test  <- datos$y[-train_idx]

# 2. Entrenamiento del Modelo (Validación Cruzada)
# alpha = 0 especifica Regresión Ridge (alpha = 1 sería Lasso)
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)
best_lambda <- cv_ridge$lambda.min

# 3. Gráficos de Diagnóstico
# Gráfico 1: Curva de Validación Cruzada (MSE vs Lambda)
png("plots/01_ridge_curva_cv.png")
plot(cv_ridge)
title("Curva de Validación Cruzada Ridge", line = 2.5)
dev.off()

# Gráfico 2: Camino de Coeficientes (Trace Plot)
# Muestra la contracción de coeficientes al aumentar la penalización lambda
ridge_full <- glmnet(x_train, y_train, alpha = 0)
png("plots/01_ridge_trace_plot.png")
plot(ridge_full, xvar = "lambda", label = TRUE)
title("Camino de Coeficientes Ridge", line = 2.5)
dev.off()

# 4. Predicción y Métricas de Desempeño
ridge_preds <- predict(cv_ridge, s = best_lambda, newx = x_test)

rmse_ridge <- sqrt(mean((y_test - ridge_preds)^2))
mae_ridge <- mean(abs(y_test - ridge_preds))

# 5. Comparación con OLS (Mínimos Cuadrados Ordinarios)
# Ajustamos un modelo lineal estándar en datos originales para comparar coeficientes
train_df_original <- datos$data_original[train_idx, ]
ols_model <- lm(medv ~ ., data = train_df_original)

# 6. Salida en Consola
cat("--- Análisis de Regresión Ridge ---\n")
cat("Lambda Óptimo:", best_lambda, "\n")
cat("RMSE (Conjunto de Prueba):", rmse_ridge, "\n")
cat("MAE (Conjunto de Prueba): ", mae_ridge, "\n\n")

cat("--- Comparación de Coeficientes (Variables con alta colinealidad) ---\n")
cat("Nota: Coeficientes Ridge están escalados; OLS en escala original.\n")
vars_to_compare <- c("nox", "tax", "rad")

comp_table <- data.frame(
  OLS_Coef = coef(ols_model)[vars_to_compare],
  Ridge_Coef_Escalado = as.numeric(coef(cv_ridge, s = best_lambda)[vars_to_compare,])
)
print(comp_table)