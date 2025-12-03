# Script: 04_logit_analysis.R
# Objetivo: Modelo de Clasificación Binaria (Logit)
#           para predecir diabetes (pos/neg) e interpretar Odds Ratios.

library(caret)
library(pROC)
library(tidyverse)

# Cargar funciones de preparación de datos (Ruta corregida)
source("data/data.R")

# 1. Carga y División de Datos
# get_data_logit() retorna el dataframe PimaIndiansDiabetes2 limpio
df_class <- get_data_logit()

set.seed(123)
# Creamos partición sobre la variable objetivo 'diabetes'
train_idx <- createDataPartition(df_class$diabetes, p = 0.7, list = FALSE)

train_data <- df_class[train_idx, ]
test_data  <- df_class[-train_idx, ]

# 2. Entrenamiento del Modelo Logit
# family = binomial(link = "logit") define la regresión logística
logit_model <- glm(diabetes ~ ., data = train_data, family = binomial(link = "logit"))

# 3. Predicciones
# type = "response" devuelve probabilidades entre 0 y 1
probs_logit <- predict(logit_model, newdata = test_data, type = "response")

# Convertir probabilidades a clases (Umbral estándar de 0.5)
# Si la probabilidad > 0.5, predecimos "pos", si no "neg"
preds_class <- ifelse(probs_logit > 0.5, "pos", "neg")
preds_class <- factor(preds_class, levels = c("neg", "pos"))

# 4. Gráficos de Diagnóstico
# Curva ROC: Evalúa la capacidad de discriminación del modelo
png("plots/03_logit_curva_roc.png")
roc_obj <- roc(test_data$diabetes, probs_logit)
plot(roc_obj, main = paste("Curva ROC Logit - AUC:", round(auc(roc_obj), 4)),
     col = "blue", lwd = 2)
dev.off()

# 5. Métricas de Desempeño
# Matriz de Confusión (Accuracy, Sensibilidad, Especificidad)
conf_matrix <- confusionMatrix(preds_class, test_data$diabetes, positive = "pos")

# 6. Interpretación de Coeficientes (Odds Ratios)
# Los coeficientes beta no son interpretables directamente, se usa la exponencial
odds_ratios <- exp(coef(logit_model))

# 7. Salida en Consola
cat("--- Análisis Logit (Regresión Logística) ---\n")
cat("AUC (Área bajo la curva):", auc(roc_obj), "\n")
cat("Accuracy (Exactitud):    ", conf_matrix$overall["Accuracy"], "\n")
cat("Sensibilidad (Recall):   ", conf_matrix$byClass["Sensitivity"], "\n")
cat("Especificidad:           ", conf_matrix$byClass["Specificity"], "\n\n")

cat("--- Interpretación de Odds Ratios (Factores de Riesgo) ---\n")
cat("Valores > 1 indican aumento de riesgo; < 1 indican disminución.\n")
# Mostramos los OR ordenados para ver qué influye más
print(sort(odds_ratios, decreasing = TRUE))