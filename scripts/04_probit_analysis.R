# Script: 05_probit_analysis.R
# Objetivo: Implementación del modelo PROBIT y su comparación técnica
#           frente al modelo LOGIT (Criterios de Ajuste AIC/BIC).

library(caret)
library(pROC)
library(tidyverse)

# Cargar funciones de preparación de datos
source("data/data.R")

# 1. Carga y División de Datos
# get_data_probit() retorna el mismo dataset limpio (Pima) que Logit
df_class <- get_data_probit()

set.seed(123)
# Usamos la misma semilla para que la partición sea IDÉNTICA al análisis Logit
# y la comparación sea justa.
train_idx <- createDataPartition(df_class$diabetes, p = 0.7, list = FALSE)

train_data <- df_class[train_idx, ]
test_data  <- df_class[-train_idx, ]

# 2. Entrenamiento de Modelos (Probit vs Logit)
# Modelo A: PROBIT (Enlace: Inversa de la Normal Estándar)
probit_model <- glm(diabetes ~ ., data = train_data, family = binomial(link = "probit"))

# Modelo B: LOGIT (Para comparación directa en este script)
logit_ref_model <- glm(diabetes ~ ., data = train_data, family = binomial(link = "logit"))

# 3. Comparación de Ajuste (AIC)
# El Criterio de Información de Akaike (AIC) penaliza la complejidad.
# Menor AIC indica mejor modelo.
aic_probit <- AIC(probit_model)
aic_logit  <- AIC(logit_ref_model)

# 4. Predicciones y Curvas ROC
# Predicciones Probit (Probabilidades)
probs_probit <- predict(probit_model, newdata = test_data, type = "response")
probs_logit  <- predict(logit_ref_model, newdata = test_data, type = "response")

# Objetos ROC
roc_probit <- roc(test_data$diabetes, probs_probit)
roc_logit  <- roc(test_data$diabetes, probs_logit)

# 5. Gráfico Comparativo Final (Logit vs Probit)
png("plots/04_comparacion_probit_logit.png", width=600, height=600)

# Dibujar curva Probit (Rojo)
plot(roc_probit, col = "red", main = "Comparación ROC: Probit vs Logit")

# Agregar curva Logit (Azul punteada)
lines(roc_logit, col = "blue", lty = 2)

legend("bottomright", 
       legend = c(paste("Probit (AUC =", round(auc(roc_probit), 3), ")"), 
                  paste("Logit (AUC =", round(auc(roc_logit), 3), ")")),
       col = c("red", "blue"), lty = c(1, 2), lwd = 2)

dev.off()

# 6. Salida en Consola (Tabla de Comparación)
cat("--- Análisis Comparativo: Probit vs Logit ---\n")
cat("Criterio de Información de Akaike (AIC) - Menor es mejor:\n")
cat("AIC Probit:", round(aic_probit, 2), "\n")
cat("AIC Logit: ", round(aic_logit, 2), "\n")

if (abs(aic_probit - aic_logit) < 2) {
  cat(">> Conclusión: La diferencia es insignificante (< 2). Ambos modelos ajustan igual de bien.\n")
} else if (aic_probit < aic_logit) {
  cat(">> Conclusión: El modelo PROBIT tiene un mejor ajuste estadístico.\n")
} else {
  cat(">> Conclusión: El modelo LOGIT tiene un mejor ajuste estadístico.\n")
}

cat("\nÁrea Bajo la Curva (AUC) en Test Set:\n")
cat("AUC Probit:", round(auc(roc_probit), 4), "\n")
cat("AUC Logit: ", round(auc(roc_logit), 4), "\n")