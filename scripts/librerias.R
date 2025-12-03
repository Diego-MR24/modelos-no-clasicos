# =========================================================================
# SCRIPT DE CONFIGURACIÓN DE LIBRERÍAS
# Objetivo: Instalar y cargar automáticamente todas las dependencias.
# Ejecuta este script AL INICIO de tu sesión de trabajo.
# =========================================================================

# 1. Definir la lista de paquetes necesarios para el proyecto
required_packages <- c(
  "tidyverse",   # Manipulación de datos y visualización
  "mlbench",     # Datasets para práctica (ej. PimaIndiansDiabetes)
  "MASS",        # Datasets clásicos y métodos base (incluye Boston, rlm)
  "caret",       # ML general: partición, preprocesamiento, model training
  "glmnet",      # Modelos de regularización (Ridge, Lasso, Elastic Net)
  "robustbase",  # Métodos avanzados de regresión robusta (lmrob)
  "pROC",        # Curvas ROC y AUC
  "gridExtra"    # Arreglar/combinar gráficos
)

# 2. Función de instalación y carga segura
install_and_load <- function(packages) {
  cat("Verificando librerías del sistema...\n")
  
  for (pkg in packages) {
    # Si el paquete NO está instalado, lo instala
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat(paste("Instalando paquete faltante:", pkg, "...\n"))
      install.packages(pkg, dependencies = TRUE)
      
      # Verifica si la instalación funcionó
      if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        stop(paste("Error crítico: No se pudo instalar el paquete:", pkg))
      }
    }
    
    # Carga la librería explícitamente
    library(pkg, character.only = TRUE)
    cat(paste("Cargado:", pkg, "\n"))
  }
  
  cat("El entorno está preparado para el análisis.\n")
}

# 3. Ejecutar la función
install_and_load(required_packages)