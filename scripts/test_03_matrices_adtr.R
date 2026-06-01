# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: test_03_matrices_adtr.R
# Propósito: Probar funciones de R/03_matrices_adtr.R
# ============================================================

cat("\n============================================================\n")
cat("Test 03 - Matrices ADTR\n")
cat("============================================================\n")

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)

source("R/01_validacion_adtr.R")
source("R/03_matrices_adtr.R")

ajuste_demo <- list(
  unidad = list(
    variable_objetivo = "y",
    entradas = c("x1", "x2"),
    modelo = "mlp_simple"
  ),
  preparado = data.frame(
    x1 = c(0.10, 0.20, 0.35),
    x2 = c(0.40, 0.30, 0.15),
    y = c(0.50, 0.60, 0.70)
  ),
  params_finales = list(
    pesos = c(0.25, -0.10),
    sesgo = 0.05
  ),
  prediccion = c(0.48, 0.62, 0.69),
  trayectoria = data.frame(
    iter = 1:5,
    loss = c(0.120, 0.080, 0.050, 0.030, 0.020),
    grad_norm = c(0.90, 0.65, 0.40, 0.22, 0.10),
    eta = c(0.10, 0.10, 0.08, 0.08, 0.05)
  ),
  metricas = list(
    loss_inicial = 0.120,
    loss_final = 0.020,
    reduccion_rel = 0.8333
  ),
  creado = Sys.time()
)

matriz_generica <- construir_matriz_adtr(
  punto = "p",
  sistema = c("S_obs", "S_apr"),
  representacion = c("dato", "representacion"),
  rol = c("origen", "aprendizaje"),
  funcion = c("observar", "aprender"),
  naturaleza = c("dato", "salida"),
  fuente_programatica = c("obj$dato", "obj$salida")
)

matriz_y <- construir_matriz_variable_aprendida_adtr(ajuste_demo, "ajuste_demo")
matriz_trayectoria <- construir_matriz_trayectoria_adtr(ajuste_demo$trayectoria, "ajuste_demo")
matriz_integrada <- construir_matrices_desde_objeto_adtr(ajuste_demo, "ajuste_demo")

cat("\nMatriz variable aprendida:\n")
print(matriz_y)

cat("\nMatriz trayectoria:\n")
print(matriz_trayectoria)

cat("\nMatriz integrada desde objeto:\n")
print(matriz_integrada)

# Pruebas lógicas mínimas
prueba_generica <- nrow(matriz_generica) == 2
prueba_variable <- nrow(matriz_y) == 5 && any(matriz_y$sistema == "S_obs")
prueba_trayectoria <- nrow(matriz_trayectoria) == 9 && all(c("loss", "grad_norm", "eta") %in% unique(matriz_trayectoria$punto))
prueba_integrada <- nrow(matriz_integrada) == 14 && length(unique(matriz_integrada$punto)) == 4
prueba_trazabilidad <- all(!is.na(matriz_integrada$fuente_programatica) & matriz_integrada$fuente_programatica != "")

error_longitudes <- tryCatch({
  construir_matriz_adtr(
    punto = "p",
    sistema = c("S1", "S2"),
    representacion = c("r1", "r2", "r3"),
    rol = "rol",
    funcion = "funcion",
    naturaleza = "naturaleza",
    fuente_programatica = "fuente"
  )
  FALSE
}, error = function(e) TRUE)

error_objeto_incompleto <- tryCatch({
  construir_matriz_variable_aprendida_adtr(list(unidad = list(variable_objetivo = "y")))
  FALSE
}, error = function(e) TRUE)

resumen_test <- data.frame(
  prueba = c(
    "construye_matriz_generica",
    "construye_variable_aprendida",
    "construye_trayectoria",
    "construye_integrada_desde_objeto",
    "mantiene_trazabilidad",
    "detecta_longitudes_incompatibles",
    "detecta_objeto_incompleto"
  ),
  resultado = c(
    prueba_generica,
    prueba_variable,
    prueba_trayectoria,
    prueba_integrada,
    prueba_trazabilidad,
    error_longitudes,
    error_objeto_incompleto
  ),
  stringsAsFactors = FALSE
)

cat("\nResumen del test 03:\n")
print(resumen_test)

if (!all(resumen_test$resultado)) {
  stop("Alguna prueba del test 03 falló.")
}

write.csv(matriz_y, "outputs/tables/test_03_matriz_variable_aprendida_adtr.csv", row.names = FALSE)
write.csv(matriz_trayectoria, "outputs/tables/test_03_matriz_trayectoria_adtr.csv", row.names = FALSE)
write.csv(matriz_integrada, "outputs/tables/test_03_matriz_integrada_adtr.csv", row.names = FALSE)
write.csv(resumen_test, "outputs/tables/test_03_matrices_adtr.csv", row.names = FALSE)

saveRDS(
  list(
    matriz_generica = matriz_generica,
    matriz_y = matriz_y,
    matriz_trayectoria = matriz_trayectoria,
    matriz_integrada = matriz_integrada,
    resumen_test = resumen_test
  ),
  "outputs/results/test_03_matrices_adtr.rds"
)

cat("\nTest 03 finalizado correctamente.\n")
cat("Archivos generados en outputs/tables y outputs/results.\n")
cat("============================================================\n")
