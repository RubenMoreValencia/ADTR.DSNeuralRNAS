# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: test_04_transformaciones_adtr.R
# Propósito:
#   Probar las funciones de R/04_transformaciones_adtr.R.
# ============================================================

cat("\n============================================================\n")
cat("Test 04 - Transformaciones ADTR\n")
cat("============================================================\n")

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)

source("R/01_validacion_adtr.R")
source("R/03_matrices_adtr.R")
source("R/04_transformaciones_adtr.R")

# Objeto de aprendizaje demostrativo
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
  )
)

matriz_y <- construir_matriz_variable_aprendida_adtr(
  objeto = ajuste_demo,
  nombre_objeto = "ajuste_demo"
)

resultado_transformaciones <- analizar_transformaciones_adtr(matriz_y)

matriz_transformaciones <- resultado_transformaciones$matriz_transformaciones
resumen_transformaciones <- resultado_transformaciones$resumen_transformaciones
metricas_transformacionales <- resultado_transformaciones$metricas_transformacionales
interpretacion_transformaciones <- resultado_transformaciones$interpretacion_transformaciones

cat("\nMatriz de transformaciones K(y):\n")
print(matriz_transformaciones)

cat("\nResumen de transformaciones:\n")
print(resumen_transformaciones)

cat("\nMétricas transformacionales:\n")
print(metricas_transformacionales)

cat("\nInterpretación transformacional:\n")
print(interpretacion_transformaciones)

# Pruebas lógicas mínimas
prueba_k_filas <- nrow(matriz_transformaciones) == 20
prueba_sin_indeterminadas <- metricas_transformacionales$relacion_indeterminada == 0
prueba_equiv_parcial_total <- metricas_transformacionales$indice_equivalencia_parcial_total == 0.45
prueba_no_reducibilidad_total <- metricas_transformacionales$indice_no_reducibilidad_total == 0.30
prueba_emergencia <- metricas_transformacionales$indice_emergencia_funcional == 0.10
prueba_interpretacion <- nrow(interpretacion_transformaciones) > 0

error_varios_puntos <- tryCatch({
  matriz_integrada <- construir_matrices_desde_objeto_adtr(ajuste_demo, "ajuste_demo")
  construir_matriz_transformaciones_adtr(matriz_integrada)
  FALSE
}, error = function(e) TRUE)

error_matriz_incompleta <- tryCatch({
  construir_matriz_transformaciones_adtr(data.frame(punto = "y", sistema = "S_obs"))
  FALSE
}, error = function(e) TRUE)

resumen_test <- data.frame(
  prueba = c(
    "construye_k_20_filas",
    "sin_relaciones_indeterminadas",
    "equivalencia_parcial_total_045",
    "no_reducibilidad_total_030",
    "emergencia_funcional_010",
    "genera_interpretacion",
    "detecta_matriz_con_varios_puntos",
    "detecta_matriz_incompleta"
  ),
  resultado = c(
    prueba_k_filas,
    prueba_sin_indeterminadas,
    prueba_equiv_parcial_total,
    prueba_no_reducibilidad_total,
    prueba_emergencia,
    prueba_interpretacion,
    error_varios_puntos,
    error_matriz_incompleta
  ),
  stringsAsFactors = FALSE
)

cat("\nResumen del test 04:\n")
print(resumen_test)

if (!all(resumen_test$resultado)) {
  stop("Alguna prueba del test 04 falló.")
}

write.csv(matriz_transformaciones, "outputs/tables/test_04_matriz_transformaciones_adtr.csv", row.names = FALSE)
write.csv(resumen_transformaciones, "outputs/tables/test_04_resumen_transformaciones_adtr.csv", row.names = FALSE)
write.csv(metricas_transformacionales, "outputs/tables/test_04_metricas_transformacionales_adtr.csv", row.names = FALSE)
write.csv(interpretacion_transformaciones, "outputs/tables/test_04_interpretacion_transformaciones_adtr.csv", row.names = FALSE)
write.csv(resumen_test, "outputs/tables/test_04_transformaciones_adtr.csv", row.names = FALSE)

saveRDS(
  list(
    matriz_y = matriz_y,
    matriz_transformaciones = matriz_transformaciones,
    resumen_transformaciones = resumen_transformaciones,
    metricas_transformacionales = metricas_transformacionales,
    interpretacion_transformaciones = interpretacion_transformaciones,
    resumen_test = resumen_test
  ),
  "outputs/results/test_04_transformaciones_adtr.rds"
)

cat("\nTest 04 finalizado correctamente.\n")
cat("Archivos generados en outputs/tables y outputs/results.\n")
cat("============================================================\n")
