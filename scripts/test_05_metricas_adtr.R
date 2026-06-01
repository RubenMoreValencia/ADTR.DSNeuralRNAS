# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: test_05_metricas_adtr.R
# Propósito:
#   Probar funciones de R/05_metricas_adtr.R
# ============================================================

cat("\n============================================================\n")
cat("Test 05 - Métricas ADTR\n")
cat("============================================================\n")

source("R/01_validacion_adtr.R")
source("R/03_matrices_adtr.R")
source("R/04_transformaciones_adtr.R")
source("R/05_metricas_adtr.R")

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)

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

matriz_y <- construir_matriz_variable_aprendida_adtr(ajuste_demo, "ajuste_demo")
analisis_metricas <- analizar_metricas_integradas_adtr(matriz_y)

metricas_ref <- analisis_metricas$metricas_referenciales
metricas_trans <- analisis_metricas$metricas_transformacionales
metricas_integradas <- analisis_metricas$metricas_integradas
indice <- analisis_metricas$indice_complejidad
interpretacion <- analisis_metricas$interpretacion_integrada

cat("\nMétricas referenciales:\n")
print(metricas_ref)

cat("\nMétricas transformacionales:\n")
print(metricas_trans)

cat("\nMétricas integradas:\n")
print(metricas_integradas)

cat("\nÍndice de complejidad:\n")
print(indice)

cat("\nInterpretación integrada:\n")
print(interpretacion)

# Pruebas lógicas mínimas
prueba_ref <- metricas_ref$cobertura_referencial == 4 && metricas_ref$indice_trazabilidad == 1
prueba_trans <- metricas_trans$indice_equivalencia_parcial_total == 0.45 && metricas_trans$indice_no_reducibilidad_total == 0.30
prueba_integradas <- nrow(metricas_integradas) == 1 && "indice_emergencia_funcional" %in% names(metricas_integradas)
prueba_indice <- indice$indice_complejidad_adtr >= 0 && indice$indice_complejidad_adtr <= 1
prueba_interpretacion <- nrow(interpretacion) > 0

error_varios_puntos <- tryCatch({
  matriz_integrada <- construir_matrices_desde_objeto_adtr(ajuste_demo, "ajuste_demo")
  analizar_metricas_integradas_adtr(matriz_integrada)
  FALSE
}, error = function(e) TRUE)

error_pesos_malos <- tryCatch({
  calcular_indice_complejidad_adtr(
    metricas_integradas,
    pesos = c(cobertura = 1)
  )
  FALSE
}, error = function(e) TRUE)

resumen_test <- data.frame(
  prueba = c(
    "metricas_referenciales_correctas",
    "metricas_transformacionales_correctas",
    "integra_metricas",
    "indice_en_rango_unitario",
    "genera_interpretacion_integrada",
    "detecta_varios_puntos",
    "detecta_pesos_incompletos"
  ),
  resultado = c(
    prueba_ref,
    prueba_trans,
    prueba_integradas,
    prueba_indice,
    prueba_interpretacion,
    error_varios_puntos,
    error_pesos_malos
  ),
  stringsAsFactors = FALSE
)

cat("\nResumen del test 05:\n")
print(resumen_test)

if (!all(resumen_test$resultado)) {
  stop("Alguna prueba del test 05 falló.")
}

write.csv(metricas_ref, "outputs/tables/test_05_metricas_referenciales_adtr.csv", row.names = FALSE)
write.csv(metricas_trans, "outputs/tables/test_05_metricas_transformacionales_adtr.csv", row.names = FALSE)
write.csv(metricas_integradas, "outputs/tables/test_05_metricas_integradas_adtr.csv", row.names = FALSE)
write.csv(indice, "outputs/tables/test_05_indice_complejidad_adtr.csv", row.names = FALSE)
write.csv(interpretacion, "outputs/tables/test_05_interpretacion_integrada_adtr.csv", row.names = FALSE)
write.csv(resumen_test, "outputs/tables/test_05_metricas_adtr.csv", row.names = FALSE)

saveRDS(
  list(
    matriz_y = matriz_y,
    analisis_metricas = analisis_metricas,
    resumen_test = resumen_test
  ),
  "outputs/results/test_05_metricas_adtr.rds"
)

cat("\nTest 05 finalizado correctamente.\n")
cat("Archivos generados en outputs/tables y outputs/results.\n")
cat("============================================================\n")
