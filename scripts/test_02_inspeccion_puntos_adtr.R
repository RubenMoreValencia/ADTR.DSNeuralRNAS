# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: scripts/test_02_inspeccion_puntos_adtr.R
# Propósito:
#   Test funcional del archivo R/02_inspeccion_puntos_adtr.R
# ============================================================

cat("\n============================================================\n")
cat("Test 02 - Inspección e identificación de puntos ADTR\n")
cat("============================================================\n")

source("R/01_validacion_adtr.R")
source("R/02_inspeccion_puntos_adtr.R")

dir.create("outputs", showWarnings = FALSE)
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

validar_objeto_aprendizaje_adtr(ajuste_demo)

inspeccion <- inspeccionar_objeto_adtr(
  objeto = ajuste_demo,
  nombre_objeto = "ajuste_demo",
  validar_aprendizaje = TRUE
)

puntos <- identificar_puntos_adtr(
  objeto = ajuste_demo,
  nombre_objeto = "ajuste_demo",
  incluir_columnas_internas = TRUE
)

puntos_trayectoria <- identificar_puntos_trayectoria_adtr(
  trayectoria = ajuste_demo$trayectoria,
  nombre_objeto = "ajuste_demo"
)

puntos_trazables <- seleccionar_puntos_trazables_adtr(
  tabla_puntos = puntos,
  excluir_pendientes = TRUE,
  permitir_revision = TRUE
)

resumen <- resumir_puntos_identificados_adtr(puntos)

cat("\nResumen de componentes inspeccionados:\n")
print(inspeccion$resumen_componentes)

cat("\nPuntos identificados:\n")
print(puntos)

cat("\nPuntos de trayectoria:\n")
print(puntos_trayectoria)

cat("\nPuntos trazables seleccionados:\n")
print(puntos_trazables)

cat("\nResumen de puntos identificados:\n")
print(resumen)

# Pruebas lógicas mínimas
prueba_inspeccion <- all(c("unidad", "preparado", "trayectoria") %in% inspeccion$nombres)
prueba_detecta_loss <- any(puntos$posible_punto_adtr == "perdida")
prueba_detecta_grad <- any(puntos$posible_punto_adtr == "gradiente")
prueba_detecta_eta <- any(puntos$posible_punto_adtr == "tasa_de_aprendizaje")
prueba_trazables <- nrow(puntos_trazables) > 0

error_objeto_sin_nombres <- tryCatch({
  identificar_puntos_adtr(c(1, 2, 3))
  FALSE
}, error = function(e) TRUE)

resumen_test <- data.frame(
  prueba = c(
    "inspeccion_componentes_base",
    "detecta_loss",
    "detecta_grad_norm",
    "detecta_eta",
    "selecciona_puntos_trazables",
    "detecta_objeto_sin_nombres"
  ),
  resultado = c(
    prueba_inspeccion,
    prueba_detecta_loss,
    prueba_detecta_grad,
    prueba_detecta_eta,
    prueba_trazables,
    error_objeto_sin_nombres
  ),
  stringsAsFactors = FALSE
)

cat("\nResumen del test 02:\n")
print(resumen_test)

if (!all(resumen_test$resultado)) {
  stop("Alguna prueba del test 02 falló.")
}

write.csv(
  inspeccion$resumen_componentes,
  file = "outputs/tables/test_02_resumen_componentes_adtr.csv",
  row.names = FALSE
)

write.csv(
  puntos,
  file = "outputs/tables/test_02_puntos_identificados_adtr.csv",
  row.names = FALSE
)

write.csv(
  puntos_trayectoria,
  file = "outputs/tables/test_02_puntos_trayectoria_adtr.csv",
  row.names = FALSE
)

write.csv(
  puntos_trazables,
  file = "outputs/tables/test_02_puntos_trazables_adtr.csv",
  row.names = FALSE
)

write.csv(
  resumen,
  file = "outputs/tables/test_02_resumen_puntos_adtr.csv",
  row.names = FALSE
)

write.csv(
  resumen_test,
  file = "outputs/tables/test_02_inspeccion_puntos_adtr.csv",
  row.names = FALSE
)

saveRDS(
  list(
    inspeccion = inspeccion,
    puntos = puntos,
    puntos_trayectoria = puntos_trayectoria,
    puntos_trazables = puntos_trazables,
    resumen = resumen,
    resumen_test = resumen_test
  ),
  file = "outputs/results/test_02_inspeccion_puntos_adtr.rds"
)

cat("\nTest 02 finalizado correctamente.\n")
cat("Archivos generados en outputs/tables y outputs/results.\n")
cat("============================================================\n")
