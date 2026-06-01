# ============================================================
# Test 06: conjuntos multirreferenciales ADTR
# ============================================================

cat("\n============================================================\n")
cat("Test 06 - Conjuntos multirreferenciales ADTR\n")
cat("============================================================\n")

source("R/01_validacion_adtr.R")
source("R/03_matrices_adtr.R")
source("R/06_conjuntos_adtr.R")

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)

ajuste_demo <- list(
  unidad = list(variable_objetivo = "y", entradas = c("x1", "x2"), modelo = "mlp_simple"),
  preparado = data.frame(x1 = c(0.1, 0.2, 0.3), x2 = c(0.4, 0.5, 0.6), y = c(0.5, 0.6, 0.7)),
  params_finales = list(w = c(0.2, 0.4), b = 0.1),
  prediccion = c(0.48, 0.62, 0.69),
  trayectoria = data.frame(iter = 1:5, loss = c(0.120, 0.080, 0.050, 0.030, 0.020), grad_norm = c(0.90, 0.65, 0.40, 0.22, 0.10), eta = c(0.10, 0.10, 0.08, 0.08, 0.05)),
  metricas = list(loss_inicial = 0.120, loss_final = 0.020, reduccion_rel = 0.8333),
  creado = Sys.time()
)

coleccion_adtr <- construir_matrices_desde_objeto_adtr(ajuste_demo, "ajuste_demo")

resultado <- analizar_conjunto_adtr(coleccion_adtr)

cat("\nMétricas por punto:\n")
print(resultado$metricas_puntos)

cat("\nPuntos críticos:\n")
print(resultado$puntos_criticos)

cat("\nAgrupación por sistema:\n")
print(resultado$agrupacion_sistemas)

cat("\nAgrupación por función:\n")
print(resultado$agrupacion_funciones)

cat("\nRelaciones entre puntos:\n")
print(resultado$relaciones_puntos)

cat("\nInterpretación del conjunto:\n")
print(resultado$interpretacion)

# Pruebas lógicas mínimas
prueba_14_filas <- nrow(resultado$coleccion_adtr) == 14
prueba_4_puntos <- nrow(resultado$metricas_puntos) == 4 && all(c("y", "loss", "grad_norm", "eta") %in% resultado$metricas_puntos$punto)
prueba_trazabilidad <- all(resultado$metricas_puntos$indice_trazabilidad == 1)
prueba_y_cobertura <- resultado$metricas_puntos$cobertura_referencial[resultado$metricas_puntos$punto == "y"] == 4
prueba_sapr_4_puntos <- resultado$agrupacion_sistemas$numero_puntos[resultado$agrupacion_sistemas$sistema == "S_apr"] == 4
prueba_relaciones <- nrow(resultado$relaciones_puntos) == 12
prueba_sistemico_funcional <- sum(resultado$relaciones_puntos$tipo_relacion == "relacion_sistemico_funcional") == 2
prueba_interpretacion <- nrow(resultado$interpretacion) > 0

error_un_punto <- tryCatch({
  construir_relaciones_puntos_adtr(resultado$coleccion_adtr[resultado$coleccion_adtr$punto == "y", ])
  FALSE
}, error = function(e) TRUE)

error_coleccion_incompleta <- tryCatch({
  calcular_metricas_por_punto_adtr(data.frame(punto = "y"))
  FALSE
}, error = function(e) TRUE)

resumen_test <- data.frame(
  prueba = c(
    "coleccion_14_filas",
    "detecta_4_puntos",
    "trazabilidad_completa",
    "y_cobertura_4",
    "sapr_contiene_4_puntos",
    "relaciones_12",
    "relaciones_sistemico_funcionales_2",
    "genera_interpretacion",
    "detecta_relaciones_con_un_punto",
    "detecta_coleccion_incompleta"
  ),
  resultado = c(
    prueba_14_filas,
    prueba_4_puntos,
    prueba_trazabilidad,
    prueba_y_cobertura,
    prueba_sapr_4_puntos,
    prueba_relaciones,
    prueba_sistemico_funcional,
    prueba_interpretacion,
    error_un_punto,
    error_coleccion_incompleta
  ),
  stringsAsFactors = FALSE
)

cat("\nResumen del test 06:\n")
print(resumen_test)

if (!all(resumen_test$resultado)) {
  stop("Alguna prueba del test 06 falló.")
}

write.csv(resultado$coleccion_adtr, "outputs/tables/test_06_coleccion_adtr.csv", row.names = FALSE)
write.csv(resultado$metricas_puntos, "outputs/tables/test_06_metricas_por_punto_adtr.csv", row.names = FALSE)
write.csv(resultado$puntos_criticos, "outputs/tables/test_06_puntos_criticos_adtr.csv", row.names = FALSE)
write.csv(resultado$agrupacion_sistemas, "outputs/tables/test_06_agrupacion_sistemas_adtr.csv", row.names = FALSE)
write.csv(resultado$agrupacion_funciones, "outputs/tables/test_06_agrupacion_funciones_adtr.csv", row.names = FALSE)
write.csv(resultado$relaciones_puntos, "outputs/tables/test_06_relaciones_puntos_adtr.csv", row.names = FALSE)
write.csv(resultado$interpretacion, "outputs/tables/test_06_interpretacion_conjunto_adtr.csv", row.names = FALSE)
write.csv(resumen_test, "outputs/tables/test_06_conjuntos_adtr.csv", row.names = FALSE)

saveRDS(
  list(resultado = resultado, resumen_test = resumen_test),
  "outputs/results/test_06_conjuntos_adtr.rds"
)

cat("\nTest 06 finalizado correctamente.\n")
cat("Archivos generados en outputs/tables y outputs/results.\n")
cat("============================================================\n")
