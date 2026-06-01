# ============================================================
# Test 09 - Dinamica y acoplamiento ADTR
# ============================================================

cat("\n============================================================\n")
cat("Test 09 - Dinamica y acoplamiento ADTR\n")
cat("============================================================\n")

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  source("R/01_validacion_adtr.R")
  source("R/09_dinamica_acoplamiento_adtr.R")
}

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)

trayectoria_demo <- data.frame(
  iter = 1:5,
  loss = c(0.120, 0.080, 0.050, 0.030, 0.020),
  grad_norm = c(0.90, 0.65, 0.40, 0.22, 0.10),
  eta = c(0.10, 0.10, 0.08, 0.08, 0.05)
)

resultado <- analizar_dinamica_acoplamiento_adtr(trayectoria_demo)

cat("\nVariaciones dinamicas:\n")
print(resultado$variaciones)

cat("\nCoordinacion direccional:\n")
print(resultado$coordinacion)

cat("\nCorrelaciones dinamicas:\n")
print(resultado$correlaciones)

cat("\nEstabilizacion conjunta:\n")
print(resultado$estabilizacion)

cat("\nRelaciones Nivel 4:\n")
print(resultado$relaciones_nivel4)

cat("\nNiveles actualizados:\n")
print(resultado$niveles_actualizados)

cat("\nTabla de acoplamiento:\n")
print(resultado$tabla_acoplamiento)

cat("\nRegla funcional:\n")
print(resultado$regla_funcional)

cat("\nConsistencia:\n")
print(resultado$consistencia)

cat("\nDependencia exploratoria:\n")
print(resultado$dependencia)

cat("\nEvidencia Nivel 5:\n")
print(resultado$evidencia_nivel5)

cat("\nEscala actualizada:\n")
print(resultado$escala_actualizada)

cat("\nDecision acoplamiento:\n")
print(resultado$decision_acoplamiento)

# Pruebas logicas minimas
prueba_variaciones <- nrow(resultado$variaciones) == 4
prueba_coord_loss_grad <- resultado$coordinacion$proporcion_coincidencia[
  resultado$coordinacion$punto_1 == "loss" & resultado$coordinacion$punto_2 == "grad_norm"
] == 1
prueba_estabilizacion <- resultado$estabilizacion$patron_estabilizacion == "estabilizacion_progresiva_con_reduccion_de_intensidad"
prueba_nivel4 <- any(resultado$niveles_actualizados$tipo == "relacion dinamica" & resultado$niveles_actualizados$evidencia_actual)
prueba_acoplamiento_fuerte <- resultado$evidencia_nivel5$tipo_acoplamiento == "acoplamiento_interpretado_fuerte"
prueba_nivel5 <- isTRUE(resultado$evidencia_nivel5$evidencia_nivel5)
prueba_decision <- resultado$decision_acoplamiento$decision == "posible_ecosistema_dinamico_aprendido_exploratorio"
prueba_regla_alta <- resultado$regla_funcional$tipo_evidencia == "evidencia_funcional_alta"
prueba_r2_alto <- resultado$dependencia$r2 > 0.90
prueba_consistencia_alta <- resultado$consistencia$tipo_consistencia == "consistencia_alta"

error_trayectoria_corta <- tryCatch({
  construir_tabla_acoplamiento_adtr(trayectoria_demo[1:3, ])
  FALSE
}, error = function(e) TRUE)

error_trayectoria_incompleta <- tryCatch({
  construir_variaciones_dinamicas_adtr(data.frame(iter = 1:4, loss = 1:4))
  FALSE
}, error = function(e) TRUE)

resumen_test <- data.frame(
  prueba = c(
    "variaciones_4_filas",
    "coordinacion_loss_grad_1",
    "estabilizacion_progresiva",
    "nivel4_demostrado",
    "acoplamiento_fuerte",
    "nivel5_exploratorio",
    "decision_ecosistema_exploratorio",
    "regla_funcional_alta",
    "r2_mayor_090",
    "consistencia_alta",
    "detecta_trayectoria_corta",
    "detecta_trayectoria_incompleta"
  ),
  resultado = c(
    prueba_variaciones,
    prueba_coord_loss_grad,
    prueba_estabilizacion,
    prueba_nivel4,
    prueba_acoplamiento_fuerte,
    prueba_nivel5,
    prueba_decision,
    prueba_regla_alta,
    prueba_r2_alto,
    prueba_consistencia_alta,
    error_trayectoria_corta,
    error_trayectoria_incompleta
  ),
  stringsAsFactors = FALSE
)

cat("\nResumen del test 09:\n")
print(resumen_test)

if (!all(resumen_test$resultado)) {
  stop("Alguna prueba del test 09 fallo.")
}

write.csv(resultado$variaciones, "outputs/tables/test_09_variaciones_dinamicas_adtr.csv", row.names = FALSE)
write.csv(resultado$coordinacion, "outputs/tables/test_09_coordinacion_direccional_adtr.csv", row.names = FALSE)
write.csv(resultado$correlaciones, "outputs/tables/test_09_correlaciones_dinamicas_adtr.csv", row.names = FALSE)
write.csv(resultado$estabilizacion, "outputs/tables/test_09_estabilizacion_conjunta_adtr.csv", row.names = FALSE)
write.csv(resultado$relaciones_nivel4, "outputs/tables/test_09_relaciones_nivel4_adtr.csv", row.names = FALSE)
write.csv(resultado$tabla_acoplamiento, "outputs/tables/test_09_tabla_acoplamiento_adtr.csv", row.names = FALSE)
write.csv(resultado$regla_funcional, "outputs/tables/test_09_regla_funcional_adtr.csv", row.names = FALSE)
write.csv(resultado$consistencia, "outputs/tables/test_09_consistencia_adtr.csv", row.names = FALSE)
write.csv(resultado$dependencia, "outputs/tables/test_09_dependencia_exploratoria_adtr.csv", row.names = FALSE)
write.csv(resultado$evidencia_nivel5, "outputs/tables/test_09_evidencia_nivel5_adtr.csv", row.names = FALSE)
write.csv(resultado$decision_acoplamiento, "outputs/tables/test_09_decision_acoplamiento_adtr.csv", row.names = FALSE)
write.csv(resumen_test, "outputs/tables/test_09_dinamica_acoplamiento_adtr.csv", row.names = FALSE)

saveRDS(resultado, "outputs/results/test_09_dinamica_acoplamiento_adtr.rds")

cat("\nTest 09 finalizado correctamente.\n")
cat("Archivos generados en outputs/tables y outputs/results.\n")
cat("============================================================\n")
