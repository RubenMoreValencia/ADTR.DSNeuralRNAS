# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: test_08_ecosistema_adtr.R
# Propósito: Probar funciones de R/08_ecosistema_adtr.R
# ============================================================

cat("\n============================================================\n")
cat("Test 08 - Madurez ecosistémica ADTR\n")
cat("============================================================\n")

# Carga en modo paquete cuando sea posible. Evita source() de /R si devtools está disponible.
if (!exists("analizar_madurez_ecosistemica_adtr", mode = "function")) {
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    source("R/01_validacion_adtr.R")
    source("R/03_matrices_adtr.R")
    source("R/06_conjuntos_adtr.R")
    source("R/07_redes_adtr.R")
    source("R/08_ecosistema_adtr.R")
  }
}

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)

ajuste_demo <- list(
  unidad = list(
    variable_objetivo = "y",
    entradas = c("x1", "x2"),
    modelo = "mlp_simple"
  ),
  preparado = data.frame(
    x1 = c(0.10, 0.20, 0.30),
    x2 = c(0.40, 0.50, 0.60),
    y = c(0.50, 0.60, 0.70)
  ),
  params_finales = list(w = c(0.25, 0.35), b = 0.10),
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
    reduccion_rel = 0.8333333
  )
)

coleccion <- construir_matrices_desde_objeto_adtr(ajuste_demo, "ajuste_demo")
resultado_conjunto <- analizar_conjunto_adtr(coleccion)
resultado_red <- analizar_red_adtr(
  coleccion_adtr = resultado_conjunto$coleccion_adtr,
  relaciones_puntos = resultado_conjunto$relaciones_puntos,
  metricas_puntos = resultado_conjunto$metricas_puntos,
  puntos_criticos = resultado_conjunto$puntos_criticos,
  graficar = FALSE
)

resultado_ecosistema <- analizar_madurez_ecosistemica_adtr(
  nodos = resultado_red$nodos,
  aristas = resultado_red$aristas,
  coleccion_adtr = resultado_conjunto$coleccion_adtr
)

cat("\nIndicadores de madurez:\n")
print(resultado_ecosistema$indicadores)

cat("\nÍndice de madurez:\n")
print(resultado_ecosistema$indice_madurez)

cat("\nNiveles relacionales:\n")
print(resultado_ecosistema$niveles_relacion)

cat("\nInterpretación:\n")
print(resultado_ecosistema$interpretacion)

cat("\nDecisión funcional:\n")
print(resultado_ecosistema$decision_funcional)

prueba_indicadores_14 <- nrow(resultado_ecosistema$indicadores) == 14
prueba_cumple_14 <- all(resultado_ecosistema$indicadores$cumple)
prueba_indice_unitario <- resultado_ecosistema$indice_madurez$indice_madurez_ecosistemica >= 0 &&
  resultado_ecosistema$indice_madurez$indice_madurez_ecosistemica <= 1
prueba_indice_1 <- abs(resultado_ecosistema$indice_madurez$indice_madurez_ecosistemica - 1) < 1e-10
prueba_nivel_alta <- resultado_ecosistema$indice_madurez$nivel_madurez == "base_ecosistemica_alta"
prueba_nivel4_falso <- !resultado_ecosistema$niveles_relacion$evidencia_actual[
  resultado_ecosistema$niveles_relacion$tipo == "relacion dinamica"
]
prueba_nivel5_falso <- !resultado_ecosistema$niveles_relacion$evidencia_actual[
  resultado_ecosistema$niveles_relacion$tipo == "acoplamiento interpretado"
]
prueba_decision <- resultado_ecosistema$decision_funcional$decision == "mantener_como_base_ecosistemica_preliminar"
prueba_interpretacion <- nrow(resultado_ecosistema$interpretacion) > 0

error_pesos_incompletos <- tryCatch({
  calcular_indice_madurez_ecosistemica_adtr(
    resultado_ecosistema$indicadores,
    pesos = c(multiplicidad_de_puntos = 1)
  )
  FALSE
}, error = function(e) TRUE)

error_nodos_incompletos <- tryCatch({
  evaluar_indicadores_madurez_adtr(
    nodos = data.frame(id = "y"),
    aristas = resultado_red$aristas,
    coleccion_adtr = resultado_conjunto$coleccion_adtr
  )
  FALSE
}, error = function(e) TRUE)

resumen_test <- data.frame(
  prueba = c(
    "indicadores_14",
    "cumple_14_indicadores",
    "indice_en_rango_unitario",
    "indice_normalizado_1",
    "nivel_base_ecosistemica_alta",
    "nivel4_no_demostrado",
    "nivel5_no_demostrado",
    "decision_base_preliminar",
    "genera_interpretacion",
    "detecta_pesos_incompletos",
    "detecta_nodos_incompletos"
  ),
  resultado = c(
    prueba_indicadores_14,
    prueba_cumple_14,
    prueba_indice_unitario,
    prueba_indice_1,
    prueba_nivel_alta,
    prueba_nivel4_falso,
    prueba_nivel5_falso,
    prueba_decision,
    prueba_interpretacion,
    error_pesos_incompletos,
    error_nodos_incompletos
  ),
  stringsAsFactors = FALSE
)

cat("\nResumen del test 08:\n")
print(resumen_test)

if (!all(resumen_test$resultado)) {
  stop("Alguna prueba del test 08 falló.")
}

write.csv(resultado_ecosistema$indicadores, "outputs/tables/test_08_indicadores_madurez_adtr.csv", row.names = FALSE)
write.csv(resultado_ecosistema$indice_madurez, "outputs/tables/test_08_indice_madurez_adtr.csv", row.names = FALSE)
write.csv(resultado_ecosistema$niveles_relacion, "outputs/tables/test_08_niveles_relacion_adtr.csv", row.names = FALSE)
write.csv(resultado_ecosistema$interpretacion, "outputs/tables/test_08_interpretacion_madurez_adtr.csv", row.names = FALSE)
write.csv(resultado_ecosistema$decision_funcional, "outputs/tables/test_08_decision_funcional_ecosistema_adtr.csv", row.names = FALSE)
write.csv(resumen_test, "outputs/tables/test_08_ecosistema_adtr.csv", row.names = FALSE)

saveRDS(
  list(
    resultado_conjunto = resultado_conjunto,
    resultado_red = resultado_red,
    resultado_ecosistema = resultado_ecosistema,
    resumen_test = resumen_test
  ),
  "outputs/results/test_08_ecosistema_adtr.rds"
)

cat("\nTest 08 finalizado correctamente.\n")
cat("Archivos generados en outputs/tables y outputs/results.\n")
cat("============================================================\n")
