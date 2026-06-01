# ============================================================
# Test: 07_redes_adtr.R
# ============================================================

cat("\n============================================================\n")
cat("Test 07 - Redes transformacionales ADTR\n")
cat("============================================================\n")

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  source("R/01_validacion_adtr.R")
  source("R/02_inspeccion_puntos_adtr.R")
  source("R/03_matrices_adtr.R")
  source("R/04_transformaciones_adtr.R")
  source("R/05_metricas_adtr.R")
  source("R/06_conjuntos_adtr.R")
  source("R/07_redes_adtr.R")
}

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

ajuste_demo <- list(
  unidad = list(variable_objetivo = "y", entradas = c("x1", "x2"), modelo = "mlp_simple"),
  preparado = data.frame(x1 = c(0.1, 0.2, 0.3), x2 = c(0.5, 0.6, 0.7), y = c(0.5, 0.6, 0.7)),
  params_finales = list(w = c(0.2, 0.4), b = 0.1),
  prediccion = c(0.48, 0.62, 0.69),
  trayectoria = data.frame(
    iter = 1:5,
    loss = c(0.120, 0.080, 0.050, 0.030, 0.020),
    grad_norm = c(0.90, 0.65, 0.40, 0.22, 0.10),
    eta = c(0.10, 0.10, 0.08, 0.08, 0.05)
  ),
  metricas = list(loss_inicial = 0.120, loss_final = 0.020, reduccion_rel = 0.8333),
  creado = Sys.time()
)

coleccion <- construir_matrices_desde_objeto_adtr(ajuste_demo, "ajuste_demo")
resultado_conjunto <- analizar_conjunto_adtr(coleccion)

resultado_red <- analizar_red_adtr(
  coleccion_adtr = resultado_conjunto$coleccion_adtr,
  relaciones_puntos = resultado_conjunto$relaciones_puntos,
  metricas_puntos = resultado_conjunto$metricas_puntos,
  puntos_criticos = resultado_conjunto$puntos_criticos,
  umbral_centralidad = 0.80,
  graficar = TRUE,
  archivo_figura = "outputs/figures/test_07_red_transformacional_adtr.png"
)

cat("\nNodos:\n")
print(resultado_red$nodos)

cat("\nAristas:\n")
print(resultado_red$aristas)

cat("\nMétricas de red:\n")
print(resultado_red$metricas_red)

cat("\nMatriz de adyacencia:\n")
print(resultado_red$matriz_adyacencia)

cat("\nResumen de relaciones:\n")
print(resultado_red$resumen_relaciones)

cat("\nNodos centrales:\n")
print(resultado_red$nodos_centrales)

cat("\nInterpretación de red:\n")
print(resultado_red$interpretacion)

prueba_nodos_4 <- nrow(resultado_red$nodos) == 4
prueba_aristas_12 <- nrow(resultado_red$aristas) == 12
prueba_pesos <- sum(resultado_red$aristas$peso == 2) == 2 && sum(resultado_red$aristas$peso == 1) == 10
prueba_centrales <- all(c("y", "loss") %in% resultado_red$nodos_centrales$punto[resultado_red$nodos_centrales$nodo_central])
prueba_centralidad_y_loss <- all(resultado_red$nodos_centrales$centralidad_peso[resultado_red$nodos_centrales$punto %in% c("y", "loss")] == 1)
prueba_matriz <- nrow(resultado_red$matriz_adyacencia) == 4 && ncol(resultado_red$matriz_adyacencia) == 4
prueba_resumen <- sum(resultado_red$resumen_relaciones$frecuencia) == 12
prueba_interpretacion <- nrow(resultado_red$interpretacion) > 0
prueba_figura <- file.exists("outputs/figures/test_07_red_transformacional_adtr.png")

error_arista_sin_nodo <- tryCatch({
  aristas_malas <- resultado_red$aristas
  aristas_malas$to[1] <- "no_existe"
  calcular_metricas_red_adtr(resultado_red$nodos, aristas_malas)
  FALSE
}, error = function(e) TRUE)

error_umbral <- tryCatch({
  identificar_nodos_centrales_adtr(resultado_red$metricas_red, umbral_centralidad = 1.5)
  FALSE
}, error = function(e) TRUE)

resumen_test <- data.frame(
  prueba = c(
    "red_4_nodos",
    "red_12_aristas",
    "pesos_correctos",
    "detecta_y_loss_centrales",
    "centralidad_y_loss_1",
    "matriz_adyacencia_4x4",
    "resumen_relaciones_12",
    "genera_interpretacion",
    "genera_figura",
    "detecta_arista_sin_nodo",
    "detecta_umbral_fuera_rango"
  ),
  resultado = c(
    prueba_nodos_4,
    prueba_aristas_12,
    prueba_pesos,
    prueba_centrales,
    prueba_centralidad_y_loss,
    prueba_matriz,
    prueba_resumen,
    prueba_interpretacion,
    prueba_figura,
    error_arista_sin_nodo,
    error_umbral
  ),
  stringsAsFactors = FALSE
)

cat("\nResumen del test 07:\n")
print(resumen_test)

if (!all(resumen_test$resultado)) {
  stop("Alguna prueba del test 07 falló.")
}

write.csv(resultado_red$nodos, "outputs/tables/test_07_nodos_red_adtr.csv", row.names = FALSE)
write.csv(resultado_red$aristas, "outputs/tables/test_07_aristas_red_adtr.csv", row.names = FALSE)
write.csv(resultado_red$metricas_red, "outputs/tables/test_07_metricas_red_adtr.csv", row.names = FALSE)
write.csv(resultado_red$matriz_adyacencia, "outputs/tables/test_07_matriz_adyacencia_adtr.csv", row.names = TRUE)
write.csv(resultado_red$resumen_relaciones, "outputs/tables/test_07_resumen_relaciones_red_adtr.csv", row.names = FALSE)
write.csv(resultado_red$nodos_centrales, "outputs/tables/test_07_nodos_centrales_adtr.csv", row.names = FALSE)
write.csv(resultado_red$interpretacion, "outputs/tables/test_07_interpretacion_red_adtr.csv", row.names = FALSE)
write.csv(resumen_test, "outputs/tables/test_07_redes_adtr.csv", row.names = FALSE)

saveRDS(resultado_red, "outputs/results/test_07_redes_adtr.rds")

cat("\nTest 07 finalizado correctamente.\n")
cat("Archivos generados en outputs/tables, outputs/results y outputs/figures.\n")
cat("============================================================\n")
