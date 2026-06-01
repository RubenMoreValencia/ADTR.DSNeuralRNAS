# ============================================================
# Test funcional: R/01_validacion_adtr.R
# ============================================================

source("R/01_validacion_adtr.R")

cat("\n============================================================\n")
cat("Test 01 - Validaciones ADTR\n")
cat("============================================================\n")

matriz_y <- data.frame(
  punto = c("y", "y"),
  sistema = c("S_obs", "S_apr"),
  representacion = c("preparado$y", "prediccion"),
  rol = c("variable observada", "salida estimada"),
  funcion = c("representar valor factual", "aproximar variable observada"),
  naturaleza = c("dato factual observado", "representacion aprendida"),
  fuente_programatica = c("ajuste_demo$preparado$y", "ajuste_demo$prediccion"),
  stringsAsFactors = FALSE
)

trayectoria_demo <- data.frame(
  iter = 1:5,
  loss = c(0.120, 0.080, 0.050, 0.030, 0.020),
  grad_norm = c(0.90, 0.65, 0.40, 0.22, 0.10),
  eta = c(0.10, 0.10, 0.08, 0.08, 0.05)
)

nodos <- data.frame(
  id = c("y", "loss"),
  tipo_punto = c("punto_factual_observado", "senal_interna_transformacional"),
  stringsAsFactors = FALSE
)

aristas <- data.frame(
  from = "loss",
  to = "y",
  tipo_relacion = "relacion_sistemico_funcional",
  justificacion = "Comparten sistemas y función de ajuste.",
  stringsAsFactors = FALSE
)

objeto_aprendizaje <- list(
  unidad = list(variable_objetivo = "y"),
  preparado = data.frame(x1 = c(0.1, 0.2), y = c(0.5, 0.6)),
  prediccion = c(0.48, 0.62),
  trayectoria = trayectoria_demo,
  metricas = list(loss_inicial = 0.12, loss_final = 0.02)
)

# Tests esperados correctos
validar_matriz_adtr(matriz_y)
validar_coleccion_adtr(matriz_y, minimo_puntos = 1)
validar_trayectoria_adtr(trayectoria_demo)
validar_nodos_adtr(nodos)
validar_aristas_adtr(aristas)
validar_red_adtr(nodos, aristas)
validar_indice_unitario_adtr(0.75, "indice_demo")
validar_objeto_aprendizaje_adtr(objeto_aprendizaje)

cat("\nValidaciones correctas ejecutadas sin errores.\n")

# Tests de error controlado
error_matriz <- tryCatch({
  validar_matriz_adtr(data.frame(punto = "y"))
  FALSE
}, error = function(e) TRUE)

error_indice <- tryCatch({
  validar_indice_unitario_adtr(1.25, "indice_fuera_de_rango")
  FALSE
}, error = function(e) TRUE)

error_red <- tryCatch({
  aristas_malas <- aristas
  aristas_malas$to <- "no_existe"
  validar_red_adtr(nodos, aristas_malas)
  FALSE
}, error = function(e) TRUE)

resumen_test <- data.frame(
  prueba = c(
    "validaciones_correctas",
    "detecta_matriz_incompleta",
    "detecta_indice_fuera_rango",
    "detecta_arista_sin_nodo"
  ),
  resultado = c(TRUE, error_matriz, error_indice, error_red),
  stringsAsFactors = FALSE
)

print(resumen_test)

if (!all(resumen_test$resultado)) {
  stop("Alguna prueba de validación falló.")
}

write.csv(
  resumen_test,
  file = "outputs/tables/test_01_validacion_adtr.csv",
  row.names = FALSE
)

cat("\nTest 01 finalizado correctamente.\n")
cat("Archivo generado: outputs/tables/test_01_validacion_adtr.csv\n")
cat("============================================================\n")
