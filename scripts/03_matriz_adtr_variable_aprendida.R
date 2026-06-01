
# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: 03_matriz_adtr_variable_aprendida.R
# Autor: Rubén Alexander More Valencia
# Propósito:
#   Construir una matriz ADTR para una variable aprendida.
#
#   El script evalúa cómo una variable objetivo, por ejemplo y,
#   puede proyectarse como:
#     - dato observado,
#     - variable objetivo del aprendizaje,
#     - predicción aprendida,
#     - error de ajuste,
#     - componente evaluable del sistema aprendido.
#
#   Este script NO entrena un nuevo modelo.
#   Usa salidas existentes de un objeto de aprendizaje.
# ============================================================


# ============================================================
# 0. Configuración general
# ============================================================

cat("\n============================================================\n")
cat("ADTR.DSNeuralRNAS - Matriz ADTR para variable aprendida\n")
cat("============================================================\n")

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. Funciones base ADTR
# ============================================================

construir_matriz_adtr_minima <- function(
    punto,
    sistema,
    representacion,
    rol,
    funcion,
    naturaleza,
    fuente_programatica = NA_character_,
    observacion = NA_character_
) {

  data.frame(
    punto = punto,
    sistema = sistema,
    representacion = representacion,
    rol = rol,
    funcion = funcion,
    naturaleza = naturaleza,
    fuente_programatica = fuente_programatica,
    observacion = observacion,
    stringsAsFactors = FALSE
  )
}


calcular_metricas_adtr_minimas <- function(matriz_adtr) {

  columnas_requeridas <- c(
    "punto", "sistema", "representacion",
    "rol", "funcion", "naturaleza", "fuente_programatica"
  )

  faltantes <- setdiff(columnas_requeridas, names(matriz_adtr))

  if (length(faltantes) > 0) {
    stop(
      "La matriz ADTR no contiene las columnas requeridas: ",
      paste(faltantes, collapse = ", ")
    )
  }

  cobertura_referencial <- length(unique(matriz_adtr$sistema))
  diversidad_representaciones <- length(unique(matriz_adtr$representacion))
  diversidad_roles <- length(unique(matriz_adtr$rol))
  diversidad_funcional <- length(unique(matriz_adtr$funcion))
  diversidad_naturalezas <- length(unique(matriz_adtr$naturaleza))

  trazabilidad <- mean(
    !is.na(matriz_adtr$fuente_programatica) &
      matriz_adtr$fuente_programatica != ""
  )

  data.frame(
    punto = unique(matriz_adtr$punto)[1],
    cobertura_referencial = cobertura_referencial,
    diversidad_representaciones = diversidad_representaciones,
    diversidad_roles = diversidad_roles,
    diversidad_funcional = diversidad_funcional,
    diversidad_naturalezas = diversidad_naturalezas,
    indice_trazabilidad = trazabilidad,
    stringsAsFactors = FALSE
  )
}


evaluar_necesidad_funcional_adtr <- function(
    necesidad,
    existe_en_dsneuralrnas = NA,
    existe_en_mldsneuralrnas = NA,
    puede_derivarse = NA,
    utilidad_aplicada = NA,
    escalamiento = NA
) {

  decision <- "pendiente"
  justificacion <- ""

  if (isTRUE(existe_en_dsneuralrnas)) {

    decision <- "usar_DSNeuralRNAS"
    justificacion <- "La funcionalidad ya existe en DSNeuralRNAS. No se propone función nueva."

  } else if (isTRUE(existe_en_mldsneuralrnas)) {

    decision <- "usar_MLDSNeuralRNAS"
    justificacion <- "La funcionalidad ya existe en ML.DSNeuralRNAS. No se propone función nueva."

  } else if (isTRUE(puede_derivarse)) {

    decision <- "derivar_de_salidas_existentes"
    justificacion <- "La necesidad puede resolverse combinando salidas existentes."

  } else if (isTRUE(utilidad_aplicada) && isTRUE(escalamiento)) {

    decision <- "posible_extension_futura"
    justificacion <- "Existe posible brecha funcional con utilidad y escalamiento. Requiere validación con resultados en R."

  } else {

    decision <- "mantener_como_lectura_conceptual"
    justificacion <- "No existe evidencia suficiente para proponer funcionalidad nueva."
  }

  data.frame(
    necesidad = necesidad,
    existe_en_dsneuralrnas = existe_en_dsneuralrnas,
    existe_en_mldsneuralrnas = existe_en_mldsneuralrnas,
    puede_derivarse = puede_derivarse,
    utilidad_aplicada = utilidad_aplicada,
    escalamiento = escalamiento,
    decision = decision,
    justificacion = justificacion,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 2. Validación del objeto de aprendizaje
# ============================================================

validar_objeto_variable_aprendida <- function(
    objeto,
    nombre_objeto = "objeto"
) {

  componentes_requeridos <- c("unidad", "preparado", "prediccion", "metricas")
  faltantes <- setdiff(componentes_requeridos, names(objeto))

  if (length(faltantes) > 0) {
    stop(
      "El objeto no contiene los componentes requeridos: ",
      paste(faltantes, collapse = ", ")
    )
  }

  if (!is.data.frame(objeto$preparado)) {
    stop("El componente `preparado` debe ser un data.frame.")
  }

  if (is.null(objeto$unidad$variable_objetivo)) {
    stop("El objeto no contiene `unidad$variable_objetivo`.")
  }

  variable_objetivo <- objeto$unidad$variable_objetivo

  if (!variable_objetivo %in% names(objeto$preparado)) {
    stop(
      "La variable objetivo `", variable_objetivo,
      "` no existe en `preparado`."
    )
  }

  if (length(objeto$prediccion) != nrow(objeto$preparado)) {
    stop("La longitud de `prediccion` no coincide con el número de filas de `preparado`.")
  }

  cat("\nObjeto validado:", nombre_objeto, "\n")
  cat("Variable objetivo detectada:", variable_objetivo, "\n")
  cat("Número de observaciones:", nrow(objeto$preparado), "\n")

  invisible(TRUE)
}


# ============================================================
# 3. Construcción de matriz ADTR para variable aprendida
# ============================================================

construir_matriz_variable_aprendida_adtr <- function(
    objeto,
    nombre_objeto = "ajuste"
) {

  variable_objetivo <- objeto$unidad$variable_objetivo

  rbind(
    construir_matriz_adtr_minima(
      punto = variable_objetivo,
      sistema = "S_obs",
      representacion = paste0("preparado$", variable_objetivo),
      rol = "variable observada",
      funcion = "representar el valor factual de referencia",
      naturaleza = "dato factual observado",
      fuente_programatica = paste0(nombre_objeto, "$preparado$", variable_objetivo),
      observacion = "La variable objetivo aparece como dato observado en la base preparada."
    ),
    construir_matriz_adtr_minima(
      punto = variable_objetivo,
      sistema = "S_apr",
      representacion = paste0("unidad$variable_objetivo = ", variable_objetivo),
      rol = "objetivo de aprendizaje",
      funcion = "orientar el ajuste del modelo",
      naturaleza = "referente supervisado",
      fuente_programatica = paste0(nombre_objeto, "$unidad$variable_objetivo"),
      observacion = "La variable objetivo define el referente que el modelo intenta aprender."
    ),
    construir_matriz_adtr_minima(
      punto = variable_objetivo,
      sistema = "S_apr",
      representacion = "prediccion",
      rol = "salida estimada",
      funcion = "aproximar la variable observada",
      naturaleza = "representación aprendida",
      fuente_programatica = paste0(nombre_objeto, "$prediccion"),
      observacion = "La predicción representa la forma aprendida de la variable objetivo."
    ),
    construir_matriz_adtr_minima(
      punto = variable_objetivo,
      sistema = "S_ctrl",
      representacion = "error observado-predicho",
      rol = "señal de discrepancia",
      funcion = "evaluar necesidad de corrección",
      naturaleza = "señal diagnóstica",
      fuente_programatica = paste0(nombre_objeto, "$preparado$", variable_objetivo, " - ", nombre_objeto, "$prediccion"),
      observacion = "La diferencia observado-predicho puede orientar revisión del ajuste."
    ),
    construir_matriz_adtr_minima(
      punto = variable_objetivo,
      sistema = "S_learned",
      representacion = "comparación observado-predicho",
      rol = "componente aprendido evaluable",
      funcion = "valorar la calidad de la representación aprendida",
      naturaleza = "evidencia de aprendizaje",
      fuente_programatica = paste0(nombre_objeto, "$metricas"),
      observacion = "La variable aprendida se evalúa mediante métricas y comparación con su referente observado."
    )
  )
}


# ============================================================
# 4. Resumen observado-predicho
# ============================================================

resumir_variable_aprendida <- function(objeto) {

  variable_objetivo <- objeto$unidad$variable_objetivo

  observado <- objeto$preparado[[variable_objetivo]]
  predicho <- objeto$prediccion
  error <- observado - predicho
  error_abs <- abs(error)
  error_cuad <- error^2

  data.frame(
    variable = variable_objetivo,
    n = length(observado),
    observado_min = min(observado),
    observado_max = max(observado),
    predicho_min = min(predicho),
    predicho_max = max(predicho),
    error_medio = mean(error),
    mae = mean(error_abs),
    mse = mean(error_cuad),
    rmse = sqrt(mean(error_cuad)),
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 5. Tabla observado-predicho
# ============================================================

construir_tabla_obs_pred_adtr <- function(objeto) {

  variable_objetivo <- objeto$unidad$variable_objetivo

  observado <- objeto$preparado[[variable_objetivo]]
  predicho <- objeto$prediccion
  error <- observado - predicho

  data.frame(
    observacion = seq_along(observado),
    variable = variable_objetivo,
    observado = observado,
    predicho = predicho,
    error = error,
    error_abs = abs(error),
    error_cuad = error^2,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 6. Interpretación prudente del aprendizaje de la variable
# ============================================================

interpretar_variable_aprendida_adtr <- function(resumen) {

  interpretaciones <- character()

  if (resumen$rmse <= 0.05) {
    interpretaciones <- c(
      interpretaciones,
      "El error RMSE es bajo para el ejemplo evaluado; la predicción se aproxima adecuadamente al valor observado."
    )
  } else if (resumen$rmse <= 0.15) {
    interpretaciones <- c(
      interpretaciones,
      "El error RMSE es moderado; la variable aprendida conserva una aproximación razonable, pero puede requerir ajuste."
    )
  } else {
    interpretaciones <- c(
      interpretaciones,
      "El error RMSE es alto; la representación aprendida requiere revisión del ajuste o de la configuración."
    )
  }

  if (abs(resumen$error_medio) <= 0.05) {
    interpretaciones <- c(
      interpretaciones,
      "El error medio se mantiene cercano a cero; no se observa sesgo promedio marcado en este ejemplo."
    )
  } else if (resumen$error_medio > 0) {
    interpretaciones <- c(
      interpretaciones,
      "El error medio es positivo; el modelo tiende a subestimar la variable observada."
    )
  } else {
    interpretaciones <- c(
      interpretaciones,
      "El error medio es negativo; el modelo tiende a sobreestimar la variable observada."
    )
  }

  interpretaciones <- c(
    interpretaciones,
    "Desde ADTR, la variable aprendida conserva identidad factual mínima como dato observado, pero adquiere representación aprendida, señal diagnóstica y evidencia de aprendizaje según el sistema referencial."
  )

  data.frame(
    numero = seq_along(interpretaciones),
    interpretacion = interpretaciones,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 7. Ejemplo mínimo reproducible
# ============================================================

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


# ============================================================
# 8. Ejecución del análisis
# ============================================================

validar_objeto_variable_aprendida(
  objeto = ajuste_demo,
  nombre_objeto = "ajuste_demo"
)

matriz_variable <- construir_matriz_variable_aprendida_adtr(
  objeto = ajuste_demo,
  nombre_objeto = "ajuste_demo"
)

metricas_variable <- calcular_metricas_adtr_minimas(matriz_variable)

tabla_obs_pred <- construir_tabla_obs_pred_adtr(ajuste_demo)

resumen_variable <- resumir_variable_aprendida(ajuste_demo)

interpretacion_variable <- interpretar_variable_aprendida_adtr(resumen_variable)

decision_funcional <- evaluar_necesidad_funcional_adtr(
  necesidad = "Construir matriz ADTR para variable aprendida",
  existe_en_dsneuralrnas = FALSE,
  existe_en_mldsneuralrnas = FALSE,
  puede_derivarse = TRUE,
  utilidad_aplicada = TRUE,
  escalamiento = TRUE
)


# ============================================================
# 9. Exportación de resultados
# ============================================================

write.csv(
  matriz_variable,
  file = "outputs/tables/03_matriz_adtr_variable_aprendida.csv",
  row.names = FALSE
)

write.csv(
  metricas_variable,
  file = "outputs/tables/03_metricas_adtr_variable_aprendida.csv",
  row.names = FALSE
)

write.csv(
  tabla_obs_pred,
  file = "outputs/tables/03_tabla_observado_predicho_variable.csv",
  row.names = FALSE
)

write.csv(
  resumen_variable,
  file = "outputs/tables/03_resumen_variable_aprendida.csv",
  row.names = FALSE
)

write.csv(
  interpretacion_variable,
  file = "outputs/tables/03_interpretacion_variable_aprendida.csv",
  row.names = FALSE
)

write.csv(
  decision_funcional,
  file = "outputs/tables/03_decision_funcional_variable_aprendida.csv",
  row.names = FALSE
)

resultado_adtr_03 <- list(
  matriz_variable = matriz_variable,
  metricas_variable = metricas_variable,
  tabla_obs_pred = tabla_obs_pred,
  resumen_variable = resumen_variable,
  interpretacion_variable = interpretacion_variable,
  decision_funcional = decision_funcional
)

saveRDS(
  resultado_adtr_03,
  file = "outputs/results/03_resultado_adtr_variable_aprendida.rds"
)


# ============================================================
# 10. Impresión final
# ============================================================

cat("\n============================================================\n")
cat("Matriz ADTR para variable aprendida:\n")
cat("============================================================\n")
print(matriz_variable)

cat("\n============================================================\n")
cat("Métricas ADTR de variable aprendida:\n")
cat("============================================================\n")
print(metricas_variable)

cat("\n============================================================\n")
cat("Tabla observado-predicho:\n")
cat("============================================================\n")
print(tabla_obs_pred)

cat("\n============================================================\n")
cat("Resumen numérico de variable aprendida:\n")
cat("============================================================\n")
print(resumen_variable)

cat("\n============================================================\n")
cat("Interpretación ADTR prudente:\n")
cat("============================================================\n")
print(interpretacion_variable)

cat("\n============================================================\n")
cat("Decisión funcional:\n")
cat("============================================================\n")
print(decision_funcional)

cat("\n============================================================\n")
cat("Script 03 finalizado correctamente.\n")
cat("Resultados exportados en outputs/tables y outputs/results.\n")
cat("============================================================\n")

