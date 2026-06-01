
# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: 02_matriz_adtr_perdida_gradiente_eta.R
# Autor: Rubén Alexander More Valencia
# Propósito:
#   Construir matrices ADTR específicas para tres puntos
#   referenciales centrales del aprendizaje:
#   loss, grad_norm y eta.
#
#   El script parte de salidas ya existentes en un objeto de
#   entrenamiento y NO propone nueva funcionalidad neuronal.
# ============================================================


# ============================================================
# 0. Configuración general
# ============================================================

cat("\n============================================================\n")
cat("ADTR.DSNeuralRNAS - Matrices para loss, grad_norm y eta\n")
cat("============================================================\n")

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. Funciones base ADTR reutilizadas
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
# 2. Validación de trayectoria
# ============================================================

validar_trayectoria_adtr <- function(objeto, nombre_objeto = "objeto") {

  if (!"trayectoria" %in% names(objeto)) {
    stop("El objeto no contiene el componente `trayectoria`.")
  }

  trayectoria <- objeto$trayectoria

  if (!is.data.frame(trayectoria)) {
    stop("El componente `trayectoria` debe ser un data.frame.")
  }

  requeridas <- c("loss", "grad_norm", "eta")
  faltantes <- setdiff(requeridas, names(trayectoria))

  if (length(faltantes) > 0) {
    stop(
      "La trayectoria no contiene las columnas requeridas: ",
      paste(faltantes, collapse = ", ")
    )
  }

  cat("\nTrayectoria validada para:", nombre_objeto, "\n")
  cat("Columnas detectadas:", paste(names(trayectoria), collapse = ", "), "\n")

  invisible(TRUE)
}


# ============================================================
# 3. Construcción de matriz ADTR para loss
# ============================================================

construir_matriz_loss_adtr <- function(nombre_objeto = "ajuste") {

  rbind(
    construir_matriz_adtr_minima(
      punto = "loss",
      sistema = "S_apr",
      representacion = "trayectoria$loss",
      rol = "medida de error",
      funcion = "orientar el ajuste del modelo",
      naturaleza = "señal de aprendizaje",
      fuente_programatica = paste0(nombre_objeto, "$trayectoria$loss"),
      observacion = "La pérdida registra la tensión de ajuste durante el entrenamiento."
    ),
    construir_matriz_adtr_minima(
      punto = "loss",
      sistema = "S_ctrl",
      representacion = "reducción de pérdida",
      rol = "criterio de control",
      funcion = "evaluar mejora y posible ajuste",
      naturaleza = "señal diagnóstica",
      fuente_programatica = paste0(nombre_objeto, "$metricas$reduccion_rel"),
      observacion = "La reducción de pérdida puede orientar decisiones de control."
    ),
    construir_matriz_adtr_minima(
      punto = "loss",
      sistema = "S_learned",
      representacion = "resumen de trayectoria",
      rol = "componente dinámico aprendido",
      funcion = "caracterizar estabilidad del aprendizaje",
      naturaleza = "evidencia dinámica",
      fuente_programatica = paste0(nombre_objeto, "$trayectoria"),
      observacion = "La trayectoria de pérdida permite interpretar estabilidad o régimen."
    )
  )
}


# ============================================================
# 4. Construcción de matriz ADTR para grad_norm
# ============================================================

construir_matriz_grad_norm_adtr <- function(nombre_objeto = "ajuste") {

  rbind(
    construir_matriz_adtr_minima(
      punto = "grad_norm",
      sistema = "S_apr",
      representacion = "trayectoria$grad_norm",
      rol = "magnitud del gradiente",
      funcion = "representar presión de ajuste",
      naturaleza = "señal diferencial del aprendizaje",
      fuente_programatica = paste0(nombre_objeto, "$trayectoria$grad_norm"),
      observacion = "La norma del gradiente resume la intensidad de cambio durante el aprendizaje."
    ),
    construir_matriz_adtr_minima(
      punto = "grad_norm",
      sistema = "S_ctrl",
      representacion = "nivel de presión dinámica",
      rol = "indicador de estabilidad o inestabilidad",
      funcion = "apoyar decisiones de ajuste de tasa o control",
      naturaleza = "señal de control potencial",
      fuente_programatica = paste0(nombre_objeto, "$trayectoria$grad_norm"),
      observacion = "Valores altos o persistentes pueden interpretarse como presión de ajuste."
    ),
    construir_matriz_adtr_minima(
      punto = "grad_norm",
      sistema = "S_learned",
      representacion = "patrón de presión de aprendizaje",
      rol = "componente dinámico aprendido",
      funcion = "caracterizar intensidad y estabilización del aprendizaje",
      naturaleza = "evidencia de dinámica interna",
      fuente_programatica = paste0(nombre_objeto, "$trayectoria"),
      observacion = "La trayectoria de grad_norm puede contribuir a identificar regímenes."
    )
  )
}


# ============================================================
# 5. Construcción de matriz ADTR para eta
# ============================================================

construir_matriz_eta_adtr <- function(nombre_objeto = "ajuste") {

  rbind(
    construir_matriz_adtr_minima(
      punto = "eta",
      sistema = "S_apr",
      representacion = "trayectoria$eta",
      rol = "tasa de aprendizaje",
      funcion = "regular la magnitud de actualización",
      naturaleza = "parámetro operativo",
      fuente_programatica = paste0(nombre_objeto, "$trayectoria$eta"),
      observacion = "Eta regula la intensidad de actualización durante el entrenamiento."
    ),
    construir_matriz_adtr_minima(
      punto = "eta",
      sistema = "S_ctrl",
      representacion = "secuencia de eta",
      rol = "variable de control",
      funcion = "ajustar la dinámica de aprendizaje",
      naturaleza = "componente de control adaptativo",
      fuente_programatica = paste0(nombre_objeto, "$trayectoria$eta"),
      observacion = "Cambios en eta pueden interpretarse como acciones de control."
    ),
    construir_matriz_adtr_minima(
      punto = "eta",
      sistema = "S_learned",
      representacion = "patrón de política de tasa",
      rol = "evidencia de estrategia adaptativa",
      funcion = "caracterizar una política de aprendizaje",
      naturaleza = "evidencia meta-dinámica",
      fuente_programatica = paste0(nombre_objeto, "$trayectoria"),
      observacion = "La secuencia de eta puede leerse como política o patrón adaptativo."
    )
  )
}


# ============================================================
# 6. Resumen numérico de trayectoria
# ============================================================

resumir_trayectoria_adtr <- function(objeto) {

  trayectoria <- objeto$trayectoria

  data.frame(
    iteraciones = nrow(trayectoria),

    loss_inicial = trayectoria$loss[1],
    loss_final = trayectoria$loss[nrow(trayectoria)],
    reduccion_loss_abs = trayectoria$loss[1] - trayectoria$loss[nrow(trayectoria)],
    reduccion_loss_rel = (
      trayectoria$loss[1] - trayectoria$loss[nrow(trayectoria)]
    ) / trayectoria$loss[1],

    grad_norm_inicial = trayectoria$grad_norm[1],
    grad_norm_final = trayectoria$grad_norm[nrow(trayectoria)],
    reduccion_grad_abs = trayectoria$grad_norm[1] - trayectoria$grad_norm[nrow(trayectoria)],
    reduccion_grad_rel = (
      trayectoria$grad_norm[1] - trayectoria$grad_norm[nrow(trayectoria)]
    ) / trayectoria$grad_norm[1],

    eta_inicial = trayectoria$eta[1],
    eta_final = trayectoria$eta[nrow(trayectoria)],
    cambio_eta_abs = trayectoria$eta[nrow(trayectoria)] - trayectoria$eta[1],
    cambio_eta_rel = (
      trayectoria$eta[nrow(trayectoria)] - trayectoria$eta[1]
    ) / trayectoria$eta[1],

    stringsAsFactors = FALSE
  )
}


# ============================================================
# 7. Interpretación automática prudente
# ============================================================

interpretar_resumen_adtr <- function(resumen) {

  interpretaciones <- character()

  if (resumen$reduccion_loss_rel > 0.5) {
    interpretaciones <- c(
      interpretaciones,
      "La pérdida muestra una reducción relativa alta; puede interpretarse como mejora clara del ajuste."
    )
  } else if (resumen$reduccion_loss_rel > 0) {
    interpretaciones <- c(
      interpretaciones,
      "La pérdida disminuye, aunque la mejora relativa es moderada."
    )
  } else {
    interpretaciones <- c(
      interpretaciones,
      "La pérdida no disminuye; se requiere revisar estabilidad o configuración del aprendizaje."
    )
  }

  if (resumen$reduccion_grad_rel > 0.5) {
    interpretaciones <- c(
      interpretaciones,
      "La norma del gradiente disminuye de forma importante; esto sugiere menor presión de ajuste hacia el final."
    )
  } else if (resumen$reduccion_grad_rel > 0) {
    interpretaciones <- c(
      interpretaciones,
      "La norma del gradiente disminuye parcialmente; aún puede existir presión de ajuste."
    )
  } else {
    interpretaciones <- c(
      interpretaciones,
      "La norma del gradiente no disminuye; puede existir inestabilidad o presión persistente."
    )
  }

  if (resumen$cambio_eta_abs < 0) {
    interpretaciones <- c(
      interpretaciones,
      "La tasa eta disminuye durante la trayectoria; puede leerse como reducción de intensidad de aprendizaje."
    )
  } else if (resumen$cambio_eta_abs > 0) {
    interpretaciones <- c(
      interpretaciones,
      "La tasa eta aumenta durante la trayectoria; puede leerse como intensificación del aprendizaje."
    )
  } else {
    interpretaciones <- c(
      interpretaciones,
      "La tasa eta permanece constante; se interpreta como configuración operativa fija."
    )
  }

  data.frame(
    numero = seq_along(interpretaciones),
    interpretacion = interpretaciones,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 8. Ejemplo mínimo reproducible
# ============================================================

# Este objeto puede reemplazarse luego por un resultado real:
# ajuste_perceptron, ajuste_mlp, unidad_aprendida, etc.

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
# 9. Ejecución del análisis ADTR
# ============================================================

validar_trayectoria_adtr(
  objeto = ajuste_demo,
  nombre_objeto = "ajuste_demo"
)

matriz_loss <- construir_matriz_loss_adtr("ajuste_demo")
matriz_grad_norm <- construir_matriz_grad_norm_adtr("ajuste_demo")
matriz_eta <- construir_matriz_eta_adtr("ajuste_demo")

matriz_adtr_trayectoria <- rbind(
  matriz_loss,
  matriz_grad_norm,
  matriz_eta
)

metricas_loss <- calcular_metricas_adtr_minimas(matriz_loss)
metricas_grad_norm <- calcular_metricas_adtr_minimas(matriz_grad_norm)
metricas_eta <- calcular_metricas_adtr_minimas(matriz_eta)

metricas_adtr_trayectoria <- rbind(
  metricas_loss,
  metricas_grad_norm,
  metricas_eta
)

resumen_trayectoria <- resumir_trayectoria_adtr(ajuste_demo)
interpretacion_trayectoria <- interpretar_resumen_adtr(resumen_trayectoria)


# ============================================================
# 10. Decisión funcional
# ============================================================

decision_funcional <- evaluar_necesidad_funcional_adtr(
  necesidad = "Construir matrices ADTR para loss, grad_norm y eta",
  existe_en_dsneuralrnas = FALSE,
  existe_en_mldsneuralrnas = FALSE,
  puede_derivarse = TRUE,
  utilidad_aplicada = TRUE,
  escalamiento = TRUE
)


# ============================================================
# 11. Exportación de resultados
# ============================================================

write.csv(
  matriz_loss,
  file = "outputs/tables/02_matriz_adtr_loss.csv",
  row.names = FALSE
)

write.csv(
  matriz_grad_norm,
  file = "outputs/tables/02_matriz_adtr_grad_norm.csv",
  row.names = FALSE
)

write.csv(
  matriz_eta,
  file = "outputs/tables/02_matriz_adtr_eta.csv",
  row.names = FALSE
)

write.csv(
  matriz_adtr_trayectoria,
  file = "outputs/tables/02_matriz_adtr_trayectoria_completa.csv",
  row.names = FALSE
)

write.csv(
  metricas_adtr_trayectoria,
  file = "outputs/tables/02_metricas_adtr_loss_grad_eta.csv",
  row.names = FALSE
)

write.csv(
  resumen_trayectoria,
  file = "outputs/tables/02_resumen_numerico_trayectoria.csv",
  row.names = FALSE
)

write.csv(
  interpretacion_trayectoria,
  file = "outputs/tables/02_interpretacion_trayectoria.csv",
  row.names = FALSE
)

write.csv(
  decision_funcional,
  file = "outputs/tables/02_decision_funcional_loss_grad_eta.csv",
  row.names = FALSE
)

resultado_adtr_02 <- list(
  matriz_loss = matriz_loss,
  matriz_grad_norm = matriz_grad_norm,
  matriz_eta = matriz_eta,
  matriz_adtr_trayectoria = matriz_adtr_trayectoria,
  metricas_adtr_trayectoria = metricas_adtr_trayectoria,
  resumen_trayectoria = resumen_trayectoria,
  interpretacion_trayectoria = interpretacion_trayectoria,
  decision_funcional = decision_funcional
)

saveRDS(
  resultado_adtr_02,
  file = "outputs/results/02_resultado_adtr_loss_grad_eta.rds"
)


# ============================================================
# 12. Impresión final
# ============================================================

cat("\n============================================================\n")
cat("Matriz ADTR completa para trayectoria:\n")
cat("============================================================\n")
print(matriz_adtr_trayectoria)

cat("\n============================================================\n")
cat("Métricas ADTR por punto:\n")
cat("============================================================\n")
print(metricas_adtr_trayectoria)

cat("\n============================================================\n")
cat("Resumen numérico de trayectoria:\n")
cat("============================================================\n")
print(resumen_trayectoria)

cat("\n============================================================\n")
cat("Interpretación ADTR prudente:\n")
cat("============================================================\n")
print(interpretacion_trayectoria)

cat("\n============================================================\n")
cat("Decisión funcional:\n")
cat("============================================================\n")
print(decision_funcional)

cat("\n============================================================\n")
cat("Script 02 finalizado correctamente.\n")
cat("Resultados exportados en outputs/tables y outputs/results.\n")
cat("============================================================\n")

