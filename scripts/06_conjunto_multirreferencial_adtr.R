
# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: 06_conjunto_multirreferencial_adtr.R
# Autor: Rubén Alexander More Valencia
# Propósito:
#   Integrar múltiples matrices ADTR M(p_i) en una colección
#   multirreferencial M(P), calcular métricas comparativas,
#   identificar puntos críticos y preparar insumos para redes
#   transformacionales.
#
#   Este script NO entrena modelos ni duplica funcionalidades
#   de DSNeuralRNAS o ML.DSNeuralRNAS. Trabaja sobre matrices
#   ADTR ya derivadas desde salidas existentes.
# ============================================================


# ============================================================
# 0. Configuración general
# ============================================================

cat("\n============================================================\n")
cat("ADTR.DSNeuralRNAS - Conjunto multirreferencial ADTR\n")
cat("============================================================\n")

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. Validación de matriz ADTR individual
# ============================================================

validar_matriz_adtr <- function(matriz_adtr) {

  columnas_requeridas <- c(
    "punto", "sistema", "representacion",
    "rol", "funcion", "naturaleza",
    "fuente_programatica"
  )

  faltantes <- setdiff(columnas_requeridas, names(matriz_adtr))

  if (length(faltantes) > 0) {
    stop(
      "La matriz ADTR no contiene las columnas requeridas: ",
      paste(faltantes, collapse = ", ")
    )
  }

  if (nrow(matriz_adtr) < 1) {
    stop("La matriz ADTR no contiene filas.")
  }

  invisible(TRUE)
}


# ============================================================
# 2. Unir matrices ADTR en una colección M(P)
# ============================================================

unir_matrices_adtr <- function(lista_matrices) {

  if (!is.list(lista_matrices)) {
    stop("Debe proporcionar una lista de matrices ADTR.")
  }

  if (length(lista_matrices) == 0) {
    stop("La lista de matrices está vacía.")
  }

  for (i in seq_along(lista_matrices)) {
    validar_matriz_adtr(lista_matrices[[i]])
  }

  coleccion <- do.call(rbind, lista_matrices)
  rownames(coleccion) <- NULL

  coleccion
}


# ============================================================
# 3. Calcular métricas referenciales por punto
# ============================================================

calcular_metricas_por_punto_adtr <- function(coleccion_adtr) {

  validar_matriz_adtr(coleccion_adtr)

  puntos <- unique(coleccion_adtr$punto)

  resultados <- list()

  for (i in seq_along(puntos)) {

    p <- puntos[i]
    sub <- coleccion_adtr[coleccion_adtr$punto == p, ]

    cobertura_referencial <- length(unique(sub$sistema))
    diversidad_representaciones <- length(unique(sub$representacion))
    diversidad_roles <- length(unique(sub$rol))
    diversidad_funcional <- length(unique(sub$funcion))
    diversidad_naturalezas <- length(unique(sub$naturaleza))

    indice_trazabilidad <- mean(
      !is.na(sub$fuente_programatica) &
        sub$fuente_programatica != ""
    )

    resultados[[i]] <- data.frame(
      punto = p,
      filas_matriz = nrow(sub),
      cobertura_referencial = cobertura_referencial,
      diversidad_representaciones = diversidad_representaciones,
      diversidad_roles = diversidad_roles,
      diversidad_funcional = diversidad_funcional,
      diversidad_naturalezas = diversidad_naturalezas,
      indice_trazabilidad = indice_trazabilidad,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, resultados)
}


# ============================================================
# 4. Identificar puntos críticos ADTR
# ============================================================

identificar_puntos_criticos_adtr <- function(
    metricas_puntos,
    umbral_cobertura = 3,
    umbral_diversidad_funcional = 3,
    umbral_trazabilidad_baja = 0.80
) {

  metricas_puntos$punto_critico <- FALSE
  metricas_puntos$criterio_critico <- ""

  for (i in seq_len(nrow(metricas_puntos))) {

    criterios <- character()

    if (metricas_puntos$cobertura_referencial[i] >= umbral_cobertura) {
      criterios <- c(criterios, "alta cobertura referencial")
    }

    if (metricas_puntos$diversidad_funcional[i] >= umbral_diversidad_funcional) {
      criterios <- c(criterios, "alta diversidad funcional")
    }

    if (metricas_puntos$indice_trazabilidad[i] < umbral_trazabilidad_baja) {
      criterios <- c(criterios, "trazabilidad baja")
    }

    if (length(criterios) > 0) {
      metricas_puntos$punto_critico[i] <- TRUE
      metricas_puntos$criterio_critico[i] <- paste(criterios, collapse = "; ")
    } else {
      metricas_puntos$criterio_critico[i] <- "sin criterio crítico dominante"
    }
  }

  metricas_puntos
}


# ============================================================
# 5. Agrupar puntos por sistema referencial
# ============================================================

agrupar_puntos_por_sistema_adtr <- function(coleccion_adtr) {

  validar_matriz_adtr(coleccion_adtr)

  tabla <- aggregate(
    punto ~ sistema,
    data = coleccion_adtr,
    FUN = function(x) paste(unique(x), collapse = ", ")
  )

  conteo <- aggregate(
    punto ~ sistema,
    data = coleccion_adtr,
    FUN = function(x) length(unique(x))
  )

  names(conteo)[2] <- "numero_puntos"

  resultado <- merge(tabla, conteo, by = "sistema")
  names(resultado)[2] <- "puntos"

  resultado[order(resultado$sistema), ]
}


# ============================================================
# 6. Agrupar puntos por función
# ============================================================

agrupar_puntos_por_funcion_adtr <- function(coleccion_adtr) {

  validar_matriz_adtr(coleccion_adtr)

  tabla <- aggregate(
    punto ~ funcion,
    data = coleccion_adtr,
    FUN = function(x) paste(unique(x), collapse = ", ")
  )

  conteo <- aggregate(
    punto ~ funcion,
    data = coleccion_adtr,
    FUN = function(x) length(unique(x))
  )

  names(conteo)[2] <- "numero_puntos"

  resultado <- merge(tabla, conteo, by = "funcion")
  names(resultado)[2] <- "puntos"

  resultado[order(-resultado$numero_puntos), ]
}


# ============================================================
# 7. Construir relaciones preliminares entre puntos
# ============================================================

construir_relaciones_puntos_adtr <- function(coleccion_adtr) {

  validar_matriz_adtr(coleccion_adtr)

  puntos <- unique(coleccion_adtr$punto)

  if (length(puntos) < 2) {
    stop("Se requieren al menos dos puntos para construir relaciones.")
  }

  pares <- expand.grid(
    punto_origen = puntos,
    punto_destino = puntos,
    stringsAsFactors = FALSE
  )

  pares <- pares[pares$punto_origen != pares$punto_destino, ]

  relaciones <- list()

  for (i in seq_len(nrow(pares))) {

    p1 <- pares$punto_origen[i]
    p2 <- pares$punto_destino[i]

    sub1 <- coleccion_adtr[coleccion_adtr$punto == p1, ]
    sub2 <- coleccion_adtr[coleccion_adtr$punto == p2, ]

    sistemas_comunes <- intersect(unique(sub1$sistema), unique(sub2$sistema))
    funciones_comunes <- intersect(unique(sub1$funcion), unique(sub2$funcion))

    tipo_relacion <- "relacion_de_coexistencia"
    justificacion <- "Los puntos pertenecen al mismo conjunto ADTR, pero no comparten sistemas o funciones dominantes."

    if (length(sistemas_comunes) > 0 && length(funciones_comunes) > 0) {

      tipo_relacion <- "relacion_sistemico_funcional"
      justificacion <- paste0(
        "Los puntos comparten sistemas: ",
        paste(sistemas_comunes, collapse = ", "),
        " y funciones: ",
        paste(funciones_comunes, collapse = "; "),
        "."
      )

    } else if (length(sistemas_comunes) > 0) {

      tipo_relacion <- "relacion_por_sistema"
      justificacion <- paste0(
        "Los puntos comparten sistemas referenciales: ",
        paste(sistemas_comunes, collapse = ", "),
        "."
      )

    } else if (length(funciones_comunes) > 0) {

      tipo_relacion <- "relacion_por_funcion"
      justificacion <- paste0(
        "Los puntos comparten funciones: ",
        paste(funciones_comunes, collapse = "; "),
        "."
      )
    }

    relaciones[[i]] <- data.frame(
      punto_origen = p1,
      punto_destino = p2,
      sistemas_comunes = paste(sistemas_comunes, collapse = ", "),
      funciones_comunes = paste(funciones_comunes, collapse = "; "),
      tipo_relacion = tipo_relacion,
      justificacion = justificacion,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, relaciones)
}


# ============================================================
# 8. Interpretación del conjunto ADTR
# ============================================================

interpretar_conjunto_adtr <- function(metricas_puntos,
                                      puntos_criticos,
                                      relaciones_puntos) {

  interpretaciones <- character()

  n_puntos <- nrow(metricas_puntos)
  n_criticos <- sum(puntos_criticos$punto_critico)

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "El conjunto ADTR evaluado contiene ",
      n_puntos,
      " puntos referenciales."
    )
  )

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "Se identificaron ",
      n_criticos,
      " puntos críticos según cobertura, diversidad funcional o trazabilidad."
    )
  )

  promedio_cobertura <- mean(metricas_puntos$cobertura_referencial)
  promedio_trazabilidad <- mean(metricas_puntos$indice_trazabilidad)

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "La cobertura referencial promedio del conjunto es ",
      round(promedio_cobertura, 3),
      "."
    )
  )

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "El índice promedio de trazabilidad es ",
      round(promedio_trazabilidad, 3),
      "."
    )
  )

  if (promedio_trazabilidad == 1) {
    interpretaciones <- c(
      interpretaciones,
      "Todas las matrices del conjunto mantienen trazabilidad completa hacia fuentes programáticas."
    )
  } else {
    interpretaciones <- c(
      interpretaciones,
      "Existen puntos con trazabilidad incompleta; se requiere revisión antes de escalar."
    )
  }

  tipos_relacion <- table(relaciones_puntos$tipo_relacion)

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "Las relaciones entre puntos muestran los siguientes tipos: ",
      paste(names(tipos_relacion), tipos_relacion, sep = "=", collapse = "; "),
      "."
    )
  )

  data.frame(
    numero = seq_along(interpretaciones),
    interpretacion = interpretaciones,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 9. Decisión funcional para conjuntos
# ============================================================

evaluar_decision_funcional_conjunto_adtr <- function(
    puede_derivarse = TRUE,
    utilidad_aplicada = TRUE,
    escalamiento = TRUE
) {

  if (isTRUE(puede_derivarse) && isTRUE(utilidad_aplicada) && isTRUE(escalamiento)) {

    decision <- "posible_funcionalidad_auxiliar_de_integracion"
    justificacion <- paste(
      "El análisis de conjuntos puede derivarse de matrices ADTR existentes.",
      "Sin embargo, por escalamiento y repetibilidad, podría justificarse",
      "una función auxiliar para unir matrices, comparar métricas e identificar puntos críticos."
    )

  } else if (isTRUE(puede_derivarse)) {

    decision <- "derivar_de_salidas_existentes"
    justificacion <- "El análisis puede mantenerse como integración de salidas existentes."

  } else {

    decision <- "mantener_como_lectura_conceptual"
    justificacion <- "No existe evidencia suficiente para proponer funcionalidad auxiliar."
  }

  data.frame(
    necesidad = "Integrar múltiples puntos ADTR en un conjunto multirreferencial",
    puede_derivarse = puede_derivarse,
    utilidad_aplicada = utilidad_aplicada,
    escalamiento = escalamiento,
    decision = decision,
    justificacion = justificacion,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 10. Ejemplo mínimo reproducible: matrices M(p)
# ============================================================

matriz_y <- data.frame(
  punto = c("y", "y", "y", "y", "y"),
  sistema = c("S_obs", "S_apr", "S_apr", "S_ctrl", "S_learned"),
  representacion = c(
    "preparado$y",
    "unidad$variable_objetivo = y",
    "prediccion",
    "error observado-predicho",
    "comparación observado-predicho"
  ),
  rol = c(
    "variable observada",
    "objetivo de aprendizaje",
    "salida estimada",
    "señal de discrepancia",
    "componente aprendido evaluable"
  ),
  funcion = c(
    "representar el valor factual de referencia",
    "orientar el ajuste del modelo",
    "aproximar la variable observada",
    "evaluar necesidad de corrección",
    "valorar la calidad de la representación aprendida"
  ),
  naturaleza = c(
    "dato factual observado",
    "referente supervisado",
    "representación aprendida",
    "señal diagnóstica",
    "evidencia de aprendizaje"
  ),
  fuente_programatica = c(
    "ajuste_demo$preparado$y",
    "ajuste_demo$unidad$variable_objetivo",
    "ajuste_demo$prediccion",
    "ajuste_demo$preparado$y - ajuste_demo$prediccion",
    "ajuste_demo$metricas"
  ),
  observacion = c(
    "La variable objetivo aparece como dato observado.",
    "La variable objetivo define el referente del aprendizaje.",
    "La predicción representa la forma aprendida.",
    "La discrepancia puede orientar corrección.",
    "La variable se evalúa mediante métricas."
  ),
  stringsAsFactors = FALSE
)

matriz_loss <- data.frame(
  punto = c("loss", "loss", "loss"),
  sistema = c("S_apr", "S_ctrl", "S_learned"),
  representacion = c(
    "trayectoria$loss",
    "reducción de pérdida",
    "resumen de trayectoria"
  ),
  rol = c(
    "medida de error",
    "criterio de control",
    "componente dinámico aprendido"
  ),
  funcion = c(
    "orientar el ajuste del modelo",
    "evaluar mejora y posible ajuste",
    "caracterizar estabilidad del aprendizaje"
  ),
  naturaleza = c(
    "señal de aprendizaje",
    "señal diagnóstica",
    "evidencia dinámica"
  ),
  fuente_programatica = c(
    "ajuste_demo$trayectoria$loss",
    "ajuste_demo$metricas$reduccion_rel",
    "ajuste_demo$trayectoria"
  ),
  observacion = c(
    "La pérdida registra tensión de ajuste.",
    "La reducción de pérdida puede orientar control.",
    "La trayectoria permite interpretar estabilidad."
  ),
  stringsAsFactors = FALSE
)

matriz_grad_norm <- data.frame(
  punto = c("grad_norm", "grad_norm", "grad_norm"),
  sistema = c("S_apr", "S_ctrl", "S_learned"),
  representacion = c(
    "trayectoria$grad_norm",
    "nivel de presión dinámica",
    "patrón de presión de aprendizaje"
  ),
  rol = c(
    "magnitud del gradiente",
    "indicador de estabilidad o inestabilidad",
    "componente dinámico aprendido"
  ),
  funcion = c(
    "representar presión de ajuste",
    "apoyar decisiones de ajuste de tasa o control",
    "caracterizar intensidad y estabilización del aprendizaje"
  ),
  naturaleza = c(
    "señal diferencial del aprendizaje",
    "señal de control potencial",
    "evidencia de dinámica interna"
  ),
  fuente_programatica = c(
    "ajuste_demo$trayectoria$grad_norm",
    "ajuste_demo$trayectoria$grad_norm",
    "ajuste_demo$trayectoria"
  ),
  observacion = c(
    "La norma del gradiente resume intensidad de cambio.",
    "Puede interpretarse como presión de ajuste.",
    "Puede contribuir a identificar regímenes."
  ),
  stringsAsFactors = FALSE
)

matriz_eta <- data.frame(
  punto = c("eta", "eta", "eta"),
  sistema = c("S_apr", "S_ctrl", "S_learned"),
  representacion = c(
    "trayectoria$eta",
    "secuencia de eta",
    "patrón de política de tasa"
  ),
  rol = c(
    "tasa de aprendizaje",
    "variable de control",
    "evidencia de estrategia adaptativa"
  ),
  funcion = c(
    "regular la magnitud de actualización",
    "ajustar la dinámica de aprendizaje",
    "caracterizar una política de aprendizaje"
  ),
  naturaleza = c(
    "parámetro operativo",
    "componente de control adaptativo",
    "evidencia meta-dinámica"
  ),
  fuente_programatica = c(
    "ajuste_demo$trayectoria$eta",
    "ajuste_demo$trayectoria$eta",
    "ajuste_demo$trayectoria"
  ),
  observacion = c(
    "Eta regula la intensidad de actualización.",
    "Cambios en eta pueden interpretarse como control.",
    "La secuencia puede leerse como política adaptativa."
  ),
  stringsAsFactors = FALSE
)


# ============================================================
# 11. Ejecución del análisis de conjunto
# ============================================================

coleccion_adtr <- unir_matrices_adtr(
  list(
    matriz_y,
    matriz_loss,
    matriz_grad_norm,
    matriz_eta
  )
)

metricas_puntos <- calcular_metricas_por_punto_adtr(
  coleccion_adtr
)

puntos_criticos <- identificar_puntos_criticos_adtr(
  metricas_puntos
)

agrupacion_sistemas <- agrupar_puntos_por_sistema_adtr(
  coleccion_adtr
)

agrupacion_funciones <- agrupar_puntos_por_funcion_adtr(
  coleccion_adtr
)

relaciones_puntos <- construir_relaciones_puntos_adtr(
  coleccion_adtr
)

interpretacion_conjunto <- interpretar_conjunto_adtr(
  metricas_puntos,
  puntos_criticos,
  relaciones_puntos
)

decision_funcional <- evaluar_decision_funcional_conjunto_adtr(
  puede_derivarse = TRUE,
  utilidad_aplicada = TRUE,
  escalamiento = TRUE
)


# ============================================================
# 12. Exportación de resultados
# ============================================================

write.csv(
  coleccion_adtr,
  file = "outputs/tables/06_coleccion_multirreferencial_adtr.csv",
  row.names = FALSE
)

write.csv(
  metricas_puntos,
  file = "outputs/tables/06_metricas_por_punto_adtr.csv",
  row.names = FALSE
)

write.csv(
  puntos_criticos,
  file = "outputs/tables/06_puntos_criticos_adtr.csv",
  row.names = FALSE
)

write.csv(
  agrupacion_sistemas,
  file = "outputs/tables/06_agrupacion_puntos_por_sistema_adtr.csv",
  row.names = FALSE
)

write.csv(
  agrupacion_funciones,
  file = "outputs/tables/06_agrupacion_puntos_por_funcion_adtr.csv",
  row.names = FALSE
)

write.csv(
  relaciones_puntos,
  file = "outputs/tables/06_relaciones_entre_puntos_adtr.csv",
  row.names = FALSE
)

write.csv(
  interpretacion_conjunto,
  file = "outputs/tables/06_interpretacion_conjunto_adtr.csv",
  row.names = FALSE
)

write.csv(
  decision_funcional,
  file = "outputs/tables/06_decision_funcional_conjunto_adtr.csv",
  row.names = FALSE
)

resultado_adtr_06 <- list(
  coleccion_adtr = coleccion_adtr,
  metricas_puntos = metricas_puntos,
  puntos_criticos = puntos_criticos,
  agrupacion_sistemas = agrupacion_sistemas,
  agrupacion_funciones = agrupacion_funciones,
  relaciones_puntos = relaciones_puntos,
  interpretacion_conjunto = interpretacion_conjunto,
  decision_funcional = decision_funcional
)

saveRDS(
  resultado_adtr_06,
  file = "outputs/results/06_resultado_conjunto_multirreferencial_adtr.rds"
)


# ============================================================
# 13. Impresión final
# ============================================================

cat("\n============================================================\n")
cat("Colección multirreferencial ADTR M(P):\n")
cat("============================================================\n")
print(coleccion_adtr)

cat("\n============================================================\n")
cat("Métricas por punto:\n")
cat("============================================================\n")
print(metricas_puntos)

cat("\n============================================================\n")
cat("Puntos críticos:\n")
cat("============================================================\n")
print(puntos_criticos)

cat("\n============================================================\n")
cat("Agrupación por sistema:\n")
cat("============================================================\n")
print(agrupacion_sistemas)

cat("\n============================================================\n")
cat("Agrupación por función:\n")
cat("============================================================\n")
print(agrupacion_funciones)

cat("\n============================================================\n")
cat("Relaciones entre puntos:\n")
cat("============================================================\n")
print(relaciones_puntos)

cat("\n============================================================\n")
cat("Interpretación del conjunto ADTR:\n")
cat("============================================================\n")
print(interpretacion_conjunto)

cat("\n============================================================\n")
cat("Decisión funcional:\n")
cat("============================================================\n")
print(decision_funcional)

cat("\n============================================================\n")
cat("Script 06 finalizado correctamente.\n")
cat("Resultados exportados en outputs/tables y outputs/results.\n")
cat("============================================================\n")

