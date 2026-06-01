
# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: 08_madurez_ecosistemica_adtr.R
# Autor: Rubén Alexander More Valencia
# Propósito:
#   Evaluar la madurez ecosistémica preliminar de una red
#   transformacional ADTR.
#
#   Este script NO entrena modelos, NO recalcula predicciones
#   y NO afirma automáticamente un ecosistema completo.
#   Evalúa si una red transformacional tiene condiciones
#   mínimas para ser interpretada como base ecosistémica inicial.
# ============================================================


# ============================================================
# 0. Configuración general
# ============================================================

cat("\n============================================================\n")
cat("ADTR.DSNeuralRNAS - Madurez ecosistémica ADTR\n")
cat("============================================================\n")

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. Validaciones
# ============================================================

validar_nodos_adtr <- function(nodos) {

  columnas_requeridas <- c(
    "id", "tipo_punto", "indice_trazabilidad",
    "cobertura_referencial", "diversidad_funcional"
  )

  faltantes <- setdiff(columnas_requeridas, names(nodos))

  if (length(faltantes) > 0) {
    stop(
      "La tabla de nodos no contiene las columnas requeridas: ",
      paste(faltantes, collapse = ", ")
    )
  }

  invisible(TRUE)
}


validar_aristas_adtr <- function(aristas) {

  columnas_requeridas <- c(
    "from", "to", "tipo_relacion", "peso", "justificacion"
  )

  faltantes <- setdiff(columnas_requeridas, names(aristas))

  if (length(faltantes) > 0) {
    stop(
      "La tabla de aristas no contiene las columnas requeridas: ",
      paste(faltantes, collapse = ", ")
    )
  }

  invisible(TRUE)
}


validar_coleccion_adtr <- function(coleccion_adtr) {

  columnas_requeridas <- c(
    "punto", "sistema", "representacion",
    "rol", "funcion", "naturaleza",
    "fuente_programatica"
  )

  faltantes <- setdiff(columnas_requeridas, names(coleccion_adtr))

  if (length(faltantes) > 0) {
    stop(
      "La colección ADTR no contiene las columnas requeridas: ",
      paste(faltantes, collapse = ", ")
    )
  }

  invisible(TRUE)
}


# ============================================================
# 2. Evaluar indicadores de madurez ecosistémica
# ============================================================

evaluar_indicadores_madurez_adtr <- function(nodos,
                                             aristas,
                                             coleccion_adtr) {

  validar_nodos_adtr(nodos)
  validar_aristas_adtr(aristas)
  validar_coleccion_adtr(coleccion_adtr)

  n_puntos <- nrow(nodos)
  n_aristas <- nrow(aristas)

  tipos_punto <- unique(nodos$tipo_punto)
  sistemas <- unique(coleccion_adtr$sistema)

  tiene_punto_factual <- any(nodos$tipo_punto == "punto_factual_observado")
  tiene_senales_internas <- any(nodos$tipo_punto == "senal_interna_transformacional")

  tiene_aprendizaje <- "S_apr" %in% sistemas
  tiene_control <- "S_ctrl" %in% sistemas
  tiene_aprendido <- "S_learned" %in% sistemas
  tiene_observacion <- "S_obs" %in% sistemas

  trazabilidad_promedio <- mean(nodos$indice_trazabilidad, na.rm = TRUE)

  relaciones_funcionales <- sum(
    aristas$tipo_relacion == "relacion_sistemico_funcional",
    na.rm = TRUE
  )

  relaciones_sistema <- sum(
    aristas$tipo_relacion == "relacion_por_sistema",
    na.rm = TRUE
  )

  diversidad_tipos_punto <- length(tipos_punto)

  presencia_componentes_dinamicos <- any(
    grepl(
      "loss|grad|eta|presión|tasa|dinámica|aprendizaje",
      paste(
        coleccion_adtr$punto,
        coleccion_adtr$representacion,
        coleccion_adtr$rol,
        coleccion_adtr$funcion,
        coleccion_adtr$naturaleza,
        collapse = " "
      ),
      ignore.case = TRUE
    )
  )

  presencia_control <- any(
    grepl(
      "control|corrección|ajuste de tasa|variable de control|señal diagnóstica",
      paste(
        coleccion_adtr$rol,
        coleccion_adtr$funcion,
        coleccion_adtr$naturaleza,
        collapse = " "
      ),
      ignore.case = TRUE
    )
  )

  presencia_evidencia_aprendida <- any(
    grepl(
      "aprendida|aprendizaje|evidencia",
      paste(
        coleccion_adtr$rol,
        coleccion_adtr$funcion,
        coleccion_adtr$naturaleza,
        collapse = " "
      ),
      ignore.case = TRUE
    )
  )

  data.frame(
    indicador = c(
      "multiplicidad_de_puntos",
      "trazabilidad_promedio",
      "diversidad_de_tipos_de_punto",
      "presencia_punto_factual",
      "presencia_senales_internas",
      "presencia_sistema_observacion",
      "presencia_sistema_aprendizaje",
      "presencia_sistema_control",
      "presencia_sistema_aprendido",
      "relaciones_entre_puntos",
      "relaciones_sistemico_funcionales",
      "presencia_componentes_dinamicos",
      "presencia_control",
      "presencia_evidencia_aprendida"
    ),
    valor = c(
      n_puntos,
      trazabilidad_promedio,
      diversidad_tipos_punto,
      tiene_punto_factual,
      tiene_senales_internas,
      tiene_observacion,
      tiene_aprendizaje,
      tiene_control,
      tiene_aprendido,
      n_aristas,
      relaciones_funcionales,
      presencia_componentes_dinamicos,
      presencia_control,
      presencia_evidencia_aprendida
    ),
    cumple = c(
      n_puntos >= 3,
      trazabilidad_promedio >= 0.90,
      diversidad_tipos_punto >= 2,
      tiene_punto_factual,
      tiene_senales_internas,
      tiene_observacion,
      tiene_aprendizaje,
      tiene_control,
      tiene_aprendido,
      n_aristas > 0,
      relaciones_funcionales > 0,
      presencia_componentes_dinamicos,
      presencia_control,
      presencia_evidencia_aprendida
    ),
    criterio = c(
      "Al menos tres puntos referenciales.",
      "Trazabilidad promedio igual o superior a 0.90.",
      "Al menos dos tipos de punto.",
      "Debe existir al menos un punto factual observado.",
      "Debe existir al menos una señal interna transformacional.",
      "Debe existir sistema de observación.",
      "Debe existir sistema de aprendizaje.",
      "Debe existir sistema de control.",
      "Debe existir sistema aprendido.",
      "Debe existir al menos una relación entre puntos.",
      "Debe existir al menos una relación sistémico-funcional.",
      "Deben existir señales o componentes dinámicos.",
      "Deben existir señales, roles o funciones de control.",
      "Debe existir evidencia aprendida o evaluable."
    ),
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 3. Calcular índice de madurez ecosistémica
# ============================================================

calcular_indice_madurez_ecosistemica_adtr <- function(indicadores) {

  if (!all(c("indicador", "cumple") %in% names(indicadores))) {
    stop("La tabla de indicadores debe contener `indicador` y `cumple`.")
  }

  pesos <- data.frame(
    indicador = c(
      "multiplicidad_de_puntos",
      "trazabilidad_promedio",
      "diversidad_de_tipos_de_punto",
      "presencia_punto_factual",
      "presencia_senales_internas",
      "presencia_sistema_observacion",
      "presencia_sistema_aprendizaje",
      "presencia_sistema_control",
      "presencia_sistema_aprendido",
      "relaciones_entre_puntos",
      "relaciones_sistemico_funcionales",
      "presencia_componentes_dinamicos",
      "presencia_control",
      "presencia_evidencia_aprendida"
    ),
    peso = c(
      0.08,
      0.12,
      0.08,
      0.08,
      0.08,
      0.06,
      0.08,
      0.08,
      0.08,
      0.06,
      0.06,
      0.08,
      0.08,
      0.06
    ),
    stringsAsFactors = FALSE
  )

  # Corrección: normalizar pesos para que sumen 1
  pesos$peso <- pesos$peso / sum(pesos$peso)

  tabla <- merge(indicadores, pesos, by = "indicador", all.x = TRUE)

  tabla$puntaje <- ifelse(tabla$cumple, tabla$peso, 0)

  indice <- sum(tabla$puntaje, na.rm = TRUE)

  nivel <- if (indice >= 0.85) {
    "base_ecosistemica_alta"
  } else if (indice >= 0.65) {
    "base_ecosistemica_intermedia"
  } else if (indice >= 0.45) {
    "base_ecosistemica_inicial"
  } else {
    "red_transformacional_sin_madurez_ecosistemica_suficiente"
  }

  data.frame(
    indice_madurez_ecosistemica = indice,
    nivel_madurez = nivel,
    observacion = "Índice exploratorio normalizado; no debe interpretarse como prueba universal de ecosistema completo.",
    stringsAsFactors = FALSE
  )
}

# ============================================================
# 4. Evaluar niveles de relación ecosistémica
# ============================================================

evaluar_niveles_relacion_ecosistemica <- function(aristas) {

  validar_aristas_adtr(aristas)

  niveles <- data.frame(
    nivel = c(
      "Nivel 1",
      "Nivel 2",
      "Nivel 3",
      "Nivel 4",
      "Nivel 5"
    ),
    tipo = c(
      "coexistencia referencial",
      "relación por sistema",
      "relación funcional",
      "relación dinámica",
      "acoplamiento interpretado"
    ),
    evidencia_actual = c(
      TRUE,
      any(aristas$tipo_relacion == "relacion_por_sistema"),
      any(aristas$tipo_relacion == "relacion_sistemico_funcional"),
      FALSE,
      FALSE
    ),
    observacion = c(
      "Los puntos pertenecen al mismo conjunto ADTR.",
      "Existen puntos que comparten sistemas referenciales.",
      "Existen puntos que comparten sistemas y una función común.",
      "Aún no se evalúan cambios coordinados de trayectoria entre puntos.",
      "Aún no se demuestra dependencia funcional fuerte o acoplamiento dinámico."
    ),
    stringsAsFactors = FALSE
  )

  niveles
}


# ============================================================
# 5. Interpretar madurez ecosistémica
# ============================================================

interpretar_madurez_ecosistemica_adtr <- function(indicadores,
                                                  indice_madurez,
                                                  niveles_relacion) {

  interpretaciones <- character()

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "El índice exploratorio de madurez ecosistémica obtenido es ",
      round(indice_madurez$indice_madurez_ecosistemica, 4),
      ", clasificado como ",
      indice_madurez$nivel_madurez,
      "."
    )
  )

  total_indicadores <- nrow(indicadores)
  indicadores_cumplidos <- sum(indicadores$cumple)

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "Se cumplen ",
      indicadores_cumplidos,
      " de ",
      total_indicadores,
      " indicadores evaluados."
    )
  )

  if (all(indicadores$cumple[indicadores$indicador %in% c(
    "trazabilidad_promedio",
    "presencia_punto_factual",
    "presencia_senales_internas"
  )])) {
    interpretaciones <- c(
      interpretaciones,
      "La estructura conserva trazabilidad suficiente y combina punto factual observado con señales internas transformacionales."
    )
  }

  if (any(indicadores$indicador == "relaciones_sistemico_funcionales" &
          indicadores$cumple)) {
    interpretaciones <- c(
      interpretaciones,
      "La presencia de relaciones sistémico-funcionales permite hablar de una base ecosistémica preliminar, no solo de coexistencia de puntos."
    )
  }

  if (!any(niveles_relacion$tipo == "relación dinámica" &
           niveles_relacion$evidencia_actual)) {
    interpretaciones <- c(
      interpretaciones,
      "Aún no se cuenta con evidencia de relación dinámica coordinada; por tanto, no debe afirmarse un ecosistema dinámico completo."
    )
  }

  if (!any(niveles_relacion$tipo == "acoplamiento interpretado" &
           niveles_relacion$evidencia_actual)) {
    interpretaciones <- c(
      interpretaciones,
      "Tampoco se demuestra todavía acoplamiento interpretado de nivel alto; se requiere análisis adicional de trayectorias, dependencia o coordinación."
    )
  }

  interpretaciones <- c(
    interpretaciones,
    "La lectura correcta es considerar la red como base ecosistémica inicial o preliminar, derivada de resultados existentes y no de una nueva funcionalidad de aprendizaje."
  )

  data.frame(
    numero = seq_along(interpretaciones),
    interpretacion = interpretaciones,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 6. Decisión funcional ecosistémica
# ============================================================

evaluar_decision_funcional_ecosistema_adtr <- function(indice_madurez,
                                                       niveles_relacion) {

  tiene_relacion_dinamica <- any(
    niveles_relacion$tipo == "relación dinámica" &
      niveles_relacion$evidencia_actual
  )

  tiene_acoplamiento <- any(
    niveles_relacion$tipo == "acoplamiento interpretado" &
      niveles_relacion$evidencia_actual
  )

  if (
    indice_madurez$indice_madurez_ecosistemica >= 0.65 &&
    !tiene_relacion_dinamica &&
    !tiene_acoplamiento
  ) {

    decision <- "mantener_como_base_ecosistemica_preliminar"
    justificacion <- paste(
      "La red cumple indicadores de base ecosistémica, pero aún no presenta",
      "evidencia suficiente de relación dinámica coordinada ni acoplamiento interpretado.",
      "No se debe afirmar ecosistema completo."
    )

  } else if (
    indice_madurez$indice_madurez_ecosistemica >= 0.65 &&
    tiene_relacion_dinamica &&
    tiene_acoplamiento
  ) {

    decision <- "posible_ecosistema_dinamico_aprendido"
    justificacion <- paste(
      "La red cumple indicadores de madurez y presenta evidencia de relación dinámica",
      "y acoplamiento. Requiere validación adicional antes de formalizarse."
    )

  } else {

    decision <- "mantener_como_red_transformacional"
    justificacion <- paste(
      "La estructura aún no alcanza madurez ecosistémica suficiente.",
      "Debe mantenerse como red transformacional o lectura conceptual."
    )
  }

  data.frame(
    necesidad = "Evaluar madurez ecosistémica de una red transformacional ADTR",
    decision = decision,
    justificacion = justificacion,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 7. Ejemplo reproducible desde Script 07
# ============================================================

nodos_adtr <- data.frame(
  id = c("eta", "grad_norm", "loss", "y"),
  etiqueta = c("eta", "grad_norm", "loss", "y"),
  filas_matriz = c(3, 3, 3, 5),
  cobertura_referencial = c(3, 3, 3, 4),
  diversidad_representaciones = c(3, 3, 3, 5),
  diversidad_roles = c(3, 3, 3, 5),
  diversidad_funcional = c(3, 3, 3, 5),
  diversidad_naturalezas = c(3, 3, 3, 5),
  indice_trazabilidad = c(1, 1, 1, 1),
  punto_critico = c(TRUE, TRUE, TRUE, TRUE),
  criterio_critico = c(
    "alta cobertura referencial; alta diversidad funcional",
    "alta cobertura referencial; alta diversidad funcional",
    "alta cobertura referencial; alta diversidad funcional",
    "alta cobertura referencial; alta diversidad funcional"
  ),
  tipo_punto = c(
    "senal_interna_transformacional",
    "senal_interna_transformacional",
    "senal_interna_transformacional",
    "punto_factual_observado"
  ),
  stringsAsFactors = FALSE
)

aristas_adtr <- data.frame(
  from = c(
    "loss", "grad_norm", "eta",
    "y", "grad_norm", "eta",
    "y", "loss", "eta",
    "y", "loss", "grad_norm"
  ),
  to = c(
    "y", "y", "y",
    "loss", "loss", "loss",
    "grad_norm", "grad_norm", "grad_norm",
    "eta", "eta", "eta"
  ),
  tipo_relacion = c(
    "relacion_sistemico_funcional",
    "relacion_por_sistema",
    "relacion_por_sistema",
    "relacion_sistemico_funcional",
    "relacion_por_sistema",
    "relacion_por_sistema",
    "relacion_por_sistema",
    "relacion_por_sistema",
    "relacion_por_sistema",
    "relacion_por_sistema",
    "relacion_por_sistema",
    "relacion_por_sistema"
  ),
  sistemas_comunes = rep("S_apr, S_ctrl, S_learned", 12),
  funciones_comunes = c(
    "orientar el ajuste del modelo", "", "",
    "orientar el ajuste del modelo", "", "",
    "", "", "",
    "", "", ""
  ),
  justificacion = c(
    "Los puntos comparten sistemas y la función orientar el ajuste del modelo.",
    "Los puntos comparten sistemas referenciales.",
    "Los puntos comparten sistemas referenciales.",
    "Los puntos comparten sistemas y la función orientar el ajuste del modelo.",
    "Los puntos comparten sistemas referenciales.",
    "Los puntos comparten sistemas referenciales.",
    "Los puntos comparten sistemas referenciales.",
    "Los puntos comparten sistemas referenciales.",
    "Los puntos comparten sistemas referenciales.",
    "Los puntos comparten sistemas referenciales.",
    "Los puntos comparten sistemas referenciales.",
    "Los puntos comparten sistemas referenciales."
  ),
  peso = c(2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1),
  stringsAsFactors = FALSE
)

coleccion_adtr <- data.frame(
  punto = c(
    "y", "y", "y", "y", "y",
    "loss", "loss", "loss",
    "grad_norm", "grad_norm", "grad_norm",
    "eta", "eta", "eta"
  ),
  sistema = c(
    "S_obs", "S_apr", "S_apr", "S_ctrl", "S_learned",
    "S_apr", "S_ctrl", "S_learned",
    "S_apr", "S_ctrl", "S_learned",
    "S_apr", "S_ctrl", "S_learned"
  ),
  representacion = c(
    "preparado$y",
    "unidad$variable_objetivo = y",
    "prediccion",
    "error observado-predicho",
    "comparación observado-predicho",
    "trayectoria$loss",
    "reducción de pérdida",
    "resumen de trayectoria",
    "trayectoria$grad_norm",
    "nivel de presión dinámica",
    "patrón de presión de aprendizaje",
    "trayectoria$eta",
    "secuencia de eta",
    "patrón de política de tasa"
  ),
  rol = c(
    "variable observada",
    "objetivo de aprendizaje",
    "salida estimada",
    "señal de discrepancia",
    "componente aprendido evaluable",
    "medida de error",
    "criterio de control",
    "componente dinámico aprendido",
    "magnitud del gradiente",
    "indicador de estabilidad o inestabilidad",
    "componente dinámico aprendido",
    "tasa de aprendizaje",
    "variable de control",
    "evidencia de estrategia adaptativa"
  ),
  funcion = c(
    "representar el valor factual de referencia",
    "orientar el ajuste del modelo",
    "aproximar la variable observada",
    "evaluar necesidad de corrección",
    "valorar la calidad de la representación aprendida",
    "orientar el ajuste del modelo",
    "evaluar mejora y posible ajuste",
    "caracterizar estabilidad del aprendizaje",
    "representar presión de ajuste",
    "apoyar decisiones de ajuste de tasa o control",
    "caracterizar intensidad y estabilización del aprendizaje",
    "regular la magnitud de actualización",
    "ajustar la dinámica de aprendizaje",
    "caracterizar una política de aprendizaje"
  ),
  naturaleza = c(
    "dato factual observado",
    "referente supervisado",
    "representación aprendida",
    "señal diagnóstica",
    "evidencia de aprendizaje",
    "señal de aprendizaje",
    "señal diagnóstica",
    "evidencia dinámica",
    "señal diferencial del aprendizaje",
    "señal de control potencial",
    "evidencia de dinámica interna",
    "parámetro operativo",
    "componente de control adaptativo",
    "evidencia meta-dinámica"
  ),
  fuente_programatica = c(
    "ajuste_demo$preparado$y",
    "ajuste_demo$unidad$variable_objetivo",
    "ajuste_demo$prediccion",
    "ajuste_demo$preparado$y - ajuste_demo$prediccion",
    "ajuste_demo$metricas",
    "ajuste_demo$trayectoria$loss",
    "ajuste_demo$metricas$reduccion_rel",
    "ajuste_demo$trayectoria",
    "ajuste_demo$trayectoria$grad_norm",
    "ajuste_demo$trayectoria$grad_norm",
    "ajuste_demo$trayectoria",
    "ajuste_demo$trayectoria$eta",
    "ajuste_demo$trayectoria$eta",
    "ajuste_demo$trayectoria"
  ),
  observacion = rep("Salida derivada de matrices ADTR previas.", 14),
  stringsAsFactors = FALSE
)


# ============================================================
# 8. Ejecución del análisis de madurez ecosistémica
# ============================================================

indicadores_madurez <- evaluar_indicadores_madurez_adtr(
  nodos = nodos_adtr,
  aristas = aristas_adtr,
  coleccion_adtr = coleccion_adtr
)

indice_madurez <- calcular_indice_madurez_ecosistemica_adtr(
  indicadores = indicadores_madurez
)

niveles_relacion <- evaluar_niveles_relacion_ecosistemica(
  aristas = aristas_adtr
)

interpretacion_madurez <- interpretar_madurez_ecosistemica_adtr(
  indicadores = indicadores_madurez,
  indice_madurez = indice_madurez,
  niveles_relacion = niveles_relacion
)

decision_funcional <- evaluar_decision_funcional_ecosistema_adtr(
  indice_madurez = indice_madurez,
  niveles_relacion = niveles_relacion
)


# ============================================================
# 9. Exportación de resultados
# ============================================================

write.csv(
  indicadores_madurez,
  file = "outputs/tables/08_indicadores_madurez_ecosistemica_adtr.csv",
  row.names = FALSE
)

write.csv(
  indice_madurez,
  file = "outputs/tables/08_indice_madurez_ecosistemica_adtr.csv",
  row.names = FALSE
)

write.csv(
  niveles_relacion,
  file = "outputs/tables/08_niveles_relacion_ecosistemica_adtr.csv",
  row.names = FALSE
)

write.csv(
  interpretacion_madurez,
  file = "outputs/tables/08_interpretacion_madurez_ecosistemica_adtr.csv",
  row.names = FALSE
)

write.csv(
  decision_funcional,
  file = "outputs/tables/08_decision_funcional_ecosistema_adtr.csv",
  row.names = FALSE
)

resultado_adtr_08 <- list(
  indicadores_madurez = indicadores_madurez,
  indice_madurez = indice_madurez,
  niveles_relacion = niveles_relacion,
  interpretacion_madurez = interpretacion_madurez,
  decision_funcional = decision_funcional
)

saveRDS(
  resultado_adtr_08,
  file = "outputs/results/08_resultado_madurez_ecosistemica_adtr.rds"
)


# ============================================================
# 10. Impresión final
# ============================================================

cat("\n============================================================\n")
cat("Indicadores de madurez ecosistémica:\n")
cat("============================================================\n")
print(indicadores_madurez)

cat("\n============================================================\n")
cat("Índice de madurez ecosistémica:\n")
cat("============================================================\n")
print(indice_madurez)

cat("\n============================================================\n")
cat("Niveles de relación ecosistémica:\n")
cat("============================================================\n")
print(niveles_relacion)

cat("\n============================================================\n")
cat("Interpretación de madurez ecosistémica:\n")
cat("============================================================\n")
print(interpretacion_madurez)

cat("\n============================================================\n")
cat("Decisión funcional:\n")
cat("============================================================\n")
print(decision_funcional)

cat("\n============================================================\n")
cat("Script 08 finalizado correctamente.\n")
cat("Resultados exportados en outputs/tables y outputs/results.\n")
cat("============================================================\n")

