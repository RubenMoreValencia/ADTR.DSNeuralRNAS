# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Archivo: 08_ecosistema_adtr.R
# Propósito: Funciones para evaluar madurez ecosistémica
# preliminar de redes transformacionales ADTR.
# ============================================================

#' Evaluar indicadores de madurez ecosistémica ADTR
#'
#' Evalúa indicadores mínimos para determinar si una red transformacional ADTR
#' puede interpretarse como base ecosistémica preliminar. La función no afirma
#' causalidad ni ecosistema dinámico completo; solo evalúa condiciones
#' estructurales, trazabilidad y presencia de componentes dinámicos.
#'
#' @param nodos data.frame de nodos ADTR. Debe contener `id`, `tipo_punto`,
#'   `indice_trazabilidad`, `cobertura_referencial` y `diversidad_funcional`.
#' @param aristas data.frame de aristas ADTR. Debe contener `from`, `to`,
#'   `tipo_relacion`, `peso` y `justificacion`.
#' @param coleccion_adtr data.frame con la colección multirreferencial ADTR.
#'
#' @return data.frame con indicadores, valores observados, cumplimiento y criterio.
#' @export
evaluar_indicadores_madurez_adtr <- function(nodos, aristas, coleccion_adtr) {
  validar_nodos_adtr(nodos)
  validar_aristas_adtr(aristas)
  validar_coleccion_adtr(coleccion_adtr)

  columnas_nodos <- c(
    "tipo_punto", "indice_trazabilidad",
    "cobertura_referencial", "diversidad_funcional"
  )
  faltantes_nodos <- setdiff(columnas_nodos, names(nodos))
  if (length(faltantes_nodos) > 0) {
    stop("La tabla de nodos no contiene columnas de madurez requeridas: ",
         paste(faltantes_nodos, collapse = ", "))
  }

  n_puntos <- nrow(nodos)
  n_aristas <- nrow(aristas)
  tipos_punto <- unique(nodos$tipo_punto)
  sistemas <- unique(coleccion_adtr$sistema)

  tiene_punto_factual <- any(nodos$tipo_punto == "punto_factual_observado")
  tiene_senales_internas <- any(nodos$tipo_punto == "senal_interna_transformacional")

  tiene_observacion <- "S_obs" %in% sistemas
  tiene_aprendizaje <- "S_apr" %in% sistemas
  tiene_control <- "S_ctrl" %in% sistemas
  tiene_aprendido <- "S_learned" %in% sistemas

  trazabilidad_promedio <- mean(nodos$indice_trazabilidad, na.rm = TRUE)
  diversidad_tipos_punto <- length(tipos_punto)

  relaciones_funcionales <- sum(
    aristas$tipo_relacion == "relacion_sistemico_funcional",
    na.rm = TRUE
  )

  texto_coleccion <- paste(
    coleccion_adtr$punto,
    coleccion_adtr$representacion,
    coleccion_adtr$rol,
    coleccion_adtr$funcion,
    coleccion_adtr$naturaleza,
    collapse = " "
  )

  presencia_componentes_dinamicos <- any(grepl(
    "loss|grad|eta|presion|tasa|dinamic|aprendizaje",
    texto_coleccion,
    ignore.case = TRUE
  ))

  presencia_control <- any(grepl(
    "control|correccion|ajuste de tasa|variable de control|senal diagnostica|señal diagnóstica",
    texto_coleccion,
    ignore.case = TRUE
  ))

  presencia_evidencia_aprendida <- any(grepl(
    "aprendida|aprendizaje|evidencia|aprendido",
    texto_coleccion,
    ignore.case = TRUE
  ))

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
    valor = as.character(c(
      n_puntos,
      round(trazabilidad_promedio, 6),
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
    )),
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

#' Calcular índice de madurez ecosistémica ADTR
#'
#' Calcula un índice exploratorio de madurez ecosistémica a partir de indicadores
#' lógicos. Los pesos se normalizan internamente para garantizar que el índice
#' quede acotado entre 0 y 1.
#'
#' @param indicadores data.frame generado por `evaluar_indicadores_madurez_adtr()`.
#' @param pesos vector numérico opcional con nombres de indicadores. Si es `NULL`,
#'   se usa una ponderación base normalizada.
#'
#' @return data.frame con índice, nivel de madurez y observación.
#' @export
calcular_indice_madurez_ecosistemica_adtr <- function(indicadores, pesos = NULL) {
  if (!all(c("indicador", "cumple") %in% names(indicadores))) {
    stop("La tabla de indicadores debe contener `indicador` y `cumple`.")
  }

  indicadores_base <- c(
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
  )

  if (is.null(pesos)) {
    pesos <- c(
      multiplicidad_de_puntos = 0.08,
      trazabilidad_promedio = 0.12,
      diversidad_de_tipos_de_punto = 0.08,
      presencia_punto_factual = 0.08,
      presencia_senales_internas = 0.08,
      presencia_sistema_observacion = 0.06,
      presencia_sistema_aprendizaje = 0.08,
      presencia_sistema_control = 0.08,
      presencia_sistema_aprendido = 0.08,
      relaciones_entre_puntos = 0.06,
      relaciones_sistemico_funcionales = 0.06,
      presencia_componentes_dinamicos = 0.08,
      presencia_control = 0.08,
      presencia_evidencia_aprendida = 0.06
    )
  }

  faltantes_pesos <- setdiff(indicadores_base, names(pesos))
  if (length(faltantes_pesos) > 0) {
    stop("Faltan pesos para los indicadores: ", paste(faltantes_pesos, collapse = ", "))
  }

  pesos <- pesos[indicadores_base]
  pesos <- pesos / sum(pesos)

  tabla <- merge(
    indicadores,
    data.frame(indicador = names(pesos), peso = as.numeric(pesos), stringsAsFactors = FALSE),
    by = "indicador",
    all.x = TRUE
  )

  tabla$puntaje <- ifelse(tabla$cumple, tabla$peso, 0)
  indice <- sum(tabla$puntaje, na.rm = TRUE)
  indice <- max(0, min(1, indice))

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

#' Evaluar niveles de relación ecosistémica ADTR
#'
#' Construye una escala relacional de cinco niveles para indicar hasta dónde llega
#' la evidencia disponible en una red transformacional ADTR.
#'
#' @param aristas data.frame de aristas ADTR.
#'
#' @return data.frame con niveles, tipo de relación, evidencia actual y observación.
#' @export
evaluar_niveles_relacion_ecosistemica <- function(aristas) {
  validar_aristas_adtr(aristas)

  data.frame(
    nivel = c("Nivel 1", "Nivel 2", "Nivel 3", "Nivel 4", "Nivel 5"),
    tipo = c(
      "coexistencia referencial",
      "relacion por sistema",
      "relacion funcional",
      "relacion dinamica",
      "acoplamiento interpretado"
    ),
    evidencia_actual = c(
      TRUE,
      any(aristas$tipo_relacion == "relacion_por_sistema"),
      any(aristas$tipo_relacion == "relacion_sistemico_funcional"),
      any(aristas$tipo_relacion %in% c("relacion_dinamica_coordinada", "patron_dinamico_conjunto")),
      any(aristas$tipo_relacion %in% c("acoplamiento_interpretado", "acoplamiento_interpretado_fuerte", "acoplamiento_interpretado_moderado"))
    ),
    observacion = c(
      "Los puntos pertenecen al mismo conjunto ADTR.",
      "Existen puntos que comparten sistemas referenciales.",
      "Existen puntos que comparten sistemas y una función común.",
      "Se requiere evidencia de cambios coordinados de trayectoria entre puntos.",
      "Se requiere evidencia de dependencia funcional fuerte o acoplamiento dinámico de nivel alto."
    ),
    stringsAsFactors = FALSE
  )
}

#' Interpretar madurez ecosistémica ADTR
#'
#' Genera una interpretación textual prudente de los indicadores, índice de
#' madurez y niveles relacionales. La interpretación distingue entre base
#' ecosistémica preliminar, evidencia dinámica y acoplamiento interpretado.
#'
#' @param indicadores data.frame de indicadores de madurez.
#' @param indice_madurez data.frame generado por `calcular_indice_madurez_ecosistemica_adtr()`.
#' @param niveles_relacion data.frame generado por `evaluar_niveles_relacion_ecosistemica()`.
#'
#' @return data.frame con interpretaciones numeradas.
#' @export
interpretar_madurez_ecosistemica_adtr <- function(indicadores,
                                                  indice_madurez,
                                                  niveles_relacion) {
  interpretaciones <- character()

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "El índice exploratorio de madurez ecosistémica obtenido es ",
      round(indice_madurez$indice_madurez_ecosistemica, 4),
      ", clasificado como ", indice_madurez$nivel_madurez, "."
    )
  )

  total_indicadores <- nrow(indicadores)
  indicadores_cumplidos <- sum(indicadores$cumple)

  interpretaciones <- c(
    interpretaciones,
    paste0("Se cumplen ", indicadores_cumplidos, " de ", total_indicadores, " indicadores evaluados.")
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

  if (any(indicadores$indicador == "relaciones_sistemico_funcionales" & indicadores$cumple)) {
    interpretaciones <- c(
      interpretaciones,
      "La presencia de relaciones sistémico-funcionales permite hablar de una base ecosistémica preliminar, no solo de coexistencia de puntos."
    )
  }

  tiene_nivel4 <- any(niveles_relacion$tipo == "relacion dinamica" & niveles_relacion$evidencia_actual)
  tiene_nivel5 <- any(niveles_relacion$tipo == "acoplamiento interpretado" & niveles_relacion$evidencia_actual)

  if (!tiene_nivel4) {
    interpretaciones <- c(
      interpretaciones,
      "Aún no se cuenta con evidencia de relación dinámica coordinada; por tanto, no debe afirmarse un ecosistema dinámico completo."
    )
  }

  if (!tiene_nivel5) {
    interpretaciones <- c(
      interpretaciones,
      "Tampoco se demuestra todavía acoplamiento interpretado de Nivel 5; se requiere análisis adicional de trayectorias, dependencia o coordinación."
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

#' Evaluar decisión funcional ecosistémica ADTR
#'
#' Determina una decisión funcional prudente a partir del índice de madurez y los
#' niveles relacionales. La decisión evita afirmar ecosistema completo cuando no
#' existe evidencia de relación dinámica o acoplamiento interpretado.
#'
#' @param indice_madurez data.frame con índice de madurez ecosistémica.
#' @param niveles_relacion data.frame con escala relacional.
#'
#' @return data.frame con necesidad, decisión y justificación.
#' @export
evaluar_decision_funcional_ecosistema_adtr <- function(indice_madurez,
                                                       niveles_relacion) {
  tiene_relacion_dinamica <- any(
    niveles_relacion$tipo == "relacion dinamica" & niveles_relacion$evidencia_actual
  )

  tiene_acoplamiento <- any(
    niveles_relacion$tipo == "acoplamiento interpretado" & niveles_relacion$evidencia_actual
  )

  indice <- indice_madurez$indice_madurez_ecosistemica[1]

  if (indice >= 0.65 && !tiene_relacion_dinamica && !tiene_acoplamiento) {
    decision <- "mantener_como_base_ecosistemica_preliminar"
    justificacion <- paste(
      "La red cumple indicadores de base ecosistémica, pero aún no presenta",
      "evidencia suficiente de relación dinámica coordinada ni acoplamiento interpretado.",
      "No se debe afirmar ecosistema completo."
    )
  } else if (indice >= 0.65 && tiene_relacion_dinamica && !tiene_acoplamiento) {
    decision <- "base_ecosistemica_con_evidencia_dinamica"
    justificacion <- paste(
      "La red cumple indicadores de base ecosistémica y presenta evidencia dinámica,",
      "pero aún no demuestra acoplamiento interpretado de Nivel 5."
    )
  } else if (indice >= 0.65 && tiene_relacion_dinamica && tiene_acoplamiento) {
    decision <- "posible_ecosistema_dinamico_aprendido_exploratorio"
    justificacion <- paste(
      "La red cumple indicadores de madurez, relación dinámica y acoplamiento interpretado.",
      "Debe mantenerse como evidencia exploratoria, no como causalidad demostrada."
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

#' Analizar madurez ecosistémica ADTR
#'
#' Ejecuta el flujo completo de evaluación ecosistémica: indicadores, índice
#' normalizado, niveles relacionales, interpretación y decisión funcional.
#'
#' @param nodos data.frame de nodos ADTR.
#' @param aristas data.frame de aristas ADTR.
#' @param coleccion_adtr data.frame de colección multirreferencial.
#' @param pesos vector opcional de pesos por indicador.
#'
#' @return lista con indicadores, índice, niveles, interpretación y decisión.
#' @export
analizar_madurez_ecosistemica_adtr <- function(nodos,
                                               aristas,
                                               coleccion_adtr,
                                               pesos = NULL) {
  indicadores <- evaluar_indicadores_madurez_adtr(
    nodos = nodos,
    aristas = aristas,
    coleccion_adtr = coleccion_adtr
  )

  indice_madurez <- calcular_indice_madurez_ecosistemica_adtr(
    indicadores = indicadores,
    pesos = pesos
  )

  niveles_relacion <- evaluar_niveles_relacion_ecosistemica(aristas)

  interpretacion <- interpretar_madurez_ecosistemica_adtr(
    indicadores = indicadores,
    indice_madurez = indice_madurez,
    niveles_relacion = niveles_relacion
  )

  decision_funcional <- evaluar_decision_funcional_ecosistema_adtr(
    indice_madurez = indice_madurez,
    niveles_relacion = niveles_relacion
  )

  list(
    indicadores = indicadores,
    indice_madurez = indice_madurez,
    niveles_relacion = niveles_relacion,
    interpretacion = interpretacion,
    decision_funcional = decision_funcional
  )
}
