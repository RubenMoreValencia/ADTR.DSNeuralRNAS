# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Archivo: 03_matrices_adtr.R
# Propósito: Construcción de matrices multirreferenciales ADTR.
# ============================================================

#' Construir una matriz ADTR genérica
#'
#' Construye una matriz multirreferencial ADTR a partir de vectores de igual
#' longitud que describen el punto, sistema, representación, rol, función,
#' naturaleza, fuente programática y observación. Esta función no entrena
#' modelos ni calcula predicciones; solo organiza evidencia referencial ya
#' disponible.
#'
#' @param punto Vector de puntos referenciales o valor único reciclable.
#' @param sistema Vector de sistemas referenciales.
#' @param representacion Vector de representaciones del punto.
#' @param rol Vector de roles interpretativos.
#' @param funcion Vector de funciones asociadas.
#' @param naturaleza Vector de naturalezas sistémicas.
#' @param fuente_programatica Vector de fuentes programáticas trazables.
#' @param observacion Vector de observaciones interpretativas. Por defecto `NA`.
#'
#' @return Un `data.frame` con estructura ADTR mínima.
#' @examples
#' construir_matriz_adtr(
#'   punto = "loss",
#'   sistema = c("S_apr", "S_ctrl"),
#'   representacion = c("trayectoria$loss", "reduccion_loss"),
#'   rol = c("medida de error", "criterio de control"),
#'   funcion = c("orientar ajuste", "evaluar correccion"),
#'   naturaleza = c("senal de aprendizaje", "senal diagnostica"),
#'   fuente_programatica = c("obj$trayectoria$loss", "obj$metricas")
#' )
#' @export
construir_matriz_adtr <- function(
    punto,
    sistema,
    representacion,
    rol,
    funcion,
    naturaleza,
    fuente_programatica,
    observacion = NA_character_
) {
  n <- max(
    length(punto), length(sistema), length(representacion),
    length(rol), length(funcion), length(naturaleza),
    length(fuente_programatica), length(observacion)
  )

  reciclar <- function(x) {
    if (length(x) == n) return(x)
    if (length(x) == 1) return(rep(x, n))
    stop("Los argumentos deben tener longitud 1 o la misma longitud máxima.")
  }

  matriz <- data.frame(
    punto = reciclar(punto),
    sistema = reciclar(sistema),
    representacion = reciclar(representacion),
    rol = reciclar(rol),
    funcion = reciclar(funcion),
    naturaleza = reciclar(naturaleza),
    fuente_programatica = reciclar(fuente_programatica),
    observacion = reciclar(observacion),
    stringsAsFactors = FALSE
  )

  if (exists("validar_matriz_adtr", mode = "function")) {
    validar_matriz_adtr(matriz)
  }

  matriz
}

#' Construir matriz ADTR para la pérdida
#'
#' Construye una matriz ADTR estándar para el punto `loss`, proyectándolo en
#' sistema de aprendizaje, sistema de control y sistema aprendido.
#'
#' @param nombre_objeto Nombre simbólico del objeto de aprendizaje usado para
#'   documentar fuentes programáticas.
#'
#' @return Un `data.frame` con tres filas ADTR para `loss`.
#' @export
construir_matriz_loss_adtr <- function(nombre_objeto = "ajuste") {
  construir_matriz_adtr(
    punto = "loss",
    sistema = c("S_apr", "S_ctrl", "S_learned"),
    representacion = c(
      "trayectoria$loss",
      "reduccion de perdida",
      "resumen de trayectoria"
    ),
    rol = c(
      "medida de error",
      "criterio de control",
      "componente dinamico aprendido"
    ),
    funcion = c(
      "orientar el ajuste del modelo",
      "evaluar mejora y posible ajuste",
      "caracterizar estabilidad del aprendizaje"
    ),
    naturaleza = c(
      "senal de aprendizaje",
      "senal diagnostica",
      "evidencia dinamica"
    ),
    fuente_programatica = c(
      paste0(nombre_objeto, "$trayectoria$loss"),
      paste0(nombre_objeto, "$metricas$reduccion_rel"),
      paste0(nombre_objeto, "$trayectoria")
    ),
    observacion = c(
      "La perdida registra la tension de ajuste durante el entrenamiento.",
      "La reduccion de perdida puede orientar decisiones de control.",
      "La trayectoria de perdida permite interpretar estabilidad o regimen."
    )
  )
}

#' Construir matriz ADTR para la norma del gradiente
#'
#' Construye una matriz ADTR estándar para el punto `grad_norm`, proyectándolo
#' como presión de ajuste, señal de control potencial y evidencia dinámica.
#'
#' @param nombre_objeto Nombre simbólico del objeto de aprendizaje.
#'
#' @return Un `data.frame` con tres filas ADTR para `grad_norm`.
#' @export
construir_matriz_grad_norm_adtr <- function(nombre_objeto = "ajuste") {
  construir_matriz_adtr(
    punto = "grad_norm",
    sistema = c("S_apr", "S_ctrl", "S_learned"),
    representacion = c(
      "trayectoria$grad_norm",
      "nivel de presion dinamica",
      "patron de presion de aprendizaje"
    ),
    rol = c(
      "magnitud del gradiente",
      "indicador de estabilidad o inestabilidad",
      "componente dinamico aprendido"
    ),
    funcion = c(
      "representar presion de ajuste",
      "apoyar decisiones de ajuste de tasa o control",
      "caracterizar intensidad y estabilizacion del aprendizaje"
    ),
    naturaleza = c(
      "senal diferencial del aprendizaje",
      "senal de control potencial",
      "evidencia de dinamica interna"
    ),
    fuente_programatica = c(
      paste0(nombre_objeto, "$trayectoria$grad_norm"),
      paste0(nombre_objeto, "$trayectoria$grad_norm"),
      paste0(nombre_objeto, "$trayectoria")
    ),
    observacion = c(
      "La norma del gradiente resume la intensidad de cambio.",
      "Valores persistentes pueden interpretarse como presion de ajuste.",
      "La trayectoria de grad_norm puede contribuir a identificar regimenes."
    )
  )
}

#' Construir matriz ADTR para la tasa de aprendizaje
#'
#' Construye una matriz ADTR estándar para el punto `eta`, proyectándolo como
#' parámetro operativo, variable de control y evidencia meta-dinámica.
#'
#' @param nombre_objeto Nombre simbólico del objeto de aprendizaje.
#'
#' @return Un `data.frame` con tres filas ADTR para `eta`.
#' @export
construir_matriz_eta_adtr <- function(nombre_objeto = "ajuste") {
  construir_matriz_adtr(
    punto = "eta",
    sistema = c("S_apr", "S_ctrl", "S_learned"),
    representacion = c(
      "trayectoria$eta",
      "secuencia de eta",
      "patron de politica de tasa"
    ),
    rol = c(
      "tasa de aprendizaje",
      "variable de control",
      "evidencia de estrategia adaptativa"
    ),
    funcion = c(
      "regular la magnitud de actualizacion",
      "ajustar la dinamica de aprendizaje",
      "caracterizar una politica de aprendizaje"
    ),
    naturaleza = c(
      "parametro operativo",
      "componente de control adaptativo",
      "evidencia meta-dinamica"
    ),
    fuente_programatica = c(
      paste0(nombre_objeto, "$trayectoria$eta"),
      paste0(nombre_objeto, "$trayectoria$eta"),
      paste0(nombre_objeto, "$trayectoria")
    ),
    observacion = c(
      "Eta regula la intensidad de actualizacion.",
      "Cambios en eta pueden interpretarse como acciones de control.",
      "La secuencia de eta puede leerse como patron adaptativo."
    )
  )
}

#' Construir matriz ADTR para una trayectoria de aprendizaje
#'
#' Construye matrices ADTR para `loss`, `grad_norm` y `eta` cuando la
#' trayectoria contiene dichas columnas. La función solo organiza salidas ya
#' existentes del objeto o trayectoria.
#'
#' @param trayectoria Data frame con columnas `loss`, `grad_norm` y `eta`.
#' @param nombre_objeto Nombre simbólico del objeto de aprendizaje.
#'
#' @return Un `data.frame` que une las matrices de `loss`, `grad_norm` y `eta`.
#' @export
construir_matriz_trayectoria_adtr <- function(trayectoria, nombre_objeto = "ajuste") {
  if (exists("validar_trayectoria_adtr", mode = "function")) {
    validar_trayectoria_adtr(trayectoria)
  } else {
    requeridas <- c("loss", "grad_norm", "eta")
    faltantes <- setdiff(requeridas, names(trayectoria))
    if (length(faltantes) > 0) {
      stop("La trayectoria no contiene: ", paste(faltantes, collapse = ", "))
    }
  }

  unir_matrices_adtr(list(
    construir_matriz_loss_adtr(nombre_objeto),
    construir_matriz_grad_norm_adtr(nombre_objeto),
    construir_matriz_eta_adtr(nombre_objeto)
  ))
}

#' Construir matriz ADTR para una variable aprendida
#'
#' Construye una matriz ADTR para la variable objetivo de un objeto de
#' aprendizaje. La variable se proyecta como dato observado, objetivo de
#' aprendizaje, salida estimada, señal de discrepancia y evidencia aprendida.
#'
#' @param objeto Objeto de aprendizaje con componentes `unidad`, `preparado`,
#'   `prediccion` y `metricas`.
#' @param nombre_objeto Nombre simbólico del objeto usado en fuentes.
#'
#' @return Un `data.frame` con la matriz ADTR de la variable aprendida.
#' @export
construir_matriz_variable_aprendida_adtr <- function(objeto, nombre_objeto = "ajuste") {
  if (!is.list(objeto)) {
    stop("El objeto debe ser una lista o estructura compatible.")
  }

  requeridos <- c("unidad", "preparado", "prediccion", "metricas")
  faltantes <- setdiff(requeridos, names(objeto))
  if (length(faltantes) > 0) {
    stop("El objeto no contiene componentes requeridos: ", paste(faltantes, collapse = ", "))
  }

  if (is.null(objeto$unidad$variable_objetivo)) {
    stop("El objeto no contiene unidad$variable_objetivo.")
  }

  variable_objetivo <- objeto$unidad$variable_objetivo

  if (!is.data.frame(objeto$preparado)) {
    stop("El componente preparado debe ser un data.frame.")
  }

  if (!variable_objetivo %in% names(objeto$preparado)) {
    stop("La variable objetivo no existe en preparado.")
  }

  if (length(objeto$prediccion) != nrow(objeto$preparado)) {
    stop("La longitud de prediccion no coincide con las filas de preparado.")
  }

  construir_matriz_adtr(
    punto = variable_objetivo,
    sistema = c("S_obs", "S_apr", "S_apr", "S_ctrl", "S_learned"),
    representacion = c(
      paste0("preparado$", variable_objetivo),
      paste0("unidad$variable_objetivo = ", variable_objetivo),
      "prediccion",
      "error observado-predicho",
      "comparacion observado-predicho"
    ),
    rol = c(
      "variable observada",
      "objetivo de aprendizaje",
      "salida estimada",
      "senal de discrepancia",
      "componente aprendido evaluable"
    ),
    funcion = c(
      "representar el valor factual de referencia",
      "orientar el ajuste del modelo",
      "aproximar la variable observada",
      "evaluar necesidad de correccion",
      "valorar la calidad de la representacion aprendida"
    ),
    naturaleza = c(
      "dato factual observado",
      "referente supervisado",
      "representacion aprendida",
      "senal diagnostica",
      "evidencia de aprendizaje"
    ),
    fuente_programatica = c(
      paste0(nombre_objeto, "$preparado$", variable_objetivo),
      paste0(nombre_objeto, "$unidad$variable_objetivo"),
      paste0(nombre_objeto, "$prediccion"),
      paste0(nombre_objeto, "$preparado$", variable_objetivo, " - ", nombre_objeto, "$prediccion"),
      paste0(nombre_objeto, "$metricas")
    ),
    observacion = c(
      "La variable objetivo aparece como dato observado.",
      "La variable objetivo define el referente del aprendizaje.",
      "La prediccion representa la forma aprendida de la variable objetivo.",
      "La diferencia observado-predicho puede orientar revision del ajuste.",
      "La variable aprendida se evalua mediante metricas y comparacion."
    )
  )
}

#' Unir matrices ADTR
#'
#' Une una lista de matrices ADTR en una sola colección tabular. Todas las
#' matrices deben cumplir la estructura mínima ADTR.
#'
#' @param lista_matrices Lista de data frames ADTR.
#'
#' @return Un `data.frame` con todas las matrices unidas.
#' @export
unir_matrices_adtr <- function(lista_matrices) {
  if (!is.list(lista_matrices) || length(lista_matrices) == 0) {
    stop("Debe proporcionar una lista no vacía de matrices ADTR.")
  }

  for (i in seq_along(lista_matrices)) {
    if (exists("validar_matriz_adtr", mode = "function")) {
      validar_matriz_adtr(lista_matrices[[i]])
    }
  }

  resultado <- do.call(rbind, lista_matrices)
  rownames(resultado) <- NULL
  resultado
}

#' Construir matriz ADTR desde objeto de aprendizaje
#'
#' Construye una matriz ADTR integrada mínima desde un objeto de aprendizaje,
#' combinando la variable aprendida y las señales internas de trayectoria si
#' están disponibles.
#'
#' @param objeto Objeto de aprendizaje con variable objetivo, predicción y
#'   trayectoria.
#' @param nombre_objeto Nombre simbólico usado para documentar fuentes.
#'
#' @return Un `data.frame` con matrices ADTR integradas.
#' @export
construir_matrices_desde_objeto_adtr <- function(objeto, nombre_objeto = "ajuste") {
  matrices <- list()

  matrices[["variable_aprendida"]] <- construir_matriz_variable_aprendida_adtr(
    objeto = objeto,
    nombre_objeto = nombre_objeto
  )

  if ("trayectoria" %in% names(objeto) && is.data.frame(objeto$trayectoria)) {
    requeridas <- c("loss", "grad_norm", "eta")
    if (all(requeridas %in% names(objeto$trayectoria))) {
      matrices[["trayectoria"]] <- construir_matriz_trayectoria_adtr(
        trayectoria = objeto$trayectoria,
        nombre_objeto = nombre_objeto
      )
    }
  }

  unir_matrices_adtr(matrices)
}
