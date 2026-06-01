# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Archivo: R/06_conjuntos_adtr.R
# Propósito: funciones para integrar múltiples matrices ADTR en
# colecciones multirreferenciales y analizar puntos en conjunto.
# ============================================================

#' Calcular métricas referenciales por punto en una colección ADTR
#'
#' Calcula cobertura referencial, diversidad de representaciones, roles,
#' funciones, naturalezas e índice de trazabilidad para cada punto contenido
#' en una colección multirreferencial \eqn{\mathcal{M}(P)}.
#'
#' @param coleccion_adtr `data.frame` con matrices ADTR unidas. Debe contener
#'   las columnas `punto`, `sistema`, `representacion`, `rol`, `funcion`,
#'   `naturaleza` y `fuente_programatica`.
#'
#' @return `data.frame` con métricas por punto.
#' @export
#'
#' @examples
#' # metricas <- calcular_metricas_por_punto_adtr(coleccion_adtr)
calcular_metricas_por_punto_adtr <- function(coleccion_adtr) {
  validar_coleccion_adtr(coleccion_adtr)

  puntos <- unique(coleccion_adtr$punto)
  resultados <- vector("list", length(puntos))

  for (i in seq_along(puntos)) {
    p <- puntos[i]
    sub <- coleccion_adtr[coleccion_adtr$punto == p, , drop = FALSE]

    resultados[[i]] <- data.frame(
      punto = p,
      filas_matriz = nrow(sub),
      cobertura_referencial = length(unique(sub$sistema)),
      diversidad_representaciones = length(unique(sub$representacion)),
      diversidad_roles = length(unique(sub$rol)),
      diversidad_funcional = length(unique(sub$funcion)),
      diversidad_naturalezas = length(unique(sub$naturaleza)),
      indice_trazabilidad = mean(!is.na(sub$fuente_programatica) & sub$fuente_programatica != ""),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, resultados)
}

#' Identificar puntos críticos ADTR
#'
#' Marca puntos críticos según cobertura referencial, diversidad funcional o
#' trazabilidad baja. La criticidad no implica importancia absoluta; solo
#' prioriza revisión interpretativa dentro del conjunto evaluado.
#'
#' @param metricas_puntos `data.frame` generado por
#'   [calcular_metricas_por_punto_adtr()].
#' @param umbral_cobertura Valor mínimo para considerar cobertura alta.
#' @param umbral_diversidad_funcional Valor mínimo para diversidad funcional alta.
#' @param umbral_trazabilidad_baja Valor bajo el cual se marca trazabilidad baja.
#'
#' @return `data.frame` con columnas adicionales `punto_critico` y
#'   `criterio_critico`.
#' @export
identificar_puntos_criticos_adtr <- function(metricas_puntos,
                                             umbral_cobertura = 3,
                                             umbral_diversidad_funcional = 3,
                                             umbral_trazabilidad_baja = 0.80) {
  requeridas <- c("punto", "cobertura_referencial", "diversidad_funcional", "indice_trazabilidad")
  adtr_validar_columnas(metricas_puntos, requeridas, "metricas_puntos")

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

#' Agrupar puntos por sistema referencial
#'
#' Resume qué puntos aparecen en cada sistema referencial de una colección ADTR.
#'
#' @param coleccion_adtr `data.frame` con una colección multirreferencial.
#'
#' @return `data.frame` con sistemas, puntos y número de puntos.
#' @export
agrupar_puntos_por_sistema_adtr <- function(coleccion_adtr) {
  validar_coleccion_adtr(coleccion_adtr)

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
  resultado[order(resultado$sistema), , drop = FALSE]
}

#' Agrupar puntos por función ADTR
#'
#' Resume qué puntos comparten una misma función interpretativa dentro de una
#' colección ADTR.
#'
#' @param coleccion_adtr `data.frame` con una colección multirreferencial.
#'
#' @return `data.frame` con funciones, puntos y número de puntos.
#' @export
agrupar_puntos_por_funcion_adtr <- function(coleccion_adtr) {
  validar_coleccion_adtr(coleccion_adtr)

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
  resultado[order(-resultado$numero_puntos), , drop = FALSE]
}

#' Construir relaciones preliminares entre puntos ADTR
#'
#' Construye relaciones dirigidas entre puntos según sistemas comunes y funciones
#' comunes. Estas relaciones son transformacionales o referenciales, no causales.
#'
#' @param coleccion_adtr `data.frame` con una colección multirreferencial.
#'
#' @return `data.frame` con origen, destino, sistemas comunes, funciones comunes,
#'   tipo de relación y justificación.
#' @export
construir_relaciones_puntos_adtr <- function(coleccion_adtr) {
  validar_coleccion_adtr(coleccion_adtr)
  puntos <- unique(coleccion_adtr$punto)

  if (length(puntos) < 2) {
    stop("Se requieren al menos dos puntos para construir relaciones.")
  }

  pares <- expand.grid(
    punto_origen = puntos,
    punto_destino = puntos,
    stringsAsFactors = FALSE
  )
  pares <- pares[pares$punto_origen != pares$punto_destino, , drop = FALSE]

  relaciones <- vector("list", nrow(pares))

  for (i in seq_len(nrow(pares))) {
    p1 <- pares$punto_origen[i]
    p2 <- pares$punto_destino[i]

    sub1 <- coleccion_adtr[coleccion_adtr$punto == p1, , drop = FALSE]
    sub2 <- coleccion_adtr[coleccion_adtr$punto == p2, , drop = FALSE]

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

#' Interpretar un conjunto multirreferencial ADTR
#'
#' Genera una interpretación textual breve del conjunto, sus puntos críticos,
#' cobertura promedio, trazabilidad promedio y tipos de relaciones.
#'
#' @param metricas_puntos Métricas por punto.
#' @param puntos_criticos Tabla de puntos críticos.
#' @param relaciones_puntos Relaciones entre puntos.
#'
#' @return `data.frame` con interpretaciones numeradas.
#' @export
interpretar_conjunto_adtr <- function(metricas_puntos,
                                      puntos_criticos,
                                      relaciones_puntos) {
  adtr_validar_columnas(metricas_puntos, c("punto", "cobertura_referencial", "indice_trazabilidad"), "metricas_puntos")
  adtr_validar_columnas(puntos_criticos, c("punto", "punto_critico"), "puntos_criticos")
  adtr_validar_columnas(relaciones_puntos, c("tipo_relacion"), "relaciones_puntos")

  interpretaciones <- character()
  n_puntos <- nrow(metricas_puntos)
  n_criticos <- sum(puntos_criticos$punto_critico)

  interpretaciones <- c(interpretaciones, paste0("El conjunto ADTR evaluado contiene ", n_puntos, " puntos referenciales."))
  interpretaciones <- c(interpretaciones, paste0("Se identificaron ", n_criticos, " puntos críticos según cobertura, diversidad funcional o trazabilidad."))

  promedio_cobertura <- mean(metricas_puntos$cobertura_referencial)
  promedio_trazabilidad <- mean(metricas_puntos$indice_trazabilidad)

  interpretaciones <- c(interpretaciones, paste0("La cobertura referencial promedio del conjunto es ", round(promedio_cobertura, 3), "."))
  interpretaciones <- c(interpretaciones, paste0("El índice promedio de trazabilidad es ", round(promedio_trazabilidad, 3), "."))

  if (promedio_trazabilidad == 1) {
    interpretaciones <- c(interpretaciones, "Todas las matrices del conjunto mantienen trazabilidad completa hacia fuentes programáticas.")
  } else {
    interpretaciones <- c(interpretaciones, "Existen puntos con trazabilidad incompleta; se requiere revisión antes de escalar.")
  }

  tipos_relacion <- table(relaciones_puntos$tipo_relacion)
  interpretaciones <- c(
    interpretaciones,
    paste0("Las relaciones entre puntos muestran los siguientes tipos: ", paste(names(tipos_relacion), tipos_relacion, sep = "=", collapse = "; "), ".")
  )

  data.frame(numero = seq_along(interpretaciones), interpretacion = interpretaciones, stringsAsFactors = FALSE)
}

#' Analizar un conjunto multirreferencial ADTR
#'
#' Ejecuta el flujo completo para conjuntos: métricas por punto, puntos críticos,
#' agrupaciones por sistema y función, relaciones entre puntos e interpretación.
#'
#' @param coleccion_adtr `data.frame` con una colección multirreferencial.
#' @param umbral_cobertura Umbral para criticidad por cobertura.
#' @param umbral_diversidad_funcional Umbral para criticidad por diversidad funcional.
#' @param umbral_trazabilidad_baja Umbral para trazabilidad baja.
#'
#' @return Lista con resultados del análisis de conjunto.
#' @export
analizar_conjunto_adtr <- function(coleccion_adtr,
                                   umbral_cobertura = 3,
                                   umbral_diversidad_funcional = 3,
                                   umbral_trazabilidad_baja = 0.80) {
  metricas_puntos <- calcular_metricas_por_punto_adtr(coleccion_adtr)

  puntos_criticos <- identificar_puntos_criticos_adtr(
    metricas_puntos,
    umbral_cobertura = umbral_cobertura,
    umbral_diversidad_funcional = umbral_diversidad_funcional,
    umbral_trazabilidad_baja = umbral_trazabilidad_baja
  )

  agrupacion_sistemas <- agrupar_puntos_por_sistema_adtr(coleccion_adtr)
  agrupacion_funciones <- agrupar_puntos_por_funcion_adtr(coleccion_adtr)
  relaciones_puntos <- construir_relaciones_puntos_adtr(coleccion_adtr)

  interpretacion <- interpretar_conjunto_adtr(
    metricas_puntos = metricas_puntos,
    puntos_criticos = puntos_criticos,
    relaciones_puntos = relaciones_puntos
  )

  list(
    coleccion_adtr = coleccion_adtr,
    metricas_puntos = metricas_puntos,
    puntos_criticos = puntos_criticos,
    agrupacion_sistemas = agrupacion_sistemas,
    agrupacion_funciones = agrupacion_funciones,
    relaciones_puntos = relaciones_puntos,
    interpretacion = interpretacion
  )
}
