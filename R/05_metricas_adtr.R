# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Archivo: 05_metricas_adtr.R
# Propósito:
#   Funciones para calcular métricas referenciales,
#   transformacionales, integradas e índices exploratorios ADTR.
# ============================================================

#' Calcular métricas referenciales ADTR para una matriz M(p)
#'
#' Calcula cobertura referencial, diversidad de representaciones,
#' diversidad de roles, diversidad funcional, diversidad de naturalezas
#' e índice de trazabilidad para una matriz multirreferencial ADTR.
#'
#' @param matriz_adtr data.frame con columnas mínimas: `punto`, `sistema`,
#'   `representacion`, `rol`, `funcion`, `naturaleza` y `fuente_programatica`.
#'
#' @return data.frame con métricas referenciales para el punto evaluado.
#' @export
calcular_metricas_referenciales_adtr <- function(matriz_adtr) {
  validar_matriz_adtr(matriz_adtr)
  
  puntos <- unique(matriz_adtr$punto)
  
  resultados <- lapply(puntos, function(p) {
    sub <- matriz_adtr[matriz_adtr$punto == p, , drop = FALSE]
    
    data.frame(
      punto = p,
      filas_matriz = nrow(sub),
      cobertura_referencial = length(unique(sub$sistema)),
      diversidad_representaciones = length(unique(sub$representacion)),
      diversidad_roles = length(unique(sub$rol)),
      diversidad_funcional = length(unique(sub$funcion)),
      diversidad_naturalezas = length(unique(sub$naturaleza)),
      indice_trazabilidad = mean(
        !is.na(sub$fuente_programatica) & sub$fuente_programatica != ""
      ),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, resultados)
}

#' Integrar métricas referenciales y transformacionales ADTR
#'
#' Une las métricas provenientes de M(p) y K(p) en una sola tabla integrada.
#'
#' @param metricas_referenciales data.frame generado por
#'   `calcular_metricas_referenciales_adtr()`.
#' @param metricas_transformacionales data.frame generado por
#'   `calcular_metricas_transformacionales_adtr()`.
#'
#' @return data.frame con métricas integradas por punto.
#' @export
integrar_metricas_adtr <- function(metricas_referenciales,
                                   metricas_transformacionales) {
  if (!"punto" %in% names(metricas_referenciales)) {
    stop("Las métricas referenciales deben contener la columna `punto`.")
  }
  
  if (!"punto" %in% names(metricas_transformacionales)) {
    stop("Las métricas transformacionales deben contener la columna `punto`.")
  }
  
  merge(
    metricas_referenciales,
    metricas_transformacionales,
    by = "punto",
    all = TRUE
  )
}

#' Calcular índice exploratorio de complejidad ADTR
#'
#' Calcula un índice exploratorio de complejidad ADTR a partir de métricas
#' integradas. Este índice no debe interpretarse como medida universal;
#' sirve para comparación interna entre puntos o casos evaluados.
#'
#' @param metricas_integradas data.frame con métricas referenciales y
#'   transformacionales integradas.
#' @param pesos named numeric opcional con pesos para los componentes:
#'   `cobertura`, `roles`, `funcional`, `no_reducibilidad`, `emergencia`,
#'   `trazabilidad`. Si no se proporciona, se usan pesos por defecto.
#' @param normalizar_pesos lógico. Si TRUE, normaliza los pesos para que sumen 1.
#'
#' @return data.frame con índice exploratorio de complejidad ADTR.
#' @export
calcular_indice_complejidad_adtr <- function(metricas_integradas,
                                             pesos = NULL,
                                             normalizar_pesos = TRUE) {
  columnas_requeridas <- c(
    "punto",
    "cobertura_referencial",
    "diversidad_roles",
    "diversidad_funcional",
    "indice_trazabilidad",
    "indice_no_reducibilidad_total",
    "indice_emergencia_funcional"
  )
  
  faltantes <- setdiff(columnas_requeridas, names(metricas_integradas))
  
  if (length(faltantes) > 0) {
    stop(
      "Las métricas integradas no contienen las columnas requeridas: ",
      paste(faltantes, collapse = ", ")
    )
  }
  
  if (is.null(pesos)) {
    pesos <- c(
      cobertura = 0.15,
      roles = 0.20,
      funcional = 0.20,
      no_reducibilidad = 0.20,
      emergencia = 0.15,
      trazabilidad = 0.10
    )
  }
  
  requeridos_pesos <- c(
    "cobertura", "roles", "funcional",
    "no_reducibilidad", "emergencia", "trazabilidad"
  )
  
  faltantes_pesos <- setdiff(requeridos_pesos, names(pesos))
  
  if (length(faltantes_pesos) > 0) {
    stop("Faltan pesos requeridos: ", paste(faltantes_pesos, collapse = ", "))
  }
  
  pesos <- pesos[requeridos_pesos]
  
  if (any(is.na(pesos)) || any(pesos < 0)) {
    stop("Los pesos deben ser numéricos, no negativos y no NA.")
  }
  
  if (sum(pesos) == 0) {
    stop("La suma de pesos no puede ser cero.")
  }
  
  if (isTRUE(normalizar_pesos)) {
    pesos <- pesos / sum(pesos)
  }
  
  normalizar_col <- function(x) {
    max_x <- max(x, na.rm = TRUE)
    if (!is.finite(max_x) || max_x == 0) {
      return(rep(0, length(x)))
    }
    x / max_x
  }
  
  cobertura_norm <- normalizar_col(metricas_integradas$cobertura_referencial)
  roles_norm <- normalizar_col(metricas_integradas$diversidad_roles)
  funcional_norm <- normalizar_col(metricas_integradas$diversidad_funcional)
  
  indice <-
    pesos[["cobertura"]] * cobertura_norm +
    pesos[["roles"]] * roles_norm +
    pesos[["funcional"]] * funcional_norm +
    pesos[["no_reducibilidad"]] * metricas_integradas$indice_no_reducibilidad_total +
    pesos[["emergencia"]] * metricas_integradas$indice_emergencia_funcional +
    pesos[["trazabilidad"]] * metricas_integradas$indice_trazabilidad
  
  data.frame(
    punto = metricas_integradas$punto,
    indice_complejidad_adtr = indice,
    observacion_indice = "Índice exploratorio; interpretar junto con M(p), K(p) y trazabilidad.",
    stringsAsFactors = FALSE
  )
}

#' Interpretar métricas integradas ADTR
#'
#' Genera una interpretación textual prudente a partir de métricas integradas
#' e índice exploratorio de complejidad ADTR.
#'
#' @param metricas_integradas data.frame con una o más filas de métricas integradas.
#' @param indice_complejidad data.frame generado por
#'   `calcular_indice_complejidad_adtr()`.
#'
#' @return data.frame con interpretaciones por punto.
#' @export
interpretar_metricas_integradas_adtr <- function(metricas_integradas,
                                                 indice_complejidad) {
  if (!"punto" %in% names(metricas_integradas)) {
    stop("Las métricas integradas deben contener `punto`.")
  }
  
  if (!all(c("punto", "indice_complejidad_adtr") %in% names(indice_complejidad))) {
    stop("El índice de complejidad debe contener `punto` e `indice_complejidad_adtr`.")
  }
  
  resultados <- list()
  puntos <- unique(metricas_integradas$punto)
  
  for (i in seq_along(puntos)) {
    p <- puntos[i]
    m <- metricas_integradas[metricas_integradas$punto == p, , drop = FALSE][1, ]
    idx <- indice_complejidad[indice_complejidad$punto == p, , drop = FALSE][1, ]
    
    interpretaciones <- character()
    
    if (!is.na(m$indice_trazabilidad) && m$indice_trazabilidad == 1) {
      interpretaciones <- c(
        interpretaciones,
        paste0("El punto ", p, " presenta trazabilidad completa hacia fuentes programáticas.")
      )
    }
    
    if (!is.na(m$cobertura_referencial) && m$cobertura_referencial >= 4) {
      interpretaciones <- c(
        interpretaciones,
        paste0("El punto ", p, " posee cobertura referencial amplia.")
      )
    }
    
    if (!is.na(m$diversidad_funcional) && m$diversidad_funcional >= 4) {
      interpretaciones <- c(
        interpretaciones,
        paste0("El punto ", p, " presenta alta diversidad funcional.")
      )
    }
    
    if (!is.na(m$indice_no_reducibilidad_total) && m$indice_no_reducibilidad_total >= 0.25) {
      interpretaciones <- c(
        interpretaciones,
        paste0("El punto ", p, " presenta no reducibilidad total relevante.")
      )
    }
    
    if (!is.na(m$indice_emergencia_funcional) && m$indice_emergencia_funcional >= 0.10) {
      interpretaciones <- c(
        interpretaciones,
        paste0("El punto ", p, " presenta evidencia inicial de emergencia funcional.")
      )
    }
    
    if (!is.na(m$indice_relacion_indeterminada) && m$indice_relacion_indeterminada == 0) {
      interpretaciones <- c(
        interpretaciones,
        paste0("El punto ", p, " no presenta relaciones indeterminadas en K(p).")
      )
    }
    
    interpretaciones <- c(
      interpretaciones,
      paste0(
        "El índice exploratorio de complejidad ADTR para ",
        p,
        " es ",
        round(idx$indice_complejidad_adtr, 4),
        "."
      )
    )
    
    resultados[[i]] <- data.frame(
      punto = p,
      numero = seq_along(interpretaciones),
      interpretacion = interpretaciones,
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, resultados)
}

#' Analizar métricas integradas ADTR desde una matriz M(p)
#'
#' Ejecuta un flujo funcional completo para un punto: calcula métricas
#' referenciales, construye K(p), calcula métricas transformacionales,
#' integra métricas, calcula índice exploratorio e interpreta resultados.
#'
#' @param matriz_adtr data.frame con matriz multirreferencial de un único punto.
#' @param pesos pesos opcionales para el índice de complejidad.
#'
#' @return lista con matriz de transformaciones, métricas e interpretación.
#' @export
analizar_metricas_integradas_adtr <- function(matriz_adtr,
                                              pesos = NULL) {
  validar_matriz_adtr(matriz_adtr)
  
  if (length(unique(matriz_adtr$punto)) != 1) {
    stop("`analizar_metricas_integradas_adtr()` requiere una matriz de un único punto.")
  }
  
  metricas_referenciales <- calcular_metricas_referenciales_adtr(matriz_adtr)
  
  analisis_transformaciones <- analizar_transformaciones_adtr(matriz_adtr)
  
  metricas_integradas <- integrar_metricas_adtr(
    metricas_referenciales,
    analisis_transformaciones$metricas_transformacionales
  )
  
  indice_complejidad <- calcular_indice_complejidad_adtr(
    metricas_integradas,
    pesos = pesos
  )
  
  interpretacion_integrada <- interpretar_metricas_integradas_adtr(
    metricas_integradas,
    indice_complejidad
  )
  
  list(
    metricas_referenciales = metricas_referenciales,
    matriz_transformaciones = analisis_transformaciones$matriz_transformaciones,
    resumen_transformaciones = analisis_transformaciones$resumen_transformaciones,
    metricas_transformacionales = analisis_transformaciones$metricas_transformacionales,
    metricas_integradas = metricas_integradas,
    indice_complejidad = indice_complejidad,
    interpretacion_integrada = interpretacion_integrada
  )
}
