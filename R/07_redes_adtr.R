# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Archivo: 07_redes_adtr.R
# Propósito: Funciones para construir y analizar redes
# transformacionales ADTR a partir de colecciones y relaciones.
# ============================================================

#' Construir nodos de una red transformacional ADTR
#'
#' Construye una tabla de nodos a partir de una colección multirreferencial ADTR.
#' Opcionalmente incorpora métricas por punto y criterios de criticidad.
#'
#' @param coleccion_adtr data.frame con una colección ADTR. Debe contener al menos
#'   las columnas `punto`, `sistema`, `representacion`, `rol`, `funcion`,
#'   `naturaleza` y `fuente_programatica`.
#' @param metricas_puntos data.frame opcional con métricas por punto.
#' @param puntos_criticos data.frame opcional con columnas `punto`,
#'   `punto_critico` y `criterio_critico`.
#'
#' @return data.frame con nodos, atributos, métricas y tipo general de punto.
#' @export
construir_nodos_adtr <- function(coleccion_adtr,
                                 metricas_puntos = NULL,
                                 puntos_criticos = NULL) {
  validar_coleccion_adtr(coleccion_adtr)

  puntos <- unique(coleccion_adtr$punto)

  nodos <- data.frame(
    id = puntos,
    etiqueta = puntos,
    stringsAsFactors = FALSE
  )

  if (!is.null(metricas_puntos)) {
    if (!all(c("punto") %in% names(metricas_puntos))) {
      stop("`metricas_puntos` debe contener la columna `punto`.")
    }

    nodos <- merge(
      nodos,
      metricas_puntos,
      by.x = "id",
      by.y = "punto",
      all.x = TRUE
    )
  }

  if (!is.null(puntos_criticos)) {
    columnas_crit <- c("punto", "punto_critico", "criterio_critico")
    faltantes <- setdiff(columnas_crit, names(puntos_criticos))
    if (length(faltantes) > 0) {
      stop("`puntos_criticos` no contiene columnas requeridas: ",
           paste(faltantes, collapse = ", "))
    }

    crit <- puntos_criticos[, columnas_crit]

    # Evitar duplicar columnas si ya vienen desde metricas_puntos/puntos_criticos
    columnas_a_remover <- intersect(c("punto_critico", "criterio_critico"), names(nodos))
    if (length(columnas_a_remover) > 0) {
      nodos <- nodos[, setdiff(names(nodos), columnas_a_remover), drop = FALSE]
    }

    nodos <- merge(
      nodos,
      crit,
      by.x = "id",
      by.y = "punto",
      all.x = TRUE
    )
  }

  nodos$tipo_punto <- NA_character_

  for (i in seq_len(nrow(nodos))) {
    p <- nodos$id[i]
    sub <- coleccion_adtr[coleccion_adtr$punto == p, ]
    sistemas <- unique(sub$sistema)

    if ("S_obs" %in% sistemas) {
      nodos$tipo_punto[i] <- "punto_factual_observado"
    } else if ("S_ctrl" %in% sistemas && "S_learned" %in% sistemas) {
      nodos$tipo_punto[i] <- "senal_interna_transformacional"
    } else {
      nodos$tipo_punto[i] <- "punto_referencial_interno"
    }
  }

  nodos
}

#' Construir aristas de una red transformacional ADTR
#'
#' Convierte una tabla de relaciones entre puntos en una tabla de aristas
#' dirigida, agregando un peso interpretativo según el tipo de relación.
#'
#' @param relaciones_puntos data.frame con columnas `punto_origen`,
#'   `punto_destino`, `tipo_relacion` y `justificacion`.
#'
#' @return data.frame con aristas `from`, `to`, tipo, atributos y peso.
#' @export
construir_aristas_adtr <- function(relaciones_puntos) {
  columnas_requeridas <- c(
    "punto_origen", "punto_destino", "tipo_relacion", "justificacion"
  )
  faltantes <- setdiff(columnas_requeridas, names(relaciones_puntos))
  if (length(faltantes) > 0) {
    stop("La tabla de relaciones no contiene columnas requeridas: ",
         paste(faltantes, collapse = ", "))
  }

  if (nrow(relaciones_puntos) < 1) {
    stop("La tabla de relaciones no contiene filas.")
  }

  aristas <- data.frame(
    from = relaciones_puntos$punto_origen,
    to = relaciones_puntos$punto_destino,
    tipo_relacion = relaciones_puntos$tipo_relacion,
    sistemas_comunes = if ("sistemas_comunes" %in% names(relaciones_puntos)) {
      relaciones_puntos$sistemas_comunes
    } else {
      NA_character_
    },
    funciones_comunes = if ("funciones_comunes" %in% names(relaciones_puntos)) {
      relaciones_puntos$funciones_comunes
    } else {
      NA_character_
    },
    justificacion = relaciones_puntos$justificacion,
    stringsAsFactors = FALSE
  )

  aristas$peso <- ifelse(
    aristas$tipo_relacion == "relacion_sistemico_funcional", 2,
    ifelse(aristas$tipo_relacion == "relacion_por_sistema", 1, 0.5)
  )

  aristas
}

#' Calcular métricas básicas de una red ADTR
#'
#' Calcula grados de entrada, salida, grado total, pesos y centralidad
#' normalizada para cada nodo.
#'
#' @param nodos data.frame de nodos construido con [construir_nodos_adtr()].
#' @param aristas data.frame de aristas construido con [construir_aristas_adtr()].
#'
#' @return data.frame con métricas de red por punto.
#' @export
calcular_metricas_red_adtr <- function(nodos, aristas) {
  validar_nodos_adtr(nodos)
  validar_aristas_adtr(aristas)
  validar_red_adtr(nodos, aristas)

  puntos <- nodos$id

  grado_salida <- sapply(puntos, function(p) sum(aristas$from == p))
  grado_entrada <- sapply(puntos, function(p) sum(aristas$to == p))
  grado_total <- grado_salida + grado_entrada

  peso_salida <- sapply(puntos, function(p) sum(aristas$peso[aristas$from == p]))
  peso_entrada <- sapply(puntos, function(p) sum(aristas$peso[aristas$to == p]))
  peso_total <- peso_salida + peso_entrada

  metricas <- data.frame(
    punto = puntos,
    grado_salida = as.numeric(grado_salida),
    grado_entrada = as.numeric(grado_entrada),
    grado_total = as.numeric(grado_total),
    peso_salida = as.numeric(peso_salida),
    peso_entrada = as.numeric(peso_entrada),
    peso_total = as.numeric(peso_total),
    stringsAsFactors = FALSE
  )

  metricas$centralidad_grado <- if (max(metricas$grado_total, na.rm = TRUE) == 0) {
    0
  } else {
    metricas$grado_total / max(metricas$grado_total, na.rm = TRUE)
  }

  metricas$centralidad_peso <- if (max(metricas$peso_total, na.rm = TRUE) == 0) {
    0
  } else {
    metricas$peso_total / max(metricas$peso_total, na.rm = TRUE)
  }

  metricas
}

#' Construir matriz de adyacencia ADTR
#'
#' Construye una matriz de adyacencia dirigida a partir de nodos y aristas.
#'
#' @param nodos data.frame de nodos.
#' @param aristas data.frame de aristas.
#' @param usar_pesos lógico. Si TRUE, usa la columna `peso`; si FALSE, usa 1.
#'
#' @return data.frame con la matriz de adyacencia.
#' @export
construir_matriz_adyacencia_adtr <- function(nodos, aristas, usar_pesos = TRUE) {
  validar_nodos_adtr(nodos)
  validar_aristas_adtr(aristas)
  validar_red_adtr(nodos, aristas)

  puntos <- nodos$id

  matriz <- matrix(
    0,
    nrow = length(puntos),
    ncol = length(puntos),
    dimnames = list(puntos, puntos)
  )

  for (i in seq_len(nrow(aristas))) {
    origen <- aristas$from[i]
    destino <- aristas$to[i]
    valor <- if (usar_pesos) aristas$peso[i] else 1
    matriz[origen, destino] <- matriz[origen, destino] + valor
  }

  as.data.frame(matriz)
}

#' Resumir tipos de relación de una red ADTR
#'
#' Resume las frecuencias y proporciones de los tipos de relación presentes
#' en una tabla de aristas.
#'
#' @param aristas data.frame de aristas.
#'
#' @return data.frame con tipo de relación, frecuencia y proporción.
#' @export
resumir_tipos_relacion_red_adtr <- function(aristas) {
  validar_aristas_adtr(aristas)

  resumen <- as.data.frame(table(aristas$tipo_relacion))
  names(resumen) <- c("tipo_relacion", "frecuencia")
  resumen$proporcion <- resumen$frecuencia / sum(resumen$frecuencia)
  resumen
}

#' Identificar nodos centrales ADTR
#'
#' Marca como centrales los nodos cuya centralidad ponderada alcanza un umbral.
#'
#' @param metricas_red data.frame con métricas de red.
#' @param umbral_centralidad valor numérico entre 0 y 1.
#'
#' @return data.frame con columna `nodo_central` y criterio textual.
#' @export
identificar_nodos_centrales_adtr <- function(metricas_red,
                                             umbral_centralidad = 0.80) {
  columnas_requeridas <- c("punto", "centralidad_peso")
  faltantes <- setdiff(columnas_requeridas, names(metricas_red))
  if (length(faltantes) > 0) {
    stop("`metricas_red` no contiene columnas requeridas: ",
         paste(faltantes, collapse = ", "))
  }

  validar_indice_unitario_adtr(umbral_centralidad, "umbral_centralidad")

  metricas_red$nodo_central <- metricas_red$centralidad_peso >= umbral_centralidad

  metricas_red$criterio_centralidad <- ifelse(
    metricas_red$nodo_central,
    "alta centralidad ponderada en la red ADTR",
    "centralidad moderada o baja"
  )

  metricas_red
}

#' Interpretar una red transformacional ADTR
#'
#' Genera una interpretación textual breve a partir de nodos, aristas,
#' métricas, resumen de relaciones y nodos centrales.
#'
#' @param nodos data.frame de nodos.
#' @param aristas data.frame de aristas.
#' @param metricas_red data.frame con métricas de red.
#' @param resumen_relaciones data.frame con resumen de tipos de relación.
#' @param nodos_centrales data.frame de nodos centrales.
#'
#' @return data.frame con interpretaciones numeradas.
#' @export
interpretar_red_adtr <- function(nodos,
                                 aristas,
                                 metricas_red,
                                 resumen_relaciones,
                                 nodos_centrales) {
  interpretaciones <- character()

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "La red transformacional ADTR contiene ",
      nrow(nodos),
      " nodos y ",
      nrow(aristas),
      " aristas dirigidas."
    )
  )

  tipos <- paste(
    resumen_relaciones$tipo_relacion,
    resumen_relaciones$frecuencia,
    sep = "=",
    collapse = "; "
  )

  interpretaciones <- c(
    interpretaciones,
    paste0("Los tipos de relación observados son: ", tipos, ".")
  )

  n_centrales <- sum(nodos_centrales$nodo_central)

  interpretaciones <- c(
    interpretaciones,
    paste0("Se identificaron ", n_centrales,
           " nodos centrales según centralidad ponderada.")
  )

  if (n_centrales > 0) {
    nombres_centrales <- paste(
      nodos_centrales$punto[nodos_centrales$nodo_central],
      collapse = ", "
    )

    interpretaciones <- c(
      interpretaciones,
      paste0("Los nodos centrales son: ", nombres_centrales, ".")
    )
  }

  if (any(aristas$tipo_relacion == "relacion_sistemico_funcional")) {
    interpretaciones <- c(
      interpretaciones,
      "La red contiene relaciones sistémico-funcionales; algunos puntos comparten sistemas y funciones, no solo coexistencia referencial."
    )
  }

  if (any(nodos$tipo_punto == "punto_factual_observado") &&
      any(nodos$tipo_punto == "senal_interna_transformacional")) {
    interpretaciones <- c(
      interpretaciones,
      "La red combina al menos un punto factual observado con señales internas transformacionales."
    )
  }

  data.frame(
    numero = seq_along(interpretaciones),
    interpretacion = interpretaciones,
    stringsAsFactors = FALSE
  )
}

#' Graficar una red ADTR con gráficos base
#'
#' Genera una visualización circular simple de la red transformacional ADTR
#' sin depender de paquetes externos.
#'
#' @param nodos data.frame de nodos.
#' @param aristas data.frame de aristas.
#' @param archivo ruta del archivo PNG de salida.
#'
#' @return data.frame invisible con coordenadas de los nodos.
#' @export
graficar_red_adtr_base <- function(nodos,
                                   aristas,
                                   archivo = "outputs/figures/red_transformacional_adtr.png") {
  validar_nodos_adtr(nodos)
  validar_aristas_adtr(aristas)
  validar_red_adtr(nodos, aristas)

  dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)

  puntos <- nodos$id
  n <- length(puntos)
  angulos <- seq(0, 2 * pi, length.out = n + 1)[1:n]

  coords <- data.frame(
    punto = puntos,
    x = cos(angulos),
    y = sin(angulos),
    stringsAsFactors = FALSE
  )

  png(filename = archivo, width = 1200, height = 900)

  plot(
    coords$x,
    coords$y,
    type = "n",
    xlim = c(-1.4, 1.4),
    ylim = c(-1.4, 1.4),
    xlab = "",
    ylab = "",
    axes = FALSE,
    main = "Red transformacional ADTR"
  )

  for (i in seq_len(nrow(aristas))) {
    origen <- coords[coords$punto == aristas$from[i], ]
    destino <- coords[coords$punto == aristas$to[i], ]

    arrows(
      origen$x, origen$y,
      destino$x, destino$y,
      length = 0.08,
      lwd = aristas$peso[i]
    )
  }

  points(coords$x, coords$y, pch = 21, cex = 4, lwd = 2)
  text(coords$x, coords$y, labels = coords$punto, cex = 1.1, font = 2)

  dev.off()

  invisible(coords)
}

#' Analizar una red transformacional ADTR
#'
#' Ejecuta el flujo completo de red: nodos, aristas, métricas,
#' adyacencia, resumen, nodos centrales e interpretación.
#'
#' @param coleccion_adtr data.frame de colección multirreferencial.
#' @param relaciones_puntos data.frame de relaciones entre puntos.
#' @param metricas_puntos data.frame opcional con métricas por punto.
#' @param puntos_criticos data.frame opcional con puntos críticos.
#' @param umbral_centralidad valor entre 0 y 1 para marcar nodos centrales.
#' @param graficar lógico. Si TRUE, genera figura PNG.
#' @param archivo_figura ruta del archivo de figura si `graficar = TRUE`.
#'
#' @return lista con nodos, aristas, métricas, matriz, resumen, centrales,
#'   interpretación y coordenadas opcionales.
#' @export
analizar_red_adtr <- function(coleccion_adtr,
                              relaciones_puntos,
                              metricas_puntos = NULL,
                              puntos_criticos = NULL,
                              umbral_centralidad = 0.80,
                              graficar = FALSE,
                              archivo_figura = "outputs/figures/red_transformacional_adtr.png") {
  nodos <- construir_nodos_adtr(
    coleccion_adtr = coleccion_adtr,
    metricas_puntos = metricas_puntos,
    puntos_criticos = puntos_criticos
  )

  aristas <- construir_aristas_adtr(relaciones_puntos)

  metricas_red <- calcular_metricas_red_adtr(nodos, aristas)

  matriz_adyacencia <- construir_matriz_adyacencia_adtr(
    nodos,
    aristas,
    usar_pesos = TRUE
  )

  resumen_relaciones <- resumir_tipos_relacion_red_adtr(aristas)

  nodos_centrales <- identificar_nodos_centrales_adtr(
    metricas_red,
    umbral_centralidad = umbral_centralidad
  )

  interpretacion <- interpretar_red_adtr(
    nodos = nodos,
    aristas = aristas,
    metricas_red = metricas_red,
    resumen_relaciones = resumen_relaciones,
    nodos_centrales = nodos_centrales
  )

  coords <- NULL
  if (isTRUE(graficar)) {
    coords <- graficar_red_adtr_base(nodos, aristas, archivo = archivo_figura)
  }

  list(
    nodos = nodos,
    aristas = aristas,
    metricas_red = metricas_red,
    matriz_adyacencia = matriz_adyacencia,
    resumen_relaciones = resumen_relaciones,
    nodos_centrales = nodos_centrales,
    interpretacion = interpretacion,
    coords = coords
  )
}
