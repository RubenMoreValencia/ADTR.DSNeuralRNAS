
# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: 07_red_transformacional_adtr.R
# Autor: Rubén Alexander More Valencia
# Propósito:
#   Construir una red transformacional ADTR a partir de una
#   colección multirreferencial de puntos y sus relaciones.
#
#   Este script convierte puntos ADTR en nodos y relaciones
#   entre puntos en aristas. No entrena modelos ni recalcula
#   predicciones. Trabaja sobre matrices y relaciones ya
#   derivadas de salidas existentes.
# ============================================================


# ============================================================
# 0. Configuración general
# ============================================================

cat("\n============================================================\n")
cat("ADTR.DSNeuralRNAS - Red transformacional ADTR\n")
cat("============================================================\n")

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. Validaciones básicas
# ============================================================

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

  if (length(unique(coleccion_adtr$punto)) < 2) {
    stop("Se requieren al menos dos puntos para construir una red.")
  }

  invisible(TRUE)
}


validar_relaciones_adtr <- function(relaciones_puntos) {

  columnas_requeridas <- c(
    "punto_origen", "punto_destino",
    "tipo_relacion", "justificacion"
  )

  faltantes <- setdiff(columnas_requeridas, names(relaciones_puntos))

  if (length(faltantes) > 0) {
    stop(
      "La tabla de relaciones no contiene las columnas requeridas: ",
      paste(faltantes, collapse = ", ")
    )
  }

  if (nrow(relaciones_puntos) < 1) {
    stop("La tabla de relaciones no contiene filas.")
  }

  invisible(TRUE)
}


# ============================================================
# 2. Construir tabla de nodos
# ============================================================

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

  # Agregar métricas si se proporcionan
  if (!is.null(metricas_puntos)) {
    nodos <- merge(
      nodos,
      metricas_puntos,
      by.x = "id",
      by.y = "punto",
      all.x = TRUE
    )
  }

  # Agregar criticidad si se proporciona
  if (!is.null(puntos_criticos)) {

    crit <- puntos_criticos[, c("punto", "punto_critico", "criterio_critico")]

    nodos <- merge(
      nodos,
      crit,
      by.x = "id",
      by.y = "punto",
      all.x = TRUE
    )
  }

  # Tipo general de punto según presencia en sistemas
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


# ============================================================
# 3. Construir tabla de aristas
# ============================================================

construir_aristas_adtr <- function(relaciones_puntos) {

  validar_relaciones_adtr(relaciones_puntos)

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

  # Peso interpretativo de la relación
  aristas$peso <- ifelse(
    aristas$tipo_relacion == "relacion_sistemico_funcional", 2,
    ifelse(aristas$tipo_relacion == "relacion_por_sistema", 1, 0.5)
  )

  aristas
}


# ============================================================
# 4. Métricas de red sin paquetes externos
# ============================================================

calcular_metricas_red_adtr <- function(nodos, aristas) {

  puntos <- nodos$id

  grado_salida <- sapply(
    puntos,
    function(p) sum(aristas$from == p)
  )

  grado_entrada <- sapply(
    puntos,
    function(p) sum(aristas$to == p)
  )

  grado_total <- grado_salida + grado_entrada

  peso_salida <- sapply(
    puntos,
    function(p) sum(aristas$peso[aristas$from == p])
  )

  peso_entrada <- sapply(
    puntos,
    function(p) sum(aristas$peso[aristas$to == p])
  )

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

  metricas$centralidad_grado <- metricas$grado_total /
    max(metricas$grado_total, na.rm = TRUE)

  metricas$centralidad_peso <- metricas$peso_total /
    max(metricas$peso_total, na.rm = TRUE)

  metricas
}


# ============================================================
# 5. Matriz de adyacencia
# ============================================================

construir_matriz_adyacencia_adtr <- function(nodos, aristas,
                                             usar_pesos = TRUE) {

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


# ============================================================
# 6. Resumen de tipos de relación
# ============================================================

resumir_tipos_relacion_red_adtr <- function(aristas) {

  resumen <- as.data.frame(table(aristas$tipo_relacion))
  names(resumen) <- c("tipo_relacion", "frecuencia")
  resumen$proporcion <- resumen$frecuencia / sum(resumen$frecuencia)
  resumen
}


# ============================================================
# 7. Identificar nodos centrales
# ============================================================

identificar_nodos_centrales_adtr <- function(metricas_red,
                                             umbral_centralidad = 0.80) {

  metricas_red$nodo_central <- metricas_red$centralidad_peso >= umbral_centralidad

  metricas_red$criterio_centralidad <- ifelse(
    metricas_red$nodo_central,
    "alta centralidad ponderada en la red ADTR",
    "centralidad moderada o baja"
  )

  metricas_red
}


# ============================================================
# 8. Interpretación de red ADTR
# ============================================================

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
    paste0(
      "Los tipos de relación observados son: ",
      tipos,
      "."
    )
  )

  n_centrales <- sum(nodos_centrales$nodo_central)

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "Se identificaron ",
      n_centrales,
      " nodos centrales según centralidad ponderada."
    )
  )

  if (n_centrales > 0) {

    nombres_centrales <- paste(
      nodos_centrales$punto[nodos_centrales$nodo_central],
      collapse = ", "
    )

    interpretaciones <- c(
      interpretaciones,
      paste0(
        "Los nodos centrales son: ",
        nombres_centrales,
        "."
      )
    )
  }

  if (any(aristas$tipo_relacion == "relacion_sistemico_funcional")) {

    interpretaciones <- c(
      interpretaciones,
      "La red contiene relaciones sistémico-funcionales, lo que indica que algunos puntos comparten sistemas y funciones, no solo coexistencia referencial."
    )
  }

  if (all(nodos$tipo_punto %in% c(
    "punto_factual_observado",
    "senal_interna_transformacional",
    "punto_referencial_interno"
  ))) {

    interpretaciones <- c(
      interpretaciones,
      "La red combina al menos un punto factual observado con señales internas transformacionales, permitiendo conectar dato, aprendizaje, control y evidencia aprendida."
    )
  }

  data.frame(
    numero = seq_along(interpretaciones),
    interpretacion = interpretaciones,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 9. Decisión funcional
# ============================================================

evaluar_decision_funcional_red_adtr <- function(
    puede_derivarse = TRUE,
    utilidad_aplicada = TRUE,
    escalamiento = TRUE
) {

  if (isTRUE(puede_derivarse) && isTRUE(utilidad_aplicada) && isTRUE(escalamiento)) {

    decision <- "posible_funcionalidad_auxiliar_de_red"
    justificacion <- paste(
      "La red transformacional puede derivarse de colecciones ADTR existentes.",
      "Por escalamiento, visualización y análisis de relaciones, podría justificarse",
      "una función auxiliar para construir nodos, aristas, métricas y matrices de adyacencia."
    )

  } else if (isTRUE(puede_derivarse)) {

    decision <- "derivar_de_salidas_existentes"
    justificacion <- "La red puede mantenerse como tabla de nodos y aristas derivada de salidas existentes."

  } else {

    decision <- "mantener_como_lectura_conceptual"
    justificacion <- "No existe evidencia suficiente para proponer funcionalidad auxiliar de red."
  }

  data.frame(
    necesidad = "Construir red transformacional ADTR desde puntos y relaciones existentes",
    puede_derivarse = puede_derivarse,
    utilidad_aplicada = utilidad_aplicada,
    escalamiento = escalamiento,
    decision = decision,
    justificacion = justificacion,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 10. Visualización base sin paquetes externos
# ============================================================

graficar_red_adtr_base <- function(nodos, aristas,
                                   archivo = "outputs/figures/07_red_transformacional_adtr.png") {

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
    coords$x, coords$y,
    type = "n",
    xlim = c(-1.4, 1.4),
    ylim = c(-1.4, 1.4),
    xlab = "",
    ylab = "",
    axes = FALSE,
    main = "Red transformacional ADTR"
  )

  # Dibujar aristas
  for (i in seq_len(nrow(aristas))) {

    origen <- coords[coords$punto == aristas$from[i], ]
    destino <- coords[coords$punto == aristas$to[i], ]

    lwd_val <- aristas$peso[i]

    arrows(
      origen$x, origen$y,
      destino$x, destino$y,
      length = 0.08,
      lwd = lwd_val
    )
  }

  # Dibujar nodos
  points(
    coords$x,
    coords$y,
    pch = 21,
    cex = 4,
    lwd = 2
  )

  text(
    coords$x,
    coords$y,
    labels = coords$punto,
    cex = 1.1,
    font = 2
  )

  dev.off()

  invisible(coords)
}


# ============================================================
# 11. Ejemplo mínimo reproducible desde Script 06
# ============================================================

# Métricas por punto del conjunto multirreferencial
metricas_puntos <- data.frame(
  punto = c("y", "loss", "grad_norm", "eta"),
  filas_matriz = c(5, 3, 3, 3),
  cobertura_referencial = c(4, 3, 3, 3),
  diversidad_representaciones = c(5, 3, 3, 3),
  diversidad_roles = c(5, 3, 3, 3),
  diversidad_funcional = c(5, 3, 3, 3),
  diversidad_naturalezas = c(5, 3, 3, 3),
  indice_trazabilidad = c(1, 1, 1, 1),
  stringsAsFactors = FALSE
)

puntos_criticos <- data.frame(
  punto = c("y", "loss", "grad_norm", "eta"),
  punto_critico = c(TRUE, TRUE, TRUE, TRUE),
  criterio_critico = c(
    "alta cobertura referencial; alta diversidad funcional",
    "alta cobertura referencial; alta diversidad funcional",
    "alta cobertura referencial; alta diversidad funcional",
    "alta cobertura referencial; alta diversidad funcional"
  ),
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
  stringsAsFactors = FALSE
)

relaciones_puntos <- data.frame(
  punto_origen = c(
    "loss", "grad_norm", "eta",
    "y", "grad_norm", "eta",
    "y", "loss", "eta",
    "y", "loss", "grad_norm"
  ),
  punto_destino = c(
    "y", "y", "y",
    "loss", "loss", "loss",
    "grad_norm", "grad_norm", "grad_norm",
    "eta", "eta", "eta"
  ),
  sistemas_comunes = rep("S_apr, S_ctrl, S_learned", 12),
  funciones_comunes = c(
    "orientar el ajuste del modelo", "", "",
    "orientar el ajuste del modelo", "", "",
    "", "", "",
    "", "", ""
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
  stringsAsFactors = FALSE
)


# ============================================================
# 12. Ejecución del análisis de red
# ============================================================

nodos_adtr <- construir_nodos_adtr(
  coleccion_adtr = coleccion_adtr,
  metricas_puntos = metricas_puntos,
  puntos_criticos = puntos_criticos
)

aristas_adtr <- construir_aristas_adtr(
  relaciones_puntos = relaciones_puntos
)

metricas_red <- calcular_metricas_red_adtr(
  nodos = nodos_adtr,
  aristas = aristas_adtr
)

matriz_adyacencia <- construir_matriz_adyacencia_adtr(
  nodos = nodos_adtr,
  aristas = aristas_adtr,
  usar_pesos = TRUE
)

resumen_relaciones <- resumir_tipos_relacion_red_adtr(
  aristas_adtr
)

nodos_centrales <- identificar_nodos_centrales_adtr(
  metricas_red,
  umbral_centralidad = 0.80
)

interpretacion_red <- interpretar_red_adtr(
  nodos = nodos_adtr,
  aristas = aristas_adtr,
  metricas_red = metricas_red,
  resumen_relaciones = resumen_relaciones,
  nodos_centrales = nodos_centrales
)

decision_funcional <- evaluar_decision_funcional_red_adtr(
  puede_derivarse = TRUE,
  utilidad_aplicada = TRUE,
  escalamiento = TRUE
)

coords_red <- graficar_red_adtr_base(
  nodos = nodos_adtr,
  aristas = aristas_adtr,
  archivo = "outputs/figures/07_red_transformacional_adtr.png"
)


# ============================================================
# 13. Exportación de resultados
# ============================================================

write.csv(
  nodos_adtr,
  file = "outputs/tables/07_nodos_red_transformacional_adtr.csv",
  row.names = FALSE
)

write.csv(
  aristas_adtr,
  file = "outputs/tables/07_aristas_red_transformacional_adtr.csv",
  row.names = FALSE
)

write.csv(
  metricas_red,
  file = "outputs/tables/07_metricas_red_transformacional_adtr.csv",
  row.names = FALSE
)

write.csv(
  matriz_adyacencia,
  file = "outputs/tables/07_matriz_adyacencia_red_adtr.csv",
  row.names = TRUE
)

write.csv(
  resumen_relaciones,
  file = "outputs/tables/07_resumen_relaciones_red_adtr.csv",
  row.names = FALSE
)

write.csv(
  nodos_centrales,
  file = "outputs/tables/07_nodos_centrales_red_adtr.csv",
  row.names = FALSE
)

write.csv(
  interpretacion_red,
  file = "outputs/tables/07_interpretacion_red_transformacional_adtr.csv",
  row.names = FALSE
)

write.csv(
  decision_funcional,
  file = "outputs/tables/07_decision_funcional_red_adtr.csv",
  row.names = FALSE
)

resultado_adtr_07 <- list(
  nodos_adtr = nodos_adtr,
  aristas_adtr = aristas_adtr,
  metricas_red = metricas_red,
  matriz_adyacencia = matriz_adyacencia,
  resumen_relaciones = resumen_relaciones,
  nodos_centrales = nodos_centrales,
  interpretacion_red = interpretacion_red,
  decision_funcional = decision_funcional,
  coords_red = coords_red
)

saveRDS(
  resultado_adtr_07,
  file = "outputs/results/07_resultado_red_transformacional_adtr.rds"
)


# ============================================================
# 14. Impresión final
# ============================================================

cat("\n============================================================\n")
cat("Nodos de la red ADTR:\n")
cat("============================================================\n")
print(nodos_adtr)

cat("\n============================================================\n")
cat("Aristas de la red ADTR:\n")
cat("============================================================\n")
print(aristas_adtr)

cat("\n============================================================\n")
cat("Métricas de red ADTR:\n")
cat("============================================================\n")
print(metricas_red)

cat("\n============================================================\n")
cat("Matriz de adyacencia ADTR:\n")
cat("============================================================\n")
print(matriz_adyacencia)

cat("\n============================================================\n")
cat("Resumen de relaciones:\n")
cat("============================================================\n")
print(resumen_relaciones)

cat("\n============================================================\n")
cat("Nodos centrales:\n")
cat("============================================================\n")
print(nodos_centrales)

cat("\n============================================================\n")
cat("Interpretación de la red:\n")
cat("============================================================\n")
print(interpretacion_red)

cat("\n============================================================\n")
cat("Decisión funcional:\n")
cat("============================================================\n")
print(decision_funcional)

cat("\n============================================================\n")
cat("Script 07 finalizado correctamente.\n")
cat("Resultados exportados en outputs/tables, outputs/results y outputs/figures.\n")
cat("============================================================\n")

