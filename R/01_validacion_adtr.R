# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Archivo: R/01_validacion_adtr.R
# Autor: Rubén Alexander More Valencia
# Propósito:
#   Funciones base de validación para objetos ADTR.
#
#   Este archivo NO entrena modelos, NO predice y NO duplica
#   funcionalidades de DSNeuralRNAS ni ML.DSNeuralRNAS.
#   Su función es asegurar que las estructuras usadas por ADTR
#   tengan columnas mínimas, tipos esperados y trazabilidad.
# ============================================================

# ------------------------------------------------------------
# 1. Utilidad interna para validar columnas
# ------------------------------------------------------------

adtr_validar_columnas <- function(objeto, columnas_requeridas, nombre_objeto = "objeto") {
  
  if (missing(objeto) || is.null(objeto)) {
    stop("`", nombre_objeto, "` no puede ser NULL.", call. = FALSE)
  }
  
  if (is.null(names(objeto))) {
    stop("`", nombre_objeto, "` no contiene nombres de columnas o componentes.", call. = FALSE)
  }
  
  faltantes <- setdiff(columnas_requeridas, names(objeto))
  
  if (length(faltantes) > 0) {
    stop(
      "`", nombre_objeto, "` no contiene las columnas requeridas: ",
      paste(faltantes, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# 2. Validar matriz ADTR M(p)
# ------------------------------------------------------------

validar_matriz_adtr <- function(matriz_adtr) {
  
  if (!is.data.frame(matriz_adtr)) {
    stop("La matriz ADTR debe ser un data.frame.", call. = FALSE)
  }
  
  columnas_requeridas <- c(
    "punto", "sistema", "representacion",
    "rol", "funcion", "naturaleza",
    "fuente_programatica"
  )
  
  adtr_validar_columnas(
    objeto = matriz_adtr,
    columnas_requeridas = columnas_requeridas,
    nombre_objeto = "matriz_adtr"
  )
  
  if (nrow(matriz_adtr) < 1) {
    stop("La matriz ADTR no contiene filas.", call. = FALSE)
  }
  
  if (any(is.na(matriz_adtr$punto)) || any(matriz_adtr$punto == "")) {
    stop("La columna `punto` contiene valores vacíos o NA.", call. = FALSE)
  }
  
  if (any(is.na(matriz_adtr$sistema)) || any(matriz_adtr$sistema == "")) {
    stop("La columna `sistema` contiene valores vacíos o NA.", call. = FALSE)
  }
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# 3. Validar colección ADTR M(P)
# ------------------------------------------------------------

validar_coleccion_adtr <- function(coleccion_adtr, minimo_puntos = 1) {
  
  validar_matriz_adtr(coleccion_adtr)
  
  n_puntos <- length(unique(coleccion_adtr$punto))
  
  if (n_puntos < minimo_puntos) {
    stop(
      "La colección ADTR debe contener al menos ", minimo_puntos,
      " punto(s) referencial(es). Actualmente contiene ", n_puntos, ".",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# 4. Validar trayectoria de aprendizaje
# ------------------------------------------------------------

validar_trayectoria_adtr <- function(trayectoria,
                                     columnas_requeridas = c("iter", "loss", "grad_norm", "eta"),
                                     min_iteraciones = 3) {
  
  if (!is.data.frame(trayectoria)) {
    stop("La trayectoria debe ser un data.frame.", call. = FALSE)
  }
  
  adtr_validar_columnas(
    objeto = trayectoria,
    columnas_requeridas = columnas_requeridas,
    nombre_objeto = "trayectoria"
  )
  
  if (nrow(trayectoria) < min_iteraciones) {
    stop(
      "La trayectoria debe tener al menos ", min_iteraciones,
      " iteraciones. Actualmente tiene ", nrow(trayectoria), ".",
      call. = FALSE
    )
  }
  
  columnas_numericas <- intersect(c("loss", "grad_norm", "eta"), columnas_requeridas)
  
  for (col in columnas_numericas) {
    if (!is.numeric(trayectoria[[col]])) {
      stop("La columna `", col, "` debe ser numérica.", call. = FALSE)
    }
  }
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# 5. Validar nodos de red ADTR
# ------------------------------------------------------------

validar_nodos_adtr <- function(nodos) {
  
  if (!is.data.frame(nodos)) {
    stop("La tabla de nodos debe ser un data.frame.", call. = FALSE)
  }
  
  columnas_requeridas <- c("id", "tipo_punto")
  
  adtr_validar_columnas(
    objeto = nodos,
    columnas_requeridas = columnas_requeridas,
    nombre_objeto = "nodos"
  )
  
  if (nrow(nodos) < 1) {
    stop("La tabla de nodos no contiene filas.", call. = FALSE)
  }
  
  if (anyDuplicated(nodos$id) > 0) {
    stop("La columna `id` de nodos contiene valores duplicados.", call. = FALSE)
  }
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# 6. Validar aristas de red ADTR
# ------------------------------------------------------------

validar_aristas_adtr <- function(aristas) {
  
  if (!is.data.frame(aristas)) {
    stop("La tabla de aristas debe ser un data.frame.", call. = FALSE)
  }
  
  columnas_requeridas <- c("from", "to", "tipo_relacion", "justificacion")
  
  adtr_validar_columnas(
    objeto = aristas,
    columnas_requeridas = columnas_requeridas,
    nombre_objeto = "aristas"
  )
  
  if (nrow(aristas) < 1) {
    stop("La tabla de aristas no contiene filas.", call. = FALSE)
  }
  
  if (any(is.na(aristas$from)) || any(aristas$from == "")) {
    stop("La columna `from` contiene valores vacíos o NA.", call. = FALSE)
  }
  
  if (any(is.na(aristas$to)) || any(aristas$to == "")) {
    stop("La columna `to` contiene valores vacíos o NA.", call. = FALSE)
  }
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# 7. Validar consistencia entre nodos y aristas
# ------------------------------------------------------------

validar_red_adtr <- function(nodos, aristas) {
  
  validar_nodos_adtr(nodos)
  validar_aristas_adtr(aristas)
  
  ids <- nodos$id
  
  faltantes_from <- setdiff(unique(aristas$from), ids)
  faltantes_to <- setdiff(unique(aristas$to), ids)
  
  if (length(faltantes_from) > 0) {
    stop(
      "Hay valores en `aristas$from` que no existen en `nodos$id`: ",
      paste(faltantes_from, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (length(faltantes_to) > 0) {
    stop(
      "Hay valores en `aristas$to` que no existen en `nodos$id`: ",
      paste(faltantes_to, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# 8. Validar índice o métrica acotada
# ------------------------------------------------------------

validar_indice_unitario_adtr <- function(valor, nombre_indice = "indice") {
  
  if (!is.numeric(valor) || length(valor) != 1 || is.na(valor)) {
    stop("`", nombre_indice, "` debe ser un único valor numérico no NA.", call. = FALSE)
  }
  
  if (valor < 0 || valor > 1) {
    stop("`", nombre_indice, "` debe estar entre 0 y 1. Valor recibido: ", valor, call. = FALSE)
  }
  
  invisible(TRUE)
}


# ------------------------------------------------------------
# 9. Validar objeto de aprendizaje mínimo compatible con ADTR
# ------------------------------------------------------------

validar_objeto_aprendizaje_adtr <- function(objeto,
                                            componentes_requeridos = c("unidad", "preparado", "prediccion", "trayectoria", "metricas")) {
  
  if (!is.list(objeto)) {
    stop("El objeto de aprendizaje debe ser una lista.", call. = FALSE)
  }
  
  adtr_validar_columnas(
    objeto = objeto,
    columnas_requeridas = componentes_requeridos,
    nombre_objeto = "objeto_aprendizaje"
  )
  
  if (!is.data.frame(objeto$preparado)) {
    stop("`objeto$preparado` debe ser un data.frame.", call. = FALSE)
  }
  
  if (!is.data.frame(objeto$trayectoria)) {
    stop("`objeto$trayectoria` debe ser un data.frame.", call. = FALSE)
  }
  
  if (is.null(objeto$unidad$variable_objetivo)) {
    stop("`objeto$unidad$variable_objetivo` no está definido.", call. = FALSE)
  }
  
  variable_objetivo <- objeto$unidad$variable_objetivo
  
  if (!variable_objetivo %in% names(objeto$preparado)) {
    stop(
      "La variable objetivo `", variable_objetivo,
      "` no existe en `objeto$preparado`.",
      call. = FALSE
    )
  }
  
  if (length(objeto$prediccion) != nrow(objeto$preparado)) {
    stop(
      "La longitud de `objeto$prediccion` no coincide con las filas de `objeto$preparado`.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}
