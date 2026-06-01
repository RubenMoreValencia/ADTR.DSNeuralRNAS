# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Archivo: 04_transformaciones_adtr.R
# Propósito:
#   Funciones auxiliares para construir y analizar matrices
#   de transformaciones K(p) a partir de matrices ADTR M(p).
#
# Nota:
#   Estas funciones NO entrenan modelos, NO recalculan
#   predicciones y NO duplican funcionalidades de DSNeuralRNAS
#   ni ML.DSNeuralRNAS. Operan sobre matrices ADTR existentes.
# ============================================================

#' Clasificar una transformación ADTR entre dos sistemas referenciales
#'
#' Clasifica de forma heurística y trazable la relación entre una fila origen
#' y una fila destino de una matriz ADTR. La clasificación distingue relaciones
#' de equivalencia fuerte, equivalencia parcial, equivalencia parcial inversa,
#' complementariedad, no reducibilidad, no reducibilidad inversa, emergencia
#' funcional y relación indeterminada.
#'
#' @param sistema_origen Cadena con el sistema referencial de origen.
#' @param sistema_destino Cadena con el sistema referencial de destino.
#' @param rol_origen Cadena con el rol del punto en el sistema origen.
#' @param rol_destino Cadena con el rol del punto en el sistema destino.
#' @param funcion_origen Cadena con la función del punto en el sistema origen.
#' @param funcion_destino Cadena con la función del punto en el sistema destino.
#' @param naturaleza_origen Cadena con la naturaleza del punto en el sistema origen.
#' @param naturaleza_destino Cadena con la naturaleza del punto en el sistema destino.
#'
#' @return Un `data.frame` con `tipo_transformacion` y `justificacion`.
#' @export
#'
#' @examples
#' clasificar_transformacion_adtr(
#'   sistema_origen = "S_obs",
#'   sistema_destino = "S_apr",
#'   rol_origen = "variable observada",
#'   rol_destino = "objetivo de aprendizaje",
#'   funcion_origen = "representar dato",
#'   funcion_destino = "orientar ajuste",
#'   naturaleza_origen = "dato factual",
#'   naturaleza_destino = "referente supervisado"
#' )
clasificar_transformacion_adtr <- function(
    sistema_origen,
    sistema_destino,
    rol_origen,
    rol_destino,
    funcion_origen,
    funcion_destino,
    naturaleza_origen,
    naturaleza_destino
) {
  tipo <- "relacion_indeterminada"
  justificacion <- "No se cuenta con evidencia suficiente para clasificar la relación."

  if (
    sistema_origen == sistema_destino &&
      funcion_origen == funcion_destino &&
      rol_origen == rol_destino &&
      naturaleza_origen == naturaleza_destino
  ) {
    tipo <- "equivalencia_fuerte"
    justificacion <- "La representación conserva sistema, rol, función y naturaleza."

  } else if (
    sistema_origen == sistema_destino &&
      (
        rol_origen != rol_destino ||
          funcion_origen != funcion_destino ||
          naturaleza_origen != naturaleza_destino
      )
  ) {
    tipo <- "equivalencia_parcial"
    justificacion <- "Las representaciones pertenecen al mismo sistema, pero reorganizan rol, función o naturaleza."

  } else if (sistema_origen == "S_obs" && sistema_destino == "S_apr") {
    tipo <- "equivalencia_parcial"
    justificacion <- "El dato observado se transforma en objetivo, entrada o representación de aprendizaje; conserva identidad factual, pero cambia función."

  } else if (sistema_origen == "S_apr" && sistema_destino == "S_obs") {
    tipo <- "equivalencia_parcial_inversa"
    justificacion <- "La representación de aprendizaje conserva vínculo con el dato observado, pero no reconstruye toda su condición factual."

  } else if (sistema_origen == "S_apr" && sistema_destino == "S_ctrl") {
    tipo <- "complementariedad"
    justificacion <- "La salida o señal de aprendizaje se usa como criterio de control; no sustituye la representación original, sino que la complementa."

  } else if (sistema_origen == "S_ctrl" && sistema_destino == "S_apr") {
    tipo <- "equivalencia_parcial_inversa"
    justificacion <- "La señal de control conserva relación con el aprendizaje, pero no reconstruye toda la representación previa."

  } else if (sistema_origen == "S_ctrl" && sistema_destino == "S_obs") {
    tipo <- "equivalencia_parcial_inversa"
    justificacion <- "La señal de discrepancia conserva vínculo con el dato observado, pero no reconstruye su condición factual completa."

  } else if (sistema_origen == "S_obs" && sistema_destino == "S_ctrl") {
    tipo <- "complementariedad"
    justificacion <- "El dato observado se vincula con una señal de control; ambas representaciones se complementan para evaluar corrección."

  } else if (sistema_origen == "S_ctrl" && sistema_destino == "S_learned") {
    tipo <- "emergencia_funcional"
    justificacion <- "La señal de control se integra como evidencia aprendida; aparece una función sistémica adicional."

  } else if (sistema_origen == "S_learned" && sistema_destino == "S_ctrl") {
    tipo <- "no_reducibilidad_inversa"
    justificacion <- "La evidencia aprendida no se reduce completamente a la señal de control que la originó."

  } else if (sistema_origen == "S_apr" && sistema_destino == "S_learned") {
    tipo <- "no_reducibilidad"
    justificacion <- "La representación aprendida se integra como componente evaluable o sistémico; no se reduce completamente al resultado de aprendizaje local."

  } else if (sistema_origen == "S_learned" && sistema_destino == "S_apr") {
    tipo <- "no_reducibilidad_inversa"
    justificacion <- "La estructura aprendida no permite reconstruir completamente la representación local de aprendizaje."

  } else if (sistema_origen == "S_obs" && sistema_destino == "S_learned") {
    tipo <- "emergencia_funcional"
    justificacion <- "El dato factual se interpreta como evidencia aprendida; la función sistémica no estaba presente en la observación inicial."

  } else if (sistema_origen == "S_learned" && sistema_destino == "S_obs") {
    tipo <- "no_reducibilidad_inversa"
    justificacion <- "La evidencia aprendida conserva vínculo con el dato observado, pero no permite reconstruir toda la condición factual inicial."
  }

  data.frame(
    tipo_transformacion = tipo,
    justificacion = justificacion,
    stringsAsFactors = FALSE
  )
}

#' Construir la matriz de transformaciones ADTR K(p)
#'
#' A partir de una matriz multirreferencial `M(p)`, construye todas las
#' transformaciones dirigidas entre pares de filas diferentes. Cada relación
#' conserva sistema, representación, rol, función, naturaleza y una clasificación
#' interpretativa.
#'
#' @param matriz_adtr `data.frame` con columnas ADTR mínimas: `punto`, `sistema`,
#'   `representacion`, `rol`, `funcion`, `naturaleza`, `fuente_programatica`.
#'
#' @return Un `data.frame` con la matriz de transformaciones `K(p)`.
#' @export
#'
#' @examples
#' matriz <- data.frame(
#'   punto = c("y", "y"),
#'   sistema = c("S_obs", "S_apr"),
#'   representacion = c("preparado$y", "prediccion"),
#'   rol = c("variable observada", "salida estimada"),
#'   funcion = c("representar dato", "aproximar dato"),
#'   naturaleza = c("dato factual", "representacion aprendida"),
#'   fuente_programatica = c("obj$preparado$y", "obj$prediccion")
#' )
#' construir_matriz_transformaciones_adtr(matriz)
construir_matriz_transformaciones_adtr <- function(matriz_adtr) {
  if (exists("validar_matriz_adtr", mode = "function")) {
    validar_matriz_adtr(matriz_adtr)
  }

  columnas_requeridas <- c(
    "punto", "sistema", "representacion", "rol", "funcion",
    "naturaleza", "fuente_programatica"
  )
  faltantes <- setdiff(columnas_requeridas, names(matriz_adtr))
  if (length(faltantes) > 0) {
    stop(
      "La matriz ADTR no contiene las columnas requeridas: ",
      paste(faltantes, collapse = ", ")
    )
  }

  if (nrow(matriz_adtr) < 2) {
    stop("La matriz ADTR debe tener al menos dos filas para construir transformaciones.")
  }

  puntos <- unique(matriz_adtr$punto)
  if (length(puntos) != 1) {
    stop("La matriz de transformaciones K(p) debe construirse para un solo punto a la vez.")
  }

  pares <- expand.grid(
    origen = seq_len(nrow(matriz_adtr)),
    destino = seq_len(nrow(matriz_adtr))
  )
  pares <- pares[pares$origen != pares$destino, ]

  resultados <- vector("list", nrow(pares))

  for (i in seq_len(nrow(pares))) {
    fila_origen <- matriz_adtr[pares$origen[i], ]
    fila_destino <- matriz_adtr[pares$destino[i], ]

    clasificacion <- clasificar_transformacion_adtr(
      sistema_origen = fila_origen$sistema,
      sistema_destino = fila_destino$sistema,
      rol_origen = fila_origen$rol,
      rol_destino = fila_destino$rol,
      funcion_origen = fila_origen$funcion,
      funcion_destino = fila_destino$funcion,
      naturaleza_origen = fila_origen$naturaleza,
      naturaleza_destino = fila_destino$naturaleza
    )

    resultados[[i]] <- data.frame(
      punto = puntos[1],
      sistema_origen = fila_origen$sistema,
      representacion_origen = fila_origen$representacion,
      rol_origen = fila_origen$rol,
      funcion_origen = fila_origen$funcion,
      naturaleza_origen = fila_origen$naturaleza,
      sistema_destino = fila_destino$sistema,
      representacion_destino = fila_destino$representacion,
      rol_destino = fila_destino$rol,
      funcion_destino = fila_destino$funcion,
      naturaleza_destino = fila_destino$naturaleza,
      tipo_transformacion = clasificacion$tipo_transformacion,
      justificacion = clasificacion$justificacion,
      stringsAsFactors = FALSE
    )
  }

  salida <- do.call(rbind, resultados)
  rownames(salida) <- NULL
  salida
}

#' Resumir transformaciones ADTR
#'
#' Calcula frecuencias y proporciones por tipo de transformación en una matriz
#' `K(p)`.
#'
#' @param matriz_transformaciones `data.frame` devuelto por
#'   `construir_matriz_transformaciones_adtr()`.
#'
#' @return Un `data.frame` con tipo de transformación, frecuencia y proporción.
#' @export
resumir_transformaciones_adtr <- function(matriz_transformaciones) {
  if (!"tipo_transformacion" %in% names(matriz_transformaciones)) {
    stop("La matriz debe contener la columna `tipo_transformacion`.")
  }

  tabla <- as.data.frame(table(matriz_transformaciones$tipo_transformacion))
  names(tabla) <- c("tipo_transformacion", "frecuencia")
  tabla$proporcion <- tabla$frecuencia / sum(tabla$frecuencia)
  tabla
}

#' Calcular métricas transformacionales ADTR
#'
#' Calcula métricas agregadas de una matriz de transformaciones `K(p)`, incluyendo
#' equivalencia parcial total, no reducibilidad total, complementariedad,
#' emergencia funcional e indeterminación.
#'
#' @param matriz_transformaciones `data.frame` con columna `tipo_transformacion`.
#'
#' @return Un `data.frame` con métricas transformacionales.
#' @export
calcular_metricas_transformacionales_adtr <- function(matriz_transformaciones) {
  if (!all(c("punto", "tipo_transformacion") %in% names(matriz_transformaciones))) {
    stop("La matriz debe contener `punto` y `tipo_transformacion`.")
  }

  total <- nrow(matriz_transformaciones)
  if (total == 0) {
    stop("La matriz de transformaciones no contiene filas.")
  }

  contar <- function(tipo) sum(matriz_transformaciones$tipo_transformacion == tipo)

  equivalencia_fuerte <- contar("equivalencia_fuerte")
  equivalencia_parcial <- contar("equivalencia_parcial")
  equivalencia_parcial_inversa <- contar("equivalencia_parcial_inversa")
  complementariedad <- contar("complementariedad")
  no_reducibilidad <- contar("no_reducibilidad")
  no_reducibilidad_inversa <- contar("no_reducibilidad_inversa")
  emergencia_funcional <- contar("emergencia_funcional")
  relacion_indeterminada <- contar("relacion_indeterminada")

  data.frame(
    punto = unique(matriz_transformaciones$punto)[1],
    total_transformaciones = total,
    equivalencia_fuerte = equivalencia_fuerte,
    equivalencia_parcial = equivalencia_parcial,
    equivalencia_parcial_inversa = equivalencia_parcial_inversa,
    complementariedad = complementariedad,
    no_reducibilidad = no_reducibilidad,
    no_reducibilidad_inversa = no_reducibilidad_inversa,
    emergencia_funcional = emergencia_funcional,
    relacion_indeterminada = relacion_indeterminada,
    indice_equivalencia_fuerte = equivalencia_fuerte / total,
    indice_equivalencia_parcial = equivalencia_parcial / total,
    indice_equivalencia_parcial_total =
      (equivalencia_parcial + equivalencia_parcial_inversa) / total,
    indice_complementariedad = complementariedad / total,
    indice_no_reducibilidad = no_reducibilidad / total,
    indice_no_reducibilidad_total =
      (no_reducibilidad + no_reducibilidad_inversa) / total,
    indice_emergencia_funcional = emergencia_funcional / total,
    indice_relacion_indeterminada = relacion_indeterminada / total,
    stringsAsFactors = FALSE
  )
}

#' Interpretar transformaciones ADTR
#'
#' Genera una interpretación textual prudente de métricas transformacionales.
#'
#' @param metricas_transformacionales `data.frame` devuelto por
#'   `calcular_metricas_transformacionales_adtr()`.
#'
#' @return Un `data.frame` con interpretaciones numeradas.
#' @export
interpretar_transformaciones_adtr <- function(metricas_transformacionales) {
  requeridas <- c(
    "punto", "indice_no_reducibilidad_total", "indice_emergencia_funcional",
    "indice_complementariedad", "indice_equivalencia_parcial_total",
    "indice_relacion_indeterminada"
  )
  faltantes <- setdiff(requeridas, names(metricas_transformacionales))
  if (length(faltantes) > 0) {
    stop("Faltan métricas requeridas: ", paste(faltantes, collapse = ", "))
  }

  interpretaciones <- character()
  punto <- metricas_transformacionales$punto[1]

  if (metricas_transformacionales$indice_no_reducibilidad_total[1] >= 0.25) {
    interpretaciones <- c(
      interpretaciones,
      paste0(
        "El punto ", punto,
        " presenta no reducibilidad total relevante; algunas representaciones no pueden explicarse completamente desde otras."
      )
    )
  }

  if (metricas_transformacionales$indice_emergencia_funcional[1] >= 0.10) {
    interpretaciones <- c(
      interpretaciones,
      paste0(
        "El punto ", punto,
        " presenta emergencia funcional inicial; algunas proyecciones generan funciones no presentes en el sistema de origen."
      )
    )
  }

  if (metricas_transformacionales$indice_complementariedad[1] >= 0.10) {
    interpretaciones <- c(
      interpretaciones,
      "La complementariedad muestra que algunas representaciones amplían la lectura del punto sin sustituirse entre sí."
    )
  }

  if (metricas_transformacionales$indice_equivalencia_parcial_total[1] >= 0.25) {
    interpretaciones <- c(
      interpretaciones,
      "La equivalencia parcial total indica conservación de identidad con reorganización de rol, función o naturaleza."
    )
  }

  if (metricas_transformacionales$indice_relacion_indeterminada[1] == 0) {
    interpretaciones <- c(
      interpretaciones,
      "No se identifican relaciones indeterminadas; la matriz K(p) queda completamente clasificada según las reglas actuales."
    )
  } else {
    interpretaciones <- c(
      interpretaciones,
      "Existen relaciones indeterminadas; se recomienda revisión conceptual o ajuste de reglas."
    )
  }

  if (length(interpretaciones) == 0) {
    interpretaciones <- "No se observa un patrón transformacional dominante; se recomienda revisar la matriz caso por caso."
  }

  data.frame(
    punto = punto,
    numero = seq_along(interpretaciones),
    interpretacion = interpretaciones,
    stringsAsFactors = FALSE
  )
}

#' Construir y resumir transformaciones ADTR en un solo flujo
#'
#' Función de conveniencia que recibe una matriz `M(p)` y devuelve `K(p)`,
#' resumen, métricas e interpretación transformacional.
#'
#' @param matriz_adtr Matriz multirreferencial de un solo punto.
#'
#' @return Lista con `matriz_transformaciones`, `resumen_transformaciones`,
#'   `metricas_transformacionales` e `interpretacion_transformaciones`.
#' @export
analizar_transformaciones_adtr <- function(matriz_adtr) {
  matriz_transformaciones <- construir_matriz_transformaciones_adtr(matriz_adtr)
  resumen_transformaciones <- resumir_transformaciones_adtr(matriz_transformaciones)
  metricas_transformacionales <- calcular_metricas_transformacionales_adtr(
    matriz_transformaciones
  )
  interpretacion_transformaciones <- interpretar_transformaciones_adtr(
    metricas_transformacionales
  )

  list(
    matriz_transformaciones = matriz_transformaciones,
    resumen_transformaciones = resumen_transformaciones,
    metricas_transformacionales = metricas_transformacionales,
    interpretacion_transformaciones = interpretacion_transformaciones
  )
}
