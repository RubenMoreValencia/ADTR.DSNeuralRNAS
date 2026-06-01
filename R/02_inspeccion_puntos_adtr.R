# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Archivo: R/02_inspeccion_puntos_adtr.R
# Autor: Rubén Alexander More Valencia
# Propósito:
#   Funciones de inspección e identificación inicial de puntos
#   referenciales ADTR en objetos de aprendizaje existentes.
#
#   Este archivo NO entrena modelos, NO predice y NO duplica
#   funcionalidades de DSNeuralRNAS ni ML.DSNeuralRNAS.
#   Su función es reconocer componentes, posibles puntos ADTR
#   y su trazabilidad programática preliminar.
# ============================================================

# ------------------------------------------------------------
# 1. Inspeccionar objeto compatible con ADTR
# ------------------------------------------------------------

inspeccionar_objeto_adtr <- function(objeto,
                                     nombre_objeto = "objeto",
                                     max_level = 2,
                                     validar_aprendizaje = FALSE) {
  
  if (missing(objeto) || is.null(objeto)) {
    stop("Debe proporcionar un objeto para inspeccionar.", call. = FALSE)
  }
  
  if (validar_aprendizaje && exists("validar_objeto_aprendizaje_adtr")) {
    validar_objeto_aprendizaje_adtr(objeto)
  }
  
  clase_objeto <- class(objeto)
  nombres_objeto <- names(objeto)
  
  if (is.null(nombres_objeto)) {
    nombres_objeto <- character(0)
  }
  
  estructura_objeto <- capture.output(str(objeto, max.level = max_level))
  
  resumen_componentes <- data.frame(
    componente = nombres_objeto,
    clase = vapply(
      nombres_objeto,
      function(nm) paste(class(objeto[[nm]]), collapse = ", "),
      character(1)
    ),
    longitud = vapply(
      nombres_objeto,
      function(nm) length(objeto[[nm]]),
      numeric(1)
    ),
    filas = vapply(
      nombres_objeto,
      function(nm) if (is.data.frame(objeto[[nm]])) nrow(objeto[[nm]]) else NA_integer_,
      integer(1)
    ),
    columnas = vapply(
      nombres_objeto,
      function(nm) if (is.data.frame(objeto[[nm]])) ncol(objeto[[nm]]) else NA_integer_,
      integer(1)
    ),
    stringsAsFactors = FALSE
  )
  
  resultado <- list(
    nombre_objeto = nombre_objeto,
    clase = clase_objeto,
    nombres = nombres_objeto,
    resumen_componentes = resumen_componentes,
    estructura = estructura_objeto
  )
  
  class(resultado) <- c("inspeccion_adtr", class(resultado))
  
  resultado
}


# ------------------------------------------------------------
# 2. Clasificar nombre de componente como punto ADTR probable
# ------------------------------------------------------------

clasificar_componente_adtr <- function(nombre_componente) {
  
  nom_min <- tolower(nombre_componente)
  
  if (grepl("loss|perdida|pérdida", nom_min)) {
    return(c(
      punto = "perdida",
      sistema = "S_apr / S_ctrl / S_learned",
      lectura = "Puede leerse como error, tensión de aprendizaje, frontera o evidencia dinámica.",
      revision = "FALSE"
    ))
  }
  
  if (grepl("grad|gradiente", nom_min)) {
    return(c(
      punto = "gradiente",
      sistema = "S_apr / S_ctrl / S_learned",
      lectura = "Puede leerse como presión de ajuste, dirección de cambio o señal dinámica.",
      revision = "FALSE"
    ))
  }
  
  if (grepl("eta|tasa|learning|lr", nom_min)) {
    return(c(
      punto = "tasa_de_aprendizaje",
      sistema = "S_apr / S_ctrl / S_learned",
      lectura = "Puede leerse como parámetro operativo, variable de control o política de tasa.",
      revision = "FALSE"
    ))
  }
  
  if (grepl("trayectoria|hist|history", nom_min)) {
    return(c(
      punto = "trayectoria",
      sistema = "S_apr / S_ctrl / S_learned",
      lectura = "Puede leerse como evidencia dinámica del proceso de aprendizaje.",
      revision = "TRUE"
    ))
  }
  
  if (grepl("theta|param|peso|sesgo|params", nom_min)) {
    return(c(
      punto = "parametro_aprendido",
      sistema = "S_apr / S_learned",
      lectura = "Puede leerse como estado paramétrico aprendido.",
      revision = "FALSE"
    ))
  }
  
  if (grepl("pred|predic", nom_min)) {
    return(c(
      punto = "prediccion",
      sistema = "S_apr / S_learned",
      lectura = "Puede leerse como salida estimada o representación aprendida.",
      revision = "FALSE"
    ))
  }
  
  if (grepl("metric|métrica|metrica", nom_min)) {
    return(c(
      punto = "metrica",
      sistema = "S_learned",
      lectura = "Puede apoyar evaluación, trazabilidad y comparación del aprendizaje.",
      revision = "FALSE"
    ))
  }
  
  if (grepl("regimen|régimen|frontera|control|politica|política", nom_min)) {
    return(c(
      punto = "componente_dinamico_control",
      sistema = "S_ctrl / S_learned",
      lectura = "Puede leerse como frontera, régimen, política o acción de control.",
      revision = "FALSE"
    ))
  }
  
  if (grepl("unidad|modelo|config", nom_min)) {
    return(c(
      punto = "estructura_de_aprendizaje",
      sistema = "S_apr",
      lectura = "Puede leerse como marco de configuración del aprendizaje.",
      revision = "TRUE"
    ))
  }
  
  if (grepl("preparado|data|datos|base", nom_min)) {
    return(c(
      punto = "base_observada_preparada",
      sistema = "S_obs / S_apr",
      lectura = "Puede contener puntos factuales observados y entradas de aprendizaje.",
      revision = "TRUE"
    ))
  }
  
  c(
    punto = "pendiente_de_clasificacion",
    sistema = "por_revisar",
    lectura = "Requiere inspección conceptual y programática.",
    revision = "TRUE"
  )
}


# ------------------------------------------------------------
# 3. Identificar puntos ADTR desde componentes principales
# ------------------------------------------------------------

identificar_puntos_adtr <- function(objeto,
                                    nombre_objeto = "objeto",
                                    incluir_columnas_internas = TRUE) {
  
  if (missing(objeto) || is.null(objeto)) {
    stop("Debe proporcionar un objeto para identificar puntos ADTR.", call. = FALSE)
  }
  
  if (is.null(names(objeto))) {
    stop("El objeto no contiene componentes con nombre.", call. = FALSE)
  }
  
  nombres <- names(objeto)
  resultados <- list()
  contador <- 1
  
  for (nm in nombres) {
    
    clas <- clasificar_componente_adtr(nm)
    fuente <- paste0(nombre_objeto, "$", nm)
    
    resultados[[contador]] <- data.frame(
      objeto = nombre_objeto,
      componente = nm,
      fuente_programatica = fuente,
      nivel = "componente",
      posible_punto_adtr = unname(clas[["punto"]]),
      sistema_referencial_probable = unname(clas[["sistema"]]),
      lectura_adtr = unname(clas[["lectura"]]),
      requiere_revision = as.logical(unname(clas[["revision"]])),
      stringsAsFactors = FALSE
    )
    contador <- contador + 1
    
    if (incluir_columnas_internas && is.data.frame(objeto[[nm]])) {
      for (col in names(objeto[[nm]])) {
        clas_col <- clasificar_componente_adtr(col)
        fuente_col <- paste0(nombre_objeto, "$", nm, "$", col)
        
        resultados[[contador]] <- data.frame(
          objeto = nombre_objeto,
          componente = paste0(nm, "$", col),
          fuente_programatica = fuente_col,
          nivel = "columna_interna",
          posible_punto_adtr = unname(clas_col[["punto"]]),
          sistema_referencial_probable = unname(clas_col[["sistema"]]),
          lectura_adtr = unname(clas_col[["lectura"]]),
          requiere_revision = as.logical(unname(clas_col[["revision"]])),
          stringsAsFactors = FALSE
        )
        contador <- contador + 1
      }
    }
  }
  
  puntos <- do.call(rbind, resultados)
  rownames(puntos) <- NULL
  puntos
}


# ------------------------------------------------------------
# 4. Identificar puntos ADTR especializados en trayectoria
# ------------------------------------------------------------

identificar_puntos_trayectoria_adtr <- function(trayectoria,
                                                nombre_objeto = "objeto",
                                                nombre_trayectoria = "trayectoria") {
  
  if (exists("validar_trayectoria_adtr")) {
    validar_trayectoria_adtr(trayectoria)
  } else if (!is.data.frame(trayectoria)) {
    stop("La trayectoria debe ser un data.frame.", call. = FALSE)
  }
  
  columnas <- names(trayectoria)
  resultados <- list()
  
  for (i in seq_along(columnas)) {
    col <- columnas[i]
    clas <- clasificar_componente_adtr(col)
    
    resultados[[i]] <- data.frame(
      objeto = nombre_objeto,
      componente = paste0(nombre_trayectoria, "$", col),
      fuente_programatica = paste0(nombre_objeto, "$", nombre_trayectoria, "$", col),
      nivel = "trayectoria",
      posible_punto_adtr = unname(clas[["punto"]]),
      sistema_referencial_probable = unname(clas[["sistema"]]),
      lectura_adtr = unname(clas[["lectura"]]),
      requiere_revision = as.logical(unname(clas[["revision"]])),
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, resultados)
}


# ------------------------------------------------------------
# 5. Seleccionar puntos con trazabilidad suficiente
# ------------------------------------------------------------

seleccionar_puntos_trazables_adtr <- function(tabla_puntos,
                                              excluir_pendientes = TRUE,
                                              permitir_revision = TRUE) {
  
  columnas_requeridas <- c(
    "componente", "fuente_programatica",
    "posible_punto_adtr", "requiere_revision"
  )
  
  if (exists("adtr_validar_columnas")) {
    adtr_validar_columnas(tabla_puntos, columnas_requeridas, "tabla_puntos")
  } else {
    faltantes <- setdiff(columnas_requeridas, names(tabla_puntos))
    if (length(faltantes) > 0) {
      stop("Faltan columnas requeridas: ", paste(faltantes, collapse = ", "), call. = FALSE)
    }
  }
  
  seleccion <- tabla_puntos
  
  seleccion$trazabilidad_suficiente <- !is.na(seleccion$fuente_programatica) &
    seleccion$fuente_programatica != "" &
    !is.na(seleccion$posible_punto_adtr) &
    seleccion$posible_punto_adtr != ""
  
  if (excluir_pendientes) {
    seleccion <- seleccion[seleccion$posible_punto_adtr != "pendiente_de_clasificacion", ]
  }
  
  if (!permitir_revision) {
    seleccion <- seleccion[!seleccion$requiere_revision, ]
  }
  
  seleccion <- seleccion[seleccion$trazabilidad_suficiente, ]
  rownames(seleccion) <- NULL
  
  seleccion
}


# ------------------------------------------------------------
# 6. Resumir puntos identificados
# ------------------------------------------------------------

resumir_puntos_identificados_adtr <- function(tabla_puntos) {
  
  columnas_requeridas <- c("posible_punto_adtr", "sistema_referencial_probable", "requiere_revision")
  
  if (exists("adtr_validar_columnas")) {
    adtr_validar_columnas(tabla_puntos, columnas_requeridas, "tabla_puntos")
  }
  
  total <- nrow(tabla_puntos)
  trazables <- if ("fuente_programatica" %in% names(tabla_puntos)) {
    sum(!is.na(tabla_puntos$fuente_programatica) & tabla_puntos$fuente_programatica != "")
  } else {
    NA_integer_
  }
  
  data.frame(
    total_componentes_evaluados = total,
    total_con_fuente_programatica = trazables,
    total_requiere_revision = sum(tabla_puntos$requiere_revision, na.rm = TRUE),
    total_no_requiere_revision = sum(!tabla_puntos$requiere_revision, na.rm = TRUE),
    puntos_distintos = length(unique(tabla_puntos$posible_punto_adtr)),
    sistemas_probables_distintos = length(unique(tabla_puntos$sistema_referencial_probable)),
    stringsAsFactors = FALSE
  )
}
