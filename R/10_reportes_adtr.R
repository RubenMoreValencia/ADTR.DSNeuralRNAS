# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Archivo: R/10_reportes_adtr.R
# Autor: Rubén Alexander More Valencia
# Propósito:
#   Funciones para crear directorios, exportar tablas/resultados
#   y generar reportes textuales del flujo ADTR.
#
#   Este archivo NO calcula matrices, redes, madurez o acoplamiento.
#   Solo organiza y documenta resultados ya generados por las capas
#   funcionales previas.
# ============================================================

#' Crear directorios base para salidas ADTR
#'
#' Crea una estructura mínima de directorios para almacenar tablas, resultados,
#' figuras y reportes generados por el flujo ADTR.
#'
#' @param base_dir Cadena con el directorio base de salidas. Por defecto `"outputs"`.
#' @param subdirs Vector de subdirectorios a crear dentro de `base_dir`.
#'
#' @return Vector con rutas creadas o verificadas.
#' @export
crear_directorios_adtr <- function(base_dir = "outputs",
                                   subdirs = c("tables", "results", "figures", "reports")) {
  if (!is.character(base_dir) || length(base_dir) != 1 || is.na(base_dir) || base_dir == "") {
    stop("`base_dir` debe ser una cadena no vacía.")
  }

  if (!is.character(subdirs) || length(subdirs) == 0) {
    stop("`subdirs` debe ser un vector de cadenas no vacío.")
  }

  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  rutas <- file.path(base_dir, subdirs)

  for (ruta in rutas) {
    dir.create(ruta, recursive = TRUE, showWarnings = FALSE)
  }

  rutas
}

#' Exportar una tabla ADTR en CSV
#'
#' Exporta un `data.frame` a un archivo CSV, creando el directorio de destino si
#' no existe. Esta función sirve para almacenar salidas tabulares ya generadas
#' por otras funciones ADTR.
#'
#' @param tabla data.frame a exportar.
#' @param archivo Nombre del archivo CSV o ruta completa.
#' @param directorio Directorio de destino cuando `archivo` no incluye ruta.
#' @param overwrite Lógico. Si `FALSE`, genera error cuando el archivo ya existe.
#'
#' @return Ruta del archivo exportado.
#' @export
exportar_tabla_adtr <- function(tabla,
                                archivo,
                                directorio = "outputs/tables",
                                overwrite = TRUE) {
  if (!is.data.frame(tabla)) {
    stop("`tabla` debe ser un data.frame.")
  }

  if (!is.character(archivo) || length(archivo) != 1 || is.na(archivo) || archivo == "") {
    stop("`archivo` debe ser una cadena no vacía.")
  }

  if (!is.logical(overwrite) || length(overwrite) != 1) {
    stop("`overwrite` debe ser TRUE o FALSE.")
  }

  tiene_directorio <- dirname(archivo) != "."

  ruta <- if (tiene_directorio) {
    archivo
  } else {
    file.path(directorio, archivo)
  }

  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(ruta) && !overwrite) {
    stop("El archivo ya existe y `overwrite = FALSE`: ", ruta)
  }

  utils::write.csv(tabla, ruta, row.names = FALSE)

  ruta
}

#' Exportar un resultado ADTR completo
#'
#' Exporta los componentes tipo `data.frame` de una lista de resultados como CSV
#' y guarda la lista completa como RDS. No modifica ni recalcula el resultado.
#'
#' @param resultado Lista con resultados ADTR.
#' @param nombre_base Prefijo para los archivos exportados.
#' @param base_dir Directorio base de salida.
#' @param exportar_csv Lógico. Si `TRUE`, exporta data.frames como CSV.
#' @param exportar_rds Lógico. Si `TRUE`, exporta la lista completa como RDS.
#' @param overwrite Lógico. Controla si se sobrescriben archivos existentes.
#'
#' @return data.frame con rutas exportadas.
#' @export
exportar_resultado_adtr <- function(resultado,
                                    nombre_base = "resultado_adtr",
                                    base_dir = "outputs",
                                    exportar_csv = TRUE,
                                    exportar_rds = TRUE,
                                    overwrite = TRUE) {
  if (!is.list(resultado) || is.data.frame(resultado)) {
    stop("`resultado` debe ser una lista de resultados ADTR, no un data.frame.")
  }

  if (!is.character(nombre_base) || length(nombre_base) != 1 || is.na(nombre_base) || nombre_base == "") {
    stop("`nombre_base` debe ser una cadena no vacía.")
  }

  crear_directorios_adtr(base_dir)

  exportados <- data.frame(
    componente = character(),
    tipo = character(),
    archivo = character(),
    stringsAsFactors = FALSE
  )

  if (isTRUE(exportar_csv)) {
    nombres <- names(resultado)

    if (is.null(nombres)) {
      nombres <- paste0("componente_", seq_along(resultado))
    }

    for (i in seq_along(resultado)) {
      componente <- resultado[[i]]
      nombre <- nombres[i]

      if (is.data.frame(componente)) {
        archivo_csv <- file.path(base_dir, "tables", paste0(nombre_base, "_", nombre, ".csv"))

        if (file.exists(archivo_csv) && !overwrite) {
          stop("El archivo ya existe y `overwrite = FALSE`: ", archivo_csv)
        }

        utils::write.csv(componente, archivo_csv, row.names = FALSE)

        exportados <- rbind(
          exportados,
          data.frame(
            componente = nombre,
            tipo = "csv",
            archivo = archivo_csv,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  if (isTRUE(exportar_rds)) {
    archivo_rds <- file.path(base_dir, "results", paste0(nombre_base, ".rds"))

    if (file.exists(archivo_rds) && !overwrite) {
      stop("El archivo ya existe y `overwrite = FALSE`: ", archivo_rds)
    }

    saveRDS(resultado, archivo_rds)

    exportados <- rbind(
      exportados,
      data.frame(
        componente = nombre_base,
        tipo = "rds",
        archivo = archivo_rds,
        stringsAsFactors = FALSE
      )
    )
  }

  exportados
}

#' Obtener un valor seguro desde una lista ADTR
#'
#' Función auxiliar interna para extraer componentes de una lista sin producir
#' errores si el componente no existe.
#'
#' @param x Lista.
#' @param nombre Nombre del componente.
#' @param default Valor por defecto.
#'
#' @return Componente solicitado o valor por defecto.
#' @keywords internal
adtr_extraer <- function(x, nombre, default = NULL) {
  if (is.list(x) && nombre %in% names(x)) {
    return(x[[nombre]])
  }
  default
}

#' Generar resumen textual ADTR
#'
#' Genera un resumen textual breve a partir de una lista de resultados ADTR. La
#' función reconoce componentes frecuentes del flujo, como métricas por punto,
#' nodos, aristas, índice de madurez, evidencia de Nivel 5 y decisión funcional.
#'
#' @param resultado Lista con resultados del flujo ADTR.
#'
#' @return data.frame con numeración e interpretación textual.
#' @export
generar_resumen_textual_adtr <- function(resultado) {
  if (!is.list(resultado)) {
    stop("`resultado` debe ser una lista.")
  }

  textos <- character()

  metricas_puntos <- adtr_extraer(resultado, "metricas_puntos")
  if (is.data.frame(metricas_puntos) && "punto" %in% names(metricas_puntos)) {
    textos <- c(
      textos,
      paste0(
        "El flujo ADTR evaluó ",
        length(unique(metricas_puntos$punto)),
        " puntos referenciales."
      )
    )
  }

  nodos <- adtr_extraer(resultado, "nodos")
  if (is.null(nodos)) nodos <- adtr_extraer(resultado, "nodos_adtr")

  aristas <- adtr_extraer(resultado, "aristas")
  if (is.null(aristas)) aristas <- adtr_extraer(resultado, "aristas_adtr")

  if (is.data.frame(nodos) && is.data.frame(aristas)) {
    textos <- c(
      textos,
      paste0(
        "La red transformacional contiene ",
        nrow(nodos),
        " nodos y ",
        nrow(aristas),
        " aristas."
      )
    )
  }

  nodos_centrales <- adtr_extraer(resultado, "nodos_centrales")
  if (is.data.frame(nodos_centrales) && all(c("punto", "nodo_central") %in% names(nodos_centrales))) {
    centrales <- nodos_centrales$punto[nodos_centrales$nodo_central]
    if (length(centrales) > 0) {
      textos <- c(
        textos,
        paste0("Los nodos centrales identificados son: ", paste(centrales, collapse = ", "), ".")
      )
    }
  }

  indice_madurez <- adtr_extraer(resultado, "indice_madurez")
  if (is.data.frame(indice_madurez) && all(c("indice_madurez_ecosistemica", "nivel_madurez") %in% names(indice_madurez))) {
    textos <- c(
      textos,
      paste0(
        "El índice de madurez ecosistémica es ",
        round(indice_madurez$indice_madurez_ecosistemica[1], 4),
        ", clasificado como ",
        indice_madurez$nivel_madurez[1],
        "."
      )
    )
  }

  niveles <- adtr_extraer(resultado, "niveles_actualizados")
  if (is.null(niveles)) niveles <- adtr_extraer(resultado, "escala_actualizada")
  if (is.null(niveles)) niveles <- adtr_extraer(resultado, "niveles_relacion")

  if (is.data.frame(niveles) && all(c("tipo", "evidencia_actual") %in% names(niveles))) {
    nivel4 <- any(niveles$tipo %in% c("relacion dinamica", "relación dinámica") & niveles$evidencia_actual)
    nivel5 <- any(niveles$tipo == "acoplamiento interpretado" & niveles$evidencia_actual)

    if (nivel4) {
      textos <- c(textos, "La estructura alcanza evidencia de relación dinámica de Nivel 4.")
    }

    if (nivel5) {
      textos <- c(textos, "La estructura alcanza evidencia exploratoria de acoplamiento interpretado de Nivel 5.")
    } else {
      textos <- c(textos, "No se afirma acoplamiento interpretado de Nivel 5 en esta etapa.")
    }
  }

  evidencia_nivel5 <- adtr_extraer(resultado, "evidencia_nivel5")
  if (is.data.frame(evidencia_nivel5) && "tipo_acoplamiento" %in% names(evidencia_nivel5)) {
    textos <- c(
      textos,
      paste0(
        "La evidencia de acoplamiento fue clasificada como ",
        evidencia_nivel5$tipo_acoplamiento[1],
        "."
      )
    )
  }

  decision <- adtr_extraer(resultado, "decision_funcional")
  if (is.null(decision)) decision <- adtr_extraer(resultado, "decision_acoplamiento")

  if (is.data.frame(decision) && "decision" %in% names(decision)) {
    textos <- c(
      textos,
      paste0("La decisión funcional del flujo fue: ", decision$decision[1], ".")
    )
  }

  if (length(textos) == 0) {
    textos <- "No se identificaron componentes reconocibles para generar resumen textual ADTR."
  }

  data.frame(
    numero = seq_along(textos),
    interpretacion = textos,
    stringsAsFactors = FALSE
  )
}

#' Generar reporte textual del flujo ADTR
#'
#' Convierte el resumen textual de un resultado ADTR en líneas de reporte y,
#' opcionalmente, las escribe en un archivo `.txt`.
#'
#' @param resultado Lista con resultados del flujo ADTR.
#' @param titulo Título del reporte.
#' @param archivo Ruta opcional de archivo `.txt` para exportar el reporte.
#'
#' @return Vector de texto con el reporte.
#' @export
generar_reporte_flujo_adtr <- function(resultado,
                                       titulo = "Reporte del flujo ADTR",
                                       archivo = NULL) {
  if (!is.character(titulo) || length(titulo) != 1 || is.na(titulo) || titulo == "") {
    stop("`titulo` debe ser una cadena no vacía.")
  }

  resumen <- generar_resumen_textual_adtr(resultado)

  lineas <- c(
    titulo,
    paste(rep("=", nchar(titulo)), collapse = ""),
    "",
    paste0(resumen$numero, ". ", resumen$interpretacion)
  )

  if (!is.null(archivo)) {
    if (!is.character(archivo) || length(archivo) != 1 || is.na(archivo) || archivo == "") {
      stop("`archivo` debe ser NULL o una cadena no vacía.")
    }

    dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)
    writeLines(lineas, archivo)
  }

  lineas
}

#' Crear índice de archivos exportados ADTR
#'
#' Construye una tabla simple con archivos existentes dentro de `outputs/tables`,
#' `outputs/results`, `outputs/figures` y `outputs/reports`.
#'
#' @param base_dir Directorio base de salidas.
#'
#' @return data.frame con tipo, nombre y ruta de archivos encontrados.
#' @export
crear_indice_archivos_adtr <- function(base_dir = "outputs") {
  subdirs <- c("tables", "results", "figures", "reports")
  rutas <- file.path(base_dir, subdirs)

  registros <- list()
  k <- 1

  for (i in seq_along(rutas)) {
    ruta <- rutas[i]
    tipo <- subdirs[i]

    if (dir.exists(ruta)) {
      archivos <- list.files(ruta, full.names = TRUE, recursive = FALSE)

      if (length(archivos) > 0) {
        for (archivo in archivos) {
          registros[[k]] <- data.frame(
            tipo = tipo,
            nombre = basename(archivo),
            archivo = archivo,
            stringsAsFactors = FALSE
          )
          k <- k + 1
        }
      }
    }
  }

  if (length(registros) == 0) {
    return(data.frame(tipo = character(), nombre = character(), archivo = character(), stringsAsFactors = FALSE))
  }

  do.call(rbind, registros)
}
