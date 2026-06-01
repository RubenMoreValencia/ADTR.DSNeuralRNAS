
# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: 04_clasificacion_transformaciones_adtr.R
# Autor: Rubén Alexander More Valencia
# Propósito:
#   Clasificar transformaciones referenciales T_ij entre
#   sistemas de una matriz ADTR ya construida.
#
#   Este script NO entrena modelos ni calcula nuevas salidas
#   neuronales. Trabaja sobre matrices ADTR derivadas de
#   resultados existentes.
# ============================================================


# ============================================================
# 0. Configuración general
# ============================================================

cat("\n============================================================\n")
cat("ADTR.DSNeuralRNAS - Clasificación de transformaciones T_ij\n")
cat("============================================================\n")

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. Funciones base
# ============================================================

validar_matriz_adtr <- function(matriz_adtr) {

  columnas_requeridas <- c(
    "punto", "sistema", "representacion",
    "rol", "funcion", "naturaleza",
    "fuente_programatica"
  )

  faltantes <- setdiff(columnas_requeridas, names(matriz_adtr))

  if (length(faltantes) > 0) {
    stop(
      "La matriz ADTR no contiene las columnas requeridas: ",
      paste(faltantes, collapse = ", ")
    )
  }

  if (nrow(matriz_adtr) < 2) {
    stop("La matriz ADTR debe tener al menos dos filas para clasificar transformaciones.")
  }

  invisible(TRUE)
}


# ============================================================
# 2. Clasificación heurística prudente de T_ij
# ============================================================

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

  # Caso 1: mismo sistema y misma función
  if (
    sistema_origen == sistema_destino &&
    funcion_origen == funcion_destino &&
    rol_origen == rol_destino
  ) {

    tipo <- "equivalencia_fuerte"
    justificacion <- "La representación conserva sistema, rol y función; se interpreta como equivalencia fuerte interna."

    # Caso 2: mismo sistema, pero rol o representación cambian
  } else if (
    sistema_origen == sistema_destino &&
    (rol_origen != rol_destino || funcion_origen != funcion_destino)
  ) {

    tipo <- "equivalencia_parcial"
    justificacion <- "Las representaciones pertenecen al mismo sistema, pero cambian rol o función; se interpreta como equivalencia parcial."

    # Caso 3: de observación a aprendizaje
  } else if (
    sistema_origen == "S_obs" &&
    sistema_destino == "S_apr"
  ) {

    tipo <- "equivalencia_parcial"
    justificacion <- "El dato observado se transforma en objetivo, entrada o representación de aprendizaje; conserva identidad factual, pero cambia función."

    # Caso 4: de aprendizaje a control
  } else if (
    sistema_origen == "S_apr" &&
    sistema_destino == "S_ctrl"
  ) {

    tipo <- "complementariedad"
    justificacion <- "La salida o señal de aprendizaje se usa como criterio de control; no sustituye la representación original, sino que la complementa."

    # Caso 5: de control a sistema aprendido
  } else if (
    sistema_origen == "S_ctrl" &&
    sistema_destino == "S_learned"
  ) {

    tipo <- "emergencia_funcional"
    justificacion <- "La señal de control se integra como evidencia aprendida; aparece una función sistémica adicional."

    # Caso 6: de aprendizaje a sistema aprendido
  } else if (
    sistema_origen == "S_apr" &&
    sistema_destino == "S_learned"
  ) {

    tipo <- "no_reducibilidad"
    justificacion <- "La representación aprendida se integra como componente evaluable o sistémico; no se reduce completamente al resultado de aprendizaje local."

    # Caso 7: de observación directa a sistema aprendido
  } else if (
    sistema_origen == "S_obs" &&
    sistema_destino == "S_learned"
  ) {

    tipo <- "emergencia_funcional"
    justificacion <- "El dato factual se interpreta como evidencia aprendida; la función sistémica no estaba presente en la observación inicial."

    # Caso 8: relaciones inversas
  } else if (
    sistema_origen == "S_learned" &&
    sistema_destino %in% c("S_obs", "S_apr", "S_ctrl")
  ) {

    tipo <- "no_reducibilidad"
    justificacion <- "La estructura aprendida no permite reconstruir completamente el sistema origen; se interpreta como no reducibilidad."

  } else if (
    sistema_origen == "S_ctrl" &&
    sistema_destino %in% c("S_obs", "S_apr")
  ) {

    tipo <- "equivalencia_parcial"
    justificacion <- "La señal de control conserva relación con el origen, pero no reconstruye toda la representación previa."

  }

  data.frame(
    tipo_transformacion = tipo,
    justificacion = justificacion,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 3. Construcción de matriz K(p)
# ============================================================

construir_matriz_transformaciones_adtr <- function(matriz_adtr) {

  validar_matriz_adtr(matriz_adtr)

  punto <- unique(matriz_adtr$punto)[1]

  pares <- expand.grid(
    origen = seq_len(nrow(matriz_adtr)),
    destino = seq_len(nrow(matriz_adtr))
  )

  pares <- pares[pares$origen != pares$destino, ]

  resultados <- list()

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
      punto = punto,
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

  do.call(rbind, resultados)
}


# ============================================================
# 4. Resumen de transformaciones
# ============================================================

resumir_transformaciones_adtr <- function(matriz_transformaciones) {

  tabla <- as.data.frame(table(matriz_transformaciones$tipo_transformacion))
  names(tabla) <- c("tipo_transformacion", "frecuencia")

  tabla$proporcion <- tabla$frecuencia / sum(tabla$frecuencia)

  tabla
}


# ============================================================
# 5. Métricas derivadas de K(p)
# ============================================================

calcular_metricas_transformacionales_adtr <- function(matriz_transformaciones) {

  total <- nrow(matriz_transformaciones)

  contar_tipo <- function(tipo) {
    sum(matriz_transformaciones$tipo_transformacion == tipo)
  }

  data.frame(
    punto = unique(matriz_transformaciones$punto)[1],
    total_transformaciones = total,
    equivalencia_fuerte = contar_tipo("equivalencia_fuerte"),
    equivalencia_parcial = contar_tipo("equivalencia_parcial"),
    complementariedad = contar_tipo("complementariedad"),
    no_reducibilidad = contar_tipo("no_reducibilidad"),
    emergencia_funcional = contar_tipo("emergencia_funcional"),
    indice_equivalencia_fuerte = contar_tipo("equivalencia_fuerte") / total,
    indice_equivalencia_parcial = contar_tipo("equivalencia_parcial") / total,
    indice_complementariedad = contar_tipo("complementariedad") / total,
    indice_no_reducibilidad = contar_tipo("no_reducibilidad") / total,
    indice_emergencia_funcional = contar_tipo("emergencia_funcional") / total,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 6. Interpretación prudente de K(p)
# ============================================================

interpretar_transformaciones_adtr <- function(metricas_transformacionales) {

  interpretaciones <- character()

  if (metricas_transformacionales$indice_no_reducibilidad > 0.25) {
    interpretaciones <- c(
      interpretaciones,
      "La matriz presenta una proporción relevante de relaciones no reducibles; esto sugiere que algunas representaciones no pueden ser explicadas completamente desde otras."
    )
  }

  if (metricas_transformacionales$indice_emergencia_funcional > 0.20) {
    interpretaciones <- c(
      interpretaciones,
      "Se observa presencia de emergencia funcional; algunas proyecciones generan funciones o naturalezas no presentes en el sistema de origen."
    )
  }

  if (metricas_transformacionales$indice_complementariedad > 0.20) {
    interpretaciones <- c(
      interpretaciones,
      "La complementariedad indica que varias representaciones amplían la lectura del punto sin sustituirse entre sí."
    )
  }

  if (metricas_transformacionales$indice_equivalencia_parcial > 0.20) {
    interpretaciones <- c(
      interpretaciones,
      "La equivalencia parcial muestra que varias transformaciones conservan identidad factual, pero pierden o reorganizan parte de la información original."
    )
  }

  if (length(interpretaciones) == 0) {
    interpretaciones <- c(
      interpretaciones,
      "Las transformaciones no muestran un patrón dominante; se recomienda revisar las clasificaciones caso por caso."
    )
  }

  data.frame(
    numero = seq_along(interpretaciones),
    interpretacion = interpretaciones,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 7. Decisión funcional
# ============================================================

evaluar_necesidad_funcional_adtr <- function(
    necesidad,
    existe_en_dsneuralrnas = NA,
    existe_en_mldsneuralrnas = NA,
    puede_derivarse = NA,
    utilidad_aplicada = NA,
    escalamiento = NA
) {

  decision <- "pendiente"
  justificacion <- ""

  if (isTRUE(existe_en_dsneuralrnas)) {

    decision <- "usar_DSNeuralRNAS"
    justificacion <- "La funcionalidad ya existe en DSNeuralRNAS. No se propone función nueva."

  } else if (isTRUE(existe_en_mldsneuralrnas)) {

    decision <- "usar_MLDSNeuralRNAS"
    justificacion <- "La funcionalidad ya existe en ML.DSNeuralRNAS. No se propone función nueva."

  } else if (isTRUE(puede_derivarse)) {

    decision <- "derivar_de_salidas_existentes"
    justificacion <- "La necesidad puede resolverse combinando salidas existentes y reglas interpretativas."

  } else if (isTRUE(utilidad_aplicada) && isTRUE(escalamiento)) {

    decision <- "posible_extension_futura"
    justificacion <- "Existe posible brecha funcional con utilidad y escalamiento. Requiere validación con resultados en R."

  } else {

    decision <- "mantener_como_lectura_conceptual"
    justificacion <- "No existe evidencia suficiente para proponer funcionalidad nueva."
  }

  data.frame(
    necesidad = necesidad,
    existe_en_dsneuralrnas = existe_en_dsneuralrnas,
    existe_en_mldsneuralrnas = existe_en_mldsneuralrnas,
    puede_derivarse = puede_derivarse,
    utilidad_aplicada = utilidad_aplicada,
    escalamiento = escalamiento,
    decision = decision,
    justificacion = justificacion,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 8. Ejemplo mínimo: matriz ADTR de variable aprendida
# ============================================================

matriz_variable <- data.frame(
  punto = c("y", "y", "y", "y", "y"),
  sistema = c("S_obs", "S_apr", "S_apr", "S_ctrl", "S_learned"),
  representacion = c(
    "preparado$y",
    "unidad$variable_objetivo = y",
    "prediccion",
    "error observado-predicho",
    "comparación observado-predicho"
  ),
  rol = c(
    "variable observada",
    "objetivo de aprendizaje",
    "salida estimada",
    "señal de discrepancia",
    "componente aprendido evaluable"
  ),
  funcion = c(
    "representar el valor factual de referencia",
    "orientar el ajuste del modelo",
    "aproximar la variable observada",
    "evaluar necesidad de corrección",
    "valorar la calidad de la representación aprendida"
  ),
  naturaleza = c(
    "dato factual observado",
    "referente supervisado",
    "representación aprendida",
    "señal diagnóstica",
    "evidencia de aprendizaje"
  ),
  fuente_programatica = c(
    "ajuste_demo$preparado$y",
    "ajuste_demo$unidad$variable_objetivo",
    "ajuste_demo$prediccion",
    "ajuste_demo$preparado$y - ajuste_demo$prediccion",
    "ajuste_demo$metricas"
  ),
  observacion = c(
    "La variable objetivo aparece como dato observado.",
    "La variable objetivo define el referente del aprendizaje.",
    "La predicción representa la forma aprendida.",
    "La discrepancia puede orientar corrección.",
    "La variable se evalúa mediante métricas."
  ),
  stringsAsFactors = FALSE
)


# ============================================================
# 9. Ejecución del análisis transformacional
# ============================================================

matriz_transformaciones <- construir_matriz_transformaciones_adtr(
  matriz_adtr = matriz_variable
)

resumen_transformaciones <- resumir_transformaciones_adtr(
  matriz_transformaciones
)

metricas_transformacionales <- calcular_metricas_transformacionales_adtr(
  matriz_transformaciones
)

interpretacion_transformaciones <- interpretar_transformaciones_adtr(
  metricas_transformacionales
)

decision_funcional <- evaluar_necesidad_funcional_adtr(
  necesidad = "Clasificar transformaciones T_ij entre sistemas referenciales",
  existe_en_dsneuralrnas = FALSE,
  existe_en_mldsneuralrnas = FALSE,
  puede_derivarse = TRUE,
  utilidad_aplicada = TRUE,
  escalamiento = TRUE
)


# ============================================================
# 10. Exportación de resultados
# ============================================================

write.csv(
  matriz_transformaciones,
  file = "outputs/tables/04_matriz_transformaciones_adtr.csv",
  row.names = FALSE
)

write.csv(
  resumen_transformaciones,
  file = "outputs/tables/04_resumen_transformaciones_adtr.csv",
  row.names = FALSE
)

write.csv(
  metricas_transformacionales,
  file = "outputs/tables/04_metricas_transformacionales_adtr.csv",
  row.names = FALSE
)

write.csv(
  interpretacion_transformaciones,
  file = "outputs/tables/04_interpretacion_transformaciones_adtr.csv",
  row.names = FALSE
)

write.csv(
  decision_funcional,
  file = "outputs/tables/04_decision_funcional_transformaciones_adtr.csv",
  row.names = FALSE
)

resultado_adtr_04 <- list(
  matriz_variable = matriz_variable,
  matriz_transformaciones = matriz_transformaciones,
  resumen_transformaciones = resumen_transformaciones,
  metricas_transformacionales = metricas_transformacionales,
  interpretacion_transformaciones = interpretacion_transformaciones,
  decision_funcional = decision_funcional
)

saveRDS(
  resultado_adtr_04,
  file = "outputs/results/04_resultado_transformaciones_adtr.rds"
)


# ============================================================
# 11. Impresión final
# ============================================================

cat("\n============================================================\n")
cat("Matriz de transformaciones ADTR:\n")
cat("============================================================\n")
print(matriz_transformaciones)

cat("\n============================================================\n")
cat("Resumen de transformaciones:\n")
cat("============================================================\n")
print(resumen_transformaciones)

cat("\n============================================================\n")
cat("Métricas transformacionales:\n")
cat("============================================================\n")
print(metricas_transformacionales)

cat("\n============================================================\n")
cat("Interpretación transformacional ADTR:\n")
cat("============================================================\n")
print(interpretacion_transformaciones)

cat("\n============================================================\n")
cat("Decisión funcional:\n")
cat("============================================================\n")
print(decision_funcional)

cat("\n============================================================\n")
cat("Script 04 finalizado correctamente.\n")
cat("Resultados exportados en outputs/tables y outputs/results.\n")
cat("============================================================\n")

