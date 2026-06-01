
# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: 05_metricas_integradas_adtr.R
# Autor: Rubén Alexander More Valencia
# Propósito:
#   Integrar matriz multirreferencial M(p), matriz de
#   transformaciones K(p), métricas referenciales, métricas
#   transformacionales e interpretación ADTR final.
#
#   Este script trabaja sobre salidas y matrices ya derivadas.
#   No entrena modelos ni duplica funcionalidades neuronales.
# ============================================================


# ============================================================
# 0. Configuración general
# ============================================================

cat("\n============================================================\n")
cat("ADTR.DSNeuralRNAS - Métricas integradas ADTR\n")
cat("============================================================\n")

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. Validación de matriz ADTR
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
    stop("La matriz ADTR debe tener al menos dos filas.")
  }

  invisible(TRUE)
}


# ============================================================
# 2. Métricas referenciales de M(p)
# ============================================================

calcular_metricas_referenciales_adtr <- function(matriz_adtr) {

  validar_matriz_adtr(matriz_adtr)

  punto <- unique(matriz_adtr$punto)[1]

  cobertura_referencial <- length(unique(matriz_adtr$sistema))
  diversidad_representaciones <- length(unique(matriz_adtr$representacion))
  diversidad_roles <- length(unique(matriz_adtr$rol))
  diversidad_funcional <- length(unique(matriz_adtr$funcion))
  diversidad_naturalezas <- length(unique(matriz_adtr$naturaleza))

  indice_trazabilidad <- mean(
    !is.na(matriz_adtr$fuente_programatica) &
      matriz_adtr$fuente_programatica != ""
  )

  data.frame(
    punto = punto,
    cobertura_referencial = cobertura_referencial,
    diversidad_representaciones = diversidad_representaciones,
    diversidad_roles = diversidad_roles,
    diversidad_funcional = diversidad_funcional,
    diversidad_naturalezas = diversidad_naturalezas,
    indice_trazabilidad = indice_trazabilidad,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 3. Clasificador mejorado de transformaciones T_ij
# ============================================================

clasificar_transformacion_adtr_v2 <- function(
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

  # Caso 1: misma representación sistémica
  if (
    sistema_origen == sistema_destino &&
    funcion_origen == funcion_destino &&
    rol_origen == rol_destino &&
    naturaleza_origen == naturaleza_destino
  ) {

    tipo <- "equivalencia_fuerte"
    justificacion <- "La representación conserva sistema, rol, función y naturaleza."

    # Caso 2: mismo sistema, diferente rol, función o naturaleza
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

    # Caso 3: observación hacia aprendizaje
  } else if (
    sistema_origen == "S_obs" &&
    sistema_destino == "S_apr"
  ) {

    tipo <- "equivalencia_parcial"
    justificacion <- "El dato observado se transforma en objetivo, entrada o representación de aprendizaje; conserva identidad factual, pero cambia función."

    # Caso 4: aprendizaje hacia observación
  } else if (
    sistema_origen == "S_apr" &&
    sistema_destino == "S_obs"
  ) {

    tipo <- "equivalencia_parcial_inversa"
    justificacion <- "La representación de aprendizaje conserva vínculo con el dato observado, pero no reconstruye toda su condición factual."

    # Caso 5: aprendizaje hacia control
  } else if (
    sistema_origen == "S_apr" &&
    sistema_destino == "S_ctrl"
  ) {

    tipo <- "complementariedad"
    justificacion <- "La salida o señal de aprendizaje se usa como criterio de control; no sustituye la representación original, sino que la complementa."

    # Caso 6: control hacia aprendizaje
  } else if (
    sistema_origen == "S_ctrl" &&
    sistema_destino == "S_apr"
  ) {

    tipo <- "equivalencia_parcial_inversa"
    justificacion <- "La señal de control conserva relación con el aprendizaje, pero no reconstruye toda la representación previa."

    # Caso 7: control hacia observación
  } else if (
    sistema_origen == "S_ctrl" &&
    sistema_destino == "S_obs"
  ) {

    tipo <- "equivalencia_parcial_inversa"
    justificacion <- "La señal de discrepancia conserva vínculo con el dato observado, pero no reconstruye su condición factual completa."

    # Caso 8: observación hacia control
  } else if (
    sistema_origen == "S_obs" &&
    sistema_destino == "S_ctrl"
  ) {

    tipo <- "complementariedad"
    justificacion <- "El dato observado se vincula con una señal de control; ambas representaciones se complementan para evaluar corrección."

    # Caso 9: control hacia sistema aprendido
  } else if (
    sistema_origen == "S_ctrl" &&
    sistema_destino == "S_learned"
  ) {

    tipo <- "emergencia_funcional"
    justificacion <- "La señal de control se integra como evidencia aprendida; aparece una función sistémica adicional."

    # Caso 10: sistema aprendido hacia control
  } else if (
    sistema_origen == "S_learned" &&
    sistema_destino == "S_ctrl"
  ) {

    tipo <- "no_reducibilidad_inversa"
    justificacion <- "La evidencia aprendida no se reduce completamente a la señal de control que la originó."

    # Caso 11: aprendizaje hacia sistema aprendido
  } else if (
    sistema_origen == "S_apr" &&
    sistema_destino == "S_learned"
  ) {

    tipo <- "no_reducibilidad"
    justificacion <- "La representación aprendida se integra como componente evaluable o sistémico; no se reduce completamente al resultado de aprendizaje local."

    # Caso 12: sistema aprendido hacia aprendizaje
  } else if (
    sistema_origen == "S_learned" &&
    sistema_destino == "S_apr"
  ) {

    tipo <- "no_reducibilidad_inversa"
    justificacion <- "La estructura aprendida no permite reconstruir completamente la representación local de aprendizaje."

    # Caso 13: observación hacia sistema aprendido
  } else if (
    sistema_origen == "S_obs" &&
    sistema_destino == "S_learned"
  ) {

    tipo <- "emergencia_funcional"
    justificacion <- "El dato factual se interpreta como evidencia aprendida; la función sistémica no estaba presente en la observación inicial."

    # Caso 14: sistema aprendido hacia observación
  } else if (
    sistema_origen == "S_learned" &&
    sistema_destino == "S_obs"
  ) {

    tipo <- "no_reducibilidad_inversa"
    justificacion <- "La evidencia aprendida conserva vínculo con el dato observado, pero no permite reconstruir toda la condición factual inicial."
  }

  data.frame(
    tipo_transformacion = tipo,
    justificacion = justificacion,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 4. Construcción de matriz K(p)
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

    clasificacion <- clasificar_transformacion_adtr_v2(
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
# 5. Métricas transformacionales de K(p)
# ============================================================

calcular_metricas_transformacionales_adtr <- function(matriz_transformaciones) {

  total <- nrow(matriz_transformaciones)

  contar <- function(tipo) {
    sum(matriz_transformaciones$tipo_transformacion == tipo)
  }

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
    indice_equivalencia_parcial_total = (
      equivalencia_parcial + equivalencia_parcial_inversa
    ) / total,
    indice_complementariedad = complementariedad / total,
    indice_no_reducibilidad = no_reducibilidad / total,
    indice_no_reducibilidad_total = (
      no_reducibilidad + no_reducibilidad_inversa
    ) / total,
    indice_emergencia_funcional = emergencia_funcional / total,
    indice_relacion_indeterminada = relacion_indeterminada / total,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 6. Resumen de transformaciones
# ============================================================

resumir_transformaciones_adtr <- function(matriz_transformaciones) {

  tabla <- as.data.frame(table(matriz_transformaciones$tipo_transformacion))
  names(tabla) <- c("tipo_transformacion", "frecuencia")
  tabla$proporcion <- tabla$frecuencia / sum(tabla$frecuencia)
  tabla
}


# ============================================================
# 7. Integración de métricas referenciales y transformacionales
# ============================================================

integrar_metricas_adtr <- function(metricas_referenciales,
                                   metricas_transformacionales) {

  merge(
    metricas_referenciales,
    metricas_transformacionales,
    by = "punto",
    all = TRUE
  )
}


# ============================================================
# 8. Índice exploratorio de complejidad ADTR
# ============================================================

calcular_indice_complejidad_adtr <- function(metricas_integradas) {

  # Índice prudente y exploratorio.
  # No debe asumirse como índice universal.

  cobertura_norm <- metricas_integradas$cobertura_referencial /
    max(metricas_integradas$cobertura_referencial, na.rm = TRUE)

  diversidad_roles_norm <- metricas_integradas$diversidad_roles /
    max(metricas_integradas$diversidad_roles, na.rm = TRUE)

  diversidad_funcional_norm <- metricas_integradas$diversidad_funcional /
    max(metricas_integradas$diversidad_funcional, na.rm = TRUE)

  no_reducibilidad_total <- metricas_integradas$indice_no_reducibilidad_total
  emergencia <- metricas_integradas$indice_emergencia_funcional
  trazabilidad <- metricas_integradas$indice_trazabilidad

  indice <- (
    0.15 * cobertura_norm +
      0.20 * diversidad_roles_norm +
      0.20 * diversidad_funcional_norm +
      0.20 * no_reducibilidad_total +
      0.15 * emergencia +
      0.10 * trazabilidad
  )

  data.frame(
    punto = metricas_integradas$punto,
    indice_complejidad_adtr = indice,
    observacion_indice = "Índice exploratorio; debe interpretarse junto con las matrices M(p) y K(p).",
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 9. Interpretación integrada ADTR
# ============================================================

interpretar_metricas_integradas_adtr <- function(metricas_integradas,
                                                 indice_complejidad) {

  interpretaciones <- character()

  if (metricas_integradas$indice_trazabilidad == 1) {
    interpretaciones <- c(
      interpretaciones,
      "La matriz presenta trazabilidad completa: todas las proyecciones están asociadas a una fuente programática identificable."
    )
  }

  if (metricas_integradas$cobertura_referencial >= 4) {
    interpretaciones <- c(
      interpretaciones,
      "El punto posee cobertura referencial amplia, al aparecer en sistemas de observación, aprendizaje, control y sistema aprendido."
    )
  }

  if (metricas_integradas$diversidad_roles >= 4) {
    interpretaciones <- c(
      interpretaciones,
      "La diversidad de roles es alta; el punto cambia de papel sistémico al pasar entre sistemas referenciales."
    )
  }

  if (metricas_integradas$indice_no_reducibilidad_total >= 0.25) {
    interpretaciones <- c(
      interpretaciones,
      "La no reducibilidad total es relevante; algunas representaciones no pueden explicarse completamente desde otras."
    )
  }

  if (metricas_integradas$indice_emergencia_funcional >= 0.10) {
    interpretaciones <- c(
      interpretaciones,
      "Existe emergencia funcional inicial; algunas proyecciones generan funciones no presentes en el sistema de origen."
    )
  }

  if (metricas_integradas$indice_relacion_indeterminada == 0) {
    interpretaciones <- c(
      interpretaciones,
      "El clasificador no dejó relaciones indeterminadas, lo que mejora la auditabilidad de la matriz K(p)."
    )
  }

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "El índice exploratorio de complejidad ADTR obtenido fue ",
      round(indice_complejidad$indice_complejidad_adtr, 4),
      ". Este valor no debe leerse como medida universal, sino como síntesis comparativa dentro del caso evaluado."
    )
  )

  data.frame(
    punto = metricas_integradas$punto,
    numero = seq_along(interpretaciones),
    interpretacion = interpretaciones,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 10. Decisión funcional
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
    justificacion <- "La necesidad puede resolverse combinando matrices ADTR y reglas interpretativas."

  } else if (isTRUE(utilidad_aplicada) && isTRUE(escalamiento)) {

    decision <- "posible_extension_futura"
    justificacion <- "Existe posible brecha funcional con utilidad y escalamiento. Requiere validación con resultados reales en R."

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
# 11. Ejemplo mínimo: matriz ADTR de variable aprendida
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
# 12. Ejecución del análisis integrado
# ============================================================

metricas_referenciales <- calcular_metricas_referenciales_adtr(
  matriz_variable
)

matriz_transformaciones <- construir_matriz_transformaciones_adtr(
  matriz_variable
)

resumen_transformaciones <- resumir_transformaciones_adtr(
  matriz_transformaciones
)

metricas_transformacionales <- calcular_metricas_transformacionales_adtr(
  matriz_transformaciones
)

metricas_integradas <- integrar_metricas_adtr(
  metricas_referenciales,
  metricas_transformacionales
)

indice_complejidad <- calcular_indice_complejidad_adtr(
  metricas_integradas
)

interpretacion_integrada <- interpretar_metricas_integradas_adtr(
  metricas_integradas,
  indice_complejidad
)

decision_funcional <- evaluar_necesidad_funcional_adtr(
  necesidad = "Integrar M(p), K(p), métricas referenciales y métricas transformacionales",
  existe_en_dsneuralrnas = FALSE,
  existe_en_mldsneuralrnas = FALSE,
  puede_derivarse = TRUE,
  utilidad_aplicada = TRUE,
  escalamiento = TRUE
)


# ============================================================
# 13. Exportación de resultados
# ============================================================

write.csv(
  matriz_variable,
  file = "outputs/tables/05_matriz_adtr_variable_base.csv",
  row.names = FALSE
)

write.csv(
  matriz_transformaciones,
  file = "outputs/tables/05_matriz_transformaciones_adtr_v2.csv",
  row.names = FALSE
)

write.csv(
  resumen_transformaciones,
  file = "outputs/tables/05_resumen_transformaciones_adtr_v2.csv",
  row.names = FALSE
)

write.csv(
  metricas_referenciales,
  file = "outputs/tables/05_metricas_referenciales_adtr.csv",
  row.names = FALSE
)

write.csv(
  metricas_transformacionales,
  file = "outputs/tables/05_metricas_transformacionales_adtr_v2.csv",
  row.names = FALSE
)

write.csv(
  metricas_integradas,
  file = "outputs/tables/05_metricas_integradas_adtr.csv",
  row.names = FALSE
)

write.csv(
  indice_complejidad,
  file = "outputs/tables/05_indice_complejidad_adtr.csv",
  row.names = FALSE
)

write.csv(
  interpretacion_integrada,
  file = "outputs/tables/05_interpretacion_integrada_adtr.csv",
  row.names = FALSE
)

write.csv(
  decision_funcional,
  file = "outputs/tables/05_decision_funcional_metricas_integradas.csv",
  row.names = FALSE
)

resultado_adtr_05 <- list(
  matriz_variable = matriz_variable,
  matriz_transformaciones = matriz_transformaciones,
  resumen_transformaciones = resumen_transformaciones,
  metricas_referenciales = metricas_referenciales,
  metricas_transformacionales = metricas_transformacionales,
  metricas_integradas = metricas_integradas,
  indice_complejidad = indice_complejidad,
  interpretacion_integrada = interpretacion_integrada,
  decision_funcional = decision_funcional
)

saveRDS(
  resultado_adtr_05,
  file = "outputs/results/05_resultado_metricas_integradas_adtr.rds"
)


# ============================================================
# 14. Impresión final
# ============================================================

cat("\n============================================================\n")
cat("Matriz de transformaciones ADTR v2:\n")
cat("============================================================\n")
print(matriz_transformaciones)

cat("\n============================================================\n")
cat("Resumen de transformaciones v2:\n")
cat("============================================================\n")
print(resumen_transformaciones)

cat("\n============================================================\n")
cat("Métricas referenciales:\n")
cat("============================================================\n")
print(metricas_referenciales)

cat("\n============================================================\n")
cat("Métricas transformacionales:\n")
cat("============================================================\n")
print(metricas_transformacionales)

cat("\n============================================================\n")
cat("Métricas integradas ADTR:\n")
cat("============================================================\n")
print(metricas_integradas)

cat("\n============================================================\n")
cat("Índice exploratorio de complejidad ADTR:\n")
cat("============================================================\n")
print(indice_complejidad)

cat("\n============================================================\n")
cat("Interpretación integrada ADTR:\n")
cat("============================================================\n")
print(interpretacion_integrada)

cat("\n============================================================\n")
cat("Decisión funcional:\n")
cat("============================================================\n")
print(decision_funcional)

cat("\n============================================================\n")
cat("Script 05 finalizado correctamente.\n")
cat("Resultados exportados en outputs/tables y outputs/results.\n")
cat("============================================================\n")

