
# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: 10_acoplamiento_interpretado_adtr.R
# Autor: Rubén Alexander More Valencia
# Propósito:
#   Evaluar evidencia exploratoria de acoplamiento interpretado
#   de Nivel 5 entre señales internas del aprendizaje.
#
#   Este script NO prueba causalidad.
#   Evalúa si eta, grad_norm y loss presentan patrones compatibles
#   con dependencia funcional interpretada dentro de ADTR.
# ============================================================


# ============================================================
# 0. Configuración general
# ============================================================

cat("\n============================================================\n")
cat("ADTR.DSNeuralRNAS - Acoplamiento interpretado ADTR\n")
cat("============================================================\n")

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. Validación de trayectoria
# ============================================================

validar_trayectoria_acoplamiento_adtr <- function(trayectoria) {

  if (!is.data.frame(trayectoria)) {
    stop("La trayectoria debe ser un data.frame.")
  }

  columnas_requeridas <- c("iter", "loss", "grad_norm", "eta")
  faltantes <- setdiff(columnas_requeridas, names(trayectoria))

  if (length(faltantes) > 0) {
    stop(
      "La trayectoria no contiene las columnas requeridas: ",
      paste(faltantes, collapse = ", ")
    )
  }

  if (nrow(trayectoria) < 4) {
    stop("La trayectoria debe tener al menos cuatro iteraciones para evaluar acoplamiento exploratorio.")
  }

  invisible(TRUE)
}


# ============================================================
# 2. Construcción de tabla con rezagos y cambios
# ============================================================

construir_tabla_acoplamiento_adtr <- function(trayectoria) {

  validar_trayectoria_acoplamiento_adtr(trayectoria)

  n <- nrow(trayectoria)

  tabla <- data.frame(
    iter = trayectoria$iter[2:n],

    loss_t = trayectoria$loss[2:n],
    loss_prev = trayectoria$loss[1:(n - 1)],
    delta_loss = trayectoria$loss[2:n] - trayectoria$loss[1:(n - 1)],
    mejora_loss = trayectoria$loss[1:(n - 1)] - trayectoria$loss[2:n],

    grad_norm_prev = trayectoria$grad_norm[1:(n - 1)],
    grad_norm_t = trayectoria$grad_norm[2:n],
    delta_grad_norm = trayectoria$grad_norm[2:n] - trayectoria$grad_norm[1:(n - 1)],

    eta_prev = trayectoria$eta[1:(n - 1)],
    eta_t = trayectoria$eta[2:n],
    delta_eta = trayectoria$eta[2:n] - trayectoria$eta[1:(n - 1)],

    stringsAsFactors = FALSE
  )

  tabla$producto_eta_grad_prev <- tabla$eta_prev * tabla$grad_norm_prev

  tabla$direccion_mejora_loss <- ifelse(
    tabla$mejora_loss > 0, "mejora",
    ifelse(tabla$mejora_loss < 0, "empeora", "estable")
  )

  tabla$direccion_grad <- ifelse(
    tabla$delta_grad_norm < 0, "disminuye",
    ifelse(tabla$delta_grad_norm > 0, "aumenta", "estable")
  )

  tabla$direccion_eta <- ifelse(
    tabla$delta_eta < 0, "disminuye",
    ifelse(tabla$delta_eta > 0, "aumenta", "estable")
  )

  tabla
}


# ============================================================
# 3. Evaluación de regla funcional eta * grad_norm -> mejora loss
# ============================================================

evaluar_regla_eta_grad_loss_adtr <- function(tabla_acoplamiento) {

  columnas_requeridas <- c(
    "producto_eta_grad_prev", "mejora_loss",
    "direccion_mejora_loss"
  )

  faltantes <- setdiff(columnas_requeridas, names(tabla_acoplamiento))

  if (length(faltantes) > 0) {
    stop("Faltan columnas requeridas: ", paste(faltantes, collapse = ", "))
  }

  producto <- tabla_acoplamiento$producto_eta_grad_prev
  mejora <- tabla_acoplamiento$mejora_loss

  if (sd(producto) == 0 || sd(mejora) == 0) {
    correlacion <- NA_real_
    tipo_evidencia <- "no_calculable_por_varianza_cero"
  } else {
    correlacion <- cor(producto, mejora)

    tipo_evidencia <- if (abs(correlacion) >= 0.70) {
      "evidencia_funcional_alta"
    } else if (abs(correlacion) >= 0.40) {
      "evidencia_funcional_moderada"
    } else {
      "evidencia_funcional_baja"
    }
  }

  proporcion_mejora <- mean(tabla_acoplamiento$direccion_mejora_loss == "mejora")

  data.frame(
    regla = "eta_prev * grad_norm_prev -> mejora_loss",
    correlacion_producto_mejora = correlacion,
    tipo_evidencia = tipo_evidencia,
    proporcion_mejora_loss = proporcion_mejora,
    observacion = "Relación funcional exploratoria; no implica causalidad.",
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 4. Evaluación de consistencia de reducción conjunta
# ============================================================

evaluar_consistencia_reduccion_conjunta_adtr <- function(tabla_acoplamiento) {

  condicion <- tabla_acoplamiento$mejora_loss > 0 &
    tabla_acoplamiento$delta_grad_norm < 0

  condicion_eta <- tabla_acoplamiento$delta_eta <= 0

  condicion_completa <- condicion & condicion_eta

  data.frame(
    total_transiciones = nrow(tabla_acoplamiento),
    transiciones_loss_mejora_grad_disminuye = sum(condicion),
    proporcion_loss_mejora_grad_disminuye = mean(condicion),
    transiciones_con_eta_no_aumenta = sum(condicion_eta),
    proporcion_eta_no_aumenta = mean(condicion_eta),
    transiciones_patron_completo = sum(condicion_completa),
    proporcion_patron_completo = mean(condicion_completa),
    tipo_consistencia = ifelse(
      mean(condicion_completa) >= 0.75,
      "consistencia_alta",
      ifelse(mean(condicion_completa) >= 0.50,
             "consistencia_moderada",
             "consistencia_baja")
    ),
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 5. Evaluación exploratoria de dependencia funcional
# ============================================================

evaluar_dependencia_funcional_exploratoria_adtr <- function(tabla_acoplamiento) {

  # Modelo exploratorio simple:
  # mejora_loss ~ producto_eta_grad_prev
  # Solo se usa como señal interpretativa, no como prueba causal.

  if (nrow(tabla_acoplamiento) < 4) {

    return(data.frame(
      modelo = "mejora_loss ~ producto_eta_grad_prev",
      r2 = NA_real_,
      pendiente = NA_real_,
      intercepto = NA_real_,
      tipo_dependencia = "muestra_insuficiente_para_modelo_estable",
      observacion = "Se requiere más trayectoria para una evaluación robusta.",
      stringsAsFactors = FALSE
    ))
  }

  modelo <- lm(mejora_loss ~ producto_eta_grad_prev, data = tabla_acoplamiento)
  resumen <- summary(modelo)

  r2 <- resumen$r.squared
  pendiente <- coef(modelo)[["producto_eta_grad_prev"]]
  intercepto <- coef(modelo)[["(Intercept)"]]

  tipo_dependencia <- if (r2 >= 0.70) {
    "dependencia_funcional_exploratoria_alta"
  } else if (r2 >= 0.40) {
    "dependencia_funcional_exploratoria_moderada"
  } else {
    "dependencia_funcional_exploratoria_baja"
  }

  data.frame(
    modelo = "mejora_loss ~ producto_eta_grad_prev",
    r2 = r2,
    pendiente = pendiente,
    intercepto = intercepto,
    tipo_dependencia = tipo_dependencia,
    observacion = "Modelo exploratorio; no debe interpretarse como causalidad.",
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 6. Construir evidencia de acoplamiento interpretado Nivel 5
# ============================================================

construir_evidencia_acoplamiento_nivel5_adtr <- function(
    regla_funcional,
    consistencia,
    dependencia
) {

  cumple_regla <- regla_funcional$tipo_evidencia %in% c(
    "evidencia_funcional_alta",
    "evidencia_funcional_moderada"
  )

  cumple_consistencia <- consistencia$tipo_consistencia %in% c(
    "consistencia_alta",
    "consistencia_moderada"
  )

  cumple_dependencia <- dependencia$tipo_dependencia %in% c(
    "dependencia_funcional_exploratoria_alta",
    "dependencia_funcional_exploratoria_moderada"
  )

  puntaje <- mean(c(cumple_regla, cumple_consistencia, cumple_dependencia))

  tipo_acoplamiento <- if (
    cumple_regla && cumple_consistencia && cumple_dependencia
  ) {
    "acoplamiento_interpretado_fuerte"
  } else if (
    sum(c(cumple_regla, cumple_consistencia, cumple_dependencia)) >= 2
  ) {
    "acoplamiento_interpretado_moderado"
  } else if (
    sum(c(cumple_regla, cumple_consistencia, cumple_dependencia)) == 1
  ) {
    "acoplamiento_interpretado_debil"
  } else {
    "sin_evidencia_suficiente_de_acoplamiento"
  }

  nivel5 <- tipo_acoplamiento %in% c(
    "acoplamiento_interpretado_fuerte",
    "acoplamiento_interpretado_moderado"
  )

  data.frame(
    nivel = "Nivel 5",
    tipo = "acoplamiento interpretado",
    evidencia_nivel5 = nivel5,
    tipo_acoplamiento = tipo_acoplamiento,
    puntaje_criterios = puntaje,
    cumple_regla_funcional = cumple_regla,
    cumple_consistencia = cumple_consistencia,
    cumple_dependencia_exploratoria = cumple_dependencia,
    observacion = "Acoplamiento interpretado exploratorio; no constituye prueba causal.",
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 7. Actualizar escala ecosistémica completa
# ============================================================

actualizar_escala_ecosistemica_nivel5_adtr <- function(evidencia_nivel5) {

  tiene_nivel5 <- isTRUE(evidencia_nivel5$evidencia_nivel5[1])

  data.frame(
    nivel = c(
      "Nivel 1",
      "Nivel 2",
      "Nivel 3",
      "Nivel 4",
      "Nivel 5"
    ),
    tipo = c(
      "coexistencia referencial",
      "relación por sistema",
      "relación funcional",
      "relación dinámica",
      "acoplamiento interpretado"
    ),
    evidencia_actual = c(
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      tiene_nivel5
    ),
    observacion = c(
      "Los puntos pertenecen al mismo conjunto ADTR.",
      "Existen puntos que comparten sistemas referenciales.",
      "Existen puntos que comparten sistemas y una función común.",
      "Se observa coordinación dinámica entre señales de trayectoria.",
      if (tiene_nivel5) {
        "Existe evidencia exploratoria de acoplamiento interpretado; no debe leerse como causalidad."
      } else {
        "Aún no se demuestra acoplamiento interpretado suficiente."
      }
    ),
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 8. Interpretación de acoplamiento
# ============================================================

interpretar_acoplamiento_adtr <- function(
    regla_funcional,
    consistencia,
    dependencia,
    evidencia_nivel5,
    escala_actualizada
) {

  interpretaciones <- character()

  interpretaciones <- c(
    interpretaciones,
    "Se evaluó el acoplamiento interpretado exploratorio entre eta, grad_norm y mejora de loss."
  )

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "La regla funcional evaluada fue ",
      regla_funcional$regla,
      ", con evidencia clasificada como ",
      regla_funcional$tipo_evidencia,
      "."
    )
  )

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "La consistencia del patrón conjunto fue ",
      consistencia$tipo_consistencia,
      ", con proporción de patrón completo igual a ",
      round(consistencia$proporcion_patron_completo, 3),
      "."
    )
  )

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "La dependencia funcional exploratoria se clasificó como ",
      dependencia$tipo_dependencia,
      "."
    )
  )

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "La evidencia de Nivel 5 fue clasificada como ",
      evidencia_nivel5$tipo_acoplamiento,
      ", con puntaje de criterios igual a ",
      round(evidencia_nivel5$puntaje_criterios, 3),
      "."
    )
  )

  if (isTRUE(evidencia_nivel5$evidencia_nivel5)) {
    interpretaciones <- c(
      interpretaciones,
      "La red alcanza evidencia exploratoria de Nivel 5; puede hablarse de acoplamiento interpretado, pero no de causalidad demostrada."
    )
  } else {
    interpretaciones <- c(
      interpretaciones,
      "La red no alcanza evidencia suficiente de Nivel 5; debe mantenerse como base ecosistémica con evidencia dinámica."
    )
  }

  interpretaciones <- c(
    interpretaciones,
    "La lectura debe mantenerse prudente: el acoplamiento interpretado es una inferencia funcional apoyada por patrones, no una prueba causal."
  )

  data.frame(
    numero = seq_along(interpretaciones),
    interpretacion = interpretaciones,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 9. Decisión funcional
# ============================================================

evaluar_decision_funcional_acoplamiento_adtr <- function(evidencia_nivel5) {

  if (isTRUE(evidencia_nivel5$evidencia_nivel5)) {

    decision <- "posible_ecosistema_dinamico_aprendido_exploratorio"
    justificacion <- paste(
      "Existe evidencia exploratoria de acoplamiento interpretado de Nivel 5.",
      "Sin embargo, no se afirma causalidad ni ecosistema completo validado.",
      "Se requiere prueba con trayectorias más extensas y objetos reales."
    )

  } else {

    decision <- "base_ecosistemica_con_evidencia_dinamica"
    justificacion <- paste(
      "La red presenta evidencia dinámica de Nivel 4, pero no alcanza evidencia suficiente",
      "de acoplamiento interpretado de Nivel 5."
    )
  }

  data.frame(
    necesidad = "Evaluar acoplamiento interpretado entre señales internas del aprendizaje",
    decision = decision,
    justificacion = justificacion,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 10. Visualización exploratoria
# ============================================================

graficar_acoplamiento_exploratorio_adtr <- function(
    tabla_acoplamiento,
    archivo = "outputs/figures/10_acoplamiento_eta_grad_loss_adtr.png"
) {

  png(filename = archivo, width = 1200, height = 800)

  plot(
    tabla_acoplamiento$producto_eta_grad_prev,
    tabla_acoplamiento$mejora_loss,
    pch = 19,
    xlab = "eta_prev * grad_norm_prev",
    ylab = "mejora_loss",
    main = "Acoplamiento interpretado exploratorio ADTR"
  )

  if (nrow(tabla_acoplamiento) >= 4 &&
      sd(tabla_acoplamiento$producto_eta_grad_prev) > 0 &&
      sd(tabla_acoplamiento$mejora_loss) > 0) {

    modelo <- lm(mejora_loss ~ producto_eta_grad_prev, data = tabla_acoplamiento)
    abline(modelo, lwd = 2)
  }

  dev.off()

  invisible(TRUE)
}


# ============================================================
# 11. Ejemplo reproducible
# ============================================================

trayectoria_demo <- data.frame(
  iter = 1:5,
  loss = c(0.120, 0.080, 0.050, 0.030, 0.020),
  grad_norm = c(0.90, 0.65, 0.40, 0.22, 0.10),
  eta = c(0.10, 0.10, 0.08, 0.08, 0.05)
)


# ============================================================
# 12. Ejecución del análisis de acoplamiento
# ============================================================

tabla_acoplamiento <- construir_tabla_acoplamiento_adtr(
  trayectoria = trayectoria_demo
)

regla_funcional <- evaluar_regla_eta_grad_loss_adtr(
  tabla_acoplamiento = tabla_acoplamiento
)

consistencia <- evaluar_consistencia_reduccion_conjunta_adtr(
  tabla_acoplamiento = tabla_acoplamiento
)

dependencia <- evaluar_dependencia_funcional_exploratoria_adtr(
  tabla_acoplamiento = tabla_acoplamiento
)

evidencia_nivel5 <- construir_evidencia_acoplamiento_nivel5_adtr(
  regla_funcional = regla_funcional,
  consistencia = consistencia,
  dependencia = dependencia
)

escala_actualizada <- actualizar_escala_ecosistemica_nivel5_adtr(
  evidencia_nivel5 = evidencia_nivel5
)

interpretacion_acoplamiento <- interpretar_acoplamiento_adtr(
  regla_funcional = regla_funcional,
  consistencia = consistencia,
  dependencia = dependencia,
  evidencia_nivel5 = evidencia_nivel5,
  escala_actualizada = escala_actualizada
)

decision_funcional <- evaluar_decision_funcional_acoplamiento_adtr(
  evidencia_nivel5 = evidencia_nivel5
)

graficar_acoplamiento_exploratorio_adtr(
  tabla_acoplamiento = tabla_acoplamiento,
  archivo = "outputs/figures/10_acoplamiento_eta_grad_loss_adtr.png"
)


# ============================================================
# 13. Exportación de resultados
# ============================================================

write.csv(
  tabla_acoplamiento,
  file = "outputs/tables/10_tabla_acoplamiento_adtr.csv",
  row.names = FALSE
)

write.csv(
  regla_funcional,
  file = "outputs/tables/10_regla_funcional_eta_grad_loss_adtr.csv",
  row.names = FALSE
)

write.csv(
  consistencia,
  file = "outputs/tables/10_consistencia_reduccion_conjunta_adtr.csv",
  row.names = FALSE
)

write.csv(
  dependencia,
  file = "outputs/tables/10_dependencia_funcional_exploratoria_adtr.csv",
  row.names = FALSE
)

write.csv(
  evidencia_nivel5,
  file = "outputs/tables/10_evidencia_acoplamiento_nivel5_adtr.csv",
  row.names = FALSE
)

write.csv(
  escala_actualizada,
  file = "outputs/tables/10_escala_ecosistemica_nivel5_adtr.csv",
  row.names = FALSE
)

write.csv(
  interpretacion_acoplamiento,
  file = "outputs/tables/10_interpretacion_acoplamiento_adtr.csv",
  row.names = FALSE
)

write.csv(
  decision_funcional,
  file = "outputs/tables/10_decision_funcional_acoplamiento_adtr.csv",
  row.names = FALSE
)

resultado_adtr_10 <- list(
  tabla_acoplamiento = tabla_acoplamiento,
  regla_funcional = regla_funcional,
  consistencia = consistencia,
  dependencia = dependencia,
  evidencia_nivel5 = evidencia_nivel5,
  escala_actualizada = escala_actualizada,
  interpretacion_acoplamiento = interpretacion_acoplamiento,
  decision_funcional = decision_funcional
)

saveRDS(
  resultado_adtr_10,
  file = "outputs/results/10_resultado_acoplamiento_interpretado_adtr.rds"
)


# ============================================================
# 14. Impresión final
# ============================================================

cat("\n============================================================\n")
cat("Tabla de acoplamiento:\n")
cat("============================================================\n")
print(tabla_acoplamiento)

cat("\n============================================================\n")
cat("Regla funcional eta * grad_norm -> mejora loss:\n")
cat("============================================================\n")
print(regla_funcional)

cat("\n============================================================\n")
cat("Consistencia de reducción conjunta:\n")
cat("============================================================\n")
print(consistencia)

cat("\n============================================================\n")
cat("Dependencia funcional exploratoria:\n")
cat("============================================================\n")
print(dependencia)

cat("\n============================================================\n")
cat("Evidencia de acoplamiento Nivel 5:\n")
cat("============================================================\n")
print(evidencia_nivel5)

cat("\n============================================================\n")
cat("Escala ecosistémica actualizada:\n")
cat("============================================================\n")
print(escala_actualizada)

cat("\n============================================================\n")
cat("Interpretación de acoplamiento:\n")
cat("============================================================\n")
print(interpretacion_acoplamiento)

cat("\n============================================================\n")
cat("Decisión funcional:\n")
cat("============================================================\n")
print(decision_funcional)

cat("\n============================================================\n")
cat("Script 10 finalizado correctamente.\n")
cat("Resultados exportados en outputs/tables, outputs/results y outputs/figures.\n")
cat("============================================================\n")

