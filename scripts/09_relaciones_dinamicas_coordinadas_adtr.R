
# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: 09_relaciones_dinamicas_coordinadas_adtr.R
# Autor: Rubén Alexander More Valencia
# Propósito:
#   Evaluar relaciones dinámicas coordinadas entre señales
#   internas del aprendizaje: loss, grad_norm y eta.
#
#   Este script busca evidencia de relación dinámica de Nivel 4
#   en ADTR. No afirma causalidad ni acoplamiento fuerte.
#   Trabaja sobre trayectorias ya generadas por objetos de
#   aprendizaje existentes.
# ============================================================


# ============================================================
# 0. Configuración general
# ============================================================

cat("\n============================================================\n")
cat("ADTR.DSNeuralRNAS - Relaciones dinámicas coordinadas\n")
cat("============================================================\n")

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. Validación de trayectoria
# ============================================================

validar_trayectoria_dinamica_adtr <- function(trayectoria) {

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

  if (nrow(trayectoria) < 3) {
    stop("La trayectoria debe tener al menos tres iteraciones.")
  }

  invisible(TRUE)
}


# ============================================================
# 2. Construir variaciones dinámicas
# ============================================================

construir_variaciones_dinamicas_adtr <- function(trayectoria) {

  validar_trayectoria_dinamica_adtr(trayectoria)

  variaciones <- data.frame(
    iter = trayectoria$iter[-1],

    loss = trayectoria$loss[-1],
    grad_norm = trayectoria$grad_norm[-1],
    eta = trayectoria$eta[-1],

    delta_loss = diff(trayectoria$loss),
    delta_grad_norm = diff(trayectoria$grad_norm),
    delta_eta = diff(trayectoria$eta),

    stringsAsFactors = FALSE
  )

  variaciones$direccion_loss <- ifelse(
    variaciones$delta_loss < 0, "disminuye",
    ifelse(variaciones$delta_loss > 0, "aumenta", "estable")
  )

  variaciones$direccion_grad_norm <- ifelse(
    variaciones$delta_grad_norm < 0, "disminuye",
    ifelse(variaciones$delta_grad_norm > 0, "aumenta", "estable")
  )

  variaciones$direccion_eta <- ifelse(
    variaciones$delta_eta < 0, "disminuye",
    ifelse(variaciones$delta_eta > 0, "aumenta", "estable")
  )

  variaciones
}


# ============================================================
# 3. Evaluar coordinación direccional
# ============================================================

evaluar_coordinacion_direccional_adtr <- function(variaciones) {

  pares <- list(
    c("loss", "grad_norm"),
    c("loss", "eta"),
    c("grad_norm", "eta")
  )

  resultados <- list()

  for (i in seq_along(pares)) {

    p1 <- pares[[i]][1]
    p2 <- pares[[i]][2]

    d1 <- variaciones[[paste0("direccion_", p1)]]
    d2 <- variaciones[[paste0("direccion_", p2)]]

    coincidencias <- d1 == d2
    proporcion_coincidencia <- mean(coincidencias)

    tipo_coordinacion <- if (proporcion_coincidencia >= 0.75) {
      "coordinacion_direccional_alta"
    } else if (proporcion_coincidencia >= 0.50) {
      "coordinacion_direccional_moderada"
    } else {
      "coordinacion_direccional_baja"
    }

    resultados[[i]] <- data.frame(
      punto_1 = p1,
      punto_2 = p2,
      coincidencias = sum(coincidencias),
      total_cambios = length(coincidencias),
      proporcion_coincidencia = proporcion_coincidencia,
      tipo_coordinacion = tipo_coordinacion,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, resultados)
}


# ============================================================
# 4. Evaluar correlación dinámica entre señales
# ============================================================

evaluar_correlacion_dinamica_adtr <- function(variaciones) {

  pares <- list(
    c("delta_loss", "delta_grad_norm"),
    c("delta_loss", "delta_eta"),
    c("delta_grad_norm", "delta_eta")
  )

  resultados <- list()

  for (i in seq_along(pares)) {

    v1 <- pares[[i]][1]
    v2 <- pares[[i]][2]

    x <- variaciones[[v1]]
    y <- variaciones[[v2]]

    if (sd(x) == 0 || sd(y) == 0) {
      correlacion <- NA_real_
      tipo_correlacion <- "no_calculable_por_varianza_cero"
    } else {
      correlacion <- cor(x, y)

      tipo_correlacion <- if (abs(correlacion) >= 0.70) {
        "correlacion_dinamica_alta"
      } else if (abs(correlacion) >= 0.40) {
        "correlacion_dinamica_moderada"
      } else {
        "correlacion_dinamica_baja"
      }
    }

    resultados[[i]] <- data.frame(
      variable_1 = v1,
      variable_2 = v2,
      correlacion = correlacion,
      tipo_correlacion = tipo_correlacion,
      observacion = "Correlación entre variaciones; no debe interpretarse como causalidad.",
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, resultados)
}


# ============================================================
# 5. Evaluar patrón conjunto de estabilización
# ============================================================

evaluar_estabilizacion_conjunta_adtr <- function(trayectoria) {

  validar_trayectoria_dinamica_adtr(trayectoria)

  loss_ini <- trayectoria$loss[1]
  loss_fin <- trayectoria$loss[nrow(trayectoria)]

  grad_ini <- trayectoria$grad_norm[1]
  grad_fin <- trayectoria$grad_norm[nrow(trayectoria)]

  eta_ini <- trayectoria$eta[1]
  eta_fin <- trayectoria$eta[nrow(trayectoria)]

  reduccion_loss <- (loss_ini - loss_fin) / loss_ini
  reduccion_grad <- (grad_ini - grad_fin) / grad_ini
  cambio_eta <- (eta_fin - eta_ini) / eta_ini

  patron <- if (
    reduccion_loss > 0.50 &&
    reduccion_grad > 0.50 &&
    cambio_eta < 0
  ) {
    "estabilizacion_progresiva_con_reduccion_de_intensidad"
  } else if (
    reduccion_loss > 0.30 &&
    reduccion_grad > 0.30
  ) {
    "mejora_dinamica_parcial"
  } else {
    "sin_patron_claro_de_estabilizacion"
  }

  data.frame(
    loss_inicial = loss_ini,
    loss_final = loss_fin,
    reduccion_loss_rel = reduccion_loss,

    grad_norm_inicial = grad_ini,
    grad_norm_final = grad_fin,
    reduccion_grad_norm_rel = reduccion_grad,

    eta_inicial = eta_ini,
    eta_final = eta_fin,
    cambio_eta_rel = cambio_eta,

    patron_estabilizacion = patron,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 6. Construir relaciones dinámicas de Nivel 4
# ============================================================

construir_relaciones_dinamicas_nivel4_adtr <- function(
    coordinacion,
    correlaciones,
    estabilizacion
) {

  relaciones <- list()

  for (i in seq_len(nrow(coordinacion))) {

    p1 <- coordinacion$punto_1[i]
    p2 <- coordinacion$punto_2[i]

    tipo <- if (
      coordinacion$tipo_coordinacion[i] %in% c(
        "coordinacion_direccional_alta",
        "coordinacion_direccional_moderada"
      )
    ) {
      "relacion_dinamica_coordinada"
    } else {
      "relacion_dinamica_debil"
    }

    justificacion <- paste0(
      "La proporción de coincidencia direccional entre ",
      p1,
      " y ",
      p2,
      " es ",
      round(coordinacion$proporcion_coincidencia[i], 3),
      "."
    )

    relaciones[[i]] <- data.frame(
      punto_origen = p1,
      punto_destino = p2,
      nivel = "Nivel 4",
      tipo_relacion = tipo,
      evidencia = coordinacion$tipo_coordinacion[i],
      valor = coordinacion$proporcion_coincidencia[i],
      justificacion = justificacion,
      stringsAsFactors = FALSE
    )
  }

  relaciones_df <- do.call(rbind, relaciones)

  if (
    estabilizacion$patron_estabilizacion ==
    "estabilizacion_progresiva_con_reduccion_de_intensidad"
  ) {

    relaciones_df <- rbind(
      relaciones_df,
      data.frame(
        punto_origen = "loss_grad_norm_eta",
        punto_destino = "trayectoria_aprendizaje",
        nivel = "Nivel 4",
        tipo_relacion = "patron_dinamico_conjunto",
        evidencia = estabilizacion$patron_estabilizacion,
        valor = 1,
        justificacion = paste(
          "La pérdida y la norma del gradiente disminuyen de forma importante,",
          "mientras eta reduce la intensidad de actualización."
        ),
        stringsAsFactors = FALSE
      )
    )
  }

  relaciones_df
}


# ============================================================
# 7. Actualizar niveles de relación ecosistémica
# ============================================================

actualizar_niveles_relacion_ecosistemica_adtr <- function(relaciones_nivel4) {

  tiene_nivel4 <- any(
    relaciones_nivel4$tipo_relacion %in% c(
      "relacion_dinamica_coordinada",
      "patron_dinamico_conjunto"
    )
  )

  niveles <- data.frame(
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
      tiene_nivel4,
      FALSE
    ),
    observacion = c(
      "Los puntos pertenecen al mismo conjunto ADTR.",
      "Existen puntos que comparten sistemas referenciales.",
      "Existen puntos que comparten sistemas y una función común.",
      if (tiene_nivel4) {
        "Se observa coordinación dinámica entre señales de trayectoria."
      } else {
        "Aún no se observa coordinación dinámica suficiente."
      },
      "Aún no se demuestra dependencia funcional fuerte o acoplamiento dinámico de nivel alto."
    ),
    stringsAsFactors = FALSE
  )

  niveles
}


# ============================================================
# 8. Interpretación de relaciones dinámicas
# ============================================================

interpretar_relaciones_dinamicas_adtr <- function(
    coordinacion,
    correlaciones,
    estabilizacion,
    relaciones_nivel4,
    niveles_actualizados
) {

  interpretaciones <- character()

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "Se evaluaron relaciones dinámicas entre loss, grad_norm y eta sobre la trayectoria de aprendizaje."
    )
  )

  if (
    estabilizacion$patron_estabilizacion ==
    "estabilizacion_progresiva_con_reduccion_de_intensidad"
  ) {
    interpretaciones <- c(
      interpretaciones,
      "La trayectoria muestra reducción importante de loss y grad_norm, acompañada por disminución de eta; esto sugiere estabilización progresiva con menor intensidad de actualización."
    )
  }

  n_rel_coord <- sum(
    relaciones_nivel4$tipo_relacion %in% c(
      "relacion_dinamica_coordinada",
      "patron_dinamico_conjunto"
    )
  )

  interpretaciones <- c(
    interpretaciones,
    paste0(
      "Se identificaron ",
      n_rel_coord,
      " evidencias de relación dinámica de Nivel 4."
    )
  )

  if (any(niveles_actualizados$tipo == "relación dinámica" &
          niveles_actualizados$evidencia_actual)) {
    interpretaciones <- c(
      interpretaciones,
      "Con esta evidencia, la red ADTR alcanza Nivel 4 en la escala relacional ecosistémica."
    )
  }

  if (!any(niveles_actualizados$tipo == "acoplamiento interpretado" &
           niveles_actualizados$evidencia_actual)) {
    interpretaciones <- c(
      interpretaciones,
      "A pesar de la coordinación dinámica, aún no se afirma acoplamiento interpretado de Nivel 5 ni causalidad."
    )
  }

  interpretaciones <- c(
    interpretaciones,
    "La lectura correcta es ampliar la base ecosistémica preliminar hacia una base ecosistémica con evidencia dinámica, manteniendo pendiente la validación de acoplamientos fuertes."
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

evaluar_decision_funcional_dinamica_adtr <- function(niveles_actualizados) {

  tiene_nivel4 <- any(
    niveles_actualizados$tipo == "relación dinámica" &
      niveles_actualizados$evidencia_actual
  )

  tiene_nivel5 <- any(
    niveles_actualizados$tipo == "acoplamiento interpretado" &
      niveles_actualizados$evidencia_actual
  )

  if (tiene_nivel4 && !tiene_nivel5) {

    decision <- "base_ecosistemica_con_evidencia_dinamica"
    justificacion <- paste(
      "La red alcanza evidencia de Nivel 4 por coordinación dinámica,",
      "pero aún no demuestra acoplamiento interpretado de Nivel 5.",
      "No se debe afirmar causalidad ni ecosistema completo fuerte."
    )

  } else if (tiene_nivel4 && tiene_nivel5) {

    decision <- "posible_ecosistema_dinamico_aprendido"
    justificacion <- paste(
      "Existen evidencias de relación dinámica y acoplamiento interpretado.",
      "Se requiere validación adicional para formalizar ecosistema completo."
    )

  } else {

    decision <- "mantener_como_base_ecosistemica_preliminar"
    justificacion <- paste(
      "No existe evidencia suficiente de relación dinámica coordinada.",
      "La estructura debe mantenerse como base preliminar."
    )
  }

  data.frame(
    necesidad = "Evaluar relaciones dinámicas coordinadas entre señales de aprendizaje",
    decision = decision,
    justificacion = justificacion,
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 10. Visualización simple de trayectorias normalizadas
# ============================================================

graficar_trayectorias_normalizadas_adtr <- function(
    trayectoria,
    archivo = "outputs/figures/09_trayectorias_normalizadas_adtr.png"
) {

  validar_trayectoria_dinamica_adtr(trayectoria)

  normalizar <- function(x) {
    if (max(x) == min(x)) {
      return(rep(0, length(x)))
    }
    (x - min(x)) / (max(x) - min(x))
  }

  datos <- data.frame(
    iter = trayectoria$iter,
    loss_norm = normalizar(trayectoria$loss),
    grad_norm_norm = normalizar(trayectoria$grad_norm),
    eta_norm = normalizar(trayectoria$eta)
  )

  png(filename = archivo, width = 1200, height = 800)

  plot(
    datos$iter,
    datos$loss_norm,
    type = "l",
    lwd = 2,
    ylim = c(0, 1),
    xlab = "Iteración",
    ylab = "Valor normalizado",
    main = "Trayectorias normalizadas ADTR"
  )

  lines(datos$iter, datos$grad_norm_norm, lwd = 2, lty = 2)
  lines(datos$iter, datos$eta_norm, lwd = 2, lty = 3)

  legend(
    "topright",
    legend = c("loss", "grad_norm", "eta"),
    lty = c(1, 2, 3),
    lwd = 2,
    bty = "n"
  )

  dev.off()

  invisible(datos)
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
# 12. Ejecución del análisis dinámico coordinado
# ============================================================

variaciones <- construir_variaciones_dinamicas_adtr(
  trayectoria = trayectoria_demo
)

coordinacion <- evaluar_coordinacion_direccional_adtr(
  variaciones = variaciones
)

correlaciones <- evaluar_correlacion_dinamica_adtr(
  variaciones = variaciones
)

estabilizacion <- evaluar_estabilizacion_conjunta_adtr(
  trayectoria = trayectoria_demo
)

relaciones_nivel4 <- construir_relaciones_dinamicas_nivel4_adtr(
  coordinacion = coordinacion,
  correlaciones = correlaciones,
  estabilizacion = estabilizacion
)

niveles_actualizados <- actualizar_niveles_relacion_ecosistemica_adtr(
  relaciones_nivel4 = relaciones_nivel4
)

interpretacion_dinamica <- interpretar_relaciones_dinamicas_adtr(
  coordinacion = coordinacion,
  correlaciones = correlaciones,
  estabilizacion = estabilizacion,
  relaciones_nivel4 = relaciones_nivel4,
  niveles_actualizados = niveles_actualizados
)

decision_funcional <- evaluar_decision_funcional_dinamica_adtr(
  niveles_actualizados = niveles_actualizados
)

trayectorias_normalizadas <- graficar_trayectorias_normalizadas_adtr(
  trayectoria = trayectoria_demo,
  archivo = "outputs/figures/09_trayectorias_normalizadas_adtr.png"
)


# ============================================================
# 13. Exportación de resultados
# ============================================================

write.csv(
  variaciones,
  file = "outputs/tables/09_variaciones_dinamicas_adtr.csv",
  row.names = FALSE
)

write.csv(
  coordinacion,
  file = "outputs/tables/09_coordinacion_direccional_adtr.csv",
  row.names = FALSE
)

write.csv(
  correlaciones,
  file = "outputs/tables/09_correlaciones_dinamicas_adtr.csv",
  row.names = FALSE
)

write.csv(
  estabilizacion,
  file = "outputs/tables/09_estabilizacion_conjunta_adtr.csv",
  row.names = FALSE
)

write.csv(
  relaciones_nivel4,
  file = "outputs/tables/09_relaciones_dinamicas_nivel4_adtr.csv",
  row.names = FALSE
)

write.csv(
  niveles_actualizados,
  file = "outputs/tables/09_niveles_relacion_ecosistemica_actualizados.csv",
  row.names = FALSE
)

write.csv(
  interpretacion_dinamica,
  file = "outputs/tables/09_interpretacion_relaciones_dinamicas_adtr.csv",
  row.names = FALSE
)

write.csv(
  decision_funcional,
  file = "outputs/tables/09_decision_funcional_relaciones_dinamicas.csv",
  row.names = FALSE
)

write.csv(
  trayectorias_normalizadas,
  file = "outputs/tables/09_trayectorias_normalizadas_adtr.csv",
  row.names = FALSE
)

resultado_adtr_09 <- list(
  variaciones = variaciones,
  coordinacion = coordinacion,
  correlaciones = correlaciones,
  estabilizacion = estabilizacion,
  relaciones_nivel4 = relaciones_nivel4,
  niveles_actualizados = niveles_actualizados,
  interpretacion_dinamica = interpretacion_dinamica,
  decision_funcional = decision_funcional,
  trayectorias_normalizadas = trayectorias_normalizadas
)

saveRDS(
  resultado_adtr_09,
  file = "outputs/results/09_resultado_relaciones_dinamicas_coordinadas_adtr.rds"
)


# ============================================================
# 14. Impresión final
# ============================================================

cat("\n============================================================\n")
cat("Variaciones dinámicas:\n")
cat("============================================================\n")
print(variaciones)

cat("\n============================================================\n")
cat("Coordinación direccional:\n")
cat("============================================================\n")
print(coordinacion)

cat("\n============================================================\n")
cat("Correlaciones dinámicas:\n")
cat("============================================================\n")
print(correlaciones)

cat("\n============================================================\n")
cat("Estabilización conjunta:\n")
cat("============================================================\n")
print(estabilizacion)

cat("\n============================================================\n")
cat("Relaciones dinámicas de Nivel 4:\n")
cat("============================================================\n")
print(relaciones_nivel4)

cat("\n============================================================\n")
cat("Niveles de relación ecosistémica actualizados:\n")
cat("============================================================\n")
print(niveles_actualizados)

cat("\n============================================================\n")
cat("Interpretación dinámica ADTR:\n")
cat("============================================================\n")
print(interpretacion_dinamica)

cat("\n============================================================\n")
cat("Decisión funcional:\n")
cat("============================================================\n")
print(decision_funcional)

cat("\n============================================================\n")
cat("Script 09 finalizado correctamente.\n")
cat("Resultados exportados en outputs/tables, outputs/results y outputs/figures.\n")
cat("============================================================\n")

