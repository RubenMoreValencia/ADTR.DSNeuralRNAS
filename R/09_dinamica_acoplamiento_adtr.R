# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Archivo: R/09_dinamica_acoplamiento_adtr.R
# Autor: Rubén Alexander More Valencia
# Propósito:
#   Funciones para evaluar relaciones dinamicas coordinadas
#   y acoplamiento interpretado exploratorio en ADTR.
#
#   Este archivo NO entrena modelos, NO recalcula predicciones
#   y NO prueba causalidad. Trabaja sobre trayectorias existentes.
# ============================================================

#' Construir variaciones dinamicas ADTR
#'
#' Calcula variaciones sucesivas de `loss`, `grad_norm` y `eta`, junto con
#' la direccion de cambio de cada senal. Es la base para evaluar relaciones
#' dinamicas coordinadas de Nivel 4 en ADTR.
#'
#' @param trayectoria data.frame con columnas `iter`, `loss`, `grad_norm` y `eta`.
#'
#' @return data.frame con variaciones y direcciones de cambio.
#' @export
construir_variaciones_dinamicas_adtr <- function(trayectoria) {
  validar_trayectoria_adtr(trayectoria, min_iteraciones = 3)

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

#' Evaluar coordinacion direccional ADTR
#'
#' Evalua si pares de senales (`loss`, `grad_norm`, `eta`) cambian en la
#' misma direccion a lo largo de la trayectoria.
#'
#' @param variaciones data.frame generado por [construir_variaciones_dinamicas_adtr()].
#' @param umbral_alta Umbral para clasificar coordinacion alta.
#' @param umbral_moderada Umbral para clasificar coordinacion moderada.
#'
#' @return data.frame con coincidencias, proporcion y tipo de coordinacion.
#' @export
evaluar_coordinacion_direccional_adtr <- function(variaciones,
                                                  umbral_alta = 0.75,
                                                  umbral_moderada = 0.50) {
  adtr_validar_columnas(
    variaciones,
    c("direccion_loss", "direccion_grad_norm", "direccion_eta"),
    "variaciones"
  )
  validar_indice_unitario_adtr(umbral_alta, "umbral_alta")
  validar_indice_unitario_adtr(umbral_moderada, "umbral_moderada")

  pares <- list(
    c("loss", "grad_norm"),
    c("loss", "eta"),
    c("grad_norm", "eta")
  )

  resultados <- vector("list", length(pares))

  for (i in seq_along(pares)) {
    p1 <- pares[[i]][1]
    p2 <- pares[[i]][2]

    d1 <- variaciones[[paste0("direccion_", p1)]]
    d2 <- variaciones[[paste0("direccion_", p2)]]

    coincidencias <- d1 == d2
    proporcion <- mean(coincidencias)

    tipo <- if (proporcion >= umbral_alta) {
      "coordinacion_direccional_alta"
    } else if (proporcion >= umbral_moderada) {
      "coordinacion_direccional_moderada"
    } else {
      "coordinacion_direccional_baja"
    }

    resultados[[i]] <- data.frame(
      punto_1 = p1,
      punto_2 = p2,
      coincidencias = sum(coincidencias),
      total_cambios = length(coincidencias),
      proporcion_coincidencia = proporcion,
      tipo_coordinacion = tipo,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, resultados)
}

#' Evaluar correlacion dinamica ADTR
#'
#' Calcula correlaciones entre variaciones de `loss`, `grad_norm` y `eta`.
#' Estas correlaciones son exploratorias y no deben interpretarse como causalidad.
#'
#' @param variaciones data.frame generado por [construir_variaciones_dinamicas_adtr()].
#'
#' @return data.frame con correlaciones y tipo de correlacion.
#' @export
evaluar_correlacion_dinamica_adtr <- function(variaciones) {
  adtr_validar_columnas(
    variaciones,
    c("delta_loss", "delta_grad_norm", "delta_eta"),
    "variaciones"
  )

  pares <- list(
    c("delta_loss", "delta_grad_norm"),
    c("delta_loss", "delta_eta"),
    c("delta_grad_norm", "delta_eta")
  )

  resultados <- vector("list", length(pares))

  for (i in seq_along(pares)) {
    v1 <- pares[[i]][1]
    v2 <- pares[[i]][2]

    x <- variaciones[[v1]]
    y <- variaciones[[v2]]

    if (stats::sd(x) == 0 || stats::sd(y) == 0) {
      correlacion <- NA_real_
      tipo <- "no_calculable_por_varianza_cero"
    } else {
      correlacion <- stats::cor(x, y)
      tipo <- if (abs(correlacion) >= 0.70) {
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
      tipo_correlacion = tipo,
      observacion = "Correlacion entre variaciones; no debe interpretarse como causalidad.",
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, resultados)
}

#' Evaluar estabilizacion conjunta ADTR
#'
#' Resume cambios relativos de `loss`, `grad_norm` y `eta`, y clasifica si la
#' trayectoria muestra estabilizacion progresiva con reduccion de intensidad.
#'
#' @param trayectoria data.frame con columnas `iter`, `loss`, `grad_norm` y `eta`.
#'
#' @return data.frame con reducciones relativas y patron de estabilizacion.
#' @export
evaluar_estabilizacion_conjunta_adtr <- function(trayectoria) {
  validar_trayectoria_adtr(trayectoria, min_iteraciones = 3)

  loss_ini <- trayectoria$loss[1]
  loss_fin <- trayectoria$loss[nrow(trayectoria)]
  grad_ini <- trayectoria$grad_norm[1]
  grad_fin <- trayectoria$grad_norm[nrow(trayectoria)]
  eta_ini <- trayectoria$eta[1]
  eta_fin <- trayectoria$eta[nrow(trayectoria)]

  reduccion_loss <- ifelse(loss_ini == 0, NA_real_, (loss_ini - loss_fin) / loss_ini)
  reduccion_grad <- ifelse(grad_ini == 0, NA_real_, (grad_ini - grad_fin) / grad_ini)
  cambio_eta <- ifelse(eta_ini == 0, NA_real_, (eta_fin - eta_ini) / eta_ini)

  patron <- if (!is.na(reduccion_loss) && !is.na(reduccion_grad) &&
      reduccion_loss > 0.50 && reduccion_grad > 0.50 && cambio_eta < 0) {
    "estabilizacion_progresiva_con_reduccion_de_intensidad"
  } else if (!is.na(reduccion_loss) && !is.na(reduccion_grad) &&
      reduccion_loss > 0.30 && reduccion_grad > 0.30) {
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

#' Construir relaciones dinamicas de Nivel 4 ADTR
#'
#' Construye una tabla de relaciones dinamicas a partir de coordinacion
#' direccional y patron conjunto de estabilizacion.
#'
#' @param coordinacion data.frame generado por [evaluar_coordinacion_direccional_adtr()].
#' @param correlaciones data.frame generado por [evaluar_correlacion_dinamica_adtr()].
#' @param estabilizacion data.frame generado por [evaluar_estabilizacion_conjunta_adtr()].
#'
#' @return data.frame con relaciones dinamicas de Nivel 4.
#' @export
construir_relaciones_dinamicas_nivel4_adtr <- function(coordinacion,
                                                        correlaciones,
                                                        estabilizacion) {
  adtr_validar_columnas(coordinacion, c("punto_1", "punto_2", "tipo_coordinacion", "proporcion_coincidencia"), "coordinacion")
  adtr_validar_columnas(correlaciones, c("variable_1", "variable_2", "correlacion"), "correlaciones")
  adtr_validar_columnas(estabilizacion, c("patron_estabilizacion"), "estabilizacion")

  relaciones <- vector("list", nrow(coordinacion))

  for (i in seq_len(nrow(coordinacion))) {
    tipo <- if (coordinacion$tipo_coordinacion[i] %in% c(
      "coordinacion_direccional_alta",
      "coordinacion_direccional_moderada"
    )) {
      "relacion_dinamica_coordinada"
    } else {
      "relacion_dinamica_debil"
    }

    relaciones[[i]] <- data.frame(
      punto_origen = coordinacion$punto_1[i],
      punto_destino = coordinacion$punto_2[i],
      nivel = "Nivel 4",
      tipo_relacion = tipo,
      evidencia = coordinacion$tipo_coordinacion[i],
      valor = coordinacion$proporcion_coincidencia[i],
      justificacion = paste0(
        "La proporcion de coincidencia direccional entre ",
        coordinacion$punto_1[i], " y ", coordinacion$punto_2[i],
        " es ", round(coordinacion$proporcion_coincidencia[i], 3), "."
      ),
      stringsAsFactors = FALSE
    )
  }

  relaciones_df <- do.call(rbind, relaciones)

  if (estabilizacion$patron_estabilizacion[1] == "estabilizacion_progresiva_con_reduccion_de_intensidad") {
    relaciones_df <- rbind(
      relaciones_df,
      data.frame(
        punto_origen = "loss_grad_norm_eta",
        punto_destino = "trayectoria_aprendizaje",
        nivel = "Nivel 4",
        tipo_relacion = "patron_dinamico_conjunto",
        evidencia = estabilizacion$patron_estabilizacion[1],
        valor = 1,
        justificacion = "La perdida y la norma del gradiente disminuyen de forma importante, mientras eta reduce la intensidad de actualizacion.",
        stringsAsFactors = FALSE
      )
    )
  }

  relaciones_df
}

#' Actualizar niveles relacionales con evidencia de Nivel 4
#'
#' Actualiza la escala relacional ecosistemica incorporando evidencia de
#' relacion dinamica coordinada.
#'
#' @param relaciones_nivel4 data.frame generado por [construir_relaciones_dinamicas_nivel4_adtr()].
#'
#' @return data.frame con niveles relacionales actualizados.
#' @export
actualizar_niveles_relacion_ecosistemica_adtr <- function(relaciones_nivel4) {
  adtr_validar_columnas(relaciones_nivel4, c("tipo_relacion"), "relaciones_nivel4")

  tiene_nivel4 <- any(relaciones_nivel4$tipo_relacion %in% c(
    "relacion_dinamica_coordinada",
    "patron_dinamico_conjunto"
  ))

  data.frame(
    nivel = c("Nivel 1", "Nivel 2", "Nivel 3", "Nivel 4", "Nivel 5"),
    tipo = c(
      "coexistencia referencial",
      "relacion por sistema",
      "relacion funcional",
      "relacion dinamica",
      "acoplamiento interpretado"
    ),
    evidencia_actual = c(TRUE, TRUE, TRUE, tiene_nivel4, FALSE),
    observacion = c(
      "Los puntos pertenecen al mismo conjunto ADTR.",
      "Existen puntos que comparten sistemas referenciales.",
      "Existen puntos que comparten sistemas y una funcion comun.",
      if (tiene_nivel4) "Se observa coordinacion dinamica entre senales de trayectoria." else "Aun no se observa coordinacion dinamica suficiente.",
      "Aun no se demuestra dependencia funcional fuerte o acoplamiento dinamico de nivel alto."
    ),
    stringsAsFactors = FALSE
  )
}

#' Interpretar relaciones dinamicas ADTR
#'
#' Genera interpretaciones textuales sobre evidencia de Nivel 4.
#'
#' @param coordinacion Resultado de coordinacion direccional.
#' @param correlaciones Resultado de correlacion dinamica.
#' @param estabilizacion Resultado de estabilizacion conjunta.
#' @param relaciones_nivel4 Relaciones dinamicas de Nivel 4.
#' @param niveles_actualizados Escala relacional actualizada.
#'
#' @return data.frame con interpretaciones.
#' @export
interpretar_relaciones_dinamicas_adtr <- function(coordinacion,
                                                   correlaciones,
                                                   estabilizacion,
                                                   relaciones_nivel4,
                                                   niveles_actualizados) {
  textos <- character()
  textos <- c(textos, "Se evaluaron relaciones dinamicas entre loss, grad_norm y eta sobre la trayectoria de aprendizaje.")

  if (estabilizacion$patron_estabilizacion[1] == "estabilizacion_progresiva_con_reduccion_de_intensidad") {
    textos <- c(textos, "La trayectoria muestra reduccion importante de loss y grad_norm, acompanada por disminucion de eta; esto sugiere estabilizacion progresiva con menor intensidad de actualizacion.")
  }

  n_rel <- sum(relaciones_nivel4$tipo_relacion %in% c("relacion_dinamica_coordinada", "patron_dinamico_conjunto"))
  textos <- c(textos, paste0("Se identificaron ", n_rel, " evidencias de relacion dinamica de Nivel 4."))

  if (any(niveles_actualizados$tipo == "relacion dinamica" & niveles_actualizados$evidencia_actual)) {
    textos <- c(textos, "Con esta evidencia, la red ADTR alcanza Nivel 4 en la escala relacional ecosistemica.")
  }

  textos <- c(textos, "A pesar de la coordinacion dinamica, aun no se afirma acoplamiento interpretado de Nivel 5 ni causalidad.")
  textos <- c(textos, "La lectura correcta es ampliar la base ecosistemica preliminar hacia una base ecosistemica con evidencia dinamica.")

  data.frame(numero = seq_along(textos), interpretacion = textos, stringsAsFactors = FALSE)
}

#' Evaluar decision funcional dinamica ADTR
#'
#' Decide si una red debe mantenerse como base preliminar o si alcanza evidencia
#' dinamica de Nivel 4.
#'
#' @param niveles_actualizados Escala relacional actualizada.
#'
#' @return data.frame con decision funcional.
#' @export
evaluar_decision_funcional_dinamica_adtr <- function(niveles_actualizados) {
  tiene_nivel4 <- any(niveles_actualizados$tipo == "relacion dinamica" & niveles_actualizados$evidencia_actual)
  tiene_nivel5 <- any(niveles_actualizados$tipo == "acoplamiento interpretado" & niveles_actualizados$evidencia_actual)

  if (tiene_nivel4 && !tiene_nivel5) {
    decision <- "base_ecosistemica_con_evidencia_dinamica"
    justificacion <- "La red alcanza evidencia de Nivel 4 por coordinacion dinamica, pero aun no demuestra acoplamiento interpretado de Nivel 5."
  } else if (tiene_nivel4 && tiene_nivel5) {
    decision <- "posible_ecosistema_dinamico_aprendido"
    justificacion <- "Existen evidencias de relacion dinamica y acoplamiento interpretado; requiere validacion adicional."
  } else {
    decision <- "mantener_como_base_ecosistemica_preliminar"
    justificacion <- "No existe evidencia suficiente de relacion dinamica coordinada."
  }

  data.frame(
    necesidad = "Evaluar relaciones dinamicas coordinadas entre senales de aprendizaje",
    decision = decision,
    justificacion = justificacion,
    stringsAsFactors = FALSE
  )
}

#' Construir tabla de acoplamiento ADTR
#'
#' Construye variables rezagadas y de cambio para evaluar la regla funcional
#' `eta_prev * grad_norm_prev -> mejora_loss`.
#'
#' @param trayectoria data.frame con columnas `iter`, `loss`, `grad_norm` y `eta`.
#'
#' @return data.frame con tabla de acoplamiento exploratorio.
#' @export
construir_tabla_acoplamiento_adtr <- function(trayectoria) {
  validar_trayectoria_adtr(trayectoria, min_iteraciones = 4)

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
  tabla$direccion_mejora_loss <- ifelse(tabla$mejora_loss > 0, "mejora", ifelse(tabla$mejora_loss < 0, "empeora", "estable"))
  tabla$direccion_grad <- ifelse(tabla$delta_grad_norm < 0, "disminuye", ifelse(tabla$delta_grad_norm > 0, "aumenta", "estable"))
  tabla$direccion_eta <- ifelse(tabla$delta_eta < 0, "disminuye", ifelse(tabla$delta_eta > 0, "aumenta", "estable"))

  tabla
}

#' Evaluar regla funcional eta-gradiente-loss ADTR
#'
#' Evalua la asociacion entre `eta_prev * grad_norm_prev` y `mejora_loss`.
#' La evidencia es exploratoria y no causal.
#'
#' @param tabla_acoplamiento data.frame generado por [construir_tabla_acoplamiento_adtr()].
#'
#' @return data.frame con correlacion, tipo de evidencia y observacion.
#' @export
evaluar_regla_eta_grad_loss_adtr <- function(tabla_acoplamiento) {
  adtr_validar_columnas(tabla_acoplamiento, c("producto_eta_grad_prev", "mejora_loss", "direccion_mejora_loss"), "tabla_acoplamiento")

  producto <- tabla_acoplamiento$producto_eta_grad_prev
  mejora <- tabla_acoplamiento$mejora_loss

  if (stats::sd(producto) == 0 || stats::sd(mejora) == 0) {
    correlacion <- NA_real_
    tipo <- "no_calculable_por_varianza_cero"
  } else {
    correlacion <- stats::cor(producto, mejora)
    tipo <- if (abs(correlacion) >= 0.70) {
      "evidencia_funcional_alta"
    } else if (abs(correlacion) >= 0.40) {
      "evidencia_funcional_moderada"
    } else {
      "evidencia_funcional_baja"
    }
  }

  data.frame(
    regla = "eta_prev * grad_norm_prev -> mejora_loss",
    correlacion_producto_mejora = correlacion,
    tipo_evidencia = tipo,
    proporcion_mejora_loss = mean(tabla_acoplamiento$direccion_mejora_loss == "mejora"),
    observacion = "Relacion funcional exploratoria; no implica causalidad.",
    stringsAsFactors = FALSE
  )
}

#' Evaluar consistencia de reduccion conjunta ADTR
#'
#' Evalua la consistencia del patron: mejora de perdida, disminucion de
#' gradiente y eta no creciente.
#'
#' @param tabla_acoplamiento data.frame generado por [construir_tabla_acoplamiento_adtr()].
#'
#' @return data.frame con proporciones y tipo de consistencia.
#' @export
evaluar_consistencia_reduccion_conjunta_adtr <- function(tabla_acoplamiento) {
  adtr_validar_columnas(tabla_acoplamiento, c("mejora_loss", "delta_grad_norm", "delta_eta"), "tabla_acoplamiento")

  condicion <- tabla_acoplamiento$mejora_loss > 0 & tabla_acoplamiento$delta_grad_norm < 0
  condicion_eta <- tabla_acoplamiento$delta_eta <= 0
  condicion_completa <- condicion & condicion_eta
  prop <- mean(condicion_completa)

  data.frame(
    total_transiciones = nrow(tabla_acoplamiento),
    transiciones_loss_mejora_grad_disminuye = sum(condicion),
    proporcion_loss_mejora_grad_disminuye = mean(condicion),
    transiciones_con_eta_no_aumenta = sum(condicion_eta),
    proporcion_eta_no_aumenta = mean(condicion_eta),
    transiciones_patron_completo = sum(condicion_completa),
    proporcion_patron_completo = prop,
    tipo_consistencia = ifelse(prop >= 0.75, "consistencia_alta", ifelse(prop >= 0.50, "consistencia_moderada", "consistencia_baja")),
    stringsAsFactors = FALSE
  )
}

#' Evaluar dependencia funcional exploratoria ADTR
#'
#' Ajusta el modelo exploratorio `mejora_loss ~ producto_eta_grad_prev`.
#' No debe interpretarse como causalidad.
#'
#' @param tabla_acoplamiento data.frame generado por [construir_tabla_acoplamiento_adtr()].
#'
#' @return data.frame con R2, pendiente, intercepto y tipo de dependencia.
#' @export
evaluar_dependencia_funcional_exploratoria_adtr <- function(tabla_acoplamiento) {
  adtr_validar_columnas(tabla_acoplamiento, c("mejora_loss", "producto_eta_grad_prev"), "tabla_acoplamiento")

  if (nrow(tabla_acoplamiento) < 4) {
    return(data.frame(
      modelo = "mejora_loss ~ producto_eta_grad_prev",
      r2 = NA_real_,
      pendiente = NA_real_,
      intercepto = NA_real_,
      tipo_dependencia = "muestra_insuficiente_para_modelo_estable",
      observacion = "Se requiere mas trayectoria para una evaluacion robusta.",
      stringsAsFactors = FALSE
    ))
  }

  modelo <- stats::lm(mejora_loss ~ producto_eta_grad_prev, data = tabla_acoplamiento)
  resumen <- summary(modelo)
  r2 <- resumen$r.squared
  pendiente <- stats::coef(modelo)[["producto_eta_grad_prev"]]
  intercepto <- stats::coef(modelo)[["(Intercept)"]]

  tipo <- if (r2 >= 0.70) {
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
    tipo_dependencia = tipo,
    observacion = "Modelo exploratorio; no debe interpretarse como causalidad.",
    stringsAsFactors = FALSE
  )
}

#' Construir evidencia de acoplamiento Nivel 5 ADTR
#'
#' Integra regla funcional, consistencia conjunta y dependencia exploratoria
#' para clasificar acoplamiento interpretado. No prueba causalidad.
#'
#' @param regla_funcional Resultado de [evaluar_regla_eta_grad_loss_adtr()].
#' @param consistencia Resultado de [evaluar_consistencia_reduccion_conjunta_adtr()].
#' @param dependencia Resultado de [evaluar_dependencia_funcional_exploratoria_adtr()].
#'
#' @return data.frame con evidencia de Nivel 5.
#' @export
construir_evidencia_acoplamiento_nivel5_adtr <- function(regla_funcional,
                                                          consistencia,
                                                          dependencia) {
  cumple_regla <- regla_funcional$tipo_evidencia[1] %in% c("evidencia_funcional_alta", "evidencia_funcional_moderada")
  cumple_consistencia <- consistencia$tipo_consistencia[1] %in% c("consistencia_alta", "consistencia_moderada")
  cumple_dependencia <- dependencia$tipo_dependencia[1] %in% c("dependencia_funcional_exploratoria_alta", "dependencia_funcional_exploratoria_moderada")

  n_cumple <- sum(c(cumple_regla, cumple_consistencia, cumple_dependencia))
  puntaje <- mean(c(cumple_regla, cumple_consistencia, cumple_dependencia))

  tipo <- if (n_cumple == 3) {
    "acoplamiento_interpretado_fuerte"
  } else if (n_cumple == 2) {
    "acoplamiento_interpretado_moderado"
  } else if (n_cumple == 1) {
    "acoplamiento_interpretado_debil"
  } else {
    "sin_evidencia_suficiente_de_acoplamiento"
  }

  nivel5 <- tipo %in% c("acoplamiento_interpretado_fuerte", "acoplamiento_interpretado_moderado")

  data.frame(
    nivel = "Nivel 5",
    tipo = "acoplamiento interpretado",
    evidencia_nivel5 = nivel5,
    tipo_acoplamiento = tipo,
    puntaje_criterios = puntaje,
    cumple_regla_funcional = cumple_regla,
    cumple_consistencia = cumple_consistencia,
    cumple_dependencia_exploratoria = cumple_dependencia,
    observacion = "Acoplamiento interpretado exploratorio; no constituye prueba causal.",
    stringsAsFactors = FALSE
  )
}

#' Actualizar escala ecosistemica con Nivel 5 ADTR
#'
#' Actualiza la escala relacional incorporando evidencia exploratoria de
#' acoplamiento interpretado.
#'
#' @param evidencia_nivel5 Resultado de [construir_evidencia_acoplamiento_nivel5_adtr()].
#'
#' @return data.frame con escala relacional actualizada.
#' @export
actualizar_escala_ecosistemica_nivel5_adtr <- function(evidencia_nivel5) {
  tiene_nivel5 <- isTRUE(evidencia_nivel5$evidencia_nivel5[1])

  data.frame(
    nivel = c("Nivel 1", "Nivel 2", "Nivel 3", "Nivel 4", "Nivel 5"),
    tipo = c(
      "coexistencia referencial",
      "relacion por sistema",
      "relacion funcional",
      "relacion dinamica",
      "acoplamiento interpretado"
    ),
    evidencia_actual = c(TRUE, TRUE, TRUE, TRUE, tiene_nivel5),
    observacion = c(
      "Los puntos pertenecen al mismo conjunto ADTR.",
      "Existen puntos que comparten sistemas referenciales.",
      "Existen puntos que comparten sistemas y una funcion comun.",
      "Se observa coordinacion dinamica entre senales de trayectoria.",
      if (tiene_nivel5) "Existe evidencia exploratoria de acoplamiento interpretado; no debe leerse como causalidad." else "Aun no se demuestra acoplamiento interpretado suficiente."
    ),
    stringsAsFactors = FALSE
  )
}

#' Interpretar acoplamiento ADTR
#'
#' Genera una interpretacion textual del acoplamiento interpretado exploratorio.
#'
#' @param regla_funcional Resultado de regla funcional.
#' @param consistencia Resultado de consistencia conjunta.
#' @param dependencia Resultado de dependencia exploratoria.
#' @param evidencia_nivel5 Resultado de evidencia Nivel 5.
#' @param escala_actualizada Escala relacional actualizada.
#'
#' @return data.frame con interpretaciones.
#' @export
interpretar_acoplamiento_adtr <- function(regla_funcional,
                                           consistencia,
                                           dependencia,
                                           evidencia_nivel5,
                                           escala_actualizada) {
  textos <- character()
  textos <- c(textos, "Se evaluo el acoplamiento interpretado exploratorio entre eta, grad_norm y mejora de loss.")
  textos <- c(textos, paste0("La regla funcional evaluada fue ", regla_funcional$regla[1], ", con evidencia clasificada como ", regla_funcional$tipo_evidencia[1], "."))
  textos <- c(textos, paste0("La consistencia del patron conjunto fue ", consistencia$tipo_consistencia[1], ", con proporcion de patron completo igual a ", round(consistencia$proporcion_patron_completo[1], 3), "."))
  textos <- c(textos, paste0("La dependencia funcional exploratoria se clasifico como ", dependencia$tipo_dependencia[1], "."))
  textos <- c(textos, paste0("La evidencia de Nivel 5 fue clasificada como ", evidencia_nivel5$tipo_acoplamiento[1], ", con puntaje de criterios igual a ", round(evidencia_nivel5$puntaje_criterios[1], 3), "."))

  if (isTRUE(evidencia_nivel5$evidencia_nivel5[1])) {
    textos <- c(textos, "La red alcanza evidencia exploratoria de Nivel 5; puede hablarse de acoplamiento interpretado, pero no de causalidad demostrada.")
  } else {
    textos <- c(textos, "La red no alcanza evidencia suficiente de Nivel 5; debe mantenerse como base ecosistemica con evidencia dinamica.")
  }

  textos <- c(textos, "La lectura debe mantenerse prudente: el acoplamiento interpretado es una inferencia funcional apoyada por patrones, no una prueba causal.")

  data.frame(numero = seq_along(textos), interpretacion = textos, stringsAsFactors = FALSE)
}

#' Evaluar decision funcional de acoplamiento ADTR
#'
#' Decide si la evidencia alcanza un posible ecosistema dinamico aprendido
#' exploratorio o si debe mantenerse como base con evidencia dinamica.
#'
#' @param evidencia_nivel5 Resultado de evidencia Nivel 5.
#'
#' @return data.frame con decision funcional.
#' @export
evaluar_decision_funcional_acoplamiento_adtr <- function(evidencia_nivel5) {
  if (isTRUE(evidencia_nivel5$evidencia_nivel5[1])) {
    decision <- "posible_ecosistema_dinamico_aprendido_exploratorio"
    justificacion <- "Existe evidencia exploratoria de acoplamiento interpretado de Nivel 5. Sin embargo, no se afirma causalidad ni ecosistema completo validado."
  } else {
    decision <- "base_ecosistemica_con_evidencia_dinamica"
    justificacion <- "La red presenta evidencia dinamica de Nivel 4, pero no alcanza evidencia suficiente de acoplamiento interpretado de Nivel 5."
  }

  data.frame(
    necesidad = "Evaluar acoplamiento interpretado entre senales internas del aprendizaje",
    decision = decision,
    justificacion = justificacion,
    stringsAsFactors = FALSE
  )
}

#' Analizar dinamica y acoplamiento ADTR
#'
#' Ejecuta el flujo completo sobre una trayectoria: variaciones, coordinacion,
#' correlaciones, estabilizacion, Nivel 4, tabla de acoplamiento, regla
#' funcional, consistencia, dependencia exploratoria, Nivel 5 e interpretacion.
#'
#' @param trayectoria data.frame con columnas `iter`, `loss`, `grad_norm` y `eta`.
#'
#' @return lista con resultados de dinamica y acoplamiento.
#' @export
analizar_dinamica_acoplamiento_adtr <- function(trayectoria) {
  variaciones <- construir_variaciones_dinamicas_adtr(trayectoria)
  coordinacion <- evaluar_coordinacion_direccional_adtr(variaciones)
  correlaciones <- evaluar_correlacion_dinamica_adtr(variaciones)
  estabilizacion <- evaluar_estabilizacion_conjunta_adtr(trayectoria)
  relaciones_nivel4 <- construir_relaciones_dinamicas_nivel4_adtr(coordinacion, correlaciones, estabilizacion)
  niveles_actualizados <- actualizar_niveles_relacion_ecosistemica_adtr(relaciones_nivel4)
  interpretacion_dinamica <- interpretar_relaciones_dinamicas_adtr(coordinacion, correlaciones, estabilizacion, relaciones_nivel4, niveles_actualizados)
  decision_dinamica <- evaluar_decision_funcional_dinamica_adtr(niveles_actualizados)

  tabla_acoplamiento <- construir_tabla_acoplamiento_adtr(trayectoria)
  regla_funcional <- evaluar_regla_eta_grad_loss_adtr(tabla_acoplamiento)
  consistencia <- evaluar_consistencia_reduccion_conjunta_adtr(tabla_acoplamiento)
  dependencia <- evaluar_dependencia_funcional_exploratoria_adtr(tabla_acoplamiento)
  evidencia_nivel5 <- construir_evidencia_acoplamiento_nivel5_adtr(regla_funcional, consistencia, dependencia)
  escala_actualizada <- actualizar_escala_ecosistemica_nivel5_adtr(evidencia_nivel5)
  interpretacion_acoplamiento <- interpretar_acoplamiento_adtr(regla_funcional, consistencia, dependencia, evidencia_nivel5, escala_actualizada)
  decision_acoplamiento <- evaluar_decision_funcional_acoplamiento_adtr(evidencia_nivel5)

  list(
    variaciones = variaciones,
    coordinacion = coordinacion,
    correlaciones = correlaciones,
    estabilizacion = estabilizacion,
    relaciones_nivel4 = relaciones_nivel4,
    niveles_actualizados = niveles_actualizados,
    interpretacion_dinamica = interpretacion_dinamica,
    decision_dinamica = decision_dinamica,
    tabla_acoplamiento = tabla_acoplamiento,
    regla_funcional = regla_funcional,
    consistencia = consistencia,
    dependencia = dependencia,
    evidencia_nivel5 = evidencia_nivel5,
    escala_actualizada = escala_actualizada,
    interpretacion_acoplamiento = interpretacion_acoplamiento,
    decision_acoplamiento = decision_acoplamiento
  )
}
