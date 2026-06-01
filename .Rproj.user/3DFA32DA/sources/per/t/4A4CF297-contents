# ============================================================
# Ejemplo mínimo: usar un objeto tipo DSNeuralRNAS y analizarlo
# con una capa ADTR exploratoria
# Archivo: ejemplo_adtr_desde_dsneuralrnas.R
# ============================================================
#
# Idea central:
#   DSNeuralRNAS / ML.DSNeuralRNAS produce un objeto.
#   ADTR interpreta ese objeto.
#
# Este script no entrena una red real. Simula un objeto tipo
# DSNeuralRNAS para demostrar cómo ADTR puede iniciar su análisis
# desde salidas ya generadas: trayectoria, pérdida, gradiente,
# eta, predicción y métricas.
# ============================================================


# ------------------------------------------------------------
# Utilidad mínima
# ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


# ------------------------------------------------------------
# 1. Crear un objeto tipo DSNeuralRNAS
# ------------------------------------------------------------

set.seed(123)

n <- 80
t <- seq_len(n)

# Variable observada
y_obs <- sin(t / 8) + rnorm(n, sd = 0.08)

# Predicción simulada que mejora progresivamente
error_decreciente <- seq(0.45, 0.05, length.out = n)
y_hat <- y_obs + rnorm(n, sd = error_decreciente)

# Señales internas del aprendizaje
loss <- exp(-t / 22) + rnorm(n, sd = 0.015)
loss <- pmax(loss, 0.001)

grad_norm <- exp(-t / 18) + rnorm(n, sd = 0.02)
grad_norm <- pmax(grad_norm, 0.001)

eta <- 0.08 * exp(-t / 90) + 0.005
eta <- pmax(eta, 0.001)

theta_hist <- data.frame(
  iter = t,
  w1 = cumsum(rnorm(n, sd = 0.02)),
  w2 = cumsum(rnorm(n, sd = 0.02)),
  b  = cumsum(rnorm(n, sd = 0.01))
)

trayectoria <- data.frame(
  iter = t,
  y = y_obs,
  prediccion = y_hat,
  error = y_obs - y_hat,
  loss = loss,
  grad_norm = grad_norm,
  eta = eta
)

ajuste_ds <- list(
  tipo = "objeto_tipo_DSNeuralRNAS",
  modelo = "mlp_simple_simulado",
  prediccion = y_hat,
  trayectoria = trayectoria,
  theta_hist = theta_hist,
  params_finales = tail(theta_hist, 1),
  metricas = list(
    loss_inicial = loss[1],
    loss_final = loss[n],
    reduccion_loss = loss[1] - loss[n],
    reduccion_relativa = (loss[1] - loss[n]) / loss[1],
    grad_norm_final = grad_norm[n],
    eta_final = eta[n]
  )
)


# ------------------------------------------------------------
# 2. Funciones mínimas de capa ADTR
# ------------------------------------------------------------

validar_objeto_adtr <- function(obj) {
  campos_minimos <- c("trayectoria", "metricas")
  presentes <- campos_minimos %in% names(obj)

  list(
    valido = all(presentes),
    campos_minimos = campos_minimos,
    presentes = campos_minimos[presentes],
    faltantes = campos_minimos[!presentes],
    tipo_objeto = obj$tipo %||% "no_declarado"
  )
}

inspeccionar_puntos_adtr <- function(obj) {
  tr <- obj$trayectoria

  puntos_posibles <- c(
    "y", "prediccion", "error",
    "loss", "grad_norm", "eta"
  )

  puntos <- puntos_posibles[puntos_posibles %in% names(tr)]

  data.frame(
    punto = puntos,
    disponible = TRUE,
    fuente = "trayectoria",
    n_observaciones = nrow(tr),
    stringsAsFactors = FALSE
  )
}

construir_matriz_mp <- function(obj, punto) {
  tr <- obj$trayectoria

  if (!punto %in% names(tr)) {
    stop("El punto no existe en la trayectoria: ", punto)
  }

  valores <- tr[[punto]]

  rol <- switch(
    punto,
    y = "variable_observada",
    prediccion = "salida_aprendida",
    error = "diferencia_observado_predicho",
    loss = "tension_global_aprendizaje",
    grad_norm = "presion_ajuste",
    eta = "intensidad_adaptativa",
    "punto_trazable"
  )

  funcion <- switch(
    punto,
    y = "referencia_del_sistema",
    prediccion = "aproximacion_modelo",
    error = "senal_desajuste",
    loss = "criterio_optimizacion",
    grad_norm = "senal_direccion_cambio",
    eta = "mecanismo_control_aprendizaje",
    "funcion_no_clasificada"
  )

  data.frame(
    punto = punto,
    sistema_referencial = c(
      "sistema_observado",
      "sistema_aprendizaje",
      "sistema_interpretacion_adtr"
    ),
    representacion = c(
      "serie_en_trayectoria",
      "senal_interna_o_salida",
      "punto_referencial_adtr"
    ),
    rol = rol,
    funcion = funcion,
    n = length(valores),
    valor_inicial = round(valores[1], 6),
    valor_final = round(valores[length(valores)], 6),
    variacion = round(valores[length(valores)] - valores[1], 6),
    trazable = TRUE,
    stringsAsFactors = FALSE
  )
}

construir_matriz_kp <- function(mp) {
  punto <- unique(mp$punto)

  sistemas <- mp$sistema_referencial

  pares <- expand.grid(
    origen = sistemas,
    destino = sistemas,
    stringsAsFactors = FALSE
  )

  pares <- pares[pares$origen != pares$destino, ]

  data.frame(
    punto = punto,
    origen = pares$origen,
    destino = pares$destino,
    tipo_transformacion = "transformacion_referencial",
    interpretacion = paste(
      "El punto", punto,
      "cambia de lectura entre", pares$origen,
      "y", pares$destino
    ),
    stringsAsFactors = FALSE
  )
}

integrar_matrices_mp <- function(lista_mp) {
  do.call(rbind, lista_mp)
}

calcular_metricas_adtr <- function(MP, lista_kp) {
  puntos <- unique(MP$punto)

  n_puntos <- length(puntos)
  n_filas_mp <- nrow(MP)
  n_transformaciones <- sum(vapply(lista_kp, nrow, numeric(1)))
  cobertura_referencial <- mean(table(MP$punto))
  diversidad_roles <- length(unique(MP$rol))
  diversidad_funcional <- length(unique(MP$funcion))

  data.frame(
    n_puntos = n_puntos,
    n_filas_mp = n_filas_mp,
    n_transformaciones = n_transformaciones,
    cobertura_referencial_promedio = cobertura_referencial,
    diversidad_roles = diversidad_roles,
    diversidad_funcional = diversidad_funcional,
    trazabilidad_completa = all(MP$trazable),
    stringsAsFactors = FALSE
  )
}

construir_red_adtr <- function(puntos) {
  nodos <- data.frame(
    id = puntos,
    tipo = "punto_referencial",
    stringsAsFactors = FALSE
  )

  aristas <- expand.grid(
    from = puntos,
    to = puntos,
    stringsAsFactors = FALSE
  )

  aristas <- aristas[aristas$from != aristas$to, ]

  aristas$tipo_relacion <- "relacion_transformacional_exploratoria"
  aristas$peso <- 1

  list(
    nodos = nodos,
    aristas = aristas,
    n_nodos = nrow(nodos),
    n_aristas = nrow(aristas)
  )
}

evaluar_madurez_ecosistemica_adtr <- function(metricas, red) {
  if (
    metricas$trazabilidad_completa &&
    red$n_nodos >= 4 &&
    red$n_aristas >= 8 &&
    metricas$diversidad_funcional >= 4
  ) {
    madurez <- "base_ecosistemica_alta"
  } else {
    madurez <- "base_ecosistemica_inicial"
  }

  data.frame(
    madurez = madurez,
    n_nodos = red$n_nodos,
    n_aristas = red$n_aristas,
    trazabilidad_completa = metricas$trazabilidad_completa,
    advertencia = "Madurez exploratoria; no implica causalidad demostrada.",
    stringsAsFactors = FALSE
  )
}

evaluar_dinamica_nivel4 <- function(obj) {
  tr <- obj$trayectoria

  d_loss <- diff(tr$loss)
  d_grad <- diff(tr$grad_norm)
  d_eta <- diff(tr$eta)

  cor_loss_grad <- suppressWarnings(cor(d_loss, d_grad))
  cor_grad_eta <- suppressWarnings(cor(d_grad, d_eta))

  evidencia_nivel4 <- (
    is.finite(cor_loss_grad) &&
      is.finite(cor_grad_eta) &&
      abs(cor_loss_grad) > 0.20
  )

  data.frame(
    cor_delta_loss_delta_grad = round(cor_loss_grad, 4),
    cor_delta_grad_delta_eta = round(cor_grad_eta, 4),
    nivel4 = evidencia_nivel4,
    lectura = ifelse(
      evidencia_nivel4,
      "relacion_dinamica_exploratoria",
      "relacion_dinamica_no_suficiente"
    ),
    advertencia = "Nivel 4 no equivale a causalidad.",
    stringsAsFactors = FALSE
  )
}

evaluar_acoplamiento_nivel5 <- function(madurez, nivel4) {
  nivel5 <- (
    madurez$madurez == "base_ecosistemica_alta" &&
      isTRUE(nivel4$nivel4)
  )

  data.frame(
    nivel5 = nivel5,
    decision_funcional = ifelse(
      nivel5,
      "posible_ecosistema_dinamico_aprendido_exploratorio",
      "evidencia_insuficiente_para_nivel5"
    ),
    advertencia = "Nivel 5 es acoplamiento interpretado exploratorio; no causalidad demostrada.",
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------
# 3. Ejecución del flujo ADTR
# ------------------------------------------------------------

validacion <- validar_objeto_adtr(ajuste_ds)

puntos <- inspeccionar_puntos_adtr(ajuste_ds)

puntos_eval <- c("y", "loss", "grad_norm", "eta")

lista_mp <- setNames(
  lapply(puntos_eval, function(p) construir_matriz_mp(ajuste_ds, p)),
  puntos_eval
)

lista_kp <- setNames(
  lapply(lista_mp, construir_matriz_kp),
  puntos_eval
)

MP <- integrar_matrices_mp(lista_mp)

metricas_adtr <- calcular_metricas_adtr(MP, lista_kp)

red_adtr <- construir_red_adtr(puntos_eval)

madurez <- evaluar_madurez_ecosistemica_adtr(metricas_adtr, red_adtr)

nivel4 <- evaluar_dinamica_nivel4(ajuste_ds)

nivel5 <- evaluar_acoplamiento_nivel5(madurez, nivel4)


# ------------------------------------------------------------
# 4. Mostrar resultados
# ------------------------------------------------------------

cat("\n================ VALIDACIÓN ADTR ================\n")
print(validacion)

cat("\n================ PUNTOS TRAZABLES ================\n")
print(puntos)

cat("\n================ M(P) INTEGRADA ================\n")
print(MP)

cat("\n================ MÉTRICAS ADTR ================\n")
print(metricas_adtr)

cat("\n================ RED ADTR: NODOS ================\n")
print(red_adtr$nodos)

cat("\n================ RED ADTR: ARISTAS ================\n")
print(red_adtr$aristas)

cat("\n================ MADUREZ ECOSISTÉMICA ================\n")
print(madurez)

cat("\n================ NIVEL 4 ================\n")
print(nivel4)

cat("\n================ NIVEL 5 ================\n")
print(nivel5)


# ------------------------------------------------------------
# 5. Exportar salidas
# ------------------------------------------------------------

dir.create("outputs_adtr", showWarnings = FALSE)

write.csv(MP, "outputs_adtr/tabla_MP_integrada.csv", row.names = FALSE)
write.csv(metricas_adtr, "outputs_adtr/metricas_adtr.csv", row.names = FALSE)
write.csv(red_adtr$nodos, "outputs_adtr/red_adtr_nodos.csv", row.names = FALSE)
write.csv(red_adtr$aristas, "outputs_adtr/red_adtr_aristas.csv", row.names = FALSE)
write.csv(madurez, "outputs_adtr/madurez_ecosistemica.csv", row.names = FALSE)
write.csv(nivel4, "outputs_adtr/nivel4_relacion_dinamica.csv", row.names = FALSE)
write.csv(nivel5, "outputs_adtr/nivel5_acoplamiento_exploratorio.csv", row.names = FALSE)

saveRDS(
  list(
    ajuste_ds = ajuste_ds,
    validacion = validacion,
    puntos = puntos,
    MP = MP,
    lista_mp = lista_mp,
    lista_kp = lista_kp,
    metricas_adtr = metricas_adtr,
    red_adtr = red_adtr,
    madurez = madurez,
    nivel4 = nivel4,
    nivel5 = nivel5
  ),
  "outputs_adtr/objeto_flujo_adtr_completo.rds"
)

cat("\nSalidas exportadas en la carpeta: outputs_adtr/\n")
