# ============================================================
# Proyecto: ADTR.DSNeuralRNAS
# Script: 11_prueba_flujo_funcional_adtr.R
# Autor: Rubén Alexander More Valencia
# Propósito:
#   Probar el flujo funcional completo ADTR usando los archivos
#   funcionales ubicados en /R.
#
#   Este script integra validación, inspección, matrices, transformaciones,
#   métricas, conjuntos, redes, madurez ecosistémica, dinámica,
#   acoplamiento interpretado exploratorio y reportes.
#
#   No entrena modelos ni duplica funcionalidad de DSNeuralRNAS o
#   ML.DSNeuralRNAS. Trabaja sobre un objeto demostrativo ya generado.
# ============================================================

cat("\n============================================================\n")
cat("Test 11 - Flujo funcional completo ADTR\n")
cat("============================================================\n")

# ============================================================
# 0. Carga de funcionalidad
# ============================================================

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  source("R/01_validacion_adtr.R")
  source("R/02_inspeccion_puntos_adtr.R")
  source("R/03_matrices_adtr.R")
  source("R/04_transformaciones_adtr.R")
  source("R/05_metricas_adtr.R")
  source("R/06_conjuntos_adtr.R")
  source("R/07_redes_adtr.R")
  source("R/08_ecosistema_adtr.R")
  source("R/09_dinamica_acoplamiento_adtr.R")
  source("R/10_reportes_adtr.R")
}

crear_directorios_adtr()

# ============================================================
# 1. Objeto demostrativo de aprendizaje
# ============================================================

ajuste_demo <- list(
  unidad = list(
    variable_objetivo = "y",
    entradas = c("x1", "x2"),
    modelo = "mlp_simple"
  ),
  preparado = data.frame(
    x1 = c(0.10, 0.20, 0.35),
    x2 = c(0.40, 0.30, 0.15),
    y = c(0.50, 0.60, 0.70)
  ),
  params_finales = list(
    pesos = c(0.25, -0.10),
    sesgo = 0.05
  ),
  prediccion = c(0.48, 0.62, 0.69),
  trayectoria = data.frame(
    iter = 1:5,
    loss = c(0.120, 0.080, 0.050, 0.030, 0.020),
    grad_norm = c(0.90, 0.65, 0.40, 0.22, 0.10),
    eta = c(0.10, 0.10, 0.08, 0.08, 0.05)
  ),
  metricas = list(
    loss_inicial = 0.120,
    loss_final = 0.020,
    reduccion_rel = 0.8333
  ),
  creado = Sys.time()
)

# Validación mínima del objeto
validar_objeto_aprendizaje_adtr(
  ajuste_demo,
  componentes_requeridos = c("unidad", "preparado", "prediccion", "trayectoria", "metricas")
)

# ============================================================
# 2. Inspección e identificación de puntos
# ============================================================

inspeccion <- inspeccionar_objeto_adtr(
  objeto = ajuste_demo,
  nombre_objeto = "ajuste_demo"
)

puntos_identificados <- identificar_puntos_adtr(
  objeto = ajuste_demo,
  nombre_objeto = "ajuste_demo"
)

puntos_trayectoria <- identificar_puntos_trayectoria_adtr(
  trayectoria = ajuste_demo$trayectoria,
  nombre_objeto = "ajuste_demo"
)

puntos_trazables <- seleccionar_puntos_trazables_adtr(
  puntos_identificados
)

resumen_puntos <- resumir_puntos_identificados_adtr(
  puntos_identificados
)

# ============================================================
# 3. Construcción de matrices M(p)
# ============================================================

matriz_y <- construir_matriz_variable_aprendida_adtr(
  objeto = ajuste_demo,
  nombre_objeto = "ajuste_demo"
)

matriz_trayectoria <- construir_matriz_trayectoria_adtr(
  trayectoria = ajuste_demo$trayectoria,
  nombre_objeto = "ajuste_demo"
)

coleccion_adtr <- construir_matrices_desde_objeto_adtr(
  objeto = ajuste_demo,
  nombre_objeto = "ajuste_demo"
)

# ============================================================
# 4. Transformaciones K(p) y métricas integradas para y
# ============================================================

resultado_transformaciones_y <- analizar_transformaciones_adtr(
  matriz_adtr = matriz_y
)

resultado_metricas_y <- analizar_metricas_integradas_adtr(
  matriz_adtr = matriz_y
)

# ============================================================
# 5. Colección multirreferencial M(P)
# ============================================================

resultado_conjunto <- analizar_conjunto_adtr(
  coleccion_adtr = coleccion_adtr
)

# ============================================================
# 6. Red transformacional G_ADTR(P,E)
# ============================================================

resultado_red <- analizar_red_adtr(
  coleccion_adtr = resultado_conjunto$coleccion_adtr,
  relaciones_puntos = resultado_conjunto$relaciones_puntos,
  metricas_puntos = resultado_conjunto$metricas_puntos,
  puntos_criticos = resultado_conjunto$puntos_criticos,
  umbral_centralidad = 0.80,
  graficar = TRUE,
  archivo_figura = "outputs/figures/test_11_red_transformacional_adtr.png"
)

# ============================================================
# 7. Madurez ecosistémica preliminar
# ============================================================

resultado_ecosistema <- analizar_madurez_ecosistemica_adtr(
  nodos = resultado_red$nodos,
  aristas = resultado_red$aristas,
  coleccion_adtr = resultado_conjunto$coleccion_adtr
)

# ============================================================
# 8. Dinámica y acoplamiento interpretado exploratorio
# ============================================================

resultado_dinamica <- analizar_dinamica_acoplamiento_adtr(
  trayectoria = ajuste_demo$trayectoria
)

# ============================================================
# 9. Resultado integrado del flujo
# ============================================================

resultado_flujo <- list(
  inspeccion = inspeccion$resumen_componentes,
  puntos_identificados = puntos_identificados,
  puntos_trayectoria = puntos_trayectoria,
  puntos_trazables = puntos_trazables,
  resumen_puntos = resumen_puntos,

  matriz_y = matriz_y,
  matriz_trayectoria = matriz_trayectoria,
  coleccion_adtr = resultado_conjunto$coleccion_adtr,

  matriz_transformaciones_y = resultado_transformaciones_y$matriz_transformaciones,
  resumen_transformaciones_y = resultado_transformaciones_y$resumen_transformaciones,
  metricas_transformacionales_y = resultado_transformaciones_y$metricas_transformacionales,
  interpretacion_transformaciones_y = resultado_transformaciones_y$interpretacion,

  metricas_referenciales_y = resultado_metricas_y$metricas_referenciales,
  metricas_integradas_y = resultado_metricas_y$metricas_integradas,
  indice_complejidad_y = resultado_metricas_y$indice_complejidad,
  interpretacion_metricas_y = resultado_metricas_y$interpretacion,

  metricas_puntos = resultado_conjunto$metricas_puntos,
  puntos_criticos = resultado_conjunto$puntos_criticos,
  agrupacion_sistemas = resultado_conjunto$agrupacion_sistemas,
  agrupacion_funciones = resultado_conjunto$agrupacion_funciones,
  relaciones_puntos = resultado_conjunto$relaciones_puntos,
  interpretacion_conjunto = resultado_conjunto$interpretacion,

  nodos = resultado_red$nodos,
  aristas = resultado_red$aristas,
  metricas_red = resultado_red$metricas_red,
  matriz_adyacencia = resultado_red$matriz_adyacencia,
  resumen_relaciones = resultado_red$resumen_relaciones,
  nodos_centrales = resultado_red$nodos_centrales,
  interpretacion_red = resultado_red$interpretacion,

  indicadores_madurez = resultado_ecosistema$indicadores,
  indice_madurez = resultado_ecosistema$indice_madurez,
  niveles_relacion_preliminar = resultado_ecosistema$niveles_relacion,
  interpretacion_madurez = resultado_ecosistema$interpretacion,
  decision_ecosistema = resultado_ecosistema$decision_funcional,

  variaciones = resultado_dinamica$variaciones,
  coordinacion = resultado_dinamica$coordinacion,
  correlaciones = resultado_dinamica$correlaciones,
  estabilizacion = resultado_dinamica$estabilizacion,
  relaciones_nivel4 = resultado_dinamica$relaciones_nivel4,
  niveles_actualizados = resultado_dinamica$niveles_actualizados,
  interpretacion_dinamica = resultado_dinamica$interpretacion_dinamica,
  decision_dinamica = resultado_dinamica$decision_dinamica,

  tabla_acoplamiento = resultado_dinamica$tabla_acoplamiento,
  regla_funcional = resultado_dinamica$regla_funcional,
  consistencia = resultado_dinamica$consistencia,
  dependencia = resultado_dinamica$dependencia,
  evidencia_nivel5 = resultado_dinamica$evidencia_nivel5,
  escala_actualizada = resultado_dinamica$escala_actualizada,
  interpretacion_acoplamiento = resultado_dinamica$interpretacion_acoplamiento,
  decision_acoplamiento = resultado_dinamica$decision_acoplamiento
)

# ============================================================
# 10. Reporte y exportación
# ============================================================

resumen_textual <- generar_resumen_textual_adtr(
  resultado = resultado_flujo
)

reporte_lineas <- generar_reporte_flujo_adtr(
  resultado = resultado_flujo,
  titulo = "Reporte del flujo funcional completo ADTR",
  archivo = "outputs/reports/test_11_reporte_flujo_funcional_adtr.txt"
)

archivos_exportados <- exportar_resultado_adtr(
  resultado = resultado_flujo,
  nombre_base = "test_11_flujo_funcional_adtr"
)

indice_archivos <- crear_indice_archivos_adtr()

# ============================================================
# 11. Impresión de resultados clave
# ============================================================

cat("\nResumen de puntos inspeccionados:\n")
print(resumen_puntos)

cat("\nMatriz integrada M(P):\n")
print(resultado_conjunto$coleccion_adtr)

cat("\nMétricas por punto:\n")
print(resultado_conjunto$metricas_puntos)

cat("\nRed transformacional - métricas:\n")
print(resultado_red$metricas_red)

cat("\nMadurez ecosistémica:\n")
print(resultado_ecosistema$indice_madurez)

cat("\nNivel 4 - relaciones dinámicas:\n")
print(resultado_dinamica$relaciones_nivel4)

cat("\nNivel 5 - evidencia de acoplamiento:\n")
print(resultado_dinamica$evidencia_nivel5)

cat("\nResumen textual del flujo:\n")
print(resumen_textual)

# ============================================================
# 12. Pruebas lógicas integradas
# ============================================================

prueba_inspeccion <- nrow(inspeccion$resumen_componentes) >= 5
prueba_puntos <- nrow(puntos_trazables) >= 4
prueba_matriz_y <- nrow(matriz_y) == 5
prueba_matriz_integrada <- nrow(resultado_conjunto$coleccion_adtr) == 14
prueba_k_y <- nrow(resultado_transformaciones_y$matriz_transformaciones) == 20
prueba_indice_y <- resultado_metricas_y$indice_complejidad$indice_complejidad_adtr >= 0 &&
  resultado_metricas_y$indice_complejidad$indice_complejidad_adtr <= 1
prueba_conjunto_4_puntos <- nrow(resultado_conjunto$metricas_puntos) == 4
prueba_red_4_nodos <- nrow(resultado_red$nodos) == 4
prueba_red_12_aristas <- nrow(resultado_red$aristas) == 12
prueba_centrales <- all(c("y", "loss") %in% resultado_red$nodos_centrales$punto[resultado_red$nodos_centrales$nodo_central])
prueba_madurez_alta <- resultado_ecosistema$indice_madurez$nivel_madurez == "base_ecosistemica_alta"
prueba_indice_madurez_unitario <- resultado_ecosistema$indice_madurez$indice_madurez_ecosistemica >= 0 &&
  resultado_ecosistema$indice_madurez$indice_madurez_ecosistemica <= 1
prueba_nivel4 <- any(resultado_dinamica$niveles_actualizados$tipo == "relacion dinamica" & resultado_dinamica$niveles_actualizados$evidencia_actual)
prueba_nivel5 <- isTRUE(resultado_dinamica$evidencia_nivel5$evidencia_nivel5)
prueba_decision <- resultado_dinamica$decision_acoplamiento$decision == "posible_ecosistema_dinamico_aprendido_exploratorio"
prueba_reporte <- file.exists("outputs/reports/test_11_reporte_flujo_funcional_adtr.txt")
prueba_exportacion <- any(archivos_exportados$tipo == "rds") &&
  file.exists(archivos_exportados$archivo[archivos_exportados$tipo == "rds"][1])
prueba_figura <- file.exists("outputs/figures/test_11_red_transformacional_adtr.png")
prueba_trazabilidad <- all(!is.na(resultado_conjunto$coleccion_adtr$fuente_programatica) &
  resultado_conjunto$coleccion_adtr$fuente_programatica != "")

resumen_test <- data.frame(
  prueba = c(
    "inspeccion_componentes",
    "selecciona_puntos_trazables",
    "matriz_y_5_filas",
    "matriz_integrada_14_filas",
    "k_y_20_transformaciones",
    "indice_y_unitario",
    "conjunto_4_puntos",
    "red_4_nodos",
    "red_12_aristas",
    "detecta_y_loss_centrales",
    "madurez_ecosistemica_alta",
    "indice_madurez_unitario",
    "nivel4_relacion_dinamica",
    "nivel5_acoplamiento_exploratorio",
    "decision_ecosistema_exploratorio",
    "genera_reporte_txt",
    "exporta_resultado_rds",
    "genera_figura_red",
    "mantiene_trazabilidad"
  ),
  resultado = c(
    prueba_inspeccion,
    prueba_puntos,
    prueba_matriz_y,
    prueba_matriz_integrada,
    prueba_k_y,
    prueba_indice_y,
    prueba_conjunto_4_puntos,
    prueba_red_4_nodos,
    prueba_red_12_aristas,
    prueba_centrales,
    prueba_madurez_alta,
    prueba_indice_madurez_unitario,
    prueba_nivel4,
    prueba_nivel5,
    prueba_decision,
    prueba_reporte,
    prueba_exportacion,
    prueba_figura,
    prueba_trazabilidad
  ),
  stringsAsFactors = FALSE
)

cat("\nResumen del test 11:\n")
print(resumen_test)

if (!all(resumen_test$resultado)) {
  stop("Alguna prueba del flujo funcional completo ADTR falló.")
}

write.csv(
  resumen_test,
  "outputs/tables/test_11_prueba_flujo_funcional_adtr.csv",
  row.names = FALSE
)

write.csv(
  resumen_textual,
  "outputs/tables/test_11_resumen_textual_flujo_adtr.csv",
  row.names = FALSE
)

write.csv(
  archivos_exportados,
  "outputs/tables/test_11_archivos_exportados_flujo_adtr.csv",
  row.names = FALSE
)

write.csv(
  indice_archivos,
  "outputs/tables/test_11_indice_archivos_adtr.csv",
  row.names = FALSE
)

saveRDS(
  resultado_flujo,
  "outputs/results/test_11_flujo_funcional_adtr.rds"
)

cat("\nTest 11 finalizado correctamente.\n")
cat("Flujo funcional ADTR completo validado.\n")
cat("Archivos generados en outputs/tables, outputs/results, outputs/figures y outputs/reports.\n")
cat("============================================================\n")
