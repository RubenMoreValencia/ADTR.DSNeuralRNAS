# ============================================================
# Test 10 - Reportes ADTR
# ============================================================

cat("\n============================================================\n")
cat("Test 10 - Reportes ADTR\n")
cat("============================================================\n")

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  source("R/10_reportes_adtr.R")
}

# Directorios de prueba
rutas <- crear_directorios_adtr()

# Resultado sintético similar al flujo ADTR completo
metricas_puntos <- data.frame(
  punto = c("y", "loss", "grad_norm", "eta"),
  cobertura_referencial = c(4, 3, 3, 3),
  indice_trazabilidad = c(1, 1, 1, 1),
  stringsAsFactors = FALSE
)

nodos <- data.frame(
  id = c("eta", "grad_norm", "loss", "y"),
  punto = c("eta", "grad_norm", "loss", "y"),
  stringsAsFactors = FALSE
)

aristas <- data.frame(
  from = c("loss", "y"),
  to = c("y", "loss"),
  tipo_relacion = c("relacion_sistemico_funcional", "relacion_sistemico_funcional"),
  peso = c(2, 2),
  stringsAsFactors = FALSE
)

nodos_centrales <- data.frame(
  punto = c("eta", "grad_norm", "loss", "y"),
  nodo_central = c(FALSE, FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

indice_madurez <- data.frame(
  indice_madurez_ecosistemica = 1,
  nivel_madurez = "base_ecosistemica_alta",
  stringsAsFactors = FALSE
)

escala_actualizada <- data.frame(
  nivel = paste("Nivel", 1:5),
  tipo = c(
    "coexistencia referencial",
    "relacion por sistema",
    "relacion funcional",
    "relacion dinamica",
    "acoplamiento interpretado"
  ),
  evidencia_actual = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

evidencia_nivel5 <- data.frame(
  tipo_acoplamiento = "acoplamiento_interpretado_fuerte",
  evidencia_nivel5 = TRUE,
  stringsAsFactors = FALSE
)

decision_acoplamiento <- data.frame(
  decision = "posible_ecosistema_dinamico_aprendido_exploratorio",
  justificacion = "Evidencia exploratoria; no causalidad.",
  stringsAsFactors = FALSE
)

resultado <- list(
  metricas_puntos = metricas_puntos,
  nodos = nodos,
  aristas = aristas,
  nodos_centrales = nodos_centrales,
  indice_madurez = indice_madurez,
  escala_actualizada = escala_actualizada,
  evidencia_nivel5 = evidencia_nivel5,
  decision_acoplamiento = decision_acoplamiento
)

# Exportar tabla individual
archivo_tabla <- exportar_tabla_adtr(
  metricas_puntos,
  archivo = "test_10_metricas_puntos_adtr.csv"
)

# Exportar resultado completo
exportados <- exportar_resultado_adtr(
  resultado,
  nombre_base = "test_10_resultado_reportes_adtr"
)

# Generar resumen y reporte
resumen_textual <- generar_resumen_textual_adtr(resultado)

lineas_reporte <- generar_reporte_flujo_adtr(
  resultado,
  titulo = "Reporte de prueba ADTR",
  archivo = "outputs/reports/test_10_reporte_flujo_adtr.txt"
)

indice_archivos <- crear_indice_archivos_adtr()

cat("\nRutas creadas:\n")
print(rutas)

cat("\nArchivo tabla individual:\n")
print(archivo_tabla)

cat("\nArchivos exportados:\n")
print(exportados)

cat("\nResumen textual:\n")
print(resumen_textual)

cat("\nReporte textual:\n")
print(lineas_reporte)

cat("\nIndice de archivos:\n")
print(indice_archivos)

# Pruebas lógicas mínimas
prueba_directorios <- all(dir.exists(file.path("outputs", c("tables", "results", "figures", "reports"))))
prueba_tabla_exportada <- file.exists(archivo_tabla)
prueba_rds_exportado <- any(exportados$tipo == "rds") && file.exists(exportados$archivo[exportados$tipo == "rds"][1])
prueba_csv_exportados <- sum(exportados$tipo == "csv") >= 5
prueba_resumen <- nrow(resumen_textual) >= 5
prueba_reporte <- file.exists("outputs/reports/test_10_reporte_flujo_adtr.txt") && length(lineas_reporte) > 3
prueba_indice <- nrow(indice_archivos) > 0
prueba_nivel5_texto <- any(grepl("Nivel 5", resumen_textual$interpretacion))
prueba_decision_texto <- any(grepl("posible_ecosistema", resumen_textual$interpretacion))

error_tabla_no_dataframe <- tryCatch({
  exportar_tabla_adtr(list(a = 1), "tabla_mala.csv")
  FALSE
}, error = function(e) TRUE)

error_resultado_no_lista <- tryCatch({
  exportar_resultado_adtr(data.frame(x = 1))
  FALSE
}, error = function(e) TRUE)

error_reporte_titulo <- tryCatch({
  generar_reporte_flujo_adtr(resultado, titulo = "")
  FALSE
}, error = function(e) TRUE)

resumen_test <- data.frame(
  prueba = c(
    "crea_directorios",
    "exporta_tabla",
    "exporta_rds",
    "exporta_csv_componentes",
    "genera_resumen_textual",
    "genera_reporte_txt",
    "crea_indice_archivos",
    "resumen_detecta_nivel5",
    "resumen_detecta_decision",
    "detecta_tabla_no_dataframe",
    "detecta_resultado_no_lista",
    "detecta_titulo_vacio"
  ),
  resultado = c(
    prueba_directorios,
    prueba_tabla_exportada,
    prueba_rds_exportado,
    prueba_csv_exportados,
    prueba_resumen,
    prueba_reporte,
    prueba_indice,
    prueba_nivel5_texto,
    prueba_decision_texto,
    error_tabla_no_dataframe,
    error_resultado_no_lista,
    error_reporte_titulo
  ),
  stringsAsFactors = FALSE
)

cat("\nResumen del test 10:\n")
print(resumen_test)

if (!all(resumen_test$resultado)) {
  stop("Alguna prueba del test 10 fallo.")
}

write.csv(resumen_textual, "outputs/tables/test_10_resumen_textual_adtr.csv", row.names = FALSE)
write.csv(exportados, "outputs/tables/test_10_exportados_adtr.csv", row.names = FALSE)
write.csv(indice_archivos, "outputs/tables/test_10_indice_archivos_adtr.csv", row.names = FALSE)
write.csv(resumen_test, "outputs/tables/test_10_reportes_adtr.csv", row.names = FALSE)

saveRDS(
  list(
    resultado = resultado,
    resumen_textual = resumen_textual,
    lineas_reporte = lineas_reporte,
    exportados = exportados,
    indice_archivos = indice_archivos,
    resumen_test = resumen_test
  ),
  "outputs/results/test_10_reportes_adtr.rds"
)

cat("\nTest 10 finalizado correctamente.\n")
cat("Archivos generados en outputs/tables, outputs/results y outputs/reports.\n")
cat("============================================================\n")
