# ADTR.DSNeuralRNAS

**ADTR.DSNeuralRNAS** es una capa funcional de trazabilidad, interpretación, integración y evaluación exploratoria del **Aprendizaje Dinámico Transformacional-Referencial (ADTR)** sobre objetos, trayectorias y salidas generadas por `DSNeuralRNAS` y `ML.DSNeuralRNAS`.

El proyecto **no entrena modelos**, **no recalcula predicciones** y **no reemplaza** la funcionalidad de las librerías base. Su propósito es leer resultados ya generados y transformarlos en estructuras interpretables: puntos referenciales, matrices multirreferenciales, transformaciones, métricas, redes, madurez ecosistémica y acoplamiento interpretado exploratorio.

---

## 1. Idea central

ADTR propone que un proceso de aprendizaje no debe evaluarse solo por su predicción final, sino por la trayectoria transformacional que conecta:

```text
dato observado
→ representación aprendida
→ señal de control
→ evidencia aprendida
→ red transformacional
→ base ecosistémica
→ acoplamiento interpretado exploratorio
```

El flujo funcional validado es:

```text
validación
→ inspección
→ puntos trazables
→ matriz M(p)
→ matriz K(p)
→ métricas integradas
→ colección M(P)
→ red transformacional G_ADTR
→ madurez ecosistémica
→ relación dinámica de Nivel 4
→ acoplamiento interpretado exploratorio de Nivel 5
→ reporte y exportación
```

---

## 2. Estructura funcional del proyecto

```text
ADTR.DSNeuralRNAS/
├── R/
│   ├── 01_validacion_adtr.R
│   ├── 02_inspeccion_puntos_adtr.R
│   ├── 03_matrices_adtr.R
│   ├── 04_transformaciones_adtr.R
│   ├── 05_metricas_adtr.R
│   ├── 06_conjuntos_adtr.R
│   ├── 07_redes_adtr.R
│   ├── 08_ecosistema_adtr.R
│   ├── 09_dinamica_acoplamiento_adtr.R
│   └── 10_reportes_adtr.R
│
├── scripts/
│   ├── test_01_validacion_adtr.R
│   ├── test_02_inspeccion_puntos_adtr.R
│   ├── test_03_matrices_adtr.R
│   ├── test_04_transformaciones_adtr.R
│   ├── test_05_metricas_adtr.R
│   ├── test_06_conjuntos_adtr.R
│   ├── test_07_redes_adtr.R
│   ├── test_08_ecosistema_adtr.R
│   ├── test_09_dinamica_acoplamiento_adtr.R
│   ├── test_10_reportes_adtr.R
│   └── 11_prueba_flujo_funcional_adtr.R
│
└── outputs/
    ├── tables/
    ├── results/
    ├── figures/
    └── reports/
```

---

## 3. Carga recomendada en R

Ubíquese en la carpeta raíz del proyecto:

```r
setwd("ADTR.DSNeuralRNAS")
```

Si el proyecto está formalizado como paquete R, use:

```r
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  stop("Se requiere devtools para cargar el proyecto como paquete en desarrollo.")
}
```

Si aún no se ha formalizado como paquete, puede cargar los archivos funcionales manualmente:

```r
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
```

> Recomendación: si se trabaja como paquete, evitar `source("R/...")` antes de `devtools::document()` o `devtools::load_all()`, porque pueden aparecer conflictos por funciones duplicadas en el entorno global.

---

## 4. Documentación con roxygen2

Los archivos funcionales en `/R` están documentados con `roxygen2`. Para regenerar documentación y `NAMESPACE`:

```r
devtools::document()
```

Luego cargue el paquete en modo desarrollo:

```r
devtools::load_all(".")
```

Si aparecen conflictos por funciones ya cargadas con `source()`, reinicie la sesión de R o limpie el entorno:

```r
objetos_adtr <- ls(.GlobalEnv)
objetos_adtr <- objetos_adtr[grepl("_adtr$|^adtr_", objetos_adtr)]
rm(list = objetos_adtr, envir = .GlobalEnv)
```

---

## 5. Ejecución de pruebas individuales

Cada archivo funcional tiene un script de prueba asociado. Se recomienda ejecutar en orden:

```r
source("scripts/test_01_validacion_adtr.R")
source("scripts/test_02_inspeccion_puntos_adtr.R")
source("scripts/test_03_matrices_adtr.R")
source("scripts/test_04_transformaciones_adtr.R")
source("scripts/test_05_metricas_adtr.R")
source("scripts/test_06_conjuntos_adtr.R")
source("scripts/test_07_redes_adtr.R")
source("scripts/test_08_ecosistema_adtr.R")
source("scripts/test_09_dinamica_acoplamiento_adtr.R")
source("scripts/test_10_reportes_adtr.R")
```

Cada test genera salidas en:

```text
outputs/tables/
outputs/results/
outputs/figures/
outputs/reports/
```

---

## 6. Ejecución del flujo funcional completo

El script integrador es:

```text
scripts/11_prueba_flujo_funcional_adtr.R
```

Ejecución:

```r
source("scripts/11_prueba_flujo_funcional_adtr.R")
```

Este script valida el flujo funcional completo:

```text
objeto demostrativo
→ inspección
→ puntos trazables
→ M(p)
→ K(p)
→ métricas integradas
→ M(P)
→ G_ADTR
→ madurez ecosistémica
→ Nivel 4 dinámico
→ Nivel 5 exploratorio
→ reporte final
```

Salidas esperadas:

```text
outputs/tables/test_11_prueba_flujo_funcional_adtr.csv
outputs/tables/test_11_resumen_textual_flujo_adtr.csv
outputs/tables/test_11_archivos_exportados_flujo_adtr.csv
outputs/tables/test_11_indice_archivos_adtr.csv
outputs/results/test_11_flujo_funcional_adtr.rds
outputs/figures/test_11_red_transformacional_adtr.png
outputs/reports/test_11_reporte_flujo_funcional_adtr.txt
```

---

## 7. Ejemplo mínimo de uso

El siguiente ejemplo reproduce un objeto demostrativo de aprendizaje y ejecuta las etapas principales.

```r
# Cargar funcionalidad
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

# Objeto demostrativo
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

# Validar objeto
validar_objeto_aprendizaje_adtr(
  ajuste_demo,
  componentes_requeridos = c("unidad", "preparado", "prediccion", "trayectoria", "metricas")
)

# Construir matrices M(p)
matriz_y <- construir_matriz_variable_aprendida_adtr(
  objeto = ajuste_demo,
  nombre_objeto = "ajuste_demo"
)

coleccion_adtr <- construir_matrices_desde_objeto_adtr(
  objeto = ajuste_demo,
  nombre_objeto = "ajuste_demo"
)

# Analizar K(y) y métricas integradas
resultado_transformaciones_y <- analizar_transformaciones_adtr(matriz_y)
resultado_metricas_y <- analizar_metricas_integradas_adtr(matriz_y)

# Analizar conjunto M(P)
resultado_conjunto <- analizar_conjunto_adtr(coleccion_adtr)

# Construir red G_ADTR
resultado_red <- analizar_red_adtr(
  coleccion_adtr = resultado_conjunto$coleccion_adtr,
  relaciones_puntos = resultado_conjunto$relaciones_puntos,
  metricas_puntos = resultado_conjunto$metricas_puntos,
  puntos_criticos = resultado_conjunto$puntos_criticos,
  graficar = TRUE,
  archivo_figura = "outputs/figures/red_transformacional_adtr.png"
)

# Evaluar madurez ecosistémica preliminar
resultado_ecosistema <- analizar_madurez_ecosistemica_adtr(
  nodos = resultado_red$nodos,
  aristas = resultado_red$aristas,
  coleccion_adtr = resultado_conjunto$coleccion_adtr
)

# Evaluar dinámica y acoplamiento interpretado exploratorio
resultado_dinamica <- analizar_dinamica_acoplamiento_adtr(
  trayectoria = ajuste_demo$trayectoria
)

# Integrar resultado
resultado_flujo <- list(
  metricas_puntos = resultado_conjunto$metricas_puntos,
  nodos = resultado_red$nodos,
  aristas = resultado_red$aristas,
  nodos_centrales = resultado_red$nodos_centrales,
  indice_madurez = resultado_ecosistema$indice_madurez,
  escala_actualizada = resultado_dinamica$escala_actualizada,
  evidencia_nivel5 = resultado_dinamica$evidencia_nivel5,
  decision_acoplamiento = resultado_dinamica$decision_acoplamiento
)

# Generar resumen y reporte
resumen_textual <- generar_resumen_textual_adtr(resultado_flujo)
print(resumen_textual)

generar_reporte_flujo_adtr(
  resultado = resultado_flujo,
  titulo = "Reporte funcional ADTR",
  archivo = "outputs/reports/reporte_funcional_adtr.txt"
)

exportar_resultado_adtr(
  resultado = resultado_flujo,
  nombre_base = "resultado_adtr"
)
```

---

## 8. Resultados esperados del flujo validado

El flujo funcional completo validado produce los siguientes resultados esperados:

```text
M(P) integrada: 14 filas
Puntos evaluados: y, loss, grad_norm, eta
K(y): 20 transformaciones
Red transformacional: 4 nodos y 12 aristas
Nodos centrales: y y loss
Madurez ecosistémica: base_ecosistemica_alta
Relación dinámica: Nivel 4 demostrado
Acoplamiento interpretado: Nivel 5 exploratorio
Decisión funcional: posible_ecosistema_dinamico_aprendido_exploratorio
Trazabilidad: completa
```

---

## 9. Advertencia metodológica

El Nivel 5 de ADTR se interpreta como **acoplamiento interpretado exploratorio**, no como causalidad demostrada.

```text
acoplamiento interpretado exploratorio ≠ causalidad
```

Para afirmar acoplamientos robustos se requieren trayectorias más extensas, objetos reales, sensibilidad, comparación de modelos, validación externa y criterios adicionales.

---

## 10. Relación con DSNeuralRNAS y ML.DSNeuralRNAS

`ADTR.DSNeuralRNAS` debe utilizarse después de obtener objetos de aprendizaje con `DSNeuralRNAS` o `ML.DSNeuralRNAS`.

La relación esperada es:

```text
DSNeuralRNAS / ML.DSNeuralRNAS
→ entrenamiento, trayectoria, predicción, métricas
→ ADTR.DSNeuralRNAS
→ trazabilidad, matrices, redes, madurez, acoplamiento, reporte
```

Por tanto, ADTR no compite con las librerías base. Las complementa como capa interpretativa y sistémica.

---

## 11. Estado actual

Estado funcional alcanzado:

```text
Arquitectura funcional completa: validada
Tests 01 al 10: validados
Test 11 integrador: validado
Documentación roxygen2: habilitada
Reporte y exportación: habilitados
```

El proyecto está listo para servir como base computacional del libro formal:

```text
ADTR-DSNeuralRNAS: teoría, formalización, arquitectura funcional y validación exploratoria
```
