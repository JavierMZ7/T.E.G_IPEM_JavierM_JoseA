# ==============================================================================
# TESIS: ÍNDICE MULTIDIMENSIONAL DE PODER ECONÓMICO (I.M.P.E. 1995-2024)
# SCRIPT MAESTRO CONSOLIDADO Y AUDITABLE (20 ÍNDICES: 4 FAMILIAS x 5 MÉTODOS)
# ==============================================================================
# Autor(es): Javier Morros & Jose A. (Escuela de Economía - UCV)
# Revisión metodológica: Prof. Goncalves
#
# MEJORAS CLAVE RESPECTO A LA VERSIÓN ANTERIOR:
#   1. PCA con base::eigen(R) y stats::prcomp() (R base):
#      - Devuelve autovectores UNITARIOS cuya suma de cuadrados es exactamente 1.0000
#        (evita los coeficientes inflados de psych::principal() que multiplican por sqrt(lambda)).
#      - Incluye verificación automática prcomp() vs. eigen(R) en cada índice.
#   2. Visibilidad total en el Environment de RStudio:
#      - Todas las 20 matrices de correlación R quedan guardadas en `MATRICES_R`.
#      - Todos los objetos de `eigen(R)` quedan en `RESULTADOS_EIGEN`.
#      - Todos los objetos de `prcomp()` quedan en `RESULTADOS_PRCOMP`.
#      - Todos los paneles procesados quedan en `PANELES_PROCESADOS`.
#   3. Almacenamiento optimizado en Parquet (paquete `arrow`):
#      - Guarda las 4 familias depuradas y las imputaciones MICE en disco (.parquet).
#      - Si ya existen en `CARPETA_CACHE`, las recarga en segundos sin recalcular MICE.
#   4. Formato condicional en Excel:
#      - Resalta en verde los valores > 0.50 en las tablas comparativas y correlaciones.
# ==============================================================================

# Limpiar el Environment para asegurar una ejecución secuencial limpia
rm(list = ls())

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN GLOBAL Y PARÁMETROS
# ------------------------------------------------------------------------------
ARCHIVO_ENTRADA       <- "DATA/PANELDATA/panel_data_consolidado.csv"
ARCHIVO_SALIDA_XLSX   <- "IMPE_Comparativa_Maestra_20_Indices.xlsx"
CARPETA_CACHE         <- "datos_procesados_parquet"

RECALCULAR_INSUMOS    <- FALSE  # FALSE = carga .parquet si existen | TRUE = recalcula todo
ANIO_INICIO           <- 1995
ANIO_FIN              <- 2024
UMBRAL_NA_PAIS        <- 0.25   # Eliminar países con > 25% de NAs en familias 1.B y 2.B
MAX_GAP_INTERP        <- 3      # Máximo de años consecutivos a interpolar (Método 2)
M_IMPUTACIONES        <- 5      # Usar 5 para pruebas rápidas; subir a 20 para corrida final
MAXIT_MICE            <- 10     # Iteraciones por cadena en MICE
MIN_COB_FILA          <- 0.50   # Mínimo 50% de variables no-NA para puntuar una fila en Raw/Interp
SEMILLA               <- 2026

# ------------------------------------------------------------------------------
# 1. INSTALACIÓN Y CARGA DE PAQUETES (CON JUSTIFICACIÓN)
# ------------------------------------------------------------------------------
# - dplyr / tidyr / tibble : Manipulación ordenada de paneles de datos.
# - readr                  : Lectura rápida de archivos CSV.
# - arrow                  : Lectura/escritura en formato Apache Parquet (ligero y veloz).
# - countrycode            : Filtra agregados regionales del Banco Mundial (WLD, EUU, etc.).
# - zoo                    : Interpolación lineal estricta con na.approx(..., maxgap = 3).
# - mice / ranger          : Imputación múltiple (PMM, Bayesiana 'norm' y Random Forest 'rf').
# - psych                  : Solo para estadísticos de fiabilidad (Alfa de Cronbach, KMO, Bartlett).
#                            NOTA: El PCA se calcula con R base (eigen y prcomp), NO con psych.
# - Matrix                 : Provee nearPD() para proyectar matrices pairwise si tienen autovalor <= 0.
# - openxlsx               : Exportación a Excel (.xlsx) con formato condicional y sin Java.

paquetes_necesarios <- c(
  "dplyr", "tidyr", "tibble", "readr", "arrow", "countrycode",
  "zoo", "mice", "ranger", "psych", "Matrix", "openxlsx"
)

paquetes_faltantes <- paquetes_necesarios[!(paquetes_necesarios %in% installed.packages()[, "Package"])]
if (length(paquetes_faltantes) > 0) {
  message("Instalando paquetes faltantes: ", paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(arrow)
  library(countrycode)
  library(zoo)
  library(mice)
  library(ranger)
  library(psych)
  library(Matrix)
  library(openxlsx)
})

set.seed(SEMILLA)
if (!dir.exists(CARPETA_CACHE)) dir.create(CARPETA_CACHE, recursive = TRUE)

# ------------------------------------------------------------------------------
# 2. DEFINICIÓN DE VARIABLES (BASE 16, BASE 12 Y LOGARÍTMICAS)
# ------------------------------------------------------------------------------
vars_base16 <- c(
  "prod_manuf_pib",
  "prod_exp_total",
  "prod_fbcf_pib",
  "tech_mnf_val_added",
  "prod_eci",
  "prod_pib_pcap_ppa",
  "fin_ied_sal_pib",
  "fin_mkt_cap_pib",
  "fin_cred_priv_pib",
  "fin_regalias_rec",
  "tech_id_pib",
  "tech_investigad",
  "tech_exp_alta",
  "tech_serv_ict_pct",
  "tech_exp_serv_dig",
  "tech_patentes_tic"
)

vars_podadas_12 <- c(
  "prod_manuf_pib",
  "prod_fbcf_pib",
  "fin_ied_sal_pib",
  "tech_serv_ict_pct"
)

vars_base12 <- setdiff(vars_base16, vars_podadas_12)

vars_log <- c(
  "prod_exp_total",
  "prod_pib_pcap_ppa",
  "fin_regalias_rec",
  "tech_exp_serv_dig",
  "tech_patentes_tic",
  "tech_mnf_val_added",
  "ctrl_pib_nominal"
)

# ------------------------------------------------------------------------------
# 3. CARGA, DEPURACIÓN Y GUARDADO EN PARQUET DE LAS 4 FAMILIAS BASE
# ------------------------------------------------------------------------------
ruta_panel_base <- file.path(CARPETA_CACHE, "panel_depurado_1995_2024.parquet")

if (file.exists(ruta_panel_base) && !RECALCULAR_INSUMOS) {
  message(">>> Cargando panel depurado desde caché Parquet: ", ruta_panel_base)
  panel_proc <- arrow::read_parquet(ruta_panel_base)
} else {
  message(">>> Leyendo CSV original y depurando panel 1995-2024...")
  panel_raw <- readr::read_csv(ARCHIVO_ENTRADA, show_col_types = FALSE)

  cols_faltantes <- setdiff(c("iso3c", "country", "year", vars_base16), names(panel_raw))
  if (length(cols_faltantes) > 0) {
    stop("Faltan las siguientes columnas en el CSV: ", paste(cols_faltantes, collapse = ", "))
  }

  isos_soberanos <- countrycode::codelist$iso3c[!is.na(countrycode::codelist$iso3c)]

  panel_depurado <- panel_raw %>%
    filter(year >= ANIO_INICIO & year <= ANIO_FIN) %>%
    filter(!is.na(iso3c) & iso3c %in% isos_soberanos)

  if ("region" %in% names(panel_depurado)) {
    panel_depurado <- panel_depurado %>% filter(region != "Aggregates")
  }

  panel_depurado <- panel_depurado %>% arrange(iso3c, year)

  # Transformación log1p segura en variables de volumen (mantiene NAs intactos)
  cols_log_presentes <- intersect(vars_log, names(panel_depurado))
  panel_proc <- panel_depurado %>%
    mutate(across(
      all_of(cols_log_presentes),
      ~ ifelse(is.na(.x) | .x < 0, NA_real_, log1p(.x))
    ))

  arrow::write_parquet(panel_proc, ruta_panel_base)
  message("    Guardado en Parquet: ", ruta_panel_base)
}

# Función para filtrar países con <= 25% de NAs
filtrar_paises_na <- function(df, vars_modelo, umbral = UMBRAL_NA_PAIS, nombre_fam = "") {
  resumen_na <- df %>%
    mutate(na_fila = rowSums(is.na(across(all_of(vars_modelo))))) %>%
    group_by(iso3c) %>%
    summarise(
      pct_na = sum(na_fila) / (n() * length(vars_modelo)),
      .groups = "drop"
    )

  paises_validos <- resumen_na %>%
    filter(pct_na <= umbral) %>%
    pull(iso3c)

  message(sprintf(
    "    Diagnóstico NAs [%s]: mediana = %.1f%% | países <= %.0f%% NAs = %d de %d",
    nombre_fam,
    median(resumen_na$pct_na) * 100,
    umbral * 100,
    length(paises_validos),
    nrow(resumen_na)
  ))

  df %>% filter(iso3c %in% paises_validos)
}

FAMILIAS <- list(
  "1A_Base16_Completa"   = list(df = panel_proc, vars = vars_base16),
  "1B_Base16_Depurada25" = list(df = filtrar_paises_na(panel_proc, vars_base16, UMBRAL_NA_PAIS, "1B_Base16"), vars = vars_base16),
  "2A_Base12_Completa"   = list(df = panel_proc, vars = vars_base12),
  "2B_Base12_Depurada25" = list(df = filtrar_paises_na(panel_proc, vars_base12, UMBRAL_NA_PAIS, "2B_Base12"), vars = vars_base12)
)

for (nom in names(FAMILIAS)) {
  message(sprintf(
    "    Familia %-22s | Países: %3d | Filas: %4d | Variables: %2d",
    nom,
    n_distinct(FAMILIAS[[nom]]$df$iso3c),
    nrow(FAMILIAS[[nom]]$df),
    length(FAMILIAS[[nom]]$vars)
  ))
}

# ------------------------------------------------------------------------------
# 4. FUNCIONES AUXILIARES (ESTANDARIZACIÓN, INTERPOLACIÓN, MICE CON CACHÉ PARQUET)
# ------------------------------------------------------------------------------

# 4.1 Estandarización Z-score tolerante a NAs
estandarizar_z <- function(df, vars) {
  df %>%
    mutate(across(all_of(vars), ~ {
      mu <- mean(.x, na.rm = TRUE)
      sd_val <- sd(.x, na.rm = TRUE)
      if (is.na(sd_val) || sd_val == 0) rep(0, length(.x)) else (.x - mu) / sd_val
    }))
}

# 4.2 Interpolación lineal estricta (solo huecos internos <= maxgap años)
interpolar_panel <- function(df, vars, maxgap = MAX_GAP_INTERP) {
  df %>%
    group_by(iso3c) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(across(
      all_of(vars),
      ~ if (sum(!is.na(.x)) >= 2) {
        zoo::na.approx(.x, x = year, maxgap = maxgap, na.rm = FALSE)
      } else {
        .x
      }
    )) %>%
    ungroup() %>%
    arrange(iso3c, year)
}

# 4.3 Matriz de correlación R con pairwise deletion + ajuste nearPD si hace falta
correlacion_robusta_pairwise <- function(mat) {
  R <- stats::cor(mat, use = "pairwise.complete.obs")
  R[is.na(R)] <- 0
  diag(R) <- 1
  if (min(base::eigen(R, symmetric = TRUE, only.values = TRUE)$values) <= 1e-6) {
    R <- as.matrix(Matrix::nearPD(R, corr = TRUE, ensureSymmetry = TRUE)$mat)
  }
  R
}

# 4.4 Combinación de matrices R vía transformación z de Fisher (Van Buuren)
combinar_cor_fisher <- function(lista_R) {
  z_list <- lapply(lista_R, function(R) {
    R_clip <- pmin(pmax(R, -0.999999), 0.999999)
    z <- atanh(R_clip)
    diag(z) <- 0
    z
  })
  z_mean <- Reduce("+", z_list) / length(z_list)
  R_pool <- tanh(z_mean)
  diag(R_pool) <- 1
  if (min(base::eigen(R_pool, symmetric = TRUE, only.values = TRUE)$values) <= 1e-6) {
    R_pool <- as.matrix(Matrix::nearPD(R_pool, corr = TRUE, ensureSymmetry = TRUE)$mat)
  }
  R_pool
}

# 4.5 Ejecución de MICE con caché automático en Parquet (evita recalcular cada vez)
obtener_o_ejecutar_mice <- function(df, vars_modelo, familia_nom, metodo,
                                    m = M_IMPUTACIONES, maxit = MAXIT_MICE) {
  ruta_cache <- file.path(CARPETA_CACHE, sprintf("%s__MICE_%s_m%d.parquet", familia_nom, metodo, m))

  if (file.exists(ruta_cache) && !RECALCULAR_INSUMOS) {
    message("    [Caché Parquet] Cargando ", basename(ruta_cache))
    df_apilado <- arrow::read_parquet(ruta_cache)
    return(split(df_apilado %>% select(-imp_id), df_apilado$imp_id))
  }

  vars_aux  <- intersect(c("ctrl_pib_nominal", "year"), names(df))
  cols_imp  <- unique(c(vars_modelo, vars_aux))
  df_imp_in <- df %>% select(all_of(cols_imp))

  pred_mat <- mice::quickpred(df_imp_in, mincor = 0.05)
  if ("year" %in% colnames(pred_mat)) {
    pred_mat["year", ] <- 0
  }

  imp_obj <- mice::mice(
    data            = df_imp_in,
    m               = m,
    method          = metodo,
    predictorMatrix = pred_mat,
    maxit           = maxit,
    printFlag       = FALSE
  )

  lista_m <- lapply(seq_len(m), function(i) {
    comp <- mice::complete(imp_obj, action = i)
    bind_cols(df %>% select(iso3c, country, year), comp %>% select(all_of(vars_modelo)))
  })

  # Guardar las m imputaciones en un único archivo Parquet liviano
  df_guardar <- bind_rows(lista_m, .id = "imp_id")
  arrow::write_parquet(df_guardar, ruta_cache)
  message("    [Caché Parquet] Guardado ", basename(ruta_cache))

  lista_m
}

# ------------------------------------------------------------------------------
# 5. MOTOR AUDITABLE DE PCA CON EIGEN(R) Y PRCOMP() (R BASE)
# ------------------------------------------------------------------------------
# Nota metodológica clave:
#   - base::eigen(R) devuelve autovalores (lambda) y autovectores unitarios (v),
#     donde sum(v[,1]^2) == 1.0000 exactamente.
#   - stats::prcomp(X, center = TRUE, scale. = TRUE) devuelve exactamente los
#     mismos autovalores (sdev^2) y autovectores unitarios ($rotation) sobre
#     la matriz de datos completa.
#   - En datos MICE (completos), aplicamos prcomp() en cada imputación m y
#     verificamos que coincide con eigen(R_m). En Raw/Interp (con NAs),
#     eigen(R_pairwise) usa toda la información disponible sin perder años, y
#     prcomp() se corre sobre las filas completas como chequeo de auditoría.

evaluar_indice_auditable <- function(nombre_indice, familia_nom, metodo_nom,
                                     vars_modelo, lista_dfs, es_imputado = FALSE) {

  if (!es_imputado) {
    df_z  <- estandarizar_z(lista_dfs[[1]], vars_modelo)
    mat_z <- as.matrix(df_z[, vars_modelo])
    R     <- correlacion_robusta_pairwise(mat_z)

    n_mat <- crossprod(!is.na(mat_z))
    n_eff <- round(mean(n_mat[lower.tri(n_mat)]))

    # Chequeo con prcomp() sobre filas completas disponibles
    mat_comp <- stats::na.omit(mat_z)
    if (nrow(mat_comp) > length(vars_modelo)) {
      prcomp_obj <- stats::prcomp(mat_comp, center = TRUE, scale. = TRUE)
      eig_comp   <- base::eigen(stats::cor(mat_comp), symmetric = TRUE)
      diff_prcomp_eigen <- max(abs((prcomp_obj$sdev^2) - eig_comp$values))
    } else {
      prcomp_obj <- NULL
      diff_prcomp_eigen <- NA_real_
    }
  } else {
    lista_z <- lapply(lista_dfs, estandarizar_z, vars = vars_modelo)
    lista_R <- lapply(lista_z, function(d) stats::cor(as.matrix(d[, vars_modelo])))
    R       <- combinar_cor_fisher(lista_R)
    n_eff   <- nrow(lista_z[[1]])

    # Ejecutar prcomp() sobre la imputación 1 y comprobar igualdad exacta con eigen(R_1)
    prcomp_obj <- stats::prcomp(as.matrix(lista_z[[1]][, vars_modelo]), center = TRUE, scale. = TRUE)
    eig_m1     <- base::eigen(lista_R[[1]], symmetric = TRUE)
    diff_prcomp_eigen <- max(abs((prcomp_obj$sdev^2) - eig_m1$values))
  }

  # 5.1 Descomposición espectral manual con base::eigen(R)
  eigen_fit <- base::eigen(R, symmetric = TRUE)
  autovalores <- eigen_fit$values
  autovectores <- eigen_fit$vectors
  rownames(autovectores) <- vars_modelo
  colnames(autovectores) <- paste0("CP", seq_len(ncol(autovectores)))

  # Alinear signo de CP1 para que sea predominantemente positivo (mayor score = mayor poder)
  if (sum(autovectores[, 1]) < 0) {
    autovectores[, 1] <- -autovectores[, 1]
  }

  # Verificación clave del tutor: la suma de cuadrados de CP1 DEBE ser 1.0000
  suma_cuadrados_cp1 <- sum(autovectores[, 1]^2)

  # Pesos del índice: cuadrado del autovector unitario CP1 (ya suman 1.0000)
  pesos <- (autovectores[, 1]^2) / suma_cuadrados_cp1

  # Varianza explicada desde autovalores
  var_prop <- autovalores / sum(autovalores)
  pct_cp1  <- var_prop[1] * 100
  pct_cp3  <- sum(var_prop[1:min(3, length(var_prop))]) * 100

  # 5.2 Estadísticos de fiabilidad (Alfa de Cronbach, KMO, Bartlett)
  alfa_res <- suppressWarnings(psych::alpha(R, check.keys = FALSE, warnings = FALSE))
  kmo_res  <- psych::KMO(R)
  bart_res <- psych::cortest.bartlett(R, n = max(n_eff, length(vars_modelo) + 2))

  # 5.3 Cálculo del puntaje del índice (Z-score ponderado, tolerante a NAs parciales)
  puntuar_matriz <- function(mat_z, pesos_vec) {
    disponible     <- !is.na(mat_z)
    cobertura      <- rowMeans(disponible)
    mat_sin_na     <- ifelse(disponible, mat_z, 0)
    suma_ponderada <- as.vector(mat_sin_na %*% pesos_vec)
    suma_pesos_obs <- as.vector(disponible %*% pesos_vec)

    ifelse(
      cobertura >= MIN_COB_FILA & suma_pesos_obs > 0,
      suma_ponderada / suma_pesos_obs,
      NA_real_
    )
  }

  if (!es_imputado) {
    score_z <- puntuar_matriz(as.matrix(df_z[, vars_modelo]), pesos)
    df_base <- df_z %>% select(iso3c, country, year)
  } else {
    mat_scores_m <- sapply(lista_z, function(dz) puntuar_matriz(as.matrix(dz[, vars_modelo]), pesos))
    score_z      <- rowMeans(mat_scores_m)
    df_base      <- lista_z[[1]] %>% select(iso3c, country, year)
  }

  rng <- range(score_z, na.rm = TRUE)
  score_100 <- if (diff(rng) == 0) rep(50, length(score_z)) else (score_z - rng[1]) / diff(rng) * 100

  df_scores <- df_base %>%
    mutate(
      Indice    = nombre_indice,
      Familia   = familia_nom,
      Metodo    = metodo_nom,
      Score_Z   = score_z,
      Score_100 = score_100
    ) %>%
    group_by(year) %>%
    mutate(Rank_Anual = ifelse(is.na(Score_100), NA_integer_, min_rank(desc(Score_100)))) %>%
    ungroup()

  df_stats <- tibble(
    Indice                 = nombre_indice,
    Familia                = familia_nom,
    Metodo                 = metodo_nom,
    N_Variables            = length(vars_modelo),
    N_Paises               = n_distinct(df_scores$iso3c[!is.na(df_scores$Score_100)]),
    N_Obs_Validas          = sum(!is.na(df_scores$Score_100)),
    Alfa_Cronbach          = round(alfa_res$total$raw_alpha, 4),
    KMO_Global             = round(kmo_res$MSA, 4),
    KMO_Min_Individual     = round(min(kmo_res$MSAi), 4),
    Bartlett_Chi2          = round(bart_res$chisq, 2),
    Bartlett_df            = bart_res$df,
    Bartlett_p_valor       = signif(bart_res$p.value, 4),
    Autovalor_CP1          = round(autovalores[1], 4),
    Var_Explicada_CP1_Pct  = round(pct_cp1, 2),
    Var_Acum_CP1_CP3_Pct   = round(pct_cp3, 2),
    Check_SumaCuad_CP1     = round(suma_cuadrados_cp1, 6),
    Check_Diff_prcomp_eig  = signif(diff_prcomp_eigen, 4)
  )

  df_cargas <- tibble(
    Indice   = nombre_indice,
    Familia  = familia_nom,
    Metodo   = metodo_nom,
    Variable = vars_modelo,
    CP1      = round(autovectores[, 1], 4),
    CP2      = round(autovectores[, 2], 4),
    CP3      = round(autovectores[, 3], 4),
    Peso_CP1 = round(pesos, 4)
  )

  list(
    R_cor       = round(R, 4),
    eigen_obj   = eigen_fit,
    prcomp_obj  = prcomp_obj,
    scores      = df_scores,
    stats       = df_stats,
    cargas      = df_cargas
  )
}

# ------------------------------------------------------------------------------
# 6. EJECUCIÓN CONSOLIDADA (OBJETOS EXPUESTOS EN EL GLOBAL ENVIRONMENT)
# ------------------------------------------------------------------------------
# Estas listas quedan visibles en tu Environment de RStudio para auditar todo:
#   - MATRICES_R         : Las 20 matrices de correlación R (ej. View(MATRICES_R$`2B_Base12_Depurada25_PMM`))
#   - RESULTADOS_EIGEN   : Los 20 objetos de eigen(R) (autovalores y autovectores)
#   - RESULTADOS_PRCOMP  : Los 20 objetos de prcomp()
#   - PANELES_PROCESADOS : Los insumos intermedios (Raw, Interp3, MICE)

MATRICES_R         <- list()
RESULTADOS_EIGEN   <- list()
RESULTADOS_PRCOMP  <- list()
PANELES_PROCESADOS <- list()
RESULTADOS_TODOS   <- list()

t_inicio <- Sys.time()

for (fam_nom in names(FAMILIAS)) {
  fam    <- FAMILIAS[[fam_nom]]
  df_f   <- fam$df
  vars_f <- fam$vars

  message("\n======================================================================")
  message("Procesando Familia: ", fam_nom)
  message("======================================================================")

  # Preparar los 5 insumos de la familia
  df_interp <- interpolar_panel(df_f, vars_f, maxgap = MAX_GAP_INTERP)
  dfs_pmm   <- obtener_o_ejecutar_mice(df_f, vars_f, fam_nom, metodo = "pmm")
  dfs_bayes <- obtener_o_ejecutar_mice(df_f, vars_f, fam_nom, metodo = "norm")
  dfs_rf    <- obtener_o_ejecutar_mice(df_f, vars_f, fam_nom, metodo = "rf")

  insumos_familia <- list(
    list(sufijo = "Raw",     met = "1_Raw_Pairwise", datos = list(df_f),      imp = FALSE),
    list(sufijo = "Interp3", met = "2_Interp_Max3",  datos = list(df_interp), imp = FALSE),
    list(sufijo = "PMM",     met = "3_MICE_PMM",     datos = dfs_pmm,         imp = TRUE),
    list(sufijo = "Bayes",   met = "4_MICE_Bayes",   datos = dfs_bayes,       imp = TRUE),
    list(sufijo = "RF",      met = "5_MICE_RF",      datos = dfs_rf,          imp = TRUE)
  )

  for (item in insumos_familia) {
    nom_ind <- paste0(fam_nom, "_", item$sufijo)
    message("  -> Calculando eigen(R) + prcomp() para: ", nom_ind)

    res <- evaluar_indice_auditable(
      nombre_indice = nom_ind,
      familia_nom   = fam_nom,
      metodo_nom    = item$met,
      vars_modelo   = vars_f,
      lista_dfs     = item$datos,
      es_imputado   = item$imp
    )

    # Exponer cada objeto clave en listas limpias del Environment
    MATRICES_R[[nom_ind]]         <- as.data.frame(res$R_cor) %>% rownames_to_column("Variable")
    RESULTADOS_EIGEN[[nom_ind]]   <- res$eigen_obj
    RESULTADOS_PRCOMP[[nom_ind]]  <- res$prcomp_obj
    PANELES_PROCESADOS[[nom_ind]] <- item$datos[[1]]
    RESULTADOS_TODOS[[nom_ind]]   <- res
  }
}

message("\n>>> Tiempo total de cálculo: ", round(difftime(Sys.time(), t_inicio, units = "mins"), 2), " minutos.")

# ------------------------------------------------------------------------------
# 7. CONSOLIDACIÓN DE TABLAS COMPARATIVAS, RANGOS [A, B] Y RANKINGS
# ------------------------------------------------------------------------------
TABLA_ESTADISTICOS <- bind_rows(lapply(RESULTADOS_TODOS, `[[`, "stats"))
TABLA_CARGAS_LARGA <- bind_rows(lapply(RESULTADOS_TODOS, `[[`, "cargas"))
TABLA_SCORES_LARGA <- bind_rows(lapply(RESULTADOS_TODOS, `[[`, "scores"))

# 7.1 Comparación de Autovectores CP1 (unitarios: suma de cuadrados = 1) y Pesos
COMP_AUTOVECTORES_CP1 <- TABLA_CARGAS_LARGA %>%
  select(Variable, Indice, CP1) %>%
  pivot_wider(names_from = Indice, values_from = CP1)

COMP_PESOS_CP1 <- TABLA_CARGAS_LARGA %>%
  select(Variable, Indice, Peso_CP1) %>%
  pivot_wider(names_from = Indice, values_from = Peso_CP1)

# 7.2 Desempeño de países por índice (Score_100, Score_Z y Rank_Anual)
PANEL_SCORES_100 <- TABLA_SCORES_LARGA %>%
  select(iso3c, country, year, Indice, Score_100) %>%
  pivot_wider(names_from = Indice, values_from = Score_100, names_prefix = "Score100_")

PANEL_SCORES_Z <- TABLA_SCORES_LARGA %>%
  select(iso3c, country, year, Indice, Score_Z) %>%
  pivot_wider(names_from = Indice, values_from = Score_Z, names_prefix = "ScoreZ_")

PANEL_RANKS <- TABLA_SCORES_LARGA %>%
  select(iso3c, country, year, Indice, Rank_Anual) %>%
  pivot_wider(names_from = Indice, values_from = Rank_Anual, names_prefix = "Rank_")

# Rangos [Min, Max] por país-año dentro de cada Familia
RANGOS_PAIS_FAMILIA <- TABLA_SCORES_LARGA %>%
  group_by(iso3c, country, year, Familia) %>%
  summarise(
    Min_Score        = round(min(Score_100, na.rm = TRUE), 2),
    Max_Score        = round(max(Score_100, na.rm = TRUE), 2),
    Media_Score      = round(mean(Score_100, na.rm = TRUE), 2),
    SD_Entre_Metodos = round(sd(Score_100, na.rm = TRUE), 2),
    Min_Rank         = min(Rank_Anual, na.rm = TRUE),
    Max_Rank         = max(Rank_Anual, na.rm = TRUE),
    .groups          = "drop"
  ) %>%
  mutate(across(c(Min_Score, Max_Score, Media_Score), ~ ifelse(is.infinite(.x) | is.nan(.x), NA_real_, .x)))

# 7.3 Correlación de rankings año a año (Spearman) entre los 20 índices
anios_panel <- sort(unique(TABLA_SCORES_LARGA$year))
ranks_wide_mat <- PANEL_RANKS %>% select(year, starts_with("Rank_"))

lista_cor_anual <- lapply(anios_panel, function(yr) {
  sub_mat <- ranks_wide_mat %>%
    filter(year == yr) %>%
    select(-year)
  suppressWarnings(stats::cor(sub_mat, method = "spearman", use = "pairwise.complete.obs"))
})
names(lista_cor_anual) <- as.character(anios_panel)

cor_prom_ranks <- Reduce("+", lapply(lista_cor_anual, function(m) ifelse(is.na(m), 0, m))) /
  Reduce("+", lapply(lista_cor_anual, function(m) ifelse(is.na(m), 0, 1)))

MATRIZ_COR_RANKINGS <- as_tibble(round(cor_prom_ranks, 4), rownames = "Indice")

COR_RANKINGS_POR_ANIO <- bind_rows(lapply(names(lista_cor_anual), function(yr) {
  as_tibble(lista_cor_anual[[yr]], rownames = "Indice_1") %>%
    pivot_longer(-Indice_1, names_to = "Indice_2", values_to = "Spearman_Rho") %>%
    filter(Indice_1 < Indice_2) %>%
    mutate(year = as.integer(yr))
})) %>%
  select(year, Indice_1, Indice_2, Spearman_Rho) %>%
  mutate(Spearman_Rho = round(Spearman_Rho, 4))

CONGRUENCIA_RANKINGS <- tibble(
  Indice = rownames(cor_prom_ranks),
  Spearman_Promedio_Otros = round((rowSums(cor_prom_ranks, na.rm = TRUE) - 1) /
                                    (ncol(cor_prom_ranks) - 1), 4)
) %>%
  arrange(desc(Spearman_Promedio_Otros))

# ------------------------------------------------------------------------------
# 8. EXPORTACIÓN A EXCEL CON FORMATO CONDICIONAL (> 0.50 EN VERDE)
# ------------------------------------------------------------------------------
message(">>> Exportando libro Excel consolidado: ", ARCHIVO_SALIDA_XLSX)

wb <- openxlsx::createWorkbook()

estilo_header <- openxlsx::createStyle(
  fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
  fgFill = "#1B365D", halign = "center", valign = "center",
  textDecoration = "bold", border = "Bottom"
)

estilo_verde <- openxlsx::createStyle(
  fontColour = "#006100", bgFill = "#C6EFCE"
)

agregar_hoja <- function(wb, nombre_hoja, df, resaltar_mayor_050 = FALSE) {
  openxlsx::addWorksheet(wb, nombre_hoja)
  openxlsx::writeData(wb, nombre_hoja, df, startRow = 1, startCol = 1)
  openxlsx::addStyle(wb, nombre_hoja, style = estilo_header, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
  openxlsx::freezePane(wb, nombre_hoja, firstRow = TRUE)
  openxlsx::setColWidths(wb, nombre_hoja, cols = 1:ncol(df), widths = "auto")

  if (resaltar_mayor_050 && ncol(df) >= 2 && nrow(df) >= 1) {
    openxlsx::conditionalFormatting(
      wb, nombre_hoja,
      cols  = 2:ncol(df),
      rows  = 2:(nrow(df) + 1),
      rule  = ">0.50",
      style = estilo_verde
    )
  }
}

agregar_hoja(wb, "1_Estadisticos_Indices",     TABLA_ESTADISTICOS)
agregar_hoja(wb, "2_PCA_Autovectores_CP1",     COMP_AUTOVECTORES_CP1, resaltar_mayor_050 = TRUE)
agregar_hoja(wb, "3_PCA_Pesos_CP1",            COMP_PESOS_CP1)
agregar_hoja(wb, "4_PCA_Detalle_Completo",     TABLA_CARGAS_LARGA)
agregar_hoja(wb, "5_Congruencia_Rankings",     CONGRUENCIA_RANKINGS,  resaltar_mayor_050 = TRUE)
agregar_hoja(wb, "6_Matriz_Cor_Rankings",      MATRIZ_COR_RANKINGS,   resaltar_mayor_050 = TRUE)
agregar_hoja(wb, "7_Cor_Rankings_Por_Anio",    COR_RANKINGS_POR_ANIO)
agregar_hoja(wb, "8_Rangos_Pais_Familia",      RANGOS_PAIS_FAMILIA)
agregar_hoja(wb, "9_Scores100_20_Indices",     PANEL_SCORES_100)
agregar_hoja(wb, "10_ScoresZ_20_Indices",      PANEL_SCORES_Z)
agregar_hoja(wb, "11_Ranks_20_Indices",        PANEL_RANKS)

openxlsx::saveWorkbook(wb, ARCHIVO_SALIDA_XLSX, overwrite = TRUE)

message("¡Listo! Archivo generado: ", ARCHIVO_SALIDA_XLSX)
message("Para ver cualquier matriz de correlación R en RStudio, ejecuta por ejemplo:")
message("  View(MATRICES_R$`2B_Base12_Depurada25_PMM`)")
message("  View(TABLA_ESTADISTICOS)")
