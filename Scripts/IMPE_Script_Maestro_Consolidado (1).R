# ==============================================================================
# TESIS: ÍNDICE MULTIDIMENSIONAL DE PODER ECONÓMICO (I.M.P.E. 1995-2024)
# SCRIPT MAESTRO CONSOLIDADO: 4 FAMILIAS x 5 MÉTODOS = 20 ÍNDICES COMPARATIVOS
# ==============================================================================
# Autor(es): Javier Morros & Jose A. (Escuela de Economía - UCV)
# Descripción:
#   Construye, valida y contrasta 20 variantes del IMPE combinando:
#     - 4 Familias muestrales/dimensionales:
#         1.A: Base 16 con todos los países
#         1.B: Base 16 eliminando países con > 25% de NAs
#         2.A: Base 12 con todos los países
#         2.B: Base 12 eliminando países con > 25% de NAs
#     - 5 Tratamientos de datos faltantes por cada familia:
#         1) Raw_Pairwise   : Sin interpolar ni imputar (correlación pairwise + pesos disponibles)
#         2) Interp_Max3    : Interpolación lineal solo en huecos <= 3 años
#         3) MICE_PMM       : Imputación múltiple MICE con Predictive Mean Matching
#         4) MICE_Bayes     : Imputación Bayesiana (Regresión Lineal Bayesiana MICE 'norm')
#         5) MICE_RF        : Imputación no paramétrica con Random Forest (MICE 'rf')
#   Exporta un único Excel consolidado con cargas PCA por variable, estadísticos
#   de fiabilidad (Alfa de Cronbach, KMO, Bartlett), desempeño país-año y
#   correlaciones de ranking año a año entre los 20 índices.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. CONFIGURACIÓN GLOBAL Y PARÁMETROS
# ------------------------------------------------------------------------------
ARCHIVO_ENTRADA <- "DATA/PANELDATA/panel_data_consolidado.csv"  # Ruta a tu panel consolidado
ARCHIVO_SALIDA  <- "IMPE_Comparativa_Maestra_20_Indices.xlsx"

ANIO_INICIO     <- 1995   # Año inicial del panel (evita NAs estructurales 1960-1994)
ANIO_FIN        <- 2024   # Año final del panel
UMBRAL_NA_PAIS  <- 0.25   # Eliminar países con > 25% de NAs en familias 1.B y 2.B
MAX_GAP_INTERP  <- 3      # Máximo de años consecutivos a interpolar (Método 2)
M_IMPUTACIONES  <- 5      # Usar 5 para pruebas rápidas; subir a 20 para corrida final
MAXIT_MICE      <- 10     # Iteraciones por cadena en MICE
MIN_COB_FILA    <- 0.50   # Mínimo 50% de variables no-NA para puntuar una fila en Raw/Interp
SEMILLA         <- 2026   # Semilla para reproducibilidad total

# ------------------------------------------------------------------------------
# 1. INSTALACIÓN Y CARGA DE PAQUETES (Y POR QUÉ SE USA CADA UNO)
# ------------------------------------------------------------------------------
# - dplyr / tidyr : Manipulación eficiente de paneles de datos (agrupación por país/año).
# - readr         : Lectura rápida y limpia de archivos CSV grandes.
# - countrycode   : Filtra agregados regionales del Banco Mundial (WLD, EUU, ARB, etc.).
# - zoo           : Contiene na.approx(), estándar para interpolación lineal con maxgap.
# - mice          : Estándar de oro para Imputación Múltiple (PMM, Bayesiana 'norm' y 'rf').
# - ranger        : Motor ultrarrápido en C++ para Random Forest dentro de mice(method='rf').
# - psych         : Calcula PCA (principal), Alfa de Cronbach, KMO y test de Bartlett.
# - Matrix        : Provee nearPD() para asegurar matrices de correlación semidefinidas positivas
#                   cuando se usa pairwise.complete.obs en datos sin imputar.
# - openxlsx      : Exporta libros Excel (.xlsx) multi-hoja con formato profesional sin Java.

paquetes_necesarios <- c(
  "dplyr", "tidyr", "readr", "countrycode", "zoo", "mice", "ranger",
  "psych", "Matrix", "openxlsx"
)

paquetes_faltantes <- paquetes_necesarios[!(paquetes_necesarios %in% installed.packages()[, "Package"])]
if (length(paquetes_faltantes) > 0) {
  message("Instalando paquetes faltantes: ", paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(countrycode)
  library(zoo)
  library(mice)
  library(ranger)
  library(psych)
  library(Matrix)
  library(openxlsx)
})

set.seed(SEMILLA)

# ------------------------------------------------------------------------------
# 2. DEFINICIÓN DE VARIABLES (BASE 16, BASE 12 Y AUXILIARES)
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

# Variables podadas en Base 12 por baja comunalidad marginal
vars_podadas_12 <- c(
  "prod_manuf_pib",
  "prod_fbcf_pib",
  "fin_ied_sal_pib",
  "tech_serv_ict_pct"
)

vars_base12 <- setdiff(vars_base16, vars_podadas_12)

# Variables con distribución fuertemente sesgada (volúmenes en USD / conteos)
# Se transforman con log1p(x) antes de estandarizar o imputar para evitar distorsión por outliers
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
# 3. CARGA, DEPURACIÓN TEMPORAL/MUESTRAL Y PREPARACIÓN DE LAS 4 FAMILIAS
# ------------------------------------------------------------------------------
message(">>> Cargando panel de datos desde: ", ARCHIVO_ENTRADA)
panel_raw <- read_csv(ARCHIVO_ENTRADA, show_col_types = FALSE)

# Verificación de columnas requeridas
cols_faltantes <- setdiff(c("iso3c", "country", "year", vars_base16), names(panel_raw))
if (length(cols_faltantes) > 0) {
  stop("Faltan las siguientes columnas en el CSV: ", paste(cols_faltantes, collapse = ", "))
}

# 3.1 Filtrar ventana temporal (1995-2024) y eliminar agregados regionales del Banco Mundial
isos_soberanos <- countrycode::codelist$iso3c[!is.na(countrycode::codelist$iso3c)]

panel_depurado <- panel_raw %>%
  filter(year >= ANIO_INICIO & year <= ANIO_FIN) %>%
  filter(!is.na(iso3c) & iso3c %in% isos_soberanos)

if ("region" %in% names(panel_depurado)) {
  panel_depurado <- panel_depurado %>% filter(region != "Aggregates")
}

panel_depurado <- panel_depurado %>% arrange(iso3c, year)

# Preprocesamiento logarítmico seguro (mantiene NAs intactos, evita log(<=0))
aplicar_log_seguro <- function(df, cols_log) {
  cols_presentes <- intersect(cols_log, names(df))
  df %>%
    mutate(across(
      all_of(cols_presentes),
      ~ ifelse(is.na(.x), NA_real_, ifelse(.x < 0, NA_real_, log1p(.x)))
    ))
}

panel_proc <- aplicar_log_seguro(panel_depurado, vars_log)

# Función robusta para calcular % de NAs por país y filtrar <= umbral
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

# Construcción de las 4 Familias
familias <- list(
  "1A_Base16_Completa"   = list(df = panel_proc, vars = vars_base16),
  "1B_Base16_Depurada25" = list(df = filtrar_paises_na(panel_proc, vars_base16, UMBRAL_NA_PAIS, "1B_Base16"), vars = vars_base16),
  "2A_Base12_Completa"   = list(df = panel_proc, vars = vars_base12),
  "2B_Base12_Depurada25" = list(df = filtrar_paises_na(panel_proc, vars_base12, UMBRAL_NA_PAIS, "2B_Base12"), vars = vars_base12)
)

for (nom in names(familias)) {
  message(sprintf(
    "    Familia %-22s | Países: %3d | Filas: %4d | Variables: %2d",
    nom,
    n_distinct(familias[[nom]]$df$iso3c),
    nrow(familias[[nom]]$df),
    length(familias[[nom]]$vars)
  ))
}

# ------------------------------------------------------------------------------
# 4. FUNCIONES AUXILIARES: ESTANDARIZACIÓN, INTERPOLACIÓN, MICE Y POOLING FISHER
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

# 4.3 Matriz de correlación robusta con pairwise deletion + ajuste nearPD
correlacion_robusta_pairwise <- function(mat) {
  R <- stats::cor(mat, use = "pairwise.complete.obs")
  R[is.na(R)] <- 0
  diag(R) <- 1
  if (min(eigen(R, symmetric = TRUE, only.values = TRUE)$values) <= 1e-6) {
    R <- as.matrix(Matrix::nearPD(R, corr = TRUE, ensureSymmetry = TRUE)$mat)
  }
  R
}

# 4.4 Combinación de matrices de correlación vía transformación z de Fisher (Van Buuren)
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
  if (min(eigen(R_pool, symmetric = TRUE, only.values = TRUE)$values) <= 1e-6) {
    R_pool <- as.matrix(Matrix::nearPD(R_pool, corr = TRUE, ensureSymmetry = TRUE)$mat)
  }
  R_pool
}

# 4.5 Ejecución de MICE unificada (PMM, Bayesiana 'norm', o Random Forest 'rf')
ejecutar_mice_panel <- function(df, vars_modelo, metodo, m = M_IMPUTACIONES, maxit = MAXIT_MICE) {
  vars_aux <- intersect(c("ctrl_pib_nominal", "year"), names(df))
  cols_imp <- unique(c(vars_modelo, vars_aux))
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

  lapply(1:m, function(i) {
    comp <- mice::complete(imp_obj, action = i)
    bind_cols(df %>% select(iso3c, country, year), comp %>% select(all_of(vars_modelo)))
  })
}

# ------------------------------------------------------------------------------
# 5. MOTOR UNIFICADO DE PCA, ESTADÍSTICOS DE VALIDACIÓN Y PUNTUACIÓN DEL ÍNDICE
# ------------------------------------------------------------------------------
evaluar_y_puntuar_indice <- function(nombre_indice, familia_nom, metodo_nom,
                                     vars_modelo, lista_dfs, es_imputado = FALSE) {

  if (!es_imputado) {
    df_z <- estandarizar_z(lista_dfs[[1]], vars_modelo)
    mat_z <- as.matrix(df_z[, vars_modelo])
    R <- correlacion_robusta_pairwise(mat_z)
    n_mat <- crossprod(!is.na(mat_z))
    n_eff <- round(mean(n_mat[lower.tri(n_mat)]))
  } else {
    lista_z <- lapply(lista_dfs, estandarizar_z, vars = vars_modelo)
    lista_R <- lapply(lista_z, function(d) stats::cor(as.matrix(d[, vars_modelo])))
    R <- combinar_cor_fisher(lista_R)
    n_eff <- nrow(lista_z[[1]])
  }

  # 5.1 PCA formal (NO manual) mediante psych::principal() sobre la matriz R
  n_fact <- min(3, length(vars_modelo))
  pca_fit <- psych::principal(r = R, nfactors = n_fact, rotate = "none", scores = FALSE)

  cargas <- as.matrix(unclass(pca_fit$loadings))
  colnames(cargas) <- paste0("CP", seq_len(ncol(cargas)))

  if (sum(cargas[, 1]) < 0) {
    cargas[, 1] <- -cargas[, 1]
  }

  pesos <- (cargas[, 1]^2) / sum(cargas[, 1]^2)

  # 5.2 Estadísticos de validación psicométrica / factorial
  alfa_res <- suppressWarnings(psych::alpha(R, check.keys = FALSE, warnings = FALSE))
  kmo_res  <- psych::KMO(R)
  bart_res <- psych::cortest.bartlett(R, n = max(n_eff, length(vars_modelo) + 2))

  var_acc  <- pca_fit$Vaccounted
  pct_cp1  <- var_acc["Proportion Var", 1] * 100
  pct_cp3  <- if (n_fact >= 3) var_acc["Cumulative Var", 3] * 100 else var_acc["Cumulative Var", n_fact] * 100

  # 5.3 Cálculo del puntaje del índice por país-año
  puntuar_matriz <- function(mat_z, pesos_vec) {
    disponible <- !is.na(mat_z)
    cobertura  <- rowMeans(disponible)
    mat_sin_na <- ifelse(disponible, mat_z, 0)
    suma_ponderada <- as.vector(mat_sin_na %*% pesos_vec)
    suma_pesos_obs <- as.vector(disponible %*% pesos_vec)

    ifelse(
      cobertura >= MIN_COB_FILA & suma_pesos_obs > 0,
      suma_ponderada / suma_pesos_obs,
      NA_real_
    )
  }

  if (!es_imputado) {
    score_bruto <- puntuar_matriz(as.matrix(df_z[, vars_modelo]), pesos)
    df_base <- df_z %>% select(iso3c, country, year)
  } else {
    mat_scores_m <- sapply(lista_z, function(dz) puntuar_matriz(as.matrix(dz[, vars_modelo]), pesos))
    score_bruto  <- rowMeans(mat_scores_m)
    df_base <- lista_z[[1]] %>% select(iso3c, country, year)
  }

  rng <- range(score_bruto, na.rm = TRUE)
  score_100 <- if (diff(rng) == 0) rep(50, length(score_bruto)) else (score_bruto - rng[1]) / diff(rng) * 100

  df_scores <- df_base %>%
    mutate(
      Indice     = nombre_indice,
      Familia    = familia_nom,
      Metodo     = metodo_nom,
      Score_Z    = score_bruto,
      Score_100  = score_100
    ) %>%
    group_by(year) %>%
    mutate(Rank_Anual = ifelse(is.na(Score_100), NA_integer_, min_rank(desc(Score_100)))) %>%
    ungroup()

  df_stats <- tibble(
    Indice                = nombre_indice,
    Familia               = familia_nom,
    Metodo                = metodo_nom,
    N_Variables           = length(vars_modelo),
    N_Paises              = n_distinct(df_scores$iso3c[!is.na(df_scores$Score_100)]),
    N_Obs_Validas         = sum(!is.na(df_scores$Score_100)),
    Alfa_Cronbach         = round(alfa_res$total$raw_alpha, 4),
    KMO_Global            = round(kmo_res$MSA, 4),
    KMO_Min_Individual    = round(min(kmo_res$MSAi), 4),
    Bartlett_Chi2         = round(bart_res$chisq, 2),
    Bartlett_df           = bart_res$df,
    Bartlett_p_valor      = signif(bart_res$p.value, 4),
    Autovalor_CP1         = round(pca_fit$values[1], 4),
    Var_Explicada_CP1_Pct = round(pct_cp1, 2),
    Var_Acum_CP1_CP3_Pct  = round(pct_cp3, 2)
  )

  df_cargas <- tibble(
    Indice   = nombre_indice,
    Familia  = familia_nom,
    Metodo   = metodo_nom,
    Variable = vars_modelo,
    Peso_CP1 = round(pesos, 4)
  ) %>%
    bind_cols(as_tibble(round(cargas, 4)))

  list(scores = df_scores, stats = df_stats, cargas = df_cargas)
}

# ------------------------------------------------------------------------------
# 6. EJECUCIÓN CONSOLIDADA DE LAS 4 FAMILIAS x 5 MÉTODOS (20 ÍNDICES)
# ------------------------------------------------------------------------------
resultados_todos <- list()
t_inicio <- Sys.time()

for (fam_nom in names(familias)) {
  fam <- familias[[fam_nom]]
  df_f <- fam$df
  vars_f <- fam$vars

  message("\n======================================================================")
  message("Procesando Familia: ", fam_nom)
  message("======================================================================")

  message("  [1/5] Raw_Pairwise...")
  resultados_todos[[paste0(fam_nom, "__Raw")]] <- evaluar_y_puntuar_indice(
    paste0(fam_nom, "_Raw"), fam_nom, "1_Raw_Pairwise", vars_f, list(df_f), es_imputado = FALSE
  )

  message("  [2/5] Interp_Max3...")
  df_interp <- interpolar_panel(df_f, vars_f, maxgap = MAX_GAP_INTERP)
  resultados_todos[[paste0(fam_nom, "__Interp3")]] <- evaluar_y_puntuar_indice(
    paste0(fam_nom, "_Interp3"), fam_nom, "2_Interp_Max3", vars_f, list(df_interp), es_imputado = FALSE
  )

  message("  [3/5] MICE_PMM (m=", M_IMPUTACIONES, ")...")
  dfs_pmm <- ejecutar_mice_panel(df_f, vars_f, metodo = "pmm")
  resultados_todos[[paste0(fam_nom, "__PMM")]] <- evaluar_y_puntuar_indice(
    paste0(fam_nom, "_PMM"), fam_nom, "3_MICE_PMM", vars_f, dfs_pmm, es_imputado = TRUE
  )

  message("  [4/5] MICE_Bayes_Norm (m=", M_IMPUTACIONES, ")...")
  dfs_bayes <- ejecutar_mice_panel(df_f, vars_f, metodo = "norm")
  resultados_todos[[paste0(fam_nom, "__Bayes")]] <- evaluar_y_puntuar_indice(
    paste0(fam_nom, "_Bayes"), fam_nom, "4_MICE_Bayes", vars_f, dfs_bayes, es_imputado = TRUE
  )

  message("  [5/5] MICE_RandomForest (m=", M_IMPUTACIONES, ")...")
  dfs_rf <- ejecutar_mice_panel(df_f, vars_f, metodo = "rf")
  resultados_todos[[paste0(fam_nom, "__RF")]] <- evaluar_y_puntuar_indice(
    paste0(fam_nom, "_RF"), fam_nom, "5_MICE_RF", vars_f, dfs_rf, es_imputado = TRUE
  )
}

message("\n>>> Tiempo total de cálculo: ", round(difftime(Sys.time(), t_inicio, units = "mins"), 2), " minutos.")

# ------------------------------------------------------------------------------
# 7. CONSOLIDACIÓN DE TABLAS, RANGOS [A, B] Y CORRELACIONES DE RANKING POR AÑO
# ------------------------------------------------------------------------------
tabla_stats  <- bind_rows(lapply(resultados_todos, `[[`, "stats"))
tabla_cargas <- bind_rows(lapply(resultados_todos, `[[`, "cargas"))
tabla_larga  <- bind_rows(lapply(resultados_todos, `[[`, "scores"))

comp_pca_cp1 <- tabla_cargas %>%
  select(Variable, Indice, CP1) %>%
  pivot_wider(names_from = Indice, values_from = CP1)

comp_pca_pesos <- tabla_cargas %>%
  select(Variable, Indice, Peso_CP1) %>%
  pivot_wider(names_from = Indice, values_from = Peso_CP1)

panel_ancho_scores <- tabla_larga %>%
  select(iso3c, country, year, Indice, Score_100) %>%
  pivot_wider(names_from = Indice, values_from = Score_100, names_prefix = "Score_")

panel_ancho_ranks <- tabla_larga %>%
  select(iso3c, country, year, Indice, Rank_Anual) %>%
  pivot_wider(names_from = Indice, values_from = Rank_Anual, names_prefix = "Rank_")

rangos_por_familia <- tabla_larga %>%
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

anios_panel <- sort(unique(tabla_larga$year))

ranks_wide_mat <- panel_ancho_ranks %>%
  select(year, starts_with("Rank_"))

lista_cor_anual <- lapply(anios_panel, function(yr) {
  sub_mat <- ranks_wide_mat %>%
    filter(year == yr) %>%
    select(-year)
  suppressWarnings(stats::cor(sub_mat, method = "spearman", use = "pairwise.complete.obs"))
})
names(lista_cor_anual) <- as.character(anios_panel)

cor_promedio_rankings <- Reduce("+", lapply(lista_cor_anual, function(m) ifelse(is.na(m), 0, m))) /
  Reduce("+", lapply(lista_cor_anual, function(m) ifelse(is.na(m), 0, 1)))

matriz_cor_rankings_df <- as_tibble(round(cor_promedio_rankings, 4), rownames = "Indice")

cor_rankings_detalle_anio <- bind_rows(lapply(names(lista_cor_anual), function(yr) {
  mat <- lista_cor_anual[[yr]]
  as_tibble(mat, rownames = "Indice_1") %>%
    pivot_longer(-Indice_1, names_to = "Indice_2", values_to = "Spearman_Rho") %>%
    filter(Indice_1 < Indice_2) %>%
    mutate(year = as.integer(yr))
})) %>%
  select(year, Indice_1, Indice_2, Spearman_Rho) %>%
  mutate(Spearman_Rho = round(Spearman_Rho, 4))

congruencia_indices <- tibble(
  Indice = rownames(cor_promedio_rankings),
  Spearman_Promedio_Otros = round((rowSums(cor_promedio_rankings, na.rm = TRUE) - 1) /
                                    (ncol(cor_promedio_rankings) - 1), 4)
) %>%
  arrange(desc(Spearman_Promedio_Otros))

# ------------------------------------------------------------------------------
# 8. EXPORTACIÓN CONSOLIDADA A EXCEL (.xlsx)
# ------------------------------------------------------------------------------
message(">>> Exportando resultados consolidados a: ", ARCHIVO_SALIDA)

wb <- openxlsx::createWorkbook()

estilo_header <- openxlsx::createStyle(
  fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
  fgFill = "#1B365D", halign = "center", valign = "center",
  textDecoration = "bold", border = "Bottom"
)

agregar_hoja <- function(wb, nombre_hoja, df) {
  openxlsx::addWorksheet(wb, nombre_hoja)
  openxlsx::writeData(wb, nombre_hoja, df, startRow = 1, startCol = 1)
  openxlsx::addStyle(wb, nombre_hoja, style = estilo_header, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
  openxlsx::freezePane(wb, nombre_hoja, firstRow = TRUE)
  openxlsx::setColWidths(wb, nombre_hoja, cols = 1:ncol(df), widths = "auto")
}

agregar_hoja(wb, "1_Estadisticos_Indices",     tabla_stats)
agregar_hoja(wb, "2_PCA_Cargas_CP1",           comp_pca_cp1)
agregar_hoja(wb, "3_PCA_Pesos_CP1",            comp_pca_pesos)
agregar_hoja(wb, "4_PCA_Detalle_Completo",     tabla_cargas)
agregar_hoja(wb, "5_Congruencia_Rankings",     congruencia_indices)
agregar_hoja(wb, "6_Matriz_Cor_Rankings",      matriz_cor_rankings_df)
agregar_hoja(wb, "7_Cor_Rankings_Por_Anio",    cor_rankings_detalle_anio)
agregar_hoja(wb, "8_Rangos_Pais_Familia",      rangos_por_familia)
agregar_hoja(wb, "9_Scores_Paises_20_Indices", panel_ancho_scores)
agregar_hoja(wb, "10_Ranks_Paises_20_Indices", panel_ancho_ranks)

openxlsx::saveWorkbook(wb, ARCHIVO_SALIDA, overwrite = TRUE)

message("¡Proceso completado con éxito! Archivo generado: ", ARCHIVO_SALIDA)
