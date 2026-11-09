# ==============================================================================
# SCRIPT MAESTRO AUTOCONTENIDO (v3):
# CONSTRUCCIÓN, ANÁLISIS DE 3 COMPONENTES PRINCIPALES (ACP), ALFA DE CRONBACH
# Y EXPORTACIÓN A UN SOLO EXCEL DE TODOS LOS ÍNDICES I.M.P.E.:
#
#   1. Índice Sin Imputar (Base 16 y Base 12 con rowSums simple, inteligente y 3CP)
#   2. Índice Exclusivamente Interpolado (Base 16 y Base 12 con zoo::na.approx + rowSums)
#   3. IMPE Países Depurados (Elimina países con > 30% de NAs, interpola brechas
#      e imputa el resto con MICE-PMM en Base 16 y Base 12 con Reglas de Rubin)
#
# Nota: Todo el código utiliza el operador explícito `::` y el pipe nativo `|>`
#       sin cargar librerías completas.
# ==============================================================================
# INSTRUCCIÓN DE EJECUCIÓN EN RSTUDIO:
# Presiona Ctrl + A (seleccionar todo) y luego Ctrl + Enter (Run),
# o haz clic en el botón "Source" arriba a la derecha del editor.
# ==============================================================================

# ------------------------------------------------------------------------------
# PASO 0: DEFINICIÓN INMEDIATA DE VECTORES DE VARIABLES (BASE 16 Y BASE 12)
# ------------------------------------------------------------------------------
vars_productiva_16  <- c("prod_manuf_pib", "prod_exp_total", "prod_fbcf_pib",
                         "tech_mnf_val_added", "prod_eci", "prod_pib_pcap_ppa")
vars_financiera_16  <- c("fin_ied_sal_pib", "fin_mkt_cap_pib",
                         "fin_cred_priv_pib", "fin_regalias_rec")
vars_tecnologica_16 <- c("tech_id_pib", "tech_investigad", "tech_exp_alta",
                         "tech_serv_ict_pct", "tech_exp_serv_dig", "tech_patentes_tic")
vars_impe_16        <- c(vars_productiva_16, vars_financiera_16, vars_tecnologica_16)

vars_productiva_12  <- c("prod_exp_total", "tech_mnf_val_added", "prod_eci", "prod_pib_pcap_ppa")
vars_financiera_12  <- c("fin_mkt_cap_pib", "fin_cred_priv_pib", "fin_regalias_rec")
vars_tecnologica_12 <- c("tech_id_pib", "tech_investigad", "tech_exp_alta",
                         "tech_exp_serv_dig", "tech_patentes_tic")
vars_impe_12        <- c(vars_productiva_12, vars_financiera_12, vars_tecnologica_12)

vars_log <- c("prod_exp_total", "prod_pib_pcap_ppa", "fin_mkt_cap_pib",
              "tech_exp_serv_dig", "tech_patentes_tic", "ctrl_pib_nominal")

# Códigos de Agregados Regionales/Financieros del Banco Mundial a excluir
agregados_wb <- c(
  "AFE", "AFW", "ARB", "CEB", "CSS", "EAP", "EAR", "EAS", "ECA", "ECS",
  "EMU", "EUU", "FCS", "HIC", "HPC", "IBD", "IBT", "IDA", "IDB", "IDX",
  "INX", "LAC", "LCN", "LDC", "LIC", "LMC", "LMY", "LTE", "MEA", "MIC",
  "MNA", "NAC", "OED", "OSS", "PRE", "PSS", "PST", "SAS", "SSA", "SSF",
  "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC", "WLD"
)

# Instalación automática de paquetes solo si faltan en tu equipo:
paquetes_requeridos <- c("dplyr", "tidyr", "readr", "readxl", "zoo", "mice", "psych", "openxlsx", "tibble")
paquetes_faltantes  <- paquetes_requeridos[!(paquetes_requeridos %in% utils::installed.packages()[, "Package"])]
if (base::length(paquetes_faltantes) > 0) {
  utils::install.packages(paquetes_faltantes)
}


# ==============================================================================
# ETAPA 1: CARGA DE DATOS Y CONSTRUCCIÓN DE LAS COHORTES DE PAÍSES (1995-2024)
# ==============================================================================

# 1.1 Carga inteligente del panel consolidado
if (!base::exists("panel_final") || !base::all(vars_impe_16 %in% base::names(panel_final))) {
  if (base::file.exists("panel_data_consolidado_actualizado.csv")) {
    panel_final <- readr::read_csv("panel_data_consolidado_actualizado.csv", show_col_types = FALSE)
  } else if (base::file.exists("panel_data_consolidado.csv")) {
    panel_raw <- readr::read_csv("panel_data_consolidado.csv", show_col_types = FALSE)
    if (!("tech_mnf_val_added" %in% base::names(panel_raw)) && base::file.exists("API_NV.MNF.TECH.ZS.UN_DS2_es_excel_v2_94833.xls")) {
      nn_wide <- readxl::read_excel("API_NV.MNF.TECH.ZS.UN_DS2_es_excel_v2_94833.xls", sheet = "Data", skip = 3)
      nn_long <- nn_wide |>
        tidyr::pivot_longer(
          cols      = dplyr::matches("^[0-9]{4}$"),
          names_to  = "year",
          values_to = "tech_mnf_val_added"
        ) |>
        dplyr::mutate(year = base::as.numeric(year)) |>
        dplyr::select(iso3c = `Country Code`, year, tech_mnf_val_added)
      panel_final <- panel_raw |>
        dplyr::left_join(nn_long, by = c("iso3c", "year"))
    } else {
      panel_final <- panel_raw
    }
  } else {
    base::stop("No se encontró 'panel_final' en memoria ni el archivo CSV en el directorio de trabajo.")
  }
}

# 1.2 Identificación de países soberanos reales (sin agregados regionales ni códigos vacíos)
if (base::file.exists("API_NV.MNF.TECH.ZS.UN_DS2_es_excel_v2_94833.xls")) {
  metadatos_wb <- readxl::read_excel(
    "API_NV.MNF.TECH.ZS.UN_DS2_es_excel_v2_94833.xls",
    sheet = "Metadata - Countries"
  )
  codigos_soberanos <- metadatos_wb |>
    dplyr::filter(
      !base::is.na(Income_Group),
      Income_Group != "Agregados",
      !base::is.na(`Country Code`),
      base::nchar(base::trimws(`Country Code`)) == 3
    ) |>
    dplyr::pull(`Country Code`) |>
    base::unique()
} else {
  codigos_soberanos <- panel_final$iso3c |>
    stats::na.omit() |>
    base::trimws()
  codigos_soberanos <- base::unique(codigos_soberanos[base::nchar(codigos_soberanos) == 3 & !(codigos_soberanos %in% agregados_wb)])
}

# 1.3 Filtrado temporal 1995-2024 y cálculo vectorizado de NAs por fila con rowSums()
panel_temp <- panel_final |>
  dplyr::mutate(iso3c = base::trimws(base::as.character(iso3c))) |>
  dplyr::filter(
    !base::is.na(iso3c),
    base::nchar(iso3c) == 3,
    iso3c %in% codigos_soberanos,
    year >= 1995,
    year <= 2024
  )

# Contamos cuántas de las 16 variables están observadas en cada país-año:
panel_temp$n_obs_fila_16 <- base::rowSums(!base::is.na(panel_temp[, vars_impe_16]))

# COHORTE GENERAL (182 países con >= 35% de datos observados, es decir <= 65% NAs):
panel_observado <- panel_temp |>
  dplyr::group_by(iso3c) |>
  dplyr::filter(base::mean(n_obs_fila_16 / base::length(vars_impe_16), na.rm = TRUE) >= 0.35) |>
  dplyr::ungroup() |>
  dplyr::select(-n_obs_fila_16) |>
  dplyr::arrange(iso3c, year)

# COHORTE EXCLUSIVAMENTE INTERPOLADA (182 países, interpolación interna sin extrapolar bordes):
interpolar_serie_interna <- function(v, t_anio, brecha_max = Inf) {
  if (base::sum(!base::is.na(v)) < 2) return(v)
  zoo::na.approx(v, x = t_anio, maxgap = brecha_max, na.rm = FALSE, rule = 1)
}

panel_interpolado <- panel_observado |>
  dplyr::group_by(iso3c) |>
  dplyr::arrange(year, .by_group = TRUE) |>
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::all_of(c(vars_impe_16, "ctrl_pib_nominal")),
      .fns  = ~ interpolar_serie_interna(.x, year, brecha_max = Inf)
    )
  ) |>
  dplyr::ungroup()

# 1.4 NUEVA COHORTE: PAÍSES DEPURADOS (Elimina países con MÁS DEL 30% de data faltante)
# Regla estricta: Porcentaje de NAs <= 30% (equivalente a >= 70% de datos observados en las 16 variables)
panel_paises_depurados_base <- panel_temp |>
  dplyr::group_by(iso3c) |>
  dplyr::mutate(
    Pct_Obs_Pais = base::mean(n_obs_fila_16 / base::length(vars_impe_16), na.rm = TRUE) * 100,
    Pct_NA_Pais  = 100 - Pct_Obs_Pais
  ) |>
  dplyr::filter(Pct_NA_Pais <= 30.0) |>
  dplyr::ungroup() |>
  dplyr::select(-n_obs_fila_16) |>
  dplyr::arrange(iso3c, year)

base::cat("
--- Diagnóstico de Selección: IMPE Países Depurados (<= 30% NAs) ---
")
base::cat("Países retenidos:", dplyr::n_distinct(panel_paises_depurados_base$iso3c),
          "| Observaciones:", base::nrow(panel_paises_depurados_base),
          "| % Medio de NAs antes de interpolar:",
          base::round(base::mean(panel_paises_depurados_base$Pct_NA_Pais), 2), "%
")


# ==============================================================================
# ETAPA 2: INTERPOLACIÓN + IMPUTACIÓN MICE-PMM SOBRE LOS PAÍSES DEPURADOS (<= 30% NA)
# ==============================================================================

# 2.1 Paso 1: Interpolación longitudinal intrapaís de brechas cortas (maxgap = 3, rule = 1)
panel_depurados_interp <- panel_paises_depurados_base |>
  dplyr::group_by(iso3c) |>
  dplyr::arrange(year, .by_group = TRUE) |>
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::all_of(c(vars_impe_16, "ctrl_pib_nominal")),
      .fns  = ~ interpolar_serie_interna(.x, year, brecha_max = 3)
    )
  ) |>
  dplyr::ungroup()

# 2.2 Paso 2: Transformación de asimetría extrema (log1p y asinh) previa a MICE-PMM
panel_depurados_pre_mice <- panel_depurados_interp |>
  dplyr::mutate(
    dplyr::across(dplyr::all_of(vars_log), ~ base::log1p(base::pmax(.x, 0))),
    fin_regalias_rec = base::asinh(fin_regalias_rec)
  )

datos_mice_depurados <- panel_depurados_pre_mice |>
  dplyr::select(iso3c, year, ctrl_pib_nominal, dplyr::all_of(vars_impe_16))

bloque_ancla_dep <- c("year", "ctrl_pib_nominal", "prod_exp_total", "prod_pib_pcap_ppa",
                      "prod_manuf_pib", "prod_fbcf_pib", "fin_cred_priv_pib",
                      "fin_ied_sal_pib", "tech_serv_ict_pct", "prod_eci", "tech_mnf_val_added")

matriz_pred_dep <- mice::quickpred(
  datos_mice_depurados,
  mincor  = 0.15,
  minpuc  = 0.30,
  include = bloque_ancla_dep,
  exclude = "iso3c"
)

# 2.3 Paso 3: Ejecución de MICE-PMM sobre la cohorte de Países Depurados (<= 30% NAs)
base::set.seed(2026)
imputacion_mice_depurados <- mice::mice(
  data            = datos_mice_depurados,
  m               = 5,
  maxit           = 10,
  method          = "pmm",
  donors          = 5,
  visitSequence   = "monotone",
  predictorMatrix = matriz_pred_dep,
  ridge           = 0.001,
  printFlag       = TRUE
)

lista_imp_depurados <- mice::complete(imputacion_mice_depurados, action = "all")


# ==============================================================================
# ETAPA 3: FUNCIONES MAESTRAS DE A.C.P. (3 COMPONENTES) Y ALFA DE CRONBACH
#   A) Función para modelos con rowSums() (Sin Imputar e Interpolado)
#   B) Función para modelos Imputados MICE-PMM (Países Depurados Base 16 y Base 12)
# ==============================================================================

reescalar_0_100 <- function(x) {
  rango <- base::max(x, na.rm = TRUE) - base::min(x, na.rm = TRUE)
  if (rango == 0) return(base::rep(0, base::length(x)))
  ((x - base::min(x, na.rm = TRUE)) / rango) * 100
}

congruencia_tucker <- function(u, v) {
  base::sum(u * v) / base::sqrt(base::sum(u^2) * base::sum(v^2))
}

# 3.A Función para Índices con rowSums() (Sin Imputar y Exclusivamente Interpolado)
ejecutar_modelo_impe_rowsums <- function(df_datos, vars_activas, dims_etiquetas, nombre_modelo) {
  mat_cruda <- base::as.matrix(df_datos[, vars_activas])
  mat_z     <- base::scale(mat_cruda, center = TRUE, scale = TRUE)
  mat_cor   <- stats::cor(mat_z, use = "pairwise.complete.obs", method = "pearson")
  
  mat_binaria    <- (!base::is.na(mat_z)) * 1
  conteo_pares   <- base::t(mat_binaria) %*% mat_binaria
  base::diag(conteo_pares) <- NA
  n_efectivo_par <- base::mean(conteo_pares, na.rm = TRUE)
  
  test_alfa     <- psych::alpha(mat_cor, n.obs = n_efectivo_par, check.keys = TRUE)
  test_kmo      <- psych::KMO(mat_cor)
  test_bartlett <- psych::cortest.bartlett(mat_cor, n = base::round(n_efectivo_par))
  
  eigen_res    <- base::eigen(mat_cor)
  autovalores  <- eigen_res$values
  autovectores <- eigen_res$vectors
  base::rownames(autovectores) <- vars_activas
  
  if (autovectores["tech_id_pib", 1]     < 0) autovectores[, 1] <- -autovectores[, 1]
  if (autovectores["prod_exp_total", 2]  < 0) autovectores[, 2] <- -autovectores[, 2]
  if (autovectores["fin_mkt_cap_pib", 3] < 0) autovectores[, 3] <- -autovectores[, 3]
  
  cargas_3cp      <- base::sweep(autovectores[, 1:3], 2, base::sqrt(autovalores[1:3]), `*`)
  comunalidad_3cp <- base::rowSums(cargas_3cp^2)
  
  pesos_cp1 <- (autovectores[, 1]^2) / base::sum(autovectores[, 1]^2)
  pesos_cp2 <- (autovectores[, 2]^2) / base::sum(autovectores[, 2]^2)
  pesos_cp3 <- (autovectores[, 3]^2) / base::sum(autovectores[, 3]^2)
  
  var_rel_3cp    <- autovalores[1:3] / base::sum(autovalores[1:3])
  pesos_3cp_comp <- (pesos_cp1 * var_rel_3cp[1]) + (pesos_cp2 * var_rel_3cp[2]) + (pesos_cp3 * var_rel_3cp[3])
  pesos_3cp_comp <- pesos_3cp_comp / base::sum(pesos_3cp_comp)
  
  n_filas <- base::nrow(mat_z)
  n_cols  <- base::ncol(mat_z)
  vars_obs_fila <- base::rowSums(!base::is.na(mat_z))
  fraccion_obs  <- vars_obs_fila / n_cols
  
  mat_w_cp1 <- base::matrix(pesos_cp1, nrow = n_filas, ncol = n_cols, byrow = TRUE)
  mat_w_3cp <- base::matrix(pesos_3cp_comp, nrow = n_filas, ncol = n_cols, byrow = TRUE)
  
  masa_activa_cp1  <- base::rowSums((!base::is.na(mat_z)) * mat_w_cp1, na.rm = TRUE)
  masa_activa_3cp  <- base::rowSums((!base::is.na(mat_z)) * mat_w_3cp, na.rm = TRUE)
  
  score_cp1_simple <- base::rowSums(mat_z * mat_w_cp1, na.rm = TRUE)
  score_cp1_smart  <- base::ifelse(masa_activa_cp1 > 0, score_cp1_simple / masa_activa_cp1, NA_real_)
  score_3cp_simple <- base::rowSums(mat_z * mat_w_3cp, na.rm = TRUE)
  score_3cp_smart  <- base::ifelse(masa_activa_3cp > 0, score_3cp_simple / masa_activa_3cp, NA_real_)
  
  comp1_raw <- base::rowSums(base::sweep(mat_z, 2, autovectores[, 1], `*`), na.rm = TRUE) / fraccion_obs
  comp2_raw <- base::rowSums(base::sweep(mat_z, 2, autovectores[, 2], `*`), na.rm = TRUE) / fraccion_obs
  comp3_raw <- base::rowSums(base::sweep(mat_z, 2, autovectores[, 3], `*`), na.rm = TRUE) / fraccion_obs
  
  tabla_resumen <- tibble::tibble(
    Modelo                  = nombre_modelo,
    Num_Paises              = dplyr::n_distinct(df_datos$iso3c),
    Num_Variables           = n_cols,
    Observaciones_Retenidas = n_filas,
    Densidad_Datos_Pct      = base::round(base::mean(!base::is.na(mat_z)) * 100, 2),
    Alfa_Cronbach_Std       = base::round(test_alfa$total$std.alpha, 4),
    Cor_InterItem_Media     = base::round(test_alfa$total$average_r, 4),
    KMO_Global_MSA          = base::round(test_kmo$MSA, 4),
    KMO_Minimo_MSA          = base::round(base::min(test_kmo$MSAi), 4),
    Bartlett_Chi2           = base::round(test_bartlett$chisq, 2),
    Autovalor_CP1           = base::round(autovalores[1], 4),
    Varianza_CP1_Pct        = base::round((autovalores[1] / n_cols) * 100, 2),
    Autovalor_CP2           = base::round(autovalores[2], 4),
    Varianza_CP2_Pct        = base::round((autovalores[2] / n_cols) * 100, 2),
    Autovalor_CP3           = base::round(autovalores[3], 4),
    Varianza_CP3_Pct        = base::round((autovalores[3] / n_cols) * 100, 2),
    Varianza_Acum_3CP_Pct   = base::round((base::sum(autovalores[1:3]) / n_cols) * 100, 2),
    Incertidumbre_Lambda_Pct = NA_real_
  )
  
  tabla_autovalores <- tibble::tibble(
    Modelo             = nombre_modelo,
    Componente         = base::paste0("CP", base::seq_len(n_cols)),
    Autovalor          = base::round(autovalores, 4),
    Varianza_Pct       = base::round((autovalores / n_cols) * 100, 2),
    Varianza_Acum_Pct  = base::round(base::cumsum(autovalores / n_cols) * 100, 2),
    Criterio_Kaiser    = base::ifelse(autovalores > 1, "Sí (> 1.0)", "No (< 1.0)"),
    Peso_Entre_3CP_Pct = base::c(base::round(var_rel_3cp * 100, 2), base::rep(0, n_cols - 3))
  )
  
  tabla_pca_3cp <- tibble::tibble(
    Modelo              = nombre_modelo,
    Variable            = vars_activas,
    Dimension_Teorica   = dims_etiquetas,
    Autovector_CP1      = base::round(autovectores[, 1], 4),
    Autovector_CP2      = base::round(autovectores[, 2], 4),
    Autovector_CP3      = base::round(autovectores[, 3], 4),
    Carga_CP1           = base::round(cargas_3cp[, 1], 4),
    Carga_CP2           = base::round(cargas_3cp[, 2], 4),
    Carga_CP3           = base::round(cargas_3cp[, 3], 4),
    Comunalidad_3CP_h2  = base::round(comunalidad_3cp, 4),
    Peso_CP1_Pct        = base::round(pesos_cp1 * 100, 2),
    Peso_CP2_Pct        = base::round(pesos_cp2 * 100, 2),
    Peso_CP3_Pct        = base::round(pesos_cp3 * 100, 2),
    Peso_3CP_Comp_Pct   = base::round(pesos_3cp_comp * 100, 2),
    KMO_Individual_MSA  = base::round(test_kmo$MSAi, 4),
    Cor_Item_Total_Drop = base::round(test_alfa$item.stats[, "r.drop"], 4),
    Alfa_Si_Se_Elimina  = base::round(test_alfa$alpha.drop[, "std.alpha"], 4)
  )
  
  panel_scores <- df_datos |>
    dplyr::select(iso3c, country, year, ctrl_pib_nominal) |>
    dplyr::mutate(
      Vars_Activas_Fila    = vars_obs_fila,
      Masa_Peso_Activo_Pct = base::round(masa_activa_cp1 * 100, 2),
      Comp_CP1_100         = base::round(reescalar_0_100(comp1_raw), 2),
      Comp_CP2_100         = base::round(reescalar_0_100(comp2_raw), 2),
      Comp_CP3_100         = base::round(reescalar_0_100(comp3_raw), 2),
      IMPE_rowSums_Simple  = base::round(reescalar_0_100(score_cp1_simple), 2),
      IMPE_rowSums_Smart   = base::round(reescalar_0_100(score_cp1_smart), 2),
      IMPE_3CP_Smart       = base::round(reescalar_0_100(score_3cp_smart), 2)
    ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      Rank_rowSums_Simple = dplyr::min_rank(dplyr::desc(IMPE_rowSums_Simple)),
      Rank_rowSums_Smart  = dplyr::min_rank(dplyr::desc(IMPE_rowSums_Smart)),
      Rank_3CP_Smart      = dplyr::min_rank(dplyr::desc(IMPE_3CP_Smart))
    ) |>
    dplyr::ungroup()
  
  base::list(
    resumen          = tabla_resumen,
    autovalores      = tabla_autovalores,
    pca_3cp          = tabla_pca_3cp,
    panel_scores     = panel_scores,
    autovectores_raw = autovectores[, 1:3]
  )
}

# 3.B Función para Índices Imputados con MICE-PMM (IMPE Países Depurados Base 16 y Base 12)
ejecutar_modelo_impe_mice_pooled <- function(lista_imp, df_meta_base, vars_activas, dims_etiquetas, nombre_modelo) {
  
  m_imp  <- base::length(lista_imp)
  n_cols <- base::length(vars_activas)
  n_obs  <- base::nrow(df_meta_base)
  
  analizar_una_imputacion <- function(df_imp) {
    df_orig <- df_imp |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(vars_log), ~ base::expm1(.x)),
        fin_regalias_rec = base::sinh(fin_regalias_rec)
      )
    
    mat_z <- base::scale(base::as.matrix(df_orig[, vars_activas]), center = TRUE, scale = TRUE)
    pca_l <- stats::prcomp(mat_z, center = FALSE, scale. = FALSE)
    
    vecs <- pca_l$rotation
    if (vecs["tech_id_pib", 1]     < 0) vecs[, 1] <- -vecs[, 1]
    if (vecs["prod_exp_total", 2]  < 0) vecs[, 2] <- -vecs[, 2]
    if (vecs["fin_mkt_cap_pib", 3] < 0) vecs[, 3] <- -vecs[, 3]
    
    evals <- pca_l$sdev^2
    w_cp1 <- (vecs[, 1]^2) / base::sum(vecs[, 1]^2)
    w_cp2 <- (vecs[, 2]^2) / base::sum(vecs[, 2]^2)
    w_cp3 <- (vecs[, 3]^2) / base::sum(vecs[, 3]^2)
    
    v_rel3 <- evals[1:3] / base::sum(evals[1:3])
    w_3cp  <- (w_cp1 * v_rel3[1]) + (w_cp2 * v_rel3[2]) + (w_cp3 * v_rel3[3])
    w_3cp  <- w_3cp / base::sum(w_3cp)
    
    score_cp1 <- base::as.vector(mat_z %*% w_cp1)
    score_3cp <- base::as.vector(mat_z %*% w_3cp)
    
    comp1 <- base::as.vector(mat_z %*% vecs[, 1])
    comp2 <- base::as.vector(mat_z %*% vecs[, 2])
    comp3 <- base::as.vector(mat_z %*% vecs[, 3])
    
    mat_cor <- stats::cor(mat_z)
    cor_z   <- base::atanh(base::pmin(base::pmax(mat_cor, -0.9999), 0.9999))
    
    base::list(
      evals     = evals,
      vecs3     = vecs[, 1:3],
      w_cp1     = w_cp1,
      w_cp2     = w_cp2,
      w_cp3     = w_cp3,
      w_3cp     = w_3cp,
      cor_z     = cor_z,
      score_cp1 = score_cp1,
      score_3cp = score_3cp,
      comp1     = comp1,
      comp2     = comp2,
      comp3     = comp3
    )
  }
  
  res_list <- base::lapply(lista_imp, analizar_una_imputacion)
  
  # Pooling de la matriz de correlaciones vía transformación z de Fisher (Reglas de Rubin)
  cor_z_media    <- base::Reduce(`+`, base::lapply(res_list, `[[`, "cor_z")) / m_imp
  mat_cor_pooled <- base::tanh(cor_z_media)
  base::diag(mat_cor_pooled) <- 1
  
  # Pooling de Autovalores, Autovectores y Pesos
  evals_mat <- base::sapply(res_list, `[[`, "evals")
  autovalores_media <- base::rowMeans(evals_mat)
  autovalores_sd    <- base::apply(evals_mat, 1, stats::sd)
  
  vecs3_media <- base::Reduce(`+`, base::lapply(res_list, `[[`, "vecs3")) / m_imp
  cargas_3cp  <- base::sweep(vecs3_media, 2, base::sqrt(autovalores_media[1:3]), `*`)
  comunalidad_3cp <- base::rowSums(cargas_3cp^2)
  
  w_cp1_mat <- base::sapply(res_list, `[[`, "w_cp1") * 100
  w_cp2_mat <- base::sapply(res_list, `[[`, "w_cp2") * 100
  w_cp3_mat <- base::sapply(res_list, `[[`, "w_cp3") * 100
  w_3cp_mat <- base::sapply(res_list, `[[`, "w_3cp") * 100
  
  var_rel_3cp <- autovalores_media[1:3] / base::sum(autovalores_media[1:3])
  
  # Pruebas Psicométricas sobre la Matriz Combinada
  test_alfa     <- psych::alpha(mat_cor_pooled, n.obs = n_obs, check.keys = TRUE)
  test_kmo      <- psych::KMO(mat_cor_pooled)
  test_bartlett <- psych::cortest.bartlett(mat_cor_pooled, n = n_obs)
  
  # Pooling de Puntajes y Cálculo de Incertidumbre (Lambda de Rubin y Van Buuren)
  scores_cp1_mat <- base::sapply(res_list, `[[`, "score_cp1")
  scores_3cp_mat <- base::sapply(res_list, `[[`, "score_3cp")
  comp1_mat      <- base::sapply(res_list, `[[`, "comp1")
  comp2_mat      <- base::sapply(res_list, `[[`, "comp2")
  comp3_mat      <- base::sapply(res_list, `[[`, "comp3")
  
  score_cp1_pooled <- base::rowMeans(scores_cp1_mat)
  score_cp1_sd     <- base::apply(scores_cp1_mat, 1, stats::sd)
  score_3cp_pooled <- base::rowMeans(scores_3cp_mat)
  
  B_vec     <- base::apply(scores_cp1_mat, 1, stats::var)
  var_total <- stats::var(score_cp1_pooled)
  lambda_ef <- base::mean(((1 + 1 / m_imp) * B_vec) / var_total) * 100
  
  tabla_resumen <- tibble::tibble(
    Modelo                   = nombre_modelo,
    Num_Paises               = dplyr::n_distinct(df_meta_base$iso3c),
    Num_Variables            = n_cols,
    Observaciones_Retenidas  = n_obs,
    Densidad_Datos_Pct       = 100.00,
    Alfa_Cronbach_Std        = base::round(test_alfa$total$std.alpha, 4),
    Cor_InterItem_Media      = base::round(test_alfa$total$average_r, 4),
    KMO_Global_MSA           = base::round(test_kmo$MSA, 4),
    KMO_Minimo_MSA           = base::round(base::min(test_kmo$MSAi), 4),
    Bartlett_Chi2            = base::round(test_bartlett$chisq, 2),
    Autovalor_CP1            = base::round(autovalores_media[1], 4),
    Varianza_CP1_Pct         = base::round((autovalores_media[1] / n_cols) * 100, 2),
    Autovalor_CP2            = base::round(autovalores_media[2], 4),
    Varianza_CP2_Pct         = base::round((autovalores_media[2] / n_cols) * 100, 2),
    Autovalor_CP3            = base::round(autovalores_media[3], 4),
    Varianza_CP3_Pct         = base::round((autovalores_media[3] / n_cols) * 100, 2),
    Varianza_Acum_3CP_Pct    = base::round((base::sum(autovalores_media[1:3]) / n_cols) * 100, 2),
    Incertidumbre_Lambda_Pct = base::round(lambda_ef, 2)
  )
  
  tabla_autovalores <- tibble::tibble(
    Modelo             = nombre_modelo,
    Componente         = base::paste0("CP", base::seq_len(n_cols)),
    Autovalor          = base::round(autovalores_media, 4),
    Autovalor_SD_Imp   = base::round(autovalores_sd, 4),
    Varianza_Pct       = base::round((autovalores_media / n_cols) * 100, 2),
    Varianza_Acum_Pct  = base::round(base::cumsum(autovalores_media / n_cols) * 100, 2),
    Criterio_Kaiser    = base::ifelse(autovalores_media > 1, "Sí (> 1.0)", "No (< 1.0)"),
    Peso_Entre_3CP_Pct = base::c(base::round(var_rel_3cp * 100, 2), base::rep(0, n_cols - 3))
  )
  
  tabla_pca_3cp <- tibble::tibble(
    Modelo              = nombre_modelo,
    Variable            = vars_activas,
    Dimension_Teorica   = dims_etiquetas,
    Autovector_CP1      = base::round(vecs3_media[, 1], 4),
    Autovector_CP2      = base::round(vecs3_media[, 2], 4),
    Autovector_CP3      = base::round(vecs3_media[, 3], 4),
    Carga_CP1           = base::round(cargas_3cp[, 1], 4),
    Carga_CP2           = base::round(cargas_3cp[, 2], 4),
    Carga_CP3           = base::round(cargas_3cp[, 3], 4),
    Comunalidad_3CP_h2  = base::round(comunalidad_3cp, 4),
    Peso_CP1_Pct        = base::round(base::rowMeans(w_cp1_mat), 2),
    Error_Imp_CP1_SD    = base::round(base::apply(w_cp1_mat, 1, stats::sd), 3),
    Peso_CP2_Pct        = base::round(base::rowMeans(w_cp2_mat), 2),
    Peso_CP3_Pct        = base::round(base::rowMeans(w_cp3_mat), 2),
    Peso_3CP_Comp_Pct   = base::round(base::rowMeans(w_3cp_mat), 2),
    KMO_Individual_MSA  = base::round(test_kmo$MSAi, 4),
    Cor_Item_Total_Drop = base::round(test_alfa$item.stats[, "r.drop"], 4),
    Alfa_Si_Se_Elimina  = base::round(test_alfa$alpha.drop[, "std.alpha"], 4)
  )
  
  panel_scores <- df_meta_base |>
    dplyr::select(iso3c, country, year, ctrl_pib_nominal, Pct_NA_Pais) |>
    dplyr::mutate(
      Comp_CP1_100          = base::round(reescalar_0_100(base::rowMeans(comp1_mat)), 2),
      Comp_CP2_100          = base::round(reescalar_0_100(base::rowMeans(comp2_mat)), 2),
      Comp_CP3_100          = base::round(reescalar_0_100(base::rowMeans(comp3_mat)), 2),
      IMPE_Bruto_Pooled     = base::round(score_cp1_pooled, 4),
      IMPE_Incertidumbre_SD = base::round(score_cp1_sd, 4),
      IMPE_Depurados_100    = base::round(reescalar_0_100(score_cp1_pooled), 2),
      IMPE_Depurados_3CP    = base::round(reescalar_0_100(score_3cp_pooled), 2)
    ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      Rank_Depurados_100 = dplyr::min_rank(dplyr::desc(IMPE_Depurados_100)),
      Rank_Depurados_3CP = dplyr::min_rank(dplyr::desc(IMPE_Depurados_3CP))
    ) |>
    dplyr::ungroup()
  
  base::list(
    resumen          = tabla_resumen,
    autovalores      = tabla_autovalores,
    pca_3cp          = tabla_pca_3cp,
    panel_scores     = panel_scores,
    autovectores_raw = vecs3_media
  )
}


# ==============================================================================
# ETAPA 4: EJECUCIÓN DE LOS 6 ÍNDICES Y CONSOLIDACIÓN DE TABLAS COMPARATIVAS
# ==============================================================================

dims_16 <- c(base::rep("Productiva", 6), base::rep("Financiera", 4), base::rep("Tecnológica", 6))
dims_12 <- c(base::rep("Productiva", 4), base::rep("Financiera", 3), base::rep("Tecnológica", 5))

# 4.1 Índices Sin Imputar y Exclusivamente Interpolados con rowSums() (182 países)
mod_obs_16 <- ejecutar_modelo_impe_rowsums(panel_observado,   vars_impe_16, dims_16, "1A_Sin_Imputar_Base16")
mod_int_16 <- ejecutar_modelo_impe_rowsums(panel_interpolado, vars_impe_16, dims_16, "2A_Interpolado_Base16")
mod_obs_12 <- ejecutar_modelo_impe_rowsums(panel_observado,   vars_impe_12, dims_12, "1B_Sin_Imputar_Base12")
mod_int_12 <- ejecutar_modelo_impe_rowsums(panel_interpolado, vars_impe_12, dims_12, "2B_Interpolado_Base12")

# 4.2 NUEVOS ÍNDICES: IMPE PAÍSES DEPURADOS (<= 30% NAs, Interpolado + MICE-PMM, Base 16 y Base 12)
mod_dep_16 <- ejecutar_modelo_impe_mice_pooled(
  lista_imp_depurados, panel_paises_depurados_base, vars_impe_16, dims_16, "3A_IMPE_Paises_Depurados_Base16"
)
mod_dep_12 <- ejecutar_modelo_impe_mice_pooled(
  lista_imp_depurados, panel_paises_depurados_base, vars_impe_12, dims_12, "3B_IMPE_Paises_Depurados_Base12"
)

# 4.3 Tabla Resumen Comparativa de los 6 Modelos (Pruebas Globales y 3 Primeras Componentes)
tabla_resumen_comparativa <- dplyr::bind_rows(
  mod_obs_16$resumen,
  mod_int_16$resumen,
  mod_dep_16$resumen,
  mod_obs_12$resumen,
  mod_int_12$resumen,
  mod_dep_12$resumen
)

base::cat("
================ TABLA COMPARATIVA DE LOS 6 ÍNDICES CONSTRUIDOS ================
")
base::print(tabla_resumen_comparativa)

# 4.4 Panel Consolidado de IMPE Países Depurados (Uniendo Base 16 y Base 12 en la cohorte <= 30% NAs)
panel_paises_depurados_final <- mod_dep_16$panel_scores |>
  dplyr::select(
    iso3c, country, year, ctrl_pib_nominal, Pct_NA_Pais,
    Dep16_CP1_100             = Comp_CP1_100,
    Dep16_CP2_100             = Comp_CP2_100,
    Dep16_CP3_100             = Comp_CP3_100,
    IMPE_Depurados_Base16_100 = IMPE_Depurados_100,
    IMPE_Depurados_Base16_3CP = IMPE_Depurados_3CP,
    IMPE_Depurados_Base16_SD  = IMPE_Incertidumbre_SD,
    Rank_Depurados_Base16     = Rank_Depurados_100
  ) |>
  dplyr::left_join(
    mod_dep_12$panel_scores |>
      dplyr::select(
        iso3c, year,
        Dep12_CP1_100             = Comp_CP1_100,
        Dep12_CP2_100             = Comp_CP2_100,
        Dep12_CP3_100             = Comp_CP3_100,
        IMPE_Depurados_Base12_100 = IMPE_Depurados_100,
        IMPE_Depurados_Base12_3CP = IMPE_Depurados_3CP,
        IMPE_Depurados_Base12_SD  = IMPE_Incertidumbre_SD,
        Rank_Depurados_Base12     = Rank_Depurados_100
      ),
    by = c("iso3c", "year")
  )

# 4.5 Unión de TODOS los índices en el Panel Maestro General
panel_comparado_maestro <- mod_obs_16$panel_scores |>
  dplyr::select(
    iso3c, country, year, ctrl_pib_nominal,
    Obs16_Vars_Activas = Vars_Activas_Fila,
    IMPE_Obs16_Simple  = IMPE_rowSums_Simple,
    IMPE_Obs16_Smart   = IMPE_rowSums_Smart,
    IMPE_Obs16_3CP     = IMPE_3CP_Smart,
    Rank_Obs16_Smart   = Rank_rowSums_Smart
  ) |>
  dplyr::left_join(
    mod_int_16$panel_scores |>
      dplyr::select(
        iso3c, year,
        Int16_Vars_Activas = Vars_Activas_Fila,
        IMPE_Int16_Simple  = IMPE_rowSums_Simple,
        IMPE_Int16_Smart   = IMPE_rowSums_Smart,
        IMPE_Int16_3CP     = IMPE_3CP_Smart,
        Rank_Int16_Smart   = Rank_rowSums_Smart
      ),
    by = c("iso3c", "year")
  ) |>
  dplyr::left_join(
    mod_obs_12$panel_scores |>
      dplyr::select(iso3c, year, IMPE_Obs12_Smart = IMPE_rowSums_Smart, Rank_Obs12_Smart = Rank_rowSums_Smart),
    by = c("iso3c", "year")
  ) |>
  dplyr::left_join(
    mod_int_12$panel_scores |>
      dplyr::select(iso3c, year, IMPE_Int12_Smart = IMPE_rowSums_Smart, Rank_Int12_Smart = Rank_rowSums_Smart),
    by = c("iso3c", "year")
  ) |>
  dplyr::left_join(
    panel_paises_depurados_final |>
      dplyr::select(
        iso3c, year, Pct_NA_Pais,
        IMPE_Depurados_Base16_100, IMPE_Depurados_Base16_3CP, Rank_Depurados_Base16,
        IMPE_Depurados_Base12_100, IMPE_Depurados_Base12_3CP, Rank_Depurados_Base12
      ),
    by = c("iso3c", "year")
  )

cols_indices_corr <- panel_comparado_maestro |>
  dplyr::select(dplyr::starts_with("IMPE_"))

matriz_pearson_indices  <- base::round(stats::cor(cols_indices_corr, use = "pairwise.complete.obs", method = "pearson"), 4) |>
  tibble::as_tibble(rownames = "Indice")
matriz_spearman_indices <- base::round(stats::cor(cols_indices_corr, use = "pairwise.complete.obs", method = "spearman"), 4) |>
  tibble::as_tibble(rownames = "Indice")

ranking_comparado_2024 <- panel_comparado_maestro |>
  dplyr::filter(year == base::max(year, na.rm = TRUE)) |>
  dplyr::arrange(Rank_Depurados_Base12, Rank_Int16_Smart)


# ==============================================================================
# ETAPA 5: EXPORTACIÓN A UN SOLO EXCEL CON TODOS LOS ÍNDICES Y SUS PESTAÑAS
# ==============================================================================

wb <- openxlsx::createWorkbook(creator = "Javier Morros - Tesis UCV")

estilo_titulo <- openxlsx::createStyle(
  fontName = "Calibri", fontSize = 13, fontColour = "#FFFFFF",
  fgFill = "#0F172A", halign = "left", valign = "center", textDecoration = "bold"
)
estilo_subtitulo <- openxlsx::createStyle(
  fontName = "Calibri", fontSize = 10, fontColour = "#E2E8F0",
  fgFill = "#1E293B", halign = "left", valign = "center", textDecoration = "italic"
)
estilo_seccion <- openxlsx::createStyle(
  fontName = "Calibri", fontSize = 11, fontColour = "#1E3A8A",
  textDecoration = "bold", valign = "center"
)
estilo_header_azul <- openxlsx::createStyle(
  fontName = "Calibri", fontSize = 10, fontColour = "#FFFFFF",
  fgFill = "#1E3A8A", halign = "center", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight", wrapText = TRUE
)
estilo_header_verde <- openxlsx::createStyle(
  fontName = "Calibri", fontSize = 10, fontColour = "#FFFFFF",
  fgFill = "#0F766E", halign = "center", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight", wrapText = TRUE
)
estilo_celda <- openxlsx::createStyle(
  fontName = "Calibri", fontSize = 10, valign = "center",
  border = "TopBottomLeftRight", borderColour = "#CBD5E1"
)

crear_hoja_profesional <- function(wb, nombre_hoja, titulo, subtitulo, lista_tablas) {
  openxlsx::addWorksheet(wb, sheetName = nombre_hoja, gridLines = TRUE)
  max_cols <- base::max(base::sapply(lista_tablas, function(x) base::ncol(x$datos)))
  max_cols <- base::max(max_cols, 8)
  
  openxlsx::mergeCells(wb, sheet = nombre_hoja, cols = 1:max_cols, rows = 1)
  openxlsx::mergeCells(wb, sheet = nombre_hoja, cols = 1:max_cols, rows = 2)
  openxlsx::writeData(wb, sheet = nombre_hoja, x = titulo, startCol = 1, startRow = 1)
  openxlsx::writeData(wb, sheet = nombre_hoja, x = subtitulo, startCol = 1, startRow = 2)
  openxlsx::addStyle(wb, sheet = nombre_hoja, style = estilo_titulo, rows = 1, cols = 1:max_cols, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = nombre_hoja, style = estilo_subtitulo, rows = 2, cols = 1:max_cols, gridExpand = TRUE)
  openxlsx::setRowHeights(wb, sheet = nombre_hoja, rows = 1:2, heights = c(26, 19))
  
  fila_actual <- 4
  for (item in lista_tablas) {
    if (!base::is.null(item$encabezado_seccion)) {
      openxlsx::writeData(wb, sheet = nombre_hoja, x = item$encabezado_seccion, startCol = 1, startRow = fila_actual)
      openxlsx::addStyle(wb, sheet = nombre_hoja, style = estilo_seccion, rows = fila_actual, cols = 1)
      fila_actual <- fila_actual + 1
    }
    df_t <- item$datos
    n_r  <- base::nrow(df_t)
    n_c  <- base::ncol(df_t)
    
    openxlsx::writeDataTable(
      wb, sheet = nombre_hoja, x = df_t,
      startRow = fila_actual, startCol = 1,
      tableStyle = "TableStyleMedium2", withFilter = TRUE
    )
    est_hdr <- if (!base::is.null(item$color_verde) && item$color_verde) estilo_header_verde else estilo_header_azul
    openxlsx::addStyle(wb, sheet = nombre_hoja, style = est_hdr, rows = fila_actual, cols = 1:n_c, gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet = nombre_hoja, style = estilo_celda, rows = (fila_actual + 1):(fila_actual + n_r), cols = 1:n_c, gridExpand = TRUE)
    openxlsx::setRowHeights(wb, sheet = nombre_hoja, rows = fila_actual, heights = 25)
    fila_actual <- fila_actual + n_r + 3
  }
  openxlsx::freezePane(wb, sheet = nombre_hoja, firstActiveRow = 6, firstActiveCol = 1)
  openxlsx::setColWidths(wb, sheet = nombre_hoja, cols = 1:max_cols, widths = "auto")
}

# Construcción de las 10 Pestañas Ordenadas en el Excel Maestro
crear_hoja_profesional(
  wb, "1_Dashboard_Comparativo",
  "CUADRO DE MANDO: COMPARACIÓN DE LAS 3 COMPONENTES PRINCIPALES Y ALFA DE CRONBACH EN LOS 6 ÍNDICES",
  "Compara: (1) Sin Imputar rowSums, (2) Exclusivamente Interpolado rowSums y (3) IMPE Países Depurados (<= 30% NAs, Interpolado + PMM)",
  base::list(
    base::list(encabezado_seccion = "1. COMPARACIÓN GLOBAL DE PRUEBAS PSICOMÉTRICAS Y LAS 3 PRIMERAS COMPONENTES (BASE 16 Y BASE 12)", datos = tabla_resumen_comparativa, color_verde = FALSE),
    base::list(encabezado_seccion = "2. MATRIZ DE CORRELACIÓN LINEAL DE PEARSON ENTRE TODOS LOS ÍNDICES", datos = matriz_pearson_indices, color_verde = TRUE),
    base::list(encabezado_seccion = "3. MATRIZ DE CORRELACIÓN DE RANGOS DE SPEARMAN ENTRE TODOS LOS ÍNDICES", datos = matriz_spearman_indices, color_verde = FALSE)
  )
)

crear_hoja_profesional(
  wb, "2_PCA_Sin_Imputar",
  "A.C.P. (3 COMPONENTES) Y ALFA DE CRONBACH — ÍNDICE 1: SIN IMPUTAR (rowSums, BASE 16 Y BASE 12)",
  "Autovectores, Cargas Factoriales, Comunalidades, Pesos y Fiabilidad sobre Datos Observados",
  base::list(
    base::list(encabezado_seccion = "1. ACP DE LAS 3 PRIMERAS COMPONENTES Y ALFA — BASE 16 SIN IMPUTAR", datos = mod_obs_16$pca_3cp, color_verde = FALSE),
    base::list(encabezado_seccion = "2. ACP DE LAS 3 PRIMERAS COMPONENTES Y ALFA — BASE 12 SIN IMPUTAR", datos = mod_obs_12$pca_3cp, color_verde = TRUE),
    base::list(encabezado_seccion = "3. AUTOVALORES COMPLETOS (SIN IMPUTAR BASE 16 Y BASE 12)", datos = dplyr::bind_rows(mod_obs_16$autovalores, mod_obs_12$autovalores), color_verde = FALSE)
  )
)

crear_hoja_profesional(
  wb, "3_PCA_Interpolado",
  "A.C.P. (3 COMPONENTES) Y ALFA DE CRONBACH — ÍNDICE 2: EXCLUSIVAMENTE INTERPOLADO (rowSums, BASE 16 Y BASE 12)",
  "Autovectores, Cargas Factoriales, Comunalidades, Pesos y Fiabilidad sobre Datos Interpolados Longitudinalmente",
  base::list(
    base::list(encabezado_seccion = "1. ACP DE LAS 3 PRIMERAS COMPONENTES Y ALFA — BASE 16 INTERPOLADO", datos = mod_int_16$pca_3cp, color_verde = TRUE),
    base::list(encabezado_seccion = "2. ACP DE LAS 3 PRIMERAS COMPONENTES Y ALFA — BASE 12 INTERPOLADO", datos = mod_int_12$pca_3cp, color_verde = FALSE),
    base::list(encabezado_seccion = "3. AUTOVALORES COMPLETOS (INTERPOLADO BASE 16 Y BASE 12)", datos = dplyr::bind_rows(mod_int_16$autovalores, mod_int_12$autovalores), color_verde = TRUE)
  )
)

crear_hoja_profesional(
  wb, "4_PCA_Paises_Depurados",
  "A.C.P. (3 COMPONENTES) Y ALFA DE CRONBACH — ÍNDICE 3: IMPE PAÍSES DEPURADOS (<= 30% NAs, INTERPOLADO + PMM)",
  "Combinación vía Reglas de Rubin sobre los Países con <= 30% de Datos Faltantes: Contraste Base 16 vs. Base 12",
  base::list(
    base::list(encabezado_seccion = "1. ACP COMBINADO (3 COMPONENTES) Y ALFA — IMPE PAÍSES DEPURADOS BASE 16", datos = mod_dep_16$pca_3cp, color_verde = FALSE),
    base::list(encabezado_seccion = "2. ACP COMBINADO (3 COMPONENTES) Y ALFA — IMPE PAÍSES DEPURADOS BASE 12 (NÚCLEO PURO)", datos = mod_dep_12$pca_3cp, color_verde = TRUE),
    base::list(encabezado_seccion = "3. AUTOVALORES COMBINADOS (IMPE PAÍSES DEPURADOS BASE 16 Y BASE 12)", datos = dplyr::bind_rows(mod_dep_16$autovalores, mod_dep_12$autovalores), color_verde = FALSE)
  )
)

crear_hoja_profesional(
  wb, "5_Ranking_Comparado_2024",
  "RANKING MUNDIAL COMPARADO EN 2024 DE TODOS LOS ÍNDICES",
  "Comparación lado a lado de IMPE Países Depurados (Base 16 y Base 12), Índice Interpolado e Índice Sin Imputar",
  base::list(
    base::list(encabezado_seccion = "1. TABLA COMPARATIVA EN EL AÑO 2024", datos = ranking_comparado_2024, color_verde = FALSE)
  )
)

crear_hoja_profesional(
  wb, "6_IMPE_Paises_Depurados",
  "RESULTADOS COMPLETOS: IMPE PAÍSES DEPURADOS (<= 30% NAs, INTERPOLADO + MICE-PMM, BASE 16 Y BASE 12)",
  "Panel Completo de los Países con <= 30% de Datos Faltantes (1995-2024) con Componentes CP1, CP2, CP3, Base 16 y Base 12",
  base::list(
    base::list(encabezado_seccion = "1. PANEL CONSOLIDADO IMPE PAÍSES DEPURADOS (BASE 16 Y BASE 12 UNIDOS)", datos = panel_paises_depurados_final, color_verde = TRUE)
  )
)

crear_hoja_profesional(
  wb, "7_IMPE_Depurados_Base16",
  "RESULTADO INDIVIDUAL: IMPE PAÍSES DEPURADOS BASE 16 (<= 30% NAs, INTERPOLADO + PMM)",
  "Puntajes en las 3 Primeras Componentes (CP1, CP2, CP3), Puntaje Bruto Pooled, Incertidumbre SD y Ranking Anual (Base 16)",
  base::list(
    base::list(encabezado_seccion = "1. PANEL DE RESULTADOS — IMPE PAÍSES DEPURADOS BASE 16", datos = mod_dep_16$panel_scores, color_verde = FALSE)
  )
)

crear_hoja_profesional(
  wb, "8_IMPE_Depurados_Base12",
  "RESULTADO INDIVIDUAL: IMPE PAÍSES DEPURADOS BASE 12 (<= 30% NAs, INTERPOLADO + PMM)",
  "Puntajes en las 3 Primeras Componentes (CP1, CP2, CP3), Puntaje Bruto Pooled, Incertidumbre SD y Ranking Anual (Base 12)",
  base::list(
    base::list(encabezado_seccion = "1. PANEL DE RESULTADOS — IMPE PAÍSES DEPURADOS BASE 12", datos = mod_dep_12$panel_scores, color_verde = TRUE)
  )
)

crear_hoja_profesional(
  wb, "9_Resultado_Sin_Imputar",
  "RESULTADOS COMPLETOS DEL ÍNDICE SIN IMPUTAR (DATOS OBSERVADOS + rowSums, 1995-2024)",
  "Panel de 182 Países Soberanos (N = 5,460) con Componentes CP1, CP2, CP3 e Índices rowSums",
  base::list(
    base::list(encabezado_seccion = "1. PANEL COMPLETO — ÍNDICE SIN IMPUTAR", datos = mod_obs_16$panel_scores, color_verde = FALSE)
  )
)

crear_hoja_profesional(
  wb, "10_Panel_Maestro_Todos",
  "PANEL MAESTRO CONSOLIDADO: TODOS LOS ÍNDICES UNIDOS LADO A LADO (1995-2024)",
  "Unifica Sin Imputar (Base 16 y 12), Interpolado (Base 16 y 12) e IMPE Países Depurados (Base 16 y 12)",
  base::list(
    base::list(encabezado_seccion = "1. MATRIZ MAESTRA DE TODOS LOS ÍNDICES CONSTRUIDOS", datos = panel_comparado_maestro, color_verde = TRUE)
  )
)

nombre_archivo_excel <- "IMPE_Consolidado_Todos_y_Paises_Depurados_R.xlsx"
openxlsx::saveWorkbook(wb, file = nombre_archivo_excel, overwrite = TRUE)

base::cat("
==============================================================================
")
base::cat("¡PROCESO COMPLETADO CON ÉXITO!
")
base::cat("Archivo Excel generado en tu directorio:", nombre_archivo_excel, "
")
base::cat("==============================================================================
")
