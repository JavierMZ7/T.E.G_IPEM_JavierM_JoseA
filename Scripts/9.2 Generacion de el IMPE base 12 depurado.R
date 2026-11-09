# ==============================================================================
# SCRIPT MAESTRO DEFINITIVO Y COMPLETO (v5 - BLINDADO CONTRA COLUMNAS FALTANTES):
# CONSTRUCCIÓN, IMPUTACIÓN MÚLTIPLE (MICE-PMM), PAÍSES DEPURADOS (<= 30% NAs),
# ÍNDICES CON rowSums() SIN drop_na(), ANÁLISIS DE LAS 3 COMPONENTES PRINCIPALES,
# AUDITORÍA PSICOMÉTRICA, GRÁFICOS Y EXPORTACIÓN A UN SOLO EXCEL MAESTRO
# ==============================================================================
# INSTRUCCIÓN IMPORTANTE ANTES DE CORRER EN RSTUDIO:
# 1. Si tu consola muestra "Browse[1]>", escribe Q (mayúscula) y presiona Enter
#    (o haz clic en el botón rojo "Stop" arriba a la derecha de la consola)
#    para salir del modo depuración.
# 2. Selecciona TODO este script (Ctrl + A) y presiona Ctrl + Enter (Run).
# ==============================================================================


# ------------------------------------------------------------------------------
# ETAPA 0: DEFINICIÓN INMEDIATA DE VECTORES GLOBALES Y VERIFICACIÓN DE PAQUETES
# ------------------------------------------------------------------------------

# 0.1 Vectores de Variables en Base 16 (Marco Teórico Completo)
vars_productiva_16  <- c("prod_manuf_pib", "prod_exp_total", "prod_fbcf_pib",
                         "tech_mnf_val_added", "prod_eci", "prod_pib_pcap_ppa")
vars_financiera_16  <- c("fin_ied_sal_pib", "fin_mkt_cap_pib",
                         "fin_cred_priv_pib", "fin_regalias_rec")
vars_tecnologica_16 <- c("tech_id_pib", "tech_investigad", "tech_exp_alta",
                         "tech_serv_ict_pct", "tech_exp_serv_dig", "tech_patentes_tic")
vars_impe_16        <- c(vars_productiva_16, vars_financiera_16, vars_tecnologica_16)

# 0.2 Vectores de Variables en Base 12 (Núcleo Esencial Depurado sin las 4 ruidosas)
vars_productiva_12  <- c("prod_exp_total", "tech_mnf_val_added", "prod_eci", "prod_pib_pcap_ppa")
vars_financiera_12  <- c("fin_mkt_cap_pib", "fin_cred_priv_pib", "fin_regalias_rec")
vars_tecnologica_12 <- c("tech_id_pib", "tech_investigad", "tech_exp_alta",
                         "tech_exp_serv_dig", "tech_patentes_tic")
vars_impe_12        <- c(vars_productiva_12, vars_financiera_12, vars_tecnologica_12)

dims_16 <- c(base::rep("Productiva", 6), base::rep("Financiera", 4), base::rep("Tecnológica", 6))
dims_12 <- c(base::rep("Productiva", 4), base::rep("Financiera", 3), base::rep("Tecnológica", 5))

# 0.3 Variables positivas con asimetría extrema que se estabilizan en log(x+1) durante MICE
vars_log_mice <- c("prod_exp_total", "prod_pib_pcap_ppa", "fin_mkt_cap_pib",
                   "tech_exp_serv_dig", "tech_patentes_tic", "ctrl_pib_nominal")

# 0.4 Lista de los 48 códigos de Agregados Regionales/Financieros del Banco Mundial a excluir
agregados_wb <- c(
  "AFE", "AFW", "ARB", "CEB", "CSS", "EAP", "EAR", "EAS", "ECA", "ECS",
  "EMU", "EUU", "FCS", "HIC", "HPC", "IBD", "IBT", "IDA", "IDB", "IDX",
  "INX", "LAC", "LCN", "LDC", "LIC", "LMC", "LMY", "LTE", "MEA", "MIC",
  "MNA", "NAC", "OED", "OSS", "PRE", "PSS", "PST", "SAS", "SSA", "SSF",
  "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC", "WLD"
)

# 0.5 Instalación automática de paquetes requeridos si alguno no está instalado
paquetes_requeridos <- c("dplyr", "tidyr", "readr", "readxl", "zoo", "psych",
                         "mice", "openxlsx", "tibble", "ggplot2")
paquetes_faltantes  <- paquetes_requeridos[!(paquetes_requeridos %in% utils::installed.packages()[, "Package"])]
if (base::length(paquetes_faltantes) > 0) {
  utils::install.packages(paquetes_faltantes)
}


# ==============================================================================
# ETAPA 1: CARGA INTELIGENTE DEL PANEL Y NORMALIZACIÓN DE LA COLUMNA 'country'
# ==============================================================================

# 1.1 Detección automática de 'panel_final' en memoria o reconstrucción desde archivos
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
    } else if (!("tech_mnf_val_added" %in% base::names(panel_raw)) && base::file.exists("NN.MNF.TECH.ZS.UN.DS2.csv")) {
      nn_csv <- readr::read_csv("NN.MNF.TECH.ZS.UN.DS2.csv", skip = 4, show_col_types = FALSE)
      nn_long <- nn_csv |>
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
    base::stop("No se encontró 'panel_final' en memoria ni el archivo CSV en tu directorio de trabajo.")
  }
}

# 1.2 Garantizar que 'panel_final' tenga siempre la columna 'country'
if (!("country" %in% base::names(panel_final))) {
  if (base::file.exists("panel_data_consolidado.csv")) {
    catalogo_paises <- readr::read_csv("panel_data_consolidado.csv", show_col_types = FALSE) |>
      dplyr::select(iso3c, dplyr::any_of("country")) |>
      dplyr::distinct(iso3c, .keep_all = TRUE)
    if ("country" %in% base::names(catalogo_paises)) {
      panel_final <- panel_final |> dplyr::left_join(catalogo_paises, by = "iso3c")
    } else {
      panel_final$country <- panel_final$iso3c
    }
  } else {
    panel_final$country <- panel_final$iso3c
  }
}

# Crear diccionario maestro iso3c -> country para inyectarlo en cualquier tabla que no lo tenga
diccionario_paises <- panel_final |>
  dplyr::filter(!base::is.na(iso3c)) |>
  dplyr::select(iso3c, country) |>
  dplyr::distinct(iso3c, .keep_all = TRUE)

asegurar_columna_country <- function(df) {
  if (!("country" %in% base::names(df))) {
    df <- df |>
      dplyr::left_join(diccionario_paises, by = "iso3c") |>
      dplyr::mutate(country = dplyr::coalesce(country, iso3c))
  }
  df
}

# 1.3 Identificación limpia de países soberanos reales (excluye agregados del Banco Mundial y vacíos "")
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

# 1.4 BASE 1: DATOS OBSERVADOS SIN IMPUTAR (1995-2024, 182 países soberanos con >= 35% cobertura)
panel_temp <- panel_final |>
  asegurar_columna_country() |>
  dplyr::mutate(iso3c = base::trimws(base::as.character(iso3c))) |>
  dplyr::filter(
    !base::is.na(iso3c),
    base::nchar(iso3c) == 3,
    iso3c %in% codigos_soberanos,
    year >= 1995,
    year <= 2024
  )

panel_temp$n_obs_fila_16 <- base::rowSums(!base::is.na(panel_temp[, vars_impe_16]))

panel_observado <- panel_temp |>
  dplyr::group_by(iso3c) |>
  dplyr::filter(base::mean(n_obs_fila_16 / base::length(vars_impe_16), na.rm = TRUE) >= 0.35) |>
  dplyr::ungroup() |>
  dplyr::select(-n_obs_fila_16) |>
  dplyr::arrange(iso3c, year)

# 1.5 BASE 2: DATOS EXCLUSIVAMENTE INTERPOLADOS (Interpolación longitudinal interna sin extrapolar bordes)
interpolar_serie_interna <- function(v, t_anio, max_brecha = Inf) {
  if (base::sum(!base::is.na(v)) < 2) return(v)
  zoo::na.approx(v, x = t_anio, maxgap = max_brecha, na.rm = FALSE, rule = 1)
}

panel_interpolado <- panel_observado |>
  dplyr::group_by(iso3c) |>
  dplyr::arrange(year, .by_group = TRUE) |>
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::all_of(c(vars_impe_16, "ctrl_pib_nominal")),
      .fns  = ~ interpolar_serie_interna(.x, year, max_brecha = Inf)
    )
  ) |>
  dplyr::ungroup()

# 1.6 BASE 3: PANEL GLOBAL PRE-MICE (182 países con interpolación corta maxgap = 3)
panel_global_pre_mice <- panel_observado |>
  dplyr::group_by(iso3c) |>
  dplyr::arrange(year, .by_group = TRUE) |>
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::all_of(c(vars_impe_16, "ctrl_pib_nominal")),
      .fns  = ~ interpolar_serie_interna(.x, year, max_brecha = 3)
    )
  ) |>
  dplyr::ungroup()

# 1.7 BASE 4: PAÍSES DEPURADOS (ELIMINANDO PAÍSES CON MÁS DEL 30% DE DATA FALTANTE)
panel_obs_conteo <- panel_observado
panel_obs_conteo$prop_na_fila <- base::rowMeans(base::is.na(panel_obs_conteo[, vars_impe_16]))

paises_depurados_30pct <- panel_obs_conteo |>
  dplyr::group_by(iso3c) |>
  dplyr::summarise(pct_na_pais = base::mean(prop_na_fila, na.rm = TRUE) * 100) |>
  dplyr::filter(pct_na_pais <= 30.0) |>
  dplyr::pull(iso3c)

base::cat("
==============================================================================
")
base::cat("DIAGNÓSTICO DE COBERTURA DE PAÍSES (1995-2024):
")
base::cat("1. Panel Global Soberano (>= 35% datos obs):", dplyr::n_distinct(panel_observado$iso3c), "países
")
base::cat("2. Panel Países Depurados (<= 30% NAs):     ", base::length(paises_depurados_30pct), "países
")
base::cat("==============================================================================
")

panel_depurados_pre_mice <- panel_global_pre_mice |>
  dplyr::filter(iso3c %in% paises_depurados_30pct)

# 1.8 Diagnóstico de Conectividad de Van Buuren (Influx y Outflux, Cap. 4)
flux_global <- mice::flux(panel_observado[, c(vars_impe_16, "ctrl_pib_nominal")]) |>
  tibble::rownames_to_column(var = "Variable") |>
  dplyr::select(Variable, pobs, influx, outflux) |>
  dplyr::mutate(
    pmis        = base::round(1 - pobs, 3),
    pobs        = base::round(pobs, 3),
    influx      = base::round(influx, 3),
    outflux     = base::round(outflux, 3),
    Suma_In_Out = base::round(influx + outflux, 3),
    Rol_MICE    = base::ifelse(outflux >= 0.65, "Bloque Ancla (Predictor Fuerte)", "Bloque Receptor (Alto Influx)")
  )


# ==============================================================================
# ETAPA 2: FUNCIONES MAESTRAS DE A.C.P. (3 COMPONENTES), ALFA, rowSums() Y MICE
# ==============================================================================

reescalar_0_100 <- function(x) {
  rango <- base::max(x, na.rm = TRUE) - base::min(x, na.rm = TRUE)
  if (rango == 0) return(base::rep(0, base::length(x)))
  ((x - base::min(x, na.rm = TRUE)) / rango) * 100
}

congruencia_tucker <- function(u, v) {
  base::sum(u * v) / base::sqrt(base::sum(u^2) * base::sum(v^2))
}

calcular_omega_mcdonald <- function(cargas_cp1) {
  suma_cargas <- base::sum(cargas_cp1)^2
  unicidades  <- base::sum(1 - (cargas_cp1^2))
  suma_cargas / (suma_cargas + unicidades)
}

# ------------------------------------------------------------------------------
# 2.1 FUNCIÓN PARA ÍNDICES SIN IMPUTAR E INTERPOLADOS (USANDO base::rowSums)
# ------------------------------------------------------------------------------
ejecutar_modelo_impe_rowsums <- function(df_datos, vars_activas, dims_etiquetas, nombre_modelo) {
  
  df_datos  <- asegurar_columna_country(df_datos)
  mat_cruda <- base::as.matrix(df_datos[, vars_activas])
  mat_z     <- base::scale(mat_cruda, center = TRUE, scale = TRUE)
  
  mat_cor <- stats::cor(mat_z, use = "pairwise.complete.obs", method = "pearson")
  
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
  
  # Orientación económica consistente en las 3 Primeras Componentes (CP1, CP2, CP3):
  if (autovectores["tech_id_pib", 1]     < 0) autovectores[, 1] <- -autovectores[, 1]
  if (autovectores["prod_exp_total", 2]  < 0) autovectores[, 2] <- -autovectores[, 2]
  if (autovectores["fin_mkt_cap_pib", 3] < 0) autovectores[, 3] <- -autovectores[, 3]
  
  cargas_3cp      <- base::sweep(autovectores[, 1:3], 2, base::sqrt(autovalores[1:3]), `*`)
  comunalidad_3cp <- base::rowSums(cargas_3cp^2)
  omega_val       <- calcular_omega_mcdonald(cargas_3cp[, 1])
  
  pesos_cp1 <- (autovectores[, 1]^2) / base::sum(autovectores[, 1]^2)
  pesos_cp2 <- (autovectores[, 2]^2) / base::sum(autovectores[, 2]^2)
  pesos_cp3 <- (autovectores[, 3]^2) / base::sum(autovectores[, 3]^2)
  
  var_rel_3cp    <- autovalores[1:3] / base::sum(autovalores[1:3])
  pesos_3cp_comp <- (pesos_cp1 * var_rel_3cp[1]) +
    (pesos_cp2 * var_rel_3cp[2]) +
    (pesos_cp3 * var_rel_3cp[3])
  pesos_3cp_comp <- pesos_3cp_comp / base::sum(pesos_3cp_comp)
  
  n_filas <- base::nrow(mat_z)
  n_cols  <- base::ncol(mat_z)
  
  vars_obs_fila <- base::rowSums(!base::is.na(mat_z))
  fraccion_obs  <- vars_obs_fila / n_cols
  
  mat_w_cp1 <- base::matrix(pesos_cp1, nrow = n_filas, ncol = n_cols, byrow = TRUE)
  mat_w_3cp <- base::matrix(pesos_3cp_comp, nrow = n_filas, ncol = n_cols, byrow = TRUE)
  
  masa_activa_cp1 <- base::rowSums((!base::is.na(mat_z)) * mat_w_cp1, na.rm = TRUE)
  masa_activa_3cp <- base::rowSums((!base::is.na(mat_z)) * mat_w_3cp, na.rm = TRUE)
  
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
    Celdas_Activas_Totales  = base::sum(!base::is.na(mat_z)),
    Densidad_Datos_Pct      = base::round(base::mean(!base::is.na(mat_z)) * 100, 2),
    Alfa_Cronbach_Std       = base::round(test_alfa$total$std.alpha, 4),
    Omega_McDonald          = base::round(omega_val, 4),
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
    Lambda_Rubin_Pct        = NA_real_
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
      Comp_CP1_Bruto_Z     = base::round(comp1_raw, 4),
      Comp_CP2_Bruto_Z     = base::round(comp2_raw, 4),
      Comp_CP3_Bruto_Z     = base::round(comp3_raw, 4),
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
    autovectores_raw = autovectores[, 1:3],
    cargas_raw       = cargas_3cp
  )
}


# ------------------------------------------------------------------------------
# 2.2 FUNCIÓN PARA EJECUTAR MICE-PMM Y COMBINAR VÍA REGLAS DE RUBIN
# ------------------------------------------------------------------------------
correr_imputacion_mice_pmm <- function(df_pre_mice, m_imp = 5, max_iter = 10, semilla = 2026) {
  df_pre_mice <- asegurar_columna_country(df_pre_mice)
  
  datos_transformados <- df_pre_mice |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(vars_log_mice), ~ base::log1p(base::pmax(.x, 0))),
      fin_regalias_rec = base::asinh(fin_regalias_rec)
    ) |>
    dplyr::select(iso3c, country, year, ctrl_pib_nominal, dplyr::all_of(vars_impe_16))
  
  cols_excluir <- base::intersect(c("iso3c", "country"), base::names(datos_transformados))
  
  mat_pred <- mice::quickpred(
    datos_transformados,
    mincor  = 0.15,
    minpuc  = 0.30,
    include = c("year", "ctrl_pib_nominal", "prod_exp_total", "prod_pib_pcap_ppa"),
    exclude = cols_excluir
  )
  
  base::set.seed(semilla)
  obj_mice <- mice::mice(
    data            = datos_transformados,
    m               = m_imp,
    maxit           = max_iter,
    method          = "pmm",
    donors          = 5,
    visitSequence   = "monotone",
    predictorMatrix = mat_pred,
    ridge           = 0.001,
    printFlag       = TRUE
  )
  
  mice::complete(obj_mice, action = "all")
}

ejecutar_modelo_mice_pooled <- function(lista_m, vars_activas, dims_etiquetas, nombre_modelo,
                                        v_prod, v_fin, v_tech) {
  
  # Garantizar que todas las m bases de la lista tengan la columna 'country'
  lista_m <- base::lapply(lista_m, asegurar_columna_country)
  
  n_cols  <- base::length(vars_activas)
  n_filas <- base::nrow(lista_m[[1]])
  m_imp   <- base::length(lista_m)
  
  analizar_base_l <- function(df_l) {
    df_orig <- df_l |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(vars_log_mice), ~ base::expm1(.x)),
        fin_regalias_rec = base::sinh(fin_regalias_rec)
      )
    
    mat_z <- base::scale(base::as.matrix(df_orig[, vars_activas]), center = TRUE, scale = TRUE)
    pca_l <- stats::prcomp(mat_z, center = FALSE, scale. = FALSE)
    
    autovec <- pca_l$rotation[, 1:3]
    autoval <- pca_l$sdev^2
    
    if (autovec["tech_id_pib", 1]     < 0) autovec[, 1] <- -autovec[, 1]
    if (autovec["prod_exp_total", 2]  < 0) autovec[, 2] <- -autovec[, 2]
    if (autovec["fin_mkt_cap_pib", 3] < 0) autovec[, 3] <- -autovec[, 3]
    
    w1 <- (autovec[, 1]^2) / base::sum(autovec[, 1]^2)
    w2 <- (autovec[, 2]^2) / base::sum(autovec[, 2]^2)
    w3 <- (autovec[, 3]^2) / base::sum(autovec[, 3]^2)
    
    v_rel3 <- autoval[1:3] / base::sum(autoval[1:3])
    w_3cp  <- (w1 * v_rel3[1]) + (w2 * v_rel3[2]) + (w3 * v_rel3[3])
    w_3cp  <- w_3cp / base::sum(w_3cp)
    
    # Subíndices por dimensión teórica (Productiva, Financiera, Tecnológica)
    w_prod_norm <- w1[v_prod] / base::sum(w1[v_prod])
    w_fin_norm  <- w1[v_fin]  / base::sum(w1[v_fin])
    w_tech_norm <- w1[v_tech] / base::sum(w1[v_tech])
    
    sub_prod_z <- base::as.vector(mat_z[, v_prod] %*% w_prod_norm)
    sub_fin_z  <- base::as.vector(mat_z[, v_fin]  %*% w_fin_norm)
    sub_tech_z <- base::as.vector(mat_z[, v_tech] %*% w_tech_norm)
    
    score_cp1 <- base::as.vector(mat_z %*% w1)
    score_3cp <- base::as.vector(mat_z %*% w_3cp)
    comp1_z   <- base::as.vector(mat_z %*% autovec[, 1])
    comp2_z   <- base::as.vector(mat_z %*% autovec[, 2])
    comp3_z   <- base::as.vector(mat_z %*% autovec[, 3])
    
    mat_cor <- stats::cor(mat_z)
    cor_z   <- base::atanh(base::pmin(base::pmax(mat_cor, -0.9999), 0.9999))
    
    base::list(
      autoval    = autoval,
      autovec1   = autovec[, 1],
      autovec2   = autovec[, 2],
      autovec3   = autovec[, 3],
      w1         = w1,
      w2         = w2,
      w3         = w3,
      w_3cp      = w_3cp,
      cor_z      = cor_z,
      score_cp1  = score_cp1,
      score_3cp  = score_3cp,
      sub_prod_z = sub_prod_z,
      sub_fin_z  = sub_fin_z,
      sub_tech_z = sub_tech_z,
      comp1_z    = comp1_z,
      comp2_z    = comp2_z,
      comp3_z    = comp3_z,
      df_orig    = df_orig
    )
  }
  
  res_l <- base::lapply(lista_m, analizar_base_l)
  
  # Matriz de correlaciones combinada vía transformación z de Fisher (Van Buuren, Sec. 6.2.2)
  cor_z_media  <- base::Reduce(`+`, base::lapply(res_l, `[[`, "cor_z")) / m_imp
  mat_cor_pool <- base::tanh(cor_z_media)
  base::diag(mat_cor_pool) <- 1
  
  test_alfa     <- psych::alpha(mat_cor_pool, n.obs = n_filas, check.keys = TRUE)
  test_kmo      <- psych::KMO(mat_cor_pool)
  test_bartlett <- psych::cortest.bartlett(mat_cor_pool, n = n_filas)
  
  autoval_mat <- base::sapply(res_l, `[[`, "autoval")
  autoval_med <- base::rowMeans(autoval_mat)
  
  vec1_med    <- base::rowMeans(base::sapply(res_l, `[[`, "autovec1"))
  vec2_med    <- base::rowMeans(base::sapply(res_l, `[[`, "autovec2"))
  vec3_med    <- base::rowMeans(base::sapply(res_l, `[[`, "autovec3"))
  autovec_med <- base::cbind(vec1_med, vec2_med, vec3_med)
  
  cargas_3cp      <- base::sweep(autovec_med, 2, base::sqrt(autoval_med[1:3]), `*`)
  comunalidad_3cp <- base::rowSums(cargas_3cp^2)
  omega_val       <- calcular_omega_mcdonald(cargas_3cp[, 1])
  
  w1_med    <- base::rowMeans(base::sapply(res_l, `[[`, "w1"))
  w2_med    <- base::rowMeans(base::sapply(res_l, `[[`, "w2"))
  w3_med    <- base::rowMeans(base::sapply(res_l, `[[`, "w3"))
  w3cp_med  <- base::rowMeans(base::sapply(res_l, `[[`, "w_3cp"))
  var_rel_3 <- autoval_med[1:3] / base::sum(autoval_med[1:3])
  
  scores_cp1_mat <- base::sapply(res_l, `[[`, "score_cp1")
  scores_3cp_mat <- base::sapply(res_l, `[[`, "score_3cp")
  sub_prod_mat   <- base::sapply(res_l, `[[`, "sub_prod_z")
  sub_fin_mat    <- base::sapply(res_l, `[[`, "sub_fin_z")
  sub_tech_mat   <- base::sapply(res_l, `[[`, "sub_tech_z")
  comp1_mat      <- base::sapply(res_l, `[[`, "comp1_z")
  comp2_mat      <- base::sapply(res_l, `[[`, "comp2_z")
  comp3_mat      <- base::sapply(res_l, `[[`, "comp3_z")
  
  score_cp1_pool <- base::rowMeans(scores_cp1_mat)
  score_cp1_sd   <- base::apply(scores_cp1_mat, 1, stats::sd)
  score_3cp_pool <- base::rowMeans(scores_3cp_mat)
  
  sub_prod_100 <- reescalar_0_100(base::rowMeans(sub_prod_mat))
  sub_fin_100  <- reescalar_0_100(base::rowMeans(sub_fin_mat))
  sub_tech_100 <- reescalar_0_100(base::rowMeans(sub_tech_mat))
  
  # Índice Equi-Dimensional (1/3 por dimensión) e Índice Geométrico No Compensatorio (OCDE, 2008)
  share_prod <- base::sum(w1_med[v_prod])
  share_fin  <- base::sum(w1_med[v_fin])
  share_tech <- base::sum(w1_med[v_tech])
  
  impe_equi_100 <- reescalar_0_100((sub_prod_100 + sub_fin_100 + sub_tech_100) / 3)
  geom_raw      <- ((sub_tech_100 + 1)^share_tech) * ((sub_prod_100 + 1)^share_prod) * ((sub_fin_100 + 1)^share_fin) - 1
  impe_geom_100 <- reescalar_0_100(geom_raw)
  
  # Proporción de varianza atribuible a datos faltantes (Lambda de Rubin, Van Buuren Sec. 2.3.5)
  B_vec     <- score_cp1_sd^2
  var_total <- stats::var(score_cp1_pool)
  lambda_ef <- base::mean(((1 + 1 / m_imp) * B_vec) / var_total)
  
  tabla_resumen <- tibble::tibble(
    Modelo                  = nombre_modelo,
    Num_Paises              = dplyr::n_distinct(res_l[[1]]$df_orig$iso3c),
    Num_Variables           = n_cols,
    Observaciones_Retenidas = n_filas,
    Celdas_Activas_Totales  = n_filas * n_cols,
    Densidad_Datos_Pct      = 100.00,
    Alfa_Cronbach_Std       = base::round(test_alfa$total$std.alpha, 4),
    Omega_McDonald          = base::round(omega_val, 4),
    Cor_InterItem_Media     = base::round(test_alfa$total$average_r, 4),
    KMO_Global_MSA          = base::round(test_kmo$MSA, 4),
    KMO_Minimo_MSA          = base::round(base::min(test_kmo$MSAi), 4),
    Bartlett_Chi2           = base::round(test_bartlett$chisq, 2),
    Autovalor_CP1           = base::round(autoval_med[1], 4),
    Varianza_CP1_Pct        = base::round((autoval_med[1] / n_cols) * 100, 2),
    Autovalor_CP2           = base::round(autoval_med[2], 4),
    Varianza_CP2_Pct        = base::round((autoval_med[2] / n_cols) * 100, 2),
    Autovalor_CP3           = base::round(autoval_med[3], 4),
    Varianza_CP3_Pct        = base::round((autoval_med[3] / n_cols) * 100, 2),
    Varianza_Acum_3CP_Pct   = base::round((base::sum(autoval_med[1:3]) / n_cols) * 100, 2),
    Lambda_Rubin_Pct        = base::round(lambda_ef * 100, 2)
  )
  
  tabla_autovalores <- tibble::tibble(
    Modelo             = nombre_modelo,
    Componente         = base::paste0("CP", base::seq_len(n_cols)),
    Autovalor          = base::round(autoval_med, 4),
    Autovalor_SD_Imp   = base::round(base::apply(autoval_mat, 1, stats::sd), 4),
    Varianza_Pct       = base::round((autoval_med / n_cols) * 100, 2),
    Varianza_Acum_Pct  = base::round(base::cumsum(autoval_med / n_cols) * 100, 2),
    Criterio_Kaiser    = base::ifelse(autoval_med > 1, "Sí (> 1.0)", "No (< 1.0)"),
    Peso_Entre_3CP_Pct = base::c(base::round(var_rel_3 * 100, 2), base::rep(0, n_cols - 3))
  )
  
  tabla_pca_3cp <- tibble::tibble(
    Modelo              = nombre_modelo,
    Variable            = vars_activas,
    Dimension_Teorica   = dims_etiquetas,
    Autovector_CP1      = base::round(vec1_med, 4),
    Autovector_CP2      = base::round(vec2_med, 4),
    Autovector_CP3      = base::round(vec3_med, 4),
    Carga_CP1           = base::round(cargas_3cp[, 1], 4),
    Carga_CP2           = base::round(cargas_3cp[, 2], 4),
    Carga_CP3           = base::round(cargas_3cp[, 3], 4),
    Comunalidad_3CP_h2  = base::round(comunalidad_3cp, 4),
    Peso_CP1_Pct        = base::round(w1_med * 100, 2),
    Error_Peso_CP1_SD   = base::round(base::apply(base::sapply(res_l, `[[`, "w1") * 100, 1, stats::sd), 3),
    Peso_CP2_Pct        = base::round(w2_med * 100, 2),
    Peso_CP3_Pct        = base::round(w3_med * 100, 2),
    Peso_3CP_Comp_Pct   = base::round(w3cp_med * 100, 2),
    KMO_Individual_MSA  = base::round(test_kmo$MSAi, 4),
    Cor_Item_Total_Drop = base::round(test_alfa$item.stats[, "r.drop"], 4),
    Alfa_Si_Se_Elimina  = base::round(test_alfa$alpha.drop[, "std.alpha"], 4)
  )
  
  df_base_scores <- asegurar_columna_country(res_l[[1]]$df_orig)
  
  panel_scores <- df_base_scores |>
    dplyr::select(iso3c, country, year, ctrl_pib_nominal) |>
    dplyr::mutate(
      IMPE_Bruto_Pooled     = base::round(score_cp1_pool, 4),
      IMPE_Incertidumbre_SD = base::round(score_cp1_sd, 4),
      Comp_CP1_Bruto_Z      = base::round(base::rowMeans(comp1_mat), 4),
      Comp_CP2_Bruto_Z      = base::round(base::rowMeans(comp2_mat), 4),
      Comp_CP3_Bruto_Z      = base::round(base::rowMeans(comp3_mat), 4),
      Comp_CP1_100          = base::round(reescalar_0_100(base::rowMeans(comp1_mat)), 2),
      Comp_CP2_100          = base::round(reescalar_0_100(base::rowMeans(comp2_mat)), 2),
      Comp_CP3_100          = base::round(reescalar_0_100(base::rowMeans(comp3_mat)), 2),
      Sub_Tecnologico_100   = base::round(sub_tech_100, 2),
      Sub_Productivo_100    = base::round(sub_prod_100, 2),
      Sub_Financiero_100    = base::round(sub_fin_100, 2),
      IMPE_100              = base::round(reescalar_0_100(score_cp1_pool), 2),
      IMPE_3CP_100          = base::round(reescalar_0_100(score_3cp_pool), 2),
      IMPE_EquiDim_100      = base::round(impe_equi_100, 2),
      IMPE_Geometrico_100   = base::round(impe_geom_100, 2)
    ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      Rank_IMPE_100  = dplyr::min_rank(dplyr::desc(IMPE_100)),
      Rank_IMPE_3CP  = dplyr::min_rank(dplyr::desc(IMPE_3CP_100)),
      Rank_IMPE_Geom = dplyr::min_rank(dplyr::desc(IMPE_Geometrico_100))
    ) |>
    dplyr::ungroup()
  
  # Diagnóstico VIF y Tolerancia entre las 3 Dimensiones
  cor_dim_mat <- stats::cor(panel_scores[, c("IMPE_100", "Sub_Tecnologico_100", "Sub_Productivo_100", "Sub_Financiero_100")])
  sub_cor_3x3 <- cor_dim_mat[2:4, 2:4]
  vif_vals    <- base::diag(base::solve(sub_cor_3x3))
  tol_vals    <- 1 / vif_vals
  
  tabla_vif <- tibble::tibble(
    Modelo             = nombre_modelo,
    Subindice          = base::colnames(sub_cor_3x3),
    Cor_con_IMPE       = base::round(cor_dim_mat[1, 2:4], 4),
    Varianza_Comun_Pct = base::round((cor_dim_mat[1, 2:4]^2) * 100, 2),
    Tolerancia_Unica   = base::round(tol_vals, 4),
    VIF_InterDim       = base::round(vif_vals, 4),
    Diagnostico_VIF    = base::ifelse(vif_vals < 5, "Óptimo (VIF < 5.0)", "Revisar Colinealidad")
  )
  
  base::list(
    resumen          = tabla_resumen,
    autovalores      = tabla_autovalores,
    pca_3cp          = tabla_pca_3cp,
    panel_scores     = panel_scores,
    tabla_vif        = tabla_vif,
    autovectores_raw = autovec_med,
    cargas_raw       = cargas_3cp,
    lambda_rubin_pct = base::round(lambda_ef * 100, 2)
  )
}


# ==============================================================================
# ETAPA 3: EJECUCIÓN DE LOS 8 MODELOS Y PRUEBAS DE ROBUSTEZ / CONGRUENCIA
# ==============================================================================

# 3.1 Modelos 1A y 1B: Sin Imputar (Observado + rowSums en Base 16 y Base 12)
mod_obs_16 <- ejecutar_modelo_impe_rowsums(panel_observado,   vars_impe_16, dims_16, "1A_Sin_Imputar_Base16")
mod_obs_12 <- ejecutar_modelo_impe_rowsums(panel_observado,   vars_impe_12, dims_12, "1B_Sin_Imputar_Base12")

# 3.2 Modelos 2A y 2B: Exclusivamente Interpolado (zoo::na.approx + rowSums en Base 16 y Base 12)
mod_int_16 <- ejecutar_modelo_impe_rowsums(panel_interpolado, vars_impe_16, dims_16, "2A_Interpolado_Base16")
mod_int_12 <- ejecutar_modelo_impe_rowsums(panel_interpolado, vars_impe_12, dims_12, "2B_Interpolado_Base12")

# 3.3 Modelos 3A y 3B: MICE-PMM Panel Global (182 Países Soberanos, Base 16 y Base 12)
if (base::exists("lista_imputaciones")) {
  lista_imp_global <- base::lapply(lista_imputaciones, asegurar_columna_country)
} else {
  base::cat("
--- EJECUTANDO MICE-PMM EN PANEL GLOBAL (182 PAÍSES) ---
")
  lista_imp_global <- correr_imputacion_mice_pmm(panel_global_pre_mice, m_imp = 5, max_iter = 10, semilla = 2026)
}

mod_mice_16 <- ejecutar_modelo_mice_pooled(
  lista_imp_global, vars_impe_16, dims_16, "3A_MICE_Global_Base16",
  vars_productiva_16, vars_financiera_16, vars_tecnologica_16
)
mod_mice_12 <- ejecutar_modelo_mice_pooled(
  lista_imp_global, vars_impe_12, dims_12, "3B_MICE_Global_Base12",
  vars_productiva_12, vars_financiera_12, vars_tecnologica_12
)

# 3.4 Modelos 4A y 4B: IMPE Países Depurados <= 30% NAs (Interpolado + MICE-PMM en Base 16 y Base 12)
base::cat("
--- EJECUTANDO MICE-PMM EN PAÍSES DEPURADOS (<= 30% NAs) ---
")
lista_imp_depurados <- correr_imputacion_mice_pmm(panel_depurados_pre_mice, m_imp = 5, max_iter = 10, semilla = 2026)

mod_dep_16 <- ejecutar_modelo_mice_pooled(
  lista_imp_depurados, vars_impe_16, dims_16, "4A_Paises_Depurados_Base16",
  vars_productiva_16, vars_financiera_16, vars_tecnologica_16
)
mod_dep_12 <- ejecutar_modelo_mice_pooled(
  lista_imp_depurados, vars_impe_12, dims_12, "4B_Paises_Depurados_Base12",
  vars_productiva_12, vars_financiera_12, vars_tecnologica_12
)

# 3.5 Tabla Resumen Maestra de los 8 Modelos
tabla_resumen_8_modelos <- dplyr::bind_rows(
  mod_obs_16$resumen,
  mod_int_16$resumen,
  mod_mice_16$resumen,
  mod_dep_16$resumen,
  mod_obs_12$resumen,
  mod_int_12$resumen,
  mod_mice_12$resumen,
  mod_dep_12$resumen
)

base::cat("
================ RESUMEN EJECUTIVO DE LOS 8 ÍNDICES CONSTRUIDOS ================
")
base::print(tabla_resumen_8_modelos)

# 3.6 Congruencia Factorial de Tucker en las 3 Primeras Componentes (CP1, CP2, CP3)
tabla_congruencia_3cp <- tibble::tibble(
  Componente_Principal      = c("CP1 (Poder Económico General)",
                                "CP2 (Escala Masiva vs. Intensidad Per Cápita)",
                                "CP3 (Hegemonía Financiera vs. Manufactura)"),
  Tucker_Obs16_vs_Int16     = c(
    base::round(congruencia_tucker(mod_obs_16$autovectores_raw[, 1], mod_int_16$autovectores_raw[, 1]), 4),
    base::round(congruencia_tucker(mod_obs_16$autovectores_raw[, 2], mod_int_16$autovectores_raw[, 2]), 4),
    base::round(congruencia_tucker(mod_obs_16$autovectores_raw[, 3], mod_int_16$autovectores_raw[, 3]), 4)
  ),
  Tucker_MICE16_vs_Dep16    = c(
    base::round(congruencia_tucker(mod_mice_16$autovectores_raw[, 1], mod_dep_16$autovectores_raw[, 1]), 4),
    base::round(congruencia_tucker(mod_mice_16$autovectores_raw[, 2], mod_dep_16$autovectores_raw[, 2]), 4),
    base::round(congruencia_tucker(mod_mice_16$autovectores_raw[, 3], mod_dep_16$autovectores_raw[, 3]), 4)
  ),
  Tucker_MICE12_vs_Dep12    = c(
    base::round(congruencia_tucker(mod_mice_12$autovectores_raw[, 1], mod_dep_12$autovectores_raw[, 1]), 4),
    base::round(congruencia_tucker(mod_mice_12$autovectores_raw[, 2], mod_dep_12$autovectores_raw[, 2]), 4),
    base::round(congruencia_tucker(mod_mice_12$autovectores_raw[, 3], mod_dep_12$autovectores_raw[, 3]), 4)
  ),
  Criterio_Lorenzo_Seva     = "Phi >= 0.90 (Similar) | Phi >= 0.95 (Equivalencia Estricta)"
)

# 3.7 Panel Consolidado Maestro (Uniendo los 8 Índices Lado a Lado para los 182 Países, N = 5,460)
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
      dplyr::select(
        iso3c, year,
        IMPE_Obs12_Smart = IMPE_rowSums_Smart,
        Rank_Obs12_Smart = Rank_rowSums_Smart
      ),
    by = c("iso3c", "year")
  ) |>
  dplyr::left_join(
    mod_int_12$panel_scores |>
      dplyr::select(
        iso3c, year,
        IMPE_Int12_Smart = IMPE_rowSums_Smart,
        Rank_Int12_Smart = Rank_rowSums_Smart
      ),
    by = c("iso3c", "year")
  ) |>
  dplyr::left_join(
    mod_mice_16$panel_scores |>
      dplyr::select(
        iso3c, year,
        IMPE_MICE_Base16       = IMPE_100,
        IMPE_MICE_Base16_SD    = IMPE_Incertidumbre_SD,
        IMPE_MICE_Base16_Geom  = IMPE_Geometrico_100,
        Sub_Tecnologico_Base16 = Sub_Tecnologico_100,
        Sub_Productivo_Base16  = Sub_Productivo_100,
        Sub_Financiero_Base16  = Sub_Financiero_100,
        Rank_MICE_Base16       = Rank_IMPE_100
      ),
    by = c("iso3c", "year")
  ) |>
  dplyr::left_join(
    mod_mice_12$panel_scores |>
      dplyr::select(
        iso3c, year,
        IMPE_MICE_Base12       = IMPE_100,
        IMPE_MICE_Base12_SD    = IMPE_Incertidumbre_SD,
        IMPE_MICE_Base12_Geom  = IMPE_Geometrico_100,
        Sub_Tecnologico_Base12 = Sub_Tecnologico_100,
        Sub_Productivo_Base12  = Sub_Productivo_100,
        Sub_Financiero_Base12  = Sub_Financiero_100,
        Rank_MICE_Base12       = Rank_IMPE_100
      ),
    by = c("iso3c", "year")
  ) |>
  dplyr::left_join(
    mod_dep_16$panel_scores |>
      dplyr::select(
        iso3c, year,
        IMPE_Depurados_Base16    = IMPE_100,
        IMPE_Depurados_Base16_SD = IMPE_Incertidumbre_SD,
        Rank_Depurados_Base16    = Rank_IMPE_100
      ),
    by = c("iso3c", "year")
  ) |>
  dplyr::left_join(
    mod_dep_12$panel_scores |>
      dplyr::select(
        iso3c, year,
        IMPE_Depurados_Base12    = IMPE_100,
        IMPE_Depurados_Base12_SD = IMPE_Incertidumbre_SD,
        Rank_Depurados_Base12    = Rank_IMPE_100
      ),
    by = c("iso3c", "year")
  )

# Panel exclusivo de Países Depurados (<= 30% NAs, uniendo Base 16 y Base 12)
panel_paises_depurados_unido <- mod_dep_16$panel_scores |>
  dplyr::rename(
    IMPE_Dep16_Bruto_Z  = IMPE_Bruto_Pooled,
    IMPE_Dep16_SD_Rubin = IMPE_Incertidumbre_SD,
    IMPE_Dep16_100      = IMPE_100,
    IMPE_Dep16_3CP_100  = IMPE_3CP_100,
    IMPE_Dep16_Geom_100 = IMPE_Geometrico_100,
    Rank_Dep16          = Rank_IMPE_100
  ) |>
  dplyr::left_join(
    mod_dep_12$panel_scores |>
      dplyr::select(
        iso3c, year,
        IMPE_Dep12_Bruto_Z  = IMPE_Bruto_Pooled,
        IMPE_Dep12_SD_Rubin = IMPE_Incertidumbre_SD,
        IMPE_Dep12_100      = IMPE_100,
        IMPE_Dep12_3CP_100  = IMPE_3CP_100,
        IMPE_Dep12_Geom_100 = IMPE_Geometrico_100,
        Rank_Dep12          = Rank_IMPE_100
      ),
    by = c("iso3c", "year")
  )

cols_indices_corr <- panel_comparado_maestro |>
  dplyr::select(dplyr::starts_with("IMPE_")) |>
  dplyr::select(-dplyr::ends_with("_SD"))

matriz_pearson_indices  <- base::round(stats::cor(cols_indices_corr, use = "pairwise.complete.obs", method = "pearson"), 4) |>
  tibble::as_tibble(rownames = "Indice")
matriz_spearman_indices <- base::round(stats::cor(cols_indices_corr, use = "pairwise.complete.obs", method = "spearman"), 4) |>
  tibble::as_tibble(rownames = "Indice")

ranking_comparado_2024 <- panel_comparado_maestro |>
  dplyr::filter(year == base::max(year, na.rm = TRUE)) |>
  dplyr::arrange(Rank_MICE_Base12)


# ==============================================================================
# ETAPA 4: EXPORTACIÓN AUTOMÁTICA A UN SOLO LIBRO DE EXCEL MAESTRO (openxlsx)
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

# Hoja 1: Cuadro de Mando de los 8 Índices, VIF, Tucker, Pearson y Spearman
crear_hoja_profesional(
  wb, "1_Dashboard_8_Indices",
  "CUADRO DE MANDO INTEGRAL: COMPARACIÓN DE LOS 8 MODELOS DEL ÍNDICE I.M.P.E. (1995-2024)",
  "Pruebas de Alfa de Cronbach, Omega de McDonald, KMO, Bartlett, las 3 Primeras Componentes (CP1, CP2, CP3), VIF y Correlaciones",
  base::list(
    base::list(encabezado_seccion = "1. TABLA COMPARATIVA GLOBAL DE LOS 8 ÍNDICES CONSTRUIDOS (BASE 16 Y BASE 12)", datos = tabla_resumen_8_modelos, color_verde = FALSE),
    base::list(encabezado_seccion = "2. EQUIVALENCIA FACTORIAL DE TUCKER EN LAS 3 PRIMERAS COMPONENTES (CP1, CP2, CP3)", datos = tabla_congruencia_3cp, color_verde = TRUE),
    base::list(encabezado_seccion = "3. DIAGNÓSTICO DE NO-COLINEALIDAD ENTRE SUBÍNDICES (TOLERANCIA Y VIF < 5.0)", datos = dplyr::bind_rows(mod_mice_16$tabla_vif, mod_mice_12$tabla_vif, mod_dep_12$tabla_vif), color_verde = FALSE),
    base::list(encabezado_seccion = "4. MATRIZ DE CORRELACIÓN LINEAL DE PEARSON ENTRE TODOS LOS ÍNDICES", datos = matriz_pearson_indices, color_verde = TRUE),
    base::list(encabezado_seccion = "5. MATRIZ DE CORRELACIÓN DE RANGOS DE SPEARMAN ENTRE TODOS LOS ÍNDICES", datos = matriz_spearman_indices, color_verde = FALSE)
  )
)

# Hoja 2: Diagnóstico de Datos Faltantes Influx y Outflux (Van Buuren, 2012)
crear_hoja_profesional(
  wb, "2_Diagnostico_Flux_NAs",
  "DIAGNÓSTICO DE DATOS FALTANTES E IMPUTACIÓN MÚLTIPLE (STEF VAN BUUREN, 2012)",
  "Métricas de Conectividad Multivariante (Influx I_j, Outflux O_j y Proporción Observada p_obs en el Panel 1995-2024)",
  base::list(
    base::list(encabezado_seccion = "1. TABLA DE CONECTIVIDAD INFLUX Y OUTFLUX (16 VARIABLES DEL IMPE + VARIABLE DE CONTROL)", datos = flux_global, color_verde = FALSE)
  )
)

# Hoja 3: ACP de las 3 Componentes — Índice Sin Imputar (Base 16 y Base 12)
crear_hoja_profesional(
  wb, "3_PCA_3CP_Sin_Imputar",
  "ANÁLISIS DE LAS 3 COMPONENTES PRINCIPALES Y ALFA — ÍNDICE 1: SIN IMPUTAR (BASE 16 Y BASE 12)",
  "Autovectores, Cargas Factoriales, Comunalidades, Pesos en CP1, CP2, CP3 y Auditoría de Cronbach (Datos Observados)",
  base::list(
    base::list(encabezado_seccion = "1. ACP DE LAS 3 COMPONENTES — SIN IMPUTAR BASE 16", datos = mod_obs_16$pca_3cp, color_verde = FALSE),
    base::list(encabezado_seccion = "2. ACP DE LAS 3 COMPONENTES — SIN IMPUTAR BASE 12", datos = mod_obs_12$pca_3cp, color_verde = TRUE),
    base::list(encabezado_seccion = "3. AUTOVALORES COMPLETOS — SIN IMPUTAR (BASE 16 Y BASE 12)", datos = dplyr::bind_rows(mod_obs_16$autovalores, mod_obs_12$autovalores), color_verde = FALSE)
  )
)

# Hoja 4: ACP de las 3 Componentes — Índice Interpolado (Base 16 y Base 12)
crear_hoja_profesional(
  wb, "4_PCA_3CP_Interpolado",
  "ANÁLISIS DE LAS 3 COMPONENTES PRINCIPALES Y ALFA — ÍNDICE 2: INTERPOLADO (BASE 16 Y BASE 12)",
  "Autovectores, Cargas Factoriales, Comunalidades, Pesos en CP1, CP2, CP3 y Auditoría de Cronbach (Datos Interpolados)",
  base::list(
    base::list(encabezado_seccion = "1. ACP DE LAS 3 COMPONENTES — INTERPOLADO BASE 16", datos = mod_int_16$pca_3cp, color_verde = TRUE),
    base::list(encabezado_seccion = "2. ACP DE LAS 3 COMPONENTES — INTERPOLADO BASE 12", datos = mod_int_12$pca_3cp, color_verde = FALSE),
    base::list(encabezado_seccion = "3. AUTOVALORES COMPLETOS — INTERPOLADO (BASE 16 Y BASE 12)", datos = dplyr::bind_rows(mod_int_16$autovalores, mod_int_12$autovalores), color_verde = TRUE)
  )
)

# Hoja 5: ACP de las 3 Componentes — IMPE MICE-PMM Global (182 Países, Base 16 y Base 12)
crear_hoja_profesional(
  wb, "5_PCA_3CP_MICE_Global",
  "ANÁLISIS DE LAS 3 COMPONENTES PRINCIPALES Y ALFA — ÍNDICE 3: MICE-PMM GLOBAL (182 PAÍSES, BASE 16 Y BASE 12)",
  "Combinación vía Reglas de Rubin en los 182 Países Soberanos (Base 16 Completa y Base 12 Núcleo Puro)",
  base::list(
    base::list(encabezado_seccion = "1. ACP DE LAS 3 COMPONENTES Y ALFA — MICE GLOBAL BASE 16", datos = mod_mice_16$pca_3cp, color_verde = FALSE),
    base::list(encabezado_seccion = "2. ACP DE LAS 3 COMPONENTES Y ALFA — MICE GLOBAL BASE 12 (NÚCLEO ÓPTIMO)", datos = mod_mice_12$pca_3cp, color_verde = TRUE),
    base::list(encabezado_seccion = "3. AUTOVALORES COMBINADOS — MICE GLOBAL (BASE 16 Y BASE 12)", datos = dplyr::bind_rows(mod_mice_16$autovalores, mod_mice_12$autovalores), color_verde = FALSE)
  )
)

# Hoja 6: ACP de las 3 Componentes — IMPE Países Depurados <= 30% NAs (Base 16 y Base 12 con MICE-PMM)
crear_hoja_profesional(
  wb, "6_PCA_3CP_Paises_Depurados",
  "ANÁLISIS DE LAS 3 COMPONENTES PRINCIPALES Y ALFA — ÍNDICE 4: PAÍSES DEPURADOS (<= 30% NAs + MICE-PMM)",
  "Combinación vía Reglas de Rubin para Países con <= 30% de Datos Faltantes (Interpolados e Imputados con PMM en Base 16 y Base 12)",
  base::list(
    base::list(encabezado_seccion = "1. ACP DE LAS 3 COMPONENTES Y ALFA — PAÍSES DEPURADOS BASE 16 (MICE-PMM)", datos = mod_dep_16$pca_3cp, color_verde = FALSE),
    base::list(encabezado_seccion = "2. ACP DE LAS 3 COMPONENTES Y ALFA — PAÍSES DEPURADOS BASE 12 (MICE-PMM)", datos = mod_dep_12$pca_3cp, color_verde = TRUE),
    base::list(encabezado_seccion = "3. AUTOVALORES COMBINADOS — PAÍSES DEPURADOS (BASE 16 Y BASE 12)", datos = dplyr::bind_rows(mod_dep_16$autovalores, mod_dep_12$autovalores), color_verde = FALSE)
  )
)

# Hoja 7: Ranking Mundial Comparado en 2024
crear_hoja_profesional(
  wb, "7_Ranking_Comparado_2024",
  "RANKING MUNDIAL COMPARADO EN 2024 (182 PAÍSES SOBERANOS)",
  "Contraste de Posiciones y Puntajes: Sin Imputar, Interpolado, MICE Global y Países Depurados (Base 16 y Base 12)",
  base::list(
    base::list(encabezado_seccion = "1. TABLA COMPARATIVA DE LOS 182 PAÍSES EN 2024 (ORDENADA POR MICE BASE 12)", datos = ranking_comparado_2024, color_verde = FALSE)
  )
)

# Hoja 8: Resultados del Índice Sin Imputar y del Índice Interpolado
crear_hoja_profesional(
  wb, "8_Resultado_SinImp_e_Interp",
  "RESULTADOS DEL PANEL (1995-2024): ÍNDICE SIN IMPUTAR E ÍNDICE INTERPOLADO (CON rowSums)",
  "Panel de 5,460 Observaciones con Puntajes en CP1, CP2, CP3 (en Z con Signo y de 0 a 100), rowSums Simple e Inteligente",
  base::list(
    base::list(encabezado_seccion = "1. PANEL DE RESULTADOS — ÍNDICE EXCLUSIVAMENTE INTERPOLADO (N = 5,460)", datos = mod_int_16$panel_scores, color_verde = TRUE)
  )
)

# Hoja 9: Resultados Exclusivos del IMPE Países Depurados (<= 30% NAs, Base 16 y Base 12 con MICE-PMM)
crear_hoja_profesional(
  wb, "9_Resultado_Paises_Depurados",
  "RESULTADOS DEL ÍNDICE 4: IMPE PAÍSES DEPURADOS (<= 30% NAs, INTERPOLADO + MICE-PMM, BASE 16 Y BASE 12)",
  "Panel de Países con <= 30% de Datos Faltantes Imputados vía MICE-PMM con Puntajes Base 16, Base 12, Geométrico e Incertidumbre de Rubin",
  base::list(
    base::list(encabezado_seccion = "1. PANEL DE RESULTADOS — IMPE PAÍSES DEPURADOS (BASE 16 Y BASE 12)", datos = panel_paises_depurados_unido, color_verde = FALSE)
  )
)

# Hoja 10: Panel Maestro Consolidado con Todos los Índices Unidos Lado a Lado
crear_hoja_profesional(
  wb, "10_Panel_Maestro_Todos",
  "PANEL MAESTRO CONSOLIDADO: TODOS LOS ÍNDICES UNIDOS LADO A LADO (1995-2024, N = 5,460)",
  "Unifica Sin Imputar (16 y 12), Interpolado (16 y 12), MICE Global (16, 12, Geométrico, Subíndices) e IMPE Países Depurados <=30% NAs (16 y 12)",
  base::list(
    base::list(encabezado_seccion = "1. MATRIZ MAESTRA CONSOLIDADA PARA MODELADO ECONOMÉTRICO (N = 5,460)", datos = panel_comparado_maestro, color_verde = TRUE)
  )
)

nombre_archivo_excel <- "IMPE_Consolidado_Maestro_Tesis_Completo.xlsx"
openxlsx::saveWorkbook(wb, file = nombre_archivo_excel, overwrite = TRUE)


# ==============================================================================
# ETAPA 5: GENERACIÓN AUTOMÁTICA DE LOS GRÁFICOS DE ALTA RESOLUCIÓN (ggplot2)
# ==============================================================================

tabla_3cp_unificada <- dplyr::bind_rows(
  mod_obs_16$pca_3cp  |> dplyr::mutate(Indice = "1A. Sin Imputar (Base 16)"),
  mod_int_16$pca_3cp  |> dplyr::mutate(Indice = "2A. Interpolado (Base 16)"),
  mod_mice_16$pca_3cp |> dplyr::mutate(Indice = "3A. MICE Global (Base 16)"),
  mod_dep_16$pca_3cp  |> dplyr::mutate(Indice = "4A. Países Depurados <=30% NA (Base 16)"),
  mod_obs_12$pca_3cp  |> dplyr::mutate(Indice = "1B. Sin Imputar (Base 12)"),
  mod_int_12$pca_3cp  |> dplyr::mutate(Indice = "2B. Interpolado (Base 12)"),
  mod_mice_12$pca_3cp |> dplyr::mutate(Indice = "3B. MICE Global (Base 12)"),
  mod_dep_12$pca_3cp  |> dplyr::mutate(Indice = "4B. Países Depurados <=30% NA (Base 12)")
)

orden_variables <- tabla_3cp_unificada |>
  dplyr::filter(Indice == "3A. MICE Global (Base 16)") |>
  dplyr::arrange(Carga_CP1) |>
  dplyr::pull(Variable)

datos_cargas_largo <- tabla_3cp_unificada |>
  dplyr::select(Indice, Variable, Dimension_Teorica, Carga_CP1, Carga_CP2, Carga_CP3) |>
  tidyr::pivot_longer(
    cols      = c(Carga_CP1, Carga_CP2, Carga_CP3),
    names_to  = "Componente",
    values_to = "Carga_Factorial"
  ) |>
  dplyr::mutate(
    Variable = base::factor(Variable, levels = orden_variables),
    Indice   = base::factor(Indice, levels = base::unique(tabla_3cp_unificada$Indice)),
    Componente_Etiqueta = dplyr::case_when(
      Componente == "Carga_CP1" ~ "CP1: Poder Económico General
(Factor Hegemónico Productivo-Tecnológico-Financiero)",
      Componente == "Carga_CP2" ~ "CP2: Escala Masiva (+) vs. Intensidad Per Cápita (-)
(Superpotencias Continentales vs. Economías Compactas)",
      Componente == "Carga_CP3" ~ "CP3: Hegemonía Financiera (+) vs. Manufactura (-)
(Plazas Bursátiles/Rentísticas vs. Potencias Fabriles)"
    ),
    Componente_Etiqueta = base::factor(
      Componente_Etiqueta,
      levels = c(
        "CP1: Poder Económico General
(Factor Hegemónico Productivo-Tecnológico-Financiero)",
        "CP2: Escala Masiva (+) vs. Intensidad Per Cápita (-)
(Superpotencias Continentales vs. Economías Compactas)",
        "CP3: Hegemonía Financiera (+) vs. Manufactura (-)
(Plazas Bursátiles/Rentísticas vs. Potencias Fabriles)"
      )
    )
  )

paleta_8_indices <- c(
  "1A. Sin Imputar (Base 16)"               = "#DC2626",
  "2A. Interpolado (Base 16)"               = "#2563EB",
  "3A. MICE Global (Base 16)"               = "#1E3A8A",
  "4A. Países Depurados <=30% NA (Base 16)" = "#7C3AED",
  "1B. Sin Imputar (Base 12)"               = "#F59E0B",
  "2B. Interpolado (Base 12)"               = "#10B981",
  "3B. MICE Global (Base 12)"               = "#047857",
  "4B. Países Depurados <=30% NA (Base 12)" = "#0D9488"
)

# 5.1 Gráfico 1: Barras Agrupadas de las 3 Componentes Principales (CP1, CP2, CP3) por Variable e Índice
grafico_1_barras_3cp <- ggplot2::ggplot(
  data = datos_cargas_largo,
  mapping = ggplot2::aes(x = Carga_Factorial, y = Variable, fill = Indice)
) +
  ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 4.5, fill = "#FEE2E2", alpha = 0.45) +
  ggplot2::geom_vline(xintercept = 0, color = "#0F172A", linewidth = 0.8) +
  ggplot2::geom_vline(xintercept = c(-0.5, 0.5), color = "#64748B", linetype = "dashed", linewidth = 0.5) +
  ggplot2::geom_col(
    position  = ggplot2::position_dodge(width = 0.84),
    width     = 0.78,
    color     = "#0F172A",
    linewidth = 0.2,
    alpha     = 0.92
  ) +
  ggplot2::facet_wrap(~ Componente_Etiqueta, ncol = 3) +
  ggplot2::scale_fill_manual(values = paleta_8_indices, name = "Índice Comparado:") +
  ggplot2::scale_x_continuous(limits = c(-0.90, 1.00), breaks = base::seq(-0.75, 0.75, by = 0.25)) +
  ggplot2::labs(
    title    = "Comparación de las 3 Primeras Componentes Principales (CP1, CP2, CP3) por Variable y por Índice",
    subtitle = "Cargas Factoriales (r) en Sin Imputar, Interpolado, MICE Global y Países Depurados <=30% NA (Base 16 y Base 12)",
    x        = "Carga Factorial (Correlación Lineal r con la Componente Principal)",
    y        = "Variables del Índice (Ordenadas por Jerarquía en CP1)",
    caption  = "Elaboración propia."
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 13.5, color = "#0F172A"),
    plot.subtitle    = ggplot2::element_text(size = 10, color = "#334155", margin = ggplot2::margin(b = 10)),
    strip.background = ggplot2::element_rect(fill = "#1E3A8A", color = "#0F172A"),
    strip.text       = ggplot2::element_text(face = "bold", color = "#FFFFFF", size = 9.2),
    axis.text.y      = ggplot2::element_text(face = "bold", color = "#0F172A", size = 9.5),
    legend.position  = "bottom",
    panel.grid.minor = ggplot2::element_blank()
  )

base::print(grafico_1_barras_3cp)
ggplot2::ggsave(
  filename = "Grafico_1_Cargas_3CP_8_Indices.png",
  plot     = grafico_1_barras_3cp,
  width    = 18.0, height = 9.5, dpi = 300, bg = "white"
)

# 5.2 Gráfico 2: Mapa de Calor (Heatmap) de las 3 Componentes a través de los 8 Índices
datos_heatmap <- datos_cargas_largo |>
  dplyr::mutate(
    Comp_Corto = dplyr::case_when(
      Componente == "Carga_CP1" ~ "CP1 (Poder General)",
      Componente == "Carga_CP2" ~ "CP2 (Escala vs. Int.)",
      Componente == "Carga_CP3" ~ "CP3 (Finanzas vs. Manuf.)"
    ),
    Comp_Corto = base::factor(
      Comp_Corto,
      levels = c("CP1 (Poder General)", "CP2 (Escala vs. Int.)", "CP3 (Finanzas vs. Manuf.)")
    )
  )

grafico_2_heatmap_3cp <- ggplot2::ggplot(
  data = datos_heatmap,
  mapping = ggplot2::aes(x = Indice, y = Variable, fill = Carga_Factorial)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.6) +
  ggplot2::geom_text(
    mapping  = ggplot2::aes(label = base::sprintf("%+.2f", Carga_Factorial)),
    size     = 2.8,
    fontface = "bold",
    color    = base::ifelse(base::abs(datos_heatmap$Carga_Factorial) > 0.45, "white", "#0F172A")
  ) +
  ggplot2::facet_wrap(~ Comp_Corto, ncol = 3) +
  ggplot2::scale_fill_gradient2(
    low = "#1D4ED8", mid = "#F8FAFC", high = "#B91C1C",
    midpoint = 0, limits = c(-0.85, 0.85), name = "Carga Factorial (r):"
  ) +
  ggplot2::labs(
    title    = "Mapa de Calor: Estabilidad de las 3 Primeras Componentes (CP1, CP2, CP3) en los 8 Índices",
    subtitle = "Contrasta Sin Imputar, Interpolado, MICE Global y Países Depurados <=30% NA (Base 16 vs. Base 12)",
    x        = "Índice / Especificación Metodológica",
    y        = "Variable del I.M.P.E.",
    caption  = "Elaboración propia."
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 13.5, color = "#0F172A"),
    strip.background = ggplot2::element_rect(fill = "#0F172A", color = "#0F172A"),
    strip.text       = ggplot2::element_text(face = "bold", color = "#FFFFFF", size = 10),
    axis.text.x      = ggplot2::element_text(angle = 30, hjust = 1, face = "bold", size = 8.0, color = "#0F172A"),
    axis.text.y      = ggplot2::element_text(face = "bold", size = 9.5, color = "#0F172A"),
    legend.position  = "bottom",
    panel.grid       = ggplot2::element_blank()
  )

base::print(grafico_2_heatmap_3cp)
ggplot2::ggsave(
  filename = "Grafico_2_Heatmap_3CP_8_Indices.png",
  plot     = grafico_2_heatmap_3cp,
  width    = 17.5, height = 8.8, dpi = 300, bg = "white"
)

# 5.3 Gráfico 3: Evolución Geopolítica Histórica (1995-2024) en el IMPE MICE Base 12
paises_clave <- c("USA", "CHN", "JPN", "DEU", "GBR", "KOR", "IND", "BRA")
df_trayectoria <- mod_mice_12$panel_scores |>
  dplyr::filter(iso3c %in% paises_clave)

grafico_3_evolucion <- ggplot2::ggplot(
  data = df_trayectoria,
  mapping = ggplot2::aes(x = year, y = IMPE_100, color = iso3c, group = iso3c)
) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::geom_point(data = df_trayectoria |> dplyr::filter(year %in% c(1995, 2005, 2015, 2024)), size = 2.2) +
  ggplot2::scale_x_continuous(breaks = base::seq(1995, 2024, by = 5)) +
  ggplot2::labs(
    title    = "Evolución Histórica del Poder Económico Mundial (1995-2024) — IMPE MICE-PMM Base 12",
    subtitle = "Trayectoria de las Principales Potencias y Economías Emergentes (Escala 0 a 100)",
    x        = "Año",
    y        = "Índice de Poder Económico (IMPE Base 12: 0-100)",
    color    = "País (ISO-3):",
    caption  = "Elaboración propia."
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(face = "bold", size = 13.5, color = "#0F172A"),
    legend.position = "bottom",
    legend.title    = ggplot2::element_text(face = "bold")
  )

base::print(grafico_3_evolucion)
ggplot2::ggsave(
  filename = "Grafico_3_Evolucion_Geopolitica_IMPE_Base12.png",
  plot     = grafico_3_evolucion,
  width    = 13.5, height = 7.5, dpi = 300, bg = "white"
)

base::cat("
==============================================================================
")
base::cat("¡EJECUCIÓN MAESTRA COMPLETADA CON ÉXITO!
")
base::cat("1. Libro de Excel con 10 Hojas:", nombre_archivo_excel, "
")
base::cat("2. Gráfico 1 (Barras 3CP):      Grafico_1_Cargas_3CP_8_Indices.png
")
base::cat("3. Gráfico 2 (Heatmap 3CP):     Grafico_2_Heatmap_3CP_8_Indices.png
")
base::cat("4. Gráfico 3 (Evolución 95-24): Grafico_3_Evolucion_Geopolitica_IMPE_Base12.png
")
base::cat("==============================================================================
")