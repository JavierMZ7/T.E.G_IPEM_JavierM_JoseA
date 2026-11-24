# ==============================================================================
# SCRIPT MAESTRO AUTOCONTENIDO (v2 - BLINDADO CONTRA ERRORES DE ENTORNO):
# CONSTRUCCIÓN, ANÁLISIS DE 3 COMPONENTES (ACP), ALFA DE CRONBACH Y EXCEL
# ==============================================================================
# INSTRUCCIÓN IMPORTANTE PARA EJECUTAR EN RSTUDIO:
# Presiona Ctrl + A (seleccionar TODO el script) y luego Ctrl + Enter (Run),
# o haz clic en el botón "Source" arriba a la derecha del editor de RStudio.
# ==============================================================================

# ------------------------------------------------------------------------------
# PASO 0: DEFINICIÓN INMEDIATA DE VARIABLES (SIEMPRE EN LAS PRIMERAS LÍNEAS)
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

# Lista de los 48 códigos de Agregados Regionales/Financieros del Banco Mundial a excluir
agregados_wb <- c(
  "AFE", "AFW", "ARB", "CEB", "CSS", "EAP", "EAR", "EAS", "ECA", "ECS",
  "EMU", "EUU", "FCS", "HIC", "HPC", "IBD", "IBT", "IDA", "IDB", "IDX",
  "INX", "LAC", "LCN", "LDC", "LIC", "LMC", "LMY", "LTE", "MEA", "MIC",
  "MNA", "NAC", "OED", "OSS", "PRE", "PSS", "PST", "SAS", "SSA", "SSF",
  "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC", "WLD"
)

# Instalación automática de paquetes solo si faltan en tu computadora:
paquetes_requeridos <- c("dplyr", "tidyr", "readr", "readxl", "zoo", "psych", "openxlsx", "tibble")
paquetes_faltantes  <- paquetes_requeridos[!(paquetes_requeridos %in% utils::installed.packages()[, "Package"])]
if (base::length(paquetes_faltantes) > 0) {
  utils::install.packages(paquetes_faltantes)
}


# ==============================================================================
# ETAPA 1: CARGA INTELIGENTE DE DATOS Y FILTRADO VECTORIZADO CON rowSums()
# ==============================================================================

# 1.1 Verifica si 'panel_final' ya está en tu Environment y tiene las 16 variables.
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

# 1.2 Identificación limpia de países soberanos (elimina "", NAs y agregados del Banco Mundial)
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

# 1.3 Construcción de BASE 1 (Datos Observados Sin Imputar, 1995-2024)
# Usamos base::rowSums() directamente para evitar problemas de dplyr::across() dentro de filter()
panel_temp <- panel_final |>
  dplyr::mutate(iso3c = base::trimws( base::as.character(iso3c) )) |>
  dplyr::filter(
    !base::is.na(iso3c),
    base::nchar(iso3c) == 3,
    iso3c %in% codigos_soberanos,
    year >= 1995,
    year <= 2024
  )

# Calculamos cuántas de las 16 variables están observadas en cada fila usando rowSums
panel_temp$n_obs_fila_16 <- base::rowSums(!base::is.na(panel_temp[, vars_impe_16]))

panel_observado <- panel_temp |>
  dplyr::group_by(iso3c) |>
  dplyr::filter(base::mean(n_obs_fila_16 / base::length(vars_impe_16), na.rm = TRUE) >= 0.35) |>
  dplyr::ungroup() |>
  dplyr::select(-n_obs_fila_16) |>
  dplyr::arrange(iso3c, year)

# 1.4 Construcción de BASE 2 (Datos Exclusivamente Interpolados con zoo::na.approx)
# Interpola linealmente brechas internas dentro de cada país (rule = 1: no extrapola extremos)
interpolar_serie_interna <- function(v, t_anio) {
  if (base::sum(!base::is.na(v)) < 2) return(v)
  zoo::na.approx(v, x = t_anio, na.rm = FALSE, rule = 1)
}

panel_interpolado <- panel_observado |>
  dplyr::group_by(iso3c) |>
  dplyr::arrange(year, .by_group = TRUE) |>
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::all_of(c(vars_impe_16, "ctrl_pib_nominal")),
      .fns  = ~ interpolar_serie_interna(.x, year)
    )
  ) |>
  dplyr::ungroup()


# ==============================================================================
# ETAPA 2: FUNCIÓN MAESTRA DE A.C.P. (3 COMPONENTES), ALFA DE CRONBACH Y rowSums()
# ==============================================================================

reescalar_0_100 <- function(x) {
  rango <- base::max(x, na.rm = TRUE) - base::min(x, na.rm = TRUE)
  if (rango == 0) return(base::rep(0, base::length(x)))
  ((x - base::min(x, na.rm = TRUE)) / rango) * 100
}

congruencia_tucker <- function(u, v) {
  base::sum(u * v) / base::sqrt(base::sum(u^2) * base::sum(v^2))
}

ejecutar_modelo_impe_rowsums <- function(df_datos, vars_activas, dims_etiquetas, nombre_modelo) {
  
  # 2.1 Estandarización Z-score por columna en R base (rápida y sin errores de entorno)
  mat_cruda <- base::as.matrix(df_datos[, vars_activas])
  mat_z     <- base::scale(mat_cruda, center = TRUE, scale = TRUE)
  
  # 2.2 Matriz de Correlaciones de Pearson por Pares Completos (sin usar drop_na)
  mat_cor <- stats::cor(mat_z, use = "pairwise.complete.obs", method = "pearson")
  
  # Tamaño muestral promedio efectivo por par de variables
  mat_binaria    <- (!base::is.na(mat_z)) * 1
  conteo_pares   <- base::t(mat_binaria) %*% mat_binaria
  base::diag(conteo_pares) <- NA
  n_efectivo_par <- base::mean(conteo_pares, na.rm = TRUE)
  
  # 2.3 Pruebas de Validación: Alfa de Cronbach, KMO y Esfericidad de Bartlett
  test_alfa     <- psych::alpha(mat_cor, n.obs = n_efectivo_par, check.keys = TRUE)
  test_kmo      <- psych::KMO(mat_cor)
  test_bartlett <- psych::cortest.bartlett(mat_cor, n = base::round(n_efectivo_par))
  
  # 2.4 Descomposición Espectral (Autovalores y Autovectores de las Componentes Principales)
  eigen_res    <- base::eigen(mat_cor)
  autovalores  <- eigen_res$values
  autovectores <- eigen_res$vectors
  base::rownames(autovectores) <- vars_activas
  
  # Orientación consistente de signos en las 3 Primeras Componentes (CP1, CP2, CP3):
  if (autovectores["tech_id_pib", 1]     < 0) autovectores[, 1] <- -autovectores[, 1]
  if (autovectores["prod_exp_total", 2]  < 0) autovectores[, 2] <- -autovectores[, 2]
  if (autovectores["fin_mkt_cap_pib", 3] < 0) autovectores[, 3] <- -autovectores[, 3]
  
  # Cargas factoriales de las 3 Primeras Componentes: Carga_{j,k} = Autovector_{j,k} * sqrt(Autovalor_k)
  cargas_3cp      <- base::sweep(autovectores[, 1:3], 2, base::sqrt(autovalores[1:3]), `*`)
  comunalidad_3cp <- base::rowSums(cargas_3cp^2)
  
  # Pesos individuales en CP1, CP2 y CP3 (autovectores al cuadrado normalizados a suma 1)
  pesos_cp1 <- (autovectores[, 1]^2) / base::sum(autovectores[, 1]^2)
  pesos_cp2 <- (autovectores[, 2]^2) / base::sum(autovectores[, 2]^2)
  pesos_cp3 <- (autovectores[, 3]^2) / base::sum(autovectores[, 3]^2)
  
  # Peso Compuesto de las 3 Primeras Componentes (ponderado por su varianza relativa, OCDE 2008)
  var_rel_3cp    <- autovalores[1:3] / base::sum(autovalores[1:3])
  pesos_3cp_comp <- (pesos_cp1 * var_rel_3cp[1]) +
    (pesos_cp2 * var_rel_3cp[2]) +
    (pesos_cp3 * var_rel_3cp[3])
  pesos_3cp_comp <- pesos_3cp_comp / base::sum(pesos_3cp_comp)
  
  # 2.5 CÁLCULO DE PUNTAJES CON base::rowSums() SIN USAR drop_na()
  n_filas <- base::nrow(mat_z)
  n_cols  <- base::ncol(mat_z)
  
  # Conteo de variables observadas en cada fila (país-año) usando rowSums:
  vars_obs_fila <- base::rowSums(!base::is.na(mat_z))
  fraccion_obs  <- vars_obs_fila / n_cols
  
  # Matrices de pesos expandidas por fila para operar vectorialmente con rowSums:
  mat_w_cp1 <- base::matrix(pesos_cp1, nrow = n_filas, ncol = n_cols, byrow = TRUE)
  mat_w_3cp <- base::matrix(pesos_3cp_comp, nrow = n_filas, ncol = n_cols, byrow = TRUE)
  
  # Masa de peso activo observada en cada fila (0 a 1):
  masa_activa_cp1 <- base::rowSums((!base::is.na(mat_z)) * mat_w_cp1, na.rm = TRUE)
  masa_activa_3cp <- base::rowSums((!base::is.na(mat_z)) * mat_w_3cp, na.rm = TRUE)
  
  # (A) Índice rowSums Simple (suma directa de Z * w ignorando NAs):
  score_cp1_simple <- base::rowSums(mat_z * mat_w_cp1, na.rm = TRUE)
  
  # (B) Índice rowSums Inteligente (re-escala por la masa de peso observada en esa fila):
  score_cp1_smart  <- base::ifelse(masa_activa_cp1 > 0, score_cp1_simple / masa_activa_cp1, NA_real_)
  
  # (C) Índice Compuesto de las 3 Componentes (CP1 + CP2 + CP3) con rowSums Inteligente:
  score_3cp_simple <- base::rowSums(mat_z * mat_w_3cp, na.rm = TRUE)
  score_3cp_smart  <- base::ifelse(masa_activa_3cp > 0, score_3cp_simple / masa_activa_3cp, NA_real_)
  
  # (D) Puntajes de las 3 Componentes Ortogonales (CP1, CP2, CP3) por país-año:
  comp1_raw <- base::rowSums(base::sweep(mat_z, 2, autovectores[, 1], `*`), na.rm = TRUE) / fraccion_obs
  comp2_raw <- base::rowSums(base::sweep(mat_z, 2, autovectores[, 2], `*`), na.rm = TRUE) / fraccion_obs
  comp3_raw <- base::rowSums(base::sweep(mat_z, 2, autovectores[, 3], `*`), na.rm = TRUE) / fraccion_obs
  
  # 2.6 Tablas de Resultados Estructuradas
  tabla_resumen <- tibble::tibble(
    Modelo                  = nombre_modelo,
    Num_Variables           = n_cols,
    Observaciones_Retenidas = n_filas,
    Celdas_Activas_Totales  = base::sum(!base::is.na(mat_z)),
    Densidad_Datos_Pct      = base::round(base::mean(!base::is.na(mat_z)) * 100, 2),
    N_Medio_Pares           = base::round(n_efectivo_par, 1),
    Alfa_Cronbach_Std       = base::round(test_alfa$total$std.alpha, 4),
    Cor_InterItem_Media     = base::round(test_alfa$total$average_r, 4),
    KMO_Global_MSA          = base::round(test_kmo$MSA, 4),
    KMO_Minimo_MSA          = base::round(base::min(test_kmo$MSAi), 4),
    Bartlett_Chi2           = base::round(test_bartlett$chisq, 2),
    Bartlett_gl             = test_bartlett$df,
    Autovalor_CP1           = base::round(autovalores[1], 4),
    Varianza_CP1_Pct        = base::round((autovalores[1] / n_cols) * 100, 2),
    Autovalor_CP2           = base::round(autovalores[2], 4),
    Varianza_CP2_Pct        = base::round((autovalores[2] / n_cols) * 100, 2),
    Autovalor_CP3           = base::round(autovalores[3], 4),
    Varianza_CP3_Pct        = base::round((autovalores[3] / n_cols) * 100, 2),
    Varianza_Acum_3CP_Pct   = base::round((base::sum(autovalores[1:3]) / n_cols) * 100, 2)
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
    autovectores_raw = autovectores[, 1:3],
    cargas_raw       = cargas_3cp
  )
}


# ==============================================================================
# ETAPA 3: EJECUCIÓN DE LOS MODELOS (BASE 16 Y BASE 12) Y PRUEBAS DE CONGRUENCIA
# ==============================================================================

dims_16 <- c(base::rep("Productiva", 6), base::rep("Financiera", 4), base::rep("Tecnológica", 6))
dims_12 <- c(base::rep("Productiva", 4), base::rep("Financiera", 3), base::rep("Tecnológica", 5))

mod_obs_16 <- ejecutar_modelo_impe_rowsums(panel_observado,   vars_impe_16, dims_16, "1A_Sin_Imputar_Base16")
mod_int_16 <- ejecutar_modelo_impe_rowsums(panel_interpolado, vars_impe_16, dims_16, "2A_Interpolado_Base16")

mod_obs_12 <- ejecutar_modelo_impe_rowsums(panel_observado,   vars_impe_12, dims_12, "1B_Sin_Imputar_Base12")
mod_int_12 <- ejecutar_modelo_impe_rowsums(panel_interpolado, vars_impe_12, dims_12, "2B_Interpolado_Base12")

tabla_resumen_comparativa <- dplyr::bind_rows(
  mod_obs_16$resumen,
  mod_int_16$resumen,
  mod_obs_12$resumen,
  mod_int_12$resumen
)

tabla_congruencia_3cp <- tibble::tibble(
  Componente_Comparada  = c("CP1 (Poder Económico General)",
                            "CP2 (Escala Absoluta vs. Intensidad Per Cápita)",
                            "CP3 (Hegemonía Financiera vs. Manufactura)"),
  Tucker_Phi_Base16     = c(
    base::round(congruencia_tucker(mod_obs_16$autovectores_raw[, 1], mod_int_16$autovectores_raw[, 1]), 4),
    base::round(congruencia_tucker(mod_obs_16$autovectores_raw[, 2], mod_int_16$autovectores_raw[, 2]), 4),
    base::round(congruencia_tucker(mod_obs_16$autovectores_raw[, 3], mod_int_16$autovectores_raw[, 3]), 4)
  ),
  Tucker_Phi_Base12     = c(
    base::round(congruencia_tucker(mod_obs_12$autovectores_raw[, 1], mod_int_12$autovectores_raw[, 1]), 4),
    base::round(congruencia_tucker(mod_obs_12$autovectores_raw[, 2], mod_int_12$autovectores_raw[, 2]), 4),
    base::round(congruencia_tucker(mod_obs_12$autovectores_raw[, 3], mod_int_12$autovectores_raw[, 3]), 4)
  ),
  Criterio_Lorenzo_Seva = "Phi >= 0.95 (Equivalencia Factorial Estricta)"
)

panel_comparado_maestro <- mod_obs_16$panel_scores |>
  dplyr::select(
    iso3c, country, year, ctrl_pib_nominal,
    Obs16_Vars_Activas  = Vars_Activas_Fila,
    Obs16_Masa_Peso_Pct = Masa_Peso_Activo_Pct,
    Obs16_CP1_100       = Comp_CP1_100,
    Obs16_CP2_100       = Comp_CP2_100,
    Obs16_CP3_100       = Comp_CP3_100,
    IMPE_Obs16_Simple   = IMPE_rowSums_Simple,
    IMPE_Obs16_Smart    = IMPE_rowSums_Smart,
    IMPE_Obs16_3CP      = IMPE_3CP_Smart,
    Rank_Obs16_Smart    = Rank_rowSums_Smart
  ) |>
  dplyr::left_join(
    mod_int_16$panel_scores |>
      dplyr::select(
        iso3c, year,
        Int16_Vars_Activas  = Vars_Activas_Fila,
        Int16_Masa_Peso_Pct = Masa_Peso_Activo_Pct,
        Int16_CP1_100       = Comp_CP1_100,
        Int16_CP2_100       = Comp_CP2_100,
        Int16_CP3_100       = Comp_CP3_100,
        IMPE_Int16_Simple   = IMPE_rowSums_Simple,
        IMPE_Int16_Smart    = IMPE_rowSums_Smart,
        IMPE_Int16_3CP      = IMPE_3CP_Smart,
        Rank_Int16_Smart    = Rank_rowSums_Smart
      ),
    by = c("iso3c", "year")
  ) |>
  dplyr::left_join(
    mod_obs_12$panel_scores |>
      dplyr::select(
        iso3c, year,
        IMPE_Obs12_Smart = IMPE_rowSums_Smart,
        IMPE_Obs12_3CP   = IMPE_3CP_Smart,
        Rank_Obs12_Smart = Rank_rowSums_Smart
      ),
    by = c("iso3c", "year")
  ) |>
  dplyr::left_join(
    mod_int_12$panel_scores |>
      dplyr::select(
        iso3c, year,
        IMPE_Int12_Smart = IMPE_rowSums_Smart,
        IMPE_Int12_3CP   = IMPE_3CP_Smart,
        Rank_Int12_Smart = Rank_rowSums_Smart
      ),
    by = c("iso3c", "year")
  )

if (base::file.exists("IMPE_Panel_Final_1995_2024.csv")) {
  df_mice_ref <- readr::read_csv("IMPE_Panel_Final_1995_2024.csv", show_col_types = FALSE) |>
    dplyr::select(iso3c, year, IMPE_MICE_100 = IMPE_100) |>
    dplyr::group_by(year) |>
    dplyr::mutate(Rank_MICE = dplyr::min_rank(dplyr::desc(IMPE_MICE_100))) |>
    dplyr::ungroup()
  panel_comparado_maestro <- panel_comparado_maestro |>
    dplyr::left_join(df_mice_ref, by = c("iso3c", "year"))
}

cols_indices_corr <- panel_comparado_maestro |>
  dplyr::select(dplyr::starts_with("IMPE_"))

matriz_pearson_indices  <- base::round(stats::cor(cols_indices_corr, use = "pairwise.complete.obs", method = "pearson"), 4) |>
  tibble::as_tibble(rownames = "Indice")
matriz_spearman_indices <- base::round(stats::cor(cols_indices_corr, use = "pairwise.complete.obs", method = "spearman"), 4) |>
  tibble::as_tibble(rownames = "Indice")

ranking_comparado_2024 <- panel_comparado_maestro |>
  dplyr::filter(year == base::max(year, na.rm = TRUE)) |>
  dplyr::arrange(Rank_Int16_Smart)


# ==============================================================================
# ETAPA 4: EXPORTACIÓN AUTOMÁTICA A UN SOLO EXCEL CON FORMATO PROFESIONAL (openxlsx)
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

crear_hoja_profesional(
  wb, "1_Dashboard_Comparativo_3CP",
  "CUADRO DE MANDO: COMPARACIÓN DE LAS 3 COMPONENTES PRINCIPALES Y ALFA DE CRONBACH",
  "Contraste Metodológico entre el Índice Sin Imputar (Observado + rowSums) y el Índice Exclusivamente Interpolado (zoo::na.approx + rowSums)",
  base::list(
    base::list(encabezado_seccion = "1. COMPARACIÓN GLOBAL DE PRUEBAS PSICOMÉTRICAS Y LAS 3 PRIMERAS COMPONENTES (BASE 16 Y BASE 12)", datos = tabla_resumen_comparativa, color_verde = FALSE),
    base::list(encabezado_seccion = "2. EQUIVALENCIA FACTORIAL DE LAS 3 COMPONENTES PRINCIPALES (CONGRUENCIA DE TUCKER: SIN IMPUTAR vs. INTERPOLADO)", datos = tabla_congruencia_3cp, color_verde = TRUE),
    base::list(encabezado_seccion = "3. MATRIZ DE CORRELACIÓN LINEAL DE PEARSON ENTRE LOS ÍNDICES CONSTRUIDOS (N = 5,460)", datos = matriz_pearson_indices, color_verde = FALSE),
    base::list(encabezado_seccion = "4. MATRIZ DE CORRELACIÓN DE RANGOS DE SPEARMAN ENTRE LOS ÍNDICES CONSTRUIDOS (N = 5,460)", datos = matriz_spearman_indices, color_verde = TRUE)
  )
)

crear_hoja_profesional(
  wb, "2_PCA_3CP_Sin_Imputar",
  "ANÁLISIS DE LAS 3 COMPONENTES PRINCIPALES (CP1, CP2, CP3) Y ALFA — ÍNDICE 1: SIN IMPUTAR",
  "Autovectores, Cargas Factoriales, Comunalidades, Pesos en CP1, Pesos Compuestos 3CP, KMO Individual y Alfa de Cronbach (Datos Observados)",
  base::list(
    base::list(encabezado_seccion = "1. MATRIZ DE CARGAS, AUTOVECTORES Y PESOS DE LAS 3 PRIMERAS COMPONENTES (BASE 16 - SIN IMPUTAR)", datos = mod_obs_16$pca_3cp, color_verde = FALSE),
    base::list(encabezado_seccion = "2. DESCOMPOSICIÓN ESPECTRAL DE LOS 16 AUTOVALORES (CP1 A CP16 - SIN IMPUTAR)", datos = mod_obs_16$autovalores, color_verde = TRUE)
  )
)

crear_hoja_profesional(
  wb, "3_PCA_3CP_Interpolado",
  "ANÁLISIS DE LAS 3 COMPONENTES PRINCIPALES (CP1, CP2, CP3) Y ALFA — ÍNDICE 2: INTERPOLADO",
  "Autovectores, Cargas Factoriales, Comunalidades, Pesos en CP1, Pesos Compuestos 3CP, KMO Individual y Alfa de Cronbach (Datos Interpolados)",
  base::list(
    base::list(encabezado_seccion = "1. MATRIZ DE CARGAS, AUTOVECTORES Y PESOS DE LAS 3 PRIMERAS COMPONENTES (BASE 16 - INTERPOLADO)", datos = mod_int_16$pca_3cp, color_verde = TRUE),
    base::list(encabezado_seccion = "2. DESCOMPOSICIÓN ESPECTRAL DE LOS 16 AUTOVALORES (CP1 A CP16 - INTERPOLADO)", datos = mod_int_16$autovalores, color_verde = FALSE)
  )
)

crear_hoja_profesional(
  wb, "4_PCA_3CP_Base12_Depurado",
  "ANÁLISIS DE LAS 3 COMPONENTES PRINCIPALES Y ALFA EN VERSIÓN DEPURADA (BASE 12: NÚCLEO ESENCIAL)",
  "Comparación del ACP de las 3 Primeras Componentes y Alfa de Cronbach (~0.898) excluyendo las 4 variables ruidosas",
  base::list(
    base::list(encabezado_seccion = "1. ACP DE LAS 3 COMPONENTES Y ALFA DE CRONBACH — BASE 12 SIN IMPUTAR (Alfa = 0.8961)", datos = mod_obs_12$pca_3cp, color_verde = FALSE),
    base::list(encabezado_seccion = "2. ACP DE LAS 3 COMPONENTES Y ALFA DE CRONBACH — BASE 12 INTERPOLADO (Alfa = 0.8986)", datos = mod_int_12$pca_3cp, color_verde = TRUE),
    base::list(encabezado_seccion = "3. AUTOVALORES COMPARADOS EN BASE 12 (CP1 A CP12: SIN IMPUTAR vs. INTERPOLADO)", datos = dplyr::bind_rows(mod_obs_12$autovalores, mod_int_12$autovalores), color_verde = FALSE)
  )
)

crear_hoja_profesional(
  wb, "5_Ranking_Comparado_2024",
  "RANKING MUNDIAL COMPARADO EN 2024 (182 PAÍSES SOBERANOS)",
  "Comparación de Puntajes y Posiciones en 2024 entre el Índice Sin Imputar (rowSums), el Índice Interpolado (rowSums) y las variantes 3CP",
  base::list(
    base::list(encabezado_seccion = "1. TABLA COMPARATIVA DE LOS 182 PAÍSES EN 2024 (ORDENADA POR EL ÍNDICE INTERPOLADO INTELIGENTE)", datos = ranking_comparado_2024, color_verde = FALSE)
  )
)

crear_hoja_profesional(
  wb, "6_Resultado_Sin_Imputar",
  "RESULTADOS COMPLETOS DEL ÍNDICE 1: DATOS OBSERVADOS SIN IMPUTAR (USO INTELIGENTE DE rowSums, 1995-2024)",
  "Panel de 5,460 Observaciones con Conteo de Variables Activas, Masa de Peso Activo, Componentes CP1, CP2, CP3 e Índices rowSums",
  base::list(
    base::list(encabezado_seccion = "1. PANEL COMPLETO (1995-2024) — ÍNDICE SIN IMPUTAR (N = 5,460)", datos = mod_obs_16$panel_scores, color_verde = FALSE)
  )
)

crear_hoja_profesional(
  wb, "7_Resultado_Interpolado",
  "RESULTADOS COMPLETOS DEL ÍNDICE 2: DATOS EXCLUSIVAMENTE INTERPOLADOS (zoo::na.approx + rowSums, 1995-2024)",
  "Panel de 5,460 Observaciones con Conteo de Variables Activas Tras Interpolar, Componentes CP1, CP2, CP3 e Índices rowSums",
  base::list(
    base::list(encabezado_seccion = "1. PANEL COMPLETO (1995-2024) — ÍNDICE EXCLUSIVAMENTE INTERPOLADO (N = 5,460)", datos = mod_int_16$panel_scores, color_verde = TRUE)
  )
)

crear_hoja_profesional(
  wb, "8_Panel_Maestro_Todos",
  "PANEL MAESTRO CONSOLIDADO: TODOS LOS ÍNDICES UNIDOS LADO A LADO (1995-2024, N = 5,460)",
  "Incluye Índice Sin Imputar (Simple, Inteligente, 3CP, Base 12), Índice Interpolado (Simple, Inteligente, 3CP, Base 12) y MICE-PMM",
  base::list(
    base::list(encabezado_seccion = "1. MATRIZ CONSOLIDADA DE TODOS LOS ÍNDICES PARA ANÁLISIS ECONOMÉTRICO (N = 5,460)", datos = panel_comparado_maestro, color_verde = FALSE)
  )
)

nombre_archivo_excel <- "IMPE_Comparativa_SinImputar_vs_Interpolado_R.xlsx"
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