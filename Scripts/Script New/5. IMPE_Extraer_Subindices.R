# ==============================================================================
# SCRIPT DE EXTRACCIÓN DE DATA COMPLEMENTARIA PARA CAPÍTULO IV — IMPE (1995-2024)
# ==============================================================================
# Genera en un solo paso los 3 insumos faltantes para cerrar el análisis del
# Capítulo IV y alimentar los gráficos de variables y subíndices por país:
#
#   1. Series país-año de los 3 subíndices (Productivo, Financiero, Tecnológico)
#      en los 20 índices + exportación estandarizada de `paneles_20_indices.rds`.
#   2. Diagnósticos distribucionales de imputación por variable en Z-scores
#      (Media, DE, Ratio DE y prueba Kolmogorov-Smirnov Real vs. Imputado)
#      para los 4 métodos de imputación (Interp3, PMM, Bayes, RF) en las 4 familias.
#   3. Descarga de PIB Nominal y PIB PPA (Banco Mundial / WDI 1995-2024) y
#      contraste formal de Validez Externa (Objetivo Específico 4):
#      correlaciones IMPE vs. PIB y tabla de divergencias estructurales de ranking.
#
# SALIDAS GENERADAS:
#   - IMPE_Data_Complementaria_Cap4.xlsx   (6 hojas listas para el Capítulo IV)
#   - IMPE_Subindices_20_Indices_Largo.csv (serie completa de subíndices 1995-2024)
#   - paneles_20_indices.rds               (insumo directo para los scripts de plots)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PAQUETES Y CONFIGURACIÓN
# ------------------------------------------------------------------------------
paquetes <- c(
  "readxl", "openxlsx", "WDI", "dplyr", "tidyr",
  "stringr", "purrr", "readr"
)

faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) install.packages(faltantes, dependencies = TRUE)
invisible(lapply(paquetes, library, character.only = TRUE))

RUTA_EXCEL_MAESTRO    <- "IMPE_Comparativa_Maestra_20_Indices.xlsx"
RUTA_SALIDA_EXCEL     <- "IMPE_Data_Complementaria_Cap4.xlsx"
RUTA_SALIDA_SUB_CSV   <- "IMPE_Subindices_20_Indices_Largo.csv"
RUTA_PANELES_RDS      <- "paneles_20_indices.rds"
CARPETA_CACHE_PARQUET <- "cache_imputacion"   # Si guardaste los MICE en Parquet

# Si tu objeto en memoria de IMPE_consolidado_maestro.R tiene un nombre específico,
# escríbelo aquí (o déjalo en NULL para autodetección verificada):
NOMBRE_OBJETO_PANELES <- NULL

# Países clave para la hoja ejecutiva de subíndices en Excel
PAISES_CLAVE <- c("USA", "CHN", "RUS", "JPN", "GBR", "DEU", "KOR", "SGP", "CHE", "VEN")
ANIOS_HITO   <- c(1995, 2000, 2005, 2010, 2015, 2020, 2024)

# ------------------------------------------------------------------------------
# 1. CARGA DEL EXCEL MAESTRO Y DETECCIÓN VERIFICADA DE LOS 20 PANELES
# ------------------------------------------------------------------------------
stopifnot("No se encontró IMPE_Comparativa_Maestra_20_Indices.xlsx." = file.exists(RUTA_EXCEL_MAESTRO))

df_estadisticos <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "1_Estadisticos_Indices")
df_pca          <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "4_PCA_Detalle_Completo")
df_scores100    <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "9_Scores100_20_Indices")
df_ranks        <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "11_Ranks_20_Indices")

ORDEN_INDICES <- df_estadisticos$Indice
VARS_MODELO   <- unique(df_pca$Variable)

clasificar_subindice <- function(v) {
  case_when(
    str_starts(v, "prod_") ~ "Productivo",
    str_starts(v, "fin_")  ~ "Financiero",
    str_starts(v, "tech_") ~ "Tecnologico",
    TRUE ~ "Otro"
  )
}

df_pca <- df_pca %>%
  mutate(Subindice = clasificar_subindice(Variable))

# Verifica si un objeto es un data.frame válido de panel IMPE (o contiene uno dentro)
extraer_df_panel <- function(elem) {
  normalizar_cols <- function(df) {
    if (!is.data.frame(df)) return(NULL)
    nms <- names(df)
    # Normalizar posibles alias de iso3c / year
    if (!"iso3c" %in% nms && "country_iso3_code" %in% nms) {
      df <- dplyr::rename(df, iso3c = country_iso3_code)
      nms <- names(df)
    }
    if (!"year" %in% nms && "Year" %in% nms) {
      df <- dplyr::rename(df, year = Year)
      nms <- names(df)
    }
    tiene_llave <- all(c("iso3c", "year") %in% nms)
    n_vars      <- length(intersect(VARS_MODELO, nms))
    if (tiene_llave && n_vars >= 8) {
      return(as_tibble(df))
    }
    return(NULL)
  }

  # Caso 1: el elemento ya es directamente el data.frame del panel
  df_directo <- normalizar_cols(elem)
  if (!is.null(df_directo)) return(df_directo)

  # Caso 2: el elemento es una lista (p. ej. resultado de cada índice) con un slot data.frame
  if (is.list(elem) && !is.data.frame(elem)) {
    for (sub in elem) {
      df_sub <- normalizar_cols(sub)
      if (!is.null(df_sub)) return(df_sub)
    }
  }
  return(NULL)
}

# Valida y extrae una lista limpia de data.frames con nombres en ORDEN_INDICES
validar_lista_paneles <- function(obj) {
  if (!is.list(obj) || is.data.frame(obj) || is.null(names(obj))) return(NULL)
  coinciden <- intersect(names(obj), ORDEN_INDICES)
  if (length(coinciden) < 10) return(NULL)

  # Probar extracción sobre los elementos coincidentes (descarta MATRICES_R, RESULTADOS_EIGEN, etc.)
  extraidos <- setNames(lapply(coinciden, function(k) extraer_df_panel(obj[[k]])), coinciden)
  validos   <- extraidos[!vapply(extraidos, is.null, logical(1))]
  if (length(validos) >= 10) {
    return(validos)
  }
  return(NULL)
}

# Localiza los 20 paneles en .GlobalEnv, .rds o Parquet (ignorando MATRICES_R / EIGEN / PRCOMP)
localizar_paneles_20 <- function() {
  # 1) Nombre explícito indicado por el usuario
  if (!is.null(NOMBRE_OBJETO_PANELES) && exists(NOMBRE_OBJETO_PANELES, envir = .GlobalEnv)) {
    res <- validar_lista_paneles(get(NOMBRE_OBJETO_PANELES, envir = .GlobalEnv))
    if (!is.null(res)) {
      message("✔ Paneles validados desde el objeto: ", NOMBRE_OBJETO_PANELES, " (", length(res), " índices)")
      return(res)
    } else {
      warning("El objeto '", NOMBRE_OBJETO_PANELES, "' no contiene data.frames con columnas iso3c, year y variables del modelo.")
    }
  }

  # 2) Búsqueda verificada en .GlobalEnv (solo acepta listas con data.frames reales de iso3c + year + variables)
  for (nm in ls(envir = .GlobalEnv)) {
    obj <- get(nm, envir = .GlobalEnv)
    res <- validar_lista_paneles(obj)
    if (!is.null(res)) {
      message("✔ Lista de paneles validada automáticamente en .GlobalEnv: ", nm, " (", length(res), " índices)")
      return(res)
    }
  }

  # 3) Archivo .rds existente (solo si pasa la validación)
  if (file.exists(RUTA_PANELES_RDS)) {
    res <- validar_lista_paneles(readRDS(RUTA_PANELES_RDS))
    if (!is.null(res)) {
      message("✔ Paneles validados desde: ", RUTA_PANELES_RDS, " (", length(res), " índices)")
      return(res)
    }
  }

  # 4) Carpeta de caché Parquet (si arrow está instalado)
  if (dir.exists(CARPETA_CACHE_PARQUET) && requireNamespace("arrow", quietly = TRUE)) {
    archivos <- list.files(CARPETA_CACHE_PARQUET, pattern = "\\.parquet$", full.names = TRUE)
    nombres  <- tools::file_path_sans_ext(basename(archivos))
    validos  <- nombres %in% ORDEN_INDICES
    if (any(validos)) {
      lista_pq <- setNames(lapply(archivos[validos], arrow::read_parquet), nombres[validos])
      res <- validar_lista_paneles(lista_pq)
      if (!is.null(res)) {
        message("✔ Paneles validados desde ", CARPETA_CACHE_PARQUET, " (", length(res), " índices)")
        return(res)
      }
    }
  }

  return(NULL)
}

paneles_20 <- localizar_paneles_20()

if (!is.null(paneles_20)) {
  saveRDS(paneles_20, RUTA_PANELES_RDS)
  message("✔ Exportado ", RUTA_PANELES_RDS, " (", length(paneles_20), " paneles verificados).")
} else {
  # Diagnóstico útil: listar qué objetos tipo lista existen en memoria para ayudar al usuario
  listas_env <- Filter(function(nm) {
    o <- get(nm, envir = .GlobalEnv)
    is.list(o) && !is.data.frame(o)
  }, ls(envir = .GlobalEnv))
  message("\nℹ AVISO: No se detectó en memoria una lista de data.frames con (iso3c, year + variables) para los 20 índices.")
  if (length(listas_env) > 0) {
    message("  Listas encontradas en tu Environment: ", paste(listas_env, collapse = ", "))
    message("  (Se descartaron automáticamente MATRICES_R / RESULTADOS_EIGEN / RESULTADOS_PRCOMP por no ser paneles país-año).")
  }
  message("  -> El Bloque 3 (PIB y Validez Externa) sí se ejecutará completo ahora.")
  message("  -> Para activar Bloques 1 y 2, asegúrate de guardar tus 20 data.frames en LISTA_PANELES_20 en IMPE_consolidado_maestro.R.\n")
}

# ==============================================================================
# BLOQUE 1: SUBÍNDICES PAÍS-AÑO (PRODUCTIVO, FINANCIERO, TECNOLÓGICO) EN 20 ÍNDICES
# ==============================================================================
df_subindices_largo   <- tibble()
df_subindices_resumen <- tibble()

if (!is.null(paneles_20)) {
  message("\n--- [1/3] Calculando los 3 subíndices país-año para los 20 índices ---")

  calcular_subindices_uno <- function(nombre_idx) {
    if (!nombre_idx %in% names(paneles_20)) return(NULL)
    df_p <- paneles_20[[nombre_idx]]

    pesos_idx <- df_pca %>%
      filter(Indice == nombre_idx) %>%
      group_by(Subindice) %>%
      mutate(Peso_Sub = Peso_CP1 / sum(Peso_CP1, na.rm = TRUE)) %>%
      ungroup()

    vars_disp <- intersect(pesos_idx$Variable, names(df_p))
    if (length(vars_disp) == 0) return(NULL)
    if (!"country" %in% names(df_p)) df_p$country <- df_p$iso3c

    df_z <- df_p %>%
      mutate(across(all_of(vars_disp), ~ as.numeric(scale(.x))))

    df_z %>%
      select(iso3c, country, year, all_of(vars_disp)) %>%
      pivot_longer(cols = all_of(vars_disp), names_to = "Variable", values_to = "Z") %>%
      left_join(pesos_idx %>% select(Variable, Subindice, Peso_Sub), by = "Variable") %>%
      group_by(iso3c, country, year, Subindice) %>%
      summarise(
        Subindice_Z = if (all(is.na(Z))) NA_real_ else sum(Z * Peso_Sub, na.rm = TRUE) / sum(Peso_Sub[!is.na(Z)]),
        .groups = "drop"
      ) %>%
      group_by(Subindice) %>%
      mutate(
        Subindice_100 = 100 * (Subindice_Z - min(Subindice_Z, na.rm = TRUE)) /
          (max(Subindice_Z, na.rm = TRUE) - min(Subindice_Z, na.rm = TRUE)),
        Indice = nombre_idx
      ) %>%
      ungroup()
  }

  df_subindices_largo <- map_dfr(ORDEN_INDICES, calcular_subindices_uno) %>%
    left_join(df_estadisticos %>% select(Indice, Familia, Metodo), by = "Indice") %>%
    select(Indice, Familia, Metodo, iso3c, country, year, Subindice, Subindice_Z, Subindice_100)

  write_csv(df_subindices_largo, RUTA_SALIDA_SUB_CSV)
  message("✔ Exportado CSV completo de subíndices: ", RUTA_SALIDA_SUB_CSV, " (", nrow(df_subindices_largo), " filas)")

  df_subindices_resumen <- df_subindices_largo %>%
    filter(iso3c %in% PAISES_CLAVE, year %in% ANIOS_HITO) %>%
    select(Indice, Familia, Metodo, iso3c, country, year, Subindice, Subindice_100) %>%
    mutate(Subindice_100 = round(Subindice_100, 2)) %>%
    pivot_wider(names_from = Subindice, values_from = Subindice_100, names_prefix = "Sub100_") %>%
    arrange(iso3c, year, Familia, Metodo)
}

# ==============================================================================
# BLOQUE 2: DIAGNÓSTICOS DISTRIBUCIONALES DE IMPUTACIÓN POR VARIABLE (Z-SCORES + KS)
# ==============================================================================
df_diag_vars_z  <- tibble()
df_diag_resumen <- tibble()

if (!is.null(paneles_20)) {
  message("\n--- [2/3] Calculando diagnósticos distribucionales (Z-scores y KS) por variable y método ---")

  familias_unicas <- unique(df_estadisticos$Familia)
  sufijos_imp <- c(
    "2_Interp_Max3" = "Interp3",
    "3_MICE_PMM"    = "PMM",
    "4_MICE_Bayes"  = "Bayes",
    "5_MICE_RF"     = "RF"
  )

  evaluar_familia_metodo <- function(fam, met_codigo, sufijo) {
    idx_raw <- paste0(fam, "_Raw")
    idx_imp <- paste0(fam, "_", sufijo)
    if (!all(c(idx_raw, idx_imp) %in% names(paneles_20))) return(NULL)

    df_raw <- paneles_20[[idx_raw]]
    df_imp <- paneles_20[[idx_imp]]

    vars_fam <- df_pca %>% filter(Indice == idx_imp) %>% pull(Variable) %>% unique()
    vars_fam <- intersect(vars_fam, intersect(names(df_raw), names(df_imp)))

    llave_raw <- df_raw %>% select(iso3c, year, all_of(vars_fam))
    llave_imp <- df_imp %>% select(iso3c, year, all_of(vars_fam))
    unido <- inner_join(llave_raw, llave_imp, by = c("iso3c", "year"), suffix = c("__raw", "__imp"))

    map_dfr(vars_fam, function(v) {
      x_raw <- unido[[paste0(v, "__raw")]]
      x_imp <- unido[[paste0(v, "__imp")]]

      mu_r <- mean(x_raw, na.rm = TRUE)
      sd_r <- sd(x_raw, na.rm = TRUE)
      if (is.na(sd_r) || sd_r == 0) return(NULL)

      z_real     <- (x_raw[!is.na(x_raw)] - mu_r) / sd_r
      mask_imp   <- is.na(x_raw) & !is.na(x_imp)
      z_solo_imp <- (x_imp[mask_imp] - mu_r) / sd_r
      z_completo <- (x_imp[!is.na(x_imp)] - mu_r) / sd_r

      ks_imp <- if (length(z_solo_imp) >= 10) suppressWarnings(ks.test(z_real, z_solo_imp)) else list(statistic = NA_real_, p.value = NA_real_)
      ks_tot <- if (length(z_completo) >= 10) suppressWarnings(ks.test(z_real, z_completo)) else list(statistic = NA_real_, p.value = NA_real_)

      tibble(
        Indice               = idx_imp,
        Familia              = fam,
        Metodo               = met_codigo,
        Variable             = v,
        Subindice            = clasificar_subindice(v),
        N_Real               = length(z_real),
        N_Imputado           = length(z_solo_imp),
        Pct_Imputado         = round(100 * length(z_solo_imp) / max(1, nrow(unido)), 2),
        Media_Z_Real         = round(mean(z_real), 4),
        Media_Z_Imputado     = if (length(z_solo_imp) > 0) round(mean(z_solo_imp), 4) else NA_real_,
        Diff_Media_Z         = if (length(z_solo_imp) > 0) round(mean(z_solo_imp) - mean(z_real), 4) else NA_real_,
        SD_Z_Real            = round(sd(z_real), 4),
        SD_Z_Imputado        = if (length(z_solo_imp) > 1) round(sd(z_solo_imp), 4) else NA_real_,
        Ratio_SD_Imp_vs_Real = if (length(z_solo_imp) > 1) round(sd(z_solo_imp) / sd(z_real), 4) else NA_real_,
        KS_D_Real_vs_Imp     = round(as.numeric(ks_imp$statistic), 4),
        KS_p_Real_vs_Imp     = round(as.numeric(ks_imp$p.value), 4),
        KS_D_Real_vs_Total   = round(as.numeric(ks_tot$statistic), 4)
      )
    })
  }

  grid_fm <- tidyr::crossing(Familia = familias_unicas, Metodo = names(sufijos_imp)) %>%
    mutate(Sufijo = sufijos_imp[Metodo])

  df_diag_vars_z <- pmap_dfr(grid_fm, ~ evaluar_familia_metodo(..1, ..2, ..3))

  if (nrow(df_diag_vars_z) > 0) {
    df_diag_resumen <- df_diag_vars_z %>%
      group_by(Indice, Familia, Metodo) %>%
      summarise(
        N_Vars_Evaluadas        = n(),
        Pct_Imputado_Medio      = round(mean(Pct_Imputado, na.rm = TRUE), 2),
        Abs_Diff_Media_Z_Medio  = round(mean(abs(Diff_Media_Z), na.rm = TRUE), 4),
        Ratio_SD_Medio          = round(mean(Ratio_SD_Imp_vs_Real, na.rm = TRUE), 4),
        KS_D_Real_vs_Imp_Medio  = round(mean(KS_D_Real_vs_Imp, na.rm = TRUE), 4),
        KS_D_Real_vs_Tot_Medio  = round(mean(KS_D_Real_vs_Total, na.rm = TRUE), 4),
        .groups = "drop"
      ) %>%
      arrange(Familia, KS_D_Real_vs_Tot_Medio)
  }
}

# ==============================================================================
# BLOQUE 3: SERIE DE PIB (WDI 1995-2024) Y VALIDEZ EXTERNA (OBJETIVO ESPECÍFICO 4)
# ==============================================================================
message("\n--- [3/3] Descargando PIB Nominal y PIB PPA (WDI) y calculando Validez Externa ---")

paises_soberanos <- unique(df_scores100$iso3c)

df_pib_raw <- WDI::WDI(
  country   = "all",
  indicator = c(
    PIB_Nominal_USD = "NY.GDP.MKTP.CD",
    PIB_PPA_Const   = "NY.GDP.MKTP.PP.KD"
  ),
  start     = 1995,
  end       = 2024,
  extra     = FALSE
) %>%
  as_tibble() %>%
  filter(iso3c %in% paises_soberanos) %>%
  select(iso3c, year, PIB_Nominal_USD, PIB_PPA_Const) %>%
  group_by(year) %>%
  mutate(
    Rank_PIB_Nominal = if_else(!is.na(PIB_Nominal_USD), min_rank(desc(PIB_Nominal_USD)), NA_integer_),
    Rank_PIB_PPA     = if_else(!is.na(PIB_PPA_Const),   min_rank(desc(PIB_PPA_Const)),   NA_integer_)
  ) %>%
  ungroup()

df_impe_pib <- df_scores100 %>%
  left_join(df_pib_raw, by = c("iso3c", "year"))

cols_score <- grep("^Score100_", names(df_scores100), value = TRUE)

df_validez_externa_cor <- map_dfr(cols_score, function(col) {
  idx_nom <- str_remove(col, "^Score100_")
  v_impe  <- df_impe_pib[[col]]
  v_nom   <- df_impe_pib$PIB_Nominal_USD
  v_ppa   <- df_impe_pib$PIB_PPA_Const

  ok_nom <- !is.na(v_impe) & !is.na(v_nom) & v_nom > 0
  ok_ppa <- !is.na(v_impe) & !is.na(v_ppa) & v_ppa > 0

  tibble(
    Indice                     = idx_nom,
    N_Pares_PIB_Nom            = sum(ok_nom),
    Spearman_IMPE_vs_PIB_Nom   = round(cor(v_impe[ok_nom], v_nom[ok_nom], method = "spearman"), 4),
    Pearson_IMPE_vs_logPIB_Nom = round(cor(v_impe[ok_nom], log1p(v_nom[ok_nom]), method = "pearson"), 4),
    N_Pares_PIB_PPA            = sum(ok_ppa),
    Spearman_IMPE_vs_PIB_PPA   = round(cor(v_impe[ok_ppa], v_ppa[ok_ppa], method = "spearman"), 4),
    Pearson_IMPE_vs_logPIB_PPA = round(cor(v_impe[ok_ppa], log1p(v_ppa[ok_ppa]), method = "pearson"), 4)
  )
}) %>%
  left_join(df_estadisticos %>% select(Indice, Familia, Metodo, N_Paises), by = "Indice") %>%
  select(Indice, Familia, Metodo, N_Paises, everything())

df_div_2024 <- df_scores100 %>%
  filter(year == 2024) %>%
  select(
    iso3c, country, year,
    Score100_2A_Bayes = Score100_2A_Base12_Completa_Bayes,
    Score100_2B_Bayes = Score100_2B_Base12_Depurada25_Bayes,
    Score100_2B_RF    = Score100_2B_Base12_Depurada25_RF
  ) %>%
  left_join(
    df_ranks %>%
      filter(year == 2024) %>%
      select(
        iso3c, year,
        Rank_IMPE_2A_Bayes = Rank_2A_Base12_Completa_Bayes,
        Rank_IMPE_2B_Bayes = Rank_2B_Base12_Depurada25_Bayes,
        Rank_IMPE_2B_RF    = Rank_2B_Base12_Depurada25_RF
      ),
    by = c("iso3c", "year")
  ) %>%
  left_join(df_pib_raw %>% filter(year == 2024), by = c("iso3c", "year")) %>%
  mutate(
    PIB_Nominal_BnUSD        = round(PIB_Nominal_USD / 1e9, 2),
    PIB_PPA_BnUSD            = round(PIB_PPA_Const / 1e9, 2),
    Score100_2A_Bayes        = round(Score100_2A_Bayes, 2),
    Score100_2B_Bayes        = round(Score100_2B_Bayes, 2),
    Score100_2B_RF           = round(Score100_2B_RF, 2),
    Brecha_Rank_2A_vs_PIBNom = Rank_PIB_Nominal - Rank_IMPE_2A_Bayes,
    Brecha_Rank_2A_vs_PIBPPA = Rank_PIB_PPA     - Rank_IMPE_2A_Bayes,
    Perfil_Estructural_2A = case_when(
      is.na(Brecha_Rank_2A_vs_PIBNom) ~ "Sin dato PIB",
      Brecha_Rank_2A_vs_PIBNom >=  15 ~ "Sobre-rinde en Poder Estructural (+15 puestos vs PIB)",
      Brecha_Rank_2A_vs_PIBNom <= -15 ~ "Sub-rinde en Poder Estructural (-15 puestos vs PIB)",
      TRUE ~ "Alineado con tamaño PIB"
    )
  ) %>%
  arrange(Rank_IMPE_2A_Bayes)

# ==============================================================================
# 4. EXPORTACIÓN CONSOLIDADA A EXCEL PROFESIONAL
# ==============================================================================
wb <- createWorkbook()

agregar_hoja <- function(wb, nombre, df) {
  addWorksheet(wb, nombre)
  if (nrow(df) == 0) {
    writeData(wb, nombre, tibble(Aviso = "Sin datos: guarda tus 20 paneles país-año en LISTA_PANELES_20 (con iso3c, year y variables) y vuelve a ejecutar."))
  } else {
    writeDataTable(wb, nombre, df, tableStyle = "TableStyleMedium2", withFilter = TRUE)
    setColWidths(wb, nombre, cols = 1:ncol(df), widths = "auto")
    freezePane(wb, nombre, firstRow = TRUE)
  }
}

agregar_hoja(wb, "1_Resumen_Imputacion_Metodo",   df_diag_resumen)
agregar_hoja(wb, "2_Diagnostico_Vars_Z_KS",       df_diag_vars_z)
agregar_hoja(wb, "3_Subindices_Potencias_Clave",  df_subindices_resumen)
agregar_hoja(wb, "4_Validez_Externa_Cor_PIB",     df_validez_externa_cor)
agregar_hoja(wb, "5_Divergencias_IMPE_vs_PIB_24", df_div_2024)
agregar_hoja(wb, "6_PIB_Panel_1995_2024",         df_pib_raw)

saveWorkbook(wb, RUTA_SALIDA_EXCEL, overwrite = TRUE)

message("\n==============================================================================")
message("✔ DATA COMPLEMENTARIA GENERADA CON ÉXITO")
message("  1) Excel consolidado : ", normalizePath(RUTA_SALIDA_EXCEL, mustWork = FALSE))
if (file.exists(RUTA_SALIDA_SUB_CSV)) {
  message("  2) Subíndices largos : ", normalizePath(RUTA_SALIDA_SUB_CSV, mustWork = FALSE))
}
if (file.exists(RUTA_PANELES_RDS)) {
  message("  3) Paneles para plots: ", normalizePath(RUTA_PANELES_RDS, mustWork = FALSE))
}
message("==============================================================================")
