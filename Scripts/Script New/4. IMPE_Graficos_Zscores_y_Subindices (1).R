# ==============================================================================
# SCRIPT ESPECÍFICO DE GRÁFICOS — IMPE (1995-2024)
#   1. Distribución de la imputación en Z-scores (1 plot por cada método):
#        - Distribución del índice (imputado vs. no imputado)
#        - Distribución de cada variable (imputada vs. no imputada)
#   2. Subíndices (Productivo, Financiero, Tecnológico) en los 20 índices
#      para China (CHN), Estados Unidos (USA) y Rusia (RUS).
# ==============================================================================
# REQUISITO PARA VARIABLES Y SUBÍNDICES POR PAÍS:
#   El Excel maestro solo trae el índice agregado (no las 16 variables por país-año).
#   Para que se generen los plots de variables (1B) y subíndices por país (2),
#   corre este script en la misma sesión de R donde tengas `LISTA_PANELES_20`,
#   o agrega al final de tu script maestro:
#       saveRDS(LISTA_PANELES_20, "paneles_20_indices.rds")
#   (También detecta automáticamente archivos `<Indice>.parquet` en CARPETA_CACHE_PARQUET).
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PAQUETES Y CONFIGURACIÓN
# ------------------------------------------------------------------------------
paquetes <- c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr", "forcats",
  "ggplot2", "scales", "patchwork"
)

faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) install.packages(faltantes, dependencies = TRUE)
invisible(lapply(paquetes, library, character.only = TRUE))

RUTA_EXCEL_MAESTRO    <- "IMPE_Comparativa_Maestra_20_Indices.xlsx"
RUTA_PANELES_RDS      <- "paneles_20_indices.rds"
CARPETA_CACHE_PARQUET <- "cache_imputacion"
CARPETA_SALIDA        <- "graficos_Zscores_y_Subindices"

# Familia base para graficar la distribución Z de todas las variables (1A tiene las 16)
FAMILIA_VARS_Z <- "1A_Base16_Completa"

# Países objetivo para subíndices
PAISES_TOP3 <- c(
  CHN = "China",
  USA = "Estados Unidos",
  RUS = "Rusia"
)

dir.create(CARPETA_SALIDA, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. CATÁLOGOS Y TEMA VISUAL
# ------------------------------------------------------------------------------
ORDEN_FAMILIAS <- c(
  "1A_Base16_Completa",
  "1B_Base16_Depurada25",
  "2A_Base12_Completa",
  "2B_Base12_Depurada25"
)

ETIQUETAS_FAMILIAS <- c(
  "1A_Base16_Completa"   = "1A · Base 16 Completa",
  "1B_Base16_Depurada25" = "1B · Base 16 Depurada (≤25% NA)",
  "2A_Base12_Completa"   = "2A · Base 12 Completa",
  "2B_Base12_Depurada25" = "2B · Base 12 Depurada (≤25% NA)"
)

ORDEN_METODOS <- c(
  "1_Raw_Pairwise",
  "2_Interp_Max3",
  "3_MICE_PMM",
  "4_MICE_Bayes",
  "5_MICE_RF"
)

METODOS_IMPUTACION <- c(
  "2_Interp_Max3",
  "3_MICE_PMM",
  "4_MICE_Bayes",
  "5_MICE_RF"
)

SUFIJOS_METODO <- c(
  "2_Interp_Max3" = "Interp3",
  "3_MICE_PMM"    = "PMM",
  "4_MICE_Bayes"  = "Bayes",
  "5_MICE_RF"     = "RF"
)

ETIQUETAS_METODOS <- c(
  "1_Raw_Pairwise" = "Raw (Pairwise)",
  "2_Interp_Max3"  = "Interpolación (≤3 años)",
  "3_MICE_PMM"     = "MICE · PMM",
  "4_MICE_Bayes"   = "MICE · Bayesiano",
  "5_MICE_RF"      = "MICE · Random Forest"
)

PALETA_METODOS <- c(
  "Raw (Pairwise)"          = "#2C3E50",
  "Interpolación (≤3 años)" = "#7F8C8D",
  "MICE · PMM"              = "#E67E22",
  "MICE · Bayesiano"        = "#2980B9",
  "MICE · Random Forest"    = "#27AE60"
)

PALETA_PAISES <- c(
  "China"          = "#C0392B",
  "Estados Unidos" = "#1F618D",
  "Rusia"          = "#1E8449"
)

clasificar_subindice <- function(variable) {
  case_when(
    str_starts(variable, "prod_") ~ "Productivo",
    str_starts(variable, "fin_")  ~ "Financiero",
    str_starts(variable, "tech_") ~ "Tecnológico",
    TRUE ~ "Otro"
  )
}

tema_impe <- function(base_size = 10.5) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 2.5, color = "#17202A", margin = margin(b = 4)),
      plot.subtitle    = element_text(size = base_size - 0.5, color = "#566573", margin = margin(b = 10)),
      plot.caption     = element_text(size = base_size - 2, color = "#7F8C8D", hjust = 0, margin = margin(t = 8)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#EAECEE", linewidth = 0.4),
      strip.background = element_rect(fill = "#F4F6F7", color = NA),
      strip.text       = element_text(face = "bold", color = "#1C2833", size = base_size - 0.5),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = base_size - 1),
      axis.title       = element_text(face = "bold", color = "#2C3E50")
    )
}

guardar_png <- function(plot_obj, nombre_archivo, width = 13, height = 8.5, dpi = 300) {
  ruta <- file.path(CARPETA_SALIDA, nombre_archivo)
  ggsave(filename = ruta, plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white")
  message("✔ Guardado: ", ruta)
}

# ------------------------------------------------------------------------------
# 2. CARGA DE DATOS (EXCEL MAESTRO + PANELES DE VARIABLES)
# ------------------------------------------------------------------------------
stopifnot("No se encontró IMPE_Comparativa_Maestra_20_Indices.xlsx." = file.exists(RUTA_EXCEL_MAESTRO))

df_estadisticos <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "1_Estadisticos_Indices") %>%
  mutate(
    Familia       = factor(Familia, levels = ORDEN_FAMILIAS),
    Metodo        = factor(Metodo, levels = ORDEN_METODOS),
    Familia_Label = factor(ETIQUETAS_FAMILIAS[as.character(Familia)], levels = ETIQUETAS_FAMILIAS),
    Metodo_Label  = factor(ETIQUETAS_METODOS[as.character(Metodo)], levels = ETIQUETAS_METODOS)
  ) %>%
  arrange(Familia, Metodo)

ORDEN_INDICES <- df_estadisticos$Indice

df_pca <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "4_PCA_Detalle_Completo") %>%
  mutate(Subindice = factor(clasificar_subindice(Variable), levels = c("Productivo", "Financiero", "Tecnológico")))

df_scoresZ <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "10_ScoresZ_20_Indices") %>%
  pivot_longer(cols = starts_with("ScoreZ_"), names_to = "Indice", values_to = "Score_Z") %>%
  mutate(Indice = str_remove(Indice, "^ScoreZ_")) %>%
  left_join(df_estadisticos %>% select(Indice, Familia, Metodo, Familia_Label, Metodo_Label), by = "Indice") %>%
  mutate(Indice = factor(Indice, levels = ORDEN_INDICES))

cargar_paneles <- function() {
  if (exists("LISTA_PANELES_20", envir = .GlobalEnv)) {
    message("✔ Usando LISTA_PANELES_20 desde el Environment de R.")
    return(get("LISTA_PANELES_20", envir = .GlobalEnv))
  }
  if (file.exists(RUTA_PANELES_RDS)) {
    message("✔ Cargando paneles desde ", RUTA_PANELES_RDS)
    return(readRDS(RUTA_PANELES_RDS))
  }
  if (dir.exists(CARPETA_CACHE_PARQUET) && requireNamespace("arrow", quietly = TRUE)) {
    archivos <- list.files(CARPETA_CACHE_PARQUET, pattern = "\\.parquet$", full.names = TRUE)
    nombres  <- tools::file_path_sans_ext(basename(archivos))
    validos  <- nombres %in% ORDEN_INDICES
    if (any(validos)) {
      message("✔ Cargando ", sum(validos), " paneles desde ", CARPETA_CACHE_PARQUET)
      return(setNames(lapply(archivos[validos], arrow::read_parquet), nombres[validos]))
    }
  }
  return(NULL)
}

paneles_20 <- cargar_paneles()

# ==============================================================================
# BLOQUE 1A: DISTRIBUCIÓN EN Z-SCORES DEL ÍNDICE (1 PLOT POR CADA MÉTODO)
# ==============================================================================
message("\n--- [1A] Generando distribución en Z-scores del ÍNDICE por cada método ---")

mapa_raw_Z <- df_scoresZ %>%
  filter(Metodo == "1_Raw_Pairwise") %>%
  transmute(iso3c, year, Familia, ScoreZ_Raw = Score_Z, Tiene_Real = !is.na(Score_Z))

plots_indice_Z <- list()

for (met in METODOS_IMPUTACION) {
  etq <- ETIQUETAS_METODOS[[met]]

  df_m <- df_scoresZ %>%
    filter(Metodo == met, !is.na(Score_Z)) %>%
    left_join(mapa_raw_Z, by = c("iso3c", "year", "Familia"))

  df_curvas <- bind_rows(
    df_m %>% filter(!is.na(ScoreZ_Raw)) %>%
      transmute(Familia_Label, Score_Z = ScoreZ_Raw, Grupo = "No imputado (Real en Raw)"),
    df_m %>% filter(!Tiene_Real) %>%
      transmute(Familia_Label, Score_Z = Score_Z, Grupo = "Solo imputados (era NA en Raw)"),
    df_m %>%
      transmute(Familia_Label, Score_Z = Score_Z, Grupo = paste0("Índice imputado completo (", etq, ")"))
  ) %>%
    mutate(
      Grupo = factor(
        Grupo,
        levels = c(
          "No imputado (Real en Raw)",
          "Solo imputados (era NA en Raw)",
          paste0("Índice imputado completo (", etq, ")")
        )
      )
    )

  df_med <- df_curvas %>%
    group_by(Familia_Label, Grupo) %>%
    summarise(Mediana = median(Score_Z, na.rm = TRUE), .groups = "drop")

  colores <- setNames(c("#1B4F72", "#E67E22", "#1E8449"), levels(df_curvas$Grupo))

  p_idx <- ggplot(df_curvas, aes(x = Score_Z, color = Grupo, fill = Grupo)) +
    geom_density(alpha = 0.22, linewidth = 0.9) +
    geom_vline(data = df_med, aes(xintercept = Mediana, color = Grupo),
               linetype = "dashed", linewidth = 0.65, show.legend = FALSE) +
    facet_wrap(~ Familia_Label, ncol = 2, scales = "free_y") +
    scale_color_manual(values = colores, name = "Distribución") +
    scale_fill_manual(values = colores, name = "Distribución") +
    labs(
      title    = paste0("Distribución del Índice en Z-Scores: No Imputado vs. ", etq),
      subtitle = "Comparación entre observaciones reales (Raw), rescatadas (imputadas) y el índice completo por familia",
      x        = "Score IMPE (Z-score)",
      y        = "Densidad",
      caption  = "Líneas punteadas verticales = mediana de cada distribución | Fuente: hoja 10_ScoresZ_20_Indices."
    ) +
    tema_impe()

  plots_indice_Z[[met]] <- p_idx
  guardar_png(p_idx, paste0("01_Zscore_indice_", met, ".png"), width = 13, height = 8.5)
}

# ==============================================================================
# BLOQUE 1B: DISTRIBUCIÓN EN Z-SCORES DE CADA VARIABLE (1 PLOT POR CADA MÉTODO)
# ==============================================================================
message("\n--- [1B] Generando distribución en Z-scores por VARIABLE para cada método ---")

if (!is.null(paneles_20)) {
  idx_raw_ref <- paste0(FAMILIA_VARS_Z, "_Raw")
  if (!idx_raw_ref %in% names(paneles_20)) {
    warning("No se encontró '", idx_raw_ref, "' dentro de los paneles cargados.")
  } else {
    df_raw <- paneles_20[[idx_raw_ref]]
    vars_modelo <- intersect(unique(df_pca$Variable), names(df_raw))

    # Estandarizar Z usando media y DE reales (Raw) para comparar en exactamente la misma escala
    params_raw <- df_raw %>%
      summarise(across(all_of(vars_modelo), list(mu = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE))))

    aplicar_z_raw <- function(df_in) {
      out <- df_in
      for (v in vars_modelo) {
        mu_v <- params_raw[[paste0(v, "_mu")]]
        sd_v <- params_raw[[paste0(v, "_sd")]]
        if (!is.na(sd_v) && sd_v > 0) out[[v]] <- (out[[v]] - mu_v) / sd_v
      }
      out
    }

    mascara_na <- df_raw %>%
      select(iso3c, year, all_of(vars_modelo)) %>%
      pivot_longer(cols = all_of(vars_modelo), names_to = "Variable", values_to = "Val_Raw") %>%
      transmute(iso3c, year, Variable, Era_NA = is.na(Val_Raw))

    for (met in METODOS_IMPUTACION) {
      etq     <- ETIQUETAS_METODOS[[met]]
      idx_met <- paste0(FAMILIA_VARS_Z, "_", SUFIJOS_METODO[[met]])
      if (!idx_met %in% names(paneles_20)) {
        warning("Falta el panel '", idx_met, "'; se omite.")
        next
      }

      df_vars_z <- aplicar_z_raw(paneles_20[[idx_met]]) %>%
        select(iso3c, year, all_of(vars_modelo)) %>%
        pivot_longer(cols = all_of(vars_modelo), names_to = "Variable", values_to = "Z_Valor") %>%
        left_join(mascara_na, by = c("iso3c", "year", "Variable")) %>%
        filter(!is.na(Z_Valor)) %>%
        mutate(
          Origen = factor(
            ifelse(Era_NA, paste0("Imputado (", etq, ")"), "No imputado (Real en Raw)"),
            levels = c("No imputado (Real en Raw)", paste0("Imputado (", etq, ")"))
          )
        )

      p_vars <- ggplot(df_vars_z, aes(x = Z_Valor, fill = Origen, color = Origen)) +
        geom_density(alpha = 0.30, linewidth = 0.7) +
        facet_wrap(~ Variable, scales = "free", ncol = 4) +
        scale_fill_manual(values = c("#1B4F72", "#E67E22"), name = "Origen del dato") +
        scale_color_manual(values = c("#1B4F72", "#D35400"), name = "Origen del dato") +
        coord_cartesian(xlim = c(-4, 4)) +
        labs(
          title    = paste0("Distribución en Z-Scores por Variable: No Imputado vs. ", etq),
          subtitle = paste0("Familia ", FAMILIA_VARS_Z, " (16 variables) | Estandarización Z sobre media y DE reales de Raw"),
          x        = "Z-score de la variable (acotado a [-4, 4])",
          y        = "Densidad"
        ) +
        tema_impe(base_size = 9)

      guardar_png(p_vars, paste0("02_Zscore_variables_", met, ".png"), width = 16, height = 11)

      # Gráfico combinado del método: arriba el índice Z y abajo las 16 variables Z
      p_comb <- plots_indice_Z[[met]] / p_vars + plot_layout(heights = c(1, 1.45))
      guardar_png(p_comb, paste0("03_Zscore_combinado_", met, ".png"), width = 16, height = 17)
    }
  }
} else {
  message(
    "ℹ AVISO (1B - Variables en Z-scores): No se encontró 'paneles_20_indices.rds' ni 'LISTA_PANELES_20'.\n",
    "  Guarda tus 20 paneles con: saveRDS(LISTA_PANELES_20, 'paneles_20_indices.rds') para activar 02_* y 03_*."
  )
}

# ==============================================================================
# BLOQUE 2: SUBÍNDICES EN CADA ÍNDICE — CHINA, ESTADOS UNIDOS Y RUSIA
# ==============================================================================
message("\n--- [2] Generando gráficos de Subíndices en los 20 índices (China, EE.UU. y Rusia) ---")

if (!is.null(paneles_20)) {
  calcular_subindices <- function(nombre_indice, df_panel) {
    pesos_idx <- df_pca %>%
      filter(Indice == nombre_indice) %>%
      group_by(Subindice) %>%
      mutate(Peso_Sub = Peso_CP1 / sum(Peso_CP1, na.rm = TRUE)) %>%
      ungroup()

    vars_disp <- intersect(pesos_idx$Variable, names(df_panel))
    if (length(vars_disp) == 0) return(NULL)

    df_panel %>%
      mutate(across(all_of(vars_disp), ~ as.numeric(scale(.x)))) %>%
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
        Indice = nombre_indice
      ) %>%
      ungroup()
  }

  df_sub_todos <- imap_dfr(paneles_20, ~ calcular_subindices(.y, .x)) %>%
    left_join(df_estadisticos %>% select(Indice, Familia, Metodo, Familia_Label, Metodo_Label), by = "Indice") %>%
    mutate(Indice = factor(Indice, levels = ORDEN_INDICES))

  df_sub_top3 <- df_sub_todos %>%
    filter(iso3c %in% names(PAISES_TOP3), !is.na(Subindice_100)) %>%
    mutate(Pais_Label = factor(PAISES_TOP3[iso3c], levels = unname(PAISES_TOP3)))

  # 2A: Un plot individual por país (China, Estados Unidos, Rusia) mostrando los 3 subíndices en los 20 índices
  for (cod in names(PAISES_TOP3)) {
    nom <- PAISES_TOP3[[cod]]
    df_p <- df_sub_top3 %>% filter(iso3c == cod)

    p_ind <- ggplot(df_p, aes(x = year, y = Subindice_100, color = Metodo_Label, group = Indice)) +
      geom_line(linewidth = 0.85, alpha = 0.9) +
      facet_grid(Subindice ~ Familia_Label) +
      scale_color_manual(values = PALETA_METODOS, name = "Método") +
      scale_x_continuous(breaks = seq(1995, 2024, by = 10)) +
      labs(
        title    = paste0(nom, " (", cod, "): Evolución de los 3 Subíndices en los 20 Índices (1995–2024)"),
        subtitle = "Pilares Productivo, Financiero y Tecnológico ponderados con Peso_CP1 de cada índice (escala 0–100)",
        x        = "Año",
        y        = "Score del Subíndice (0–100)"
      ) +
      tema_impe(base_size = 9.5)

    guardar_png(p_ind, paste0("04_subindices_20_indices_", cod, ".png"), width = 14.5, height = 9.5)
  }

  # 2B: Plot conjunto comparando China, Estados Unidos y Rusia (evolución 1995-2024 con banda entre métodos)
  df_banda_top3 <- df_sub_top3 %>%
    group_by(Pais_Label, year, Subindice, Familia_Label) %>%
    summarise(
      Min_Sub = min(Subindice_100, na.rm = TRUE),
      Max_Sub = max(Subindice_100, na.rm = TRUE),
      Med_Sub = median(Subindice_100, na.rm = TRUE),
      .groups = "drop"
    )

  p_evol_3 <- ggplot(df_banda_top3, aes(x = year, y = Med_Sub, color = Pais_Label, fill = Pais_Label)) +
    geom_ribbon(aes(ymin = Min_Sub, ymax = Max_Sub), alpha = 0.18, color = NA) +
    geom_line(linewidth = 1.05) +
    facet_grid(Subindice ~ Familia_Label) +
    scale_color_manual(values = PALETA_PAISES, name = "País") +
    scale_fill_manual(values = PALETA_PAISES, name = "País") +
    scale_x_continuous(breaks = seq(1995, 2024, by = 10)) +
    labs(
      title    = "China, Estados Unidos y Rusia: Comparativa de Subíndices en los 20 Índices (1995–2024)",
      subtitle = "Línea = mediana entre los 5 métodos | Banda sombreada = rango [Mín–Máx] entre métodos en cada familia",
      x        = "Año",
      y        = "Score del Subíndice (0–100)"
    ) +
    tema_impe(base_size = 9.5)

  guardar_png(p_evol_3, "05_comparativa_subindices_CHN_USA_RUS_evolucion.png", width = 15, height = 9.5)

  # 2C: Comparativa directa de China, Estados Unidos y Rusia en CADA UNO DE LOS 20 ÍNDICES (último año)
  anio_max <- max(df_sub_top3$year, na.rm = TRUE)

  p_20_idx <- ggplot(df_sub_top3 %>% filter(year == anio_max),
                     aes(x = fct_rev(Indice), y = Subindice_100, color = Pais_Label, group = Pais_Label)) +
    geom_line(linewidth = 0.85, alpha = 0.75) +
    geom_point(size = 2.6) +
    facet_wrap(~ Subindice, ncol = 3) +
    coord_flip() +
    scale_color_manual(values = PALETA_PAISES, name = "País") +
    labs(
      title    = paste0("China, Estados Unidos y Rusia: Subíndices en Cada Uno de los 20 Índices (Año ", anio_max, ")"),
      subtitle = "Contraste directo por índice en las dimensiones Productiva, Financiera y Tecnológica",
      x        = NULL,
      y        = "Score del Subíndice (0–100)"
    ) +
    tema_impe(base_size = 9.5)

  guardar_png(p_20_idx, paste0("06_comparativa_subindices_CHN_USA_RUS_20_indices_", anio_max, ".png"), width = 15.5, height = 9.5)

} else {
  message(
    "ℹ AVISO (Bloque 2 - Subíndices de China, EE.UU. y Rusia): No se encontró 'paneles_20_indices.rds' ni 'LISTA_PANELES_20'.\n",
    "  Guarda tu lista de 20 paneles al final de tu script maestro con:\n",
    "      saveRDS(LISTA_PANELES_20, 'paneles_20_indices.rds')\n",
    "  y vuelve a ejecutar este script."
  )
}

message("\n✔ Listo. Gráficos guardados en: ", normalizePath(CARPETA_SALIDA, mustWork = FALSE))
