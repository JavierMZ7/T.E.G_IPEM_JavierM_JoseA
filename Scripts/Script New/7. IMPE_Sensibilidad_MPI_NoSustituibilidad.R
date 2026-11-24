# ==============================================================================
# PRUEBA DE SENSIBILIDAD NO COMPENSATORIA: ÍNDICE DE MAZZIOTTA-PARETO (MPI)
# Validación Empírica del Principio de No Sustituibilidad (Objetivo Específico 3)
# ==============================================================================
# Contrasta la agregación lineal del IMPE (CP1) frente a la agregación no
# compensatoria de Mazziotta-Pareto (MPI = M - S * cv) aplicada sobre los tres
# subíndices estructurales (Productivo, Financiero y Tecnológico) para:
#   - 2B_Base12_Depurada25_Bayes (índice óptimo principal, N = 54)
#   - 2B_Base12_Depurada25_RF    (control no paramétrico, N = 54)
#   - 2A_Base12_Completa_Bayes   (cobertura universal, N = 215)
#   - 2A_Base12_Completa_RF      (control universal, N = 215)
#
# INSUMOS REQUERIDOS (en el directorio de trabajo):
#   - IMPE_Subindices_20_Indices_Largo.csv
#   - IMPE_Comparativa_Maestra_20_Indices.xlsx
#
# SALIDAS GENERADAS:
#   - IMPE_Sensibilidad_MPI_NoSustituibilidad.xlsx (5 hojas listas para Cap. IV)
#   - Grafico_Sensibilidad_Lineal_vs_MPI_2024.png  (contraste visual 2B_Bayes)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PAQUETES Y CONFIGURACIÓN
# ------------------------------------------------------------------------------
paquetes <- c("readr", "readxl", "openxlsx", "dplyr", "tidyr", "stringr", "purrr", "ggplot2")
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) install.packages(faltantes, dependencies = TRUE)
invisible(lapply(paquetes, library, character.only = TRUE))

RUTA_SUB_CSV     <- "IMPE_Subindices_20_Indices_Largo.csv"
RUTA_MAESTRO_XLS <- "IMPE_Comparativa_Maestra_20_Indices.xlsx"
RUTA_SALIDA_XLS  <- "IMPE_Sensibilidad_MPI_NoSustituibilidad.xlsx"
RUTA_SALIDA_PNG  <- "Grafico_Sensibilidad_Lineal_vs_MPI_2024.png"

stopifnot("No se encontró IMPE_Subindices_20_Indices_Largo.csv." = file.exists(RUTA_SUB_CSV))
stopifnot("No se encontró IMPE_Comparativa_Maestra_20_Indices.xlsx." = file.exists(RUTA_MAESTRO_XLS))

INDICES_EVAL <- c(
  "2B_Base12_Depurada25_Bayes",
  "2B_Base12_Depurada25_RF",
  "2A_Base12_Completa_Bayes",
  "2A_Base12_Completa_RF"
)

PAISES_CLAVE <- c("USA", "JPN", "SGP", "CHE", "KOR", "GBR", "HKG", "DEU", "CHN", "RUS", "IND", "BRA", "VEN")

# ------------------------------------------------------------------------------
# 1. CARGA DE DATOS Y PESOS ENDÓGENOS DE SUBÍNDICES (CP1)
# ------------------------------------------------------------------------------
df_sub_largo <- read_csv(RUTA_SUB_CSV, show_col_types = FALSE) %>%
  filter(Indice %in% INDICES_EVAL)

df_pca <- read_excel(RUTA_MAESTRO_XLS, sheet = "4_PCA_Detalle_Completo") %>%
  filter(Indice %in% INDICES_EVAL) %>%
  mutate(
    Subindice = case_when(
      str_starts(Variable, "prod_") ~ "Productivo",
      str_starts(Variable, "fin_")  ~ "Financiero",
      str_starts(Variable, "tech_") ~ "Tecnologico",
      TRUE ~ "Otro"
    )
  )

# Pesos agregados de los 3 subíndices en cada modelo (suman 1.0000)
pesos_sub <- df_pca %>%
  group_by(Indice, Subindice) %>%
  summarise(Peso_Pilar = sum(Peso_CP1, na.rm = TRUE), .groups = "drop") %>%
  group_by(Indice) %>%
  mutate(Peso_Pilar = Peso_Pilar / sum(Peso_Pilar)) %>%
  ungroup()

df_scores_maestro <- read_excel(RUTA_MAESTRO_XLS, sheet = "9_Scores100_20_Indices")
df_ranks_maestro  <- read_excel(RUTA_MAESTRO_XLS, sheet = "11_Ranks_20_Indices")

# ------------------------------------------------------------------------------
# 2. FUNCIÓN DE CÁLCULO MAZZIOTTA-PARETO (MPI ESTÁNDAR Y MPI PONDERADO POR CP1)
# ------------------------------------------------------------------------------
# Siguiendo a Mazziotta y Pareto (2016) y OECD (2008):
#   1) Estandarización a media = 100 y DE = 10: z_ij = 100 + 10 * (x_ij - mu_j) / sd_j
#      (evita medias cercanas a 0 y garantiza estabilidad numérica de cv = S / M).
#   2) MPI = M - S * cv = M - (S^2 / M)  [penalización negativa por desequilibrio].
# Se estiman dos variantes:
#   - MPI_Estandar : pesos iguales (1/3 cada dimensión)
#   - MPI_Ponderado: pesos endógenos de CP1 por subíndice (~53.9% Tech, ~27.5% Prod, ~18.6% Fin)

calcular_mpi_indice <- function(nombre_idx) {
  sub_i <- df_sub_largo %>%
    filter(Indice == nombre_idx) %>%
    group_by(Subindice) %>%
    mutate(
      Z_MPI = 100 + 10 * ((Subindice_Z - mean(Subindice_Z, na.rm = TRUE)) /
                            sd(Subindice_Z, na.rm = TRUE))
    ) %>%
    ungroup()

  w_i <- pesos_sub %>%
    filter(Indice == nombre_idx) %>%
    pivot_wider(names_from = Subindice, values_from = Peso_Pilar)

  w_prod <- w_i$Productivo[1]
  w_fin  <- w_i$Financiero[1]
  w_tech <- w_i$Tecnologico[1]

  ancho <- sub_i %>%
    select(Indice, Familia, Metodo, iso3c, country, year, Subindice, Subindice_100, Z_MPI) %>%
    pivot_wider(
      names_from  = Subindice,
      values_from = c(Subindice_100, Z_MPI)
    ) %>%
    filter(
      !is.na(Z_MPI_Productivo),
      !is.na(Z_MPI_Financiero),
      !is.na(Z_MPI_Tecnologico)
    ) %>%
    rowwise() %>%
    mutate(
      # --- Variante A: MPI Estándar (pesos iguales 1/3) ---
      M_Est          = mean(c(Z_MPI_Productivo, Z_MPI_Financiero, Z_MPI_Tecnologico)),
      S_Est          = sqrt(mean((c(Z_MPI_Productivo, Z_MPI_Financiero, Z_MPI_Tecnologico) - M_Est)^2)),
      CV_Est         = S_Est / M_Est,
      Penalizacion_Est = S_Est * CV_Est,
      MPI_Estandar   = M_Est - Penalizacion_Est,

      # --- Variante B: MPI Ponderado por pesos CP1 ---
      M_Pond         = w_prod * Z_MPI_Productivo + w_fin * Z_MPI_Financiero + w_tech * Z_MPI_Tecnologico,
      S_Pond         = sqrt(
        w_prod * (Z_MPI_Productivo - M_Pond)^2 +
        w_fin  * (Z_MPI_Financiero - M_Pond)^2 +
        w_tech * (Z_MPI_Tecnologico - M_Pond)^2
      ),
      CV_Pond          = S_Pond / M_Pond,
      Penalizacion_Pond = S_Pond * CV_Pond,
      MPI_Ponderado    = M_Pond - Penalizacion_Pond
    ) %>%
    ungroup() %>%
    mutate(
      MPI_Ponderado_100 = 100 * (MPI_Ponderado - min(MPI_Ponderado, na.rm = TRUE)) /
        (max(MPI_Ponderado, na.rm = TRUE) - min(MPI_Ponderado, na.rm = TRUE)),
      MPI_Estandar_100  = 100 * (MPI_Estandar - min(MPI_Estandar, na.rm = TRUE)) /
        (max(MPI_Estandar, na.rm = TRUE) - min(MPI_Estandar, na.rm = TRUE))
    )

  # Unir con el IMPE Lineal (Score100 y Rank del Excel Maestro)
  col_sc <- paste0("Score100_", nombre_idx)
  col_rk <- paste0("Rank_", nombre_idx)

  sc_join <- df_scores_maestro %>%
    select(iso3c, year, IMPE_Lineal_100 = all_of(col_sc))
  rk_join <- df_ranks_maestro %>%
    select(iso3c, year, Rank_Lineal = all_of(col_rk))

  ancho %>%
    left_join(sc_join, by = c("iso3c", "year")) %>%
    left_join(rk_join, by = c("iso3c", "year")) %>%
    group_by(year) %>%
    mutate(
      Rank_MPI_Ponderado = min_rank(desc(MPI_Ponderado)),
      Rank_MPI_Estandar  = min_rank(desc(MPI_Estandar)),
      # Negativo = país pierde posiciones al penalizar asimetría interdimensional
      Delta_Rank_Pond    = Rank_Lineal - Rank_MPI_Ponderado,
      Delta_Rank_Est     = Rank_Lineal - Rank_MPI_Estandar
    ) %>%
    ungroup()
}

message("Calculando sensibilidad Mazziotta-Pareto (MPI) para los 4 modelos clave...")
df_mpi_todos <- map_dfr(INDICES_EVAL, calcular_mpi_indice)

# ------------------------------------------------------------------------------
# 3. TABLAS DE RESUMEN Y DIAGNÓSTICOS PARA EL CAPÍTULO IV
# ------------------------------------------------------------------------------

# Hoja 1: Resumen de concordancia Lineal vs. MPI (panel completo 1995-2024 y corte 2024)
resumen_concordancia <- df_mpi_todos %>%
  group_by(Indice, Familia, Metodo) %>%
  summarise(
    N_Obs_Panel              = n(),
    Spearman_Lineal_vs_MPI_P = round(cor(IMPE_Lineal_100, MPI_Ponderado, method = "spearman", use = "complete.obs"), 4),
    Kendall_Lineal_vs_MPI_P  = round(cor(IMPE_Lineal_100, MPI_Ponderado, method = "kendall",  use = "complete.obs"), 4),
    Spearman_Lineal_vs_MPI_E = round(cor(IMPE_Lineal_100, MPI_Estandar,  method = "spearman", use = "complete.obs"), 4),
    Penalizacion_Media_Pond  = round(mean(Penalizacion_Pond, na.rm = TRUE), 3),
    Penalizacion_Max_Pond    = round(max(Penalizacion_Pond, na.rm = TRUE), 3),
    Desplaz_Abs_Medio_Rank   = round(mean(abs(Delta_Rank_Pond), na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  left_join(
    df_mpi_todos %>%
      filter(year == 2024) %>%
      group_by(Indice) %>%
      summarise(
        N_Paises_2024             = n(),
        Spearman_2024_Lineal_MPIP = round(cor(IMPE_Lineal_100, MPI_Ponderado, method = "spearman", use = "complete.obs"), 4),
        Spearman_2024_Lineal_MPIE = round(cor(IMPE_Lineal_100, MPI_Estandar,  method = "spearman", use = "complete.obs"), 4),
        Max_Caida_Rank_2024       = min(Delta_Rank_Pond, na.rm = TRUE),
        Max_Subida_Rank_2024      = max(Delta_Rank_Pond, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "Indice"
  )

# Función auxiliar para limpiar columnas en las tablas de corte 2024
formatear_corte_2024 <- function(df, nombre_idx) {
  df %>%
    filter(Indice == nombre_idx, year == 2024) %>%
    transmute(
      iso3c,
      country,
      Sub_Prod_100       = round(Subindice_100_Productivo, 2),
      Sub_Fin_100        = round(Subindice_100_Financiero, 2),
      Sub_Tech_100       = round(Subindice_100_Tecnologico, 2),
      IMPE_Lineal_100    = round(IMPE_Lineal_100, 2),
      MPI_Ponderado_100  = round(MPI_Ponderado_100, 2),
      MPI_Estandar_100   = round(MPI_Estandar_100, 2),
      Penalizacion_MPI_P = round(Penalizacion_Pond, 3),
      Penalizacion_MPI_E = round(Penalizacion_Est, 3),
      Rank_Lineal,
      Rank_MPI_Ponderado,
      Delta_Rank_Pond,
      Rank_MPI_Estandar,
      Delta_Rank_Est
    ) %>%
    arrange(Rank_Lineal)
}

tab_2B_2024 <- formatear_corte_2024(df_mpi_todos, "2B_Base12_Depurada25_Bayes")
tab_2A_2024 <- formatear_corte_2024(df_mpi_todos, "2A_Base12_Completa_Bayes")

# Hoja 4: Potencias clave (2B_Bayes + Venezuela desde 2A_Bayes)
tab_potencias_2024 <- bind_rows(
  tab_2B_2024 %>% filter(iso3c %in% PAISES_CLAVE) %>% mutate(Modelo = "2B_Bayes", .before = 1),
  tab_2A_2024 %>% filter(iso3c == "VEN")          %>% mutate(Modelo = "2A_Bayes", .before = 1)
) %>%
  arrange(Modelo == "2A_Bayes", Rank_Lineal)

# Hoja 5: Top 15 países más penalizados por asimetría interdimensional en 2024
tab_top_penalizados <- bind_rows(
  tab_2B_2024 %>%
    arrange(desc(Penalizacion_MPI_P)) %>%
    slice_head(n = 15) %>%
    mutate(Modelo = "2B_Bayes", .before = 1),
  tab_2A_2024 %>%
    arrange(desc(Penalizacion_MPI_P)) %>%
    slice_head(n = 15) %>%
    mutate(Modelo = "2A_Bayes", .before = 1)
)

# ------------------------------------------------------------------------------
# 4. EXPORTACIÓN A EXCEL (5 HOJAS FORMATEADAS)
# ------------------------------------------------------------------------------
wb <- createWorkbook()
estilo_hdr <- createStyle(
  fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
  fgFill = "#1B365D", halign = "center", textDecoration = "bold"
)

agregar_hoja <- function(wb, nombre, datos) {
  addWorksheet(wb, nombre)
  writeData(wb, nombre, datos, headerStyle = estilo_hdr)
  setColWidths(wb, nombre, cols = 1:ncol(datos), widths = "auto")
  freezePane(wb, nombre, firstRow = TRUE)
}

agregar_hoja(wb, "1_Resumen_Concordancia_MPI", resumen_concordancia)
agregar_hoja(wb, "2_Comparativa_2B_Bayes_2024", tab_2B_2024)
agregar_hoja(wb, "3_Comparativa_2A_Bayes_2024", tab_2A_2024)
agregar_hoja(wb, "4_Potencias_Clave_MPI_2024",  tab_potencias_2024)
agregar_hoja(wb, "5_Top_Penalizados_Asimetria", tab_top_penalizados)

saveWorkbook(wb, RUTA_SALIDA_XLS, overwrite = TRUE)
message("✔ Excel exportado correctamente: ", RUTA_SALIDA_XLS)

# ------------------------------------------------------------------------------
# 5. GRÁFICO DE SENSIBILIDAD: RANKING LINEAL vs. RANKING MPI (2B_Bayes 2024)
# ------------------------------------------------------------------------------
p_mpi <- ggplot(tab_2B_2024, aes(x = Rank_Lineal, y = Rank_MPI_Ponderado)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#6B7280", linewidth = 0.7) +
  geom_point(aes(size = Penalizacion_MPI_P, color = Delta_Rank_Pond), alpha = 0.85) +
  geom_text(
    data = tab_2B_2024 %>% filter(iso3c %in% PAISES_CLAVE | abs(Delta_Rank_Pond) >= 3),
    aes(label = iso3c),
    vjust = -0.8, size = 3, fontface = "bold", color = "#111827", check_overlap = TRUE
  ) +
  scale_color_gradient2(
    low = "#B91C1C", mid = "#4B5563", high = "#047857", midpoint = 0,
    name = "Cambio de\nRanking (Δ)"
  ) +
  scale_size_continuous(name = "Penalización\nMPI (S·cv)", range = c(2, 7)) +
  scale_x_reverse() +
  scale_y_reverse() +
  labs(
    title = "Sensibilidad No Compensatoria: IMPE Lineal vs. Mazziotta-Pareto (2B_Bayes, 2024)",
    subtitle = sprintf(
      "N = 54 Estados | Correlación de Spearman ρ = %.4f | Puntos bajo la diagonal pierden posiciones por asimetría",
      resumen_concordancia$Spearman_2024_Lineal_MPIP[resumen_concordancia$Indice == "2B_Base12_Depurada25_Bayes"]
    ),
    x = "Posición en IMPE Lineal (CP1)",
    y = "Posición en IMPE Penalizado (MPI Ponderado)",
    caption = "Fuente: Elaboración propia con base en IMPE_Subindices_20_Indices_Largo.csv"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", color = "#1B365D"),
    plot.subtitle = element_text(color = "#374151", size = 9.5),
    legend.position = "right"
  )

ggsave(RUTA_SALIDA_PNG, plot = p_mpi, width = 9.5, height = 6.5, dpi = 300, bg = "white")
message("✔ Gráfico exportado correctamente: ", RUTA_SALIDA_PNG)

# Resumen rápido en consola
print(resumen_concordancia)
print(tab_potencias_2024 %>% select(Modelo, iso3c, IMPE_Lineal_100, MPI_Ponderado_100, Penalizacion_MPI_P, Rank_Lineal, Rank_MPI_Ponderado, Delta_Rank_Pond))
