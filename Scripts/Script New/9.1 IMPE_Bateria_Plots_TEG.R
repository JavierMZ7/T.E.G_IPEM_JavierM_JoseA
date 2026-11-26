# ==============================================================================
# BATERÍA MAESTRA DE GRÁFICOS PARA PUBLICACIÓN DEL T.E.G. — IMPE (1995-2024)
# Escuela de Economía — Universidad Central de Venezuela (UCV)
# ==============================================================================
# Genera 12 figuras en alta resolución (300 dpi) con identidad visual unificada,
# numeradas secuencialmente para insertarse en cada sección del Capítulo IV:
#
#   BLOQUE A — VALIDACIÓN METODOLÓGICA, PODA Y SELECCIÓN DEL ÍNDICE ÓPTIMO
#     Fig_01_Seleccion_Indice_Optimo_20_Modelos.png
#     Fig_02_Diagnostico_Imputacion_KS_vs_RatioSD.png
#     Fig_03_Poda_Variables_Pesos_CP1.png
#     Fig_04_Participacion_Subindices_20_Indices.png
#
#   BLOQUE B — SUBÍNDICES Y PRINCIPIO DE NO SUSTITUIBILIDAD (OBJ. ESP. 3)
#     Fig_05_Evolucion_Subindices_Potencias_1995_2024.png
#     Fig_06_Perfil_Asimetria_Subindices_2024.png
#     Fig_07_Sensibilidad_Lineal_vs_MPI_2024.png
#
#   BLOQUE C — JERARQUÍA GLOBAL, ESCALA VS. DENSIDAD, CHINA Y PIB (OBJ. ESP. 4)
#     Fig_08_Trayectoria_Historica_IMPE_1995_2024.png
#     Fig_09_Bandas_Incertidumbre_InterMetodo_Top20_2024.png
#     Fig_10_Validez_Externa_IMPE_vs_PIB_Nominal_2024.png
#     Fig_11_Brechas_Estructurales_IMPE_vs_PIB_2024.png
#     Fig_12_Caso_China_Escala_Base16_vs_Densidad_Base12.png
#
# INSUMOS REQUERIDOS (en el directorio de trabajo):
#   - IMPE_Comparativa_Maestra_20_Indices.xlsx
#   - IMPE_Data_Complementaria_Cap4.xlsx
#   - IMPE_Sensibilidad_MPI_NoSustituibilidad.xlsx (opcional para Fig_07)
#   - IMPE_Subindices_20_Indices_Largo.csv         (opcional para serie anual Fig_05)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PAQUETES, PALETA Y TEMA EDITORIAL DEL T.E.G.
# ------------------------------------------------------------------------------
paquetes <- c("readxl", "readr", "dplyr", "tidyr", "stringr", "ggplot2", "scales", "ggrepel")
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) install.packages(faltantes, dependencies = TRUE)
invisible(lapply(paquetes, library, character.only = TRUE))

RUTA_MAESTRO <- "IMPE_Comparativa_Maestra_20_Indices.xlsx"
RUTA_COMP    <- "IMPE_Data_Complementaria_Cap4.xlsx"
RUTA_MPI     <- "IMPE_Sensibilidad_MPI_NoSustituibilidad.xlsx"
RUTA_SUB_CSV <- "IMPE_Subindices_20_Indices_Largo.csv"
DIR_SALIDA   <- "Figuras_TEG_Cap4"

stopifnot("No se encontró IMPE_Comparativa_Maestra_20_Indices.xlsx" = file.exists(RUTA_MAESTRO))
stopifnot("No se encontró IMPE_Data_Complementaria_Cap4.xlsx"        = file.exists(RUTA_COMP))
if (!dir.exists(DIR_SALIDA)) dir.create(DIR_SALIDA, recursive = TRUE)

# Paleta institucional sobria (apto para impresión y PDF académico)
COL_NAVY    <- "#1B365D"
COL_TEAL    <- "#0F766E"
COL_AMBER   <- "#D97706"
COL_CRIMSON <- "#B91C1C"
COL_SLATE   <- "#475569"
COL_LIGHT   <- "#94A3B8"

PAL_FAMILIAS <- c(
  "1A_Base16_Completa"   = "#94A3B8",
  "1B_Base16_Depurada25" = "#D97706",
  "2A_Base12_Completa"   = "#0F766E",
  "2B_Base12_Depurada25" = "#1B365D"
)

PAL_SUB <- c(
  "Tecnologico" = "#1B365D",
  "Productivo"  = "#0F766E",
  "Financiero"  = "#D97706"
)

tema_teg <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", color = COL_NAVY, size = base_size + 2, margin = margin(b = 4)),
      plot.subtitle    = element_text(color = "#334155", size = base_size - 1, margin = margin(b = 10)),
      plot.caption     = element_text(color = "#64748B", size = base_size - 2.5, hjust = 0, margin = margin(t = 8)),
      axis.title       = element_text(face = "bold", color = "#1E293B", size = base_size - 0.5),
      axis.text        = element_text(color = "#334155"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E2E8F0", linewidth = 0.4),
      legend.title     = element_text(face = "bold", size = base_size - 1, color = "#1E293B"),
      legend.text      = element_text(size = base_size - 1.5),
      strip.text       = element_text(face = "bold", color = COL_NAVY, size = base_size - 0.5),
      strip.background = element_rect(fill = "#F1F5F9", color = NA)
    )
}

guardar_fig <- function(plot_obj, nombre_archivo, w = 10, h = 6.2) {
  ruta <- file.path(DIR_SALIDA, nombre_archivo)
  ggsave(ruta, plot = plot_obj, width = w, height = h, dpi = 300, bg = "white")
  message("✔ Exportada: ", ruta)
}

# ------------------------------------------------------------------------------
# 1. CARGA DE BASES
# ------------------------------------------------------------------------------
df_est    <- read_excel(RUTA_MAESTRO, sheet = "1_Estadisticos_Indices")
df_pca    <- read_excel(RUTA_MAESTRO, sheet = "4_PCA_Detalle_Completo")
df_rangos <- read_excel(RUTA_MAESTRO, sheet = "8_Rangos_Pais_Familia")
df_scores <- read_excel(RUTA_MAESTRO, sheet = "9_Scores100_20_Indices")
df_ranks  <- read_excel(RUTA_MAESTRO, sheet = "11_Ranks_20_Indices")

df_imp    <- read_excel(RUTA_COMP, sheet = "1_Resumen_Imputacion_Metodo")
df_sub_cl <- read_excel(RUTA_COMP, sheet = "3_Subindices_Potencias_Clave")
df_div24  <- read_excel(RUTA_COMP, sheet = "5_Divergencias_IMPE_vs_PIB_24")

# ==============================================================================
# FIGURA 01: SELECCIÓN DEL ÍNDICE ÓPTIMO (KMO MÍNIMO vs. VARIANZA CP1)
# ==============================================================================
df_f1 <- df_est %>%
  mutate(
    Etiqueta = str_replace_all(Indice, c("Base16_Completa_" = "", "Base16_Depurada25_" = "",
                                         "Base12_Completa_" = "", "Base12_Depurada25_" = "")),
    Destacado = Indice %in% c("2B_Base12_Depurada25_Bayes", "2B_Base12_Depurada25_RF", "2A_Base12_Completa_Bayes")
  )

p01 <- ggplot(df_f1, aes(x = KMO_Min_Individual, y = Var_Explicada_CP1_Pct, color = Familia, size = Alfa_Cronbach)) +
  annotate("rect", xmin = 0.50, xmax = Inf, ymin = 50, ymax = Inf, fill = "#ECFDF5", alpha = 0.6) +
  geom_vline(xintercept = 0.50, linetype = "dashed", color = COL_CRIMSON, linewidth = 0.7) +
  geom_hline(yintercept = 50.0, linetype = "dashed", color = COL_CRIMSON, linewidth = 0.7) +
  geom_point(alpha = 0.9) +
  geom_text_repel(
    aes(label = Etiqueta, fontface = if_else(Destacado, "bold", "plain")),
    size = 3, max.overlaps = 25, show.legend = FALSE
  ) +
  annotate("text", x = 0.78, y = 61.5, label = "ZONA ÓPTIMA\n(KMO_Min ≥ 0,50 y Var. CP1 ≥ 50%)",
           color = COL_TEAL, fontface = "bold", size = 3.2) +
  scale_color_manual(values = PAL_FAMILIAS, name = "Familia de Índice") +
  scale_size_continuous(name = "Alfa de\nCronbach (α)", range = c(2.5, 6.5)) +
  labs(
    title = "Selección Multicriterio del Índice Óptimo entre las 20 Especificaciones",
    subtitle = "Contraste de adecuación muestral individual (KMO mínimo), poder explicativo (CP1) y consistencia interna (α)",
    x = "KMO Mínimo Individual (umbral de Kaiser ≥ 0,50)",
    y = "Varianza Explicada por CP1 (%)",
    caption = "Fuente: Elaboración propia con base en IMPE_Comparativa_Maestra_20_Indices.xlsx"
  ) +
  tema_teg()

guardar_fig(p01, "Fig_01_Seleccion_Indice_Optimo_20_Modelos.png", w = 10.5, h = 6.5)

# ==============================================================================
# FIGURA 02: DIAGNÓSTICO DE IMPUTACIÓN (DISTANCIA KS vs. PRESERVACIÓN DE DE)
# ==============================================================================
df_f2 <- df_imp %>%
  filter(Metodo != "2_Interp_Max3") %>%
  mutate(Metodo_Limpio = recode(Metodo,
    "3_MICE_PMM"   = "MICE PMM",
    "4_MICE_Bayes" = "MICE Bayes",
    "5_MICE_RF"    = "MICE Random Forest"
  ))

p02 <- ggplot(df_f2, aes(x = KS_D_Real_vs_Tot_Medio, y = Ratio_SD_Medio, color = Familia, shape = Metodo_Limpio)) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = COL_SLATE, linewidth = 0.6) +
  geom_vline(xintercept = 0.05, linetype = "dotted", color = COL_TEAL, linewidth = 0.7) +
  geom_point(size = 4.2, alpha = 0.9) +
  geom_text_repel(aes(label = paste0(substr(Familia, 1, 2), "_", str_replace(Metodo_Limpio, "MICE ", ""))),
                  size = 3, show.legend = FALSE) +
  annotate("text", x = 0.025, y = 1.015, label = "Menor distorsión distribucional\n(KS_D < 0,05 y Ratio DE ≈ 1)",
           color = COL_TEAL, fontface = "bold", size = 3) +
  scale_color_manual(values = PAL_FAMILIAS, name = "Familia") +
  scale_shape_manual(values = c(16, 17, 15), name = "Algoritmo MICE") +
  labs(
    title = "Evaluación Distribucional de los Métodos de Imputación Múltiple (MICE)",
    subtitle = "Distancia de Kolmogorov-Smirnov (Real vs. Total) frente a preservación de dispersión (Ratio DE Imputado/Real)",
    x = "Distancia KS Media (Real vs. Serie Total; menor es mejor)",
    y = "Cociente de Desviación Estándar (Imputado / Real; ideal ≈ 1,0)",
    caption = "Fuente: Elaboración propia con base en IMPE_Data_Complementaria_Cap4.xlsx (se excluye Interp3 por imputar ~1%)"
  ) +
  tema_teg()

guardar_fig(p02, "Fig_02_Diagnostico_Imputacion_KS_vs_RatioSD.png", w = 10, h = 6.2)

# ==============================================================================
# FIGURA 03: PODA FACTORIAL DE VARIABLES (PESOS PROMEDIO EN CP1)
# ==============================================================================
vars_podadas <- c("prod_manuf_pib", "prod_fbcf_pib", "fin_ied_sal_pib", "tech_serv_ict_pct")

nombres_vars <- c(
  tech_exp_serv_dig  = "Exportación de servicios digitales",
  tech_patentes_tic  = "Solicitudes de patentes TIC",
  fin_regalias_rec   = "Ingresos por regalías y licencias",
  prod_eci           = "Índice de Complejidad Económica (ECI)",
  tech_id_pib        = "Gasto en I+D (% del PIB)",
  tech_investigad    = "Investigadores en I+D (per cápita)",
  prod_exp_total     = "Exportaciones totales de bienes y serv.",
  tech_mnf_val_added = "Valor agregado manufacturero (USD)",
  prod_pib_pcap_ppa  = "PIB per cápita (PPA)",
  fin_cred_priv_pib  = "Crédito privado doméstico (% PIB)",
  tech_exp_alta      = "Exportaciones de alta tecnología",
  fin_mkt_cap_pib    = "Capitalización bursátil (% PIB)",
  prod_manuf_pib     = "Manufactura (% PIB) [PODADA]",
  prod_fbcf_pib      = "Formación bruta capital fijo (% PIB) [PODADA]",
  tech_serv_ict_pct  = "Servicios TIC (% exp. servicios) [PODADA]",
  fin_ied_sal_pib    = "IED saliente (% PIB) [PODADA]"
)

df_f3 <- df_pca %>%
  filter(Familia %in% c("1A_Base16_Completa", "1B_Base16_Depurada25")) %>%
  group_by(Variable) %>%
  summarise(Peso_Pct = mean(Peso_CP1, na.rm = TRUE) * 100, .groups = "drop") %>%
  mutate(
    Estado = if_else(Variable %in% vars_podadas, "Podada (Base 16 → 12)",
             case_when(
               str_starts(Variable, "tech_") ~ "Retenida — Tecnológico",
               str_starts(Variable, "prod_") ~ "Retenida — Productivo",
               TRUE                          ~ "Retenida — Financiero"
             )),
    Etiqueta = recode(Variable, !!!nombres_vars)
  ) %>%
  arrange(Peso_Pct) %>%
  mutate(Etiqueta = factor(Etiqueta, levels = Etiqueta))

p03 <- ggplot(df_f3, aes(x = Peso_Pct, y = Etiqueta, color = Estado)) +
  geom_segment(aes(x = 0, xend = Peso_Pct, y = Etiqueta, yend = Etiqueta), linewidth = 1.1, alpha = 0.85) +
  geom_point(size = 3.8) +
  geom_text(aes(label = sprintf("%.2f%%", Peso_Pct)), hjust = -0.25, size = 3.1, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(
    values = c(
      "Retenida — Tecnológico" = COL_NAVY,
      "Retenida — Productivo"  = COL_TEAL,
      "Retenida — Financiero"  = COL_AMBER,
      "Podada (Base 16 → 12)"  = COL_CRIMSON
    ),
    name = "Categoría / Estado"
  ) +
  scale_x_continuous(limits = c(0, 13.5), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Jerarquía de Pesos Endógenos en CP1 y Justificación de la Poda Factorial",
    subtitle = "Contribución promedio en Base 16 (10 especificaciones): las 4 variables excluidas aportan < 2% conjunto",
    x = "Peso Promedio en el Primer Componente Principal (w_j = v_1j²)",
    y = NULL,
    caption = "Fuente: Elaboración propia con base en IMPE_Comparativa_Maestra_20_Indices.xlsx (hoja 4_PCA_Detalle_Completo)"
  ) +
  tema_teg() +
  theme(legend.position = "bottom")

guardar_fig(p03, "Fig_03_Poda_Variables_Pesos_CP1.png", w = 10.5, h = 6.8)

# ==============================================================================
# FIGURA 04: ESTABILIDAD DEL PESO DE LOS 3 SUBÍNDICES EN LOS 20 ÍNDICES
# ==============================================================================
df_f4 <- df_pca %>%
  mutate(Subindice = case_when(
    str_starts(Variable, "tech_") ~ "Tecnologico",
    str_starts(Variable, "prod_") ~ "Productivo",
    TRUE                          ~ "Financiero"
  )) %>%
  group_by(Indice, Subindice) %>%
  summarise(Peso = sum(Peso_CP1, na.rm = TRUE) * 100, .groups = "drop") %>%
  mutate(
    Indice = factor(Indice, levels = rev(df_est$Indice)),
    Subindice = factor(Subindice, levels = c("Tecnologico", "Productivo", "Financiero"))
  )

p04 <- ggplot(df_f4, aes(x = Peso, y = Indice, fill = Subindice)) +
  geom_col(width = 0.72, alpha = 0.92) +
  geom_text(aes(label = sprintf("%.1f%%", Peso)), position = position_stack(vjust = 0.5),
            color = "white", size = 2.7, fontface = "bold") +
  scale_fill_manual(
    values = PAL_SUB,
    labels = c("Tecnologico" = "Tecnológico", "Productivo" = "Productivo", "Financiero" = "Financiero"),
    name = "Subíndice Estructural"
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Estabilidad de la Estructura de Ponderación por Subíndice (20 Modelos)",
    subtitle = "El pilar Tecnológico absorbe consistentemente > 51% del peso endógeno en todas las familias y tratamientos",
    x = "Participación en el Peso Total de CP1 (%)",
    y = NULL,
    caption = "Fuente: Elaboración propia con base en IMPE_Comparativa_Maestra_20_Indices.xlsx"
  ) +
  tema_teg() +
  theme(legend.position = "bottom")

guardar_fig(p04, "Fig_04_Participacion_Subindices_20_Indices.png", w = 10.5, h = 7.5)

# ==============================================================================
# FIGURA 05: EVOLUCIÓN DE LOS 3 SUBÍNDICES EN POTENCIAS CLAVE + VENEZUELA
# ==============================================================================
paises_f5 <- c("USA", "CHN", "JPN", "SGP", "DEU", "GBR", "KOR", "RUS", "VEN")
nombres_p5 <- c(
  USA = "Estados Unidos (2B)", CHN = "China (2B)", JPN = "Japón (2B)",
  SGP = "Singapur (2B)", DEU = "Alemania (2B)", GBR = "Reino Unido (2B)",
  KOR = "Corea del Sur (2B)", RUS = "Rusia (2B)", VEN = "Venezuela (2A*)"
)

if (file.exists(RUTA_SUB_CSV)) {
  df_f5 <- read_csv(RUTA_SUB_CSV, show_col_types = FALSE) %>%
    filter(
      (Indice == "2B_Base12_Depurada25_Bayes" & iso3c %in% setdiff(paises_f5, "VEN")) |
        (Indice == "2A_Base12_Completa_Bayes"   & iso3c == "VEN")
    ) %>%
    rename(Valor = Subindice_100)
} else {
  df_f5 <- df_sub_cl %>%
    filter(
      (Indice == "2B_Base12_Depurada25_Bayes" & iso3c %in% setdiff(paises_f5, "VEN")) |
        (Indice == "2A_Base12_Completa_Bayes"   & iso3c == "VEN")
    ) %>%
    pivot_longer(cols = starts_with("Sub100_"), names_to = "Subindice", values_to = "Valor") %>%
    mutate(Subindice = str_remove(Subindice, "Sub100_"))
}

df_f5 <- df_f5 %>%
  mutate(
    Pais_Label = factor(recode(iso3c, !!!nombres_p5), levels = nombres_p5[paises_f5]),
    Subindice  = factor(Subindice, levels = c("Tecnologico", "Productivo", "Financiero"))
  )

p05 <- ggplot(df_f5, aes(x = year, y = Valor, color = Subindice)) +
  geom_line(linewidth = 1.05) +
  facet_wrap(~ Pais_Label, ncol = 3) +
  scale_color_manual(
    values = PAL_SUB,
    labels = c("Tecnologico" = "Tecnológico", "Productivo" = "Productivo", "Financiero" = "Financiero"),
    name = "Subíndice"
  ) +
  scale_y_continuous(limits = c(20, 100)) +
  labs(
    title = "Evolución Longitudinal de los Tres Subíndices Estructurales (1995–2024)",
    subtitle = "Trayectorias en 2B_Bayes para potencias clave (*Venezuela evaluada en 2A_Bayes por >25% NAs)",
    x = "Año", y = "Puntaje del Subíndice (0–100)",
    caption = "Fuente: Elaboración propia con base en IMPE_Data_Complementaria_Cap4.xlsx / IMPE_Subindices_20_Indices_Largo.csv"
  ) +
  tema_teg(base_size = 10.5) +
  theme(legend.position = "bottom")

guardar_fig(p05, "Fig_05_Evolucion_Subindices_Potencias_1995_2024.png", w = 11, h = 7.5)

# ==============================================================================
# FIGURA 06: PERFIL DE ASIMETRÍA INTERDIMENSIONAL EN 2024 (DUMBBELL CHART)
# ==============================================================================
df_f6 <- df_sub_cl %>%
  filter(
    year == 2024,
    (Indice == "2B_Base12_Depurada25_Bayes" & iso3c %in% c("USA", "JPN", "SGP", "CHE", "KOR", "GBR", "DEU", "CHN", "RUS")) |
      (Indice == "2A_Base12_Completa_Bayes"   & iso3c == "VEN")
  ) %>%
  mutate(
    Min_P = pmin(Sub100_Productivo, Sub100_Financiero, Sub100_Tecnologico),
    Max_P = pmax(Sub100_Productivo, Sub100_Financiero, Sub100_Tecnologico),
    Media = (Sub100_Productivo + Sub100_Financiero + Sub100_Tecnologico) / 3,
    Pais  = if_else(iso3c == "VEN", "Venezuela (2A*)", country)
  ) %>%
  arrange(Media) %>%
  mutate(Pais = factor(Pais, levels = Pais))

df_f6_long <- df_f6 %>%
  pivot_longer(cols = starts_with("Sub100_"), names_to = "Subindice", values_to = "Valor") %>%
  mutate(Subindice = str_remove(Subindice, "Sub100_"))

p06 <- ggplot() +
  geom_segment(data = df_f6, aes(x = Min_P, xend = Max_P, y = Pais, yend = Pais),
               color = "#CBD5E1", linewidth = 2.2) +
  geom_point(data = df_f6_long, aes(x = Valor, y = Pais, color = Subindice), size = 4) +
  scale_color_manual(
    values = PAL_SUB,
    labels = c("Tecnologico" = "Tecnológico", "Productivo" = "Productivo", "Financiero" = "Financiero"),
    name = "Subíndice"
  ) +
  labs(
    title = "Asimetría Interna entre Pilares de Poder Económico (2024)",
    subtitle = "Amplitud entre el pilar más fuerte y el más débil en 2B_Bayes (*Venezuela en 2A_Bayes)",
    x = "Puntaje del Subíndice en 2024 (0–100)", y = NULL,
    caption = "Fuente: Elaboración propia con base en IMPE_Data_Complementaria_Cap4.xlsx"
  ) +
  tema_teg() +
  theme(legend.position = "bottom")

guardar_fig(p06, "Fig_06_Perfil_Asimetria_Subindices_2024.png", w = 10, h = 6)

# ==============================================================================
# FIGURA 07: SENSIBILIDAD NO COMPENSATORIA (IMPE LINEAL vs. MPI PONDERADO)
# ==============================================================================
if (file.exists(RUTA_MPI)) {
  df_mpi_res <- read_excel(RUTA_MPI, sheet = "1_Resumen_Concordancia_MPI")
  df_mpi_2B  <- read_excel(RUTA_MPI, sheet = "2_Comparativa_2B_Bayes_2024")
  rho_24     <- df_mpi_res$Spearman_2024_Lineal_MPIP[df_mpi_res$Indice == "2B_Base12_Depurada25_Bayes"]
  
  p07 <- ggplot(df_mpi_2B, aes(x = Rank_Lineal, y = Rank_MPI_Ponderado)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = COL_SLATE, linewidth = 0.7) +
    geom_point(aes(size = Penalizacion_MPI_P, color = Delta_Rank_Pond), alpha = 0.88) +
    geom_text_repel(
      data = df_mpi_2B %>% filter(iso3c %in% c("USA", "JPN", "SGP", "CHE", "KOR", "HKG", "DEU", "CHN", "ISR", "NLD", "AUT", "RUS", "IND", "ARG") | abs(Delta_Rank_Pond) >= 4),
      aes(label = iso3c), size = 3, fontface = "bold", max.overlaps = 20
    ) +
    scale_color_gradient2(low = COL_CRIMSON, mid = COL_SLATE, high = COL_TEAL, midpoint = 0, name = "Cambio de\nRanking (Δ)") +
    scale_size_continuous(name = "Penalización\nMPI (S·cv)", range = c(2, 7)) +
    scale_x_reverse() + scale_y_reverse() +
    labs(
      title = "Validación de No Sustituibilidad: IMPE Lineal vs. Mazziotta-Pareto (2B_Bayes, 2024)",
      subtitle = sprintf("N = 54 Estados | Spearman ρ = %.4f | Bajo la diagonal pierden puestos por desequilibrio sectorial", rho_24),
      x = "Posición en IMPE Lineal (CP1)",
      y = "Posición en IMPE Penalizado (MPI Ponderado)",
      caption = "Fuente: Elaboración propia (Mazziotta & Pareto, 2016) con base en IMPE_Sensibilidad_MPI_NoSustituibilidad.xlsx"
    ) +
    tema_teg()
  
  guardar_fig(p07, "Fig_07_Sensibilidad_Lineal_vs_MPI_2024.png", w = 10, h = 6.5)
} else {
  message("Aviso: Se omitió Fig_07 porque no se encontró ", RUTA_MPI)
}

# ==============================================================================
# FIGURA 08: TRAYECTORIA HISTÓRICA DEL IMPE (1995–2024) EN POTENCIAS CLAVE
# ==============================================================================
paises_f8 <- c("USA", "JPN", "SGP", "CHE", "KOR", "DEU", "GBR", "CHN", "RUS")
df_f8 <- df_scores %>%
  filter(iso3c %in% paises_f8) %>%
  select(iso3c, year, Score = Score100_2B_Base12_Depurada25_Bayes)

df_f8_end <- df_f8 %>% filter(year == 2024)

p08 <- ggplot(df_f8, aes(x = year, y = Score, color = iso3c)) +
  geom_line(aes(linewidth = iso3c %in% c("USA", "CHN", "RUS")), alpha = 0.9) +
  geom_text_repel(
    data = df_f8_end,
    aes(label = sprintf("%s (%.1f)", iso3c, Score)),
    nudge_x = 1.2, direction = "y", hjust = 0, size = 3.1, fontface = "bold", segment.color = "#CBD5E1"
  ) +
  scale_linewidth_manual(values = c("FALSE" = 0.85, "TRUE" = 1.5), guide = "none") +
  scale_color_manual(
    values = c(
      USA = COL_NAVY, CHN = COL_CRIMSON, JPN = "#0284C7", SGP = COL_TEAL,
      CHE = "#64748B", KOR = "#7C3AED", DEU = COL_AMBER, GBR = "#334155", RUS = "#991B1B"
    ),
    guide = "none"
  ) +
  scale_x_continuous(limits = c(1995, 2028.5), breaks = seq(1995, 2025, 5)) +
  labs(
    title = "Trayectoria Histórica del Poder Económico Estructural (1995–2024)",
    subtitle = "Evolución en el índice óptimo principal 2B_Base12_Depurada25_Bayes (escala 0–100)",
    x = "Año", y = "Puntaje IMPE (0–100)",
    caption = "Fuente: Elaboración propia con base en IMPE_Comparativa_Maestra_20_Indices.xlsx"
  ) +
  tema_teg()

guardar_fig(p08, "Fig_08_Trayectoria_Historica_IMPE_1995_2024.png", w = 10.5, h = 6.2)

# ==============================================================================
# FIGURA 09: BANDAS DE INCERTIDUMBRE INTER-MÉTODO (TOP 20 EN 2B, 2024)
# ==============================================================================
df_f9 <- df_rangos %>%
  filter(Familia == "2B_Base12_Depurada25", year == 2024) %>%
  arrange(desc(Media_Score)) %>%
  slice_head(n = 20) %>%
  arrange(Media_Score) %>%
  mutate(country = factor(country, levels = country))

p09 <- ggplot(df_f9, aes(y = country)) +
  geom_segment(aes(x = Min_Score, xend = Max_Score, yend = country), color = "#94A3B8", linewidth = 2.4) +
  geom_point(aes(x = Media_Score), color = COL_NAVY, size = 3.5) +
  geom_text(aes(x = Max_Score, label = sprintf("DE = %.2f", SD_Entre_Metodos)),
            hjust = -0.15, size = 2.9, color = COL_SLATE) +
  scale_x_continuous(limits = c(60, 104)) +
  labs(
    title = "Estabilidad y Bandas de Incertidumbre Inter-Método: Top 20 Mundial (2024)",
    subtitle = "Rango mínimo–máximo y media entre los 5 tratamientos de datos faltantes en la familia 2B_Base12_Depurada25",
    x = "Puntaje IMPE (0–100)", y = NULL,
    caption = "Fuente: Elaboración propia con base en IMPE_Comparativa_Maestra_20_Indices.xlsx (hoja 8_Rangos_Pais_Familia)"
  ) +
  tema_teg()

guardar_fig(p09, "Fig_09_Bandas_Incertidumbre_InterMetodo_Top20_2024.png", w = 10, h = 7)

# ==============================================================================
# FIGURA 10: VALIDEZ EXTERNA — IMPE vs. PIB NOMINAL (OBJETIVO ESPECÍFICO 4)
# ==============================================================================
df_f10 <- df_div24 %>%
  filter(!is.na(PIB_Nominal_BnUSD), PIB_Nominal_BnUSD > 0, !is.na(Score100_2A_Bayes))

etiq_f10 <- c("USA", "CHN", "JPN", "DEU", "GBR", "IND", "RUS", "BRA", "MEX", "SGP", "HKG", "CHE", "KOR", "VEN", "NGA", "CYM")

p10 <- ggplot(df_f10, aes(x = PIB_Nominal_BnUSD, y = Score100_2A_Bayes, color = Perfil_Estructural_2A)) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = COL_NAVY, fill = "#CBD5E1", alpha = 0.25, linewidth = 0.9) +
  geom_point(alpha = 0.8, size = 2.5) +
  geom_text_repel(
    data = df_f10 %>% filter(iso3c %in% etiq_f10),
    aes(label = iso3c), size = 3, fontface = "bold", max.overlaps = 25, show.legend = FALSE
  ) +
  scale_x_log10(labels = label_comma(suffix = " B")) +
  scale_color_manual(
    values = c(
      "Sobre-rinde en Poder Estructural (+15 puestos vs PIB)" = COL_TEAL,
      "Alineado con tamaño PIB"                               = COL_NAVY,
      "Sub-rinde en Poder Estructural (-15 puestos vs PIB)"   = COL_CRIMSON
    ),
    name = "Perfil Estructural (2024)"
  ) +
  labs(
    title = "Validez Externa y Divergencia Estructural: IMPE frente al PIB Nominal (2024)",
    subtitle = "N = 199 Estados con dato PIB | Escala logarítmica en PIB Nominal (miles de millones USD)",
    x = "PIB Nominal 2024 (miles de millones USD, escala log10)",
    y = "Puntaje IMPE 2A_Base12_Completa_Bayes (0–100)",
    caption = "Fuente: Elaboración propia con base en IMPE_Data_Complementaria_Cap4.xlsx (hoja 5_Divergencias_IMPE_vs_PIB_24)"
  ) +
  tema_teg() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 3))

guardar_fig(p10, "Fig_10_Validez_Externa_IMPE_vs_PIB_Nominal_2024.png", w = 10.5, h = 6.8)

# ==============================================================================
# FIGURA 11: BRECHAS DE RANKING IMPE vs. PIB NOMINAL (CASOS EMBLEMÁTICOS 2024)
# ==============================================================================
casos_f11 <- c("HKG", "SGP", "SWE", "CHE", "KOR", "JPN", "USA", "CHN", "DEU", "SAU", "RUS", "BRA", "IND", "MEX", "IDN", "ARG", "VEN", "NGA")

df_f11 <- df_div24 %>%
  filter(iso3c %in% casos_f11, !is.na(Brecha_Rank_2A_vs_PIBNom)) %>%
  mutate(
    Sentido = if_else(Brecha_Rank_2A_vs_PIBNom >= 0, "Sobre-rinde frente a su PIB", "Sub-rinde frente a su PIB"),
    Etiqueta_Pais = paste0(country, " (#", Rank_IMPE_2A_Bayes, " vs PIB #", Rank_PIB_Nominal, ")")
  ) %>%
  arrange(Brecha_Rank_2A_vs_PIBNom) %>%
  mutate(Etiqueta_Pais = factor(Etiqueta_Pais, levels = Etiqueta_Pais))

p11 <- ggplot(df_f11, aes(x = Brecha_Rank_2A_vs_PIBNom, y = Etiqueta_Pais, fill = Sentido)) +
  geom_vline(xintercept = 0, color = "#1E293B", linewidth = 0.7) +
  geom_col(width = 0.72, alpha = 0.9) +
  geom_text(
    aes(label = sprintf("%+d", Brecha_Rank_2A_vs_PIBNom),
        hjust = if_else(Brecha_Rank_2A_vs_PIBNom >= 0, -0.2, 1.2)),
    size = 3, fontface = "bold"
  ) +
  scale_fill_manual(values = c("Sobre-rinde frente a su PIB" = COL_TEAL, "Sub-rinde frente a su PIB" = COL_CRIMSON), name = NULL) +
  scale_x_continuous(limits = c(-145, 55)) +
  labs(
    title = "Brecha de Posición Estructural: Ranking IMPE (2A_Bayes) frente a PIB Nominal (2024)",
    subtitle = "Valores positivos indican mayor poder estructural que tamaño nominal; negativos evidencian rezago estructural",
    x = "Diferencia de Puestos (Rank PIB Nominal − Rank IMPE 2A)", y = NULL,
    caption = "Fuente: Elaboración propia con base en IMPE_Data_Complementaria_Cap4.xlsx"
  ) +
  tema_teg() +
  theme(legend.position = "bottom")

guardar_fig(p11, "Fig_11_Brechas_Estructurales_IMPE_vs_PIB_2024.png", w = 10.5, h = 6.8)

# ==============================================================================
# FIGURA 12: CASO CHINA — ESCALA (BASE 16) vs. DENSIDAD (BASE 12) (1995–2024)
# ==============================================================================
df_f12 <- df_ranks %>%
  filter(iso3c %in% c("USA", "CHN", "JPN", "SGP", "DEU", "KOR")) %>%
  select(
    iso3c, year,
    `Base 16 Completa (1A — Con Volumen)` = Rank_1A_Base16_Completa_Bayes,
    `Base 12 Completa (2A — Densidad)`    = Rank_2A_Base12_Completa_Bayes,
    `Base 12 Depurada (2B — Óptimo)`      = Rank_2B_Base12_Depurada25_Bayes
  ) %>%
  pivot_longer(cols = -c(iso3c, year), names_to = "Especificacion", values_to = "Ranking")

p12 <- ggplot(df_f12, aes(x = year, y = Ranking, color = iso3c)) +
  geom_line(aes(linewidth = iso3c == "CHN"), alpha = 0.9) +
  facet_wrap(~ Especificacion, ncol = 3) +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 25, 30)) +
  scale_linewidth_manual(values = c("FALSE" = 0.85, "TRUE" = 1.6), guide = "none") +
  scale_color_manual(
    values = c(USA = COL_NAVY, CHN = COL_CRIMSON, JPN = "#0284C7", SGP = COL_TEAL, DEU = COL_AMBER, KOR = "#7C3AED"),
    name = "País"
  ) +
  labs(
    title = "Escala frente a Densidad Estructural: El Caso de China (1995–2024)",
    subtitle = "En Base 16 (con manufactura e inversión sobre PIB) China alcanza el #2 global; en Base 12 se ubica #6 (2A) y #11 (2B)",
    x = "Año", y = "Posición en el Ranking Mundial (1 = Líder)",
    caption = "Fuente: Elaboración propia con base en IMPE_Comparativa_Maestra_20_Indices.xlsx (hoja 11_Ranks_20_Indices)"
  ) +
  tema_teg(base_size = 10.5) +
  theme(legend.position = "bottom")

guardar_fig(p12, "Fig_12_Caso_China_Escala_Base16_vs_Densidad_Base12.png", w = 11.5, h = 5.5)

message("\n✔ BATERÍA COMPLETA DE FIGURAS DEL T.E.G. EXPORTADA EN: ", DIR_SALIDA, "/")
