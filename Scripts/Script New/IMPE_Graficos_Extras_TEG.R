# ==============================================================================
# GRÁFICAS COMPLEMENTARIAS DE ALTO IMPACTO PARA EL CAPÍTULO IV — IMPE (1995-2024)
# ==============================================================================
# Genera 4 figuras editoriales (300 dpi) en Figuras_TEG_Cap4/ con la misma
# identidad visual de IMPE_Bateria_Plots_TEG.R:
#   - Fig_18_Mapa_Mundial_IMPE_2024.png
#   - Fig_19_Transiciones_Clusters_1995_2024.png
#   - Fig_20_America_Latina_vs_Venezuela_1995_2024.png
#   - Fig_20B_Venezuela_Subindices_1995_2024.png
#
# INSUMOS REQUERIDOS:
#   - IMPE_Comparativa_Maestra_20_Indices.xlsx
#   - IMPE_Extension_Clusters_Zonas_Determinantes.xlsx
#   - IMPE_Data_Complementaria_Cap4.xlsx (o IMPE_Subindices_20_Indices_Largo.csv)
# ==============================================================================

paquetes <- c("readxl", "readr", "dplyr", "tidyr", "stringr", "ggplot2", "ggrepel", "sf", "rnaturalearth", "rnaturalearthdata")
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) install.packages(faltantes, dependencies = TRUE)
invisible(lapply(paquetes, library, character.only = TRUE))

RUTA_MAESTRO <- "IMPE_Comparativa_Maestra_20_Indices.xlsx"
RUTA_EXT     <- "IMPE_Extension_Clusters_Zonas_Determinantes.xlsx"
RUTA_COMP    <- "IMPE_Data_Complementaria_Cap4.xlsx"
RUTA_SUB_CSV <- "IMPE_Subindices_20_Indices_Largo.csv"
DIR_SALIDA   <- "Figuras_TEG_Cap4"

stopifnot("Falta IMPE_Comparativa_Maestra_20_Indices.xlsx"        = file.exists(RUTA_MAESTRO))
stopifnot("Falta IMPE_Extension_Clusters_Zonas_Determinantes.xlsx" = file.exists(RUTA_EXT))
if (!dir.exists(DIR_SALIDA)) dir.create(DIR_SALIDA, recursive = TRUE)

COL_NAVY    <- "#1B365D"
COL_TEAL    <- "#0F766E"
COL_AMBER   <- "#D97706"
COL_CRIMSON <- "#B91C1C"
COL_SLATE   <- "#475569"

tema_teg <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", color = COL_NAVY, size = base_size + 2, margin = margin(b = 4)),
      plot.subtitle    = element_text(color = "#334155", size = base_size - 1, margin = margin(b = 10)),
      plot.caption     = element_text(color = "#64748B", size = base_size - 2.5, hjust = 0, margin = margin(t = 8)),
      axis.title       = element_text(face = "bold", color = "#1E293B", size = base_size - 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E2E8F0", linewidth = 0.4),
      legend.title     = element_text(face = "bold", size = base_size - 1),
      legend.position  = "bottom"
    )
}

df_scores <- read_excel(RUTA_MAESTRO, sheet = "9_Scores100_20_Indices")

# ==============================================================================
# FIGURA 18: MAPA COROPLÉTICO MUNDIAL CONTINUO DEL IMPE (2A_Bayes, 2024)
# ==============================================================================
sf::sf_use_s2(FALSE)
mundo <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(iso3c = if_else(iso_a3 == "-99" | is.na(iso_a3), adm0_a3, iso_a3)) %>%
  filter(iso3c != "ATA")

mapa_impe <- mundo %>%
  left_join(
    df_scores %>% filter(year == 2024) %>% select(iso3c, IMPE_2024 = Score100_2A_Base12_Completa_Bayes),
    by = "iso3c"
  )

p18 <- ggplot(mapa_impe) +
  geom_sf(aes(fill = IMPE_2024), color = "#E2E8F0", linewidth = 0.15) +
  scale_fill_gradientn(
    colours = c("#B91C1C", "#F59E0B", "#E2E8F0", "#0F766E", "#1B365D"),
    values  = c(0, 0.28, 0.50, 0.72, 1.0),
    limits  = c(0, 85),
    na.value = "#F1F5F9",
    name = "Puntaje IMPE 2024 (2A_Bayes, 0–100)"
  ) +
  labs(
    title = "Distribución Geográfica Mundial del Poder Económico Estructural (2024)",
    subtitle = "Gradiente continuo del IMPE 2A_Base12_Completa_Bayes en 214 economías cartografiadas",
    caption = "Fuente: Elaboración propia con base en IMPE_Comparativa_Maestra_20_Indices.xlsx"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", color = COL_NAVY, hjust = 0.5, size = 13),
    plot.subtitle = element_text(color = "#334155", hjust = 0.5, margin = margin(b = 8)),
    plot.caption  = element_text(color = "#64748B", size = 8.5, hjust = 0),
    legend.position = "bottom",
    legend.key.width = unit(2.2, "cm")
  )

ggsave(file.path(DIR_SALIDA, "Fig_18_Mapa_Mundial_IMPE_2024.png"), p18, width = 11.5, height = 6.2, dpi = 300, bg = "white")
message("✔ Exportada: Fig_18_Mapa_Mundial_IMPE_2024.png")

# ==============================================================================
# FIGURA 19: DIAGRAMA DE TRANSICIÓN DE CLUSTERS (1995 → 2024)
# ==============================================================================
t2b <- read_excel(RUTA_EXT, sheet = "4_Clusters_Trans_2B")
t2a <- read_excel(RUTA_EXT, sheet = "5_Clusters_Trans_2A")

casos_dest <- c("USA", "JPN", "CHN", "SGP", "KOR", "HKG", "ISR", "DEU", "GBR", "IND", "RUS", "BRA", "MEX", "CHL", "ARG", "COL")

df_slope <- bind_rows(
  t2b %>% filter(iso3c %in% casos_dest) %>% mutate(Origen = "2B_Bayes"),
  t2a %>% filter(iso3c == "VEN")        %>% mutate(Origen = "2A_Bayes (VEN)")
) %>%
  mutate(
    Tipo_Mov = case_when(
      iso3c == "CHN"       ~ "Ascenso único (+3 estratos: China)",
      iso3c == "VEN"       ~ "Retroceso único (-2 estratos: Venezuela*)",
      Salto_Estrato >= 2   ~ "Ascenso rápido (+2 estratos)",
      Salto_Estrato == 1   ~ "Ascenso moderado (+1 estrato)",
      TRUE                 ~ "Estable en su estrato"
    ),
    # Pequeño jitter determinista para evitar superposición total de líneas
    j = (row_number() - mean(row_number())) * 0.022,
    C95 = Cluster_1995 + j,
    C24 = Cluster_2024 + j
  )

p19 <- ggplot(df_slope) +
  geom_segment(aes(x = 1, xend = 2, y = C95, yend = C24, color = Tipo_Mov,
                   linetype = iso3c == "VEN", linewidth = iso3c %in% c("CHN", "VEN")), alpha = 0.88) +
  geom_point(aes(x = 1, y = C95, color = Tipo_Mov), size = 2.6) +
  geom_point(aes(x = 2, y = C24, color = Tipo_Mov), size = 2.6) +
  geom_text_repel(aes(x = 1, y = C95, label = iso3c, color = Tipo_Mov),
                  nudge_x = -0.10, direction = "y", hjust = 1, size = 3, fontface = "bold", show.legend = FALSE, max.overlaps = 30) +
  geom_text_repel(aes(x = 2, y = C24, label = iso3c, color = Tipo_Mov),
                  nudge_x = 0.10, direction = "y", hjust = 0, size = 3, fontface = "bold", show.legend = FALSE, max.overlaps = 30) +
  scale_x_continuous(breaks = c(1, 2), labels = c("1995\n(sobre frontera 2024)", "2024\n(clasificación actual)"), limits = c(0.65, 2.35)) +
  scale_y_reverse(
    breaks = 1:4,
    labels = c("Cluster 1\n(Núcleo Hegemónico)", "Cluster 2\n(Potencias Consolidadas)",
               "Cluster 3\n(Semiperiferia)", "Cluster 4\n(Periferia Estructural)")
  ) +
  scale_color_manual(
    values = c(
      "Ascenso único (+3 estratos: China)"        = COL_NAVY,
      "Ascenso rápido (+2 estratos)"              = COL_TEAL,
      "Ascenso moderado (+1 estrato)"             = "#64748B",
      "Estable en su estrato"                     = "#94A3B8",
      "Retroceso único (-2 estratos: Venezuela*)" = COL_CRIMSON
    ),
    name = "Trayectoria 1995–2024"
  ) +
  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed"), guide = "none") +
  scale_linewidth_manual(values = c("FALSE" = 0.85, "TRUE" = 1.6), guide = "none") +
  labs(
    title = "Movilidad entre Estratos de Poder Económico (1995 → 2024)",
    subtitle = "Proyección de 1995 sobre centroides fijos de 2024 en 2B_Bayes (*Venezuela incorporada desde 2A_Bayes)",
    x = NULL, y = NULL,
    caption = "Fuente: Elaboración propia con base en IMPE_Extension_Clusters_Zonas_Determinantes.xlsx"
  ) +
  tema_teg() +
  guides(color = guide_legend(nrow = 2))

ggsave(file.path(DIR_SALIDA, "Fig_19_Transiciones_Clusters_1995_2024.png"), p19, width = 10.5, height = 6.8, dpi = 300, bg = "white")
message("✔ Exportada: Fig_19_Transiciones_Clusters_1995_2024.png")

# ==============================================================================
# FIGURA 20: AMÉRICA LATINA vs. VENEZUELA (1995–2024 EN 2A_Bayes)
# ==============================================================================
latam <- c("BRA", "MEX", "CHL", "COL", "ARG", "PER", "URY", "VEN")
df_latam <- df_scores %>%
  filter(iso3c %in% latam) %>%
  select(iso3c, country, year, Score = Score100_2A_Base12_Completa_Bayes)

df_latam_end <- df_latam %>% filter(year == 2024)

p20 <- ggplot(df_latam, aes(x = year, y = Score, color = iso3c)) +
  geom_line(aes(linewidth = iso3c %in% c("VEN", "BRA", "MEX")), alpha = 0.92) +
  geom_text_repel(
    data = df_latam_end,
    aes(label = sprintf("%s (%.1f)", iso3c, Score)),
    nudge_x = 1.0, direction = "y", hjust = 0, size = 3.1, fontface = "bold", segment.color = "#CBD5E1"
  ) +
  scale_linewidth_manual(values = c("FALSE" = 0.8, "TRUE" = 1.55), guide = "none") +
  scale_color_manual(
    values = c(
      BRA = COL_NAVY, MEX = COL_TEAL, CHL = "#0284C7", COL = COL_AMBER,
      ARG = "#7C3AED", PER = "#64748B", URY = "#94A3B8", VEN = COL_CRIMSON
    ),
    guide = "none"
  ) +
  scale_x_continuous(limits = c(1995, 2028.5), breaks = seq(1995, 2025, 5)) +
  labs(
    title = "Trayectoria de América Latina y Desplome Estructural de Venezuela (1995–2024)",
    subtitle = "Evolución comparada en 2A_Base12_Completa_Bayes (escala 0–100; Venezuela cae del puesto #53 al #164)",
    x = "Año", y = "Puntaje IMPE 2A_Bayes (0–100)",
    caption = "Fuente: Elaboración propia con base en IMPE_Comparativa_Maestra_20_Indices.xlsx"
  ) +
  tema_teg()

ggsave(file.path(DIR_SALIDA, "Fig_20_America_Latina_vs_Venezuela_1995_2024.png"), p20, width = 10.2, height = 6.2, dpi = 300, bg = "white")
message("✔ Exportada: Fig_20_America_Latina_vs_Venezuela_1995_2024.png")

# ==============================================================================
# FIGURA 20B: DESCOMPOSICIÓN DE SUBÍNDICES DE VENEZUELA (1995–2024, 2A_Bayes)
# ==============================================================================
if (file.exists(RUTA_SUB_CSV)) {
  df_ven_sub <- read_csv(RUTA_SUB_CSV, show_col_types = FALSE) %>%
    filter(Indice == "2A_Base12_Completa_Bayes", iso3c == "VEN") %>%
    rename(Valor = Subindice_100)
} else {
  df_ven_sub <- read_excel(RUTA_COMP, sheet = "3_Subindices_Potencias_Clave") %>%
    filter(Indice == "2A_Base12_Completa_Bayes", iso3c == "VEN") %>%
    pivot_longer(cols = starts_with("Sub100_"), names_to = "Subindice", values_to = "Valor") %>%
    mutate(Subindice = str_remove(Subindice, "Sub100_"))
}

p20b <- ggplot(df_ven_sub, aes(x = year, y = Valor, color = Subindice)) +
  geom_line(linewidth = 1.3) +
  geom_point(data = df_ven_sub %>% filter(year %in% c(1995, 2005, 2015, 2024)), size = 2.8) +
  scale_color_manual(
    values = c("Tecnologico" = COL_NAVY, "Productivo" = COL_TEAL, "Financiero" = COL_AMBER),
    labels = c("Tecnologico" = "Tecnológico", "Productivo" = "Productivo", "Financiero" = "Financiero"),
    name = "Subíndice Estructural"
  ) +
  scale_y_continuous(limits = c(20, 65)) +
  labs(
    title = "Caso Venezuela: Contracción Simultánea de los Tres Pilares Estructurales (1995–2024)",
    subtitle = "Evolución en 2A_Base12_Completa_Bayes (Productivo -26,48 pts | Tecnológico -19,08 pts | Financiero -12,60 pts)",
    x = "Año", y = "Puntaje del Subíndice (0–100)",
    caption = "Fuente: Elaboración propia con base en IMPE_Data_Complementaria_Cap4.xlsx / IMPE_Subindices_20_Indices_Largo.csv"
  ) +
  tema_teg()

ggsave(file.path(DIR_SALIDA, "Fig_20B_Venezuela_Subindices_1995_2024.png"), p20b, width = 9.5, height = 5.8, dpi = 300, bg = "white")
message("✔ Exportada: Fig_20B_Venezuela_Subindices_1995_2024.png")
