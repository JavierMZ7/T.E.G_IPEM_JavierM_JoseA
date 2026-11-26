# ==============================================================================
# FIGURA COMPUESTA DE VALIDEZ EXTERNA: IMPE vs. PIB NOMINAL Y PPA (2024)
# ==============================================================================
# Diseñada específicamente para insertarse justo después del párrafo de
# Validez Externa (Objetivo Específico 4) en la Sección 4.3 del Capítulo IV.
#
# Combina en una sola lámina editorial (300 dpi) sin paquetes extra complejos:
#   - Panel A: Rank PIB Nominal vs. Rank IMPE 2A_Bayes (N = 199 economías) con
#              franja sombreada de ±15 puestos (58 alineadas, 60 sobre-rinden,
#              81 sub-rinden) y etiquetas de los casos citados en el texto.
#   - Panel B: Contraste de correlación de Spearman (PIB Nominal vs. PIB PPA)
#              en 2B_Bayes y 2A_Bayes (prueba visual de la brecha PPA).
#
# INSUMO REQUERIDO:
#   - IMPE_Data_Complementaria_Cap4.xlsx
# ==============================================================================

paquetes <- c("readxl", "dplyr", "tidyr", "ggplot2", "ggrepel", "grid")
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) install.packages(faltantes, dependencies = TRUE)
invisible(lapply(paquetes, library, character.only = TRUE))

RUTA_COMP  <- "IMPE_Data_Complementaria_Cap4.xlsx"
DIR_SALIDA <- "Figuras_TEG_Cap4"

stopifnot("No se encontró IMPE_Data_Complementaria_Cap4.xlsx" = file.exists(RUTA_COMP))
if (!dir.exists(DIR_SALIDA)) dir.create(DIR_SALIDA, recursive = TRUE)

COL_NAVY    <- "#1B365D"
COL_TEAL    <- "#0F766E"
COL_CRIMSON <- "#B91C1C"
COL_AMBER   <- "#D97706"
COL_SLATE   <- "#475569"

df_div <- read_excel(RUTA_COMP, sheet = "5_Divergencias_IMPE_vs_PIB_24") %>%
  filter(!is.na(Rank_PIB_Nominal), !is.na(Rank_IMPE_2A_Bayes))

df_cor <- read_excel(RUTA_COMP, sheet = "4_Validez_Externa_Cor_PIB") %>%
  filter(Indice %in% c("2B_Base12_Depurada25_Bayes", "2A_Base12_Completa_Bayes"))

# ------------------------------------------------------------------------------
# PANEL A: RANK PIB NOMINAL vs. RANK IMPE 2A_Bayes (CON FRANJA ±15 PUESTOS)
# ------------------------------------------------------------------------------
casos_etiqueta <- c("USA", "CHN", "HKG", "SGP", "CHE", "IND", "RUS", "BRA", "MEX", "IDN", "ARG", "VEN", "NGA")

df_et <- df_div %>%
  filter(iso3c %in% casos_etiqueta) %>%
  mutate(
    Etiq = if_else(
      iso3c == "USA", "USA (0)",
      sprintf("%s (%+d)", iso3c, as.integer(Brecha_Rank_2A_vs_PIBNom))
    )
  )

franja <- tibble(
  x    = seq(1, 200, by = 1),
  ymin = pmax(1, x - 15),
  ymax = pmin(215, x + 15)
)

pA <- ggplot(df_div, aes(x = Rank_PIB_Nominal, y = Rank_IMPE_2A_Bayes)) +
  geom_ribbon(data = franja, aes(x = x, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "#CBD5E1", alpha = 0.35) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = COL_NAVY, linewidth = 0.7) +
  geom_point(aes(color = Perfil_Estructural_2A), size = 2.4, alpha = 0.85) +
  geom_text_repel(
    data = df_et,
    aes(label = Etiq, color = Perfil_Estructural_2A),
    size = 3.0, fontface = "bold", max.overlaps = 30, box.padding = 0.35, show.legend = FALSE
  ) +
  annotate("text", x = 148, y = 22, label = "SOBRE-RINDEN (+15 pts)\n60 economías",
           color = COL_TEAL, fontface = "bold", size = 3.1) +
  annotate("text", x = 45, y = 195, label = "SUB-RINDEN (-15 pts)\n81 economías",
           color = COL_CRIMSON, fontface = "bold", size = 3.1) +
  scale_x_reverse(limits = c(202, 0), breaks = c(1, 50, 100, 150, 200)) +
  scale_y_reverse(limits = c(215, 0), breaks = c(1, 50, 100, 150, 200)) +
  scale_color_manual(
    values = c(
      "Sobre-rinde en Poder Estructural (+15 puestos vs PIB)" = COL_TEAL,
      "Alineado con tamaño PIB"                               = COL_NAVY,
      "Sub-rinde en Poder Estructural (-15 puestos vs PIB)"   = COL_CRIMSON
    ),
    labels = c(
      "Sobre-rinde en Poder Estructural (+15 puestos vs PIB)" = "Sobre-rinde (n = 60)",
      "Alineado con tamaño PIB"                               = "Alineado ±15 (n = 58)",
      "Sub-rinde en Poder Estructural (-15 puestos vs PIB)"   = "Sub-rinde (n = 81)"
    ),
    name = NULL
  ) +
  labs(
    title = "A. Divergencia de Rankings: PIB Nominal vs. IMPE 2A_Bayes (2024)",
    subtitle = "N = 199 economías con PIB 2024 (franja gris = alineación ±15 puestos)",
    x = "Posición en PIB Nominal 2024 (1 = Mayor PIB)",
    y = "Posición en IMPE 2A_Bayes 2024 (1 = Mayor Poder)"
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    plot.title       = element_text(face = "bold", color = COL_NAVY, size = 11.5),
    plot.subtitle    = element_text(color = "#334155", size = 9),
    axis.title       = element_text(face = "bold", color = "#1E293B", size = 9.5),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.text      = element_text(size = 8.8)
  )

# ------------------------------------------------------------------------------
# PANEL B: CORRELACIONES SPEARMAN — PIB NOMINAL vs. PIB PPA (2B y 2A)
# ------------------------------------------------------------------------------
df_cor_long <- df_cor %>%
  transmute(
    Modelo = if_else(Indice == "2B_Base12_Depurada25_Bayes", "2B_Bayes\n(N = 54)", "2A_Bayes\n(N = 215)"),
    `PIB Nominal` = Spearman_IMPE_vs_PIB_Nom,
    `PIB PPA`     = Spearman_IMPE_vs_PIB_PPA
  ) %>%
  pivot_longer(cols = c(`PIB Nominal`, `PIB PPA`), names_to = "Referencia", values_to = "Spearman")

pB <- ggplot(df_cor_long, aes(x = Modelo, y = Spearman, fill = Referencia)) +
  geom_col(position = position_dodge(0.68), width = 0.60, alpha = 0.92) +
  geom_text(
    aes(label = sprintf("%.4f", Spearman)),
    position = position_dodge(0.68), vjust = -0.5, size = 3.2, fontface = "bold", color = "#1E293B"
  ) +
  scale_fill_manual(values = c("PIB Nominal" = COL_NAVY, "PIB PPA" = COL_AMBER), name = NULL) +
  scale_y_continuous(limits = c(0, 0.88), breaks = seq(0, 0.8, 0.2)) +
  labs(
    title = "B. Correlación Spearman (1995–2024)",
    subtitle = "Menor ajuste con PIB PPA confirma\nbrecha tamaño vs. excedente neto",
    x = NULL,
    y = "Coeficiente de Spearman (ρ)"
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    plot.title       = element_text(face = "bold", color = COL_NAVY, size = 11.5),
    plot.subtitle    = element_text(color = "#334155", size = 9),
    axis.title       = element_text(face = "bold", color = "#1E293B", size = 9.5),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

# ------------------------------------------------------------------------------
# EXPORTACIÓN COMBINADA EN UNA SOLA LÁMINA (SIN PAQUETES EXTRA)
# ------------------------------------------------------------------------------
ruta_out <- file.path(DIR_SALIDA, "Fig_10B_Validez_Externa_Rankings_y_Correlaciones_PIB_2024.png")
png(ruta_out, width = 12.2, height = 6.2, units = "in", res = 300, bg = "white")
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2, widths = unit(c(2.15, 1), "null"), heights = unit(c(1, 0.06), "null"))))
print(pA, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pB, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
grid.text(
  "Fuente: Elaboración propia con base en IMPE_Data_Complementaria_Cap4.xlsx (hojas 4_Validez_Externa_Cor_PIB y 5_Divergencias_IMPE_vs_PIB_24).",
  vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2),
  x = 0.01, hjust = 0, gp = gpar(fontsize = 8.5, col = "#64748B")
)
dev.off()

message("✔ Exportada figura compuesta lista para Sección 4.3: ", ruta_out)
