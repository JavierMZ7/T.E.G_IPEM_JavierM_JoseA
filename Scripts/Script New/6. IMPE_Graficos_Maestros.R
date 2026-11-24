# ==============================================================================
# SCRIPT MAESTRO DE VISUALIZACIÓN — IMPE (1995-2024) | 20 ÍNDICES (4 FAMILIAS x 5 MÉTODOS)
# ==============================================================================
# Entradas principales (exactas de tu flujo IMPE):
#   1. IMPE_Comparativa_Maestra_20_Indices.xlsx
#   2. Auditoria_IMPE.xlsx
#
# Entradas opcionales (para los gráficos a nivel de variable/subíndice por país):
#   - Si ejecutas este script en la misma sesión de R donde corriste
#     IMPE_consolidado_maestro.R, o si guardas los paneles estandarizados
#     (ver SECCIÓN 0B), se activan automáticamente:
#       * Plot 1B: Distribución real vs. imputada por variable individual
#       * Plot 3B: Evolución de scores de los 3 subíndices en los 20 índices
#       * Plot 7 : Comparativa de los 3 subíndices por país (CHN, USA, JPN, GBR)
#   - Si solo tienes los dos archivos Excel, el script NO falla: genera
#     todos los gráficos respaldados por el Excel y te muestra las 2 líneas
#     exactas para exportar los paneles cuando quieras activar 1B, 3B y 7.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PAQUETES Y CONFIGURACIÓN GENERAL
# ------------------------------------------------------------------------------
paquetes_requeridos <- c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr", "forcats",
  "ggplot2", "ggrepel", "scales", "patchwork",
  "plotly", "htmlwidgets", "scatterplot3d", "viridis"
)

instalar_si_falta <- function(pkgs) {
  faltantes <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(faltantes) > 0) {
    message("Instalando paquetes faltantes: ", paste(faltantes, collapse = ", "))
    install.packages(faltantes, dependencies = TRUE)
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

instalar_si_falta(paquetes_requeridos)

# --- RUTAS DE ENTRADA Y SALIDA (ajusta si los archivos están en otra carpeta) ---
RUTA_EXCEL_MAESTRO   <- "IMPE_Comparativa_Maestra_20_Indices.xlsx"
RUTA_EXCEL_AUDITORIA <- "Auditoria_IMPE.xlsx"
CARPETA_SALIDA       <- "graficos_IMPE"

# Opcional: archivo .rds con una lista nombrada de los 20 data.frames
# (nombres = los 20 códigos de Indice, columnas = iso3c, country, year + variables).
# Ejemplo al final de IMPE_consolidado_maestro.R:
#   saveRDS(LISTA_PANELES_20, "paneles_20_indices.rds")
RUTA_PANELES_RDS <- "paneles_20_indices.rds"

# Países objetivo para las comparativas individuales (Plots 6 y 7)
PAISES_OBJETIVO <- c(
  CHN = "China",
  USA = "Estados Unidos",
  JPN = "Japón",
  GBR = "Reino Unido (Inglaterra)"
)

# Cantidad de variables de mayor impacto a seleccionar por cada componente (Plot 2)
TOP_VARS_POR_CP <- 5

# Crear subcarpetas ordenadas de salida
subcarpetas <- c(
  "01_distribucion_real_vs_imputada",
  "02_pca_3d_componentes",
  "03_subindices_20_indices",
  "04_pca_cp1_vs_cp2",
  "05_fiabilidad_alfa_kmo",
  "06_evolucion_por_pais",
  "07_subindices_por_pais",
  "08_seleccion_indice_optimo"
)
dir.create(CARPETA_SALIDA, showWarnings = FALSE, recursive = TRUE)
for (sc in subcarpetas) {
  dir.create(file.path(CARPETA_SALIDA, sc), showWarnings = FALSE, recursive = TRUE)
}

# ------------------------------------------------------------------------------
# 0A. ESTILO GRÁFICO UNIFICADO, PALETAS Y CATÁLOGOS
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

ETIQUETAS_METODOS <- c(
  "1_Raw_Pairwise" = "Raw (Pairwise)",
  "2_Interp_Max3"  = "Interp. (≤3 años)",
  "3_MICE_PMM"     = "MICE · PMM",
  "4_MICE_Bayes"   = "MICE · Bayesiano",
  "5_MICE_RF"      = "MICE · Random Forest"
)

PALETA_FAMILIAS <- c(
  "1A_Base16_Completa"   = "#1B4F72",
  "1B_Base16_Depurada25" = "#2E86C1",
  "2A_Base12_Completa"   = "#B9770E",
  "2B_Base12_Depurada25" = "#1E8449"
)

PALETA_METODOS <- c(
  "1_Raw_Pairwise" = "#2C3E50",
  "2_Interp_Max3"  = "#7F8C8D",
  "3_MICE_PMM"     = "#E67E22",
  "4_MICE_Bayes"   = "#2980B9",
  "5_MICE_RF"      = "#27AE60"
)

PALETA_SUBINDICES <- c(
  "Productivo"  = "#1F618D",
  "Financiero"  = "#B7950B",
  "Tecnológico" = "#1E8449"
)

clasificar_subindice <- function(variable) {
  dplyr::case_when(
    str_starts(variable, "prod_") ~ "Productivo",
    str_starts(variable, "fin_")  ~ "Financiero",
    str_starts(variable, "tech_") ~ "Tecnológico",
    TRUE ~ "Otro"
  )
}

tema_impe <- function(base_size = 11) {
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

guardar_png <- function(plot_obj, subcarpeta, nombre_archivo, width = 12, height = 7.5, dpi = 300) {
  ruta <- file.path(CARPETA_SALIDA, subcarpeta, nombre_archivo)
  ggsave(filename = ruta, plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white")
  message("✔ Guardado: ", ruta)
}

# ------------------------------------------------------------------------------
# 1. CARGA Y NORMALIZACIÓN DE DATOS DESDE LOS DOS EXCEL
# ------------------------------------------------------------------------------
stopifnot(
  "No se encontró IMPE_Comparativa_Maestra_20_Indices.xlsx en el directorio de trabajo." = file.exists(RUTA_EXCEL_MAESTRO),
  "No se encontró Auditoria_IMPE.xlsx en el directorio de trabajo." = file.exists(RUTA_EXCEL_AUDITORIA)
)

message("\nCargando hojas de ", RUTA_EXCEL_MAESTRO, " y ", RUTA_EXCEL_AUDITORIA, "...")

df_estadisticos <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "1_Estadisticos_Indices") %>%
  mutate(
    Familia       = factor(Familia, levels = ORDEN_FAMILIAS),
    Metodo        = factor(Metodo, levels = ORDEN_METODOS),
    Familia_Label = factor(ETIQUETAS_FAMILIAS[as.character(Familia)], levels = ETIQUETAS_FAMILIAS),
    Metodo_Label  = factor(ETIQUETAS_METODOS[as.character(Metodo)], levels = ETIQUETAS_METODOS)
  ) %>%
  arrange(Familia, Metodo)

ORDEN_INDICES <- df_estadisticos$Indice

df_pca_largo <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "4_PCA_Detalle_Completo") %>%
  mutate(
    Indice        = factor(Indice, levels = ORDEN_INDICES),
    Familia       = factor(Familia, levels = ORDEN_FAMILIAS),
    Metodo        = factor(Metodo, levels = ORDEN_METODOS),
    Familia_Label = factor(ETIQUETAS_FAMILIAS[as.character(Familia)], levels = ETIQUETAS_FAMILIAS),
    Metodo_Label  = factor(ETIQUETAS_METODOS[as.character(Metodo)], levels = ETIQUETAS_METODOS),
    Subindice     = factor(clasificar_subindice(Variable), levels = c("Productivo", "Financiero", "Tecnológico"))
  )

df_congruencia <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "5_Congruencia_Rankings") %>%
  mutate(Indice = str_remove(Indice, "^Rank_"))

df_mat_spearman <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "6_Matriz_Cor_Rankings") %>%
  mutate(Indice = str_remove(Indice, "^Rank_"))

df_cor_anual <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "7_Cor_Rankings_Por_Anio") %>%
  mutate(
    Indice_1 = str_remove(Indice_1, "^Rank_"),
    Indice_2 = str_remove(Indice_2, "^Rank_")
  )

df_rangos_fam <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "8_Rangos_Pais_Familia") %>%
  mutate(
    Familia       = factor(Familia, levels = ORDEN_FAMILIAS),
    Familia_Label = factor(ETIQUETAS_FAMILIAS[as.character(Familia)], levels = ETIQUETAS_FAMILIAS)
  )

df_scores100_ancho <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "9_Scores100_20_Indices")
df_scoresZ_ancho   <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "10_ScoresZ_20_Indices")
df_ranks_ancho     <- read_excel(RUTA_EXCEL_MAESTRO, sheet = "11_Ranks_20_Indices")

# Pasar Scores_100 y Scores_Z a formato largo y unir metadatos de Familia/Método
df_scores100 <- df_scores100_ancho %>%
  pivot_longer(
    cols      = starts_with("Score100_"),
    names_to  = "Indice",
    values_to = "Score_100"
  ) %>%
  mutate(Indice = str_remove(Indice, "^Score100_")) %>%
  left_join(
    df_estadisticos %>% select(Indice, Familia, Metodo, Familia_Label, Metodo_Label),
    by = "Indice"
  ) %>%
  mutate(Indice = factor(Indice, levels = ORDEN_INDICES))

df_scoresZ <- df_scoresZ_ancho %>%
  pivot_longer(
    cols      = starts_with("ScoreZ_"),
    names_to  = "Indice",
    values_to = "Score_Z"
  ) %>%
  mutate(Indice = str_remove(Indice, "^ScoreZ_")) %>%
  left_join(
    df_estadisticos %>% select(Indice, Familia, Metodo, Familia_Label, Metodo_Label),
    by = "Indice"
  ) %>%
  mutate(Indice = factor(Indice, levels = ORDEN_INDICES))

# Extraer sesgo de autovalor (Δλ pairwise vs. complete) desde Auditoria_IMPE.xlsx
df_auditoria <- read_excel(RUTA_EXCEL_AUDITORIA, sheet = "Reporte")

df_delta_lambda <- df_auditoria %>%
  filter(
    str_detect(Seccion, "prcomp vs eigen"),
    str_detect(Item, "pairwise vs complete")
  ) %>%
  mutate(
    Indice       = str_trim(str_extract(Item, "^[^|]+")),
    Delta_Lambda = as.numeric(str_extract(Detalle, "(?<==)[0-9]+\\.[0-9]+"))
  ) %>%
  select(Indice, Delta_Lambda) %>%
  distinct(Indice, .keep_all = TRUE)

# Tabla maestra unificada para diagnóstico del índice óptimo
df_diagnostico <- df_estadisticos %>%
  left_join(df_congruencia, by = "Indice") %>%
  left_join(df_delta_lambda, by = "Indice")

# ==============================================================================
# PLOT 1: DISTRIBUCIÓN REAL VS. IMPUTADA EN CADA UNO DE LOS 20 ÍNDICES
# ==============================================================================
message("\n--- [1/8] Generando gráficos de distribución Real vs. Imputada ---")

# Para cada familia, identificamos qué filas (iso3c, year) tenían dato REAL en _Raw
mapa_real_raw <- df_scores100 %>%
  filter(Metodo == "1_Raw_Pairwise") %>%
  transmute(iso3c, year, Familia, Tiene_Dato_Real = !is.na(Score_100))

df_dist_real_imp <- df_scores100 %>%
  filter(!is.na(Score_100)) %>%
  left_join(mapa_real_raw, by = c("iso3c", "year", "Familia")) %>%
  mutate(
    Tipo_Obs = case_when(
      Metodo == "1_Raw_Pairwise" ~ "Real (observado en Raw)",
      Tiene_Dato_Real            ~ "Observado (presente en Raw)",
      TRUE                       ~ "Rescatado / Imputado (era NA en Raw)"
    ),
    Tipo_Obs = factor(
      Tipo_Obs,
      levels = c(
        "Real (observado en Raw)",
        "Observado (presente en Raw)",
        "Rescatado / Imputado (era NA en Raw)"
      )
    )
  )

# 1A-i: Densidad comparativa de los 20 índices (observado vs. imputado en cada panel)
p1a_20 <- ggplot(df_dist_real_imp, aes(x = Score_100, fill = Tipo_Obs, color = Tipo_Obs)) +
  geom_density(alpha = 0.35, linewidth = 0.65) +
  facet_grid(Familia_Label ~ Metodo_Label, scales = "free_y") +
  scale_fill_manual(
    values = c(
      "Real (observado en Raw)"              = "#1B4F72",
      "Observado (presente en Raw)"          = "#2E86C1",
      "Rescatado / Imputado (era NA en Raw)" = "#E67E22"
    ),
    name = "Origen de la observación"
  ) +
  scale_color_manual(
    values = c(
      "Real (observado en Raw)"              = "#1B4F72",
      "Observado (presente en Raw)"          = "#2E86C1",
      "Rescatado / Imputado (era NA en Raw)" = "#D35400"
    ),
    name = "Origen de la observación"
  ) +
  labs(
    title    = "Distribución Real vs. Imputada del IMPE (Score 0–100) en los 20 Índices",
    subtitle = "Comparación entre observaciones con dato real en Raw y observaciones rescatadas por interpolación o MICE",
    x        = "Score IMPE (escala 0–100)",
    y        = "Densidad",
    caption  = "Fuente: Elaboración propia con base en IMPE_Comparativa_Maestra_20_Indices.xlsx (hoja 9_Scores100_20_Indices)."
  ) +
  tema_impe(base_size = 10)

guardar_png(p1a_20, "01_distribucion_real_vs_imputada", "01A_distribucion_real_vs_imputada_20_indices.png", width = 15, height = 9.5)

# 1A-ii: Superposición de densidades de los 5 métodos dentro de cada familia (Raw vs. Imputados)
p1a_fam <- ggplot(df_scores100 %>% filter(!is.na(Score_100)),
                  aes(x = Score_100, color = Metodo_Label, fill = Metodo_Label)) +
  geom_density(alpha = 0.12, linewidth = 0.85) +
  facet_wrap(~ Familia_Label, ncol = 2, scales = "free_y") +
  scale_color_manual(values = setNames(PALETA_METODOS, ETIQUETAS_METODOS), name = "Tratamiento de NAs") +
  scale_fill_manual(values = setNames(PALETA_METODOS, ETIQUETAS_METODOS), name = "Tratamiento de NAs") +
  labs(
    title    = "Distribución Superpuesta de los 5 Métodos por Familia de Índices (Score 0–100)",
    subtitle = "Evaluación visual de si la imputación desplaza o sesga la distribución respecto a Raw (Pairwise)",
    x        = "Score IMPE (escala 0–100)",
    y        = "Densidad",
    caption  = "Nota: Un solapamiento estrecho entre Raw y los métodos MICE indica baja distorsión distribucional."
  ) +
  tema_impe()

guardar_png(p1a_fam, "01_distribucion_real_vs_imputada", "01B_densidades_metodos_por_familia.png", width = 13, height = 8.5)

# ==============================================================================
# PLOT 2: GRÁFICO TRIDIMENSIONAL (CP1, CP2, CP3) DE VARIABLES DE MAYOR IMPACTO
# ==============================================================================
message("\n--- [2/8] Generando gráficos 3D de CP1, CP2 y CP3 ---")

# Alinear signos de CP2 y CP3 frente a un índice de referencia (evita saltos
# arbitrarios de signo +v / -v propios de eigen()/prcomp() entre los 20 índices)
ref_indice <- "1A_Base16_Completa_Raw"
ref_pca <- df_pca_largo %>%
  filter(Indice == ref_indice) %>%
  select(Variable, CP1_ref = CP1, CP2_ref = CP2, CP3_ref = CP3)

signos_alineacion <- df_pca_largo %>%
  left_join(ref_pca, by = "Variable") %>%
  group_by(Indice) %>%
  summarise(
    s1 = ifelse(sum(CP1 * CP1_ref, na.rm = TRUE) < 0, -1, 1),
    s2 = ifelse(sum(CP2 * CP2_ref, na.rm = TRUE) < 0, -1, 1),
    s3 = ifelse(sum(CP3 * CP3_ref, na.rm = TRUE) < 0, -1, 1),
    .groups = "drop"
  )

df_pca_alin <- df_pca_largo %>%
  left_join(signos_alineacion, by = "Indice") %>%
  mutate(
    CP1_a = CP1 * s1,
    CP2_a = CP2 * s2,
    CP3_a = CP3 * s3
  )

# Identificar las variables de mayor impacto promedio en cada una de las 3 componentes
top_cp1 <- df_pca_alin %>% group_by(Variable) %>% summarise(m = mean(abs(CP1_a)), .groups = "drop") %>% slice_max(m, n = TOP_VARS_POR_CP) %>% mutate(Comp_Dominante = "Mayor impacto en CP1")
top_cp2 <- df_pca_alin %>% group_by(Variable) %>% summarise(m = mean(abs(CP2_a)), .groups = "drop") %>% slice_max(m, n = TOP_VARS_POR_CP) %>% mutate(Comp_Dominante = "Mayor impacto en CP2")
top_cp3 <- df_pca_alin %>% group_by(Variable) %>% summarise(m = mean(abs(CP3_a)), .groups = "drop") %>% slice_max(m, n = TOP_VARS_POR_CP) %>% mutate(Comp_Dominante = "Mayor impacto en CP3")

df_top_vars <- bind_rows(top_cp1, top_cp2, top_cp3) %>%
  group_by(Variable) %>%
  summarise(Comp_Dominante = paste(unique(Comp_Dominante), collapse = " / "), .groups = "drop")

df_pca_3d <- df_pca_alin %>%
  inner_join(df_top_vars, by = "Variable")

# Centroides por variable (para trazar vectores desde el origen en 3D)
df_centroides_3d <- df_pca_3d %>%
  group_by(Variable, Subindice, Comp_Dominante) %>%
  summarise(
    CP1_m = mean(CP1_a, na.rm = TRUE),
    CP2_m = mean(CP2_a, na.rm = TRUE),
    CP3_m = mean(CP3_a, na.rm = TRUE),
    .groups = "drop"
  )

# 2A: 3D Interactivo (HTML con plotly — rotar, hacer zoom y ver cada índice al pasar el cursor)
fig_3d <- plot_ly() %>%
  add_markers(
    data   = df_pca_3d,
    x      = ~CP1_a, y = ~CP2_a, z = ~CP3_a,
    color  = ~Variable,
    symbol = ~Familia,
    size   = ~Peso_CP1,
    sizes  = c(25, 110),
    text   = ~paste0(
      "<b>Variable:</b> ", Variable, " (", Subindice, ")<br>",
      "<b>Rol:</b> ", Comp_Dominante, "<br>",
      "<b>Índice:</b> ", Indice, "<br>",
      "<b>CP1:</b> ", round(CP1_a, 4),
      " | <b>CP2:</b> ", round(CP2_a, 4),
      " | <b>CP3:</b> ", round(CP3_a, 4), "<br>",
      "<b>Peso CP1:</b> ", percent(Peso_CP1, accuracy = 0.01)
    ),
    hoverinfo = "text",
    marker = list(opacity = 0.85)
  )

# Agregar vectores promedio desde el origen (0,0,0) hacia el centroide de cada variable clave
for (i in seq_len(nrow(df_centroides_3d))) {
  fila <- df_centroides_3d[i, ]
  fig_3d <- fig_3d %>%
    add_trace(
      type = "scatter3d",
      mode = "lines+text",
      x    = c(0, fila$CP1_m),
      y    = c(0, fila$CP2_m),
      z    = c(0, fila$CP3_m),
      text = c("", fila$Variable),
      textposition = "top center",
      textfont = list(size = 10, color = "#17202A"),
      line = list(width = 3, color = "#566573", dash = "dot"),
      showlegend = FALSE,
      hoverinfo = "none"
    )
}

fig_3d <- fig_3d %>%
  layout(
    title = list(
      text = "Espacio PCA 3D (CP1, CP2, CP3): Variables de Mayor Impacto en los 20 Índices",
      font = list(size = 15)
    ),
    scene = list(
      xaxis = list(title = "CP1 (Eje principal de Poder Económico)"),
      yaxis = list(title = "CP2 (Segunda componente, signo alineado)"),
      zaxis = list(title = "CP3 (Tercera componente, signo alineado)")
    ),
    legend = list(orientation = "v", x = 1.02, y = 0.95)
  )

ruta_3d_html <- file.path(CARPETA_SALIDA, "02_pca_3d_componentes", "02A_PCA_3D_interactivo_top_variables.html")
htmlwidgets::saveWidget(fig_3d, file = normalizePath(ruta_3d_html, mustWork = FALSE), selfcontained = TRUE)
message("✔ Guardado (interactivo 3D): ", ruta_3d_html)

# 2B: Versión 3D estática (PNG 300 dpi lista para insertar en el documento del T.E.G.)
ruta_3d_png <- file.path(CARPETA_SALIDA, "02_pca_3d_componentes", "02B_PCA_3D_estatico_top_variables.png")
vars_unicas <- sort(unique(df_pca_3d$Variable))
colores_vars <- setNames(viridis::turbo(length(vars_unicas)), vars_unicas)
pch_fam <- c("1A_Base16_Completa" = 16, "1B_Base16_Depurada25" = 17, "2A_Base12_Completa" = 15, "2B_Base12_Depurada25" = 18)

png(ruta_3d_png, width = 3400, height = 2400, res = 300)
par(mar = c(4, 4, 4, 11), xpd = TRUE)
s3d <- scatterplot3d::scatterplot3d(
  x = df_pca_3d$CP1_a,
  y = df_pca_3d$CP2_a,
  z = df_pca_3d$CP3_a,
  color = colores_vars[df_pca_3d$Variable],
  pch   = pch_fam[as.character(df_pca_3d$Familia)],
  cex.symbols = 1.15,
  angle = 50,
  grid  = TRUE,
  box   = TRUE,
  main  = "Comportamiento 3D de CP1, CP2 y CP3 — Variables de Mayor Impacto (20 Índices)",
  xlab  = "CP1 (Autovector 1)",
  ylab  = "CP2 (Autovector 2, alineado)",
  zlab  = "CP3 (Autovector 3, alineado)"
)
# Etiquetar centroides de cada variable
coords_c <- s3d$xyz.convert(df_centroides_3d$CP1_m, df_centroides_3d$CP2_m, df_centroides_3d$CP3_m)
text(coords_c$x, coords_c$y, labels = df_centroides_3d$Variable, cex = 0.72, pos = 3, font = 2, col = "#17202A")
legend("topright", inset = c(-0.28, 0.02), legend = vars_unicas, col = colores_vars, pch = 16, cex = 0.78, title = "Variable clave", bty = "n")
legend("bottomright", inset = c(-0.28, 0.05), legend = names(pch_fam), pch = pch_fam, col = "#2C3E50", cex = 0.72, title = "Familia", bty = "n")
dev.off()
message("✔ Guardado (estático 3D): ", ruta_3d_png)

# ==============================================================================
# PLOT 3: COMPORTAMIENTO DE LOS SUBÍNDICES EN LOS 20 ÍNDICES / 4 FAMILIAS
# ==============================================================================
message("\n--- [3/8] Generando gráficos de comportamiento de Subíndices (Productivo, Financiero, Tecnológico) ---")

df_subindices_peso <- df_pca_alin %>%
  group_by(Indice, Familia, Familia_Label, Metodo, Metodo_Label, Subindice) %>%
  summarise(
    Peso_Total_CP1  = sum(Peso_CP1, na.rm = TRUE),
    Carga_Media_CP1 = mean(CP1_a, na.rm = TRUE),
    Carga_Media_CP2 = mean(abs(CP2_a), na.rm = TRUE),
    N_Vars_Sub      = n(),
    .groups = "drop"
  )

# 3A-i: Composición porcentual de los 3 subíndices en el peso total CP1 de cada uno de los 20 índices
p3a_barras <- ggplot(df_subindices_peso, aes(x = Indice, y = Peso_Total_CP1, fill = Subindice)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.25) +
  geom_text(
    aes(label = percent(Peso_Total_CP1, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    size = 2.7, color = "white", fontface = "bold"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.03))) +
  scale_fill_manual(values = PALETA_SUBINDICES, name = "Subíndice / Dimensión") +
  coord_flip() +
  labs(
    title    = "Peso Estructural de los Subíndices en Cada Uno de los 20 Índices (Σ Peso CP1)",
    subtitle = "Contribución de las dimensiones Productiva (prod_), Financiera (fin_) y Tecnológica (tech_) al índice agregado",
    x        = NULL,
    y        = "Participación en la suma de pesos de CP1 (Σ v² = 100%)",
    caption  = "Fuente: Elaboración propia con base en 4_PCA_Detalle_Completo (IMPE_Comparativa_Maestra_20_Indices.xlsx)."
  ) +
  tema_impe(base_size = 10)

guardar_png(p3a_barras, "03_subindices_20_indices", "03A_peso_subindices_en_20_indices.png", width = 12.5, height = 9)

# 3A-ii: Estabilidad de los pesos por subíndice a través de los 5 métodos dentro de cada familia
p3a_lineas <- ggplot(df_subindices_peso, aes(x = Metodo_Label, y = Peso_Total_CP1, color = Subindice, group = Subindice)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.8) +
  facet_wrap(~ Familia_Label, ncol = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = PALETA_SUBINDICES, name = "Subíndice / Dimensión") +
  labs(
    title    = "Estabilidad del Peso de los Subíndices entre Métodos de Imputación (por Familia)",
    subtitle = "Permite verificar si algún método (PMM, Bayes, RF) altera el equilibrio entre pilares frente a Raw",
    x        = "Método de tratamiento de NAs",
    y        = "Peso acumulado en CP1 (%)"
  ) +
  tema_impe() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

guardar_png(p3a_lineas, "03_subindices_20_indices", "03B_estabilidad_subindices_por_familia.png", width = 13, height = 8)

# ==============================================================================
# PLOT 4: DIAGRAMA CP1 VS. CP2 POR VARIABLE A TRAVÉS DE LOS 20 ÍNDICES
# ==============================================================================
message("\n--- [4/8] Generando diagramas de distribución CP1 vs. CP2 ---")

df_centroides_2d <- df_pca_alin %>%
  group_by(Variable, Subindice) %>%
  summarise(
    CP1_m = mean(CP1_a, na.rm = TRUE),
    CP2_m = mean(CP2_a, na.rm = TRUE),
    SD_CP1 = sd(CP1_a, na.rm = TRUE),
    SD_CP2 = sd(CP2_a, na.rm = TRUE),
    .groups = "drop"
  )

# 4A: Nube consolidada de las 20 posiciones de cada variable en el plano (CP1, CP2)
p4a_nube <- ggplot(df_pca_alin, aes(x = CP1_a, y = CP2_a, color = Subindice)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#ABB2B9") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#ABB2B9") +
  geom_point(aes(shape = Familia_Label), alpha = 0.65, size = 2.2) +
  geom_segment(
    data = df_centroides_2d,
    aes(x = 0, y = 0, xend = CP1_m, yend = CP2_m, color = Subindice),
    arrow = arrow(length = unit(0.18, "cm")), linewidth = 0.75, alpha = 0.85, inherit.aes = FALSE
  ) +
  ggrepel::geom_label_repel(
    data = df_centroides_2d,
    aes(x = CP1_m, y = CP2_m, label = Variable, color = Subindice),
    size = 3.0, fontface = "bold", fill = "white", alpha = 0.9,
    max.overlaps = 30, show.legend = FALSE, inherit.aes = FALSE
  ) +
  scale_color_manual(values = PALETA_SUBINDICES, name = "Subíndice") +
  scale_shape_discrete(name = "Familia") +
  labs(
    title    = "Distribución de las Componentes Principales (CP1 vs. CP2) por Variable en los 20 Índices",
    subtitle = "Cada punto representa la posición de una variable en uno de los 20 índices; las flechas señalan su centroide promedio",
    x        = "Primera Componente Principal (CP1 — Autovector unitario)",
    y        = "Segunda Componente Principal (CP2 — Signo alineado a 1A_Raw)",
    caption  = "Nota: Nubes compactas alrededor de cada flecha indican alta estabilidad estructural de la variable entre índices."
  ) +
  tema_impe()

guardar_png(p4a_nube, "04_pca_cp1_vs_cp2", "04A_distribucion_CP1_vs_CP2_consolidado.png", width = 13, height = 9)

# 4B: Faceta individual para cada uno de los 20 índices (matriz 4 Familias x 5 Métodos)
p4b_20 <- ggplot(df_pca_alin, aes(x = CP1_a, y = CP2_a, color = Subindice)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "#BDC3C7") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "#BDC3C7") +
  geom_segment(aes(x = 0, y = 0, xend = CP1_a, yend = CP2_a), alpha = 0.45, linewidth = 0.45) +
  geom_point(size = 1.9) +
  ggrepel::geom_text_repel(
    aes(label = str_remove(Variable, "^(prod_|fin_|tech_)")),
    size = 2.1, max.overlaps = 20, segment.alpha = 0.35, show.legend = FALSE
  ) +
  facet_grid(Familia_Label ~ Metodo_Label) +
  scale_color_manual(values = PALETA_SUBINDICES, name = "Subíndice") +
  labs(
    title    = "Plano Factorial CP1 vs. CP2 en Cada Uno de los 20 Índices",
    subtitle = "Posición exacta de cada variable (etiquetas abreviadas sin prefijo) por Familia y Método de imputación",
    x        = "CP1",
    y        = "CP2 (signo alineado)"
  ) +
  tema_impe(base_size = 9.5)

guardar_png(p4b_20, "04_pca_cp1_vs_cp2", "04B_CP1_vs_CP2_panel_20_indices.png", width = 16.5, height = 11)

# ==============================================================================
# PLOT 5: ALFAS DE CRONBACH Y KMO EN LOS 20 ÍNDICES
# ==============================================================================
message("\n--- [5/8] Generando gráficos de Alfa de Cronbach y KMO ---")

df_fiab_largo <- df_estadisticos %>%
  select(Indice, Familia, Familia_Label, Metodo_Label, Alfa_Cronbach, KMO_Global, KMO_Min_Individual) %>%
  pivot_longer(
    cols      = c(Alfa_Cronbach, KMO_Global, KMO_Min_Individual),
    names_to  = "Metrica",
    values_to = "Valor"
  ) %>%
  mutate(
    Metrica = factor(
      Metrica,
      levels = c("Alfa_Cronbach", "KMO_Global", "KMO_Min_Individual"),
      labels = c("Alfa de Cronbach (α)", "KMO Global", "KMO Mínimo Individual")
    )
  )

umbrales_fiab <- tibble(
  Metrica = factor(
    c("Alfa de Cronbach (α)", "Alfa de Cronbach (α)", "KMO Global", "KMO Global", "KMO Mínimo Individual"),
    levels = c("Alfa de Cronbach (α)", "KMO Global", "KMO Mínimo Individual")
  ),
  Umbral = c(0.80, 0.90, 0.80, 0.90, 0.50),
  Etiqueta = c("Bueno (0.80)", "Excelente (0.90)", "Meritorio (0.80)", "Óptimo (0.90)", "Mínimo aceptable (0.50)")
)

# 5A: Lollipop comparativo de las 3 métricas de fiabilidad/adecuación en los 20 índices
p5a_barras <- ggplot(df_fiab_largo, aes(x = fct_rev(Indice), y = Valor, color = Familia_Label)) +
  geom_segment(aes(xend = fct_rev(Indice), y = 0, yend = Valor), linewidth = 0.75, alpha = 0.6) +
  geom_point(size = 2.9) +
  geom_text(aes(label = sprintf("%.3f", Valor)), hjust = -0.25, size = 2.5, fontface = "bold", show.legend = FALSE) +
  geom_hline(data = umbrales_fiab, aes(yintercept = Umbral), linetype = "dashed", color = "#C0392B", linewidth = 0.5) +
  facet_wrap(~ Metrica, ncol = 3) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1.08), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = setNames(PALETA_FAMILIAS, ETIQUETAS_FAMILIAS), name = "Familia") +
  labs(
    title    = "Fiabilidad y Adecuación Muestral: Alfa de Cronbach y KMO en los 20 Índices",
    subtitle = "Líneas rojas punteadas: α = 0.80/0.90, KMO Global = 0.80/0.90 y KMO Mínimo Individual = 0.50",
    x        = NULL,
    y        = "Valor del estadístico",
    caption  = "Nota: Solo las familias Base 12 (2A y 2B) superan el umbral mínimo KMO ≥ 0.50 en todas sus variables individuales."
  ) +
  tema_impe(base_size = 9.5)

guardar_png(p5a_barras, "05_fiabilidad_alfa_kmo", "05A_alfa_cronbach_y_kmo_20_indices.png", width = 15, height = 9)

# 5B: Mapa bivariado Alfa de Cronbach vs. KMO Global (tamaño = KMO mínimo individual)
p5b_scatter <- ggplot(df_estadisticos, aes(x = Alfa_Cronbach, y = KMO_Global, color = Familia_Label, shape = Metodo_Label)) +
  annotate("rect", xmin = 0.90, xmax = Inf, ymin = 0.80, ymax = Inf, fill = "#D4EFDF", alpha = 0.4) +
  geom_vline(xintercept = 0.90, linetype = "dashed", color = "#1E8449", linewidth = 0.5) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "#1E8449", linewidth = 0.5) +
  geom_point(aes(size = KMO_Min_Individual), alpha = 0.9) +
  ggrepel::geom_text_repel(
    aes(label = paste0(str_extract(Indice, "^[12][AB]"), "_", str_extract(Indice, "[^_]+$"))),
    size = 2.8, max.overlaps = 25, show.legend = FALSE
  ) +
  scale_color_manual(values = setNames(PALETA_FAMILIAS, ETIQUETAS_FAMILIAS), name = "Familia") +
  scale_shape_discrete(name = "Método") +
  scale_size_continuous(range = c(2.5, 7.5), name = "KMO Mín. Individual") +
  labs(
    title    = "Mapa Conjunto: Alfa de Cronbach vs. KMO Global (20 Índices)",
    subtitle = "Zona sombreada verde: α ≥ 0.90 y KMO Global ≥ 0.80 | Tamaño del punto proporcional al KMO mínimo individual",
    x        = "Alfa de Cronbach (consistencia interna)",
    y        = "KMO Global (adecuación muestral)"
  ) +
  tema_impe()

guardar_png(p5b_scatter, "05_fiabilidad_alfa_kmo", "05B_mapa_alfa_vs_kmo.png", width = 12, height = 8)

# ==============================================================================
# PLOT 6: EVOLUCIÓN POR PAÍS (CHINA, EE.UU., JAPÓN, REINO UNIDO) EN LOS 20 ÍNDICES
# ==============================================================================
message("\n--- [6/8] Generando gráficos individuales por país (CHN, USA, JPN, GBR) ---")

for (cod_iso in names(PAISES_OBJETIVO)) {
  nombre_pais <- PAISES_OBJETIVO[[cod_iso]]

  df_pais <- df_scores100 %>%
    filter(iso3c == cod_iso, !is.na(Score_100))

  df_banda <- df_rangos_fam %>%
    filter(iso3c == cod_iso)

  # Panel superior: las 20 series superpuestas en un solo eje (coloreadas por familia)
  p_superpuesto <- ggplot(df_pais, aes(x = year, y = Score_100, group = Indice, color = Familia_Label, linetype = Metodo_Label)) +
    geom_line(linewidth = 0.8, alpha = 0.85) +
    scale_color_manual(values = setNames(PALETA_FAMILIAS, ETIQUETAS_FAMILIAS), name = "Familia") +
    scale_linetype_discrete(name = "Método") +
    scale_x_continuous(breaks = seq(1995, 2024, by = 5)) +
    labs(
      title    = paste0(nombre_pais, " (", cod_iso, "): Evolución del IMPE en los 20 Índices (1995–2024)"),
      subtitle = "Panel superior: las 20 trayectorias superpuestas | Panel inferior: desglose por familia con banda [Mín–Máx] entre métodos",
      x        = NULL,
      y        = "Score IMPE (0–100)"
    ) +
    tema_impe(base_size = 10)

  # Panel inferior: 4 facetas por familia con banda de rango [Min_Score, Max_Score]
  p_facetado <- ggplot(df_pais, aes(x = year, y = Score_100)) +
    geom_ribbon(
      data = df_banda,
      aes(x = year, ymin = Min_Score, ymax = Max_Score),
      fill = "#D6EAF8", alpha = 0.55, inherit.aes = FALSE
    ) +
    geom_line(aes(color = Metodo_Label), linewidth = 0.9) +
    facet_wrap(~ Familia_Label, ncol = 4) +
    scale_color_manual(values = setNames(PALETA_METODOS, ETIQUETAS_METODOS), name = "Método de imputación") +
    scale_x_continuous(breaks = seq(1995, 2024, by = 10)) +
    labs(
      x       = "Año",
      y       = "Score IMPE (0–100)",
      caption = "Fuente: Hojas 8_Rangos_Pais_Familia y 9_Scores100_20_Indices (IMPE_Comparativa_Maestra_20_Indices.xlsx)."
    ) +
    tema_impe(base_size = 9.5)

  p_pais_final <- p_superpuesto / p_facetado + plot_layout(heights = c(1.15, 1))
  guardar_png(
    p_pais_final,
    "06_evolucion_por_pais",
    paste0("06_evolucion_20_indices_", cod_iso, ".png"),
    width = 14, height = 10
  )
}

# ==============================================================================
# PLOT 7 (Y EXTENSIÓN DE 1B / 3B): SUBÍNDICES POR PAÍS Y DISTRIBUCIÓN POR VARIABLE
# ==============================================================================
# NOTA IMPORTANTE:
# El Excel IMPE_Comparativa_Maestra_20_Indices.xlsx exporta los scores agregados
# (Score100, ScoreZ, Rank) y los autovectores/pesos por variable, pero NO incluye
# las series individuales de cada variable ni de cada subíndice por país-año.
#
# Si deseas generar automáticamente:
#   - Plot 1B: Distribución real vs. imputada por variable individual
#   - Plot 3B: Evolución global de los 3 subíndices en los 20 índices
#   - Plot 7 : Comparativa entre los 20 índices de los 3 subíndices para CHN, USA, JPN, GBR
# solo guarda tu lista de 20 paneles al final de IMPE_consolidado_maestro.R así:
#   saveRDS(LISTA_PANELES_20, "paneles_20_indices.rds")
# (donde cada elemento tiene columnas iso3c, country, year y las variables z/transformadas).
# ==============================================================================
message("\n--- [7/8] Verificando disponibilidad de paneles por variable para Subíndices por país ---")

cargar_paneles_opcionales <- function() {
  if (exists("LISTA_PANELES_20", envir = .GlobalEnv)) {
    message("✔ Detectado objeto LISTA_PANELES_20 en el Environment de R.")
    return(get("LISTA_PANELES_20", envir = .GlobalEnv))
  }
  if (file.exists(RUTA_PANELES_RDS)) {
    message("✔ Cargando paneles desde ", RUTA_PANELES_RDS)
    return(readRDS(RUTA_PANELES_RDS))
  }
  return(NULL)
}

paneles_20 <- cargar_paneles_opcionales()

if (!is.null(paneles_20)) {
  # Calcular los 3 subíndices (Productivo, Financiero, Tecnológico) para cada uno de los 20 índices
  # usando los pesos exactos Peso_CP1 de 4_PCA_Detalle_Completo renormalizados dentro de cada pilar
  calcular_subindices_panel <- function(nombre_indice, df_panel) {
    pesos_idx <- df_pca_largo %>%
      filter(as.character(Indice) == nombre_indice) %>%
      group_by(Subindice) %>%
      mutate(Peso_Sub = Peso_CP1 / sum(Peso_CP1, na.rm = TRUE)) %>%
      ungroup()

    vars_disp <- intersect(pesos_idx$Variable, names(df_panel))
    if (length(vars_disp) == 0) return(NULL)

    # Estandarizar Z-score pooled por si el panel viene sin estandarizar
    df_z <- df_panel %>%
      mutate(across(all_of(vars_disp), ~ as.numeric(scale(.x))))

    df_largo <- df_z %>%
      select(iso3c, country, year, all_of(vars_disp)) %>%
      pivot_longer(cols = all_of(vars_disp), names_to = "Variable", values_to = "Valor_Z") %>%
      left_join(pesos_idx %>% select(Variable, Subindice, Peso_Sub), by = "Variable") %>%
      group_by(iso3c, country, year, Subindice) %>%
      summarise(
        Subindice_Z = if (all(is.na(Valor_Z))) NA_real_ else sum(Valor_Z * Peso_Sub, na.rm = TRUE) / sum(Peso_Sub[!is.na(Valor_Z)]),
        .groups = "drop"
      ) %>%
      group_by(Subindice) %>%
      mutate(
        Subindice_100 = 100 * (Subindice_Z - min(Subindice_Z, na.rm = TRUE)) /
          (max(Subindice_Z, na.rm = TRUE) - min(Subindice_Z, na.rm = TRUE)),
        Indice = nombre_indice
      ) %>%
      ungroup()

    df_largo
  }

  df_subindices_todos <- imap_dfr(paneles_20, ~ calcular_subindices_panel(.y, .x)) %>%
    left_join(df_estadisticos %>% select(Indice, Familia, Metodo, Familia_Label, Metodo_Label), by = "Indice") %>%
    mutate(Indice = factor(Indice, levels = ORDEN_INDICES))

  # Plot 7: 1 gráfico por país comparando los 3 subíndices a través de los 20 índices
  for (cod_iso in names(PAISES_OBJETIVO)) {
    nombre_pais <- PAISES_OBJETIVO[[cod_iso]]
    df_sub_pais <- df_subindices_todos %>%
      filter(iso3c == cod_iso, !is.na(Subindice_100))

    p7_pais <- ggplot(df_sub_pais, aes(x = year, y = Subindice_100, color = Metodo_Label, group = Indice)) +
      geom_line(linewidth = 0.85, alpha = 0.9) +
      facet_grid(Subindice ~ Familia_Label) +
      scale_color_manual(values = setNames(PALETA_METODOS, ETIQUETAS_METODOS), name = "Método") +
      scale_x_continuous(breaks = seq(1995, 2024, by = 10)) +
      labs(
        title    = paste0(nombre_pais, " (", cod_iso, "): Comparativa de Subíndices en los 20 Índices (1995–2024)"),
        subtitle = "Evolución de los pilares Productivo, Financiero y Tecnológico reescalados 0–100 según pesos CP1 por pilar",
        x        = "Año",
        y        = "Score del Subíndice (0–100)"
      ) +
      tema_impe(base_size = 9.5)

    guardar_png(p7_pais, "07_subindices_por_pais", paste0("07_subindices_20_indices_", cod_iso, ".png"), width = 14.5, height = 9.5)
  }

  # Plot 3C (bonus): Evolución de la mediana mundial de los 3 subíndices por familia/método
  df_sub_global <- df_subindices_todos %>%
    group_by(year, Familia_Label, Metodo_Label, Subindice) %>%
    summarise(Mediana_100 = median(Subindice_100, na.rm = TRUE), .groups = "drop")

  p3c_global <- ggplot(df_sub_global, aes(x = year, y = Mediana_100, color = Metodo_Label)) +
    geom_line(linewidth = 0.85) +
    facet_grid(Subindice ~ Familia_Label) +
    scale_color_manual(values = setNames(PALETA_METODOS, ETIQUETAS_METODOS), name = "Método") +
    labs(
      title    = "Evolución de la Mediana Mundial de los 3 Subíndices en los 20 Índices (1995–2024)",
      x        = "Año",
      y        = "Mediana del Subíndice (0–100)"
    ) +
    tema_impe(base_size = 9.5)

  guardar_png(p3c_global, "03_subindices_20_indices", "03C_evolucion_global_subindices_20_indices.png", width = 14.5, height = 9.5)

} else {
  message(
    "ℹ AVISO (Plot 7 - Subíndices por país): No se encontró 'paneles_20_indices.rds' ni 'LISTA_PANELES_20' en memoria.\n",
    "  El Excel maestro solo exporta el Score agregado (no los subíndices individuales por país-año).\n",
    "  Para generar automáticamente los 4 gráficos de Plot 7, agrega al final de IMPE_consolidado_maestro.R:\n",
    "      saveRDS(LISTA_PANELES_20, 'paneles_20_indices.rds')\n",
    "  y vuelve a ejecutar este script."
  )
}

# ==============================================================================
# PLOT 8: SELECCIÓN Y JUSTIFICACIÓN DEL ÍNDICE ÓPTIMO DE PODER ECONÓMICO
# ==============================================================================
message("\n--- [8/8] Generando gráficos de diagnóstico para selección del Índice Óptimo ---")

# Normalización min-max auxiliar [0, 100] para construir el Scorecard Multicriterio
norm_0_100 <- function(x, invertir = FALSE) {
  r <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  if (invertir) r <- 1 - r
  100 * r
}

df_scorecard <- df_diagnostico %>%
  mutate(
    S_VarCP1     = norm_0_100(Var_Explicada_CP1_Pct),
    S_Alfa       = norm_0_100(Alfa_Cronbach),
    S_KMO_Global = norm_0_100(KMO_Global),
    S_KMO_Min    = norm_0_100(KMO_Min_Individual),
    S_Congruencia = norm_0_100(Spearman_Promedio_Otros),
    S_BajoSesgo  = norm_0_100(Delta_Lambda, invertir = TRUE),
    # Índice compuesto de calidad metodológica (promedio ponderado equitativo de los 6 criterios)
    Score_Calidad_Global = rowMeans(
      cbind(S_VarCP1, S_Alfa, S_KMO_Global, S_KMO_Min, S_Congruencia, S_BajoSesgo),
      na.rm = TRUE
    ),
    Cumple_Umbrales = (Alfa_Cronbach >= 0.80 & KMO_Global >= 0.80 & KMO_Min_Individual >= 0.50)
  ) %>%
  arrange(desc(Score_Calidad_Global)) %>%
  mutate(Indice_Rank = factor(Indice, levels = rev(Indice)))

# 8A: Heatmap + Ranking Multicriterio de los 20 índices
df_heat_sc <- df_scorecard %>%
  select(
    Indice_Rank, Cumple_Umbrales, Score_Calidad_Global,
    `Var. CP1 (%)`        = S_VarCP1,
    `Alfa Cronbach`       = S_Alfa,
    `KMO Global`          = S_KMO_Global,
    `KMO Mín. Ind.`       = S_KMO_Min,
    `Congruencia (ρ)`     = S_Congruencia,
    `Bajo Sesgo (1/Δλ)`   = S_BajoSesgo
  ) %>%
  pivot_longer(
    cols      = c(`Var. CP1 (%)`, `Alfa Cronbach`, `KMO Global`, `KMO Mín. Ind.`, `Congruencia (ρ)`, `Bajo Sesgo (1/Δλ)`),
    names_to  = "Criterio",
    values_to = "Puntaje"
  ) %>%
  mutate(
    Criterio = factor(Criterio, levels = c(
      "Var. CP1 (%)", "Alfa Cronbach", "KMO Global",
      "KMO Mín. Ind.", "Congruencia (ρ)", "Bajo Sesgo (1/Δλ)"
    ))
  )

p8a_heat <- ggplot(df_heat_sc, aes(x = Criterio, y = Indice_Rank, fill = Puntaje)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.0f", Puntaje)), size = 3, fontface = "bold", color = "#17202A") +
  scale_fill_gradientn(colors = c("#E74C3C", "#F9E79F", "#27AE60"), limits = c(0, 100), name = "Puntaje (0–100)") +
  labs(
    title    = "Scorecard Multicriterio: Selección del Índice Óptimo (20 Índices)",
    subtitle = "Ordenados de mayor a menor puntaje compuesto (0–100 normalizado en 6 criterios estadísticos y de auditoría)",
    x        = NULL,
    y        = NULL
  ) +
  tema_impe(base_size = 9.5) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "none")

p8a_bar <- ggplot(df_scorecard, aes(x = Score_Calidad_Global, y = Indice_Rank, fill = Cumple_Umbrales)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = sprintf("%.1f  (n=%d)", Score_Calidad_Global, N_Paises)), hjust = -0.08, size = 2.8, fontface = "bold") +
  scale_x_continuous(limits = c(0, 118)) +
  scale_fill_manual(
    values = c("TRUE" = "#1E8449", "FALSE" = "#95A5A6"),
    labels = c("TRUE" = "Sí (α≥0.80, KMO≥0.80, KMO_min≥0.50)", "FALSE" = "No (KMO_min < 0.50)"),
    name   = "¿Cumple todos los umbrales?"
  ) +
  labs(x = "Puntaje Global (0–100) y N° países", y = NULL) +
  tema_impe(base_size = 9.5) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

p8a_final <- (p8a_heat | p8a_bar) + plot_layout(widths = c(1.6, 1))
guardar_png(p8a_final, "08_seleccion_indice_optimo", "08A_scorecard_multicriterio_indice_optimo.png", width = 15.5, height = 9)

# 8B: Frontera de Eficiencia Metodológica (Varianza CP1 vs. KMO Mínimo Individual, tamaño = N° Países)
p8b_frontera <- ggplot(df_scorecard, aes(x = Var_Explicada_CP1_Pct, y = KMO_Min_Individual, color = Familia_Label, shape = Metodo_Label)) +
  annotate("rect", xmin = 50, xmax = Inf, ymin = 0.50, ymax = Inf, fill = "#D4EFDF", alpha = 0.35) +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "#C0392B", linewidth = 0.6) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "#1E8449", linewidth = 0.6) +
  geom_point(aes(size = N_Paises), alpha = 0.88) +
  ggrepel::geom_text_repel(
    aes(label = paste0(str_extract(Indice, "^[12][AB]"), "_", str_extract(Indice, "[^_]+$"))),
    size = 2.9, fontface = "bold", max.overlaps = 25, show.legend = FALSE
  ) +
  scale_color_manual(values = setNames(PALETA_FAMILIAS, ETIQUETAS_FAMILIAS), name = "Familia") +
  scale_shape_discrete(name = "Método") +
  scale_size_continuous(range = c(3, 9), name = "Cobertura (N° Países)") +
  labs(
    title    = "Frontera de Eficiencia: Poder Explicativo (CP1) vs. Adecuación Mínima (KMO Individual)",
    subtitle = "Cuadrante verde (Var. CP1 ≥ 50% y KMO mínimo ≥ 0.50) | El tamaño del círculo muestra el trade-off con la cobertura de países",
    x        = "Varianza Explicada por CP1 (%)",
    y        = "KMO Mínimo Individual (variable más débil)"
  ) +
  tema_impe()

guardar_png(p8b_frontera, "08_seleccion_indice_optimo", "08B_frontera_eficiencia_cp1_vs_kmo_min.png", width = 12.5, height = 8.5)

# 8C: Sesgo por patrón de NAs (Δλ pairwise vs. complete extraído de Auditoria_IMPE.xlsx)
p8c_sesgo <- ggplot(df_scorecard %>% filter(!is.na(Delta_Lambda)),
                    aes(x = Metodo_Label, y = Delta_Lambda, fill = Familia_Label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_text(
    aes(label = sprintf("%.3f", Delta_Lambda)),
    position = position_dodge(width = 0.8), vjust = -0.35, size = 2.7, fontface = "bold"
  ) +
  scale_fill_manual(values = setNames(PALETA_FAMILIAS, ETIQUETAS_FAMILIAS), name = "Familia") +
  labs(
    title    = "Auditoría de Sesgo por Patrón de NAs (Δλ Pairwise vs. Casos Completos)",
    subtitle = "Menor Δλ indica mayor coherencia estructural entre la matriz de correlación pairwise y la matriz completa",
    x        = "Método de tratamiento de NAs",
    y        = "Δλ (magnitud del sesgo en autovalores)",
    caption  = "Fuente: Auditoria_IMPE.xlsx (hoja Reporte, sección 4. prcomp vs eigen)."
  ) +
  tema_impe()

guardar_png(p8c_sesgo, "08_seleccion_indice_optimo", "08C_auditoria_sesgo_delta_lambda.png", width = 12.5, height = 7.5)

# 8D: Heatmap 20x20 de Correlaciones de Spearman entre Rankings + Evolución Temporal (1995-2024)
df_mat_largo <- df_mat_spearman %>%
  pivot_longer(cols = -Indice, names_to = "Indice_2", values_to = "Rho") %>%
  mutate(
    Indice_2 = str_remove(Indice_2, "^Rank_"),
    Indice   = factor(Indice, levels = ORDEN_INDICES),
    Indice_2 = factor(Indice_2, levels = rev(ORDEN_INDICES))
  )

p8d_heat_rho <- ggplot(df_mat_largo, aes(x = Indice, y = Indice_2, fill = Rho)) +
  geom_tile(color = "white", linewidth = 0.25) +
  geom_text(aes(label = sprintf("%.2f", Rho)), size = 2.1, color = "#17202A") +
  scale_fill_gradientn(colors = c("#FADBD8", "#F9E79F", "#27AE60"), limits = c(0.94, 1.00), name = "Spearman ρ") +
  labs(
    title    = "Matriz 20×20 de Congruencia de Rankings (Correlación de Spearman)",
    subtitle = "Todas las correlaciones entre los 20 índices superan ρ = 0.94, confirmando alta robustez del ordenamiento",
    x        = NULL,
    y        = NULL
  ) +
  tema_impe(base_size = 8.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

guardar_png(p8d_heat_rho, "08_seleccion_indice_optimo", "08D_matriz_spearman_20x20.png", width = 13, height = 11)

# 8E: Estabilidad temporal (1995-2024) de la congruencia promedio de cada índice con los demás
df_cor_anual_sim <- bind_rows(
  df_cor_anual %>% select(year, Indice = Indice_1, Spearman_Rho),
  df_cor_anual %>% select(year, Indice = Indice_2, Spearman_Rho)
) %>%
  group_by(year, Indice) %>%
  summarise(Rho_Medio_Anual = mean(Spearman_Rho, na.rm = TRUE), .groups = "drop") %>%
  left_join(df_estadisticos %>% select(Indice, Familia_Label, Metodo_Label), by = "Indice")

p8e_temporal <- ggplot(df_cor_anual_sim, aes(x = year, y = Rho_Medio_Anual, color = Metodo_Label)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ Familia_Label, ncol = 2) +
  scale_color_manual(values = setNames(PALETA_METODOS, ETIQUETAS_METODOS), name = "Método") +
  scale_x_continuous(breaks = seq(1995, 2024, by = 5)) +
  labs(
    title    = "Estabilidad Histórica de la Congruencia de Rankings por Año (1995–2024)",
    subtitle = "Correlación promedio de Spearman de cada índice con los otros 19 índices en cada año",
    x        = "Año",
    y        = "Spearman ρ promedio anual",
    caption  = "Fuente: Hoja 7_Cor_Rankings_Por_Anio (IMPE_Comparativa_Maestra_20_Indices.xlsx)."
  ) +
  tema_impe()

guardar_png(p8e_temporal, "08_seleccion_indice_optimo", "08E_estabilidad_temporal_congruencia.png", width = 13, height = 8.5)

# ==============================================================================
# RESUMEN FINAL EN CONSOLA
# ==============================================================================
message("\n==============================================================================")
message("✔ PROCESO COMPLETADO. Top 5 índices según Scorecard Multicriterio:")
print(
  df_scorecard %>%
    select(Indice, N_Paises, Var_Explicada_CP1_Pct, Alfa_Cronbach, KMO_Global,
           KMO_Min_Individual, Spearman_Promedio_Otros, Delta_Lambda, Score_Calidad_Global) %>%
    head(5)
)
message("Todos los gráficos fueron guardados en: ", normalizePath(CARPETA_SALIDA, mustWork = FALSE))
message("==============================================================================")
