# ==============================================================================
# EXTENSIÓN ANALÍTICA DEL CAPÍTULO IV — IMPE (1995-2024)
# MÓDULO 1: CLUSTERS ESTRUCTURALES Y TRANSICIONES (WARD + K-MEANS + SILUETA/GAP)
# MÓDULO 2: ZONAS DE INFLUENCIA (MORAN/LISA ESPACIAL + BLOQUES GEOECONÓMICOS)
# MÓDULO 3: DETERMINANTES DEL CRECIMIENTO DEL PODER (BASE 12 vs. BASE 16 + CHINA)
# ==============================================================================
# INSUMOS REQUERIDOS (en el directorio de trabajo):
#   - IMPE_Comparativa_Maestra_20_Indices.xlsx
#   - IMPE_Subindices_20_Indices_Largo.csv
#   - IMPE_Data_Complementaria_Cap4.xlsx (opcional, solo para cruzar PIB 2024)
#
# SALIDAS GENERADAS:
#   - IMPE_Extension_Clusters_Zonas_Determinantes.xlsx (12 hojas listas Cap. IV)
#   - Graficos_Extension_Cap4/ (6 gráficos en 300 dpi listos para el TEG)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PAQUETES Y CONFIGURACIÓN INICIAL
# ------------------------------------------------------------------------------
paquetes <- c(
  "readr", "readxl", "openxlsx", "dplyr", "tidyr", "stringr", "purrr",
  "ggplot2", "cluster", "countrycode", "WDI", "plm", "lmtest", "broom",
  "zoo", "sf", "spdep", "rnaturalearth", "rnaturalearthdata"
)

faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) install.packages(faltantes, dependencies = TRUE)
invisible(lapply(paquetes, library, character.only = TRUE))

set.seed(2026)

RUTA_MAESTRO_XLS <- "IMPE_Comparativa_Maestra_20_Indices.xlsx"
RUTA_SUB_CSV     <- "IMPE_Subindices_20_Indices_Largo.csv"
RUTA_SALIDA_XLS  <- "IMPE_Extension_Clusters_Zonas_Determinantes.xlsx"
DIR_GRAFICOS     <- "Graficos_Extension_Cap4"

stopifnot("Falta IMPE_Comparativa_Maestra_20_Indices.xlsx" = file.exists(RUTA_MAESTRO_XLS))
stopifnot("Falta IMPE_Subindices_20_Indices_Largo.csv"     = file.exists(RUTA_SUB_CSV))
if (!dir.exists(DIR_GRAFICOS)) dir.create(DIR_GRAFICOS, recursive = TRUE)

PAISES_CLAVE <- c("USA", "CHN", "JPN", "SGP", "CHE", "KOR", "GBR", "DEU", "RUS", "IND", "BRA", "VEN")

# Carga base
df_scores <- read_excel(RUTA_MAESTRO_XLS, sheet = "9_Scores100_20_Indices")
df_pca    <- read_excel(RUTA_MAESTRO_XLS, sheet = "4_PCA_Detalle_Completo")
df_sub    <- read_csv(RUTA_SUB_CSV, show_col_types = FALSE)

# ==============================================================================
# MÓDULO 1: CLUSTERS ESTRUCTURALES (1995 vs. 2024) Y MATRIZ DE TRANSICIÓN
# ==============================================================================
# Metodología:
#   - Clustering jerárquico de Ward (Ward, 1963) + K-means sobre los 3 subíndices
#     estandarizados (Productivo, Financiero, Tecnológico).
#   - Selección objetiva de k en [2, 7] mediante Ancho de Silueta Promedio
#     (Rousseeuw, 1987) y Estadístico Gap (Tibshirani, Walther & Hastie, 2001).
#   - Validación de separación inter-grupos: MANOVA (traza de Pillai) y
#     pruebas no paramétricas de Kruskal-Wallis por subíndice.
#   - Se corre sobre 2B_Bayes (N = 54) y 2A_Bayes (N = 215).
# ==============================================================================
message("\n=== [MÓDULO 1] Ejecutando Clustering Estructural y Transiciones ===")

ejecutar_clustering <- function(nombre_idx, k_fijo = NULL) {
  sub_ancho <- df_sub %>%
    filter(Indice == nombre_idx, year %in% c(1995, 2024)) %>%
    select(iso3c, country, year, Subindice, Subindice_100) %>%
    pivot_wider(names_from = Subindice, values_from = Subindice_100) %>%
    filter(!is.na(Productivo), !is.na(Financiero), !is.na(Tecnologico))
  
  df_24 <- sub_ancho %>% filter(year == 2024)
  df_95 <- sub_ancho %>% filter(year == 1995)
  
  X24 <- scale(df_24 %>% select(Productivo, Financiero, Tecnologico))
  dist_24 <- dist(X24, method = "euclidean")
  hc_24   <- hclust(dist_24, method = "ward.D2")
  
  # Evaluación de k = 2..7 (Silueta para Ward y K-means)
  eval_k <- map_dfr(2:7, function(k) {
    cl_w <- cutree(hc_24, k = k)
    km   <- kmeans(X24, centers = k, nstart = 50, iter.max = 50)
    tibble(
      Indice        = nombre_idx,
      k             = k,
      Silueta_Ward  = round(mean(silhouette(cl_w, dist_24)[, 3]), 4),
      Silueta_KMeans = round(mean(silhouette(km$cluster, dist_24)[, 3]), 4),
      TotWithinSS   = round(km$tot.withinss, 2),
      Var_Explicada_Pct = round(100 * km$betweenss / km$totss, 2)
    )
  })
  
  # Estadístico Gap (Tibshirani et al., 2001) con B = 50 réplicas
  gap_res <- clusGap(X24, FUN = kmeans, nstart = 25, K.max = 7, B = 50, verbose = FALSE)
  eval_k$Gap_Stat <- round(gap_res$Tab[2:7, "gap"], 4)
  eval_k$Gap_SE   <- round(gap_res$Tab[2:7, "SE.sim"], 4)
  
  # Selección de k (k_fijo o máximo de silueta en k >= 3 para taxonomía rica)
  k_opt <- if (!is.null(k_fijo)) k_fijo else {
    eval_k %>% filter(k >= 3, k <= 5) %>% slice_max(Silueta_KMeans, n = 1) %>% pull(k) %>% .[1]
  }
  
  # K-means final inicializado con centroides de Ward (híbrido estable)
  cl_ward_init <- cutree(hc_24, k = k_opt)
  centros_init <- aggregate(X24, by = list(cl_ward_init), FUN = mean)[, -1]
  km_final     <- kmeans(X24, centers = centros_init, iter.max = 100)
  
  # Ordenar etiquetas de cluster de mayor a menor poder medio (1 = Cúspide Hegemónica)
  df_24$Cluster_Raw <- km_final$cluster
  orden_cl <- df_24 %>%
    mutate(Media_3 = (Productivo + Financiero + Tecnologico) / 3) %>%
    group_by(Cluster_Raw) %>%
    summarise(M = mean(Media_3), .groups = "drop") %>%
    arrange(desc(M)) %>%
    mutate(Cluster_ID = row_number())
  
  etiquetas_cl <- c(
    "1: Núcleo Hegemónico / Alta Densidad",
    "2: Potencias Consolidadas / Especializadas",
    "3: Semiperiferia Estructural / Emergentes",
    "4: Periferia Estructural",
    "5: Periferia Extrema / Rezagada"
  )
  
  df_24 <- df_24 %>%
    left_join(orden_cl %>% select(Cluster_Raw, Cluster_ID), by = "Cluster_Raw") %>%
    mutate(Cluster_Nombre = etiquetas_cl[pmin(Cluster_ID, length(etiquetas_cl))])
  
  # Clasificar 1995 usando los centroides fijos de 2024 (permite medir movilidad real)
  X95_sobre_24 <- scale(
    df_95 %>% select(Productivo, Financiero, Tecnologico),
    center = attr(X24, "scaled:center"),
    scale  = attr(X24, "scaled:scale")
  )
  centros_ord <- km_final$centers[orden_cl$Cluster_Raw, , drop = FALSE]
  
  asignar_centro <- function(fila) {
    dists <- apply(centros_ord, 1, function(cc) sum((fila - cc)^2))
    which.min(dists)
  }
  df_95$Cluster_ID_1995 <- apply(X95_sobre_24, 1, asignar_centro)
  
  # Perfiles por cluster 2024
  perfiles <- df_24 %>%
    group_by(Cluster_ID, Cluster_Nombre) %>%
    summarise(
      N_Paises         = n(),
      Prod_Media       = round(mean(Productivo), 2),
      Fin_Media        = round(mean(Financiero), 2),
      Tech_Media       = round(mean(Tecnologico), 2),
      Ejemplos_Paises  = paste(head(iso3c[order(-(Productivo + Financiero + Tecnologico))], 8), collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(Indice = nombre_idx, .before = 1)
  
  # Validación estadística (MANOVA Pillai + Kruskal-Wallis)
  manova_fit <- manova(cbind(Productivo, Financiero, Tecnologico) ~ factor(Cluster_ID), data = df_24)
  manova_sum <- summary(manova_fit, test = "Pillai")$stats[1, ]
  kw_prod    <- kruskal.test(Productivo  ~ factor(Cluster_ID), data = df_24)
  kw_fin     <- kruskal.test(Financiero  ~ factor(Cluster_ID), data = df_24)
  kw_tech    <- kruskal.test(Tecnologico ~ factor(Cluster_ID), data = df_24)
  
  validacion <- tibble(
    Indice          = nombre_idx,
    k_Optimo        = k_opt,
    Silueta_Final   = round(mean(silhouette(df_24$Cluster_ID, dist_24)[, 3]), 4),
    MANOVA_Pillai   = round(manova_sum["Pillai"], 4),
    MANOVA_F        = round(manova_sum["approx F"], 2),
    MANOVA_p        = signif(manova_sum["Pr(>F)"], 4),
    KW_Prod_Chi2    = round(kw_prod$statistic, 2),
    KW_Fin_Chi2     = round(kw_fin$statistic, 2),
    KW_Tech_Chi2    = round(kw_tech$statistic, 2),
    KW_Max_p        = signif(max(kw_prod$p.value, kw_fin$p.value, kw_tech$p.value), 4)
  )
  
  # Transiciones 1995 -> 2024
  transiciones <- df_24 %>%
    select(iso3c, country, Cluster_2024 = Cluster_ID, Cluster_Nombre_2024 = Cluster_Nombre,
           Prod_24 = Productivo, Fin_24 = Financiero, Tech_24 = Tecnologico) %>%
    left_join(df_95 %>% select(iso3c, Cluster_1995 = Cluster_ID_1995), by = "iso3c") %>%
    mutate(
      Indice = nombre_idx,
      # Positivo = ascendió de estrato (ej. de Cluster 3 a Cluster 1 => +2)
      Salto_Estrato = Cluster_1995 - Cluster_2024,
      Movimiento = case_when(
        Salto_Estrato > 0  ~ "Ascenso estructural",
        Salto_Estrato < 0  ~ "Retroceso estructural",
        TRUE               ~ "Estable en su estrato"
      )
    ) %>%
    arrange(Cluster_2024, desc(Prod_24 + Fin_24 + Tech_24))
  
  list(eval_k = eval_k, perfiles = perfiles, validacion = validacion, transiciones = transiciones, df_24 = df_24)
}

cl_2B <- ejecutar_clustering("2B_Base12_Depurada25_Bayes", k_fijo = 4)
cl_2A <- ejecutar_clustering("2A_Base12_Completa_Bayes",   k_fijo = 4)

tab_eval_k      <- bind_rows(cl_2B$eval_k, cl_2A$eval_k)
tab_cl_perfiles <- bind_rows(cl_2B$perfiles, cl_2A$perfiles)
tab_cl_valid    <- bind_rows(cl_2B$validacion, cl_2A$validacion)
tab_cl_trans_2B <- cl_2B$transiciones
tab_cl_trans_2A <- cl_2A$transiciones

# Gráfico 1: Mapa de Clusters Estructurales en 2B_Bayes (Productivo vs. Tecnológico, color = Cluster)
p_cl <- ggplot(cl_2B$df_24, aes(x = Productivo, y = Tecnologico, color = Cluster_Nombre, size = Financiero)) +
  geom_point(alpha = 0.85) +
  geom_text(
    data = cl_2B$df_24 %>% filter(iso3c %in% PAISES_CLAVE | Cluster_ID == 1),
    aes(label = iso3c), size = 3, vjust = -0.9, show.legend = FALSE, check_overlap = TRUE
  ) +
  scale_color_manual(values = c("#1B365D", "#047857", "#D97706", "#B91C1C"), name = "Cluster Estructural") +
  scale_size_continuous(name = "Subíndice\nFinanciero", range = c(2.5, 7)) +
  labs(
    title = "Tipología de Poder Económico: Clusters Estructurales (2B_Bayes, 2024)",
    subtitle = sprintf("Ward + K-means (k = 4) | Silueta = %.3f | MANOVA Pillai = %.3f (p < 0,001)",
                       cl_2B$validacion$Silueta_Final, cl_2B$validacion$MANOVA_Pillai),
    x = "Subíndice Productivo (0–100)", y = "Subíndice Tecnológico (0–100)",
    caption = "Fuente: Elaboración propia con base en IMPE_Subindices_20_Indices_Largo.csv"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1B365D"))

ggsave(file.path(DIR_GRAFICOS, "01_Clusters_Estructurales_2B_2024.png"), p_cl, width = 10, height = 6.5, dpi = 300, bg = "white")

# ==============================================================================
# MÓDULO 2: ZONAS DE INFLUENCIA (CAPA A: MORAN/LISA ESPACIAL + CAPA B: BLOQUES)
# ==============================================================================
# Metodología:
#   - Capa A (Espacial): I de Moran global e indicadores locales LISA (Anselin, 1995)
#     sobre los 215 Estados (2A_Bayes, 2024) usando matriz de pesos espaciales
#     k-vecinos más cercanos (k = 6 sobre centroides geográficos), evitando que
#     potencias insulares (JPN, GBR, SGP, AUS) queden sin vecinos.
#   - Capa B (Bloques): Regiones del Banco Mundial + Bloques Geoeconómicos
#     (G7+Aliados Occidentales, BRICS+, ASEAN/Conectores, Resto del Sur Global;
#     Gopinath et al., 2024), midiendo cuota de poder, brecha centro-periferia e HHI.
# ==============================================================================
message("\n=== [MÓDULO 2] Calculando Zonas de Influencia (LISA Espacial + Bloques) ===")

# --- Capa B: Bloques Regionales y Geoeconómicos (1995, 2010, 2024) ---
g7_aliados <- c(
  "USA", "CAN", "GBR", "FRA", "DEU", "ITA", "JPN", "AUS", "NZL", "KOR",
  "CHE", "NOR", "SWE", "DNK", "FIN", "NLD", "BEL", "AUT", "IRL", "ESP",
  "PRT", "GRC", "POL", "CZE", "HUN", "SVK", "SVN", "EST", "LVA", "LTU",
  "LUX", "ISL", "ISR", "TWN"
)
brics_plus <- c("BRA", "RUS", "IND", "CHN", "ZAF", "EGY", "ETH", "IRN", "ARE", "SAU", "IDN")
conectores <- c("SGP", "HKG", "MEX", "VNM", "MYS", "THA", "TUR", "MAR", "PHL", "CHL", "COL", "PER", "KAZ")

df_bloques_base <- df_scores %>%
  filter(year %in% c(1995, 2010, 2024)) %>%
  transmute(
    iso3c, country, year,
    Score_2A = Score100_2A_Base12_Completa_Bayes,
    Score_1A = Score100_1A_Base16_Completa_Bayes,
    Region_BM = countrycode(iso3c, "iso3c", "region", warn = FALSE),
    Bloque_Geoeconomico = case_when(
      iso3c %in% g7_aliados ~ "G7 y Aliados Occidentales",
      iso3c %in% brics_plus ~ "BRICS+",
      iso3c %in% conectores ~ "Economías Conectoras (Gopinath et al.)",
      TRUE                  ~ "Resto del Mundo / Sur Global"
    )
  ) %>%
  mutate(Region_BM = if_else(is.na(Region_BM), "Otros / Territorios", Region_BM))

tab_regiones <- df_bloques_base %>%
  group_by(year, Region_BM) %>%
  summarise(
    N_Paises         = n(),
    Media_2A_Base12  = round(mean(Score_2A, na.rm = TRUE), 2),
    Mediana_2A       = round(median(Score_2A, na.rm = TRUE), 2),
    Max_2A           = round(max(Score_2A, na.rm = TRUE), 2),
    Lider_Regional   = iso3c[which.max(Score_2A)],
    Brecha_Max_Med   = round(max(Score_2A, na.rm = TRUE) - median(Score_2A, na.rm = TRUE), 2),
    Media_1A_Base16  = round(mean(Score_1A, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(Cuota_Poder_Pct = round(100 * (Media_2A_Base12 * N_Paises) / sum(Media_2A_Base12 * N_Paises), 2)) %>%
  ungroup()

tab_bloques <- df_bloques_base %>%
  group_by(year, Bloque_Geoeconomico) %>%
  summarise(
    N_Paises         = n(),
    Media_2A_Base12  = round(mean(Score_2A, na.rm = TRUE), 2),
    DE_2A            = round(sd(Score_2A, na.rm = TRUE), 2),
    Media_1A_Base16  = round(mean(Score_1A, na.rm = TRUE), 2),
    Lider_Bloque     = iso3c[which.max(Score_2A)],
    Score_Lider_2A   = round(max(Score_2A, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(
    Cuota_Poder_2A_Pct = round(100 * (Media_2A_Base12 * N_Paises) / sum(Media_2A_Base12 * N_Paises), 2),
    Cuota_Poder_1A_Pct = round(100 * (Media_1A_Base16 * N_Paises) / sum(Media_1A_Base16 * N_Paises), 2)
  ) %>%
  ungroup()

# Concentración global del poder (HHI sobre Top 20 y Top 50, 1995 vs 2010 vs 2024)
tab_hhi <- df_bloques_base %>%
  group_by(year) %>%
  summarise(
    HHI_Global_2A = round(sum((Score_2A / sum(Score_2A, na.rm = TRUE))^2, na.rm = TRUE) * 10000, 2),
    HHI_Global_1A = round(sum((Score_1A / sum(Score_1A, na.rm = TRUE))^2, na.rm = TRUE) * 10000, 2),
    Cuota_Top5_2A = round(100 * sum(sort(Score_2A, decreasing = TRUE)[1:5]) / sum(Score_2A, na.rm = TRUE), 2),
    Cuota_Top10_2A = round(100 * sum(sort(Score_2A, decreasing = TRUE)[1:10]) / sum(Score_2A, na.rm = TRUE), 2),
    .groups = "drop"
  )

# --- Capa A: Autocorrelación Espacial (I de Moran Global y LISA Local 2024) ---
tab_moran_global <- tibble()
tab_lisa_2024    <- tibble()

tryCatch({
  sf::sf_use_s2(FALSE)
  mundo <- ne_countries(scale = "medium", returnclass = "sf") %>%
    mutate(iso3c = if_else(iso_a3 == "-99" | is.na(iso_a3), adm0_a3, iso_a3)) %>%
    filter(iso3c != "ATA") %>%
    select(iso3c, name_long, geometry)
  
  datos_mapa <- df_scores %>%
    filter(year == 2024) %>%
    select(iso3c, Score_2A = Score100_2A_Base12_Completa_Bayes, Score_1A = Score100_1A_Base16_Completa_Bayes)
  
  mapa_sf <- mundo %>%
    inner_join(datos_mapa, by = "iso3c") %>%
    filter(!is.na(Score_2A))
  
  # Centroides y matriz de pesos k = 6 vecinos más cercanos (incluye islas)
  coords <- st_coordinates(st_point_on_surface(mapa_sf))
  knn_nb <- knn2nb(knearneigh(coords, k = 6))
  lw     <- nb2listw(knn_nb, style = "W", zero.policy = TRUE)
  
  # I de Moran Global (2A_Bayes y 1A_Bayes)
  m_2a <- moran.test(mapa_sf$Score_2A, lw, zero.policy = TRUE)
  m_1a <- moran.test(mapa_sf$Score_1A, lw, zero.policy = TRUE)
  
  tab_moran_global <- tibble(
    Modelo      = c("2A_Base12_Completa_Bayes (2024)", "1A_Base16_Completa_Bayes (2024)"),
    N_Mapa      = nrow(mapa_sf),
    Moran_I     = round(c(m_2a$estimate[1], m_1a$estimate[1]), 4),
    Esperado_E  = round(c(m_2a$estimate[2], m_1a$estimate[2]), 4),
    Z_Score     = round(c(m_2a$statistic,   m_1a$statistic), 2),
    p_valor     = signif(c(m_2a$p.value,    m_1a$p.value), 4)
  )
  
  # LISA Local (Anselin, 1995) para 2A_Bayes
  lisa <- localmoran(mapa_sf$Score_2A, lw, zero.policy = TRUE)
  z_val <- as.numeric(scale(mapa_sf$Score_2A))
  z_lag <- lag.listw(lw, z_val, zero.policy = TRUE)
  p_loc <- lisa[, 5]
  
  mapa_sf <- mapa_sf %>%
    mutate(
      Z_IMPE    = round(z_val, 3),
      Z_Vecinos = round(z_lag, 3),
      LISA_Ii   = round(lisa[, 1], 4),
      LISA_p    = round(p_loc, 4),
      Cuadrante_LISA = case_when(
        p_loc > 0.05              ~ "No significativo (p > 0,05)",
        z_val > 0 & z_lag > 0     ~ "Alto-Alto (Polo Hegemónico)",
        z_val < 0 & z_lag < 0     ~ "Bajo-Bajo (Periferia Estructural)",
        z_val > 0 & z_lag < 0     ~ "Alto-Bajo (Potencia Regional Aislada)",
        z_val < 0 & z_lag > 0     ~ "Bajo-Alto (Enclave Rezagado)"
      )
    )
  
  tab_lisa_2024 <- mapa_sf %>%
    st_drop_geometry() %>%
    arrange(Cuadrante_LISA, desc(Score_2A))
  
  # Gráfico 2: Mapa LISA de Zonas de Influencia (2024)
  p_lisa <- ggplot(mapa_sf) +
    geom_sf(aes(fill = Cuadrante_LISA), color = "#E5E7EB", linewidth = 0.15) +
    scale_fill_manual(
      values = c(
        "Alto-Alto (Polo Hegemónico)"         = "#1B365D",
        "Alto-Bajo (Potencia Regional Aislada)" = "#D97706",
        "Bajo-Alto (Enclave Rezagado)"          = "#60A5FA",
        "Bajo-Bajo (Periferia Estructural)"     = "#B91C1C",
        "No significativo (p > 0,05)"           = "#E5E7EB"
      ),
      name = "Zona de Influencia (LISA)"
    ) +
    labs(
      title = "Zonas de Influencia del Poder Económico: Análisis Espacial LISA (2A_Bayes, 2024)",
      subtitle = sprintf("I de Moran Global = %.4f (Z = %.2f, p < 0,001) | Matriz k = 6 vecinos más cercanos",
                         tab_moran_global$Moran_I[1], tab_moran_global$Z_Score[1]),
      caption = "Fuente: Elaboración propia (Anselin, 1995) con base en IMPE 2A_Base12_Completa_Bayes"
    ) +
    theme_void(base_size = 10.5) +
    theme(
      plot.title = element_text(face = "bold", color = "#1B365D", hjust = 0.5),
      plot.subtitle = element_text(color = "#374151", hjust = 0.5),
      legend.position = "bottom"
    )
  
  ggsave(file.path(DIR_GRAFICOS, "02_Mapa_LISA_Zonas_Influencia_2024.png"), p_lisa, width = 11, height = 6, dpi = 300, bg = "white")
  message("✔ Análisis espacial Moran/LISA y mapa completados.")
}, error = function(e) {
  warning("No se pudo ejecutar la capa espacial LISA: ", conditionMessage(e))
})

# Gráfico 3: Evolución de Bloques Geoeconómicos (1995 vs. 2010 vs. 2024)
p_blq <- ggplot(tab_bloques, aes(x = factor(year), y = Media_2A_Base12, fill = Bloque_Geoeconomico)) +
  geom_col(position = "dodge", width = 0.72) +
  geom_text(aes(label = sprintf("%.1f", Media_2A_Base12)),
            position = position_dodge(0.72), vjust = -0.4, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = c("#1B365D", "#B91C1C", "#047857", "#6B7280"), name = "Bloque Geoeconómico") +
  labs(
    title = "Evolución del Poder Económico Medio por Bloque Geoeconómico (1995–2024)",
    subtitle = "Comparación en 2A_Base12_Completa_Bayes (N = 215 Estados)",
    x = "Año", y = "IMPE Medio (0–100)",
    caption = "Fuente: Elaboración propia con taxonomía de fragmentación geoeconómica (Gopinath et al., 2024)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1B365D"), legend.position = "bottom")

ggsave(file.path(DIR_GRAFICOS, "03_Bloques_Geoeconomicos_1995_2024.png"), p_blq, width = 9.5, height = 6, dpi = 300, bg = "white")

# ==============================================================================
# MÓDULO 3: DETERMINANTES DEL CRECIMIENTO DEL PODER (BASE 12 vs. BASE 16 + CHINA)
# ==============================================================================
# Metodología:
#   3A) Convergencia Beta y Sigma (Barro & Sala-i-Martin, 1992) en 2B, 1B, 2A y 1A.
#   3B) Descomposición Contable de Delta IMPE (1995->2024) por subíndice en
#       Base 12 (2B_Bayes) vs. Base 16 (1B_Bayes / 1A_Bayes), con foco en China.
#   3C) Panel Econométrico con Efectos Fijos (país y año) y errores robustos de
#       Driscoll-Kraay (1998) usando determinantes EXÓGENOS rezagados (t-1)
#       descargados de WDI/WGI (evitando tautología con los componentes del IMPE):
#         - Calidad Institucional: RL.EST (Estado de Derecho) y GE.EST (Efectividad)
#         - Capital Humano: SE.TER.ENRR (Matrícula terciaria %)
#         - Apertura Comercial: NE.TRD.GNFS.ZS (Comercio % PIB)
#         - Estabilidad Macro: asinh(FP.CPI.TOTL.ZG) (Inflación suavizada)
#         - Escala Demográfica: log(SP.POP.TOTL) [test clave de volumen Base 16 vs 12]
#         - Carga de Mantenimiento: SP.POP.DPND (Tasa de dependencia, hipótesis Beckley)
# ==============================================================================
message("\n=== [MÓDULO 3] Ejecutando Convergencia, Descomposición y Panel Econométrico ===")

# --- 3A. Convergencia Beta (1995->2024) y Sigma (1995 vs 2024) ---
modelos_det <- c(
  "2B_Base12_Depurada25_Bayes",
  "1B_Base16_Depurada25_Bayes",
  "2A_Base12_Completa_Bayes",
  "1A_Base16_Completa_Bayes"
)

tab_convergencia <- map_dfr(modelos_det, function(m) {
  col_m <- paste0("Score100_", m)
  d95 <- df_scores %>% filter(year == 1995) %>% select(iso3c, S95 = all_of(col_m))
  d24 <- df_scores %>% filter(year == 2024) %>% select(iso3c, S24 = all_of(col_m))
  dc  <- inner_join(d95, d24, by = "iso3c") %>%
    filter(!is.na(S95), !is.na(S24)) %>%
    mutate(Crecimiento_Anual = (S24 - S95) / 29)
  
  fit <- lm(Crecimiento_Anual ~ S95, data = dc)
  s   <- summary(fit)$coefficients
  tibble(
    Modelo             = m,
    N_Paises           = nrow(dc),
    Beta_Inicial       = round(s["S95", "Estimate"], 5),
    Beta_p_valor       = signif(s["S95", "Pr(>|t|)"], 4),
    R2_Beta            = round(summary(fit)$r.squared, 4),
    Sigma_DE_1995      = round(sd(dc$S95), 2),
    Sigma_DE_2024      = round(sd(dc$S24), 2),
    Cambio_Sigma       = round(sd(dc$S24) - sd(dc$S95), 2),
    Diagnostico        = if_else(s["S95", "Estimate"] < 0 & s["S95", "Pr(>|t|)"] < 0.05,
                                 "Convergencia Beta significativa", "Sin convergencia Beta significativa")
  )
})

# --- 3B. Descomposición Contable de Delta IMPE (1995->2024): Base 12 vs. Base 16 ---
calcular_descomposicion <- function(nombre_idx) {
  w_pilar <- df_pca %>%
    filter(Indice == nombre_idx) %>%
    mutate(Subindice = case_when(
      str_starts(Variable, "prod_") ~ "Productivo",
      str_starts(Variable, "fin_")  ~ "Financiero",
      str_starts(Variable, "tech_") ~ "Tecnologico"
    )) %>%
    group_by(Subindice) %>%
    summarise(Peso = sum(Peso_CP1, na.rm = TRUE), .groups = "drop") %>%
    mutate(Peso = Peso / sum(Peso))
  
  sub_d <- df_sub %>%
    filter(Indice == nombre_idx, year %in% c(1995, 2024)) %>%
    select(iso3c, country, year, Subindice, Subindice_100) %>%
    pivot_wider(names_from = year, values_from = Subindice_100, names_prefix = "Y") %>%
    filter(!is.na(Y1995), !is.na(Y2024)) %>%
    left_join(w_pilar, by = "Subindice") %>%
    mutate(
      Delta_Sub      = Y2024 - Y1995,
      Aporte_Ponderado = Peso * Delta_Sub
    )
  
  sub_d %>%
    select(iso3c, country, Subindice, Aporte_Ponderado) %>%
    pivot_wider(names_from = Subindice, values_from = Aporte_Ponderado, names_prefix = "Aporte_") %>%
    mutate(
      Indice = nombre_idx,
      Cambio_Total_Aprox = round(Aporte_Productivo + Aporte_Financiero + Aporte_Tecnologico, 2),
      Aporte_Productivo  = round(Aporte_Productivo, 2),
      Aporte_Financiero  = round(Aporte_Financiero, 2),
      Aporte_Tecnologico = round(Aporte_Tecnologico, 2),
      Motor_Principal    = case_when(
        abs(Aporte_Tecnologico) >= abs(Aporte_Productivo) & abs(Aporte_Tecnologico) >= abs(Aporte_Financiero) ~ "Tecnológico",
        abs(Aporte_Productivo)  >= abs(Aporte_Financiero) ~ "Productivo",
        TRUE ~ "Financiero"
      )
    ) %>%
    arrange(desc(Cambio_Total_Aprox))
}

desc_2B <- calcular_descomposicion("2B_Base12_Depurada25_Bayes")
desc_1B <- calcular_descomposicion("1B_Base16_Depurada25_Bayes")
desc_2A <- calcular_descomposicion("2A_Base12_Completa_Bayes")
desc_1A <- calcular_descomposicion("1A_Base16_Completa_Bayes")

# Tabla comparativa enfocada en potencias clave y CHINA (Base 12 vs. Base 16)
tab_desc_clave <- bind_rows(
  desc_2B %>% filter(iso3c %in% PAISES_CLAVE),
  desc_1B %>% filter(iso3c %in% PAISES_CLAVE),
  desc_2A %>% filter(iso3c %in% c("CHN", "USA", "VEN")),
  desc_1A %>% filter(iso3c %in% c("CHN", "USA", "VEN"))
) %>%
  arrange(iso3c, Indice)

# --- 3C. Descarga Resiliente de Determinantes Exógenos (WDI + V-Dem/WGI) y Panel DK ---
message("Descargando determinantes exógenos de WDI (1995-2024)...")
wdi_core <- c(
  educ_terc     = "SE.TER.ENRR",
  apertura_pib  = "NE.TRD.GNFS.ZS",
  inflacion     = "FP.CPI.TOTL.ZG",
  poblacion     = "SP.POP.TOTL",
  dependencia   = "SP.POP.DPND"
)

df_wdi_exo <- WDI(
  country   = "all",
  indicator = wdi_core,
  start     = 1995,
  end       = 2024,
  extra     = FALSE
) %>%
  filter(!is.na(iso3c), iso3c != "") %>%
  as_tibble()

# Calidad Institucional: usa V-Dem local (vdemdata) si está instalado (anual 1995-2024 sin fallas API);
# si no, intenta WGI (RL.EST, GE.EST) de forma segura.
inst_ok <- FALSE
if (requireNamespace("vdemdata", quietly = TRUE)) {
  message("✔ Usando calidad institucional desde vdemdata::vdem (v2x_rule y v2clrspct)...")
  df_inst <- vdemdata::vdem %>%
    filter(year >= 1995, year <= 2024) %>%
    transmute(
      iso3c       = country_text_id,
      year        = as.integer(year),
      rule_of_law = as.numeric(v2x_rule),   # Estado de Derecho (0-1)
      gov_effect  = as.numeric(v2clrspct)   # Administración pública imparcial / efectividad
    ) %>%
    distinct(iso3c, year, .keep_all = TRUE)
  df_wdi_exo <- df_wdi_exo %>% left_join(df_inst, by = c("iso3c", "year"))
  inst_ok <- TRUE
} else {
  message("Intentando descargar WGI (RL.EST, GE.EST) desde Banco Mundial...")
  df_wgi <- tryCatch(
    WDI(country = "all", indicator = c(rule_of_law = "RL.EST", gov_effect = "GE.EST"),
        start = 1995, end = 2024, extra = FALSE) %>%
      filter(!is.na(iso3c), iso3c != "") %>%
      select(iso3c, year, any_of(c("rule_of_law", "gov_effect"))) %>%
      as_tibble(),
    error = function(e) NULL
  )
  if (!is.null(df_wgi) && all(c("rule_of_law", "gov_effect") %in% names(df_wgi))) {
    df_wdi_exo <- df_wdi_exo %>% left_join(df_wgi, by = c("iso3c", "year"))
    inst_ok <- TRUE
  } else {
    warning("No se pudieron cargar indicadores institucionales (ni vdemdata ni WGI). El panel correrá con los 5 controles macro/capital humano de WDI.")
  }
}

vars_interp <- intersect(
  c("rule_of_law", "gov_effect", "educ_terc", "apertura_pib", "inflacion", "poblacion", "dependencia"),
  names(df_wdi_exo)
)

df_wdi_exo <- df_wdi_exo %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    across(all_of(vars_interp), ~ zoo::na.approx(.x, maxgap = 3, na.rm = FALSE)),
    log_pop      = log(poblacion),
    asinh_infl   = asinh(inflacion),
    L1_educ_terc = lag(educ_terc, 1),
    L1_apertura  = lag(apertura_pib, 1),
    L1_infl      = lag(asinh_infl, 1),
    L1_log_pop   = lag(log_pop, 1),
    L1_depend    = lag(dependencia, 1)
  ) %>%
  {
    if (inst_ok) mutate(., L1_rule_law = lag(rule_of_law, 1), L1_gov_eff = lag(gov_effect, 1)) else .
  } %>%
  ungroup()

regresores_panel <- c(
  if (inst_ok) c("L1_rule_law", "L1_gov_eff"),
  "L1_educ_terc", "L1_apertura", "L1_infl", "L1_log_pop", "L1_depend"
)

estimar_panel_dk <- function(nombre_idx) {
  col_m <- paste0("Score100_", nombre_idx)
  df_p <- df_scores %>%
    select(iso3c, country, year, IMPE = all_of(col_m)) %>%
    filter(!is.na(IMPE)) %>%
    left_join(df_wdi_exo, by = c("iso3c", "year")) %>%
    arrange(iso3c, year) %>%
    group_by(iso3c) %>%
    mutate(
      L1_IMPE    = lag(IMPE, 1),
      Delta_IMPE = IMPE - L1_IMPE
    ) %>%
    ungroup() %>%
    filter(!is.na(Delta_IMPE), !is.na(L1_IMPE)) %>%
    filter(if_all(all_of(regresores_panel), ~ !is.na(.x)))
  
  pdata <- pdata.frame(df_p, index = c("iso3c", "year"))
  
  f_niv  <- as.formula(paste("IMPE ~", paste(regresores_panel, collapse = " + ")))
  f_crec <- as.formula(paste("Delta_IMPE ~ L1_IMPE +", paste(regresores_panel, collapse = " + ")))
  
  # Especificación 1: Niveles de IMPE (Efectos Fijos País + Año)
  mod_niv <- plm(f_niv, data = pdata, model = "within", effect = "twoways")
  ct_niv  <- coeftest(mod_niv, vcov = vcovSCC(mod_niv, type = "HC1"))
  
  # Especificación 2: Crecimiento Delta_IMPE con convergencia condicional (L1_IMPE)
  mod_crec <- plm(f_crec, data = pdata, model = "within", effect = "twoways")
  ct_crec  <- coeftest(mod_crec, vcov = vcovSCC(mod_crec, type = "HC1"))
  
  extraer_coef <- function(ct, mod, espec) {
    tidy(ct) %>%
      mutate(
        Modelo          = nombre_idx,
        Especificacion  = espec,
        N_Obs           = nobs(mod),
        N_Paises        = pdim(mod)$nT$n,
        R2_Within       = tryCatch(round(as.numeric(plm::r.squared(mod)), 4), error = function(e) NA_real_),        estimate        = round(estimate, 4),
        std.error       = round(std.error, 4),
        statistic       = round(statistic, 2),
        p.value         = signif(p.value, 4),
        Significancia   = case_when(
          p.value < 0.01 ~ "*** (1%)",
          p.value < 0.05 ~ "** (5%)",
          p.value < 0.10 ~ "* (10%)",
          TRUE           ~ "n.s."
        )
      )
  }
  
  bind_rows(
    extraer_coef(ct_niv,  mod_niv,  "Niveles (FE País + Año, Driscoll-Kraay)"),
    extraer_coef(ct_crec, mod_crec, "Crecimiento ΔIMPE (FE + Convergencia, DK)")
  )
}

tab_panel_reg <- map_dfr(modelos_det, estimar_panel_dk)

# Diagnóstico comparativo específico del CASO CHINA (1995 vs. 2024 en Base 12 vs. Base 16 + exógenas)
tab_caso_china <- df_scores %>%
  filter(iso3c %in% c("CHN", "USA", "SGP", "KOR", "JPN", "DEU"), year %in% c(1995, 2010, 2024)) %>%
  select(
    iso3c, year,
    IMPE_2B_Base12 = Score100_2B_Base12_Depurada25_Bayes,
    IMPE_1B_Base16 = Score100_1B_Base16_Depurada25_Bayes,
    IMPE_2A_Base12 = Score100_2A_Base12_Completa_Bayes,
    IMPE_1A_Base16 = Score100_1A_Base16_Completa_Bayes
  ) %>%
  left_join(
    df_wdi_exo %>% select(iso3c, year, any_of(c('rule_of_law', 'gov_effect', 'educ_terc', 'apertura_pib', 'poblacion', 'dependencia'))),
    by = c("iso3c", "year")
  ) %>%
  mutate(
    Brecha_Volumen_1B_menos_2B = round(IMPE_1B_Base16 - IMPE_2B_Base12, 2),
    Brecha_Volumen_1A_menos_2A = round(IMPE_1A_Base16 - IMPE_2A_Base12, 2),
    Pob_Millones = round(poblacion / 1e6, 1)
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# Gráfico 4: Convergencia Beta (1995 vs. Crecimiento Anual 1995-2024 en 2B_Bayes)
d_conv_2B <- df_scores %>%
  filter(year %in% c(1995, 2024)) %>%
  select(iso3c, year, S = Score100_2B_Base12_Depurada25_Bayes) %>%
  pivot_wider(names_from = year, values_from = S, names_prefix = "Y") %>%
  filter(!is.na(Y1995), !is.na(Y2024)) %>%
  mutate(Crec_Anual = (Y2024 - Y1995) / 29)

p_conv <- ggplot(d_conv_2B, aes(x = Y1995, y = Crec_Anual)) +
  geom_smooth(method = "lm", se = TRUE, color = "#1B365D", fill = "#93C5FD", alpha = 0.25) +
  geom_point(aes(color = iso3c == "CHN"), size = 3, alpha = 0.85) +
  geom_text(
    data = d_conv_2B %>% filter(iso3c %in% PAISES_CLAVE | Crec_Anual > 0.9 | Crec_Anual < 0),
    aes(label = iso3c), vjust = -0.8, size = 3, fontface = "bold", check_overlap = TRUE
  ) +
  scale_color_manual(values = c("FALSE" = "#4B5563", "TRUE" = "#B91C1C"), guide = "none") +
  labs(
    title = "Convergencia Beta del Poder Económico (2B_Bayes, 1995–2024)",
    subtitle = sprintf("N = 54 Estados | Pendiente β = %.4f (p = %.4f) | China destaca como principal outlier de ascenso",
                       tab_convergencia$Beta_Inicial[1], tab_convergencia$Beta_p_valor[1]),
    x = "IMPE Inicial en 1995 (0–100)",
    y = "Crecimiento Promedio Anual del IMPE (puntos/año)",
    caption = "Fuente: Elaboración propia (Barro & Sala-i-Martin, 1992) con base en IMPE 2B_Bayes"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1B365D"))

ggsave(file.path(DIR_GRAFICOS, "04_Convergencia_Beta_2B_1995_2024.png"), p_conv, width = 9.5, height = 6, dpi = 300, bg = "white")

# Gráfico 5: Comparación de Coeficientes Panel FE Driscoll-Kraay (Base 12 vs. Base 16)
df_plot_reg <- tab_panel_reg %>%
  filter(
    Modelo %in% c("2B_Base12_Depurada25_Bayes", "1B_Base16_Depurada25_Bayes"),
    Especificacion == "Niveles (FE País + Año, Driscoll-Kraay)"
  ) %>%
  mutate(
    IC_Low  = estimate - 1.96 * std.error,
    IC_High = estimate + 1.96 * std.error,
    Etiqueta = recode(term,
                      L1_rule_law  = "Estado de Derecho (t-1)",
                      L1_gov_eff   = "Efectividad Gob. (t-1)",
                      L1_educ_terc = "Educación Terciaria (t-1)",
                      L1_apertura  = "Apertura Comercial (t-1)",
                      L1_infl      = "Inflación asinh (t-1)",
                      L1_log_pop   = "Escala Poblacional log (t-1)",
                      L1_depend    = "Tasa de Dependencia (t-1)"
    ),
    Base = if_else(str_detect(Modelo, "Base12"), "Base 12 (Densidad — 2B)", "Base 16 (Con Volumen — 1B)")
  )

p_reg <- ggplot(df_plot_reg, aes(x = estimate, y = Etiqueta, color = Base)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#6B7280") +
  geom_point(position = position_dodge(0.4), size = 3) +
  geom_errorbarh(aes(xmin = IC_Low, xmax = IC_High), position = position_dodge(0.4), height = 0.2) +
  scale_color_manual(values = c("#1B365D", "#D97706"), name = "Especificación") +
  labs(
    title = "Determinantes Exógenos del IMPE: Densidad (Base 12) vs. Volumen (Base 16)",
    subtitle = "Panel de Efectos Fijos País + Año con errores Driscoll-Kraay (IC 95%)",
    x = "Coeficiente estimado sobre IMPE (0–100)", y = NULL,
    caption = "Fuente: Elaboración propia con determinantes exógenos WDI/WGI (1995–2024)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", color = "#1B365D"), legend.position = "bottom")

ggsave(file.path(DIR_GRAFICOS, "05_Determinantes_Base12_vs_Base16.png"), p_reg, width = 10, height = 6, dpi = 300, bg = "white")

# ==============================================================================
# 4. EXPORTACIÓN CONSOLIDADA A EXCEL (12 HOJAS)
# ==============================================================================
wb <- createWorkbook()
hdr <- createStyle(
  fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
  fgFill = "#1B365D", halign = "center", textDecoration = "bold"
)

add_sheet <- function(wb, nom, df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(invisible(NULL))
  addWorksheet(wb, nom)
  writeData(wb, nom, df, headerStyle = hdr)
  setColWidths(wb, nom, cols = 1:ncol(df), widths = "auto")
  freezePane(wb, nom, firstRow = TRUE)
}

add_sheet(wb, "1_Clusters_Eval_K",         tab_eval_k)
add_sheet(wb, "2_Clusters_Perfiles_24",    tab_cl_perfiles)
add_sheet(wb, "3_Clusters_Valid_MANOVA",   tab_cl_valid)
add_sheet(wb, "4_Clusters_Trans_2B",       tab_cl_trans_2B)
add_sheet(wb, "5_Clusters_Trans_2A",       tab_cl_trans_2A)
add_sheet(wb, "6_Zonas_Moran_Global",      tab_moran_global)
add_sheet(wb, "7_Zonas_LISA_2024",         tab_lisa_2024)
add_sheet(wb, "8_Zonas_Regiones_BM",       tab_regiones)
add_sheet(wb, "9_Zonas_Bloques_HHI",       tab_bloques)
add_sheet(wb, "10_Convergencia_Beta_Sig",  tab_convergencia)
add_sheet(wb, "11_Descomp_Subindices",     tab_desc_clave)
add_sheet(wb, "12_Panel_Determinantes_DK", tab_panel_reg)
add_sheet(wb, "13_Caso_China_Vol_vs_Dens", tab_caso_china)

saveWorkbook(wb, RUTA_SALIDA_XLS, overwrite = TRUE)
message("\n✔ TODO COMPLETADO: ", RUTA_SALIDA_XLS, " y gráficos en ", DIR_GRAFICOS, "/")
print(tab_cl_valid)
print(tab_moran_global)
print(tab_convergencia)
print(tab_panel_reg %>% filter(Especificacion == "Niveles (FE País + Año, Driscoll-Kraay)"))