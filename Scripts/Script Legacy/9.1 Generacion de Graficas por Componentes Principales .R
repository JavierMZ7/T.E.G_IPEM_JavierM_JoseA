# ==============================================================================
# SCRIPT NUEVO E INDEPENDIENTE: VISUALIZACIÓN DE LAS 3 PRIMERAS COMPONENTES
# PRINCIPALES (CP1, CP2, CP3) POR VARIABLE Y POR ÍNDICE
# ==============================================================================
# Requisito previo: Haber corrido el script anterior (objetos mod_obs_16,
# mod_int_16, mod_obs_12 y mod_int_12 ya creados en memoria, o bien el archivo
# "IMPE_Comparativa_SinImputar_vs_Interpolado_R.xlsx" en tu carpeta).
#
# Nota: Utiliza exclusivamente el operador explícito `::` y el pipe nativo `|>`
#       sin cargar librerías completas con library().
# ==============================================================================

# 0. Instalación automática de ggplot2, dplyr, tidyr o readxl solo si faltan:
paquetes_viz <- c("ggplot2", "dplyr", "tidyr", "tibble", "readxl")
faltantes_viz <- paquetes_viz[!(paquetes_viz %in% utils::installed.packages()[, "Package"])]
if (base::length(faltantes_viz) > 0) {
  utils::install.packages(faltantes_viz)
}

# ==============================================================================
# PASO 1: RECUPERACIÓN AUTOMÁTICA DE LAS TABLAS PCA_3CP DE LOS 4 ÍNDICES
# ==============================================================================
# Si los objetos mod_obs_16, mod_int_16, mod_obs_12 y mod_int_12 están en tu
# Environment de RStudio (porque ya corriste el script anterior), los une directo.
# Si cerraste sesión, los lee desde el Excel generado por el script anterior.

if (base::exists("mod_obs_16") && base::exists("mod_int_16") &&
    base::exists("mod_obs_12") && base::exists("mod_int_12")) {
  
  tabla_3cp_unificada <- dplyr::bind_rows(
    mod_obs_16$pca_3cp |> dplyr::mutate(Indice = "1A. Sin Imputar (Base 16)"),
    mod_int_16$pca_3cp |> dplyr::mutate(Indice = "2A. Interpolado (Base 16)"),
    mod_obs_12$pca_3cp |> dplyr::mutate(Indice = "1B. Sin Imputar (Base 12)"),
    mod_int_12$pca_3cp |> dplyr::mutate(Indice = "2B. Interpolado (Base 12)")
  )
  
  # Si además creaste en tu sesión el modelo de Países Depurados (<= 30% NAs con PMM),
  # el script lo detecta y lo agrega automáticamente al gráfico:
  if (base::exists("mod_dep_16")) {
    tabla_3cp_unificada <- dplyr::bind_rows(
      tabla_3cp_unificada,
      mod_dep_16$pca_3cp |> dplyr::mutate(Indice = "3A. Países Depurados <=30% NA (Base 16)")
    )
  }
  if (base::exists("mod_dep_12")) {
    tabla_3cp_unificada <- dplyr::bind_rows(
      tabla_3cp_unificada,
      mod_dep_12$pca_3cp |> dplyr::mutate(Indice = "3B. Países Depurados <=30% NA (Base 12)")
    )
  }
  
} else if (base::file.exists("IMPE_Comparativa_SinImputar_vs_Interpolado_R.xlsx")) {
  
  t_obs16 <- readxl::read_excel("IMPE_Comparativa_SinImputar_vs_Interpolado_R.xlsx", sheet = "2_PCA_3CP_Sin_Imputar", skip = 4, n_max = 16) |>
    dplyr::mutate(Indice = "1A. Sin Imputar (Base 16)")
  t_int16 <- readxl::read_excel("IMPE_Comparativa_SinImputar_vs_Interpolado_R.xlsx", sheet = "3_PCA_3CP_Interpolado", skip = 4, n_max = 16) |>
    dplyr::mutate(Indice = "2A. Interpolado (Base 16)")
  t_obs12 <- readxl::read_excel("IMPE_Comparativa_SinImputar_vs_Interpolado_R.xlsx", sheet = "4_PCA_3CP_Base12_Depurado", skip = 4, n_max = 12) |>
    dplyr::mutate(Indice = "1B. Sin Imputar (Base 12)")
  t_int12 <- readxl::read_excel("IMPE_Comparativa_SinImputar_vs_Interpolado_R.xlsx", sheet = "4_PCA_3CP_Base12_Depurado", skip = 20, n_max = 12) |>
    dplyr::mutate(Indice = "2B. Interpolado (Base 12)")
  
  tabla_3cp_unificada <- dplyr::bind_rows(t_obs16, t_int16, t_obs12, t_int12)
  
} else {
  base::stop("Ejecuta primero el script anterior para tener los objetos 'mod_obs_16' y 'mod_int_16' en tu Environment.")
}


# ==============================================================================
# PASO 2: TRANSFORMACIÓN A FORMATO LARGO Y ORDENAMIENTO JERÁRQUICO DE VARIABLES
# ==============================================================================

# 2.1 Ordenamos las 16 variables de menor a mayor según su Carga en CP1 (Interpolado Base 16)
orden_variables <- tabla_3cp_unificada |>
  dplyr::filter(Indice == "2A. Interpolado (Base 16)") |>
  dplyr::arrange(Carga_CP1) |>
  dplyr::pull(Variable)

# 2.2 Reestructuramos las Cargas de CP1, CP2 y CP3 a formato largo para graficar con facet_wrap
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


# ==============================================================================
# PASO 3: GRÁFICO 1 — BARRAS AGRUPADAS DE LAS 3 COMPONENTES POR VARIABLE E ÍNDICE
# ==============================================================================

paleta_indices <- c(
  "1A. Sin Imputar (Base 16)"               = "#DC2626",
  "2A. Interpolado (Base 16)"               = "#2563EB",
  "1B. Sin Imputar (Base 12)"               = "#F59E0B",
  "2B. Interpolado (Base 12)"               = "#10B981",
  "3A. Países Depurados <=30% NA (Base 16)" = "#7C3AED",
  "3B. Países Depurados <=30% NA (Base 12)" = "#EC4899"
)

grafico_1_barras_3cp <- ggplot2::ggplot(
  data = datos_cargas_largo,
  mapping = ggplot2::aes(
    x    = Carga_Factorial,
    y    = Variable,
    fill = Indice
  )
) +
  # Banda sombrada suave en la parte inferior para señalar las 4 variables podadas en Base 12
  ggplot2::annotate(
    "rect",
    xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 4.5,
    fill = "#FEE2E2", alpha = 0.45
  ) +
  # Líneas de referencia en 0 y en umbrales de carga fuerte (+0.50 y -0.50)
  ggplot2::geom_vline(xintercept = 0, color = "#0F172A", linewidth = 0.8) +
  ggplot2::geom_vline(xintercept = c(-0.5, 0.5), color = "#64748B", linetype = "dashed", linewidth = 0.5) +
  # Barras agrupadas por índice para cada variable
  ggplot2::geom_col(
    position  = ggplot2::position_dodge(width = 0.78),
    width     = 0.72,
    color     = "#0F172A",
    linewidth = 0.25,
    alpha     = 0.92
  ) +
  ggplot2::facet_wrap(~ Componente_Etiqueta, ncol = 3) +
  ggplot2::scale_fill_manual(values = paleta_indices, name = "Índice Comparado:") +
  ggplot2::scale_x_continuous(limits = c(-0.90, 1.00), breaks = base::seq(-0.75, 0.75, by = 0.25)) +
  ggplot2::labs(
    title    = "Análisis de las 3 Primeras Componentes Principales (CP1, CP2, CP3) por Variable y por Índice",
    subtitle = "Comparación de Cargas Factoriales (r) entre el Índice Sin Imputar y el Índice Interpolado (Base 16 Completa vs. Base 12 Depurada)",
    x        = "Carga Factorial (Correlación Lineal r entre la Variable y la Componente Principal)",
    y        = "Variables del Índice (Ordenadas por Jerarquía en CP1)",
    caption  = "Nota: La franja rosada inferior identifica las 4 variables con peso < 1.5% en CP1 podadas en la versión Base 12. Elaboración propia."
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title         = ggplot2::element_text(face = "bold", size = 14, color = "#0F172A"),
    plot.subtitle      = ggplot2::element_text(size = 10.5, color = "#334155", margin = ggplot2::margin(b = 12)),
    strip.background   = ggplot2::element_rect(fill = "#1E3A8A", color = "#0F172A"),
    strip.text         = ggplot2::element_text(face = "bold", color = "#FFFFFF", size = 9.5),
    axis.text.y        = ggplot2::element_text(face = "bold", color = "#0F172A", size = 9.5),
    axis.title         = ggplot2::element_text(face = "bold", color = "#0F172A"),
    legend.position    = "bottom",
    legend.title       = ggplot2::element_text(face = "bold"),
    panel.grid.minor   = ggplot2::element_blank(),
    panel.spacing      = ggplot2::unit(1.2, "lines")
  )

# Mostrar en la pestaña Plots de RStudio y guardar en alta resolución (300 DPI)
base::print(grafico_1_barras_3cp)
ggplot2::ggsave(
  filename = "Grafico_1_Cargas_3CP_por_Variable_e_Indice_R.png",
  plot     = grafico_1_barras_3cp,
  width    = 17,
  height   = 8.5,
  dpi      = 300,
  bg       = "white"
)


# ==============================================================================
# PASO 4: GRÁFICO 2 — MAPA DE CALOR (HEATMAP) DE LAS 3 COMPONENTES POR ÍNDICE
# ==============================================================================

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
  mapping = ggplot2::aes(
    x    = Indice,
    y    = Variable,
    fill = Carga_Factorial
  )
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.7) +
  ggplot2::geom_text(
    mapping  = ggplot2::aes(label = base::sprintf("%+.2f", Carga_Factorial)),
    size     = 3.2,
    fontface = "bold",
    color    = base::ifelse(base::abs(datos_heatmap$Carga_Factorial) > 0.45, "white", "#0F172A")
  ) +
  ggplot2::facet_wrap(~ Comp_Corto, ncol = 3) +
  ggplot2::scale_fill_gradient2(
    low      = "#1D4ED8",
    mid      = "#F8FAFC",
    high     = "#B91C1C",
    midpoint = 0,
    limits   = c(-0.85, 0.85),
    name     = "Carga Factorial (r):"
  ) +
  ggplot2::labs(
    title    = "Mapa de Calor Matricial: Estabilidad de las 3 Primeras Componentes (CP1, CP2, CP3) a través de los Índices",
    subtitle = "Valores exactos de correlación variable-componente en Base 16 y Base 12 (Celdas vacías inferiores = Variables podadas en Base 12)",
    x        = "Índice / Especificación Metodológica",
    y        = "Variable del I.M.P.E.",
    caption  = "Elaboración propia."
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 13.5, color = "#0F172A"),
    plot.subtitle    = ggplot2::element_text(size = 10, color = "#334155", margin = ggplot2::margin(b = 10)),
    strip.background = ggplot2::element_rect(fill = "#0F172A", color = "#0F172A"),
    strip.text       = ggplot2::element_text(face = "bold", color = "#FFFFFF", size = 10),
    axis.text.x      = ggplot2::element_text(angle = 25, hjust = 1, face = "bold", size = 9, color = "#0F172A"),
    axis.text.y      = ggplot2::element_text(face = "bold", size = 9.5, color = "#0F172A"),
    legend.position  = "bottom",
    panel.grid       = ggplot2::element_blank()
  )

base::print(grafico_2_heatmap_3cp)
ggplot2::ggsave(
  filename = "Grafico_2_Heatmap_3CP_por_Variable_e_Indice_R.png",
  plot     = grafico_2_heatmap_3cp,
  width    = 15.5,
  height   = 8.2,
  dpi      = 300,
  bg       = "white"
)


# ==============================================================================
# PASO 5: GRÁFICO 3 — PONDERACIONES RELATIVAS (%) EN CP1, CP2, CP3 Y 3CP COMPUESTO
# ==============================================================================

datos_pesos_largo <- tabla_3cp_unificada |>
  dplyr::select(
    Indice, Variable, Dimension_Teorica,
    `1. Peso en CP1 (%)`              = Peso_CP1_Pct,
    `2. Peso en CP2 (%)`              = Peso_CP2_Pct,
    `3. Peso en CP3 (%)`              = Peso_CP3_Pct,
    `4. Peso Compuesto 3CP (OCDE, %)` = Peso_3CP_Comp_Pct
  ) |>
  tidyr::pivot_longer(
    cols      = dplyr::starts_with(c("1.", "2.", "3.", "4.")),
    names_to  = "Tipo_Peso",
    values_to = "Peso_Porcentaje"
  ) |>
  dplyr::mutate(
    Variable = base::factor(Variable, levels = orden_variables),
    Indice   = base::factor(Indice, levels = base::unique(tabla_3cp_unificada$Indice))
  )

grafico_3_pesos_3cp <- ggplot2::ggplot(
  data = datos_pesos_largo,
  mapping = ggplot2::aes(
    x    = Peso_Porcentaje,
    y    = Variable,
    fill = Indice
  )
) +
  ggplot2::geom_col(
    position  = ggplot2::position_dodge(width = 0.78),
    width     = 0.72,
    color     = "#0F172A",
    linewidth = 0.25,
    alpha     = 0.92
  ) +
  ggplot2::facet_wrap(~ Tipo_Peso, ncol = 4, scales = "free_x") +
  ggplot2::scale_fill_manual(values = paleta_indices, name = "Índice Comparado:") +
  ggplot2::labs(
    title    = "Ponderaciones Relativas (%) de cada Variable en CP1, CP2, CP3 y en el Índice Compuesto 3CP",
    subtitle = "Muestra qué variables dominan cada componente individual y cómo se equilibran en la agregación tridimensional (OCDE, 2008)",
    x        = "Peso Relativo dentro de la Componente / Índice (%)",
    y        = "Variable del I.M.P.E.",
    caption  = "Elaboración propia."
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 13.5, color = "#0F172A"),
    plot.subtitle    = ggplot2::element_text(size = 10, color = "#334155", margin = ggplot2::margin(b = 10)),
    strip.background = ggplot2::element_rect(fill = "#0F766E", color = "#0F172A"),
    strip.text       = ggplot2::element_text(face = "bold", color = "#FFFFFF", size = 9.5),
    axis.text.y      = ggplot2::element_text(face = "bold", size = 9.5, color = "#0F172A"),
    legend.position  = "bottom",
    panel.grid.minor = ggplot2::element_blank()
  )

base::print(grafico_3_pesos_3cp)
ggplot2::ggsave(
  filename = "Grafico_3_Pesos_CP1_CP2_CP3_y_Compuesto_R.png",
  plot     = grafico_3_pesos_3cp,
  width    = 17.5,
  height   = 8.5,
  dpi      = 300,
  bg       = "white"
)

base::cat("
==============================================================================
")
base::cat("¡LOS 3 GRÁFICOS DE LAS COMPONENTES PRINCIPALES SE HAN GENERADO Y GUARDADO!
")
base::cat("1. Grafico_1_Cargas_3CP_por_Variable_e_Indice_R.png
")
base::cat("2. Grafico_2_Heatmap_3CP_por_Variable_e_Indice_R.png
")
base::cat("3. Grafico_3_Pesos_CP1_CP2_CP3_y_Compuesto_R.png
")
base::cat("==============================================================================
")