
install.packages(c("naniar", "visdat", "moments", "scales"))


#library(ggplot2)
library(naniar)    # Análisis especializado de Missing Data
library(visdat)    # Visualización de estructura de datos
library(moments)   # Cálculo de asimetría (skewness) y curtosis



## panel_data <- read.csv("panel_data_consolidado.csv", stringsAsFactors = FALSE)


# Estandarización de las variables a Z-Scores
panel_normalizado <- panel_data %>%
  mutate(
    across(
      # 1. ¿A QUÉ COLUMNAS APLICAMOS EL Z-SCORE?
      # Seleccionamos todas las columnas numéricas, pero excluimos 'year' (el año)
      .cols = where(is.numeric) & !year, 
      
      # 2. ¿QUÉ OPERACIÓN MATEMÁTICA HACEMOS? (Fórmula del Z-score)
      # .x representa cada columna. A cada valor le restamos la media y lo dividimos por la desviación estándar.
      # na.rm = TRUE es vital para ignorar los datos faltantes que son comunes en bases del Banco Mundial.
      .fns = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE),
      
      # 3. ¿CÓMO LLAMAMOS A LAS NUEVAS COLUMNAS?
      # Esto agrega "_z" al final del nombre original (ej: tech_mnf_val_added_z).
      # Así conservas tu data cruda intacta y agregas las variables estandarizadas.
      .names = "{.col}_z"
    )
  ) %>%
select(country, iso3c, year, ends_with("_z"))

  write.csv(panel_normalizado,"panel_data_Z_Scores.csv", row.names = FALSE)
  arrow::write_parquet(panel_normalizado, sink = "panel_data_Z_Scores_parquet") 
  
  
  
  
  ## GRAFICOS DE NAS
  ## 
  ## 
  
  resumen_faltantes <- panel_data %>%
    miss_var_summary()
  
  print("--- PORCENTAJE DE DATOS FALTANTES POR VARIABLE ---")
  print(resumen_faltantes, n)
  
  # 2.2 Gráfico 1: Vista general de NAs en todo el dataset
  p1 <- vis_miss(panel_data, warn_large_data = FALSE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Mapa General de Datos Faltantes - Panel IMPE",
         subtitle = "Porcentaje de valores perdidos por columna")
  print(p1)
  
  # 2.3 Gráfico 2: Mapa de Calor (Heatmap) de Cobertura Temporal por País
  # Seleccionamos una muestra representativa de potencias y países emergentes clave
  p2 <- panel_data %>%
    # Creamos una variable que cuente cuántos indicadores están completos por país-año
    mutate(completitud_pct = (rowSums(!is.na(select(., -iso3c, -country, -year))) / 
                                (ncol(.) - 3)) * 100) %>%
    ggplot(aes(x = factor(year), y = iso3c, fill = completitud_pct)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "magma", name = "% Completitud", limits = c(0, 100)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) +
    labs(title = "Cobertura del Panel Desbalanceado (1960-2025)",
         subtitle = "Porcentaje de indicadores disponibles por país y año",
         x = "Año", y = "Código ISO3")
  print(p2)
  
  
  
  # 1. PREPARACIÓN DE LOS DATOS DE NAs
  nas_por_pais <- panel_data |>
    # Se extrae país y variables numéricas (excluyendo el año)
    dplyr::select(iso3c, tidyselect::where(is.numeric), -year) |>
    # Transformación a formato largo
    tidyr::pivot_longer(cols = -iso3c, names_to = "variable", values_to = "valor") |>
    # Agrupación y cálculo del porcentaje
    dplyr::group_by(iso3c) |>
    dplyr::summarise(pct_na = mean(is.na(valor)) * 100) |>
    # Eliminación de posibles filas vacías generadas por agrupaciones nulas
    tidyr::drop_na(iso3c)
  
  
  # 2. CURVA DE DISTRIBUCIÓN DE NAs (Gráfico de Densidad)
  grafico_distribucion <- ggplot2::ggplot(nas_por_pais, ggplot2::aes(x = pct_na)) +
    ggplot2::geom_density(fill = "#3498db", alpha = 0.6, color = "#2980b9", linewidth = 1) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = stats::median(pct_na)), 
      color = "red", 
      linetype = "dashed"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Curva de Densidad: Porcentaje de Datos Faltantes por País",
      subtitle = "La línea roja indica la mediana de NAs",
      x = "Porcentaje de Datos Faltantes (%)",
      y = "Densidad"
    )
  
  print(grafico_distribucion)
  
  
  # 3. MAPAMUNDI DE NAs
  # Carga directa del polígono espacial sin adjuntar toda la librería
  mapa_mundo <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  # Emparejamiento usando funciones específicas
  mapa_nas <- mapa_mundo |>
    dplyr::left_join(nas_por_pais, by = c("iso_a3" = "iso3c"))
  
  # Construcción de la capa geoespacial
  grafico_mapa <- ggplot2::ggplot(data = mapa_nas) +
    ggplot2::geom_sf(ggplot2::aes(fill = pct_na), color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_viridis_c(
      option = "magma", 
      direction = -1, 
      na.value = "grey80", 
      name = "% de NAs"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Distribución Geográfica de Datos Faltantes",
      subtitle = "Concentración de NAs en el panel de indicadores económicos",
      caption = "Elaboración propia"
    )
  
  print(grafico_mapa)
  
  