# ==============================================================================
# SCRIPT MAESTRO DE EXPORTACIÓN A EXCEL CON FORMATO PROFESIONAL (openxlsx)
# Guarda todos los tests, diagnósticos MICE, pesos (Base 16 y 12) y paneles
# ==============================================================================

# Instalar openxlsx si no está instalado (no depende de Java)
install.packages("openxlsx")

library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
# 1. CONSOLIDACIÓN DE LAS TABLAS RESUMEN DESDE TUS OBJETOS EN MEMORIA
# ------------------------------------------------------------------------------

# 1.1 Tabla Resumen Ejecutivo de todas las Pruebas Globales (Base 16 vs. Base 12)
tabla_resumen_tests <- dplyr::tibble(
  Prueba_Estadistica = c(
    "Países Soberanos Analizados (N)",
    "Observaciones Totales del Panel (1995-2024)",
    "Índice KMO Global (Kaiser-Meyer-Olkin)",
    "KMO Mínimo Individual (min MSA_i)",
    "KMO Promedio de las Variables (Media MSA_i)",
    "Chi-Cuadrado de Esfericidad de Bartlett",
    "Grados de Libertad de Bartlett",
    "Alfa de Cronbach Estandarizado (Consistencia Interna)",
    "Correlación Inter-Ítem Promedio (average_r)",
    "Correlación Ítem-Total Media (r.drop)",
    "Primer Autovalor Combinado (Lambda_1 del CP1)",
    "Varianza Explicada por el CP1 (%)",
    "Razón de Unidimensionalidad (Lambda_1 / Lambda_2)",
    "Congruencia Longitudinal Media de Tucker (1995-2024)",
    "Correlación de Rangos de Spearman (Base 16 vs. Base 12)",
    "Varianza Total del Índice Bruto entre Países-Año (T)",
    "Varianza Media Entre-Imputaciones de Rubin (B)",
    "Proporción de Varianza por Datos Faltantes (Lambda %)"
  ),
  IMPE_Base_16 = c(
    dplyr::n_distinct(panel_impe_final$iso3c),
    nrow(panel_impe_final),
    round(kmo_pooled$MSA, 4),
    round(min(kmo_pooled$MSAi), 4),
    round(mean(kmo_pooled$MSAi), 4),
    round(bartlett_pooled$chisq, 2),
    bartlett_pooled$df,
    round(alfa_pooled$total$std.alpha, 4),
    round(alfa_pooled$total$average_r, 4),
    round(mean(alfa_pooled$item.stats[, "r.drop"]), 4),
    round(tabla_autovalores_pooled$Autovalor_Media[1], 4),
    round(tabla_autovalores_pooled$Varianza_Pct[1], 2),
    round(tabla_autovalores_pooled$Autovalor_Media[1] / tabla_autovalores_pooled$Autovalor_Media[2], 4),
    round(mean(matriz_tucker[upper.tri(matriz_tucker)]), 4),
    1.0000,
    round(stats::var(panel_impe_final$IMPE_Bruto_Pooled), 4),
    round(mean(B_vec), 4),
    round(lambda_medio * 100, 2)
  ),
  IMPE_Base_12 = c(
    dplyr::n_distinct(panel_impe_base12$iso3c),
    nrow(panel_impe_base12),
    round(kmo_base12$MSA, 4),
    round(min(kmo_base12$MSAi), 4),
    round(mean(kmo_base12$MSAi), 4),
    round(bart_base12$chisq, 2),
    bart_base12$df,
    round(alfa_base12$total$std.alpha, 4),
    round(alfa_base12$total$average_r, 4),
    round(mean(alfa_base12$item.stats[, "r.drop"]), 4),
    round(tabla_autovalores_base12$Autovalor_Media[1], 4),
    round(tabla_autovalores_base12$Varianza_Pct[1], 2),
    round(tabla_autovalores_base12$Autovalor_Media[1] / tabla_autovalores_base12$Autovalor_Media[2], 4),
    round(mean(matriz_tucker_12[upper.tri(matriz_tucker_12)]), 4),
    round(cor_spearman_rob["IMPE_Base_16", "IMPE_Depurado_12"], 4),
    round(var_total_12, 4),
    round(mean(B_vec_12), 4),
    round(lambda_12 * 100, 2)
  )
) |>
  dplyr::mutate(Diferencia_Base12_vs_Base16 = round(IMPE_Base_12 - IMPE_Base_16, 4))

# 1.2 Tabla Unificada de Diagnóstico de Datos Faltantes (Influx / Outflux de Van Buuren)
tabla_flux_export <- tabla_flux |>
  tibble::rownames_to_column(var = "Variable") |>
  dplyr::select(Variable, pobs, influx, outflux) |>
  dplyr::mutate(
    pmis            = round(1 - pobs, 3),
    pobs            = round(pobs, 3),
    influx          = round(influx, 3),
    outflux         = round(outflux, 3),
    Suma_In_Out     = round(influx + outflux, 3),
    Bloque_MICE_PMM = ifelse(outflux >= 0.65, "Bloque Ancla (Predictor Fuerte)", "Bloque Receptor (Imputado)")
  )

# 1.3 Tabla Maestra Unificada de Pesos y Auditoría (Cruzando Base 16 y Base 12)
tabla_maestra_pesos <- auditoria_items |>
  dplyr::rename(
    Peso_Base16_Pct        = Peso_IMPE_Pct,
    Carga_CP1_Base16       = Carga_CP1,
    KMO_MSA_Base16         = KMO_Individual_MSA,
    Alfa_Drop_Base16       = Alfa_Si_Se_Elimina,
    Cor_Item_Total_Base16  = Cor_Item_Total
  ) |>
  dplyr::left_join(
    tabla_auditoria_pesos_base12 |>
      dplyr::select(
        Variable, Dimension_Teorica,
        Autovector_CP1_Base12 = Autovector_CP1,
        Carga_CP1_Base12      = Carga_CP1,
        Peso_Base12_Pct       = Peso_IMPE12_Media_Pct,
        Error_Imp_SD_Base12   = Error_Entre_Imp_SD,
        KMO_MSA_Base12        = KMO_Individual_MSA,
        Cor_Item_Total_Base12 = Cor_Item_Total,
        Alfa_Drop_Base12      = Alfa_Si_Se_Elimina
      ),
    by = "Variable"
  )

# 1.4 Panel Consolidado Final con TODAS las variantes del IMPE y Rankings Anuales
panel_maestro_total <- panel_robustez |>
  dplyr::left_join(
    panel_impe_base12 |>
      dplyr::select(
        iso3c, year,
        IMPE12_Bruto_Pooled, IMPE12_Incertidumbre_SD, IMPE12_100,
        Sub_Tecnologico12_100, Sub_Productivo12_100, Sub_Financiero12_100
      ),
    by = c("iso3c", "year")
  ) |>
  dplyr::group_by(year) |>
  dplyr::mutate(
    Rank_Base16  = dplyr::min_rank(dplyr::desc(IMPE_100)),
    Rank_Base12  = dplyr::min_rank(dplyr::desc(IMPE12_100)),
    Rank_EquiDim = dplyr::min_rank(dplyr::desc(IMPE_Equi_Dim))
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(iso3c, year)

# 1.5 Corte Transversal del Ranking Mundial en el Último Año (2024)
ranking_2024_export <- panel_maestro_total |>
  dplyr::filter(year == max(year)) |>
  dplyr::arrange(Rank_Base12) |>
  dplyr::select(
    Rank_Base12, Rank_Base16, Rank_EquiDim, iso3c, year,
    IMPE12_100, IMPE_100, IMPE_Equi_Dim, IMPE_Robusto_Log,
    IMPE12_Incertidumbre_SD, Sub_Tecnologico12_100, Sub_Productivo12_100,
    Sub_Financiero12_100, ctrl_pib_nominal
  )


# ------------------------------------------------------------------------------
# 2. CREACIÓN Y ESTILIZADO DEL LIBRO EXCEL CON openxlsx
# ------------------------------------------------------------------------------
wb <- openxlsx::createWorkbook(creator = "Javier Morros - Tesis UCV")

# Estilos corporativos
estilo_header <- openxlsx::createStyle(
  fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
  fgFill = "#1E3A8A", halign = "center", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight", wrapText = TRUE
)

estilo_cuerpo <- openxlsx::createStyle(
  fontName = "Calibri", fontSize = 10, valign = "center",
  border = "TopBottomLeftRight", borderColour = "#CBD5E1"
)

# Función auxiliar para crear hojas limpias con autofiltro, ancho automático y panel fijo
agregar_hoja_formateada <- function(wb, nombre_hoja, df_datos) {
  openxlsx::addWorksheet(wb, sheetName = nombre_hoja, gridLines = TRUE)
  openxlsx::writeDataTable(
    wb, sheet = nombre_hoja, x = df_datos,
    startRow = 1, startCol = 1,
    tableStyle = "TableStyleMedium2", withFilter = TRUE
  )
  openxlsx::addStyle(
    wb, sheet = nombre_hoja, style = estilo_header,
    rows = 1, cols = 1:ncol(df_datos), gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb, sheet = nombre_hoja, style = estilo_cuerpo,
    rows = 2:(nrow(df_datos) + 1), cols = 1:ncol(df_datos), gridExpand = TRUE
  )
  openxlsx::freezePane(wb, sheet = nombre_hoja, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = nombre_hoja, cols = 1:ncol(df_datos), widths = "auto")
}

# Agregamos las 8 pestañas en orden metodológico
agregar_hoja_formateada(wb, "1_Resumen_Pruebas",       tabla_resumen_tests)
agregar_hoja_formateada(wb, "2_Imputacion_Flux_NAs",   tabla_flux_export)
agregar_hoja_formateada(wb, "3_Autovalores_Base16",    tabla_autovalores_pooled)
agregar_hoja_formateada(wb, "4_Autovalores_Base12",    tabla_autovalores_base12)
agregar_hoja_formateada(wb, "5_Pesos_y_Auditoria",     tabla_maestra_pesos)
agregar_hoja_formateada(wb, "6_Matrices_Tucker_Spear", dplyr::bind_rows(
  tibble::as_tibble(matriz_tucker_12, rownames = "Matriz_Tucker_Base12"),
  tibble::as_tibble(cor_spearman_rob, rownames = "Matriz_Spearman_Robustez")
))
agregar_hoja_formateada(wb, "7_Ranking_Mundial_2024",  ranking_2024_export)
agregar_hoja_formateada(wb, "8_Panel_Completo_95_24",  panel_maestro_total)

# Guardar el archivo Excel definitivo en tu directorio de trabajo
openxlsx::saveWorkbook(wb, file = "IMPE_Resultados_Consolidados_Tesis.xlsx", overwrite = TRUE)
cat("\n¡Libro de Excel 'IMPE_Resultados_Consolidados_Tesis.xlsx' exportado exitosamente!\n")
