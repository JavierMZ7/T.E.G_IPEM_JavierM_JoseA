# ==============================================================================
# CONSTRUCCIÓN Y VALIDACIÓN INTEGRAL DEL ÍNDICE I.M.P.E. BASE 12 (NÚCLEO PURO)
# Horizonte: 1995 - 2024 | 182 Países Soberanos | Combinación vía Reglas de Rubin
# ==============================================================================

library(dplyr)
library(tidyr)

# 1. DEFINICIÓN DE LAS 12 VARIABLES DEL NÚCLEO ESENCIAL POR DIMENSIÓN TEÓRICA
# Excluimos las 4 variables candidatas a poda (prod_manuf_pib, prod_fbcf_pib, 
# fin_ied_sal_pib y tech_serv_ict_pct) que ya cumplieron su rol auxiliar en MICE.
vars_prod_12 <- c("prod_exp_total", "prod_eci", "prod_pib_pcap_ppa")
vars_fin_12  <- c("fin_mkt_cap_pib", "fin_cred_priv_pib", "fin_regalias_rec")
vars_tech_12 <- c("tech_id_pib", "tech_investigad", "tech_exp_alta", 
                  "tech_exp_serv_dig", "tech_patentes_tic", "tech_mnf_val_added")

vars_impe_12 <- c(vars_prod_12, vars_fin_12, vars_tech_12)

# Vector de variables transformadas en logaritmo durante la Etapa 2
vars_log <- c("prod_exp_total", "prod_pib_pcap_ppa", "fin_mkt_cap_pib", 
              "tech_exp_serv_dig", "tech_patentes_tic", "ctrl_pib_nominal")


# 2. EXTRACCIÓN DEL A.C.P. EN CADA IMPUTACIÓN l = 1..m (SOBRE LAS 12 VARIABLES)
analizar_imputacion_base12 <- function(df_imp) {
  # 2.1 Revertir transformaciones a escala original
  df_escala_original <- df_imp |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(vars_log), ~ base::expm1(.x)),
      fin_regalias_rec = base::sinh(fin_regalias_rec)
    )
  
  # 2.2 Estandarizar a Z-scores las 12 variables del núcleo esencial
  mat_z12 <- df_escala_original |>
    dplyr::select(dplyr::all_of(vars_impe_12)) |>
    base::scale(center = TRUE, scale = TRUE)
  
  # 2.3 Análisis de Componentes Principales (12 x 12)
  pca_12 <- stats::prcomp(mat_z12, center = FALSE, scale. = FALSE)
  
  # Garantizar signo positivo del CP1 respecto al gasto en I+D
  signo <- ifelse(pca_12$rotation["tech_id_pib", 1] < 0, -1, 1)
  autovector_cp1 <- pca_12$rotation[, 1] * signo
  autovalores_12 <- pca_12$sdev^2
  pesos_12       <- (autovector_cp1^2) / sum(autovector_cp1^2)
  
  # 2.4 Puntaje bruto del I.M.P.E. Base 12
  score_bruto_12 <- as.vector(mat_z12 %*% pesos_12)
  
  # 2.5 Matriz de correlación 12x12 transformada a escala z de Fisher (Sec. 6.2.2)
  mat_cor12 <- stats::cor(mat_z12)
  cor_z12   <- base::atanh(pmin(pmax(mat_cor12, -0.9999), 0.9999))
  
  list(
    autovalores = autovalores_12,
    autovector1 = autovector_cp1,
    pesos       = pesos_12,
    cor_z       = cor_z12,
    df_scores   = df_escala_original |> dplyr::mutate(IMPE12_Bruto = score_bruto_12)
  )
}

# Ejecutar sobre las m bases de lista_imputaciones
resultados_m_12 <- lapply(lista_imputaciones, analizar_imputacion_base12)


# 3. COMBINACIÓN (POOLING) DE CORRELACIONES, AUTOVALORES Y PESOS (BASE 12)
cor_fisher_media_12  <- Reduce(`+`, lapply(resultados_m_12, `[[`, "cor_z")) / length(resultados_m_12)
matriz_cor_pooled_12 <- base::tanh(cor_fisher_media_12)
diag(matriz_cor_pooled_12) <- 1

pesos_mat_12   <- sapply(resultados_m_12, `[[`, "pesos") * 100
autoval_mat_12 <- sapply(resultados_m_12, `[[`, "autovalores")
autovec_mat_12 <- sapply(resultados_m_12, `[[`, "autovector1")

tabla_autovalores_base12 <- dplyr::tibble(
  Componente         = paste0("CP", seq_len(nrow(autoval_mat_12))),
  Autovalor_Media    = round(rowMeans(autoval_mat_12), 4),
  Autovalor_SD_Imp   = round(apply(autoval_mat_12, 1, stats::sd), 4),
  Varianza_Pct       = round(rowMeans(autoval_mat_12) / sum(rowMeans(autoval_mat_12)) * 100, 2),
  Varianza_Acum_Pct  = round(cumsum(rowMeans(autoval_mat_12) / sum(rowMeans(autoval_mat_12)) * 100), 2),
  Criterio_Kaiser    = ifelse(rowMeans(autoval_mat_12) > 1, "Sí (> 1)", "No (< 1)")
)

cat("\n================ 1. AUTOVALORES COMBINADOS (I.M.P.E. BASE 12) ================\n")
print(tabla_autovalores_base12, n = 12)


# 4. PRUEBAS PSICOMÉTRICAS GLOBALES Y AUDITORÍA POR VARIABLE (KMO, BARTLETT, ALFA)
n_obs_12      <- nrow(resultados_m_12[[1]]$df_scores)
kmo_base12    <- psych::KMO(matriz_cor_pooled_12)
bart_base12   <- psych::cortest.bartlett(matriz_cor_pooled_12, n = n_obs_12)
alfa_base12   <- psych::alpha(matriz_cor_pooled_12, n.obs = n_obs_12, check.keys = TRUE)

cat("\n================ 2. VALIDACIÓN GLOBAL DEL I.M.P.E. BASE 12 ================\n")
cat("1. Índice KMO Global (12 vars):", round(kmo_base12$MSA, 4), "\n")
cat("2. KMO Mínimo Individual:", round(min(kmo_base12$MSAi), 4), "\n")
cat("3. Chi-Cuadrado de Bartlett:", round(bart_base12$chisq, 2), 
    "| gl:", bart_base12$df, "| p-valor:", format.pval(bart_base12$p.value, digits = 4), "\n")
cat("4. Alfa de Cronbach Estandarizado:", round(alfa_base12$total$std.alpha, 4), "\n")
cat("5. Correlación Inter-Ítem Promedio (average_r):", round(alfa_base12$total$average_r, 4), "\n")

# Tabla unificada de Pesos, Cargas, Error de Imputación y Auditoría de Cronbach en Base 12
lambda1_base12 <- tabla_autovalores_base12$Autovalor_Media[1]

tabla_auditoria_pesos_base12 <- dplyr::tibble(
  Variable              = vars_impe_12,
  Dimension_Teorica     = c(rep("Productiva", 4), rep("Financiera", 3), rep("Tecnológica", 5)),
  Autovector_CP1        = round(rowMeans(autovec_mat_12), 4),
  Carga_CP1             = round(rowMeans(autovec_mat_12) * sqrt(lambda1_base12), 4),
  Peso_IMPE12_Media_Pct = round(rowMeans(pesos_mat_12), 2),
  Error_Entre_Imp_SD    = round(apply(pesos_mat_12, 1, stats::sd), 3),
  KMO_Individual_MSA    = round(kmo_base12$MSAi, 4),
  Cor_Item_Total        = round(alfa_base12$item.stats[, "r.drop"], 4),
  Alfa_Si_Se_Elimina    = round(alfa_base12$alpha.drop[, "std.alpha"], 4)
) |>
  dplyr::arrange(dplyr::desc(Peso_IMPE12_Media_Pct))

cat("\n================ 3. TABLA DE PESOS Y AUDITORÍA DE ÍTEMS (BASE 12) ================\n")
print(tabla_auditoria_pesos_base12, n = 12)

# Resumen de pesos por Dimensión Teórica en Base 12
resumen_dimensiones_base12 <- tabla_auditoria_pesos_base12 |>
  dplyr::group_by(Dimension_Teorica) |>
  dplyr::summarise(
    Num_Variables  = dplyr::n(),
    Peso_Total_Pct = round(sum(Peso_IMPE12_Media_Pct), 2),
    Carga_Media_r  = round(mean(Carga_CP1), 3),
    KMO_Medio_Dim  = round(mean(KMO_Individual_MSA), 4)
  ) |>
  dplyr::arrange(dplyr::desc(Peso_Total_Pct))

cat("\n================ 4. PESO AGREGADO POR DIMENSIÓN TEÓRICA (BASE 12) ================\n")
print(resumen_dimensiones_base12)


# 5. CONSTRUCCIÓN DEL PANEL FINAL BASE 12, SUBÍNDICES Y DIAGNÓSTICO VIF / TOLERANCIA
pesos_vec12 <- stats::setNames(rowMeans(pesos_mat_12) / 100, vars_impe_12)
pesos_vec12 <- pesos_vec12 / sum(pesos_vec12)

pesos_prod12_norm <- pesos_vec12[vars_prod_12] / sum(pesos_vec12[vars_prod_12])
pesos_fin12_norm  <- pesos_vec12[vars_fin_12]  / sum(pesos_vec12[vars_fin_12])
pesos_tech12_norm <- pesos_vec12[vars_tech_12] / sum(pesos_vec12[vars_tech_12])

calcular_subindices_base12 <- function(res_l) {
  mat_z12 <- res_l$df_scores |>
    dplyr::select(dplyr::all_of(vars_impe_12)) |>
    base::scale(center = TRUE, scale = TRUE)
  
  dplyr::tibble(
    Sub_Prod12 = as.vector(mat_z12[, vars_prod_12] %*% pesos_prod12_norm),
    Sub_Fin12  = as.vector(mat_z12[, vars_fin_12]  %*% pesos_fin12_norm),
    Sub_Tech12 = as.vector(mat_z12[, vars_tech_12] %*% pesos_tech12_norm)
  )
}

lista_sub_12   <- lapply(resultados_m_12, calcular_subindices_base12)
scores_mat_12  <- sapply(resultados_m_12, function(x) x$df_scores$IMPE12_Bruto)

reescalar_0_100 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
}

panel_impe_base12 <- resultados_m_12[[1]]$df_scores |>
  dplyr::as_tibble() |>
  dplyr::select(iso3c, year, ctrl_pib_nominal) |>
  dplyr::mutate(
    IMPE12_Bruto_Pooled     = rowMeans(scores_mat_12),
    IMPE12_Incertidumbre_SD = apply(scores_mat_12, 1, stats::sd),
    IMPE12_100              = reescalar_0_100(IMPE12_Bruto_Pooled),
    Sub_Productivo12_100    = reescalar_0_100(rowMeans(sapply(lista_sub_12, `[[`, "Sub_Prod12"))),
    Sub_Financiero12_100    = reescalar_0_100(rowMeans(sapply(lista_sub_12, `[[`, "Sub_Fin12"))),
    Sub_Tecnologico12_100   = reescalar_0_100(rowMeans(sapply(lista_sub_12, `[[`, "Sub_Tech12")))
  )

# Matriz de Correlación y Prueba de No-Redundancia (VIF y Tolerancia) entre Subíndices Base 12
cor_dim_12 <- panel_impe_base12 |>
  dplyr::select(IMPE12_100, Sub_Tecnologico12_100, Sub_Productivo12_100, Sub_Financiero12_100) |>
  stats::cor()

sub_cor_3x3 <- cor_dim_12[2:4, 2:4]
vif_dim_12  <- diag(solve(sub_cor_3x3))
tol_dim_12  <- 1 / vif_dim_12

tabla_vif_base12 <- dplyr::tibble(
  Subindice         = colnames(sub_cor_3x3),
  Cor_con_IMPE12    = round(cor_dim_12[1, 2:4], 4),
  R2_con_IMPE12_Pct = round((cor_dim_12[1, 2:4]^2) * 100, 2),
  Tolerancia_Unica  = round(tol_dim_12, 4),
  VIF_InterDim      = round(vif_dim_12, 4)
)

cat("\n================ 5. INTERACCIÓN DIMENSIONAL Y VIF (BASE 12) ================\n")
print(round(cor_dim_12, 4))
cat("\n--- Diagnóstico de Validez Discriminante (VIF < 5.0) ---\n")
print(tabla_vif_base12)


# 6. INVARIANZA TEMPORAL DE TUCKER ENTRE DÉCADAS Y LAMBDA DE RUBIN (BASE 12)
extraer_autovector_periodo_12 <- function(anio_ini, anio_fin) {
  vecs <- sapply(resultados_m_12, function(res_l) {
    sub_df <- res_l$df_scores |>
      dplyr::filter(year >= anio_ini, year <= anio_fin) |>
      dplyr::select(dplyr::all_of(vars_impe_12)) |>
      base::scale(center = TRUE, scale = TRUE)
    pca_sub <- stats::prcomp(sub_df, center = FALSE, scale. = FALSE)
    signo   <- ifelse(pca_sub$rotation["tech_id_pib", 1] < 0, -1, 1)
    pca_sub$rotation[, 1] * signo
  })
  rowMeans(vecs)
}

congruencia_tucker <- function(u, v) sum(u * v) / sqrt(sum(u^2) * sum(v^2))

mat_vecs_12 <- cbind(
  Global_95_24 = rowMeans(autovec_mat_12),
  Decada_95_04 = extraer_autovector_periodo_12(1995, 2004),
  Decada_05_14 = extraer_autovector_periodo_12(2005, 2014),
  Decada_15_24 = extraer_autovector_periodo_12(2015, 2024)
)

matriz_tucker_12 <- outer(
  1:ncol(mat_vecs_12), 1:ncol(mat_vecs_12),
  Vectorize(function(i, j) congruencia_tucker(mat_vecs_12[, i], mat_vecs_12[, j]))
)
dimnames(matriz_tucker_12) <- list(colnames(mat_vecs_12), colnames(mat_vecs_12))

cat("\n================ 6. INVARIANZA TEMPORAL DE TUCKER (BASE 12) ================\n")
print(round(matriz_tucker_12, 4))

# Proporción de varianza atribuible a datos faltantes (Lambda de Van Buuren, Sec. 2.3.5)
m_imp_12     <- length(resultados_m_12)
B_vec_12     <- apply(scores_mat_12, 1, stats::var)
var_total_12 <- stats::var(panel_impe_base12$IMPE12_Bruto_Pooled)
lambda_12    <- mean(((1 + 1/m_imp_12) * B_vec_12) / var_total_12)

cat("\n================ 7. INCERTIDUMBRE DE IMPUTACIÓN EN BASE 12 ================\n")
cat("Varianza Total del Índice Base 12 (T):", round(var_total_12, 4), "\n")
cat("Varianza Media Entre-Imputaciones (B):", round(mean(B_vec_12), 4), "\n")
cat("Proporción de Varianza por Datos Faltantes (Lambda):", round(lambda_12 * 100, 2), "%\n")


# 7. EXPORTACIÓN DE ARCHIVOS CSV DEFINITIVOS DEL MODELO BASE 12
readr::write_csv(panel_impe_base12, "IMPE_Base12_Panel_Final_1995_2024.csv")
readr::write_csv(tabla_autovalores_base12, "IMPE_Base12_Tabla_Autovalores.csv")
readr::write_csv(tabla_auditoria_pesos_base12, "IMPE_Base12_Auditoria_y_Pesos.csv")
readr::write_csv(resumen_dimensiones_base12, "IMPE_Base12_Resumen_Dimensiones.csv")
