# ==============================================================================
# ETAPA 6: BATERÍA DE BLINDAJE METODOLÓGICO, AUDITORÍA POR ÍTEM Y ROBUSTEZ
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. AUDITORÍA INDIVIDUAL POR VARIABLE (MSA de KMO y Alfa si se elimina el ítem)
# ------------------------------------------------------------------------------
auditoria_items <- dplyr::tibble(
  Variable             = vars_impe,
  Peso_IMPE_Pct        = tabla_pesos_pooled$Peso_IMPE_Media_Pct,
  Carga_CP1            = round(tabla_pesos_pooled$Autovector_CP1 * sqrt(tabla_autovalores_pooled$Autovalor_Media[1]), 3),
  KMO_Individual_MSA   = round(kmo_pooled$MSAi, 4),
  Alfa_Si_Se_Elimina   = round(alfa_pooled$alpha.drop[, "std.alpha"], 4),
  Cor_Item_Total       = round(alfa_pooled$item.stats[, "r.drop"], 4),
  Mejora_Alfa_Al_Borrar = ifelse(alfa_pooled$alpha.drop[, "std.alpha"] > alfa_pooled$total$std.alpha, 
                                 "Sí (Candidata a poda)", "No (Núcleo esencial)")
) |>
  dplyr::arrange( dplyr::desc(Peso_IMPE_Pct))

cat("\n================ 1. AUDITORÍA PSICOMÉTRICA POR VARIABLE INDIVIDUAL ================\n")
print(auditoria_items, n = 16)


# ------------------------------------------------------------------------------
# 2. PRUEBA DE INVARIANZA TEMPORAL POR DÉCADAS (Congruencia de Tucker)
# ¿Se mantienen estables los autovectores entre 1995-2004, 2005-2014 y 2015-2024?
# ------------------------------------------------------------------------------
extraer_autovector_periodo <- function(anio_inicio, anio_fin) {
  # Promediamos los autovectores del subperíodo a través de las m imputaciones
  vecs <- sapply(resultados_m, function(res_l) {
    sub_df <- res_l$df_scores |>
      dplyr::filter(year >= anio_inicio, year <= anio_fin) |>
      dplyr::select(dplyr::all_of(vars_impe)) |>
      base::scale(center = TRUE, scale = TRUE)
    
    pca_sub <- stats::prcomp(sub_df, center = FALSE, scale. = FALSE)
    signo   <- ifelse(pca_sub$rotation["tech_id_pib", 1] < 0, -1, 1)
    pca_sub$rotation[, 1] * signo
  })
  rowMeans(vecs)
}

vec_global    <- tabla_pesos_pooled$Autovector_CP1
vec_1995_2004 <- extraer_autovector_periodo(1995, 2004)
vec_2005_2014 <- extraer_autovector_periodo(2005, 2014)
vec_2015_2024 <- extraer_autovector_periodo(2015, 2024)

# Función del Coeficiente de Congruencia de Tucker (phi >= 0.95 = equivalencia factorial)
congruencia_tucker <- function(u, v) {
  sum(u * v) / sqrt(sum(u^2) * sum(v^2))
}

matriz_vecs_decadas <- cbind(
  Global_95_24 = vec_global,
  Decada_95_04 = vec_1995_2004,
  Decada_05_14 = vec_2005_2014,
  Decada_15_24 = vec_2015_2024
)

matriz_tucker <- outer(
  1:ncol(matriz_vecs_decadas), 1:ncol(matriz_vecs_decadas),
  Vectorize(function(i, j) congruencia_tucker(matriz_vecs_decadas[, i], matriz_vecs_decadas[, j]))
)
dimnames(matriz_tucker) <- list(colnames(matriz_vecs_decadas), colnames(matriz_vecs_decadas))

cat("\n================ 2. INVARIANZA TEMPORAL (CONGRUENCIA DE TUCKER ENTRE DÉCADAS) ================\n")
cat("Nota: Valores > 0.90 indican invarianza estructural; > 0.95 equivalencia factorial estricta.\n")
print(round(matriz_tucker, 4))


# ------------------------------------------------------------------------------
# 3. ANÁLISIS DE SENSIBILIDAD Y ROBUSTEZ BAJO 4 ESQUEMAS ALTERNATIVOS
# Compara: (1) Base ACP 16 vars, (2) Depurado 12 vars (sin las 4 < 1.5%),
# (3) Equi-Dimensional (1/3 cada dimensión), (4) Escala Log/Winsorizada (sin outliers extremos)
# ------------------------------------------------------------------------------
vars_ruido <- c("prod_manuf_pib", "fin_ied_sal_pib", "prod_fbcf_pib", "tech_serv_ict_pct")
w_base     <- stats::setNames(tabla_pesos_pooled$Peso_IMPE_Media_Pct / 100, vars_impe)
w_base     <- w_base / sum(w_base)

# Esquema 2: 12 variables del núcleo duro
w_12 <- w_base
w_12[vars_ruido] <- 0
w_12 <- w_12 / sum(w_12)

# Esquema 3: Pesos iguales por dimensión (1/3 Productiva, 1/3 Financiera, 1/3 Tecnológica)
w_equi <- c(
  (w_base[vars_productiva]  / sum(w_base[vars_productiva]))  * (1/3),
  (w_base[vars_financiera]  / sum(w_base[vars_financiera]))  * (1/3),
  (w_base[vars_tecnologica] / sum(w_base[vars_tecnologica])) * (1/3)
)[vars_impe]

# Función para Winsorizar al 1% y 99% (controla outliers en escala log/asinh)
winsorizar_1_99 <- function(x) {
  q <- stats::quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

# Calculamos los 4 índices alternativos promediados en las m imputaciones
calcular_esquemas_robustez <- function( idx_l ) {
  df_orig <- resultados_m[[idx_l]]$df_scores
  mat_z_orig <- df_orig |>
    dplyr::select(dplyr::all_of(vars_impe)) |>
    base::scale(center = TRUE, scale = TRUE)
  
  # Matriz estabilizada (en escala log/asinh de MICE + Winsorización 1%-99%)
  df_log_imp <- lista_imputaciones[[idx_l]]
  mat_z_rob  <- df_log_imp |>
    dplyr::select(dplyr::all_of(vars_impe)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), winsorizar_1_99)) |>
    base::scale(center = TRUE, scale = TRUE)
  
  dplyr::tibble(
    Score_Base_16    = as.vector(mat_z_orig %*% w_base),
    Score_Depurado12 = as.vector(mat_z_orig %*% w_12),
    Score_EquiDim    = as.vector(mat_z_orig %*% w_equi),
    Score_RobustoLog = as.vector(mat_z_rob  %*% w_base)
  )
}

lista_rob <- lapply(seq_along(resultados_m), calcular_esquemas_robustez)

panel_robustez <- panel_impe_final |>
  dplyr::mutate(
    IMPE_Base_16     = reescalar_0_100(rowMeans(sapply(lista_rob, `[[`, "Score_Base_16"))),
    IMPE_Depurado_12 = reescalar_0_100(rowMeans(sapply(lista_rob, `[[`, "Score_Depurado12"))),
    IMPE_Equi_Dim    = reescalar_0_100(rowMeans(sapply(lista_rob, `[[`, "Score_EquiDim"))),
    IMPE_Robusto_Log = reescalar_0_100(rowMeans(sapply(lista_rob, `[[`, "Score_RobustoLog")))
  )

# Correlación de Rangos de Spearman entre las 4 especificaciones
cor_spearman_rob <- panel_robustez |>
  dplyr::select(IMPE_Base_16, IMPE_Depurado_12, IMPE_Equi_Dim, IMPE_Robusto_Log) |>
  stats::cor(method = "spearman") |>
  round(4)

cat("\n================ 3. MATRIZ DE CORRELACIÓN DE RANGOS DE SPEARMAN (ROBUSTEZ) ================\n")
print(cor_spearman_rob)


# ------------------------------------------------------------------------------
# 4. MÉTRICAS DE INCERTIDUMBRE DE VAN BUUREN (Sección 2.3.5: B, U_bar, T, Lambda)
# ------------------------------------------------------------------------------
m_imp <- length(resultados_m)
# Varianza entre imputaciones (B) para el puntaje bruto de cada observación
B_vec <- apply(scores_matriz, 1, stats::var)
# Varianza muestral media del índice (U_bar aproximada por error estándar del score ponderado)
U_bar <- mean(apply(scores_matriz, 2, stats::var)) / nrow(scores_matriz)
T_vec <- U_bar + (1 + 1/m_imp) * B_vec
lambda_medio <- mean(((1 + 1/m_imp) * B_vec) / ( stats::var(panel_impe_final$IMPE_Bruto_Pooled) ))

cat("\n================ 4. DIAGNÓSTICO DE INCERTIDUMBRE DE IMPUTACIÓN (VAN BUUREN, SEC. 2.3.5) ================\n")
cat("Varianza Total del Índice (Varianza entre países-año):", round(stats::var(panel_impe_final$IMPE_Bruto_Pooled), 4), "\n")
cat("Varianza Media Entre-Imputaciones (B promedio):", round(mean(B_vec), 4), "\n")
cat("Proporción de Varianza del Índice atribuible a NAs (Lambda efectivo):", round(lambda_medio * 100, 2), "%\n")
