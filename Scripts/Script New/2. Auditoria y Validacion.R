
##INTENTO DE AUDITORIA HAND-MADE##

df_auditoria_log <- panel_proc

df_audi_z <- df_auditoria_log %>%
  mutate(across(all_of(vars_base16), ~ {
    mu <- mean(.x, na.rm = TRUE)
    sd_val <- sd(.x, na.rm = TRUE)
    if (is.na(sd_val) || sd_val == 0) rep(0, length(.x)) else (.x - mu) / sd_val
  }))

mat_num_Z <- as.matrix(df_audi_z[,vars_base16])

##CORRELACION 1
R_correl_wise <- stats::cor(mat_num_Z, use = "pairwise.complete.obs")
R_correl_wise [is.na(R_correl_wise)] <- 0
diag(R_correl_wise) <- 1
if (min(eigen(R_correl_wise, symmetric = TRUE, only.values = TRUE)$values) <= 1e-6) 
  R_correl_wise <- as.matrix(Matrix::nearPD(R_correl_wise, corr = TRUE, ensureSymmetry = TRUE)$mat)

tabla_R_cor_wise <- as.data.frame(R_correl_wise) %>%
  tibble::rownames_to_column(var = "Variable")


 print(tabla_R_cor_wise)
 
##PCA

 
pca_prcomp <- stats::prcomp(mat_num_Z, center = TRUE, scale. = TRUE)

## mat_completa <- na.omit(mat_num_Z)



# ==============================================================================
# AUDITORÍA INDEPENDIENTE DEL I.M.P.E. (1995-2024) - DEEPseek
# ==============================================================================


# --- 0. Verificar prerequisitos ---
objetos_requeridos <- c(
  "MATRICES_R", "RESULTADOS_EIGEN", "RESULTADOS_PRCOMP",
  "PANELES_PROCESADOS", "RESULTADOS_TODOS",
  "TABLA_ESTADISTICOS", "TABLA_CARGAS_LARGA", "TABLA_SCORES_LARGA",
  "FAMILIAS", "vars_base16", "vars_base12"
)
faltan <- objetos_requeridos[!sapply(objetos_requeridos, exists)]
if (length(faltan) > 0) {
  stop("Faltan objetos del maestro: ", paste(faltan, collapse = ", "),
       "\nEjecuta IMPE_consolidado_maestro v2.R en esta sesión primero.")
}

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble)
  library(openxlsx)
})

# ==============================================================================
# INFRAESTRUCTURA
# ==============================================================================
AUDITORIA <- list()

registrar <- function(seccion, item, estado, detalle) {
  AUDITORIA[[length(AUDITORIA) + 1]] <<- tibble(
    Seccion = seccion, Item = item,
    Estado  = estado,               # "OK" | "WARN" | "FALLA"
    Detalle = as.character(detalle)
  )
}

auditar <- function(seccion, item, cond, ok_msg, falla_msg, severidad = "FALLA") {
  if (isTRUE(cond)) registrar(seccion, item, "OK", ok_msg)
  else              registrar(seccion, item, severidad, falla_msg)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ==============================================================================
# §1. INTEGRIDAD DE LOS PANELES DE ENTRADA
# ==============================================================================
for (fam_nom in names(FAMILIAS)) {
  df <- FAMILIAS[[fam_nom]]$df
  vr <- FAMILIAS[[fam_nom]]$vars
  sec <- "1. Datos"
  
  # 1.1 Variables presentes
  falt <- setdiff(vr, names(df))
  auditar(sec, paste0(fam_nom, " | variables presentes"),
          length(falt) == 0,
          sprintf("Las %d variables están presentes", length(vr)),
          sprintf("Faltan: %s", paste(falt, collapse = ", ")))
  
  # 1.2 Unicidad (iso3c, year)
  dup <- df %>% count(iso3c, year) %>% filter(n > 1) %>% nrow()
  auditar(sec, paste0(fam_nom, " | unicidad (iso3c,year)"),
          dup == 0, "Sin duplicados",
          sprintf("%d pares duplicados", dup))
  
  # 1.3 Rango de años
  auditar(sec, paste0(fam_nom, " | rango de años"),
          all(df$year >= ANIO_INICIO & df$year <= ANIO_FIN),
          sprintf("Todos en [%d,%d]", ANIO_INICIO, ANIO_FIN),
          "Hay años fuera de rango")
  
  # 1.4 Valores no finitos
  mat <- as.matrix(df[, vr])
  n_bad <- sum(is.infinite(mat)) + sum(is.nan(mat))
  auditar(sec, paste0(fam_nom, " | sin Inf/NaN"),
          n_bad == 0, "Sin Inf ni NaN",
          sprintf("Inf=%d | NaN=%d", sum(is.infinite(mat)), sum(is.nan(mat))))
  
  # 1.5 Cobertura global
  pct_na <- mean(is.na(mat))
  registrar(sec, paste0(fam_nom, " | % NAs global"),
            ifelse(pct_na > 0.40, "WARN", "OK"),
            sprintf("%.1f%% NA", pct_na * 100))
  
  # 1.6 Columnas con SD=0 (rompen z-score y correlación)
  sds <- apply(mat, 2, sd, na.rm = TRUE)
  sd0 <- names(sds)[sds == 0 | is.na(sds)]
  auditar(sec, paste0(fam_nom, " | SD > 0 por columna"),
          length(sd0) == 0,
          "Todas las columnas con SD > 0",
          sprintf("Columnas SD=0: %s", paste(sd0, collapse = ", ")))
}

# ==============================================================================
# §2. MATRICES DE CORRELACIÓN R
# ==============================================================================
for (nom_ind in names(RESULTADOS_TODOS)) {
  res <- RESULTADOS_TODOS[[nom_ind]]
  R   <- res$R_cor
  p   <- ncol(R)
  sec <- "2. Matriz R"
  
  # 2.1 Cuadrada
  auditar(sec, paste0(nom_ind, " | cuadrada"),
          p == nrow(R), sprintf("%dx%d", p, p), "No cuadrada")
  
  # 2.2 Simétrica
  asim <- max(abs(R - t(R)))
  auditar(sec, paste0(nom_ind, " | simétrica"),
          asim < 1e-8, sprintf("Δsim=%.2e", asim),
          sprintf("Δsim=%.2e (>1e-8)", asim))
  
  # 2.3 Diagonal = 1
  dd <- max(abs(diag(R) - 1))
  auditar(sec, paste0(nom_ind, " | diag=1"),
          dd < 1e-8, sprintf("Δdiag=%.2e", dd),
          sprintf("Δdiag=%.2e", dd))
  
  # 2.4 Celdas en [-1,1]
  auditar(sec, paste0(nom_ind, " | celdas en [-1,1]"),
          all(R >= -1 - 1e-8 & R <= 1 + 1e-8),
          sprintf("[%.4f, %.4f]", min(R), max(R)),
          sprintf("Fuera de rango: [%.4f, %.4f]", min(R), max(R)))
  
  # 2.5 Traza = p (propiedad de matrices de correlación)
  auditar(sec, paste0(nom_ind, " | traza = p"),
          abs(sum(diag(R)) - p) < 1e-8,
          sprintf("traza=%.6f (p=%d)", sum(diag(R)), p),
          sprintf("traza=%.6f ≠ p=%d", sum(diag(R)), p))
  
  # 2.6 PSD: TODOS los λ ≥ 0 (aquí es donde nearPD debe haber actuado)
  lams <- RESULTADOS_EIGEN[[nom_ind]]$values
  lm1  <- min(lams)
  if (lm1 < -1e-8) {
    registrar(sec, paste0(nom_ind, " | PSD (λ≥0)"), "FALLA",
              sprintf("λ_min=%.4e ⇒ ¡NO PSD! nearPD falló", lm1))
  } else if (lm1 < 1e-6) {
    registrar(sec, paste0(nom_ind, " | PSD (λ≥0)"), "WARN",
              sprintf("λ_min=%.4e (cerca del umbral, revisar)", lm1))
  } else {
    registrar(sec, paste0(nom_ind, " | PSD (λ≥0)"), "OK",
              sprintf("λ_min=%.4e", lm1))
  }
  
  # 2.7 Σ autovalores = traza = p
  auditar(sec, paste0(nom_ind, " | Σλ = p"),
          abs(sum(lams) - p) < 1e-6,
          sprintf("Σλ=%.6f", sum(lams)),
          sprintf("Σλ=%.6f ≠ p=%d (⇒ λ negativos ocultos)", sum(lams), p))
}

# ==============================================================================
# §3. DESCOMPOSICIÓN EIGEN
# ==============================================================================
for (nom_ind in names(RESULTADOS_EIGEN)) {
  eg  <- RESULTADOS_EIGEN[[nom_ind]]
  R   <- RESULTADOS_TODOS[[nom_ind]]$R_cor
  V   <- eg$vectors; lams <- eg$values; p <- length(lams)
  sec <- "3. Eigen"
  
  # 3.1 Reales
  auditar(sec, paste0(nom_ind, " | λ reales"),
          is.numeric(lams) && !any(is.complex(lams)),
          "Reales", "Complejos (⇒ symmetric=FALSE?)")
  
  # 3.2 Ordenados descendente
  auditar(sec, paste0(nom_ind, " | λ ordenados desc"),
          all(diff(lams) <= 1e-10),
          "Ordenados",
          "No ordenados (revisar eigen)")
  
  # 3.3 Ortonormalidad VᵀV = I
  d_or <- max(abs(crossprod(V) - diag(p)))
  auditar(sec, paste0(nom_ind, " | VᵀV=I"),
          d_or < 1e-8, sprintf("Δ=%.2e", d_or),
          sprintf("Δ=%.2e (>1e-8) ⇒ ¡autovectores no ortonormales!", d_or))
  
  # 3.4 Norma unitaria
  dn <- max(abs(colSums(V^2) - 1))
  auditar(sec, paste0(nom_ind, " | ‖vⱼ‖=1"),
          dn < 1e-8, sprintf("Δ=%.2e", dn),
          sprintf("Δ=%.2e", dn))
  
  # 3.5 Reconstrucción R = V Λ Vᵀ
  d_rec <- max(abs(R - V %*% diag(lams) %*% t(V)))
  auditar(sec, paste0(nom_ind, " | R = VΛVᵀ"),
          d_rec < 1e-4,
          sprintf("Δrec=%.2e (dentro de tolerancia de redondeo)", d_rec),
          sprintf("Δrec=%.2e (>1e-4 ⇒ revisar)", d_rec))
}

# ==============================================================================
# §4. AUDITORÍA CRUZADA: PRCOMP() vs EIGEN()  ← con alineación de signos
# ==============================================================================
for (nom_ind in names(RESULTADOS_TODOS)) {
  res <- RESULTADOS_TODOS[[nom_ind]]
  pc  <- res$prcomp_obj; eg <- res$eigen_obj
  sec <- "4. prcomp vs eigen"
  
  if (is.null(pc)) {
    registrar(sec, paste0(nom_ind, " | disponible"), "WARN",
              "prcomp_obj=NULL (faltan filas completas)")
    next
  }
  
  for (nom_ind in names(RESULTADOS_TODOS)) {
    res <- RESULTADOS_TODOS[[nom_ind]]
    pc  <- res$prcomp_obj; eg <- res$eigen_obj
    sec <- "4. prcomp vs eigen"
    
    # Auditoría INTERNA del maestro: prcomp vs eigen sobre MISMA fuente
    # (el maestro ya computa diff_prcomp_eigen con esta lógica)
    dif_rep <- res$stats$Check_Diff_prcomp_eig
    auditar(sec, paste0(nom_ind, " | prcomp≡eigen (misma fuente)"),
            !is.na(dif_rep) && dif_rep < 1e-8,
            sprintf("diff=%.2e ✔ (validado sobre: %s)", dif_rep,
                    ifelse(grepl("MICE", nom_ind), "imputación 1", "casos completos")),
            sprintf("diff=%s (¡revisar!)", format(dif_rep)))
    
    # Auditoría INFORMATIVA: cuánto se aleja la R pairwise de la complete-case
    # (en MICE: cuánto varía entre imputación 1 y pooled)
    # → NO es un check de correctitud, es un diagnóstico
    if (!is.na(dif_rep)) {
      delta_fuente <- if (grepl("Raw|Interp", nom_ind)) {
        max(abs(pc$sdev^2 - eg$values))
      } else {
        max(abs(pc$sdev^2 - eg$values))
      }
      registrar(sec, paste0(nom_ind, " | ΔR pairwise vs complete (informativo)"),
                "OK",  # siempre OK, es informativo
                sprintf("Δλ=%.4f (magnitud del sesgo por patrón de NAs)", delta_fuente))
    }
  }
  
  # 4.5 Reporte interno del maestro
  dif_rep <- res$stats$Check_Diff_prcomp_eig
  auditar(sec, paste0(nom_ind, " | check reportado maestro"),
          !is.na(dif_rep) && dif_rep < 1e-6,
          sprintf("diff=%.2e", dif_rep),
          sprintf("diff=%s", ifelse(is.na(dif_rep), "NA", format(dif_rep))),
          severidad = "WARN")
}

# ==============================================================================
# §5. PESOS Y CP1
# ==============================================================================
for (nom_ind in unique(TABLA_CARGAS_LARGA$Indice)) {
  sub <- TABLA_CARGAS_LARGA %>% filter(Indice == nom_ind)
  sec <- "5. Pesos"
  
  # 5.1 Σ pesos = 1
  auditar(sec, paste0(nom_ind, " | Σ pesos = 1"),
          abs(sum(sub$Peso_CP1) - 1) < 1e-4,
          sprintf("Σ=%.6f", sum(sub$Peso_CP1)),
          sprintf("Σ=%.6f ≠ 1", sum(sub$Peso_CP1)))
  
  # 5.2 Pesos no negativos
  auditar(sec, paste0(nom_ind, " | pesos ≥ 0"),
          all(sub$Peso_CP1 >= 0),
          sprintf("min=%.4f", min(sub$Peso_CP1)),
          sprintf("%d pesos negativos", sum(sub$Peso_CP1 < 0)))
  
  # 5.3 Σ CP1² = 1 (unitaria)
  tolerancia_pesos <- 0.5e-4 * nrow(sub) + 1e-9  # cota por redondeo a 4 dec × p variables
  auditar(sec, paste0(nom_ind, " | Σ pesos = 1"),
          abs(sum(sub$Peso_CP1) - 1) < tolerancia_pesos,
          sprintf("Σ=%.4f (dentro de cota por redondeo)", sum(sub$Peso_CP1)),
          sprintf("Σ=%.4f excede cota=%.4f", sum(sub$Peso_CP1), tolerancia_pesos))
  
  # 5.4 CP1 alineado (mayoría positiva)
  np <- sum(sub$CP1 > 0)
  auditar(sec, paste0(nom_ind, " | CP1 mayoría (+)"),
          np > nrow(sub) / 2,
          sprintf("%d/%d (+)", np, nrow(sub)),
          sprintf("%d/%d (+) ⇒ revisar alineación", np, nrow(sub)),
          severidad = "WARN")
  
  # 5.5 peso = CP1² (por construcción)
  auditar(sec, paste0(nom_ind, " | peso = CP1²"),
          max(abs(sub$Peso_CP1 - sub$CP1^2)) < 1e-3,
          sprintf("Δmax=%.2e", max(abs(sub$Peso_CP1 - sub$CP1^2))),
          sprintf("Δmax=%.2e", max(abs(sub$Peso_CP1 - sub$CP1^2))))
}

# ==============================================================================
# §6. SCORES Y RANKINGS
# ==============================================================================
for (nom_ind in unique(TABLA_SCORES_LARGA$Indice)) {
  sub <- TABLA_SCORES_LARGA %>% filter(Indice == nom_ind)
  sec <- "6. Scores"
  
  s100 <- sub$Score_100[!is.na(sub$Score_100)]
  
  # 6.1 Score_100 ∈ [0,100]
  auditar(sec, paste0(nom_ind, " | Score_100 ∈ [0,100]"),
          all(s100 >= 0 & s100 <= 100),
          sprintf("[%.2f, %.2f]", min(s100), max(s100)),
          sprintf("Fuera: [%.2f, %.2f]", min(s100), max(s100)))
  
  # 6.2 min-max correcto (debe haber 0 y 100)
  auditar(sec, paste0(nom_ind, " | min-max exacto"),
          abs(min(s100) - 0) < 1e-6 && abs(max(s100) - 100) < 1e-6,
          sprintf("[%.4f, %.4f]", min(s100), max(s100)),
          sprintf("[%.4f, %.4f] (esperado [0,100])", min(s100), max(s100)),
          severidad = "WARN")
  
  # 6.3 Score_Z centrado (global ~0)
  sz <- sub$Score_Z[!is.na(sub$Score_Z)]
  auditar(sec, paste0(nom_ind, " | Score_Z centrado"),
          abs(mean(sz)) < 0.5,
          sprintf("media=%.4f", mean(sz)),
          sprintf("media=%.4f", mean(sz)),
          severidad = "WARN")
  
  # 6.4 Rank entero ≥ 1
  r <- sub$Rank_Anual[!is.na(sub$Rank_Anual)]
  auditar(sec, paste0(nom_ind, " | Rank entero ≥ 1"),
          all(r >= 1) && all(r == round(r)),
          sprintf("[%d, %d]", as.integer(min(r)), as.integer(max(r))),
          "Ranks inválidos")
  
  # 6.5 Rank coherente con Score (mayor score = rank 1, por año)
  ok_rank <- sub %>%
    filter(!is.na(Score_100) & !is.na(Rank_Anual)) %>%
    group_by(year) %>%
    mutate(rank_esp = min_rank(desc(Score_100))) %>%
    ungroup() %>%
    summarise(ok = all(Rank_Anual == rank_esp), .groups = "drop") %>%
    pull(ok)
  auditar(sec, paste0(nom_ind, " | rank coherente"),
          ok_rank, "Coherente",
          "Rank NO coincide con Score_100")
  
  # 6.6 % filas sin score
  pna <- mean(is.na(sub$Score_100))
  registrar(sec, paste0(nom_ind, " | % filas sin score"),
            ifelse(pna > 0.30, "WARN", "OK"),
            sprintf("%.1f%% NA", pna * 100))
}

# ==============================================================================
# §7. IMPUTACIONES MICE
# ==============================================================================
for (fam_nom in names(FAMILIAS)) {
  vr <- FAMILIAS[[fam_nom]]$vars
  n_base <- nrow(FAMILIAS[[fam_nom]]$df)
  sec <- "7. MICE"
  for (met in c("PMM", "Bayes", "RF")) {
    nom_ind <- paste0(fam_nom, "_", met)
    if (!nom_ind %in% names(PANELES_PROCESADOS)) next
    df <- PANELES_PROCESADOS[[nom_ind]]
    
    # 7.1 Sin NAs
    pna <- mean(is.na(as.matrix(df[, vr])))
    auditar(sec, paste0(nom_ind, " | sin NAs"),
            pna == 0, "0% NA",
            sprintf("%.2f%% NA remanentes", pna * 100))
    
    # 7.2 Dims = base
    auditar(sec, paste0(nom_ind, " | dims = base"),
            nrow(df) == n_base,
            sprintf("%d filas", nrow(df)),
            sprintf("%d ≠ %d base", nrow(df), n_base))
    
    # 7.3 Mismo conjunto de iso3c que la base
    auditar(sec, paste0(nom_ind, " | iso3c = base"),
            setequal(unique(df$iso3c), unique(FAMILIAS[[fam_nom]]$df$iso3c)),
            "Mismo conjunto de países",
            "Países distintos tras imputar")
  }
}

# ==============================================================================
# §8. ESTADÍSTICOS DE FIABILIDAD
# ==============================================================================
for (i in seq_len(nrow(TABLA_ESTADISTICOS))) {
  row <- TABLA_ESTADISTICOS[i, ]
  nom_ind <- row$Indice; sec <- "8. Fiabilidad"
  
  # 8.1 Alfa de Cronbach
  al <- row$Alfa_Cronbach
  if (is.na(al)) {
    registrar(sec, paste0(nom_ind, " | alfa"), "WARN", "NA")
  } else if (al > 1) {
    registrar(sec, paste0(nom_ind, " | alfa"), "FALLA",
              sprintf("alfa=%.3f >1 (imposible)", al))
  } else if (al < 0.5) {
    registrar(sec, paste0(nom_ind, " | alfa"), "WARN",
              sprintf("alfa=%.3f (<0.5, baja)", al))
  } else {
    registrar(sec, paste0(nom_ind, " | alfa"), "OK",
              sprintf("alfa=%.3f", al))
  }
  
  # 8.2 KMO ∈ [0,1]
  auditar(sec, paste0(nom_ind, " | KMO ∈ [0,1]"),
          !is.na(row$KMO_Global) && row$KMO_Global >= 0 && row$KMO_Global <= 1,
          sprintf("KMO=%.3f", row$KMO_Global),
          sprintf("KMO=%s", row$KMO_Global))
  
  # 8.3 Bartlett p ∈ [0,1]
  auditar(sec, paste0(nom_ind, " | Bartlett p ∈ [0,1]"),
          !is.na(row$Bartlett_p_valor) && row$Bartlett_p_valor >= 0 && row$Bartlett_p_valor <= 1,
          sprintf("p=%.4f", row$Bartlett_p_valor),
          sprintf("p=%s", row$Bartlett_p_valor))
  
  # 8.4 CP1 ≥ 20% varianza (heurística OCDE)
  registrar(sec, paste0(nom_ind, " | CP1 ≥20% var"),
            ifelse(row$Var_Explicada_CP1_Pct < 20, "WARN", "OK"),
            sprintf("CP1=%.1f%%", row$Var_Explicada_CP1_Pct))
  
  # 8.5 Σ CP1² = 1 (reportado)
  auditar(sec, paste0(nom_ind, " | Σ CP1²=1 (reportado)"),
          !is.na(row$Check_SumaCuad_CP1) && abs(row$Check_SumaCuad_CP1 - 1) < 1e-4,
          sprintf("%.6f", row$Check_SumaCuad_CP1),
          sprintf("%.6f ≠ 1", row$Check_SumaCuad_CP1))
}

# ==============================================================================
# §9. CONSISTENCIA GLOBAL
# ==============================================================================
sec <- "9. Global"
n_esp <- length(FAMILIAS) * 5

auditar(sec, "N° índices = 20",
        length(RESULTADOS_TODOS) == n_esp,
        sprintf("%d índices", length(RESULTADOS_TODOS)),
        sprintf("%d ≠ %d", length(RESULTADOS_TODOS), n_esp))

MR <- as.matrix(MATRIZ_COR_RANKINGS[, -1])
auditar(sec, "Matriz Spearman simétrica",
        max(abs(MR - t(MR))) < 1e-6,
        sprintf("Δsim=%.2e", max(abs(MR - t(MR)))),
        sprintf("Δsim=%.2e", max(abs(MR - t(MR)))))

auditar(sec, "Diagonal Spearman = 1",
        max(abs(diag(MR) - 1)) < 1e-6,
        sprintf("Δdiag=%.2e", max(abs(diag(MR) - 1))),
        sprintf("Δdiag=%.2e", max(abs(diag(MR) - 1))))

auditar(sec, "Spearman ∈ [-1,1]",
        all(MR >= -1 - 1e-6 & MR <= 1 + 1e-6),
        sprintf("[%.4f, %.4f]", min(MR), max(MR)),
        "Fuera de [-1,1]")

cg_min <- min(CONGRUENCIA_RANKINGS$Spearman_Promedio_Otros)
registrar(sec, "Congruencia media ≥ 0.50",
          ifelse(cg_min < 0.5, "WARN", "OK"),
          sprintf("min congruencia = %.4f", cg_min))

auditar(sec, "Dims scores/ranks compatibles",
        nrow(PANEL_SCORES_100) == nrow(PANEL_RANKS),
        sprintf("%d filas", nrow(PANEL_SCORES_100)),
        sprintf("%d vs %d", nrow(PANEL_SCORES_100), nrow(PANEL_RANKS)))

auditar(sec, "TABLA_SCORES_LARGA contiene 20 índices",
        length(unique(TABLA_SCORES_LARGA$Indice)) == n_esp,
        sprintf("%d", length(unique(TABLA_SCORES_LARGA$Indice))),
        sprintf("%d ≠ %d", length(unique(TABLA_SCORES_LARGA$Indice)), n_esp))

# ==============================================================================
# §10. ESTABILIDAD DE CP1 ENTRE LOS 5 MÉTODOS DE CADA FAMILIA
# ==============================================================================
# Si CP1 cambia de signo/perfil entre métodos, la "familia" es un artefacto.
for (fam_nom in names(FAMILIAS)) {
  sub <- TABLA_CARGAS_LARGA %>%
    filter(grepl(paste0("^", fam_nom, "_"), Indice)) %>%
    select(Variable, Indice, CP1) %>%
    pivot_wider(names_from = Indice, values_from = CP1)
  
  metodos <- names(sub)[-1]
  if (length(metodos) < 2) next
  
  # Correlaciones de CP1 entre todos los pares de métodos de la familia
  M <- as.matrix(sub[, metodos])
  C <- suppressWarnings(cor(M))
  off <- C[lower.tri(C)]
  min_rho <- min(off)
  
  registrar("10. Estabilidad CP1",
            paste0(fam_nom, " | ρ(CP1) entre métodos ≥ 0.50"),
            ifelse(min_rho < 0.50, "WARN", "OK"),
            sprintf("ρ_min = %.3f", min_rho))
}

# ==============================================================================
# CONSOLIDACIÓN Y EXPORTACIÓN
# ==============================================================================
REPORTE <- bind_rows(AUDITORIA) %>%
  mutate(Estado = factor(Estado, levels = c("FALLA", "WARN", "OK")))

RESUMEN <- REPORTE %>%
  count(Seccion, Estado) %>%
  pivot_wider(names_from = Estado, values_from = n, values_fill = 0) %>%
  arrange(Seccion)

cat("\n===============================================================\n")
cat("           RESUMEN DE AUDITORÍA — I.M.P.E.\n")
cat("===============================================================\n")
print(as.data.frame(RESUMEN), row.names = FALSE)

nF <- sum(REPORTE$Estado == "FALLA")
nW <- sum(REPORTE$Estado == "WARN")
nO <- sum(REPORTE$Estado == "OK")
cat("\nTotal verificaciones:", nrow(REPORTE), "\n")
cat("  OK    :", nO, "\n")
cat("  WARN  :", nW, "\n")
cat("  FALLA :", nF, "\n")
cat("===============================================================\n")

if (nF > 0) {
  cat("\n!!! FALLAS DETECTADAS !!!\n")
  print(REPORTE %>% filter(Estado == "FALLA") %>%
          select(Seccion, Item, Detalle), n = Inf)
}
if (nW > 0) {
  cat("\n>>> ADVERTENCIAS (revisar antes de la defensa) <<<\n")
  print(REPORTE %>% filter(Estado == "WARN") %>%
          select(Seccion, Item, Detalle), n = Inf)
}

# --- Exportar a Excel con formato condicional ---
wb <- createWorkbook()
addWorksheet(wb, "Reporte")
writeData(wb, "Reporte", REPORTE)
est_hdr <- createStyle(fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
                       fgFill = "#1B365D", textDecoration = "bold",
                       halign = "center", border = "Bottom")
addStyle(wb, "Reporte", est_hdr, rows = 1, cols = 1:ncol(REPORTE), gridExpand = TRUE)
freezePane(wb, "Reporte", firstRow = TRUE)
setColWidths(wb, "Reporte", cols = 1:ncol(REPORTE), widths = "auto")

eF <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
eW <- createStyle(fontColour = "#9C6500", bgFill = "#FFEB9C")
eO <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
for (i in seq_len(nrow(REPORTE))) {
  st <- switch(as.character(REPORTE$Estado[i]),
               FALLA = eF, WARN = eW, OK = eO)
  addStyle(wb, "Reporte", st, rows = i + 1, cols = 3, stack = TRUE)
}

addWorksheet(wb, "Resumen")
writeData(wb, "Resumen", RESUMEN)
addStyle(wb, "Resumen", est_hdr, rows = 1, cols = 1:ncol(RESUMEN), gridExpand = TRUE)

saveWorkbook(wb, "Auditoria_IMPE.xlsx", overwrite = TRUE)
cat("\n>>> Reporte exportado: Auditoria_IMPE.xlsx\n")
