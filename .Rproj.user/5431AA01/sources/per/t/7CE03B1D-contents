# ==============================================================================
# PIPELINE DE IMPUTACIÓN CIENTÍFICA PARA EL ÍNDICE I.M.P.E.
# Basado en: Van Buuren, S. (2012). Flexible Imputation of Missing Data. CRC Press.
# ==============================================================================

# Instalación de paquetes requeridos (descomentar si no están instalados)
#install.packages(c("mice", "zoo", "psych", "readxl", "dplyr", "tidyr"))


library(tidyverse)

# ------------------------------------------------------------------------------
# ETAPA 0: LIMPIEZA DE AGREGADOS DEL BANCO MUNDIAL Y DEFINICIÓN DEL HORIZONTE
# ------------------------------------------------------------------------------
# Leemos la hoja de metadatos del Excel para identificar y excluir los 48 
# agregados regionales/financieros ("WLD", "EUU", "ARB", etc.) que contaminan el panel
metadatos_wb <- readxl::read_excel(
 "Tesis_data/API_NV.MNF.TECH.ZS.UN_DS2_es_excel_v2_94833.xls", 
  sheet = "Metadata - Countries"
)

codigos_paises_reales <- metadatos_wb |>
  filter(Income_Group != "Agregados" & !is.na(`Country Code`)) |>
  pull(`Country Code`)

# Definición de las 16 variables del I.M.P.E. y la variable de control
vars_productiva  <- c("prod_manuf_pib", "prod_exp_total", "prod_fbcf_pib", 
                       "prod_eci", "prod_pib_pcap_ppa")
vars_financiera  <- c("fin_ied_sal_pib", "fin_mkt_cap_pib", 
                      "fin_cred_priv_pib", "fin_regalias_rec")
vars_tecnologica <- c("tech_id_pib", "tech_investigad", "tech_exp_alta", 
                      "tech_serv_ict_pct", "tech_exp_serv_dig", "tech_patentes_tic","tech_mnf_val_added")
vars_impe        <- c(vars_productiva, vars_financiera, vars_tecnologica)

# Filtramos: 1) Solo países soberanos reales, 2) Horizonte 1995-2024 (evita 
# extrapolación fuera del soporte histórico, Van Buuren, 2012, Sec. 3.4.4),
# 3) Países con al menos 35% de datos observados en el período (evita Unit Nonresponse, Sec. 2.2.2)
# 
panel_final <- read.csv("Tesis_data/panel_data_consolidado.csv")

panel_base <- panel_final |>
  filter(iso3c %in% codigos_paises_reales, year >= 1995, year <= 2024) |>
  group_by(iso3c) |>
  filter(mean(!is.na(across(all_of(vars_impe)))) >= 0.35) |>
  ungroup() |>
  arrange(iso3c, year)

# Diagnóstico de conectividad de datos faltantes: Influx y Outflux (Van Buuren, Sec. 4.1.3)
tabla_flux <- mice::flux(panel_base |> select(all_of(vars_impe), ctrl_pib_nominal))
cat("\n--- Diagnóstico de Influx y Outflux (Van Buuren, 2012, Sec. 4.1.3) ---\n")
print(round(tabla_flux[, c("pobs", "influx", "outflux")], 3))


# ==============================================================================
# CONTINUACIÓN OPTIMIZADA SEGÚN RESULTADOS REALES DE INFLUX / OUTFLUX
# ==============================================================================

# ------------------------------------------------------------------------------
# ETAPA 1: INTERPOLACIÓN LONGITUDINAL DE HUECOS INTERNOS CORTOS (<= 3 AÑOS)
# ------------------------------------------------------------------------------
panel_etapa1 <- panel_base |>
  dplyr::group_by(iso3c) |>
  dplyr::arrange(year, .by_group = TRUE) |>
  dplyr::mutate(
    dplyr::across(
      .cols = c(dplyr::all_of(vars_impe), ctrl_pib_nominal),
      .fns  = ~ zoo::na.approx(.x, x = year, maxgap = 3, na.rm = FALSE, rule = 1)
    )
  ) |>
  dplyr::ungroup()

# Verificar cuánto subió 'pobs' tras cerrar los huecos cortos de 1 a 3 años:
cat("\n--- Flux tras Etapa 1 (Interpolación de brechas <= 3 años) ---\n")
print(round(mice::flux(panel_etapa1 |> dplyr::select(dplyr::all_of(vars_impe), ctrl_pib_nominal))[, 1:3], 3))


# ------------------------------------------------------------------------------
# ETAPA 2: TRANSFORMACIÓN DE ASIMETRÍA PREVIA A MICE (Sec. 3.3.1 y 4.4.2)
# ------------------------------------------------------------------------------
vars_log <- c("prod_exp_total", "prod_pib_pcap_ppa", "fin_mkt_cap_pib", 
              "tech_exp_serv_dig", "tech_patentes_tic", "ctrl_pib_nominal")

panel_pre_mice <- panel_etapa1 |>
  dplyr::mutate(
    dplyr::across(dplyr::all_of(vars_log), ~ log1p(pmax(.x, 0))),
    fin_regalias_rec = asinh(fin_regalias_rec)
  )


# ------------------------------------------------------------------------------
# ETAPA 3: IMPUTACIÓN MÚLTIPLE MICE-PMM CON VISIT SEQUENCE MONOTONE
# ------------------------------------------------------------------------------
datos_para_mice <- panel_pre_mice |>
  dplyr::select(iso3c, year, ctrl_pib_nominal, dplyr::all_of(vars_impe))

# Definimos las variables del Bloque Ancla (Outflux > 0.66 en tu tabla) 
# para forzar su inclusión como predictoras universales (Van Buuren, Sec. 5.3.2)
bloque_ancla <- c("year", "ctrl_pib_nominal", "prod_exp_total", "prod_pib_pcap_ppa",
                  "prod_manuf_pib", "prod_fbcf_pib", "fin_cred_priv_pib", 
                  "fin_ied_sal_pib", "tech_serv_ict_pct", "prod_eci", "tech_mnf_val_added")

matriz_predictores <- mice::quickpred(
  datos_para_mice,
  mincor  = 0.15,
  minpuc  = 0.30,
  include = bloque_ancla,
  exclude = "iso3c"
)

# Nota: Usa m = 5 para verificar convergencia rápida. 
# Para la corrida definitiva de la tesis, cámbialo a m = 30 (tasa media de NAs = 32.3%, Sec. 2.7)
set.seed(2026)
tictoc::tic()
imputacion_mice <- mice::mice(
  data            = datos_para_mice,
  m               = 30,            # Cambiar a 30 para la corrida final de publicación
  maxit           = 10,
  method          = "pmm",
  donors          = 5,
  visitSequence   = "monotone",   # Ordena de menor a mayor % de NAs (Sec. 5.5.1)
  predictorMatrix = matriz_predictores,
  ridge           = 0.001,
  printFlag       = TRUE
)
tictoc::toc()

tictoc::tic()
imputacion_mice <- mice::mice(
  data            = datos_para_mice,
  m               = 30,            # Cambiar a 30 para la corrida final de publicación
  maxit           = 10,
  method          = "norm.predict",
  donors          = 5,
  visitSequence   = "monotone",   # Ordena de menor a mayor % de NAs (Sec. 5.5.1)
  predictorMatrix = matriz_predictores,
  ridge           = 0.001,
  printFlag       = TRUE
)
tictoc::toc()

mice::densityplot(
  imputacion_mice, 
  ~ tech_id_pib + tech_investigad + tech_exp_alta + tech_exp_serv_dig + 
    tech_patentes_tic + fin_mkt_cap_pib + fin_regalias_rec
)

?mice::mice

imputacion_mice$imp

# Diagnóstico de eventos registrados (verifica que no haya colinealidad crítica, Sec. 5.3.2)
print(imputacion_mice$loggedEvents)

# Gráficos de diagnóstico de Van Buuren (Sec. 5.6.2):
# 1. Densidades comparadas de las 7 variables del Bloque Receptor (Azul = Observado, Rojo = Imputado)
mice::densityplot(
  imputacion_mice, 
  ~ tech_id_pib + tech_investigad + tech_exp_alta + tech_exp_serv_dig + 
    tech_patentes_tic + fin_mkt_cap_pib + fin_regalias_rec
)

 # ETAPA 4: COMBINACIÓN (POOLING) DE LAS m IMPUTACIONES Y CÁLCULO DEL ACP E I.M.P.E.
 # Justificación: En lugar de promediar la base antes de analizar (Sec. 6.1.1), 
 # calculamos los pesos del ACP en cada una de las m bases y promediamos las cargas 
 # y puntajes respetando la varianza entre imputaciones (Cap. 6 y Reglas de Rubin, Sec. 2.3.2)
 # ------------------------------------------------------------------------------
 lista_imputaciones <- mice::complete(imputacion_mice, action = "all")
 analizar_imputacion <- function(df_imp) {
    # 1. Revertir transformaciones logarítmicas e hiperbólicas a su escala original
     df_escala_original <- df_imp |>
       dplyr::mutate(
           dplyr::across(dplyr::all_of(vars_log), ~ base::expm1(.x)),
         fin_regalias_rec = base::sinh(fin_regalias_rec))
  
       # 2. Estandarizar a Z-scores las 16 variables del I.M.P.E.
       mat_z <- df_escala_original |>
         dplyr::select(dplyr::all_of(vars_impe)) |>
         base::scale(center = TRUE, scale = TRUE)
        
           # 3. Análisis de Componentes Principales (ACP) sobre la imputación l
           pca_l <- stats::prcomp(mat_z, center = FALSE, scale. = FALSE)
          
             # Asegurar dirección positiva del CP1 (correlacionado positivamente con I+D)
             signo <- ifelse(pca_l$rotation["tech_id_pib", 1] < 0, -1, 1)
             autovector_cp1 <- pca_l$rotation[, 1] * signo
             autovalores_l  <- pca_l$sdev^2
            pesos_l        <- (autovector_cp1^2) / sum(autovector_cp1^2)
           
               # 4. Puntaje bruto del I.M.P.E.
               score_bruto <- as.vector(mat_z %*% pesos_l)
              
                 # 5. Matriz de correlación transformada a escala z de Fisher (base::atanh)
                 mat_cor <- stats::cor(mat_z)
                 cor_z   <- base::atanh(pmin(pmax(mat_cor, -0.9999), 0.9999))
                 
                   list(
                       autovalores = autovalores_l,
                       autovector1 = autovector_cp1,
                       pesos       = pesos_l,
                       cor_z       = cor_z,
                       df_scores   = df_escala_original |> dplyr::mutate(IMPE_Bruto = score_bruto))
}
# Ejecutar sobre las m bases imputadas
resultados_m <- lapply(lista_imputaciones, analizar_imputacion)
# 1. Matriz de Correlaciones Combinada vía inversa de Fisher (base::tanh)
cor_fisher_media  <- Reduce(`+`, lapply(resultados_m, `[[`, "cor_z")) / length(resultados_m)
matriz_cor_pooled <- base::tanh(cor_fisher_media)
diag(matriz_cor_pooled) <- 1




# 2. Tabla de Autovalores y Pesos Combinados (Reglas de Rubin)
pesos_matriz <- sapply(resultados_m, `[[`, "pesos") * 100
autoval_mat  <- sapply(resultados_m, `[[`, "autovalores")
tabla_autovalores_pooled <- dplyr::tibble(
Componente         = paste0("CP", seq_len(nrow(autoval_mat))),
Autovalor_Media    = round(rowMeans(autoval_mat), 4),
Autovalor_SD_Imp   = round(apply(autoval_mat, 1, stats::sd), 4),
Varianza_Pct       = round(rowMeans(autoval_mat) / sum(rowMeans(autoval_mat)) * 100, 2),
Varianza_Acum_Pct  = round(cumsum(rowMeans(autoval_mat) / sum(rowMeans(autoval_mat)) * 100), 2))
tabla_pesos_pooled <- dplyr::tibble(
  Variable           = vars_impe,
  Autovector_CP1     = round(rowMeans(sapply(resultados_m, `[[`, "autovector1")), 4),
  Peso_IMPE_Media_Pct = round(rowMeans(pesos_matriz), 2),
  Error_Entre_Imp_SD = round(apply(pesos_matriz, 1, stats::sd), 3))

cat("\n--- 1. Autovalores Combinados (Pooled Eigenvalues) ---\n")
print(tabla_autovalores_pooled, n = 16)

cat("\n--- 2. Ponderaciones Finales del I.M.P.E. Combinadas (Rubin's Rules) ---\n")
print(tabla_pesos_pooled, n = 16)

# 3. Consolidación del Panel Final con Puntaje I.M.P.E. (0 a 100)
scores_matriz <- sapply(resultados_m, function(x) x$df_scores$IMPE_Bruto)

panel_impe_final <- resultados_m[[1]]$df_scores |>
  dplyr::select(iso3c, year, ctrl_pib_nominal) |>
  dplyr::mutate(
    IMPE_Bruto_Pooled     = rowMeans(scores_matriz),
    IMPE_Incertidumbre_SD = apply(scores_matriz, 1, stats::sd),
    IMPE_100 = (IMPE_Bruto_Pooled - min(IMPE_Bruto_Pooled)) / 
      (max(IMPE_Bruto_Pooled) - min(IMPE_Bruto_Pooled)) * 100
  )

cat("\n--- 3. Muestra del Panel Final con el Índice I.M.P.E. ---\n")
print(head(panel_impe_final, 10))


# ==============================================================================
# ETAPA 5: VALIDACIÓN ESTATÍSTICA FINAL, SUBÍNDICES DIMENSIONALES Y EXPORTACIÓN
# ==============================================================================

# 1. PRUEBAS DE ADECUACIÓN MUESTRAL Y CONSISTENCIA INTERNA SOBRE LA MATRIZ COMBINADA
n_obs_panel <- nrow(panel_impe_final)

# 1.1 Índice KMO (Kaiser-Meyer-Olkin) sobre la matriz de correlación combinada (Fisher z)
kmo_pooled <- psych::KMO(matriz_cor_pooled)

# 1.2 Prueba de Esfericidad de Bartlett
bartlett_pooled <- psych::cortest.bartlett(matriz_cor_pooled, n = n_obs_panel)

# 1.3 Coeficiente Alfa de Cronbach Estandarizado
alfa_pooled <- psych::alpha(matriz_cor_pooled, n.obs = n_obs_panel, check.keys = TRUE)

cat("\n================ PRUEBAS DE VALIDACIÓN DEL I.M.P.E. ================\n")
cat("1. Índice KMO Global:", round(kmo_pooled$MSA, 4), "(> 0.80 = Meritorio)\n")
cat("2. Chi-Cuadrado de Bartlett:", round(bartlett_pooled$chisq, 2), 
    "| p-valor:", format.pval(bartlett_pooled$p.value, digits = 4), "\n")
cat("3. Alfa de Cronbach Estandarizado:", round(alfa_pooled$total$std.alpha, 4), 
    "(> 0.80 = Alta Consistencia Interna)\n")


# 2. CÁLCULO DE LOS SUBÍNDICES POR DIMENSIÓN TEÓRICA Y SU MATRIZ DE INTERACCIÓN
pesos_vector <- stats::setNames(tabla_pesos_pooled$Peso_IMPE_Media_Pct / 100, tabla_pesos_pooled$Variable)

pesos_prod_norm <- pesos_vector[vars_productiva]  / sum(pesos_vector[vars_productiva])
pesos_fin_norm  <- pesos_vector[vars_financiera]  / sum(pesos_vector[vars_financiera])
pesos_tech_norm <- pesos_vector[vars_tecnologica] / sum(pesos_vector[vars_tecnologica])

# Extraemos los subíndices en cada imputación y los combinamos por la media
calcular_subindices <- function(res_l) {
  mat_z <- res_l$df_scores |>
    dplyr::select(dplyr::all_of(vars_impe)) |>
    base::scale(center = TRUE, scale = TRUE)
  
  dplyr::tibble(
    Sub_Prod = as.vector(mat_z[, vars_productiva]  %*% pesos_prod_norm),
    Sub_Fin  = as.vector(mat_z[, vars_financiera]  %*% pesos_fin_norm),
    Sub_Tech = as.vector(mat_z[, vars_tecnologica] %*% pesos_tech_norm)
  )
}

lista_sub <- lapply(resultados_m, calcular_subindices)

reescalar_0_100 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100
}

panel_impe_final <- panel_impe_final |>
  dplyr::mutate(
    Sub_Productivo_100  = reescalar_0_100(rowMeans(sapply(lista_sub, `[[`, "Sub_Prod"))),
    Sub_Financiero_100  = reescalar_0_100(rowMeans(sapply(lista_sub, `[[`, "Sub_Fin"))),
    Sub_Tecnologico_100 = reescalar_0_100(rowMeans(sapply(lista_sub, `[[`, "Sub_Tech")))
  )

# Matriz de Correlación entre los Subíndices y el I.M.P.E. Global
cor_dimensiones <- panel_impe_final |>
  dplyr::select(IMPE_100, Sub_Tecnologico_100, Sub_Productivo_100, Sub_Financiero_100) |>
  stats::cor() |>
  round(4)

cat("\n================ INTERACCIÓN ENTRE LAS 3 DIMENSIONES Y EL I.M.P.E. ================\n")
print(cor_dimensiones)


# 3. TOP 15 POTENCIAS ECONÓMICAS EN EL ÚLTIMO AÑO DISPONIBLE Y EXPORTACIÓN
top15_impe <- panel_impe_final |>
  dplyr::filter(year == max(year)) |>
  dplyr::arrange(dplyr::desc(IMPE_100)) |>
  dplyr::select(iso3c, year, IMPE_100, IMPE_Incertidumbre_SD, 
                Sub_Tecnologico_100, Sub_Productivo_100, Sub_Financiero_100) |>
  head(15)

cat("\n================ TOP 15 PAÍSES EN EL RANKING I.M.P.E. ================\n")
print(top15_impe, n = 15)

# Exportar tablas finales a CSV para los anexos y gráficos de la Tesis
readr::write_csv(panel_impe_final, "IMPE_Panel_Final_1995_2024.csv")
readr::write_csv(tabla_autovalores_pooled, "IMPE_Tabla_Autovalores_Pooled.csv")
readr::write_csv(tabla_pesos_pooled, "IMPE_Tabla_Pesos_Pooled.csv")
readr::write_csv(auditoria_items, "Auditoria_de_Itmes.csv")
