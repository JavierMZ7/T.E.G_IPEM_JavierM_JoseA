library(naniar)
paneles_20 <- readRDS("paneles_20_indices.rds")

vars_12 <- c(
  "prod_eci", "prod_exp_total", "prod_pib_pcap_ppa",
  "fin_regalias_rec", "fin_cred_priv_pib", "fin_mkt_cap_pib",
  "tech_exp_serv_dig", "tech_patentes_tic", "tech_id_pib",
  "tech_investigad", "tech_mnf_val_added", "tech_exp_alta"
)

# Prueba de Little (1988) para 2B (depurada) y 2A (completa)
mcar_2B <- mcar_test(paneles_20[["2B_Base12_Depurada25_Raw"]][, vars_12])
mcar_2A <- mcar_test(paneles_20[["2A_Base12_Completa_Raw"]][, vars_12])

print(mcar_2B)
print(mcar_2A)