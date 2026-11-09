install.packages("dplyr")
install.packages("WDI")
install.packages("tidyr")
install.packages("tidyverse")
##remotes::install_github("vdeminstitute/vdemdata") ##no me deja instalar Vdem el indos defender 

library(WDI)       
##library(vdemdata)  
library(dplyr)     
library(tidyr)
library(tidyverse)
library(readxl)


wb_data = WDI(
  indicator = c(
    # Dimensión Productiva
    "prod_manuf_pib" = "NV.IND.MANF.ZS",       # Valor agregado manufactura (% PIB)
    "prod_exp_total" = "TX.VAL.MRCH.CD.WT",     # Exportaciones de mercancías (USD)
    "prod_fbcf_pib" = "NE.GDI.FTOT.ZS",        # Formación Bruta de Capital Fijo (% PIB)
    "prod_pib_pcap_ppa" = "NY.GDP.PCAP.PP.KD",      # PIB per cápita PPA (USD constantes)  Proxy de Riqueza y Desarrollo o Excendente
    ## va a ser incluid ocomo un excel, y enviado a la dimension Technologica "prod_manuf_mid&alta_tech_valadd" = "NV_MNF_TECH_ZS_UN", #Medium and high-tech manufacturing value added (% manufacturing value added)
    
    # Dimensión financiera
    "fin_ied_sal_pib"   = "BM.KLT.DINV.WD.GD.ZS",  # IED saliente (% PIB)
    "fin_mkt_cap_pib"   = "CM.MKT.LCAP.GD.ZS",     # Capitalización bursátil (% PIB)
    "fin_cred_priv_pib" = "FS.AST.PRVT.GD.ZS",     # Crédito al sector privado (% PIB)
    "fin_regalias_rec"  = "BX.GSR.ROYL.CD",        # Ingresos por propiedad intelectual (USD)
    
    # Dimensión tecnologica 
    "tech_id_pib"       = "GB.XPD.RSDV.GD.ZS",     # Gasto en I+D (% PIB)
    "tech_investigad"   = "SP.POP.SCIE.RD.P6",     # Investigadores en I+D (por millón)
    "tech_exp_alta"     = "TX.VAL.TECH.MF.ZS",     # Exportaciones alta tecnología (% manuf.)
    "tech_exp_mid&alta" = "TX_MNF_TECH_ZS_UN",     # Medium and high-tech exports (% manufactured exports)
    "tech_serv_ict_pct" = "BX.GSR.CCIS.ZS",        # Servicios TIC (% exportación servicios)
    
    #VARIABLE DE CONTORL
    "ctrl_pib_nominal"   = "NY.GDP.MKTP.CD"  
    ),
  country = "all",
  start = 1960,   
  end = 2025     
  )

stringsAsfactors = FALSE

eci_data <- read.csv("eci.csv") %>%
  select(iso3c = country_iso3_code, year, prod_eci = eci_hs92, crtl_eci_rank = eci_rank_hs92)


unctad_export_data <- read.csv("UNCTAD_DE_DIG_SERVTRADE_ANN_EXP.csv", stringsAsFactors = FALSE) %>%
  select(iso3c = REF_AREA, year = TIME_PERIOD, tech_exp_serv_dig = OBS_VALUE) %>%
  mutate(year = as.numeric(year), tech_exp_serv_dig = as.numeric(tech_exp_serv_dig))

unctad_import_data <- read.csv("UNCTAD_DE_DIG_SERVTRADE_ANN_IMP.csv", stringsAsFactors = FALSE) %>%
  select(iso3c = REF_AREA, year = TIME_PERIOD, tech_imp_serv_dig = OBS_VALUE) %>%
  mutate(year = as.numeric(year), tech_imp_serv_dig = as.numeric(tech_imp_serv_dig))


wipo_data <- read.csv("P.csv") %>%
  select(iso3c = REF_AREA, year = TIME_PERIOD, tech_patentes_tic = OBS_VALUE) %>%
  mutate(year = as.numeric(year), tech_patentes_tic = as.numeric(tech_patentes_tic))


tech_manf_panel <- read_excel("API_NV.MNF.TECH.ZS.UN_DS2_es_excel_v2_94833.xls", sheet = "Data", skip = 3) %>%
  rename(iso3c = `Country Code`) %>%
  pivot_longer(cols = matches("^[12][0-9]{3}"), names_to = "year", values_to = "tech_mnf_val_added") %>%
  mutate(year = as.numeric(year)) %>%
  select(iso3c, year, tech_mnf_val_added)


# 5. UNIÓN EN UNA SOLA TABLA MAESTRA
panel_data <- wb_data %>%
  left_join(eci_data, by = c("iso3c", "year")) %>%
  left_join(unctad_export_data, by = c("iso3c", "year")) %>%
  left_join(unctad_import_data, by = c("iso3c", "year")) %>%
  left_join(wipo_data, by = c("iso3c", "year")) %>%
  left_join(tech_manf_panel, by = c("iso3c", "year")) %>%
  arrange(iso3c, year)

# 6. GUARDAR Y REVISAR EL RESULTADO
write.csv(panel_data, "panel_data_consolidado.csv", row.names = FALSE)

arrow::write_parquet(panel_data, sink = "panel_data_parquet") 

