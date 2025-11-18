# Get the RF that will no change regardless of the GCAM scenario 
library(dplyr)
library(tidyr)
library(ggplot2)

# 0. Load the data  ----------------------------------------------------------------

# The outputstream written to the logs file. 
here::here("GCAM-V7.0-materials", "gcam-hector-outputstream.csv") %>% 
  read.csv(comment.char = "#") %>% 
  filter(spinup == 0) %>% 
  distinct() %>%  
  mutate(source = "gh-outputstream") %>% 
  filter(grepl(pattern = "RF|FCH4", variable)) -> 
  all_rf_values

# 1. Default RF ----------------------------------------------------------------
# Extract the forcing that are going to be consistent regardless of GCAM 
# emissions. 
"GCAM-V7.0-materials/climate/default_emissions.csv" %>% 
  read.csv(comment.char = ";") %>% 
  select(-c(Date, SV)) %>% 
  names() %>% 
  gsub(pattern = "_emissions", replace = "") %>% 
  c(., RF_VOL(), RF_MISC()) %>% 
  # PROBLEM QUEREIS -- should remove 
  c(., c(RF_ACI(), RF_NH3())) -> 
  default_species

all_rf_values %>% 
  filter(grepl(x = variable, pattern = paste0(default_species, collapse = "|"))) -> 
  individ_default_rf

individ_default_rf %>% 
  summarise(value = sum(value), .by = "year") %>% 
  mutate(variable = "RF_default") -> 
  default_RF

#write.csv(default_RF, file = "auxiliary_data/default_RF.csv", row.names = FALSE)


# 2. QAQC ----------------------------------------------------------------------
# These are the RF values that are easy to extract from GCAM at the moment 
gcam_rf_vars <- c( "RF_OC", "RF_H2O_strat", "RF_O3_trop", "RF_BC", "RF_SO2", 
                   "RF_N2O", "FCH4", "RF_CO2", "RF_HFC125", "RF_HFC143a", "RF_HFC134a", 
                   "RF_SF6", "RF_CF4", "RF_HFC23", "RF_HFC32", "RF_HFC245fa") # RF_aci, RF_NH3 -- these might be a problem but i htin kthat those might be helpufl for 


get_gcam_rf <- function(dat_file){
  
  gcam_db <- loadProject(dat_file)
  
  
  lapply(gcam_rf_vars, function(v){
    getQuery(gcam_db, v) %>% 
      mutate(variable = v)
  }) %>% 
    bind_rows -> 
    individual_gcam_rf
  
  # From GCAM xmldb outputter
  # RF_C2F6 can be extracted from the PFCs Forcing 
  individual_gcam_rf %>%  
    filter(variable == "RF_CF4") %>% 
    select(year, RF_CF4 = value) -> 
    RF_CF4_df
  
  getQuery(gcam_db, "forcing-PFCs") %>% 
    left_join(RF_CF4_df) %>% 
    mutate(value = value - RF_CF4) %>% 
    select(scenario, year, value) %>% 
    mutate(variable = "RF_C2F6") -> 
    RF_C2F6_df
  
  # From GCAM xmldb outputter
  # HFCs Forcing can be used to get RF_HFC227ea
  individual_gcam_rf %>% 
    filter(grepl(x = tolower(variable),
                 pattern = tolower("HFC125|HFC134A|HFC143A|HFC245fa|HFC23|HFC32"))) %>% 
    summarise(agg = sum(value), .by = "year")  -> 
    hfc_agg
  
  getQuery(gcam_db, "forcing-HFCs") %>% 
    left_join(hfc_agg) %>% 
    mutate(value = value - agg) %>% 
    select(scenario, year, value) %>% 
    mutate(variable = "RF_HFC227ea") -> 
    RF_HFC227ea_df
  
  # Adjust the RF_CF4 based using the value from the log file 
  RF_CF4_ref <- 0.00346497
  
  # Add the extracted halocarbon RF values the results. 
  bind_rows(individual_gcam_rf, 
            RF_HFC227ea_df, 
            RF_C2F6_df) %>% 
    mutate(value = if_else(variable == "RF_CF4", value - RF_CF4_ref, value)) -> 
    out
  
  return(out)
  
}


gcam_rf <-  get_gcam_rf("gcam_db_ref.dat")
# okay what are the 

gcam_rf %>% 
  bind_rows(default_RF) %>% 
  summarise(my_total = sum(value), .by = "year") -> 
  my_total

getQuery(gcam_db, "RF_tot")  %>% 
  left_join(my_total) %>% 
  filter(year > 1975) %>% 
  mutate(err = value - my_total) %>% 
  ggplot(aes(year, err))  + 
  geom_line()
