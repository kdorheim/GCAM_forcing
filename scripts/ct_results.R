# Reconstructing total RF from CT's results. 

# 0. Set Up --------------------------------------------------------------------

library(rgcam)

ct_data <- loadProject("gcam_db_ct.dat")
release_data <- loadProject("gcam_db.dat")


# Compare RF that we don't expect any changes during the historical period, 
# so N2O... 

ct_data$Reference$RF_tot %>% 
  select(scenario, year, total_rf = value) -> 
  total_rf

setdiff(all_rf_vars, gcam_rf_vars)


gcam_rf_vars <- c("RF_aci", "RF_OC", "RF_H2O_strat", "RF_O3_trop", "RF_BC", "RF_SO2", 
"RF_NH3", "RF_N2O", "FCH4", "RF_CO2", "RF_HFC125", "RF_HFC143a", "RF_HFC134a", "RF_SF6")

19 

# there are 20 inputs that are not modeled by gcam 

read.csv("auxiliary_data/default_RF.csv") %>% 
  filter(year %in% individual_gcam_rf$year) -> 
  default_rf


lapply(gcam_rf_vars, function(q){
  getQuery(ct_data, q) %>%  
    mutate(variable = q)
}) %>% 
  bind_rows -> 
  individual_gcam_rf


individual_gcam_rf %>% 
  bind_rows(default_rf) %>% 
  summarise(value = sum(value), .by = "year") -> 
  my_total

total_rf %>% 
  left_join(my_total) %>%  
  mutate(err = total_rf-value) %>% 
  filter(year > 1975) %>% 
  ggplot() + 
  geom_line(aes(year, err))


