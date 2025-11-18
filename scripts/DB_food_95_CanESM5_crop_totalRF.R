# Reconstructing DB_food_95_CanESM5_crop total RF from individual RF components. 


# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(rgcam)
library(hector)
library(ggplot2)
theme_set(theme_bw())

# Function that extracts all the individual RF values from a GCAM database. 
# Args
#   dat_file: the path to the data extracted from a GCAM xml database 
# Returns: data.frame of the individual RF values 
get_gcam_rf <- function(dat_file){
  
  # Load the exported GCAM file 
  stopifnot(file.exists(dat_file))
  gcam_db <- loadProject(dat_file)
  
  # Get all the individual RF queires from from 
  # the GCAM data. 
  c("RF_OC", "RF_H2O_strat", "RF_O3_trop", "RF_BC", "RF_SO2", 
    "RF_N2O", "FCH4", "RF_CO2", "RF_HFC125", "RF_HFC143a", "RF_HFC134a", 
    "RF_SF6", "RF_CF4", "RF_HFC23", "RF_HFC32", "RF_HFC245fa", 
    "RF_aci", "RF_NH3") %>% 
    lapply(function(v){
      getQuery(gcam_db, v) %>% 
        mutate(variable = v)
    }) %>% 
    bind_rows -> 
    individual_gcam_rf
  
  # There are two halocarbon RF time series that have to be 
  # extracted from aggregate queries because the individual 
  # ones are not passed to the GCAM xml output (see the 
  # GCAM xmldb outputter cpp script) 
  
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
  
  # Due to a bug, the RF_CH4 halocabron rf has to be normalized to the 
  # 1750 value. 
  RF_CF4_ref <- 0.00346497 # Adjust ref RF_CF4 value comes from a hector log file 
  
  bind_rows(individual_gcam_rf, 
            RF_HFC227ea_df, 
            RF_C2F6_df) %>% 
    mutate(value = if_else(variable == "RF_CF4", value - RF_CF4_ref, value)) -> 
    out
  
  return(out)
  
}



# 1. Individual RF -------------------------------------------------------------

# From the GCAM data base
gcam_indivd_rf <- get_gcam_rf("ct_gcam_db.dat")

# Load the default RF (do not vary with GCAM run)
default_indvid_rf <- read.csv("auxiliary_data/default_RF.csv")

# Save results
all_individ_rf <- bind_rows(gcam_indivd_rf, default_indvid_rf)

# 2. Total RF Comparison--------------------------------------------------------

getQuery(loadProject("ct_gcam_db.dat"), "RF_tot") %>% 
  mutate(variable = "GCAM total rf") -> 
  gcam_rf_total

ggplot() + 
  geom_area(data = all_individ_rf, aes(year, value, fill = variable)) + 
 geom_line(data = gcam_rf_total, aes(year, value)) + 
  labs(title = "Comparison of GCAM total RF and individual RF", 
       y = "W/m-2", x = "Year", caption = "GCAM's RF total (black line)")

all_individ_rf %>% 
  summarise(value = sum(value), .by = "year") %>% 
  mutate(variable = "aggregate total rf") -> 
  my_total


# summary of the differences 
bind_rows(gcam_rf_total, 
          my_total) %>% 
  select(year, variable, value) %>% 
  mutate(value = round(value, 2)) %>% 
  pivot_wider(names_from = "variable") %>% 
  mutate(AE = abs(`GCAM total rf` - `aggregate total rf`)) %>% 
  summarise(min_AE = min(AE), 
            max_AE = max(AE), 
            mean_AE = mean(AE)) -> 
  summary_stats

bind_rows(gcam_rf_total, 
          my_total) %>% 
  ggplot(aes(year, value, color = variable, linetype = variable)) + 
  geom_line(linewidth = 1.5) + 
  labs(title = "Comparison of GCAM total RF and sum of individual RF", 
       y = "W/m-2", x = "Year", caption = paste0("MAE between total RF: ", summary_stats$mean_AE)) 





