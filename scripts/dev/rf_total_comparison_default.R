# GCAM V7 aggregating up to total RF 


# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(rgcam)

# This is the V3.2 version that actually works on my machine, hopeful 
# it is consistent with the version of Hector that is currently being run... 
remotes::install_github("jgcri/hector@a61d64a")
library(hector)
packageVersion("hector")



## 0A. Load Data ---------------------------------------------------------------
# The outputstream written to the logs file. 
here::here("GCAM-V7.0-materials", "gcam-hector-outputstream.csv") %>% 
  read.csv(comment.char = "#") %>% 
  filter(spinup == 0) %>% 
  distinct() %>%  
  mutate(source = "gh-outputstream") -> 
  all_rf_gh_outputstream

# Load the reference run total RF 
proj_data <- loadProject("gcam_db.dat") # this assume that the extract gcam output script has already been run

vars <- c("CO2_concentration", "RF_OC", "RF_H2O_strat", "RF_O3_trop", 
          "RF_BC", "RF_SO2", "RF_N2O", "FCH4", "RF_CO2", "RF_tot", "gmst")


lapply(vars, function(v){
  
  getQuery(proj_data, v) %>% 
    mutate(source = "xmldb") %>% 
    mutate(variable = v)
  
}) %>% 
  bind_rows() -> 
  gcam_xmldb

# 1. tot RF from output stream -------------------------------------------------
# Are we able to get the total RF from the output stream?  
all_rf_gh_outputstream %>% 
  filter(variable == RF_TOTAL()) %>% 
  select(year, variable, value, source) -> 
  total_rf

all_rf_gh_outputstream %>% 
  filter(grepl(x = variable, pattern = "RF|FCH4")) %>% 
  filter(variable != RF_TOTAL()) -> 
  rf_values
  
rf_values %>% 
  summarise(agg = sum(value), .by = "year") -> 
  sum_componets

total_rf %>% 
  left_join(sum_componets) %>% 
  mutate(AE =  abs(value - agg)) %>% 
  summarise(MAE = mean(AE), 
            min = min(AE), 
            max = max(AE), 
            sd = sd(AE))


gcam_xmldb %>% 
  filter(variable == RF_TOTAL()) %>% 
  bind_rows(total_rf) %>% 
  filter(variable != 1975) %>% 
  ggplot(aes(year, value, color = source)) + 
  geom_line()


# Extract the forcing that are going to be consistent regardless of GCAM 
# emissions. 
"GCAM-V7.0-materials/climate/default_emissions.csv" %>% 
  read.csv(comment.char = ";") %>% 
  select(-c(Date, SV)) %>% 
  names() %>% 
  gsub(pattern = "_emissions", replace = "") %>% 
  c(., RF_VOL(), RF_MISC()) -> 
  default_species

rf_values %>% 
  filter(grepl(x = variable, pattern = paste0(default_species, collapse = "|"))) -> 
  individ_default_rf

individ_default_rf %>% 
  summarise(value = sum(value), .by = "year") %>% 
  mutate(variable = "RF_default") -> 
  default_RF

write.csv(default_RF, file = "auxiliary_data/default_RF.csv", row.names = FALSE)


individ_default_rf$variable %>%  unique() %>%  length()



# What are the remaining RF values that are missing and needs to be accounted 
# for... 

# 20, so it seems like there are 20 RF that we need... 
gcam_rf_names <- setdiff(unique(rf_values$variable), individ_default_rf$variable) 
setdiff(rf_list, gcam_rf_names )

setdiff(gcam_rf_names, listQueries(proj_data))


