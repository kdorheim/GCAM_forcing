# Compare the total RF from the output stream with the GCAM results... 

# 0. Set Up --------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(rgcam)

# This is the V3.2 version that actually works on my machine, this should 
# be consistent with the version of Hector that is currently being run... 
remotes::install_github("jgcri/hector@a61d64a")
library(hector)
packageVersion("hector")


# 1. Load Data -----------------------------------------------------------------


vars <- c("CO2_concentration", "RF_OC", "RF_H2O_strat", "RF_O3_trop", 
          "RF_BC", "RF_SO2", "RF_N2O", "FCH4", "RF_CO2", "RF_tot", "gmst")


here::here("GCAM-V7.0-materials", "gcam-hector-outputstream.csv") %>% 
  read.csv(comment.char = "#") %>% 
  filter(spinup == 0) %>% 
  distinct() %>%  
  mutate(source = "GHoutputstream") -> 
  all_gh_outputstream

all_gh_outputstream %>% 
  filter(variable %in% vars) %>% 
  distinct -> 
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
  bind_rows() %>% 
  filter(year > 1975) -> 
  gcam_xmldb


# 2. Compare the absolute error of the different variables ---------------------
# What is the level of precision of difference we are expecting... 
# I think there is something going on related to the second digit... 

bind_rows(gcam_xmldb, all_rf_gh_outputstream) %>% 
  select(year, value, source, variable) %>% 
  mutate(value = round(value, 2)) %>% 
  na.omit %>% 
  pivot_wider(names_from = "source")  %>% 
  na.omit %>% 
  mutate(err = (xmldb - GHoutputstream), .by = "variable") -> 
  err_df

err_df %>%  
  filter(variable == CONCENTRATIONS_CO2()) %>% 
  ggplot(aes(year, err)) + 
  geom_line()

err_df %>% 
  summarise(MAE = mean(abs(err)), .by = "variable") %>% 
  arrange(desc(MAE))


# 3. Total vs. Cumulative RF (output stream )-----------------------------------

# For the output stream 
all_gh_outputstream %>% 
  filter(grepl(pattern = "rf|fch4", x = tolower(variable))) -> 
  outputstream_rf_only

# This is the native total RF reported in the gcam hector output stream. 
outputstream_rf_only %>% 
  filter(variable == RF_TOTAL()) %>% 
  select(year, total_rf = value) -> 
  gh_total_rf
  
# So it looks like there are only 38 RF contributions to total RF but 
outputstream_rf_only %>% 
  filter(!variable == RF_TOTAL()) %>% #pull(variable) %>% unique() -> all_rf_vars #%>% length()
  summarise(my_total = sum(value), .by = year) -> 
  my_total_rf

gh_total_rf %>%  
  left_join(my_total_rf) %>% 
  mutate(err = total_rf - my_total) -> 
  err_df

err_df %>% 
  summarise(min = min(err), 
            max = max(err), 
            MAE = mean(abs(err)), 
            sd = sd(err))

plot(err_df$err)









