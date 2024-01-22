
# Script to load data & functions for analysis 

# load required packages and functions
pacman::p_load(tidyverse,
               plm, 
               countrycode, 
               magrittr, 
               zoo, 
               ggdendro, 
               cowplot, 
               readxl,
               lubridate,
               data.table,
               pvclust,
               e1071,
               factoextra,
               Hmisc,
               ggdendro,
               progress,
               ggsci,
               gridExtra,
               ggh4x,
               # covidregionaldata,
               MMWRweek,
               imputeTS,
               qs,
               sf,
               tidyverse,
               EpiNow2, 
               countrycode, 
               zoo, 
               cowplot, 
               lubridate,
               covidregionaldata)
# remotes::install_github("epiforecasts/covidregionaldata")
require(covidregionaldata)

# Custom functions for regression exercise
source("code/util/aicbic_plm.R")
source("code/util/select_variable.R")
source("code/util/find_lag.R")
source("code/util/plot_all_lags.R")
source("code/util/aicbic_select.R")

path_data <- "/Users/yangliu/Library/CloudStorage/Dropbox/Github_Data/NPI_Europe/"

country_index <- data.frame(CountryName = c("Albania","Andorra","Armenia","Austria","Azerbaijan","Belarus",
                                            "Belgium","Bosnia & Herzegovina","Bulgaria","Croatia","Cyprus","Czechia",
                                            "Denmark","Estonia","Finland","France","Georgia","Germany",
                                            "Greece","Hungary","Iceland","Ireland","Israel","Italy",
                                            "Kazakhstan","Kyrgyzstan","Latvia","Lithuania", "Luxembourg","Malta",
                                            "Moldova","Monaco","Montenegro","Netherlands","North Macedonia","Norway","Poland",
                                            "Portugal","Romania","Russia","San Marino","Serbia",
                                            "Slovakia","Slovenia", "Spain","Sweden","Switzerland","Tajikistan",
                                            "Turkey","Turkmenistan","Ukraine",
                                            "United Kingdom","Uzbekistan")) %>% 
  mutate(iso3c = countrycode::countrycode(CountryName,"country.name","iso3c"),
         cctld = countrycode(CountryName, "country.name", "cctld"),
         cctld = gsub("\\.","",cctld),
         cctld = toupper(cctld))

case_data <- read_rds(paste0(path_data, "case_data.rds"))
rt_estimates <- read_rds(paste0(path_data, "rt_20230404.rds")) %>% 
  left_join(country_index, by = c("country" = "CountryName")) %>% 
  dplyr::select(country, iso3c, cctld, date, median, mean, lower_90, upper_90, lower_50, upper_50, sd)

# Load full data for analysis 
# joined <- readRDS("data/joined_all_V8.RDS")
# joined <- readRDS("data/joined_V9.rds")

#~#~# The following script creates the data used in the analysis (joined_all_V8.RDS) #~#~#

# Only run the following script to see how it was compiled
# oxcgrt <- read_csv(file = "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_nat_latest.csv",
#                    col_names = T)
oxford_data       <- read_csv(paste0(path_data, "NPI_OX.csv"))    # Oxford government response tracker data for NPIs/PHSMs
oxford_data_latest <- read_rds(paste0(path_data, "oxford_data_latest.rds"))
source("code/0_1_VaccineData.R")
# vaccine_data_owin <- read_csv(paste0(path_data, "VAC_OWIN_v2.csv"))  # Vaccine coverage data, calculated from script - 
# rt_estimates      <- read_csv(paste0(path_data, "rt_EURO.csv"))   # Rt estimate data, calculated from script - 
comix <- qread(paste0(path_data, "20220531_bs_means_2w.qs")) %>% 
  mutate(panel = factor(panel)) %>% 
  split(., 1:nrow(.)) %>% 
  map(mutate, date_range = list(seq(start_date,
                               end_date,
                               by = "day"))) %>% 
  map(unnest, cols = date_range) %>% 
  bind_rows() %>% 
  dplyr::select(-start_date, - mid_date, - end_date) %>% 
  rename(date = date_range)

source("code/0_2_VariantPeriod.R")

# Build policy_dic, lookup tibble of policy codes and names 
policy_dic <- colnames(oxford_data) %>% 
  .[grep(paste(c(paste0("C",1:8),
                 paste0("E",1:2),
                 paste0("H",1:6)), collapse = "|"), .,ignore.case = F)] %>% 
  str_split(pattern = "_") %>% 
  do.call("rbind",.) %>% 
  as_tibble %>% 
  setNames(c("policy_code","policy_name")) %>% 
  mutate(policy_max = c(3, 3, 2, 4, 2, 3, 2, 4,
                        2, 2,
                        2, 3, 2, 4)) %>% 
  mutate(cat = case_when(policy_code %in% 
                           paste0("C",1:8) ~ "Closure & Containment  ",
                         # policy_code == "C8" ~ "Intl Travel Restriction  ",
                         policy_code %in%
                           paste0("E", 1:4) ~ "Economic Response  ",
                         policy_code %in%
                           paste0("H", 1:6) ~ "Public Health & Health System Response  "),
         lab = gsub("\\.", " ", policy_name))#

policy_dic_V <- policy_dic %>% 
  add_row(policy_code = "prop", 
          policy_name = "Vaccine coverage - all population", 
          policy_max = 1, 
          cat = "Vaccination ", 
          lab = "Vaccine coverage - all population")

# Build policy_data
policy_data <- oxford_data_latest
# policy_data <-  oxford_data %>%
#   select(country, cnt, date, contains("_")) %>%
#   pivot_longer(cols = contains("_"), names_to = "policy_name") %>%
#   mutate(policy_name = str_replace(policy_name, "_.*", ""))

# check for missing data
# for(i in policy_dic$policy_code){
#   policy_data %>% 
#     filter(policy_name == i) %>% 
#     ggplot(., aes(x = date, y = value)) +
#     geom_point() +
#     geom_line() +
#     facet_wrap(~country) -> p
#   
#   ggsave(paste0("figs/intermediate/policy_raw/",i,"_new2.png"),
#          p)
# }
# 
# policy_data %>% 
#   filter(is.na(value)) %>% 
#   group_by(country, policy_name) %>% tally %>% 
#   arrange(desc(n))

# Impute missing data
# policy_data_1 <-  policy_data %>% 
#   group_by(cnt, policy_name) %>% 
#   arrange(date) %>% 
#   nest() %>% 
#   mutate(missing = map(data, ~any(is.na(.x$value))) %>% unlist,
#          all = map(data, ~all(is.na(.x$value))) %>% unlist) %>% 
#   group_by(missing) %>% 
#   group_split()
# 
# policy_data_2 <-  policy_data_1[[2]] %>% 
#   mutate(data = map(data, arrange, date),
#          # missingness at the tails are replaced by the last or the next 
#          # non-missing values; missingness not at the tails are replaced
#          # based on linear interpolation.
#          data = map(data, mutate, 
#                     value = imputeTS::na_interpolation(value))) %>% 
#   bind_rows(policy_data_1[[1]]) %>% 
#   dplyr::select(-c(missing, all)) %>% 
#   unnest(cols = data)

# set maximum and minimum values
# policy_data_3 <-  policy_data_2 %>% 
#   group_by(policy_name) %>% 
#   summarise(policy_value_UL = max(value, na.rm = T)) %>% 
#   right_join(policy_data_2, by = "policy_name") %>% 
#   arrange(cnt, policy_name) %>% 
#   mutate(policy_value_LL = 0) %>% 
#   select(country, cnt, date, policy_name, value, policy_value_LL, policy_value_UL) %>% 
#   mutate(policy_max = if_else(value < policy_value_UL, 0, 1)) %>% 
#   mutate(policy_any = if_else(value > 0, 1, 0)) %>% 
#   mutate(country = if_else(country == "Bosnia and Herzegovina", "Bosnia & Herzegovina", country))

policy_data_3 <-  policy_data %>% 
  group_by(policy_name) %>% 
  summarise(policy_value_UL = max(value, na.rm = T)) %>% 
  right_join(policy_data, by = "policy_name") %>% 
  arrange(iso3c, policy_name) %>% 
  mutate(policy_value_LL = 0) %>% 
  dplyr::select(country, iso3c, date, policy_name, value, policy_value_LL, policy_value_UL) %>% 
  mutate(policy_max = if_else(value < policy_value_UL, 0, 1)) %>% 
  mutate(policy_any = if_else(value > policy_value_LL, 1, 0)) %>% 
  mutate(country = if_else(country == "Bosnia and Herzegovina", "Bosnia & Herzegovina", country)) 

# Total country in WHO Europe region, n = 53
# Check to see if these can be resolved later 

unique(policy_data_3$country) %>% length() # n = 49,  rm(Turkmenistan, Armenia, Montenegro & North Macedonia)
unique(rt_estimates$country) %>% length()  # n = 52, rm(Turkmenistan)
unique(cov$iso3c) %>% length() # n = 52, rm(Turkmenistan)

# Pivot to wide datasets and set factors

# Any Efforts
# joined_any <- policy_data_3 %>%
#   dplyr::select(-c(policy_max,
#                    policy_value_LL,
#                    policy_value_UL,
#                    value,
#                    country)) %>%
#   rename(value = policy_any) %>%
#   pivot_wider(names_from = policy_name, values_from = value) %>%
#   group_by(iso3c, date) %>%
#   left_join(cov %>% dplyr::select(-CountryName),
#             by = c("iso3c", "date")) %>%
#   left_join(rt_estimates %>% dplyr::select(-country),
#             by = c("iso3c", "date")) %>%
#   ungroup() %>% 
#   mutate(phase = case_when(date < voc_switch_inuse$date[1] ~ "Wildtype",
#                            date >= voc_switch_inuse$date[1] & date < voc_switch_inuse$date[2] ~ voc_switch_inuse$voc_name_short[1],
#                            date >= voc_switch_inuse$date[2] & date < voc_switch_inuse$date[3] ~ voc_switch_inuse$voc_name_short[2],
#                            date >= voc_switch_inuse$date[3] ~ voc_switch_inuse$voc_name_short[3]),
#          phase = factor(phase,
#                         levels = c("Wildtype",
#                                    "Alpha",
#                                    "Delta",
#                                    "Omicron"))) 

# # Continuous Efforts
generate_con <- function(i){
  policy_data_3 %>%
    dplyr::select(-c(policy_max,
                     policy_value_LL,
                     policy_any)) %>%
    mutate(value = value/policy_value_UL) %>%
    dplyr::select(-policy_value_UL) %>%
    pivot_wider(names_from = policy_name, values_from = value) %>%
    group_by(iso3c, date) %>%
    left_join(cov %>% dplyr::select(-CountryName),
              by = c("iso3c", "date")) %>%
    left_join(rt_estimates %>%
                dplyr::select(-country),
              by = c("iso3c", "date")) %>%
    ungroup() %>% 
    mutate(phase = case_when(date < voc_switch_list[[i]]$date[1] ~ "Wildtype",
                             date >= voc_switch_list[[i]]$date[1] & date < voc_switch_list[[i]]$date[2] ~ voc_switch_list[[i]]$voc_name_short[1],
                             date >= voc_switch_list[[i]]$date[2] & date < voc_switch_list[[i]]$date[3] ~ voc_switch_list[[i]]$voc_name_short[2],
                             date >= voc_switch_list[[i]]$date[3] ~ voc_switch_list[[i]]$voc_name_short[3]),
           phase = factor(phase,
                          levels = c("Wildtype",
                                     "Alpha",
                                     "Delta",
                                     "Omicron"))) -> tmp
  return(tmp)
}

joined_con <- policy_data_3 %>%
  dplyr::select(-c(policy_max,
                   policy_value_LL,
                   policy_any)) %>%
  mutate(value = value/policy_value_UL) %>%
  dplyr::select(-policy_value_UL) %>%
  pivot_wider(names_from = policy_name, values_from = value) %>%
  group_by(iso3c, date) %>%
  left_join(cov %>% dplyr::select(-CountryName),
            by = c("iso3c", "date")) %>%
  left_join(rt_estimates %>%
              dplyr::select(-country),
            by = c("iso3c", "date")) %>%
  ungroup() %>% 
  mutate(phase = case_when(date < voc_switch_inuse$date[1] ~ "Wildtype",
                           date >= voc_switch_inuse$date[1] & date < voc_switch_inuse$date[2] ~ voc_switch_inuse$voc_name_short[1],
                           date >= voc_switch_inuse$date[2] & date < voc_switch_inuse$date[3] ~ voc_switch_inuse$voc_name_short[2],
                           date >= voc_switch_inuse$date[3] ~ voc_switch_inuse$voc_name_short[3]),
         phase = factor(phase,
                        levels = c("Wildtype",
                                   "Alpha",
                                   "Delta",
                                   "Omicron"))) 
# 
# # Max effort
# joined_max <- policy_data_3 %>%
#   dplyr::select(-c(policy_value_UL,
#                    policy_value_LL,
#                    policy_any)) %>%
#   mutate(value = policy_max) %>%
#   dplyr::select(-policy_max) %>%
#   pivot_wider(names_from = policy_name, values_from = value) %>%
#   group_by(iso3c, date) %>%
#   left_join(cov %>% dplyr::select(-CountryName),
#             by = c("iso3c", "date")) %>%
#   left_join(rt_estimates %>%
#               dplyr::select(-country),
#             by = c("iso3c", "date")) %>%
#   ungroup() %>% 
#   mutate(phase = case_when(date < voc_switch_inuse$date[1] ~ "Wildtype",
#                            date >= voc_switch_inuse$date[1] & date < voc_switch_inuse$date[2] ~ voc_switch_inuse$voc_name_short[1],
#                            date >= voc_switch_inuse$date[2] & date < voc_switch_inuse$date[3] ~ voc_switch_inuse$voc_name_short[2],
#                            date >= voc_switch_inuse$date[3] ~ voc_switch_inuse$voc_name_short[3]),
#          phase = factor(phase,
#                         levels = c("Wildtype",
#                                    "Alpha",
#                                    "Delta",
#                                    "Omicron"))) 

colors_cat <- c('#e78ac3','#66c2a5','#8da0cb','#a6d854')

joined <- list(joined_con) %>% setNames(c("con"))
joined_list <- lapply(c(1:5), function(i) generate_con(i))

voc_switch_list %>% 
  map(select, date) %>% 
  map(add_row, date = c(ymd("2020-01-01"),
                        ymd("2022-12-31"))) %>% 
  map(arrange, date) %>% 
  map(mutate, date_lag = shift(date, 1)) %>% 
  map(mutate, diff = date - date_lag) -> phase_lengths
