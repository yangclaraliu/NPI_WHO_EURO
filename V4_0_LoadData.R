rm(list=ls())

# packages and functions
pacman::p_load(tidyverse,
               here, 
               plm, 
               countrycode, 
               magrittr, 
               zoo, 
               ggdendro, 
               cowplot, 
               lubridate,
               data.table,
               pvclust,
               e1071,
               ggdendro,
               progress,
               ggsci,
               gridExtra,
               ggh4x,
               covidregionaldata,
               MMWRweek)

# custom functions
# cluster analysis
#source(here("hclust_plot_max.R"))
#source(here("hclust_plot_mid.R"))
#source(here("hclust_plot_any.R"))
# regression exercise
source(here("aicbic_plm.R"))
#source(here("gen_reg_data.R"))
source(here("select_var.R"))
source(here("find_lag.R"))
#source(here("calc_all_lags.R"))
source(here("plot_all_lags.R"))
source(here("aicbic_select.R"))

# recompile the datafile only if it doesn't already exist
#if(!file.exists(here("data","joined_all.RDS"))){
oxford_data       <- read_csv("data/NPI_OX.csv") 
vaccine_data_owin <- read_csv("data/VAC_OWIN.csv")
vaccine_data_who  <- read_csv("data/VAC_WHO.csv")
rt_estimates      <- read_csv("data/rt_EURO.csv")


# Build stringency_data from oxford_data
stringency  <- oxford_data %>%
  as_tibble %>%
  rename(stringency = StringencyIndex) %>%
  select(cnt, date, stringency) %>%
  pivot_wider(names_from = date, values_from = stringency) %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "date",
               values_to = "stringency") %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  arrange(date) %>% 
  group_by(cnt) %>% 
  mutate(stringency = imputeTS::na_locf(stringency)) %>% 
  mutate(VOC = if_else(date <= as.Date("2020-11-30"), "Original", "Variant")) %>% 
  mutate(VOC = if_else(VOC == "Variant" & date >= as.Date("2020-12-01") & date <= as.Date("2021-04-30"), "Alpha", VOC)) %>% 
  mutate(VOC = if_else(VOC == "Variant" & date >= as.Date("2021-05-01") & date <= as.Date("2021-09-30"), "Delta", VOC)) %>% 
  mutate(VOC = factor(VOC, levels = c("Original", "Alpha", "Delta")))

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
                           paste0("C",1:7) ~ "Closure & Containment  ",
                         policy_code == "C8" ~ "Intl Travel Restriction  ",
                         policy_code %in%
                           paste0("E", 1:4) ~ "Economic Response  ",
                         policy_code %in%
                           paste0("H", 1:6) ~ "Public Health & Health System Response  "),
         lab = gsub("\\.", " ", policy_name))#

policy_dic_V <- policy_dic %>% 
  add_row(policy_code = "V_all_adj", policy_name = "Vaccine coverage - all population (OWIN)", policy_max = 1, cat = "Vaccination ", lab = "Vaccine coverage - all population (OWIN)") %>% 
  add_row(policy_code = "V_18_60_adj", policy_name = "Vaccine coverage - 18 to 60 (WHO)", policy_max = 1, cat = "Vaccination ", lab = "Vaccine coverage - 18 to 60 (WHO)") %>% 
  add_row(policy_code = "V_60_adj", policy_name = "Vaccine coverage - 60+ (WHO)", policy_max = 1, cat = "Vaccination ", lab = "Vaccine coverage - 60+ (WHO)") %>% 
  add_row(policy_code = "V_tot_adj", policy_name = "Vaccine coverage - all population (WHO)", policy_max = 1, cat = "Vaccination ", lab = "Vaccine coverage - all population (WHO)")

# Build policy_data
policy_data <-  oxford_data %>%
  select(country, cnt, date, contains("_")) %>%
  pivot_longer(cols = contains("_"), names_to = "policy_name") %>%
  mutate(policy_name = str_replace(policy_name, "_.*", ""))

# Impute missing data
policy_data_1 <-  policy_data %>% 
  group_by(cnt, policy_name) %>% 
  arrange(date) %>% 
  nest() %>% 
  mutate(missing = map(data, ~any(is.na(.x$value))) %>% unlist,
         all = map(data, ~all(is.na(.x$value))) %>% unlist) %>% 
  group_by(missing) %>% 
  group_split()

policy_data_2 <-  policy_data_1[[2]] %>% 
  mutate(data = map(data, arrange, date),
         # missingness at the tails are replaced by the last or the next 
         # non-missing values; missingness not at the tails are replaced
         # based on linear interpolation.
         data = map(data, mutate, 
                    value = imputeTS::na_interpolation(value))) %>% 
  bind_rows(policy_data_1[[1]]) %>% 
  dplyr::select(-c(missing, all)) %>% 
  unnest(cols = data)

# set maximum and minimum values
policy_data_3 <-  policy_data_2 %>% 
  group_by(policy_name) %>% 
  summarise(policy_value_max = max(value, na.rm = T)) %>% 
  right_join(policy_data_2, by = "policy_name") %>% 
  arrange(cnt, policy_name) %>% 
  mutate(policy_value_min = 0) %>% 
  select(country, cnt, date, policy_name, value, policy_value_min, policy_value_max) %>% 
  mutate(policy_value_hi = if_else(value < policy_value_max, 0, 1)) %>% 
  mutate(policy_value_lo = if_else(value > 0, 1, 0)) %>% 
  mutate(country = if_else(country == "Bosnia and Herzegovina", "Bosnia & Herzegovina", country))

# Build other data sets
rt_estimates <-  rt_estimates %>% 
  mutate(cnt = countrycode(country, "country.name", "iso3c"),
         date = lubridate::ymd(date)) %>%
  as_tibble()

vaccine_data_join <- full_join(vaccine_data_owin %>% 
                                 select(-V_all),
                               vaccine_data_who %>% 
                                 select(-c(V_18_60, V_60, V_tot)),
                               by = c("country" = "country", "cnt" = "cnt", "date" = "date")) 


# Total country in WHO Europe region, n = 53
# Check to see if these can be resolved later 

unique(policy_data_3$country) # n = 49,  rm(Turkmenistan, Armenia, Montenegro & North Macedonia)
unique(rt_estimates$country) # n = 52, rm(Turkmenistan)
unique(vaccine_data_owin$country)# n = 52, rm(Turkmenistan)
unique(vaccine_data_who$country) # n = 26, rm()


# Pivot to wide datasets and set factors

# Maximum effort 
joined_hi <- policy_data_3 %>% 
  select(-c(value, policy_value_min, policy_value_max, policy_value_lo)) %>% 
  pivot_wider(names_from = policy_name, values_from = policy_value_hi) %>% 
  group_by(cnt, date) %>% 
  left_join(vaccine_data_join,
            by = c("country", "cnt", "date")) %>% 
  left_join(rt_estimates %>% 
              dplyr::select(-X1),
            by = c("country", "cnt", "date")) %>% 
  ungroup()


# Standardized between 0-1 effort
joined_mid <- policy_data_3 %>% 
  dplyr::select(-c(policy_value_hi, 
                   policy_value_min,
                   policy_value_lo)) %>% 
  mutate(value = value/policy_value_max) %>%
  select(-policy_value_max) %>% 
  pivot_wider(names_from = policy_name, values_from = value) %>% 
  group_by(cnt, date) %>% 
  left_join(vaccine_data_join,
            by = c("country", "cnt", "date")) %>% 
  left_join(rt_estimates %>% 
              dplyr::select(-X1),
            by = c("country", "cnt", "date")) %>% 
  ungroup()


# Max effort, Wild virus 
joined_hi_W <- joined_hi %>%
  ungroup() %>% 
  filter(date <= as.Date("2020-11-30"))

# Max effort, Alpha
joined_hi_A <- joined_hi %>% 
  ungroup() %>% 
  filter(date > as.Date("2020-11-30")) %>% 
  filter(date <= as.Date("2021-04-30"))

# Max effort, Delta virus 
joined_hi_D <- joined_hi %>% 
  ungroup() %>% 
  filter(date > as.Date("2021-04-30")) %>% 
  filter(date <= as.Date("2021-09-30"))

# Scaled effort, Wild virus 
joined_mid_W <- joined_mid %>% 
  ungroup() %>% 
  filter(date <= as.Date("2020-11-30"))

# Scaled effort, Alpha virus 
joined_mid_A <- joined_mid %>% 
  ungroup() %>% 
  filter(date > as.Date("2020-11-30")) %>% 
  filter(date <= as.Date("2021-04-30"))

# Scaled effort, Delta virus 
joined_mid_D <- joined_mid %>% 
  ungroup() %>% 
  filter(date > as.Date("2021-04-30")) %>% 
  filter(date <= as.Date("2021-09-30"))


# Save RDS
saveRDS(list(hi_W = joined_hi_W,
             mid_W = joined_mid_W,
             hi_A = joined_hi_A,
             mid_A = joined_mid_A,
             hi_D = joined_hi_D,
             mid_D = joined_mid_D,
             stringency = stringency,
             policy_dic = policy_dic_V), 
        here("data", "joined_all_V4.RDS"))
