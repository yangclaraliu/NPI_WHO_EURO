# NPI data

# WHO Europe countries (52 - https://www.euro.who.int/en/countries) 
## Turkmenistan removed due to no covid data 
WHO_cty <- c("Albania","Andorra","Armenia","Austria","Azerbaijan","Belarus","Belgium","Bosnia & Herzegovina","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Kazakhstan","Kyrgyzstan","Latvia","Lithuania","Luxembourg","Malta","Moldova","Monaco","Montenegro","Netherlands","North Macedonia","Norway","Poland","Portugal","Romania","Russia","San Marino","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Tajikistan","Turkey","Ukraine","United Kingdom","Uzbekistan")

# Countries ISO3C code
cty_code <- countrycode::codelist %>% 
  dplyr::select(iso3c, country.name.en) %>%
  filter(!is.na(iso3c))

# Data used in previous analysis (OxCGRT)
oxford_data_original <- read.csv("data/oxford_data_2020-07-05.csv", as.is = TRUE) %>% 
  as_tibble()

# Read in updated Oxford NPI data (OxCGRT)
oxford_all <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv", as.is = TRUE) %>% 
  filter(Date < 20211001) 
  #mutate(Date = ymd(Date)) %>% 
  #filter(Date < as.Date("2021-10-01"))

### Filter to WHO EURO countries 
# No data on NPIs for:
# Armenia
# Montenegro
# North Macedonia
oxford_EURO_all <- oxford_all %>% 
  as_tibble() %>% 
  left_join(., cty_code, by = c("CountryCode" = "iso3c")) %>% 
  filter(country.name.en %in% WHO_cty) %>% 
  select(-country.name.en) %>% 
  filter(Jurisdiction == "NAT_TOTAL") %>% 
  select(names(oxford_data_original)[-1]) %>% 
  mutate(X = row_number()) %>% 
  select(X, everything())


write_csv(oxford_EURO_all, "data/NPI_EURO.csv")

