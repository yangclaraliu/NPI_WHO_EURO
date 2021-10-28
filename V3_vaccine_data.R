## Vaccination data 

# WHO Europe country names
WHO_cty <- c("Albania","Andorra","Armenia","Austria","Azerbaijan","Belarus","Belgium","Bosnia & Herzegovina","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Kazakhstan","Kyrgyzstan","Latvia","Lithuania","Luxembourg","Malta","Moldova","Monaco","Montenegro","Netherlands","North Macedonia","Norway","Poland","Portugal","Romania","Russia","San Marino","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Tajikistan","Turkey","Ukraine","United Kingdom","Uzbekistan")

# Read in raw vaccination data (Our World in Numbers)
win_data <- read_csv("data/owid-covid-data.csv")

V1_data <- win_data %>% 
  select(1,3,4, 42) %>% 
  mutate(location = if_else(location == "Bosnia and Herzegovina", "Bosnia & Herzegovina", location)) %>% # Change naming of Bosnia to WHO format
  filter(location %in% WHO_cty) %>% 
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2021-09-30")) %>% 
  rename(V1 = people_vaccinated_per_hundred)

# Vector of iso codes for 52 WHO Europe countries
iso <- V1_data %>% 
  select(iso_code) %>% 
  distinct() %>% 
  pull()

# Function to interpolate NAs from after data of first vaccination
clean_V1 <- function(i){

tmp <- which(!is.na(V1_data %>% 
                    filter(iso_code == iso[i]) %>% 
                    select(V1) %>% 
                    pull())) %>% 
      first()

tmp_date <- V1_data %>% 
  filter(iso_code == iso[i]) %>% 
  slice(tmp) %>% 
  select(date) %>% 
  pull()
  
vacc <- V1_data %>% 
  filter(iso_code == iso[i]) %>% 
  #mutate(V2 = V1) %>% 
  mutate(V1 = if_else(date < tmp_date, 0, V1)) %>% 
  mutate(V1 = imputeTS::na_interpolation(V1))

rm(tmp, tmp_date)

return(vacc)

}

V1_cleaned <- map(1:length(iso), clean_V1) %>% 
  bind_rows()


write_csv(V1_cleaned, "data/VAC_EURO.csv")
