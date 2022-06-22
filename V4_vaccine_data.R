## Vaccination data 

# install.packages("MMWRweek")
# library("MMWRweek")

# WHO Europe country names
WHO_cty <- c("Albania","Andorra","Armenia","Austria","Azerbaijan",
             "Belarus","Belgium","Bosnia & Herzegovina","Bulgaria","Croatia",
             "Cyprus","Czechia","Denmark","Estonia","Finland","France",
             "Georgia","Germany","Greece","Hungary","Iceland","Ireland",
             "Israel","Italy","Kazakhstan","Kyrgyzstan","Latvia","Lithuania",
             "Luxembourg","Malta","Moldova","Monaco","Montenegro",
             "Netherlands","North Macedonia","Norway","Poland","Portugal",
             "Romania","Russia","San Marino","Serbia","Slovakia","Slovenia",
             "Spain","Sweden","Switzerland","Tajikistan","Turkey","Ukraine",
             "United Kingdom","Uzbekistan")

# Read in Our world in numbers raw vaccination data (Only total population)
# 52 countries 
# win_data <- read_csv("data/owid-covid-data.csv")
win_data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

V1_data <- win_data %>% 
  select(1,3,4, 42) %>% 
  mutate(location = if_else(location == "Bosnia and Herzegovina", "Bosnia & Herzegovina", location)) %>% # Change naming of Bosnia to WHO format
  filter(location %in% WHO_cty) %>% 
  # filter(date >= as.Date("2020-01-01") & date <= as.Date("2021-09-30")) %>% 
  rename(V1 = people_vaccinated_per_hundred)

# Vector of iso codes for 52 WHO Europe countries
iso <- V1_data %>% 
  select(iso_code) %>% 
  distinct() %>% 
  pull()

# Function to interpolate NAs from after date of first vaccination
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
  mutate(V1 = if_else(date < tmp_date, 0, V1)) %>% 
  mutate(V1 = imputeTS::na_interpolation(V1))

rm(tmp, tmp_date)

return(vacc)

}
 # Bind all countrues together
V1_cleaned <- map(1:length(iso), clean_V1) %>% 
  bind_rows()

# Save cleaned as Our World in Numbers vaccine data (2020-01-01 to 2021-09-30)
V1_cleaned %>% 
  group_by(location) %>% 
  mutate(max_cnt = max(V1)) %>% 
  mutate(V1_adj = V1/max_cnt) %>%
  select(cnt = iso_code, country = location, date, V_all = V1, V_all_adj = V1_adj) %>% 
  group_by(country, cnt) %>% 
  arrange(date) %>% 
  # complete(., date = seq.Date(as.Date("2020-01-01"), as.Date("2021-09-30"), by = "day")) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  ungroup() %>%
  mutate(V_all = V_all/100) -> V1_cleaned2

V1_cleaned2 %>% 
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2021-09-30")) %>% 
  ggplot(., aes(x = date, y = V_all_adj)) +
  geom_point() +
  facet_wrap(~country )

V1_cleaned2  %>% 
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2021-09-30")) %>% 
  write_csv(., "data/VAC_OWIN_v2.csv")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

# Read in WHO raw vaccination data (Age groups included)
# Only 27 countries
cov_raw <- read_csv("data/WHO_VAC.csv") %>% 
  mutate(country = countrycode(ReportingCountry,
                               origin = "iso2c",
                               destination = "country.name")) %>% 
  mutate(cnt = countrycode(country,
                           origin = "country.name",
                           destination = "iso3c")) %>% 
  mutate(region_yes = if_else(ReportingCountry == Region, 1, 0)) %>% 
  filter(region_yes == 1) %>% 
  select(week = 1, 4, 7, 11, 13, 14, 15) %>% 
  filter(TargetGroup == "18-60yr"| TargetGroup == "60+yr"| TargetGroup == "total") %>% 
  mutate(year = as.double(substr(week, start = 1, stop = 4))) %>% 
  mutate(week_no = as.double(substr(week, start = 7, stop = 8))) %>% 
  mutate(date = as.Date(MMWRweek2Date(MMWRyear = year, MMWRweek = week_no, MMWRday = 1))) %>% 
  group_by(country, week, TargetGroup) %>% 
  arrange(date) %>% 
  mutate(sum = sum(DoseFirst)) %>% 
  filter(row_number()==1) %>% 
  group_by(country, TargetGroup) %>% 
  mutate(cum_sum = cumsum(sum)) %>% 
  mutate(cov_raw = cum_sum/Denominator) %>% 
  ungroup() %>% 
  select(date, week, country, cnt,  TargetGroup, cov_raw) %>% 
  mutate(origin = "WHO") %>% 
  drop_na(country)

# Function to complete and interpolate vaccine TS
# Remove Germany Liechtenstein & Netherlands as only have total population with no age stratification
tmp_cnt <- cov_raw %>% 
  distinct(country) %>% 
  filter(country != "Germany" & country != "Liechtenstein" & country != "Netherlands") %>% 
  pull()

complete_vac <- function(i){
  
  tmp_1 <- cov_raw %>% 
    select(date, country, cnt, TargetGroup, cov_raw, origin) %>% 
    filter(TargetGroup == "18-60yr") %>% 
    mutate(max = max(cov_raw)) %>% 
    mutate(V_18_60_adj = cov_raw/max) %>% 
    rename(V_18_60 = cov_raw) %>% 
    select(1, 2, 3, 5, 8)
  
  tmp_2 <- cov_raw %>% 
    select(date, country, cnt, TargetGroup, cov_raw, origin) %>% 
    filter(TargetGroup == "60+yr") %>% 
    mutate(max = max(cov_raw)) %>% 
    mutate(V_60_adj = cov_raw/max) %>% 
    rename(V_60 = cov_raw) %>% 
    select(1, 2, 3, 5, 8)
  
  tmp_3 <- cov_raw %>% 
    select(date, country, cnt, TargetGroup, cov_raw, origin) %>% 
    filter(TargetGroup == "total") %>% 
    mutate(max = max(cov_raw)) %>% 
    mutate(V_tot_adj = cov_raw/max) %>% 
    rename(V_tot = cov_raw) %>% 
    select(1, 2, 3, 5, 8)
  
  tmp_ts <- full_join(tmp_1, tmp_2, by = c("country", "cnt", "date")) %>% 
    full_join(., tmp_3, by = c("country", "cnt", "date")) 
  
  tmp_ts %>% 
    filter(country == tmp_cnt[i]) %>% 
    complete(., date = seq.Date(min(date), as.Date("2021-09-30"), by = "day") ) %>% 
    
    mutate_at( vars(contains("V_")),imputeTS::na_interpolation) %>% 
    complete(., date = seq.Date(as.Date("2020-01-01"), as.Date("2021-09-30"), by = "day") )  %>% 
    mutate(country = tmp_cnt[i]) %>% 
    mutate(cnt = countrycode(country,
                             origin = "country.name", 
                             destination = "iso3c")) %>% 
    filter(date < as.Date("2021-10-01")) %>% 
    mutate_if(is.numeric, ~replace_na(., 0)) 
}

# Bind all countries together
vac <- map(1:length(tmp_cnt), complete_vac) %>% 
  bind_rows() 

# Save as .csv
write_csv(vac, "data/VAC_WHO.csv")


#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

# Compare datasets

# Plot Age group vaccine coverage (proportion of population vaccinated)
vac %>% 
  pivot_longer(cols = c(V_18_60, V_60, V_tot), names_to = "Age group", values_to = "Coverage") %>% 
  ggplot(aes(x = date))+
  geom_line(aes(y = Coverage, color = `Age group`))+
  facet_wrap(~country)+
  theme_bw()+
  scale_color_discrete(name = "Age group", labels = c("18 to 60", "60+", "Total population"))+
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")+
  labs(title = "WHO Vaccine Data", y = "Proportion of population vaccinated", x = "")+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8))

ggsave(filename = "figs/EURO_2/fig_s_WHO_vac.png",
       plot = last_plot(),
       width = 15,
       height = 10)

# Compare proprotion vaccinated between OWIN data and WHO vaccine data 
# Read in OWIN data 
OWIN <- read_csv("data/VAC_OWIN.csv") %>% 
  mutate(origin = "Our world in numbers") %>% 
  mutate(TargetGroup = "Total_OWIN")

# Plot comparison of total population coverage 
# Very similar results although OWIN n = 52, WHO n = 26
vac %>%
  mutate(origin = "WHO vaccine data") %>% 
  select(date, country, V_tot, origin) %>% 
  bind_rows(OWIN %>% 
              select(date, country, V_tot = V_all, origin)) %>% 
  ggplot(aes(x = date, y= V_tot))+
  geom_line(aes(color = origin))+
  facet_wrap(~country)+
  theme_bw()+
  scale_color_discrete(name = "Data source", labels = c("Our World in Numbers", "WHO"))+
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")+
  labs(title = "Total Population Vaccine Coverage Comparison", y = "Proportion of population vaccinated", x = "")+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8))

# Save
ggsave(filename = "figs/EURO_2/fig_s_cov_compare.png",
       plot = last_plot(),
       width = 15,
       height = 10)


### Function to find number of countries with data for each age grouping catagory
t_group <- who_vac_data %>% 
  select(TargetGroup) %>% 
  distinct() %>% 
  pull()

cnt_group <- function(i){
  who_vac_data %>% 
    mutate(country = countrycode(ReportingCountry,
                                 origin = "iso2c",
                                 destination = "country.name"))  %>% 
    filter(TargetGroup == t_group[i]) %>% 
    distinct(country) %>% 
    nrow() %>% 
    enframe() %>% 
    mutate(age_group = t_group[i])
}

map(1:length(t_group), cnt_group) %>% 
  bind_rows()
