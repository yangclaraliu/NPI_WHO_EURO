## Compare WHO vaccination data with Our world in numbers 

# install.packages("MMWRweek")
# library("MMWRweek")

# Read in Our world in numbers vaccination data (Only total)
# 52 countries 
OWIN <- read_csv("data/VAC_EURO.csv") %>% 
  rename(country = location, cnt = iso_code) %>% 
  mutate(cov_raw = V1/100) %>% 
  mutate(origin = "Our world in numbers") %>% 
  mutate(TargetGroup = "Total_OWIN") %>% 
  select(date, country, TargetGroup, cov_raw, origin)


# Read in WHO raw vaccination data (Age groups included)
# Only 27 countries
who_vac_data <- read_csv("data/WHO_VAC.csv")

cov_raw <- who_vac_data %>% 
  mutate(country = countrycode(ReportingCountry,
                               origin = "iso2c",
                               destination = "country.name")) %>% 
  mutate(cnt = countrycode(country,
                           origin = "country.name",
                           destination = "iso3c")) %>% 
  mutate(region_yes = if_else(ReportingCountry == Region, 1, 0)) %>% 
  filter(region_yes == 1) %>% 
  select(week = 1, 4, 7, 11, 13, 14, 15) %>% 
  group_by(country, week, TargetGroup) %>% 
  mutate(sum = sum(DoseFirst)) %>% 
  filter(row_number()==1) %>% 
  group_by(country, TargetGroup) %>% 
  mutate(cum_sum = cumsum(sum)) %>% 
  mutate(cov_raw = cum_sum/Denominator) %>% 
  mutate(year = as.double(substr(week, start = 1, stop = 4))) %>% 
  mutate(week_no = as.double(substr(week, start = 7, stop = 8))) %>% 
  mutate(date = as.Date(MMWRweek2Date(MMWRyear = year, MMWRweek = week_no, MMWRday = 1))) %>% 
  ungroup() %>% 
  filter(TargetGroup == "18-60yr"| TargetGroup == "60+yr"| TargetGroup == "total") %>% 
  select(date, week, country, cnt,  TargetGroup, cov_raw) %>% 
  mutate(origin = "WHO")


WHO_age <- cov_raw %>% 
  select(date, country, TargetGroup, cov_raw, origin)

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
  filter(country == "Denmark") %>% 
  complete(., date = seq.Date(min(date), as.Date("2021-09-30"), by = "day") ) %>% 

  mutate_at( vars(contains("V_")),imputeTS::na_interpolation) %>% 
  complete(., date = seq.Date(as.Date("2020-01-01"), as.Date("2021-09-30"), by = "day") )  %>% 
  mutate(country = "Denmark") %>% 
  mutate(cnt = countrycode(country,
                           origin = "country.name", 
                           destination = "iso3c")) %>% 
  filter(date < as.Date("2021-10-01")) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) 



  
  
# Plot comparison 
bind_rows(OWIN,WHO_age) %>% 
  ggplot(aes(x= date, y = cov_raw))+
  geom_line(aes(color = TargetGroup))+
  facet_wrap(~country)


### Function to find number of countries with data for each age grouping 
t_group <- who_vac_data %>% 
  select(TargetGroup) %>% 
  distinct() %>% 
  pull()

cnt_group <- function(i){
  who_vac_data %>% 
    mutate(country = countrycode(ReportingCountry,
                                 origin = "iso2c",
                                 destination = "country.name")) %>% 
    mutate(cnt = countrycode(country,
                             origin = "country.name",
                             destination = "iso3c")) %>% 
    mutate(region_yes = if_else(ReportingCountry == Region, 1, 0)) %>% 
    filter(region_yes == 1) %>% 
    select(week = 1, 4, 7, 11, 13, 14, 15) %>% 
    group_by(country, week, TargetGroup) %>% 
    mutate(sum = sum(DoseFirst)) %>% 
    filter(row_number()==1) %>% 
    group_by(country, TargetGroup) %>% 
    mutate(cum_sum = cumsum(sum)) %>% 
    mutate(cov_raw = cum_sum/Denominator) %>% 
    mutate(year = as.double(substr(week, start = 1, stop = 4))) %>% 
    mutate(week_no = as.double(substr(week, start = 7, stop = 8))) %>% 
    mutate(date = as.Date(MMWRweek2Date(MMWRyear = year, MMWRweek = week_no, MMWRday = 1))) %>% 
    ungroup() %>% 
    filter(TargetGroup == t_group[i]) %>% 
    distinct(country) %>% 
    nrow() %>% 
    enframe() %>% 
    mutate(age_group = t_group[i])
}

map(1:length(t_group), cnt_group) %>% 
  bind_rows()


