
# Stringnency data 

# Get country name data
cty_code <- countrycode::codelist %>% 
  dplyr::select(iso3c, region, country.name.en) %>% 
  filter(!is.na(region),
         !is.na(iso3c)) %>% 
  group_by(region)

# Vaccination data   
vac_ox <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_vaccines_full.csv")

# Original data used 
# 187 days in 178 countries
oxford_data <- here("data","oxford_data_2020-07-05.csv") %>% 
  read.csv(as.is = TRUE)

# Updated stringency data - filter to Europe region
upd_oxford <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv") %>% 
  #filter(Date >= 20200706) %>% 
  left_join(., cty_code, by = c("CountryName" = "country.name.en")) %>% 
  filter(region == "Europe & Central Asia")

# Missingness in data 
oxford_data_missing_new <- upd_oxford %>% 
  filter_at(vars(contains("_") & !contains("Flag")), 
            any_vars(!is.na(.))) %>%
  mutate(marker = 1) %>%
  pivot_wider(names_from = Date,
              values_from = marker) %>% 
  pivot_longer(cols = starts_with("202"), names_to = "Date") %>%
  group_by(Date) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  mutate(Date = ymd(Date))

# Missing data plot 
oxford_data_missing_new %>%
  # As of October 1st, things look fine until 2021-09-14
  ggplot(aes(x = Date, y= value)) +
  geom_line(stat="identity") +
  labs(x = "Date", y = "Number of countries with data") +
  cowplot::theme_cowplot() +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 26)) +
  geom_vline(xintercept = as.Date("2021-09-14"))
  
# Stringency data all timeline 
stringency <- upd_oxford %>%
  as_tibble %>%
  mutate(Date = lubridate::ymd(Date)) %>%
  rename(cnt = CountryCode, date = Date, stringency = StringencyIndex) %>%
  select(cnt, date, stringency) %>%
  arrange(date) %>% 
  filter(cnt != "GIB") %>% 
  group_by(cnt) %>% 
  mutate(stringency = imputeTS::na_locf(stringency))

# All timeline from start of pandemic 
p_stringency_all <- stringency %>%
  filter(date <= "2021-09-14") %>%
  filter(date >= "2020-01-01") %>% # if focus is  on post July 2020
  mutate(region = countrycode(cnt, "iso3c","region")) %>%
  filter(!is.na(region)) %>% 
  ggplot(., aes(x = date, y = stringency, group = cnt)) + 
  geom_line(aes(color = region), alpha = 0.1) +
  #facet_wrap(~region, ncol = 4) +
  theme_cowplot() +
  geom_smooth(aes(group = region, color = region, fill = region))+
  ggsci::scale_color_lancet()+
  ggsci::scale_fill_lancet()+
  labs(x = "Date",
       y = "Stringency Index",
       color = "Region",
       fill = "Region")  +
  theme(axis.text = element_text(size = 20),
        legend.position = "bottom") 

p_stringency_all

# Truncated timeline for
p_stringency <- stringency %>%
  filter(date <= "2021-09-14") %>%
  filter(date >= "2020-08-01") %>% # 
  mutate(region = countrycode(cnt, "iso3c","region")) %>%
  filter(!is.na(region)) %>% 
  ggplot(., aes(x = date, y = stringency, group = cnt)) + 
  geom_line(aes(color = region), alpha = 0.1) +
  #facet_wrap(~region, ncol = 4) +
  theme_cowplot() +
  geom_smooth(aes(group = region, color = region, fill = region))+
  ggsci::scale_color_lancet()+
  ggsci::scale_fill_lancet()+
  labs(x = "Date",
       y = "Stringency Index",
       color = "Region",
       fill = "Region")  +
  theme(axis.text = element_text(size = 20),
        legend.position = "bottom") 

p_stringency


turnpoint_val <- ggplot2::ggplot_build(p_stringency)$data[[2]] %>% 
  group_by(group) %>% 
  mutate(rk = rank(desc(y))) %>%
  filter(rk == 1) %>% 
  pull(x)

cbind(turnpoint_val,
      stringency %>%
        filter(date <= "2021-09-14") %>%
        filter(date >= "2020-08-01") %>% # 
        mutate(region = countrycode(cnt, "iso3c","region")) %>%
        filter(!is.na(region)) %>% pull(region) %>% table %>% names) %>% 
  as_tibble %>% 
  setNames(c("date","region")) %>% 
  mutate(date = as.numeric(date) %>% round) %>% 
  left_join(seq(lubridate::ymd("2020-07-01"),
                lubridate::ymd("2021-08-31"),
                by = "day") %>% 
              enframe(., name = "ID", value = "date") %>% 
              mutate(date_val = as.numeric(date)),
            by = c("date" = "date_val"))

# Peak is Jan 14 2021

## Find out the odd county which is zig zagging - do plot for all countyies indiviually