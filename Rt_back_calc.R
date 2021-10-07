# Calculate Rt values

pacman::p_load(tidyverse,
               EpiNow2, 
               countrycode, 
               zoo, 
               cowplot, 
               lubridate,
               covidregionaldata)

# WHO Europe and Cenral Asia contries
cty_code <- countrycode::codelist %>% 
  dplyr::select(iso3c, region, country.name.en) %>% 
  filter(region == "Europe & Central Asia") %>% 
  filter(!is.na(region),
         !is.na(iso3c)) %>% 
  filter(country.name.en != "Åland Islands",
         country.name.en != "Faroe Islands",
         country.name.en != "Gibraltar",
         country.name.en != "Greenland",
         country.name.en != "Guernsey",
         country.name.en != "Isle of Man",
         country.name.en != "Jersey",
         country.name.en != "Liechtenstein",
         country.name.en != "Vatican City")

cty <- cty_code %>% 
  pull(country.name.en) 

case_data <- covidregionaldata::get_national_data(cty, source = "who") %>% 
  select(date, country, cases_new) %>% 
  filter(date < as.Date("2021-10-01"))

case_data %>% 
  filter(country == cty[50]) %>% 
  ggplot() +
  aes(x = date, y = cases_new, col = country) +
  geom_line(alpha = 0.9) +
  labs(x = "Date", y = "Reported Covid-19 cases") +
  #scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(col = guide_legend(title = "Country"))


# Estimate Rt backcalc

options(mc.cores = 4)

# Literature distributions - please reach out if there are others you think should be supported
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

# define reporting delay as lognormal with mean of 4 days and sd of 1 day in absence of
# evidence. If data on onset -> report then can use estimate_delay to estimate the delay
reporting_delay <- list(mean = convert_to_logmean(4, 1),
                        mean_sd = 0.1,
                        sd = convert_to_logsd(4, 1),
                        sd_sd = 0.1,
                        max = 15)

# Function to estimate Rt from back calculation 
Rt_back_cty <- function(i){

reported_cases <- case_data %>% 
  filter(country == cty[i]) %>% 
  select(date, confirm = cases_new)

backcalc_cty <- estimate_infections(reported_cases,
                    generation_time = generation_time,
                    delays = delay_opts(incubation_period, reporting_delay),
                    rt = NULL, 
                    backcalc = backcalc_opts(),
                    obs = obs_opts(scale = list(mean = 0.4, sd = 0.05)),
                    horizon = 0)

backcalc_cty$summarised %>% 
  filter(variable == "R") %>% 
  mutate(cty = cty[i]) %>% 
  write_csv(paste0("data/Rt_back/", cty[i], ".csv"))

#backcalc_cty_np <- estimate_infections(reported_cases,
#                                    generation_time = generation_time,
#                                    delays = delay_opts(incubation_period, reporting_delay),
#                                    rt = NULL, 
#                                    backcalc = backcalc_opts(prior = "none"),
#                                    obs = obs_opts(scale = list(mean = 0.4, sd = 0.05)),
#                                    horizon = 0)
#
#backcalc_cty_np$summarised %>% 
#  filter(variable == "R") %>% 
#  mutate(cty = cty[i]) %>% 
#  write_csv(paste0("data/Rt_back_np/", cty[i], ".csv"))

}

#Rt_back_cty(1)

###

map(1:length(cty), Rt_back_cty)


new <- read_csv(paste0("data/Rt_back/", cty[2], ".csv")) %>% 
  select(date, median, lower_90, upper_90) %>% 
  mutate(cty = cty[2]) %>% 
  mutate(Rt_method = "Back calculation")

#new_np <- read_csv(paste0("data/Rt_back/", cty[1], ".csv")) %>% 
#  select(date, median, lower_90, upper_90) %>% 
#  mutate(cty = cty[1]) %>% 
#  mutate(Rt_method = "Back calculation, no prior")

old <- read_csv("data/rt_estimates_2020-07-05.csv") %>% 
  filter(country == cty[2]) %>% 
  select(date, median, lower_90, upper_90) %>% 
  mutate(cty = cty[2]) %>% 
  mutate(Rt_method = "Previous published")

bind_rows( new, old) %>% 
  ggplot()+
  geom_line(aes(x= date, y= median, color = Rt_method))+
  geom_ribbon(aes(x= date, ymin = lower_90, ymax = upper_90, fill = Rt_method), alpha = 0.4)+
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y")+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+
  labs(title = cty[2], x = "Date", y = "Rt")+
  theme(legend.position = "bottom")



