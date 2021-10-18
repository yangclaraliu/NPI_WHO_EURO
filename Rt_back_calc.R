# Calculate Rt values

pacman::p_load(tidyverse,
               EpiNow2, 
               countrycode, 
               zoo, 
               cowplot, 
               lubridate,
               covidregionaldata)

# WHO Europe and Cenral Asia contries
# Add Israel and Malta
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

# Plot example case data up to Oct 1st 2021
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


# Estimate Rt backcalc (EpiNow2 package)

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

}



### Run calaculation - takes ~ 8 hours (10 mins per country)

# Rt_back_cty(1) # test code

map(1:length(cty), Rt_back_cty)
# Turkmenistan failed 

# Do remaining counties 
map(49:51, Rt_back_cty)

# Create dataframe to use in analysis 

format_rt <- function(i){

read_csv(paste0("data/Rt_back/", list.files("data/Rt_back/"))[i]) %>% 
  group_by(cty) %>% 
  mutate(X1 = row_number()) %>% 
  select(X1, country = cty, date, median, lower_90, upper_90, lower_50, upper_50)
  }

rt_euro <- map(1:length(list.files("data/Rt_back/")), format_rt) %>% 
  bind_rows() %>% 
  filter(date < as.Date("2021-10-01")) %>% 
  ungroup()

write_csv(rt_euro, "data/rt_estimates_2021-09-30.csv")



### Function to plot all Rt comparison between analysis 

plot_rt_est <- function(i){
  
  # New Rt data
  new <- read_csv(paste0("data/Rt_back/", cty[i], ".csv")) %>% 
    select(date, median, lower_90, upper_90) %>% 
    mutate(cty = cty[i]) %>% 
    mutate(Rt_method = "Back calculation")
  
  #Rt data from pervious analysis
  old <- read_csv("data/rt_estimates_2020-07-05.csv") %>% 
    filter(country == cty[i]) %>% 
    select(date, median, lower_90, upper_90) %>% 
    mutate(cty = cty[i]) %>% 
    mutate(Rt_method = "Previous published")
  
  # Plot comparison
  bind_rows(new, old) %>% 
    ggplot()+
    geom_line(aes(x= date, y= median, color = Rt_method))+
    geom_ribbon(aes(x= date, ymin = lower_90, ymax = upper_90, fill = Rt_method), alpha = 0.4)+
    scale_x_date(date_breaks = "2 months", date_labels = "%b %y")+
    scale_color_brewer(palette = "Set2")+
    scale_fill_brewer(palette = "Set2")+
    theme_minimal()+
    labs(title = cty[i], x = "Date", y = "Rt")+
    theme(legend.position = "bottom")
  
  ggsave(paste0("data/Rt_back_plot/", cty[i], ".png"),
         width = 210,
         height = 180,
         dpi = 320,
         units = "mm")
  
}

# Plot example
plot_rt_est(1)

# Turkmenistan problem - need to mix this (Malta and Israel too)
map(c(1:47, 49:51), plot_rt_est)

