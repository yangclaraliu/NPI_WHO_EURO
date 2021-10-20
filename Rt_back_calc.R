# Calculate Rt values

pacman::p_load(tidyverse,
               EpiNow2, 
               countrycode, 
               zoo, 
               cowplot, 
               lubridate,
               covidregionaldata)

# WHO Europe countries (52 - https://www.euro.who.int/en/countries) 
## Turkmenistan removed due to no covid data 
WHO_cty <- c("Albania","Andorra","Armenia","Austria","Azerbaijan","Belarus","Belgium","Bosnia & Herzegovina","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Kazakhstan","Kyrgyzstan","Latvia","Lithuania","Luxembourg","Malta","Moldova","Monaco","Montenegro","Netherlands","North Macedonia","Norway","Poland","Portugal","Romania","Russia","San Marino","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Tajikistan","Turkey","Ukraine","United Kingdom","Uzbekistan")

# Countries ISO3C code
cty_code <- countrycode::codelist %>% 
  dplyr::select(iso3c, country.name.en) %>%
  filter(!is.na(iso3c)) %>% 
  filter(country.name.en %in% WHO_cty) 

# Covid reported cases
case_data <- covidregionaldata::get_national_data(WHO_cty, source = "who") %>% 
  select(date, country, cases_new) %>% 
  filter(date < as.Date("2021-10-01"))

# Plot example case data up to Oct 1st 2021
case_data %>% 
  filter(country == WHO_cty[30]) %>% 
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
  filter(country == WHO_cty[i]) %>% 
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
  mutate(cty = WHO_cty[i]) %>% 
  write_csv(paste0("data/Rt_back/", WHO_cty[i], ".csv"))

}



### Run calaculation - takes ~ 8 hours (10 mins per country)

# Rt_back_cty(1) # test code

map(1:length(WHO_cty), Rt_back_cty)


### Function to plot all Rt comparison between analysis 

plot_rt_est <- function(i){
  
  # New Rt data
  new <- read_csv(paste0("data/Rt_back/", WHO_cty[i], ".csv")) %>% 
    select(date, median, lower_90, upper_90) %>% 
    mutate(WHO_cty = WHO_cty[i]) %>% 
    mutate(Rt_method = "Back calculation")
  
  #Rt data from pervious analysis
  old <- read_csv("data/rt_estimates_2020-07-05.csv") %>% 
    filter(country == WHO_cty[i]) %>% 
    select(date, median, lower_90, upper_90) %>% 
    mutate(WHO_cty = WHO_cty[i]) %>% 
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
    labs(title = WHO_cty[i], x = "Date", y = "Rt")+
    theme(legend.position = "bottom")
  
  ggsave(paste0("data/Rt_back_plot/", WHO_cty[i], ".png"),
         width = 210,
         height = 180,
         dpi = 320,
         units = "mm")
  
}

# Plot example
#plot_rt_est(1)

# Plot and save all
map(1:length(WHO_cty), plot_rt_est)



# Create EURO dataframe to use in analysis 

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

write_csv(rt_euro, "data/rt_EURO.csv")
