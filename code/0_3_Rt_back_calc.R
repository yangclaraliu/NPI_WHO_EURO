options(mc.cores = 4)

case_data <- covidregionaldata::get_national_data(country_index$CountryName, source = "who") %>% 
  select(date, country, cases_new) 

case_data %>% 
  ggplot(., aes(x = date, y = cases_new)) +
  geom_line() +
  facet_wrap(~country, scales = "free")

# Literature distributions - please reach out if there are others you think should be supported
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")
reporting_delay_2 <- read_rds(paste0(path_data, "public_onset_to_report_delay.rds"))
reporting_delay_1 <- read_rds(paste0(path_data, "onset_to_report.rds"))
reporting_delay_3 <- list(mean = convert_to_logmean(3, 1), mean_sd = 0.1,
                        sd = convert_to_logsd(3, 1), sd_sd = 0.1, max = 15)
# for(i in 1:nrow(country_index)){
#   case_data %>% 
#     filter(country == country_index$CountryName[i]) %>% 
#     ggplot(., aes(x = date, y = cases_new)) +
#     geom_line() +
#     labs(title = country_index$CountryName[i]) -> p
#   
#   ggsave(paste0("figs/case_data/", country_index$CountryName[i], ".png"),
#          plot = p)
# }

# Function to estimate Rt from back calculation 
# Rt_back_cty <- function(i){
# 
# reported_cases <- case_data %>% 
#   filter(country == country_index$CountryName[i]) %>% 
#   select(date, confirm = cases_new) %>% 
#   arrange(date)
# 
# backcalc_cty <- estimate_infections(reported_cases,
#                     generation_time = generation_time,
#                     delays = delay_opts(incubation_period, reporting_delay_1),
#                     rt = NULL,
#                     backcalc = backcalc_opts(),
#                     obs = obs_opts(scale = list(mean = 0.1, sd = 0.025), return_likelihood = TRUE),
#                     horizon = 0)
# 
# backcalc_cty$summarised %>% 
#   filter(variable == "R") %>% 
#   mutate(country = country_index$CountryName[i]) %>% 
#   write_csv(paste0(path_data, "Rt_back_20230403/", country_index$CountryName[i], ".csv"))
# 
# }

paste0(path_data, "Rt_back_20230403/",list.files(paste0(path_data, "Rt_back_20230403/"))) %>% 
  map(read_csv) %>% 
  map(rownames_to_column) %>% 
  bind_rows() %>% 
  filter(!country %in% c("Belarus", "Tajikistan"))-> tmp

write_rds(tmp,
          paste0(path_data,"rt_20230404.rds"))



### Run calaculation - takes ~ 8 hours (10 mins per country)

# Rt_back_cty(1) # test code
# for(i in 9:nrow(country_index)){
#   Rt_back_cty(i)
# }

# map(2:nrow(country_index), Rt_back_cty)


### Function to plot all Rt comparison between analysis 

# plot_rt_est <- function(i){
#   
#   # New Rt data
#   new <- read_csv(paste0("data/Rt_back/", WHO_cty[i], ".csv")) %>% 
#     select(date, median, lower_90, upper_90) %>% 
#     mutate(WHO_cty = WHO_cty[i]) %>% 
#     mutate(Rt_method = "Back calculation")
#   
#   #Rt data from pervious analysis
#   old <- read_csv("data/rt_estimates_2020-07-05.csv") %>% 
#     filter(country == WHO_cty[i]) %>% 
#     select(date, median, lower_90, upper_90) %>% 
#     mutate(WHO_cty = WHO_cty[i]) %>% 
#     mutate(Rt_method = "Previous published")
#   
#   # Plot comparison
#   bind_rows(new, old) %>% 
#     ggplot()+
#     geom_line(aes(x= date, y= median, color = Rt_method))+
#     geom_ribbon(aes(x= date, ymin = lower_90, ymax = upper_90, fill = Rt_method), alpha = 0.4)+
#     scale_x_date(date_breaks = "2 months", date_labels = "%b %y")+
#     scale_color_brewer(palette = "Set2")+
#     scale_fill_brewer(palette = "Set2")+
#     theme_minimal()+
#     labs(title = WHO_cty[i], x = "Date", y = "Rt")+
#     theme(legend.position = "bottom")
#   
#   ggsave(paste0("data/Rt_back_plot/", WHO_cty[i], ".png"),
#          width = 210,
#          height = 180,
#          dpi = 320,
#          units = "mm")
#   
# }

# Plot example
#plot_rt_est(1)

# Plot and save all
# map(2:length(WHO_cty), plot_rt_est)

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

# write_csv(rt_euro, "data/rt_EURO.csv")
