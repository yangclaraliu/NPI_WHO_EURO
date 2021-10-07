# packages
# install.packages(c("data.table", "remotes", "EpiNow2"))		
# remotes::install_github("epiforecasts/covidregionaldata")
library(data.table)
library(EpiNow2)
library(covidregionaldata)

# target country (must be present in ECDC data)
country <- "france"

# set number of cores to use fitting the model
# no benefit on runtime if cores > chains which is set to 4 by default
options(mc.cores = 4)

# literature distributions - please reach out if there are others you think should be supported
generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

# define reporting delay as lognormal with mean of 4 days and sd of 1 day in absence of
# evidence. If data on onset -> report then can use estimate_delay to estimate the delay
reporting_delay <- list(mean = convert_to_logmean(4, 1),
                        mean_sd = 0.1,
                        sd = convert_to_logsd(4, 1),
                        sd_sd = 0.1,
                        max = 15)

# precleaned ECDC data - alternative is to bring your own 
reported_cases <- covidregionaldata::get_national_data(country, source = "ecdc")
reported_cases <- data.table::setDT(reported_cases)
reported_cases <- reported_cases[, .(date, confirm = cases_new)]
# filter to the last 3 months of data (to speed up computation)
#reported_cases <- reported_cases[date >= (max(date) - 12*7)]

# estimate Rt and nowcast/forecast cases by date of infection
# on a 4 core computer this example should take between 2 ~ 5 minutes to run
# to see saved logs view the dated logs folder
# to see saved results see the dated results folder
# some data sets may produce divergent transition warnings
# this is not unexpected and is usually fine (if dts < 1% of samples)
# but is an area of research as we seek to optimise the underlying model.
# If you have some prior knowledge of the scaling between observations and 
# reports see ?obs_opts for options.
# If you have some prior knowledge on the truncation in your data or multiple
# snapshots of data see ?trunc_opts for options.
# Note that the default settings may not be appropriate for your use case.
# Example configurations are here: https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.html
out <- epinow(reported_cases = reported_cases, 
              generation_time = generation_time,
              delays = delay_opts(incubation_period, reporting_delay),
              rt = rt_opts(prior = list(mean = 1.5, sd = 0.5)),
              # here we define the quality of the gaussian process approximation
              # if the fit to data appears poor try increasing basis_prop and
              # potentially the boundary_scale (see ?gp_opts for details)
              # though this will likely increase runtimes.
              gp = gp_opts(basis_prop = 0.2),
              # in some instances stan chains can get stuck when estimating in 
              # these instances consider using the future fitting mode by passing 
              # `future = TRUE, max_execution_time = 60 * 30` to stan_opts and calling 
              # `future::plan("multiprocess")` prior to running epinow this will time out
              # chains after 30 minutes but still return results from completed chains
              stan = stan_opts(),
              horizon = 0, 
              target_folder = "results",
              logs = file.path("logs", Sys.Date()),
              return_output = TRUE, 
              verbose = TRUE)

# summary of the latest estimates
summary(out)
# plot estimates
plot(out)
# summary of R estimates
summary(out, type = "parameters", params = "R")

out$estimates$summarised

france_ex


france_est_inf <- epinow(reported_cases = reported_cases, 
                         generation_time = generation_time,
                         delays = delay_opts(incubation_period, reporting_delay),
                         rt = rt_opts(prior = list(mean = 1.5, sd = 0.5)),
                         # here we define the quality of the gaussian process approximation
                         # if the fit to data appears poor try increasing basis_prop and
                         # potentially the boundary_scale (see ?gp_opts for details)
                         # though this will likely increase runtimes.
                         gp = gp_opts(basis_prop = 0.2),
                         # in some instances stan chains can get stuck when estimating in 
                         # these instances consider using the future fitting mode by passing 
                         # `future = TRUE, max_execution_time = 60 * 30` to stan_opts and calling 
                         # `future::plan("multiprocess")` prior to running epinow this will time out
                         # chains after 30 minutes but still return results from completed chains
                         stan = stan_opts(),
                         horizon = 14, 
                         target_folder = "results",
                         logs = file.path("logs", Sys.Date()),
                         return_output = TRUE, 
                         verbose = TRUE)


##

backcalc <- estimate_infections(reported_cases,
                                generation_time = generation_time,
                                delays = delay_opts(incubation_period, reporting_delay),
                                rt = NULL, backcalc = backcalc_opts(),
                                obs = obs_opts(scale = list(mean = 0.4, sd = 0.05)),
                                horizon = 0
)

plot(backcalc)

backcalc$summarised %>% 
  filter(variable == "R") %>% 
  left_join(., france_ex %>% 
              select(date, rt_old = median), by = c("date" = "date")) %>% 
  ggplot()+
  geom_line(aes(x = date, y = median))+
  geom_line(aes(x = date, y = rt_old), color = "red")

france_ex %>% 
  select(date, rt_old = median)

backcalc_p <- estimate_infections(reported_cases,
                                generation_time = generation_time,
                                delays = delay_opts(incubation_period, reporting_delay),
                                rt = NULL, backcalc = backcalc_opts(prior = "none"),
                                obs = obs_opts(scale = list(mean = 0.4, sd = 0.05)),
                                horizon = 0)

backcalc_p$summarised %>% 
  filter(variable == "R") %>% 
  left_join(., france_ex %>% 
              select(date, rt_old = median), by = c("date" = "date")) %>% 
  ggplot()+
  geom_line(aes(x = date, y = median))+
  geom_line(aes(x = date, y = rt_old), color = "red")


covidregionaldata::get_national_data(country, source = "ecdc")

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

rt_estimates <- here("data","rt_estimates_2020-07-05.csv") %>% read.csv 

rt_estimates %>% 
  mutate(region = countrycode(country, "iso3c","region")) 
