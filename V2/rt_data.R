
# Rt estimates

remove.packages("Rcpp")
install.packages('Rcpp')
library(Rcpp)


# Install STAN
remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")

install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)

library("rstan")

example(stan_model, package = "rstan", run.dontrun = TRUE)




install.packages("EpiNow2")

library("EpiNow2")

generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")
incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

reporting_delay <- list(
  mean = convert_to_logmean(2, 1), mean_sd = 0.1,
  sd = convert_to_logsd(2, 1), sd_sd = 0.1,
  max = 10
)

estimate_delay(rlnorm(1000, log(2), 1),max_value = 15, bootstraps = 1)

spain_ex <- read_csv("data/rt_estimates_2020-07-05.csv") %>% 
  filter(country == "Spain")

france_ex <- read_csv("data/rt_estimates_2020-07-05.csv") %>% 
  filter(country == "France")

estimate_infections

reported_cases <- example_confirmed[1:90]

epinow(reported_cases = reported_cases,
       generation_time = generation_time,
       delays = delay_opts(incubation_period, reporting_delay),
       rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
       stan = stan_opts(cores = 4))
