# WHO Europe countries 

WHO_cty <- c("Albania","Andorra","Armenia","Austria","Azerbaijan","Belarus","Belgium","Bosnia & Herzegovina","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Kazakhstan","Kyrgyzstan","Latvia","Lithuania","Luxembourg","Malta","Moldova","Monaco","Montenegro","Netherlands","North Macedonia","Norway","Poland","Portugal","Romania","Russia","San Marino","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Tajikistan","Turkey","Ukraine","United Kingdom","Uzbekistan")

who <- covidregionaldata::get_national_data(source = "WHO") %>% 
  mutate(region = countrycode(country,
                              origin = "country.name",
                              destination = "region",
                              nomatch = NULL)) %>% 
  dplyr::filter(country %in% WHO_cty) %>% 
  filter(!is.na(region))


start <- who %>% 
  mutate(region = "WHO EUROPE") %>% 
  group_by(date, region) %>% 
  summarise(value = sum(cases_new, na.rm = T)) %>% 
  filter(value != 0) %>% 
  group_by(region) %>% 
  mutate(rk = rank(date, ties.method = "first")) %>% 
  arrange(region) %>% 
  filter(rk == 1) %>% 
  arrange(region) %>% 
  data.frame %>%
  mutate(date = if_else(region == "East Asia & Pacific", 
                        as.Date("2020-01-01"), 
                        date),
         date = date) %>% 
  mutate(rk = factor(rk,
                     labels = c("First case\ndetected")))

start %>%
  mutate(tag = "Turning\npoint", tag_date = as.Date("2020-04-06"), rk = paste0(rk,"\n"))
