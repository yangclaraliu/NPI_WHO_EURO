oxford_data_latest <- read_csv(file = "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_nat_latest.csv") %>%  
  as.tibble() %>% 
  filter(Jurisdiction == "NAT_TOTAL")%>% 
  mutate(Date = lubridate::ymd(Date)) %>% 
  dplyr::select(-Jurisdiction,-RegionName,-RegionCode) 

oxford_data_latest %>% 
  dplyr::select(CountryName) %>% 
  distinct() %>% 
  mutate(iso3c = countrycode::countrycode(CountryName,
                                          "country.name",
                                          "iso3c")) -> dic_tmp

oxford_data_latest %<>% 
  left_join(dic_tmp, by = "CountryName") %>% 
  filter(iso3c %in% country_index$iso3c)

date_tmp <- data.frame(Date = seq(range(oxford_data_latest$Date)[1],
                                  range(oxford_data_latest$Date)[2],
                                  by = "day"))

oxford_data_latest %<>% 
  dplyr::select(-ends_with("Flag"), -starts_with("V"), -CountryCode) %>% 
  dplyr::select(CountryName, Date, 
                starts_with(paste0("C",1:8),ignore.case = F),
                starts_with(paste0("E",1:2),ignore.case = F),
                starts_with(paste0("H",c(1:3,6)),ignore.case = F)) %>% 
  right_join(date_tmp, by = "Date") %>% 
  group_by(CountryName,Date) %>% 
  pivot_longer(cols = 3:16,
               names_to = "policy_name") %>% 
  mutate(policy_name = substr(policy_name,1,2),
         value = as.numeric(value))

oxford_data_latest %<>% 
  left_join(dic_tmp, by = "CountryName") %>% 
  dplyr::select(CountryName,iso3c,Date,policy_name,value) %>% 
  setNames(c("country","iso3c","date","policy_name","value"))

write_rds(oxford_data_latest,
          paste0(path_data, "oxford_data_latest.rds"))
