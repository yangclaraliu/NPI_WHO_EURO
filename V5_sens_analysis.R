

VOC_raw <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/variants/covid-variants.csv")# %>% 
  #filter(variant %in% c("Alpha", "Beta", "Gamma"))

VOC_raw_EURO <- VOC_raw %>% 
  mutate(cnt = countrycode(location,
                           origin = "country.name",
                           destination = "iso3c")) %>% 
  mutate(country = countrycode(cnt,
                               origin = "iso3c",
                               destination = "country.name")) %>% 
  mutate(location = if_else(location == "Bosnia and Herzegovina", "Bosnia & Herzegovina", location)) %>% # Change naming of Bosnia to WHO format
  filter(location %in% WHO_cty) %>% 
  select(country, cnt, date, variant, num_sequences, perc_sequences, num_sequences_total)


VOC_raw_EURO %>%
  filter(variant %in% c("Alpha", "Delta")) %>% 
  filter(country %in% VOC_data_cnt) %>% 
  ggplot()+
  geom_line(aes(x = date, y = perc_sequences, color = variant))+
  facet_wrap(~country)


VOC_data_cnt <- VOC_raw_EURO %>% 
  group_by(country, date) %>% 
  filter(row_number()==1) %>% 
  group_by(country) %>% 
  summarise(mean_no = mean(num_sequences_total)) %>% 
  arrange(mean_no) %>%
  filter(mean_no > 500) %>% 
  pull(country)

VOC_raw_EURO %>%
  filter(variant %in% c("Alpha", "Delta")) %>% 
  filter(country %in% VOC_data_cnt) %>% 
  group_by(date, variant) %>% 
  summarise(sum_seq = sum(num_sequences), sum_all = sum(num_sequences_total)) %>% 
  mutate(proportion = sum_seq/ sum_all) %>% 
  ggplot(aes(x = date, y = proportion))+
  geom_line(aes(color = variant))+
  geom_hline(yintercept = 0.05, linetype = "dashed")+
  geom_hline(yintercept = 0.1, linetype = "dashed")+
  geom_hline(yintercept = 0.25, linetype = "dashed")+
  geom_hline(yintercept = 0.5, linetype = "dashed")+
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = "dashed")+
  xlim(as.Date("2019-12-22"), as.Date("2021-09-30"))
  scale_x_date(date_breaks = "1 months", date_labels = "%b %Y")


find_dates <- VOC_raw_EURO %>%
  filter(variant %in% c("Alpha", "Delta")) %>% 
  filter(country %in% VOC_data_cnt) %>% 
  group_by(date, variant) %>% 
  summarise(sum_seq = sum(num_sequences), sum_all = sum(num_sequences_total)) %>% 
  mutate(proportion = sum_seq/ sum_all)

VOC_dates <- bind_rows(find_dates %>% 
    group_by(variant) %>% 
    filter(proportion >=0.1) %>% 
    filter(row_number()==1) %>% 
    mutate(sen = "A_10"),
  
  find_dates %>% 
    group_by(variant) %>% 
    filter(proportion >=0.05) %>% 
    filter(row_number()==1) %>% 
    mutate(sen = "A_5"),
  
  find_dates %>% 
    group_by(variant) %>% 
    filter(proportion >=0.25) %>% 
    filter(row_number()==1) %>% 
    mutate(sen = "A_25"),
  
  find_dates %>% 
    group_by(variant) %>% 
    filter(proportion >=0.5) %>% 
    filter(row_number()==1) %>% 
    mutate(sen = "A_50")) %>% 
  ungroup() 

VOC_dates %>% 
  mutate(mm = rep(c("xmin", "xmax"),4)) %>% 
  select(date, sen, mm) %>% 
  pivot_wider(names_from = mm, values_from = date) %>% 
  mutate(ymin = 0, ymax = 1) %>% 
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))+
  geom_rect(aes(color = sen,, fill = sen), alpha = 0.1)
  
  
  ggplot(aes(xmin = date, xmax = date, ymin = height, ymax = height, color = sen))+
  
  
  

  geom_rect()
  

