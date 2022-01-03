
# Read in variant data from Our World in Data
WHO_cty <- c("Albania","Andorra","Armenia","Austria","Azerbaijan","Belarus","Belgium","Bosnia & Herzegovina","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Kazakhstan","Kyrgyzstan","Latvia","Lithuania","Luxembourg","Malta","Moldova","Monaco","Montenegro","Netherlands","North Macedonia","Norway","Poland","Portugal","Romania","Russia","San Marino","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Tajikistan","Turkey","Ukraine","United Kingdom","Uzbekistan")

VOC_raw <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/variants/covid-variants.csv")

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


# Countries with more than 500 average weekly sequences 
VOC_data_cnt <- VOC_raw_EURO %>% 
  group_by(country, date) %>% 
  filter(row_number()==1) %>% 
  group_by(country) %>% 
  summarise(mean_no = mean(num_sequences_total)) %>% 
  arrange(mean_no) %>%
  filter(mean_no > 500) %>% 
  pull(country)

# Find estimated date in which variants become domiant, using differnt thresholds
find_dates <- VOC_raw_EURO %>%
  filter(variant %in% c("Alpha", "Delta")) %>% # Filter ot only Alpha and Delta 
  filter(country %in% VOC_data_cnt) %>% # Filter to countires with more than 500 sequences a week
  group_by(date, variant) %>% 
  summarise(sum_seq = sum(num_sequences, na.rm = TRUE), sum_all = sum(num_sequences_total, na.rm = TRUE)) %>% 
  mutate(proportion = sum_seq/ sum_all) %>% 
  group_by(variant) %>% 
  complete(date = seq.Date(as.Date("2020-01-01"), as.Date("2021-09-30"), by = "day")) %>% 
  mutate(proportion = imputeTS::na_interpolation(proportion))


# Dates at which proprtion is exceeded
VOC_dates <- bind_rows(find_dates %>% 
                         group_by(variant) %>% 
                         filter(proportion >=0.05) %>% 
                         filter(row_number()==1) %>% 
                         mutate(sen = "A_5"),
                       
                       find_dates %>% 
                         group_by(variant) %>% 
                         filter(proportion >=0.1) %>% 
                         filter(row_number()==1) %>% 
                         mutate(sen = "A_10"),
                       
                       
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
  ungroup() %>% 
  select(-c(sum_seq, sum_all))

# Date boundries
sen_dates <- VOC_dates %>% 
  mutate(mm = rep(c("xmin", "xmax"),4)) %>% 
  select(date, sen, mm) %>% 
  pivot_wider(names_from = mm, values_from = date) 


# Plot table for bars (quite manual)
sens_bars <- tibble(sen = c(rep("A_5", 3)),
                    
                    variant = c("Wild type", "Alpha", "Delta"),
                    
                    xmin = as.Date(c("2020-01-22", "2020-11-22", "2021-05-10")),
                    
                    xmax = as.Date(c("2020-11-22", "2021-05-10", "2021-09-30")),
                    
                    ymin = c(rep(-0.05,3)),
                    
                    ymax = c(rep(-0.1,3))) %>% 
  mutate(variant = factor(variant, levels = c("Wild type", "Alpha", "Delta")))


# Plot
VOC_raw_EURO %>%
  filter(variant %in% c("Alpha", "Delta")) %>% 
  filter(country %in% VOC_data_cnt) %>% 
  group_by(date, variant) %>% 
  summarise(sum_seq = sum(num_sequences, na.rm = TRUE), sum_all = sum(num_sequences_total, na.rm = TRUE)) %>% 
  mutate(proportion = sum_seq/ sum_all) %>% 
  group_by(variant) %>% 
  complete(date = seq.Date(as.Date("2020-01-01"), as.Date("2021-09-30"), by = "day")) %>% 
  mutate(proportion = imputeTS::na_interpolation(proportion)) %>% 
  filter(date < as.Date("2021-09-30")) %>% 
  ggplot()+
  geom_line(aes(x = date, y = proportion, color = variant), size = 1.5) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", size = 0.5) +
  geom_vline(xintercept = as.Date("2020-11-22"), linetype = "dashed", color = "red", size = 0.5) +
  geom_vline(xintercept = as.Date("2021-05-10"), linetype = "dashed", color = "red", size = 0.5) +
 # geom_hline(yintercept = 0.1, linetype = "dashed", color = "#fc8d62", size = 1) +
#  geom_hline(yintercept = 0.25, linetype = "dashed", color = "#8da0cb", size = 1) +
 # geom_hline(yintercept = 0.5, linetype = "dashed", color = "#e78ac3", size = 1) +
  geom_rect(data = sens_bars, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = variant), color = "grey50")+
  scale_fill_brewer(palette = "Set2", name = "Virus period")+
  scale_color_manual(values = c("grey40", "grey70"), name = "SARS-CoV-2 Variant")+
  scale_alpha_discrete(range = c(0.3, 1))+
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")+
 # geom_text(x=as.Date("2020-07-22"), y= -0.16, label="Wild virus dominant period")+
 # geom_text(x=as.Date("2021-04-01"), y= -0.16, label="Alpha variant\n dominant period")+
 # geom_text(x=as.Date("2021-08-05"), y= -0.16, label="Delta variant\n dominant period")+
  guides(alpha = FALSE)+
  coord_cartesian()+
  #ylim(-0.15, 1.01)+
  labs(x = "Date", y = "Proportion of sequenced samples")+
  theme_bw()

# Save mask max
ggsave(filename = "figs/EURO_V6/variant_periods.png",
       plot = last_plot(),
       width = 10,
       height = 6)

