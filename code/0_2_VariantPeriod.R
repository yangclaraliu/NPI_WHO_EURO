voc <- read_xlsx(paste0(path_data, "voc_data.xlsx")) %>% 
  filter(source == "GISAID")

# only save country combinations with number sequenced greater than 100
voc %>% 
  dplyr::select(country, year_week, number_sequenced) %>% 
  unique() %>% 
  filter(number_sequenced > 100) %>% 
  mutate(labels = paste(country, year_week, sep = "_")) %>% pull(labels) -> country_year_week_to_keep

voc_index <- data.frame(voc_name_long = unique(voc$variant)) %>% 
  mutate(voc_name_short = case_when(grepl("B.1.1.7", voc_name_long) ~ "Alpha",
                                    grepl("B.1.351", voc_name_long) ~ "Beta",
                                    grepl("B.1.525", voc_name_long) ~ "Eta",
                                    grepl("B.1.526", voc_name_long) ~ "Iota",
                                    grepl("B.1.617.1", voc_name_long) ~ "Kappa",
                                    grepl("B.1.617.2|AY.4.2", voc_name_long) ~ "Delta",
                                    grepl("B.1.621", voc_name_long) ~ "Mu",
                                    grepl("B.1.427|B.1.429", voc_name_long) ~ "Epsilon",
                                    grepl("C.37", voc_name_long) ~ "Lambda",
                                    grepl("P.1|B.1.1.28.1", voc_name_long) ~ "Gamma",
                                    grepl("P.2", voc_name_long) ~ "Zeta",
                                    grepl("P.3", voc_name_long) ~ "Theta",
                                    grepl("B.1.1.529|BA.1|BA.2|BA.4|BA.5|BA.3|XBB|BQ.1", voc_name_long) ~ "Omicron",
                                    TRUE ~ voc_name_long)) %>% 
  rename(variant = voc_name_long)

voc %>% 
  mutate(labels = paste(country, year_week, sep = "_")) %>% 
  dplyr::filter(labels %in% country_year_week_to_keep) %>% 
  left_join(voc_index,
            by = "variant") %>% 
  dplyr::filter(!voc_name_short %in% c("UNK", "Other"),
                voc_name_short %in% c("Alpha", "Beta", "Delta", "Omicron","Gamma","Mu","Eta")) %>% 
  group_by(country, year_week, voc_name_short) %>% 
  summarise(n = sum(number_detections_variant),
            m = mean(number_sequenced_known_variant)) %>%
  group_by(year_week, voc_name_short) %>% 
  summarise(n = sum(n),
            m = sum(m)) %>% 
  mutate(prop = n/m) %>% 
  separate(year_week, into = c("year", "week"), sep = "-") %>% 
  mutate(week = parse_number(week),
         date = parse_date_time(paste(year, week, 1, sep = "/"), "Y/W/w"),
         date = if_else(is.na(date), ymd("2020-12-28"), ymd(date))) -> voc

lapply(seq(0.05,0.5,0.05),
       function(x){
         voc %>% 
           filter(prop > x) %>% 
           group_by(voc_name_short) %>% 
           group_split() %>% 
           map(arrange, date) %>% 
           map(.f = function(x)x[1,]) %>% 
           bind_rows()
       }) %>% 
  setNames(seq(0.05,0.5,0.05)) %>% 
  bind_rows(.id = "threshold") %>% 
  mutate(threshold = as.numeric(threshold)) -> voc_switch

voc_switch_inuse <- voc_switch %>% filter(threshold == 0.3)

# VOC_raw_EURO <- VOC_raw %>% 
#   mutate(cnt = countrycode(location,
#                            origin = "country.name",
#                            destination = "iso3c")) %>% 
#   mutate(country = countrycode(cnt,
#                                origin = "iso3c",
#                                destination = "country.name")) %>% 
#   mutate(location = if_else(location == "Bosnia and Herzegovina", "Bosnia & Herzegovina", location)) %>% # Change naming of Bosnia to WHO format
#   filter(location %in% WHO_cty) %>% 
#   select(country, cnt, date, variant, num_sequences, perc_sequences, num_sequences_total)


# Countries with more than 500 average weekly sequences 
# VOC_data_cnt <- VOC_raw_EURO %>% 
#   group_by(country, date) %>% 
#   filter(row_number()==1) %>% 
#   group_by(country) %>% 
#   summarise(mean_no = mean(num_sequences_total)) %>% 
#   arrange(mean_no) %>%
#   filter(mean_no > 500) %>% 
#   pull(country)

# Find estimated date in which variants become domiant, using differnt thresholds
# find_dates <- VOC_raw_EURO %>%
#   filter(variant %in% c("Alpha", "Delta")) %>% # Filter ot only Alpha and Delta 
#   filter(country %in% VOC_data_cnt) %>% # Filter to countires with more than 500 sequences a week
#   group_by(date, variant) %>% 
#   summarise(sum_seq = sum(num_sequences, na.rm = TRUE), sum_all = sum(num_sequences_total, na.rm = TRUE)) %>% 
#   mutate(proportion = sum_seq/ sum_all) %>% 
#   group_by(variant) %>% 
#   complete(date = seq.Date(as.Date("2020-01-01"), as.Date("2021-09-30"), by = "day")) %>% 
#   mutate(proportion = imputeTS::na_interpolation(proportion))


# Dates at which proprtion is exceeded
# VOC_dates <- bind_rows(find_dates %>% 
#                          group_by(variant) %>% 
#                          filter(proportion >=0.05) %>% 
#                          filter(row_number()==1) %>% 
#                          mutate(sen = "A_5"),
#                        
#                        find_dates %>% 
#                          group_by(variant) %>% 
#                          filter(proportion >=0.1) %>% 
#                          filter(row_number()==1) %>% 
#                          mutate(sen = "A_10"),
#                        
#                        
#                        find_dates %>% 
#                          group_by(variant) %>% 
#                          filter(proportion >=0.25) %>% 
#                          filter(row_number()==1) %>% 
#                          mutate(sen = "A_25"),
#                        
#                        find_dates %>% 
#                          group_by(variant) %>% 
#                          filter(proportion >=0.5) %>% 
#                          filter(row_number()==1) %>% 
#                          mutate(sen = "A_50")) %>% 
#   ungroup() %>% 
#   select(-c(sum_seq, sum_all))

# Date boundries
# sen_dates <- VOC_dates %>% 
#   mutate(mm = rep(c("xmin", "xmax"),4)) %>% 
#   select(date, sen, mm) %>% 
#   pivot_wider(names_from = mm, values_from = date) 


# Plot table for bars (quite manual)
# sens_bars <- tibble(sen = c(rep("A_5", 3)),
#                     
#                     variant = c("Wild type", "Alpha", "Delta"),
#                     
#                     xmin = as.Date(c("2020-01-22", "2020-11-22", "2021-05-10")),
#                     
#                     xmax = as.Date(c("2020-11-22", "2021-05-10", "2021-09-30")),
#                     
#                     ymin = c(rep(-0.05,3)),
#                     
#                     ymax = c(rep(-0.1,3))) %>% 
#   mutate(variant = factor(variant, levels = c("Wild type", "Alpha", "Delta")))


# Plot
# voc %>% 
#   filter(voc_name_short %in% c("Alpha", "Delta", "Omicron")) %>% 
#   ggplot(., aes(x = date, y = prop, group = voc_name_short, col = voc_name_short)) +
#   geom_line() +
#   geom_vline(data = voc_switch %>% filter(threshold == 0.3),
#              aes(xintercept = date, color = voc_name_short),
#              linetype = 2)

# VOC_raw_EURO %>%
#   filter(variant %in% c("Alpha", "Delta")) %>% 
#   filter(country %in% VOC_data_cnt) %>% 
#   group_by(date, variant) %>% 
#   summarise(sum_seq = sum(num_sequences, na.rm = TRUE), sum_all = sum(num_sequences_total, na.rm = TRUE)) %>% 
#   mutate(proportion = sum_seq/ sum_all) %>% 
#   group_by(variant) %>% 
#   complete(date = seq.Date(as.Date("2020-01-01"), as.Date("2021-09-30"), by = "day")) %>% 
#   mutate(proportion = imputeTS::na_interpolation(proportion)) %>% 
#   filter(date < as.Date("2021-09-30")) %>% 
#   ggplot()+
#   geom_line(aes(x = date, y = proportion, color = variant), size = 1.5) +
#   geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", size = 0.5) +
#   geom_vline(xintercept = as.Date("2020-11-22"), linetype = "dashed", color = "red", size = 0.5) +
#   geom_vline(xintercept = as.Date("2021-05-10"), linetype = "dashed", color = "red", size = 0.5) +
#  # geom_hline(yintercept = 0.1, linetype = "dashed", color = "#fc8d62", size = 1) +
# #  geom_hline(yintercept = 0.25, linetype = "dashed", color = "#8da0cb", size = 1) +
#  # geom_hline(yintercept = 0.5, linetype = "dashed", color = "#e78ac3", size = 1) +
#   geom_rect(data = sens_bars, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = variant), color = "grey50")+
#   scale_fill_brewer(palette = "Set2", name = "Virus period")+
#   scale_color_manual(values = c("grey40", "grey70"), name = "SARS-CoV-2 Variant")+
#   scale_alpha_discrete(range = c(0.3, 1))+
#   scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")+
#  # geom_text(x=as.Date("2020-07-22"), y= -0.16, label="Wild virus dominant period")+
#  # geom_text(x=as.Date("2021-04-01"), y= -0.16, label="Alpha variant\n dominant period")+
#  # geom_text(x=as.Date("2021-08-05"), y= -0.16, label="Delta variant\n dominant period")+
#   guides(alpha = FALSE)+
#   coord_cartesian()+
#   #ylim(-0.15, 1.01)+
#   labs(x = "Date", y = "Proportion of sequenced samples")+
#   theme_bw()

# Save mask max
# ggsave(filename = "figs/EURO_V6/variant_periods.png",
#        plot = last_plot(),
#        width = 10,
#        height = 6)

