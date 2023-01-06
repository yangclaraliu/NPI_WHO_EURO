
# Script for Figure 1 and Figure S1

# Read in data (if not already done so)
# joined <- readRDS("data/joined_all_V8.RDS")
joined <- readRDS("data/joined_v9.rds")

# Codes for PHSM of interest 
policy_raw_desc <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6")

# Max effort PHSM data
joined_max_S1_W <- joined$s1_W_max
joined_max_S1_A <- joined$s1_A_max
joined_max_S1_D <- joined$s1_D_max

# Multilevel effort PHSM
joined_mid_S1_W <- joined$s1_W_mid
joined_mid_S1_A <- joined$s1_A_mid
joined_mid_S1_D <- joined$s1_D_mid


# Number of days, Wild type period (308)
no_days_wild <- joined_max_S1_W %>% 
  filter(date >= as.Date("2020-01-20")) %>% 
  group_by(cnt) %>% 
  mutate(marker = 1) %>% 
  summarise(days = sum(marker)) %>%
  pull(days) %>% 
  first()

# Number of days, Alpha period (169)
no_days_alpha <- joined_max_S1_A %>% 
  group_by(cnt) %>% 
  mutate(marker = 1) %>% 
  summarise(days = sum(marker)) %>%
  pull(days) %>% 
  first()

# Number of days, Delta period (143)
no_days_delta <- joined_max_S1_D %>% 
  group_by(cnt) %>% 
  mutate(marker = 1) %>% 
  summarise(days = sum(marker)) %>%
  pull(days) %>% 
  first()

# Mean days active - Max wild type
max_wild <- map(1:length(policy_raw_desc),
                function(i){joined_max_S1_W %>% 
                    filter(date >= as.Date("2020-01-20")) %>% 
                    group_by(country) %>% 
                    mutate(tot = row_number()) %>% 
                    summarise_at(c(policy_raw_desc), sum) %>% 
                    select(country, policy_raw_desc[i]) %>% 
                    rename(var = policy_raw_desc[i]) %>% 
                    mutate(var = (var/no_days_wild)*100) %>% 
                    summarise(mean = mean(var, na.rm = TRUE),
                              sd = sd(var, na.rm = TRUE),
                              n = n()) %>%
                    mutate(se = sd/ sqrt(n),
                           lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
                           upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
                    mutate(var = policy_raw_desc[i])
                }) %>% 
  bind_rows()%>% 
  mutate(variant = "Wild type") %>% 
  mutate(intervention = "Max. effort")

# Mean days active - Max Alpha
max_alpha <- map(1:length(policy_raw_desc),
                 function(i){joined_max_S1_A %>% 
                     group_by(country) %>% 
                     mutate(tot = row_number()) %>% 
                     summarise_at(c(policy_raw_desc), sum) %>% 
                     select(country, policy_raw_desc[i]) %>% 
                     rename(var = policy_raw_desc[i]) %>% 
                     mutate(var = (var/no_days_alpha)*100) %>% 
                     summarise(mean = mean(var, na.rm = TRUE),
                               sd = sd(var, na.rm = TRUE),
                               n = n()) %>%
                     mutate(se = sd/ sqrt(n),
                            lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
                            upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
                     mutate(var = policy_raw_desc[i])
                 }) %>% 
  bind_rows()%>% 
  mutate(variant = "Alpha") %>% 
  mutate(intervention = "Max. effort")

# Mean days active - Max Delta
max_delta <- map(1:length(policy_raw_desc),
                 function(i){joined_max_S1_D %>% 
                     group_by(country) %>% 
                     mutate(tot = row_number()) %>% 
                     summarise_at(c(policy_raw_desc), sum) %>% 
                     select(country, policy_raw_desc[i]) %>% 
                     rename(var = policy_raw_desc[i]) %>% 
                     mutate(var = (var/no_days_delta)*100) %>% 
                     summarise(mean = mean(var, na.rm = TRUE),
                               sd = sd(var, na.rm = TRUE),
                               n = n()) %>%
                     mutate(se = sd/ sqrt(n),
                            lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
                            upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
                     mutate(var = policy_raw_desc[i])
                 }) %>% 
  bind_rows() %>% 
  mutate(variant = "Delta") %>% 
  mutate(intervention = "Max. effort")

# Mean days active - Any wild type
any_wild <- map(1:length(policy_raw_desc),
                function(i){joined_mid_S1_W %>% 
                    filter(date >= as.Date("2020-01-20")) %>% 
                    group_by(country) %>% 
                    mutate(tot = row_number()) %>% 
                    mutate_at(c(policy_raw_desc), function(x){if_else(x > 0, 1, 0)}) %>% 
                    summarise_at(c(policy_raw_desc), sum) %>% 
                    select(country, policy_raw_desc[i]) %>% 
                    rename(var = policy_raw_desc[i]) %>% 
                    mutate(var = (var/no_days_wild)*100) %>% 
                    summarise(mean = mean(var, na.rm = TRUE),
                              sd = sd(var, na.rm = TRUE),
                              n = n()) %>%
                    mutate(se = sd/ sqrt(n),
                           lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
                           upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
                    mutate(var = policy_raw_desc[i])
                }) %>% 
  bind_rows() %>% 
  mutate(variant = "Wild type") %>% 
  mutate(intervention = "Any effort")

# Mean days active - Any alpha type
any_alpha <- map(1:length(policy_raw_desc),
                 function(i){joined_mid_S1_A %>% 
                     group_by(country) %>% 
                     mutate(tot = row_number()) %>% 
                     mutate_at(c(policy_raw_desc), function(x){if_else(x > 0, 1, 0)}) %>% 
                     summarise_at(c(policy_raw_desc), sum) %>% 
                     select(country, policy_raw_desc[i]) %>% 
                     rename(var = policy_raw_desc[i]) %>% 
                     mutate(var = (var/no_days_alpha)*100) %>% 
                     summarise(mean = mean(var, na.rm = TRUE),
                               sd = sd(var, na.rm = TRUE),
                               n = n()) %>%
                     mutate(se = sd/ sqrt(n),
                            lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
                            upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
                     mutate(var = policy_raw_desc[i])
                 }) %>% 
  bind_rows() %>% 
  mutate(variant = "Alpha") %>% 
  mutate(intervention = "Any effort")

# Mean days active - Max delta type
any_delta <- map(1:length(policy_raw_desc),
                 function(i){joined_mid_S1_D %>% 
                     group_by(country) %>% 
                     mutate(tot = row_number()) %>% 
                     mutate_at(c(policy_raw_desc), function(x){if_else(x > 0, 1, 0)}) %>% 
                     summarise_at(c(policy_raw_desc), sum) %>% 
                     select(country, policy_raw_desc[i]) %>% 
                     rename(var = policy_raw_desc[i]) %>% 
                     mutate(var = (var/no_days_delta)*100) %>% 
                     summarise(mean = mean(var, na.rm = TRUE),
                               sd = sd(var, na.rm = TRUE),
                               n = n()) %>%
                     mutate(se = sd/ sqrt(n),
                            lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
                            upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
                     mutate(var = policy_raw_desc[i])
                 }) %>% 
  bind_rows() %>% 
  mutate(variant = "Delta") %>% 
  mutate(intervention = "Any effort")

# Bind all scenarios
desc_data <- bind_rows(max_wild, max_alpha, max_delta,
                       any_wild, any_alpha, any_delta) %>% 
  mutate(variant = factor(variant, levels = c("Wild type", "Alpha", "Delta"))) %>% 
  left_join(., joined$policy_dic %>% 
              filter(policy_code %in% policy_raw_desc), by = c("var" = "policy_code")) %>% 
  mutate(lab = factor(lab, levels = rev(joined$policy_dic %>% 
                                          filter(policy_code %in% policy_raw_desc) %>% 
                                          filter(lab !="Vaccine coverage - all population (OWIN)") %>% 
                                          pull(lab)))) %>%
  mutate(lab = as.character(lab)) 

## Factor order of y-axis
# By mean rank of any effort across 3 variant periods
plot_rank_order_any <- desc_data %>% 
  filter(intervention == "Any effort") %>% 
  group_by(variant) %>% 
  mutate(grp_rank = rank(mean)) %>% 
  group_by(var) %>% 
  mutate(grp_mean_rank = mean(grp_rank)) %>% 
  arrange(grp_mean_rank) %>% 
  filter(variant == "Wild type") %>% 
  mutate(lab = as.character(lab)) %>% 
  pull(lab)

# Stacked bar chart panel of plot 
bar_int <- desc_data %>% 
  mutate(lab = factor(lab, levels = plot_rank_order_any)) %>% 
  ggplot(aes(x = mean, y = lab, fill = variant, color = variant))+
  geom_col(data = . %>% filter( intervention=="Max. effort"), position = position_dodge(width = 0.9), alpha = 1) +
  geom_col(data = . %>% filter( intervention=="Any effort"), position = position_dodge(width = 0.9), alpha = 0.4) +
  geom_tile(aes(y=NA_integer_, alpha = factor(intervention)))+
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"))+
  scale_color_manual(values = c("#66c2a5","#fc8d62", "#8da0cb"))+
  
  facet_wrap(~variant)+
  theme_cowplot() +
  theme(axis.text    = element_text(size = 10),
        axis.title = element_text(size = 15),
        legend.text  = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.position = "bottom",
        strip.background = element_rect(fill = "white", 
                                        color = "black"),
        legend.box.just = "left",
        legend.margin=margin()) +
  labs(x = "Mean % of days intervention enacted", 
       y = "", 
       fill = "Variant period",
       alpha = "PHSM strength")+
  guides(fill = FALSE,
         alpha = guide_legend(nrow = 1),
         color = FALSE)


# Vaccine panel of plot 

vaccine_data_owin <- read_csv("data/VAC_OWIN.csv")  # Vaccine coverage data, calculated from script

mean_vac <- vaccine_data_owin %>% 
  select(country, date, V_all) %>% 
  group_by(date) %>% 
  summarise(V_all = mean(V_all)) %>% 
  mutate(country = "Mean")

all_vac_data <- bind_rows(vaccine_data_owin %>% 
                            select(country, date, V_all), mean_vac)

filter_all_vac <- all_vac_data %>% 
  filter(country %in% c("Mean")) %>% 
  mutate(country = "WHO Europe Region mean")

vac_int <- ggplot()+
  geom_rect(aes(xmin = as.Date("2020-01-20"), xmax = as.Date("2020-11-22"), ymin = 0.9, ymax = 1), alpha = 0.6, fill = "#66c2a5")+
  geom_rect(aes(xmin = as.Date("2020-11-22"), xmax = as.Date("2021-05-10"), ymin = 0.9, ymax = 1), alpha = 0.6, fill = "#fc8d62")+
  geom_rect(aes(xmin = as.Date("2021-05-10"), xmax = as.Date("2021-09-30"), ymin = 0.9, ymax = 1), alpha = 0.6, fill = "#8da0cb")+
  geom_rect(aes(xmin = as.Date("2020-01-20"), xmax = as.Date("2020-11-22"), ymin = 0.9, ymax = 1), alpha = 1, color = "#66c2a5", fill = NA)+
  geom_rect(aes(xmin = as.Date("2020-11-22"), xmax = as.Date("2021-05-10"), ymin = 0.9, ymax = 1), alpha = 1, color = "#fc8d62", fill = NA)+
  geom_rect(aes(xmin = as.Date("2021-05-10"), xmax = as.Date("2021-09-30"), ymin = 0.9, ymax = 1), alpha = 1, color = "#8da0cb", fill = NA)+
  geom_text(aes(x=  as.Date("2020-06-16"), y = 0.95, label = "Wild type"))+
  geom_text(aes(x=  as.Date("2021-02-08"), y = 0.95, label = "Alpha"))+
  geom_text(aes(x=  as.Date("2021-07-20"), y = 0.95, label = "Delta"))+
  geom_line(data = all_vac_data, aes(x = date, y= V_all, group = country), color = "grey70", alpha = 0.7)+
  geom_line(data = filter_all_vac, aes(x = date, y= V_all, color = country),linetype = "dashed", size = 1, alpha =0.9)+
  scale_color_manual(values = c( "red"), breaks = c("WHO Europe Region mean"))+
  scale_x_date(breaks = "3 months", date_labels = "%b-%y")+
  theme_cowplot() +
  theme(axis.text    = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text  = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = "white", 
                                        color = "black"),
        legend.box.just = "left",
        legend.margin = margin()) +
  labs(x = "", 
       y = "Proportion of population\n received 1 dose", 
       fill = "Variant period")

plot_grid(
  bar_int ,
  NULL,
  vac_int , 
  labels = c("A", "", "B"),
  ncol = 1, rel_heights = c(0.55, 0.05, 0.45), align = "hv", axis = "l")

ggsave("figs/figs/fig1.png",
       width = 9,
       height = 8)  


# Mean percentage of days PHSM were in place 
desc_data %>% 
  group_by(variant, intervention) %>%
  mutate(mean2 = mean(mean)) %>% 
  mutate(sd2 = sd(mean)) %>% 
  mutate(n2 = n()) %>% 
  mutate(se2 = sd2/ sqrt(n2),
         lower.ci2 = mean2 - qt(1 - (0.05 / 2), n2 - 1) * se2,
         upper.ci2 = mean2 + qt(1 - (0.05 / 2), n2 - 1) * se2) %>% 
  select(variant, intervention, mean2, lower.ci2, upper.ci2) %>% 
  mutate(marker = row_number()) %>% 
  filter(marker == 1)

# Variables to remove for each scenario 
# PHSM at 100% throughout study periods
desc_data %>%
  filter(mean == 100)



###

# Data manipulation for supplementary plot 
max_pro_cnt <- joined$s1_full_max %>% 
  dplyr::select(date, cnt, policy_raw_desc) %>%
  data.table::as.data.table() %>% 
  data.table::melt(., id.vars = c("cnt", "date")) %>% 
  .[, value := as.numeric(as.character(value))] %>% 
  data.table::dcast(., cnt + date ~ variable) %>% 
  mutate(region = "WHO EUROPE") %>%
  select(-cnt) %>% 
  .[, keyby = .(date, region),
    .(C1 = sum(C1),
      C2 = sum(C2),
      C3 = sum(C3),
      C4 = sum(C4),
      C5 = sum(C5),
      C6 = sum(C6),
      C7 = sum(C7),
      C8 = sum(C8),
      E1 = sum(E1),
      E2 = sum(E2),
      #E3 = sum(E3),
      #E4 = sum(E4),
      H1 = sum(H1),
      H2 = sum(H2),
      H3 = sum(H3),
      #H4 = sum(H4),
      H6 = sum(H6))] %>% 
  data.table::melt(., id.vars = c("date", "region")) %>% 
  mutate(value = value/joined$s1_full_max %>% 
           distinct(cnt) %>% 
           nrow()) %>% 
  mutate(effort = "Max effort")

any_pro_cnt <- joined$s1_full_mid %>% 
  dplyr::select(date, cnt, policy_raw_desc) %>%
  mutate_at(c(policy_raw_desc), function(x){if_else(x > 0, 1, 0)}) %>% 
  data.table::as.data.table() %>% 
  data.table::melt(., id.vars = c("cnt", "date")) %>% 
  .[, value := as.numeric(as.character(value))] %>% 
  data.table::dcast(., cnt + date ~ variable) %>% 
  mutate(region = "WHO EUROPE") %>%
  select(-cnt) %>% 
  .[, keyby = .(date, region),
    .(C1 = sum(C1),
      C2 = sum(C2),
      C3 = sum(C3),
      C4 = sum(C4),
      C5 = sum(C5),
      C6 = sum(C6),
      C7 = sum(C7),
      C8 = sum(C8),
      E1 = sum(E1),
      E2 = sum(E2),
      H1 = sum(H1),
      H2 = sum(H2),
      H3 = sum(H3),
      H6 = sum(H6))] %>% 
  data.table::melt(., id.vars = c("date", "region")) %>% 
  mutate(value = value/joined$s1_full_mid %>% 
           distinct(cnt) %>% 
           nrow()) %>% 
  mutate(effort = "Any effort")

# Key dates
markers <- tibble(marker = factor(c("first", "alpha", "delta"), levels = c("first", "alpha", "delta"), labels = c("First case\ndetected", "Alpha becomes\ndominant variant", "Delta becomes\ndominant variant")),
                  date = c(as.Date("2020-01-20"), as.Date("2020-11-23"), as.Date("2021-05-11")))

# Supplementary plot of proportion of countries with PHSMs implemented time series
bind_rows(max_pro_cnt, any_pro_cnt) %>% 
  left_join(., joined$policy_dic %>% 
              filter(policy_code %in% policy_raw_desc), by = c("variable" = "policy_code")) %>%
  as_tibble() %>%
  mutate(lab = factor(lab, levels = joined$policy_dic$lab)) %>% 
  ggplot(., aes(x = date,  y = value, color = effort)) +
  geom_step(size = 1.2) +
  geom_vline(data = markers,
             aes(xintercept =  date,
                 linetype = marker))+
  scale_color_manual(values = c("#66c2a5", "#fc8d62"), label = c("Any effort", "Max effort"))+
  scale_linetype_manual(values = c(3,4,5))+
  facet_wrap(~lab, ncol = 2) + 
  xlim(as.Date("2020-01-01"), as.Date("2021-09-30")) + 
  labs(x = "", y = "Proportion of countries with PHSM implemented", 
       title = "", color = "NPI strength", linetype = "") +
  theme_cowplot() +
  theme(strip.background = element_rect(NA),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 8),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))+
  guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))

# Save fig
ggsave("figs/figs/fig_S1.png",
       width = 8,
       height = 10)  



