if(!exists("joined")) joined <- here("data", "joined_all.RDS") %>% readRDS
if(!exists("who") | !exists("tags") | !exists("start")) here("extract_who.R") %>% source

start <- start %>%
  mutate(tag = "Turning\npoint", tag_date = as.Date("2020-04-06"), rk = paste0(rk,"\n")) %>% 
  mutate(alpha = "Alpha becomes\ndominant variant", alpha_date = as.Date("2020-12-01")) %>% 
  mutate(delta = "Delta becomes\ndominant variant", delta_date = as.Date("2021-05-01"))

markers <- tibble(marker = factor(c("first", "alpha", "delta"), 
                                  levels = c("first", "alpha", "delta"),
                                  labels = c("First case\ndetected", "Alpha becomes\ndominant variant", "Delta becomes\ndominant variant")),
                  date = c(as.Date("2020-01-20"), as.Date("2020-12-01"), as.Date("2021-05-01")))

joined <-  readRDS("data/joined_all_V1.RDS") 

policy_raw <- joined$policy_dic$policy_code

counts_data <- joined$hi %>% 
  mutate(scenario = "Maximum effort scenario") %>% 
  
 
  dplyr::select(date, cnt, scenario, policy_raw) %>%
  data.table::as.data.table() %>% 
  mutate_at(vars(policy_raw),
            ~as.numeric(.))
  data.table::melt(., id.vars = c("cnt", "date")) %>% 
  .[, value := as.numeric(as.character(value))] %>% 
  data.table::dcast(., cnt + date + scenario ~ variable) %>% 
  .[, ':=' (P1 = if_else(C1 + C2 + C3 + C4 + C5 + C6 + C7> 0, 1, 0),
            P2 = if_else(C8 >0, 1, 0),
            P3 = if_else(E1 + E2 + E3 > 0, 1, 0),
            P4 = if_else(H1 + H2 + H3 + H4 > 0, 1, 0),
            region = "WHO EUROPE")] %>%
  .[,!..policy_raw] %>% 
  .[,!"cnt"] %>% 
  .[, keyby = .(date, region, scenario),
    .(P1 = sum(P1),
      P2 = sum(P2),
      P3 = sum(P3),
      P4 = sum(P4))] %>% 
  data.table::melt(., id.vars = c("date", "region", "scenario")) %>% 
  merge(., start[c("region", "date", "rk")], all.x = T, by = c("date", "region")) 


oxford_cty <- joined$hi %>% 
  ungroup() %>% 
  distinct(cnt) %>% 
  pull()

region_count <- countrycode::codelist %>% 
  dplyr::select(iso3c, region, country.name.en) %>% 
  dplyr::filter(iso3c %in% oxford_cty) %>% 
  mutate(region = "WHO EUROPE") %>% 
  filter(!is.na(region),
         !is.na(iso3c)) %>% 
  group_by(region) %>% 
  tally() %>% 
  mutate(lab = paste0(region, "\n(n = ", n,")")) 

start <- start %>% left_join(region_count,by = "region")

fig_counts <- counts_data %>%
  merge(., region_count, all.x = TRUE, by = "region") %>%
  .[, proportion := value/n] %>% 
  .[, scenario := factor(scenario,
                         levels = c("Maximum effort scenario", "Any effort scenario"),
                         labels = c("Max. Efforts", "Any Effort"))] %>% 
  .[!is.na(region)] %>% 
  ggplot(., aes(x = date, 
                y = proportion,
                group = variable,
                color = variable)) +
  geom_step(size = 1.2) +
  facet_grid(scenario ~ lab) + 
  xlim(as.Date("2020-01-01"), as.Date("2021-09-30")) + 
  labs(x = "", y = "Proportion of countries in Region with NPI") +
  ggsci::scale_color_lancet(name = "Policy group", 
                            labels = c("Internal\nrestrictions\n",
                                       "Int'l travel \nrestrictions\n",
                                       "Economic \nmeasures\n",
                                       "Health systems\nactions"))+
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%Y")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(NA),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 20),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))+
  geom_vline(data = markers,
             aes(xintercept =  date,
                 linetype = marker))+
  scale_linetype_manual(values = c(3,4,5), name  = "Epidemic\nprogression")


ggsave(filename = "figs/EURO_0/fig1_v2.png",
       plot = fig_counts,
       width = 20,
       height = 10)

counts_data_all_var <- bind_rows(`Maximum effort scenario` = joined$hi, 
                                 `Any effort scenario` = joined$lo, .id = "scenario") %>%
  dplyr::select(date, cnt, scenario, policy_raw) %>%
  data.table::as.data.table() %>% 
  data.table::melt(., id.vars = c("cnt", "date","scenario")) %>% 
  .[, value := as.numeric(as.character(value))] %>% 
  data.table::dcast(., cnt + date + scenario ~ variable) %>% 
  mutate(region = "WHO EUROPE") %>%
  select(-cnt) %>% 
  .[, keyby = .(date, region, scenario),
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
      E3 = sum(E3),
      E4 = sum(E4),
      H1 = sum(H1),
      H2 = sum(H2),
      H3 = sum(H3),
      H4 = sum(H4),
      H5 = sum(H5))] %>% 
  data.table::melt(., id.vars = c("date", "region", "scenario")) %>% 
  merge(., start[c("region", "date", "rk")], all.x = T, by = c("date", "region"))  

fig_counts_all_var_any <-  counts_data_all_var %>%
  merge(., region_count, all.x = TRUE, by = "region") %>%
  .[, proportion := value/n] %>% 
  .[, scenario := factor(scenario,
                         levels = c("Maximum effort scenario", "Any effort scenario"),
                         labels = c("Max. Efforts", "Any Effort"))] %>% 
  .[!is.na(region)] %>% 
  filter(scenario == "Any Effort") %>% 
  ggplot(., aes(x = date, 
                y = proportion)) +
  geom_step(size = 1.2) +
  facet_wrap(~variable, ncol = 4) + 
  xlim(as.Date("2020-01-01"), as.Date("2021-09-30")) + 
  labs(x = "", y = "Proportion of countries with NPI", title = "Any effort scenario") +
  ggsci::scale_color_lancet(name = "Policy group", 
                            labels = c("Internal\nrestrictions\n",
                                       "Int'l travel \nrestrictions\n",
                                       "Economic \nmeasures\n",
                                       "Health systems\nactions"))+
  #scale_x_date(date_breaks = "2 months", date_labels = "%b-%Y")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(NA),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 15),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))+
  geom_vline(data = markers,
             aes(xintercept =  date,
                 linetype = marker))+
  scale_linetype_manual(values = c(3,4,5), name  = "Epidemic\nprogression")

ggsave(filename = "figs/EURO_0/fig1_v3_any_effort.png",
       plot = fig_counts_all_var_any,
       width = 15,
       height = 10)

fig_counts_all_var_max <-  counts_data_all_var %>%
  merge(., region_count, all.x = TRUE, by = "region") %>%
  .[, proportion := value/n] %>% 
  .[, scenario := factor(scenario,
                         levels = c("Maximum effort scenario", "Any effort scenario"),
                         labels = c("Max. Efforts", "Any Effort"))] %>% 
  .[!is.na(region)] %>% 
  filter(scenario == "Max. Efforts") %>% 
  ggplot(., aes(x = date, 
                y = proportion)) +
  geom_step(size = 1.2) +
  facet_wrap(~variable, ncol = 4) + 
  xlim(as.Date("2020-01-01"), as.Date("2021-09-30")) + 
  labs(x = "", y = "Proportion of countries with NPI", title = "Max effort scenario") +
  ggsci::scale_color_lancet(name = "Policy group", 
                            labels = c("Internal\nrestrictions\n",
                                       "Int'l travel \nrestrictions\n",
                                       "Economic \nmeasures\n",
                                       "Health systems\nactions"))+
  #scale_x_date(date_breaks = "2 months", date_labels = "%b-%Y")+
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(NA),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 15),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))+
  geom_vline(data = markers,
             aes(xintercept =  date,
                 linetype = marker))+
  scale_linetype_manual(values = c(3,4,5), name  = "Epidemic\nprogression")

ggsave(filename = "figs/EURO_0/fig1_v3_max_effort.png",
       plot = fig_counts_all_var_max,
       width = 15,
       height = 10) 


