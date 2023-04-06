comix %>% 
  rename(cctld = country,
         contact_mean = mean,
         contact_ll = lci,
         contact_ul = uci) %>% 
  mutate(cctld = toupper(cctld)) %>% 
  left_join(rt[,c("date", "cctld", "mean","median", "sd","country")],
            by = c("date", "cctld")) %>% 
  rename(rt_mean = mean,
         rt_median = median,
         rt_sd = sd) %>% 
  group_by(setting, country) %>% 
  mutate(n = n()) -> tab

tab %>% 
  filter(n > 90) %>%
  pull(cctld) %>% 
  unique -> cctld_to_keep
  
tab %>% 
  filter(cctld %in% cctld_to_keep) %>% 
  mutate(phase = case_when(date < voc_switch_inuse$date[1] ~ "wildtype",
                           date >= voc_switch_inuse$date[1] & date < voc_switch_inuse$date[2] ~ voc_switch_inuse$voc_name_short[1],
                           date >= voc_switch_inuse$date[2] & date < voc_switch_inuse$date[3] ~ voc_switch_inuse$voc_name_short[2],
                           date >= voc_switch_inuse$date[3] ~ voc_switch_inuse$voc_name_short[3])) -> tab

# ANOVA and TUKEY
aov_res_rt <- aov(rt_median ~ phase, data = tab %>% mutate(phase = factor(phase, levels = c("wildtype", "Alpha", "Delta", "Omicron"))))
TK_rt <- glht(aov_res_rt, linfct = mcp(phase = "Tukey")) 
summary(TK_rt)

setting_all <- unique(tab$setting) 
aov_res_contact_mean <- list()
for(i in 1:4){
  aov_res_contact_mean[[i]] <- aov(contact_mean ~ phase, 
                                   data = tab %>% 
                                     filter(setting == setting_all[i],
                                            phase != "Omicron") %>% 
                                     mutate(phase = factor(phase, levels = c("wildtype", "Alpha", "Delta"))))
}
TK_contact <- list()
for(i in 1:4) TK_contact[[i]] <- glht(  aov_res_contact_mean[[i]], linfct = mcp(phase = "Tukey")) 

TK_contact %>% map(summary)

# tab %>% 
#   mutate(phase = factor(phase,
#                           levels = c("wildtype",
#                                      "Alpha","Delta","Omicron"))) %>%
#   ggplot(., aes(x = phase, y = rt_median)) +
#   geom_boxplot() 

tab %>% group_by(setting, country) %>% group_split() -> tmp
tab %>% group_by(setting, country, phase) %>% group_split() %>% map(mutate, n_phased = n()) %>% bind_rows() %>% filter(n_phased > 30) %>% group_by(setting, country, phase) %>% group_split() -> tmp_phased
tmp %>% map(dplyr::select, country, setting) %>% map(unique) %>% bind_rows() %>% mutate(setting = as.character(setting)) -> tmp_index
tmp_phased %>% map(dplyr::select, country, setting, phase) %>% map(unique) %>% bind_rows() %>% mutate(setting = as.character(setting)) -> tmp_phased_index

tmp_ccf <- list()
for(i in 1:length(tmp)){
  res <- ccf(tmp[[i]]$contact_mean, tmp[[i]]$rt_median)
  data.frame(acf = res$acf,
             lags = res$lag,
             sig_threshold = 2/sqrt(res$n.used)) %>% 
    mutate(acf_abs = abs(acf),
           n = nrow(tmp[[i]]),
           sig = if_else(acf_abs > sig_threshold, T, F),
           stat_rank = rank(desc(acf_abs)),
           country = tmp_index$country[i],
           setting = tmp_index$setting[i]) -> tmp_ccf[[i]]
  print(i)
}

tmp_phased_ccf <- list()
for(i in 1:length(tmp_phased)){
  res <- ccf(tmp_phased[[i]]$contact_mean, tmp_phased[[i]]$rt_median)
  data.frame(acf = res$acf,
             lags = res$lag,
             sig_threshold = 2/sqrt(res$n.used)) %>% 
    mutate(acf_abs = abs(acf),
           n = nrow(tmp_phased[[i]]),
           sig = if_else(acf_abs > sig_threshold, T, F),
           stat_rank = rank(desc(acf_abs)),
           country = tmp_phased_index$country[i],
           setting = tmp_phased_index$setting[i],
           phase = tmp_phased_index$phase[i]) -> tmp_phased_ccf[[i]]
  print(i)
}

tmp_ccf %>% 
  map(dplyr::filter, stat_rank == 1) %>% 
  bind_rows() %>% 
  mutate(setting = gsub("_genderage","",setting),
         setting = factor(setting,
                          levels = rev(c("All", "Work", "Home", "Other")))) %>% 
  ggplot(., aes(x = country, y =setting)) +
  geom_tile(aes(fill = acf)) +
  geom_text(aes(label = round(acf, 2), color = sig), size = 6) +
  scale_fill_gradient2(low = "#ef8a62",
                       high = "#67a9cf",
                       mid = "white",
                       midpoint = 0) +
  scale_color_manual(values = c("grey80", "black")) +
  theme_cowplot() +
  labs(x = "", y = "", fill = "Autocorreltion Coefficient") +
  theme(axis.text = element_text(size = 12),
        legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "cm"),
        legend.key.height =  unit(0.6, "cm")) +
  guides(color = "none") -> p_acf

tmp_ccf %>% 
  map(dplyr::filter, stat_rank == 1) %>% 
  bind_rows() %>% 
  mutate(setting = gsub("_genderage","",setting),
         setting = factor(setting,
                          levels = rev(c("All", "Work", "Home", "Other")))) %>% 
  ggplot(., aes(x = country, y =setting)) +
  geom_tile(aes(fill = lags)) +
  geom_text(aes(label = lags, color = sig), size = 6) +
  scale_fill_gradient2(low = "#ef8a62",
                       high = "#67a9cf",
                       mid = "white",
                       midpoint = 0) +
  scale_color_manual(values = c("grey80", "black")) +
  theme_cowplot() +
  labs(x = "", y = "", fill = "Autocorreltion Coefficient") +
  theme(axis.text = element_text(size = 12),
        legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "cm"),
        legend.key.height =  unit(0.6, "cm")) +
  guides(color = "none") -> p_lags

ggsave("figs/acf.png",
       p_acf,
       width = 10,
       height = 6)

tmp_phased_ccf %>% 
  map(dplyr::filter, stat_rank == 1) %>% 
  bind_rows() %>% 
  mutate(setting = gsub("_genderage","",setting),
         setting = factor(setting,
                          levels = rev(c("All", "Work", "Home", "Other"))),
         phase = factor(phase,
                        levels = c("wildtype",
                                   "Alpha",
                                   "Delta"),
                        labels = c("Wildtype phase",
                                   "Alpha phase",
                                   "Delta phase"))) %>%
  filter(phase != "Omicron") %>% 
  ggplot(., aes(x = country, y =setting)) +
  geom_tile(aes(fill = acf),
            color = "black") +
  geom_text(aes(label = round(acf, 2)), size = 6) +
  theme_cowplot() +
  scale_fill_gradient2(low = "#ef8a62",
                       high = "#67a9cf",
                       mid = "white",
                       midpoint = 0) +
  labs(x = "", y = "", fill = "Autocorreltion Coefficient") +
  theme(axis.text = element_text(size = 12),
        legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.size = unit(2, "cm"),
        legend.key.height =  unit(0.6, "cm"),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = NA, color = "black")) +
  facet_wrap(~phase,
             ncol = 1) -> p_phased

ggsave("figs/acf_phased.png",
       p_phased,
       width = 10,
       height = 15)


