comix %>% 
  rename(cctld = country,
         contact_mean = mean,
         contact_ll = lci,
         contact_ul = uci) %>% 
  mutate(cctld = toupper(cctld)) %>% 
  group_by(setting) %>% group_split() %>% 
  map(left_join, country_index, by = "cctld") %>% 
  map(left_join, joined_list[[3]], by = c("iso3c", "date", "cctld")) %>% 
  bind_rows() %>% 
  rename(rt_mean = mean,
         rt_median = median) %>% 
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
                           date >= voc_switch_inuse$date[3] ~ voc_switch_inuse$voc_name_short[3]),
         phase = factor(phase,
                        levels = c("wildtype",
                                   "Alpha",
                                   "Delta",
                                   "Omicron"))) -> tab

formula_interaction <- paste0("contact_mean ~ ", policy_dic_V$policy_code, "*phase + setting + iso3c") 
formula_no_interaction <- paste0("contact_mean ~ ", policy_dic_V$policy_code, "+ phase + setting + iso3c") 
formula_all_no_interaction <- paste0("contact_mean ~ ", paste0(policy_dic_V$policy_code, collapse = " + "), "+ phase + setting + iso3c") 
formula_all_interaction <- paste0("contact_mean ~ ", paste0(paste0(policy_dic_V$policy_code, " * phase"), collapse = " + "), " + setting + iso3c") 

glm(data = tab, formula = formula_all_no_interaction) -> model_all_no_interaction
glm(data = tab, formula = formula_all_interaction) -> model_all_interaction
lapply(1:length(formula_interaction), function(x) glm(data = tab, formula = formula_interaction[x], family = "Gamma")) -> models_interaction
lapply(1:length(formula_no_interaction), function(x) glm(data = tab, formula = formula_interaction[x], family = "Gamma")) -> models_no_interaction

summary(model_all_interaction) -> model_all_interaction_summary
summary(model_all_no_interaction) -> model_all_no_interaction_summary
models_interaction %>% map(summary) -> models_interaction_summary
models_no_interaction %>% map(summary) -> models_no_interaction_summary

# full, multi-variable model
model_all_interaction_summary$coefficients %>% 
  data.frame() %>% 
  mutate(sig = `Pr...t..` <= 0.05) %>% 
  rownames_to_column() %>% 
  separate(rowname, into = c("rowname_1","rowname_2"), sep = ":") %>% 
  mutate(park_test = grepl("phase", rowname_2),
         park = if_else(park_test == FALSE, rowname_1, rowname_2),
         park2 = if_else(park_test, rowname_1, rowname_2)) %>% 
  dplyr::select(-rowname_1, -rowname_2) %>% 
  mutate(park3 = substr(park2, 1, 2),
         nc = nchar(park2),
         park2 = substr(park2, 3, nc),
         park3_test = is.na(park3),
         park3 = if_else(is.na(park3), park, park3),
         park3 = if_else(park3 == "pr", "prop", park3)) %>% 
  dplyr::select(-park2, -park3_test, -nc, -park_test) -> model_coef

model_coef %>% 
  filter(park3 %in% policy_dic_V$policy_code) %>% 
  mutate(park = if_else(park %in% policy_dic_V$policy_code,
                        "phaseWildtype",
                        park),
         park = gsub("phase", "", park)) %>% 
  rename(phase = park,
         policy_code = park3) %>% 
  mutate(phase = factor(phase,
                        levels = c("Wildtype",
                                   "Alpha",
                                   "Delta",
                                   "Omicron"))) %>% 
  rename(sd = `Std..Error`) %>% 
  left_join(policy_dic_V, by = "policy_code") %>% 
  mutate(mu = as.numeric(NA),
         LL = as.numeric(NA),
         UL = as.numeric(NA)) %>% 
  group_by(policy_code) %>% 
  group_split() -> model_coef_policy_list

# check all policies to express in incremental effect
for(j in 1:length(model_coef_policy_list)){
  for(i in 1:nrow(model_coef_policy_list[[j]])){
    if(i == 1) {
      model_coef_policy_list[[j]]$mu[1] <- model_coef_policy_list[[j]]$Estimate[1]
      model_coef_policy_list[[j]]$LL[1] <- model_coef_policy_list[[j]]$Estimate[1] - 1.96*model_coef_policy_list[[j]]$sd[1]
      model_coef_policy_list[[j]]$UL[1] <- model_coef_policy_list[[j]]$Estimate[1] + 1.96*model_coef_policy_list[[j]]$sd[1]
    } else {
      sample_baseline <- rnorm(5000, model_coef_policy_list[[j]]$Estimate[1], model_coef_policy_list[[j]]$sd[1])
      sample_target <- rnorm(5000, model_coef_policy_list[[j]]$Estimate[i], model_coef_policy_list[[j]]$sd[i])
      model_coef_policy_list[[j]]$mu[i] <- mean(sample_baseline + sample_target)
      model_coef_policy_list[[j]]$LL[i] <- quantile(sample_baseline + sample_target, 0.025)
      model_coef_policy_list[[j]]$UL[i] <- quantile(sample_baseline + sample_target, 0.975)
    }
  }
}

model_coef_policy_list %>% 
  bind_rows() %>% 
  mutate(sig = if_else(LL*UL > 0, T, F),
         lab = factor(lab,
                      levels = policy_dic_V$lab)) -> p_tab

p_tab %>% 
  dplyr::filter(policy_code != "prop") %>% 
  mutate(policy_code = factor(policy_code,
                              levels = policy_dic$policy_code,
                              labels = policy_dic$lab)) %>% 
  ggplot(., aes(x = phase, y = mu, shape = sig, color = cat)) +
  geom_point(size = 5) +
  geom_segment(aes(x = phase, xend = phase, y = LL, yend = UL))+
  facet_wrap(~policy_code, 
             # scales = "free", 
             drop = F, ncol = 3) +
  scale_color_manual(values = colors_cat) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_discrete(drop = F) +
  scale_shape_manual(values = c(4, 16)) +
  theme_cowplot()+
  theme(#axis.text.x = element_text(angle = 90),
    legend.position = "none",
    strip.background = element_rect(fill = NA),
    panel.border = element_rect(colour = "black", fill=NA, size=0.4)) +
  labs(x = "Variant phases",
       y = "Effects")  -> p

legend_shape <- get_legend(p + theme(legend.position = "bottom", legend.box = "vertical") + guides(color = FALSE))
legend_color <- get_legend(p + theme(legend.position = "bottom", legend.box = "vertical") + guides(shape = FALSE,
                                                                                                   color=guide_legend(nrow = 2,
                                                                                                                      byrow = FALSE)))
plot_grid(legend_shape, NULL, legend_color, ncol = 1, rel_heights = c(1,-1,2)) -> p_legend
plot_grid(p_legend, NULL, p, ncol = 1, rel_heights = c(1,-0.4,10)) -> p_save

ggsave("figs/manuscript_fig5.png",
       p_save,
       height = 18, width = 10)

# ggsave("figs/diagnostics/multivariate.png",
#        p,
#        width = 15,
#        height = 8)
#  

# ANOVA and TUKEY
# aov_res_rt <- aov(rt_median ~ phase, data = tab %>% mutate(phase = factor(phase, levels = c("wildtype", "Alpha", "Delta", "Omicron"))))
# TK_rt <- glht(aov_res_rt, linfct = mcp(phase = "Tukey")) 
# summary(TK_rt)
# 
# setting_all <- unique(tab$setting) 
# aov_res_contact_mean <- list()
# for(i in 1:4){
#   aov_res_contact_mean[[i]] <- aov(contact_mean ~ phase, 
#                                    data = tab %>% 
#                                      filter(setting == setting_all[i],
#                                             phase != "Omicron") %>% 
#                                      mutate(phase = factor(phase, levels = c("wildtype", "Alpha", "Delta"))))
# }
# TK_contact <- list()
# for(i in 1:4) TK_contact[[i]] <- glht(  aov_res_contact_mean[[i]], linfct = mcp(phase = "Tukey")) 
# 
# TK_contact %>% map(summary)
# 
# # tab %>% 
# #   mutate(phase = factor(phase,
# #                           levels = c("wildtype",
# #                                      "Alpha","Delta","Omicron"))) %>%
# #   ggplot(., aes(x = phase, y = rt_median)) +
# #   geom_boxplot() 
# 
# tab %>% group_by(setting, country) %>% group_split() -> tmp
# tab %>% group_by(setting, country, phase) %>% group_split() %>% map(mutate, n_phased = n()) %>% bind_rows() %>% filter(n_phased > 30) %>% group_by(setting, country, phase) %>% group_split() -> tmp_phased
# tmp %>% map(dplyr::select, country, setting) %>% map(unique) %>% bind_rows() %>% mutate(setting = as.character(setting)) -> tmp_index
# tmp_phased %>% map(dplyr::select, country, setting, phase) %>% map(unique) %>% bind_rows() %>% mutate(setting = as.character(setting)) -> tmp_phased_index
# 
# tmp_ccf <- list()
# for(i in 1:length(tmp)){
#   res <- ccf(tmp[[i]]$contact_mean, tmp[[i]]$rt_median)
#   data.frame(acf = res$acf,
#              lags = res$lag,
#              sig_threshold = 2/sqrt(res$n.used)) %>% 
#     mutate(acf_abs = abs(acf),
#            n = nrow(tmp[[i]]),
#            sig = if_else(acf_abs > sig_threshold, T, F),
#            stat_rank = rank(desc(acf_abs)),
#            country = tmp_index$country[i],
#            setting = tmp_index$setting[i]) -> tmp_ccf[[i]]
#   print(i)
# }
# 
# tmp_phased_ccf <- list()
# for(i in 1:length(tmp_phased)){
#   res <- ccf(tmp_phased[[i]]$contact_mean, tmp_phased[[i]]$rt_median)
#   data.frame(acf = res$acf,
#              lags = res$lag,
#              sig_threshold = 2/sqrt(res$n.used)) %>% 
#     mutate(acf_abs = abs(acf),
#            n = nrow(tmp_phased[[i]]),
#            sig = if_else(acf_abs > sig_threshold, T, F),
#            stat_rank = rank(desc(acf_abs)),
#            country = tmp_phased_index$country[i],
#            setting = tmp_phased_index$setting[i],
#            phase = tmp_phased_index$phase[i]) -> tmp_phased_ccf[[i]]
#   print(i)
# }
# 
# tmp_ccf %>% 
#   map(dplyr::filter, stat_rank == 1) %>% 
#   bind_rows() %>% 
#   mutate(setting = gsub("_genderage","",setting),
#          setting = factor(setting,
#                           levels = rev(c("All", "Work", "Home", "Other")))) %>% 
#   ggplot(., aes(x = country, y =setting)) +
#   geom_tile(aes(fill = acf)) +
#   geom_text(aes(label = round(acf, 2), color = sig), size = 6) +
#   scale_fill_gradient2(low = "#ef8a62",
#                        high = "#67a9cf",
#                        mid = "white",
#                        midpoint = 0) +
#   scale_color_manual(values = c("grey80", "black")) +
#   theme_cowplot() +
#   labs(x = "", y = "", fill = "Autocorreltion Coefficient") +
#   theme(axis.text = element_text(size = 12),
#         legend.position = "top",
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 12),
#         legend.key.size = unit(2, "cm"),
#         legend.key.height =  unit(0.6, "cm")) +
#   guides(color = "none") -> p_acf
# 
# tmp_ccf %>% 
#   map(dplyr::filter, stat_rank == 1) %>% 
#   bind_rows() %>% 
#   mutate(setting = gsub("_genderage","",setting),
#          setting = factor(setting,
#                           levels = rev(c("All", "Work", "Home", "Other")))) %>% 
#   ggplot(., aes(x = country, y =setting)) +
#   geom_tile(aes(fill = lags)) +
#   geom_text(aes(label = lags, color = sig), size = 6) +
#   scale_fill_gradient2(low = "#ef8a62",
#                        high = "#67a9cf",
#                        mid = "white",
#                        midpoint = 0) +
#   scale_color_manual(values = c("grey80", "black")) +
#   theme_cowplot() +
#   labs(x = "", y = "", fill = "Autocorreltion Coefficient") +
#   theme(axis.text = element_text(size = 12),
#         legend.position = "top",
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 12),
#         legend.key.size = unit(2, "cm"),
#         legend.key.height =  unit(0.6, "cm")) +
#   guides(color = "none") -> p_lags
# 
# ggsave("figs/acf.png",
#        p_acf,
#        width = 10,
#        height = 6)
# 
# tmp_phased_ccf %>% 
#   map(dplyr::filter, stat_rank == 1) %>% 
#   bind_rows() %>% 
#   mutate(setting = gsub("_genderage","",setting),
#          setting = factor(setting,
#                           levels = rev(c("All", "Work", "Home", "Other"))),
#          phase = factor(phase,
#                         levels = c("wildtype",
#                                    "Alpha",
#                                    "Delta"),
#                         labels = c("Wildtype phase",
#                                    "Alpha phase",
#                                    "Delta phase"))) %>%
#   filter(phase != "Omicron") %>% 
#   ggplot(., aes(x = country, y =setting)) +
#   geom_tile(aes(fill = acf),
#             color = "black") +
#   geom_text(aes(label = round(acf, 2)), size = 6) +
#   theme_cowplot() +
#   scale_fill_gradient2(low = "#ef8a62",
#                        high = "#67a9cf",
#                        mid = "white",
#                        midpoint = 0) +
#   labs(x = "", y = "", fill = "Autocorreltion Coefficient") +
#   theme(axis.text = element_text(size = 12),
#         legend.position = "top",
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 12),
#         legend.key.size = unit(2, "cm"),
#         legend.key.height =  unit(0.6, "cm"),
#         strip.text = element_text(size = 12),
#         strip.background = element_rect(fill = NA, color = "black")) +
#   facet_wrap(~phase,
#              ncol = 1) -> p_phased
# 
# ggsave("figs/acf_phased.png",
#        p_phased,
#        width = 10,
#        height = 15)


