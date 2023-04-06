# Script for panel regression analysis 

# Read in data if not already done so...
# joined <- readRDS("data/joined_all_V8.RDS") #[s1_W_mid,s1_A_mid, s1_D_mid, s1_W_max, s1_A_max, s1_D_max]
# joined <- read_rds("data/joined_v9.rds")

# Codes for PHSMs investigated 
policy_raw <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6","prop")
policy_raw_discrete <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6")
policy_code <- policy_dic$policy_code

# codify things as binary variables
joined$con
f <- paste("median ~", paste(policy_raw, collapse = " + "))
lapply(c("wildtype", "Alpha", "Delta", "Omicron"),
       function(x){
         plm(f, 
             data = joined$con %>%
               filter(!is.na(median),
                      phase == x) %>% 
               pdata.frame(., index=c("iso3c","date"), drop.index=TRUE, row.names=TRUE) %>% 
               mutate(median = as.numeric(median)), model = "within") 
         
       }) %>% 
  map(summary) -> model_all

model_all %>% 
  map(~.$coefficients) %>% 
  setNames(c("wildtype", "Alpha", "Delta", "Omicron")) %>% 
  map(data.frame) %>% 
  map(rownames_to_column, var = "policy_code") %>% 
  bind_rows(.id = "phase") %>% 
  mutate(sig = if_else(`Pr...t..` <= 0.05, T, F),
         CI_LL = Estimate - 1.96*`Std..Error`,
         CI_UL = Estimate + 1.96*`Std..Error`,
         phase = factor(phase,
                        levels = c("wildtype", "Alpha", "Delta", "Omicron"),
                        labels = c("Wiletype phase", "Alpha phase", "Delta phase", "Omicron phase"))) %>% 
  left_join(policy_dic_V, by = "policy_code") -> p_tab

p_tab %>% 
  ggplot(., aes(x = phase, y = Estimate)) +
  geom_point(aes(shape = sig, color = cat)) +
  facet_wrap(~policy_name, scales = "free") +
  scale_shape_manual(values = c(4, 16))









geom_discretize_list <- names(joined)[!(grepl("con|full", names(joined)))]
for(i in 1:length(discretize_list)){
  joined[[discretize_list[i]]] %<>% 
    mutate_at(.vars = vars(policy_raw_discrete),
              factor,
              levels = c(0,1))
}

# we need to drop things with only one levels
for(i in 1:36){
  loc_remove <- rep(F, length(policy_raw))
  for(j in 1:length(policy_raw_discrete)){
    test_res <- joined[[i]][policy_raw_discrete[j]] %>% table
    test_levels <- length(test_res)
    test_val <- min(test_res)
    if(test_val < 28 | test_levels <2){
      loc_remove[j] <- T
    }
  }

  joined[[i]] %<>% 
    dplyr::select(-policy_raw_discrete[which(loc_remove == T)])
}

joined %>% map(ncol)

# Function to prepare data for regression analysis
# gen_regress_data <- function(scen){
#   
#   # S1 Multi-level effort
#   if(scen == "mid_W") tmp <- joined$s1_W_mid 
#   if(scen == "mid_A") tmp <- joined$s1_A_mid 
#   if(scen == "mid_D") tmp <- joined$s1_D_mid 
#   
#   # S2 Max effort
#   if(scen == "max_W") tmp <- joined$s1_W_max 
#   if(scen == "max_A") tmp <- joined$s1_A_max 
#   if(scen == "max_D") tmp <- joined$s1_D_max 
#   
#   tmp %>% 
#     ungroup %>% 
#     left_join(country_list %>% dplyr::select(-name), by = "cnt") -> tmp
#   
#   tmp <- pdata.frame(tmp, index=c("cnt","date"), drop.index=TRUE, row.names=TRUE)
#   
#   return(tmp)
# }
# 
# WHO Region
# regions_dic <- country_list$region %>% unique %>% sort

# All combinations of scenarios and lags 
# res_tab_forward <- res_tab <- expand.grid(scen = c("mid_W", "mid_A", "mid_D", 
#                                                    "max_W", "max_A","max_D"), 
#                                           optim_lag = c(-1, -14, -28)) %>% 
#   mutate(select = NA)

res_tab <- expand.grid(scen = names(joined)[!grepl("full", names(joined))], 
                       optim_lag = c(-1, -14, -28)) %>% 
  mutate(select = NA)

# Progress bar for loop
pb <- progress::progress_bar$new(
  format = "[:bar] :current/:total (:percent in :elapsed) eta: :eta",
  total = nrow(res_tab),
  width = 80)

# Backward variable selection process for all candidate regression models:
# takes a few minutes to run...
for(i in 1:nrow(res_tab)){
  
  data_tmp <-   pdata.frame(joined[[res_tab$scen[i]]], 
                            index = c("cnt","date"),
                            drop.index = T,
                            row.names = T) 
  
  res_tab$select[i] <- list(aicbic_select(optim_lag = res_tab$optim_lag[i], 
                                          data = data_tmp))
  pb$tick()
}

# Save as crashes are common here after if you try and view data
# save(res_tab, res_tab_forward, file = "results/res_tab_V8.rdata")
# load("results/res_tab_V8.rdata")
# write_rds(res_tab, "data/res_tab_v9_back.rds")


# Chosen model for each scenario by BIC and AIC values respectively
# chosen <- lapply(res_tab$select,"[[",3) %>%
#   map(mutate, rk_AIC = rank(AIC_val, ties.method = "first")) %>% # Rank by AIC
#   map(mutate, rk_BIC = rank(BIC_val, ties.method = "first")) %>% # Rank by BIC
#   map(filter, rk_AIC == 1| rk_BIC == 1) %>% # Filter to best model by criteria
#   map(dplyr::select, model, rk_AIC, rk_BIC) %>%
#   map(pivot_longer, cols =starts_with ("rk"), names_to = "criterion") %>%
#   map(filter, value == 1) %>%
#   bind_rows(.id = "set") %>%
#   dplyr::select(-value) %>%
#   mutate(criterion = gsub("rk_","",criterion))

# write_rds(chosen, "data/chosen.rds")

chosen <- read_rds("data/chosen.rds")

# extract p-values
p_val <- list()
# Set p-values for each coefficient and each model set
for(i in 1:length(res_tab$scen)){
  # first index AIC, second index BIC
  index_tmp <- chosen %>% filter(set == i) %>% arrange(criterion) %>% pull(model)
  model_selected <- list()
  model_selected[["AIC_back"]] <- res_tab$select[[i]]$AIC_back$model[index_tmp[1]] %>% .[[1]]
  model_selected[["BIC_back"]] <- res_tab$select[[i]]$BIC_back$model[index_tmp[2]] %>% .[[1]]

  res <- list()
  summary(model_selected$AIC_back) %>% 
    .$coefficients %>% 
    .[,4] %>% 
    enframe %>%
    setNames(c("var","p_val")) %>% 
    mutate(p_lab = case_when(p_val > 0.05 ~ "ns",
                             p_val <= 0.05 & p_val > 0.01 ~ "*",
                             p_val <= 0.01 & p_val > 0.001 ~ "**",
                             p_val <= 0.001 ~ "***"),
           set = i,
           criterion = "AIC") -> res[["AIC_back"]]
  
  summary(model_selected$BIC_back) %>% 
    .$coefficients %>% 
    .[,4] %>% 
    enframe %>%
    setNames(c("var","p_val")) %>% 
    mutate(p_lab = case_when(p_val > 0.05 ~ "ns",
                             p_val <= 0.05 & p_val > 0.01 ~ "*",
                             p_val <= 0.01 & p_val > 0.001 ~ "**",
                             p_val <= 0.001 ~ "***"),
           set = i,
           criterion = "BIC") -> res[["BIC_back"]]
  
  p_val[[i]] <- res %>% bind_rows()
  
}

# Bind rows of value results, for all scenarios 
p_val %<>%
  setNames(res_tab$scen) %>% 
  bind_rows(.id = "scenario") %>% 
  mutate(set = as.character(set)) %>% 
  left_join(res_tab[,1:2] %>% 
              rownames_to_column(var = "set"), by = "set")

write_rds(p_val, "data/p_val.rds")
# Variable included/excluded in each set by criteria
# lapply(lapply(res_tab$select,"[[",1),"[[","var_combo") -> tmp
# 
# AIC_panel <- lapply(1:18, function(x) tmp[[x]] %>% do.call("rbind", .)) %>% # change 1:18 to the number of sets
#   map(mutate, model = 1:n()) %>% 
#   bind_rows(.id = "set") 
# 
# lapply(lapply(res_tab$select,"[[",2),"[[","var_combo") -> tmp 
# 
# BIC_panel <- lapply(1:18, function(x) tmp[[x]] %>% do.call("rbind", .)) %>% 
#   map(mutate, model = 1:n()) %>% 
#   bind_rows(.id = "set")
# 
# 
# # Format data for visulisation
# var_select_res <- chosen[chosen$criterion=="AIC",] %>% 
#   left_join(AIC_panel, by = c("set","model")) %>% 
#   pivot_longer(cols = starts_with("X"), names_to = "var") %>% 
#   bind_rows(chosen[chosen$criterion=="BIC",] %>% 
#               left_join(BIC_panel, by = c("set","model")) %>% 
#               pivot_longer(cols = starts_with("X"), names_to = "var")) %>%  
#   mutate(var = parse_number(var),
#          value = factor(value)) %>% 
#   left_join(joined$policy_dic %>% 
#               filter(policy_code %in% policy_raw) %>% 
#               mutate(var = 1:n()),
#             by = "var") %>% 
#   left_join(res_tab[,1:3] %>% 
#               mutate(set = 1:n() %>% as.character),
#             by = "set") %>% 
#   mutate(lags = case_when(optim_lag == -1 ~ 1,
#                           optim_lag == -14 ~ 2,
#                           optim_lag == -28 ~ 3),
#          scen = factor(scen, 
#                        levels = c("mid_W", "mid_A", "mid_D", 
#                                   "max_W", "max_A","max_D"))) %>% 
#   left_join(p_val,
#             by = c("policy_code" = "var",
#                    "set",
#                    "criterion")) %>% 
#   mutate(scen_grp = case_when(grepl("mid", scen) ~ "Multi-level",
#                               grepl("max", scen) ~ "Max")) %>% 
#   mutate(virus = case_when(grepl("W", scen) ~ "Wild type",
#                            grepl("A", scen) ~ "Alpha",
#                            grepl("D", scen) ~ "Delta")) %>% 
#   mutate(virus = factor(virus, levels = c("Wild type", "Alpha", "Delta")))



# Plot original timeline significance boxes
# var_select_res %>%
#   ggplot(.)  +
#   geom_rect(aes(xmin = lags-0.5,
#                 ymin = var-0.5, 
#                 xmax = lags+0.5,
#                 ymax = var+0.5, 
#                 fill = value)) +
#   geom_text(aes(x = lags,
#                 y = var,
#                 label = p_lab)) + 
#   ggh4x::facet_nested(scen_grp ~  virus + criterion) +
#   geom_point(aes(x = 1, y = 5, color = cat), alpha = 0) +
#   scale_color_manual(values = c('#66c2a5',
#                                 '#fc8d62',
#                                 '#8da0cb',
#                                 '#e78ac3',
#                                 '#a6d854')) +
#   geom_segment(data = data.frame(y = c(0.5, 0.5, 0.5, 0.5),
#                                  yend = c(15.5, 15.5, 15.5, 15.5),
#                                  x = c(0.5, 1.5, 2.5,3.5),
#                                  xend = c(0.5,1.5, 2.5, 3.5)),
#                aes(x = x, xend = xend, y = y, yend = yend)) +
#   theme_cowplot() +
#   scale_y_continuous(breaks = seq(1,15,1),
#                      labels = joined$policy_dic %>% 
#                        filter(policy_code %in% policy_raw) %>% 
#                        pull(lab),
#                      trans = "reverse") +
#   scale_x_continuous(breaks = c(1,2, 3),
#                      labels = c("-1", "-14", "-28")) +
#   theme(axis.text    = element_text(size = 12),
#         axis.title = element_text(size = 15),
#         legend.text  = element_text(size = 12),
#         legend.title = element_text(size = 15),
#         legend.position = "bottom",
#         strip.background = element_rect(fill = "white", 
#                                         color = "black"),
#         
#         axis.text.y = element_text(color = c(rep('#66c2a5',7),  #'#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854'
#                                              '#8da0cb',
#                                              rep('#fc8d62',2),
#                                              rep('#e78ac3',4),
#                                              '#a6d854')),
#         legend.box = "vertical",
#         legend.box.just = "left",
#         legend.margin=margin(),
#         text = element_text(family = "Times New Roman"),
#         strip.text.y = element_text(face = "italic"),
#         strip.text = element_text(size = 20)) +
#   scale_fill_manual(values = c("snow2","darkgrey"),
#                     labels = c("Variable Excluded","Variable Chosen")) +
#   labs(x = "Temporal Lags", 
#        y = "", 
#        fill = "", 
#        color = "Intervention Category") +
#   guides(color = guide_legend(override.aes = list(alpha = 1,
#                                                   size = 3),
#                               nrow = 2))

# Save figure
# ggsave("figs/figs/p_val_sig_box.png",
#        width = 15,
#        height = 10) 


#~#~#~#~# Coefficient Effect Sizes #~#~#~#~#

# Coefficient effect sizes for selected candidate models 
model_selected <- list()
for(i in 1:nrow(res_tab[,1:2])){
  index_tmp <- chosen %>% filter(set == i) %>% arrange(criterion) %>% pull(model)
  model_selected[[i]] <- list()
  model_selected[[i]][["AIC_back"]] <- res_tab$select[[i]]$AIC_back$model[index_tmp[1]] %>% .[[1]]
  model_selected[[i]][["BIC_back"]] <- res_tab$select[[i]]$BIC_back$model[index_tmp[2]] %>% .[[1]]
}

model_selected %>%
  map(map, broom::tidy) %>% 
  map(bind_rows, .id = "criterion") -> effect_size

# chosen_models <- lapply(1:nrow(chosen), function(i) {
#   res_tab$select[[as.numeric(chosen$set[i])]] %>%
#     .[[(paste0(chosen$criterion[i], "_back"))]] %>%
#     .[["model"]] %>%
#     .[[chosen$model[i]]]
# })

# Format data from regression models
effect_size <- chosen_models %>% 
  map(broom::tidy) %>% 
  bind_rows(.id = "id") %>% 
  left_join(chosen %>% mutate(id = 1:n() %>% as.character),
            by = "id") %>% 
  left_join(res_tab[,1:3] %>% mutate(set = 1:n() %>% as.character), by = "set") %>% 
  separate(., term, into = c("var","dim"), sep = "\\.") %>% 
  select(-dim, -select)

# Calculate confidence intervals 
effect_size %>% 
  mutate(var_nchar = nchar(var),
         var_new = if_else(var_nchar == 3, substr(var,1,2), var)) %>% 
  left_join(policy_dic %>% 
              filter(policy_code %in% policy_raw) %>% 
              dplyr::select(policy_code, cat, lab) %>% 
              mutate(policy_no = 1:n()),
            by = c("var_new" = "policy_code")) %>%
  mutate(x = estimate - 1.96*std.error,
         xend = estimate + 1.96*std.error,
         optim_lag = factor(optim_lag,
                            levels = c(-1, -14, -28),
                            labels = c("1 day", "14 days", "28 days"))) %>% 
  mutate(scen = factor(scen),
         var_new = factor(var_new,
                      labels = policy_dic %>% 
                        filter(policy_code %in% policy_raw_all) %>% 
                        add_row(policy_code = "V_all_adj",
                                policy_name = "Vaccine uptake",
                                policy_max = 1,
                                cat = "Vaccinations",
                                lab = "Vaccine uptake") %>% 
                        pull(lab)),
         stat_sig = case_when(x >= 0 ~ "Significantly Positive",
                              xend <0 ~ "Significantly Negative",
                              TRUE ~ "Null Effect") %>% 
           factor(., levels = c("Significantly Negative",
                                "Null Effect",
                                "Significantly Positive"))) %>% 
  mutate(scen_val = case_when(grepl("any", scen) ~ "Any Effort",
                              grepl("con", scen) ~ "Multi-level",
                              grepl("max", scen) ~ "Max. Effort")) %>% 
  mutate(scen_phase = case_when(grepl("s1", scen) ~ "s1",
                              grepl("s2", scen) ~ "s2",
                              grepl("s3", scen) ~ "s3",
                              grepl("s4", scen) ~ "s4")) %>% 
  mutate(virus = case_when(grepl("W", scen, ignore.case = F) ~ "Wild type",
                           grepl("A", scen, ignore.case = F) ~ "Alpha",
                           grepl("D", scen, ignore.case = F) ~ "Delta")) %>% 
  mutate(virus = factor(virus, levels = c("Wild type", "Alpha", "Delta"))) -> effect_data

write_rds(effect_data, "data/effect_data.rds")

# Plot result, point range plot

# Multi-level effort

effect_data %>% 
  filter(scen_val == "Multi-level") %>%
  filter(criterion == "AIC") %>% 
  filter(scen_phase == "s1") %>% 
  filter(!is.na(cat)) %>% 
  filter(cat == "Closure & Containment  ") %>% 
  ggplot(.) +
  geom_pointrange(aes(x = optim_lag,
                      y = estimate,
                      ymin = x,
                      ymax = xend,
                      color = cat,
                      shape = stat_sig),
                  size = 0.5) +
  scale_alpha_manual(values = c(1, 0.5, 0.2)) +
  scale_shape_manual(values = c(16, 1, 13)) +
  ggh4x::facet_nested(virus  ~   var_new ,
                      labeller = label_wrap_gen(multi_line = T,
                                                width = 12)) +
  scale_color_manual(values = c('#66c2a5',
                                '#fc8d62',
                                '#8da0cb',
                                '#e78ac3',
                                '#a6d854'))+
  theme_bw()+
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(vjust = 0.5,
                                   angle = 90,
                                   hjust = 1),
        axis.text    = element_text(size = 8),
        axis.title = element_text(size = 15),
        legend.text  = element_text(size = 8),
        strip.text = element_text(size = 8)) +
  labs(y = "Effect on Rt",
       x = "Temporal lag",
       color = "PHSM category",
       shape = "Effect Type",
       title = "") +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))


# ggsave("figs/figs/fig2.png",
#        width = 12,
#        height = 6)  
# 
# # Max effort
# effect_data %>% 
#   filter(scen_grp == "Max") %>%
#   filter(criterion == "AIC") %>% 
#   ggplot(.) +
#   geom_pointrange(aes(x = optim_lag,
#                       y = estimate,
#                       ymin = x,
#                       ymax = xend,
#                       color = cat,
#                       shape = stat_sig),
#                   size = 0.5) +
#   scale_alpha_manual(values = c(1, 0.5, 0.2)) +
#   scale_shape_manual(values = c(16, 1, 13)) +
#   ggh4x::facet_nested(virus  ~   var ,
#                       labeller = label_wrap_gen(multi_line = T,
#                                                 width = 12)) +
#   scale_color_manual(values = c('#66c2a5',
#                                 '#fc8d62',
#                                 '#8da0cb',
#                                 '#e78ac3',
#                                 '#a6d854'))+
#   theme_bw()+
#   geom_hline(yintercept = 0, color = "black", linetype = 2) +
#   theme(panel.grid = element_blank(),
#         legend.position = "bottom",
#         legend.title = element_text(size = 12),
#         strip.background = element_rect(fill = NA),
#         axis.text.x = element_text(vjust = 0.5,
#                                    angle = 90,
#                                    hjust = 1),
#         axis.text    = element_text(size = 8),
#         axis.title = element_text(size = 15),
#         legend.text  = element_text(size = 8),
#         strip.text = element_text(size = 8)) +
#   labs(y = "Effect on Rt",
#        x = "Temporal lag",
#        color = "PHSM category",
#        shape = "Effect Type",
#        title = "") +
#   guides(color = guide_legend(nrow = 2),
#          shape = guide_legend(nrow = 2))
# 
# 
# ggsave("figs/figs/max_effect.png",
#        width = 12,
#        height = 6)  

# Model diagnostics 

# Darbin Wu test 
# Fixed/random effects test
tmp <- joined$s1_full_max %>% 
  ungroup %>% 
  left_join(country_list %>% dplyr::select(-name), by = "cnt")

tmp <- pdata.frame(tmp, index=c("cnt","date"), drop.index=TRUE, row.names=TRUE)

f <-  joined$policy_dic$policy_code %>% 
  paste(., collapse = " + ") %>% 
  paste0("lag(median,", c(0, -1, -14, -28), ") ~ ",.)

f %>% 
  map(as.formula) %>% 
  map(plm, data = tmp, model = "within") -> g1
# 
f %>% 
  map(as.formula) %>% 
  map(plm, data = tmp, model = "random") -> g2
# 
map2(g1, g2, phtest)

# P-value less than 0.05, reject null hypothesis of random effects


# R-squared data for each models
r2_data <- lapply(res_tab$select,"[[",3) %>% 
  map(mutate, rk_AIC = rank(AIC_val, ties.method = "first")) %>% 
  map(mutate, rk_BIC = rank(BIC_val, ties.method = "first")) %>% 
  map(filter, rk_AIC == 1| rk_BIC == 1) %>% #here to get R2
  map(dplyr::select, model, rk_AIC, rk_BIC, AIC_ar2, BIC_ar2) %>% 
  map(pivot_longer, cols =contains("r2"), names_to = "criterion") %>% 
  map(first) %>% 
  bind_rows(.id = "set") %>%
  select(set, value) %>% 
  cbind(res_tab_forward %>% 
          mutate(set = c(1:18)) %>% 
          select(-select) %>% 
          mutate(set_1 = as.character(set))) %>% 
  select(-set) %>% 
  as_tibble() %>% 
  mutate(scen_grp = case_when(grepl("mid", scen) ~ "Multi-level",
                              grepl("max", scen) ~ "Max")) %>% 
  mutate(virus = case_when(grepl("W", scen) ~ "Wild type",
                           grepl("A", scen) ~ "Alpha",
                           grepl("D", scen) ~ "Delta")) %>% 
  mutate(virus = factor(virus, levels = c("Wild type", "Alpha", "Delta"))) %>% 
  mutate(optim_lag = factor(optim_lag,
                            levels = c(-1, -14, -28),
                            labels = c("1 day", "14 days", "28 days")))



# Average mean absolute error calculation

# Wild = 2, 14, 26
# Alpha = 4, 16, 28
# Delta = 6, 18, 30

# Wild type MAE
WT_MAE <- chosen_models[c(2)] %>% 
  map(~.$residuals) %>% 
  map(data.frame) %>% 
  map(rownames_to_column) %>% 
  map(as_tibble) %>% 
  map(setNames, c("rowname", "value")) %>% 
  map(mutate, iso3c = substr(rowname,1,3),
      date = substr(rowname,5,14)) %>% 
  map(dplyr::select, -rowname) %>% 
  map(group_by, iso3c) %>% 
  map(summarise, value = mean(abs(value))) %>% 
  map(arrange, value) %>% 
  map(mutate, 
      region = "WHO Europe",
      country = countrycode::countrycode(iso3c,
                                         origin = "iso3c",
                                         destination = "country.name")) %>% 
  map(rownames_to_column, var = "rank") %>% 
  bind_rows() %>% 
  group_by(country, iso3c) %>% 
  mutate(rank = as.numeric(rank)) %>% 
  summarise(rank = mean(rank),
            value = mean(value)) %>% 
  arrange(rank) %>% 
  ungroup() %>% 
  mutate(max = max(value)) %>% 
  mutate(adj_value = value/max)

# Alpha MAE
A_MAE <- chosen_models[c(4)] %>% 
  map(~.$residuals) %>% 
  map(data.frame) %>% 
  map(rownames_to_column) %>% 
  map(as_tibble) %>% 
  map(setNames, c("rowname", "value")) %>% 
  map(mutate, iso3c = substr(rowname,1,3),
      date = substr(rowname,5,14)) %>% 
  map(dplyr::select, -rowname) %>% 
  map(group_by, iso3c) %>% 
  map(summarise, value = mean(abs(value))) %>% 
  map(arrange, value) %>% 
  map(mutate, 
      region = "WHO Europe",
      country = countrycode::countrycode(iso3c,
                                         origin = "iso3c",
                                         destination = "country.name")) %>% 
  map(rownames_to_column, var = "rank") %>% 
  bind_rows() %>% 
  group_by(country, iso3c) %>% 
  mutate(rank = as.numeric(rank)) %>% 
  summarise(rank = mean(rank),
            value = mean(value)) %>% 
  arrange(rank)%>% 
  ungroup() %>% 
  mutate(max = max(value)) %>% 
  mutate(adj_value = value/max)


# Delta MAE
D_MAE <- chosen_models[c(6)] %>% 
  map(~.$residuals) %>% 
  map(data.frame) %>% 
  map(rownames_to_column) %>% 
 # as_tibble()
  #map(rownames_to_column) %>% 
 # map(as_tibble) %>% 
  map(setNames, c("rowname", "value")) %>% 
  map(mutate, iso3c = substr(rowname,1,3),
      date = substr(rowname,5,14)) %>% 
  map(dplyr::select, -rowname) %>% 
  map(group_by, iso3c) %>% 
  map(summarise, value = mean(abs(value))) %>% 
  map(arrange, value) %>% 
  map(mutate, 
      region = "WHO Europe",
      country = countrycode::countrycode(iso3c,
                                         origin = "iso3c",
                                         destination = "country.name")) %>% 
  map(rownames_to_column, var = "rank") %>% 
  bind_rows() %>% 
  group_by(country, iso3c) %>% 
  mutate(rank = as.numeric(rank)) %>% 
  summarise(rank = mean(rank),
            value = mean(value)) %>% 
  arrange(rank)%>% 
  ungroup() %>% 
  mutate(max = max(value)) %>% 
  mutate(adj_value = value/max)


# World shp
world <- st_read("shp/World_Countries__Generalized_.shp") 

cnt_study <- WT_MAE %>%
  pull(iso3c)

# Filter to just WHO Europe countries
WHO_EUROPE_SF <- world %>% 
  mutate(cnt = countrycode(COUNTRY,"country.name", "iso3c")) %>% 
  filter(cnt %in% cnt_study)

# Plot of WT MAE
WT_map <- ggplot()+
  geom_sf(data= world,fill="grey90",colour="white") +
  geom_sf(data = WHO_EUROPE_SF %>% 
            left_join(., WT_MAE, by = c("cnt" = "iso3c")), aes(fill = rank)) +
  colorspace::scale_fill_continuous_sequential(palette = "oranges", rev = TRUE, begin = 0, end = 0.7,
                                               guide= guide_colorsteps(show.limits=TRUE, barwidth=15, title.position="top", frame.colour = "black"))+
  
  labs(fill = "Mean Absolute Error Rank")+
  theme_void()+
  coord_sf(xlim=c(-25, 85), ylim=c(30, 70))+
  theme(legend.position = "bottom")

A_map <- ggplot()+
  geom_sf(data= world,fill="grey90",colour="white") +
  geom_sf(data = WHO_EUROPE_SF %>% 
            left_join(., A_MAE, by = c("cnt" = "iso3c")), aes(fill = rank)) +
  colorspace::scale_fill_continuous_sequential(palette = "oranges", rev = TRUE, begin = 0, end = 0.7,
                                               guide= guide_colorsteps(show.limits=TRUE, barwidth=15, title.position="top", frame.colour = "black"))+
  
  labs(fill = "Mean Absolute Error Rank")+
  theme_void()+
  coord_sf(xlim=c(-25, 85), ylim=c(30, 70))+
  theme(legend.position = "bottom")

D_map <- ggplot()+
  geom_sf(data= world,fill="grey90",colour="white") +
  geom_sf(data = WHO_EUROPE_SF %>% 
            left_join(., D_MAE, by = c("cnt" = "iso3c")), aes(fill = rank)) +
  colorspace::scale_fill_continuous_sequential(palette = "oranges", rev = TRUE, begin = 0, end = 0.7,
                       guide= guide_colorsteps(show.limits=TRUE, barwidth=15, title.position="top", frame.colour = "black"))+
  
  labs(fill = "Mean Absolute Error Rank")+
  theme_void()+
  coord_sf(xlim=c(-25, 85), ylim=c(30, 70))+
  theme(legend.position = "bottom")


##

plot_no_legend <- plot_grid(
  WT_map + theme(legend.position = "none"),
  A_map + theme(legend.position = "none"), 
  D_map + theme(legend.position = "none"), 
  labels = c("A", "B", "C"),
  ncol = 1)

legend <- get_legend(WT_map + theme(legend.box.margin = margin(0,0,0,0)))

plot_grid(plot_no_legend, legend, ncol = 1, rel_heights = c(10, 1), align = "h")


ggsave("figs/figs/figS3.png",
       width = 6,
       height = 12)  

