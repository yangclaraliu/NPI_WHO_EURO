# Reload data
#if(!exists("joined")) joined <- here("v2", "joined_all.RDS") %>% readRDS

joined <- readRDS("data/joined_all_V6.RDS")#[s1_W_mid,s1_A_mid, s1_D_mid, s1_W_max, s1_A_max, s1_D_max]

# Full number of countries, no age stratification (original was called policy_raw_all)
policy_raw <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6","V_all_adj")

policy_code <- joined$policy_dic$policy_code

# get available data list
country_list <- unique(joined$s1_W_mid$cnt) %>% 
  enframe(value = "cnt") %>% 
  mutate(country_name = countrycode::countrycode(cnt, "iso3c", "country.name"),
         region = "WHO EUROPE") 

# Countries with no data for variables
cnt_remove <- joined$s1_W_mid %>% 
  filter(is.na(V_all_adj)) %>% 
  distinct(country) %>% 
  pull()

variables_remove_age <- c("V_18_60_adj", "V_60_adj", "V_tot_adj")

gen_regress_data <- function(scen){
  
  # S1
  if(scen == "mid_W") tmp <- joined$s1_W_mid %>% 
      filter(!country %in% cnt_remove) %>% 
      select(-c(variables_remove_age))
  
  if(scen == "mid_A") tmp <- joined$s1_A_mid %>% 
      filter(!country %in% cnt_remove) %>% 
      select(-c(variables_remove_age))
  
  if(scen == "mid_D") tmp <- joined$s1_D_mid %>% 
      filter(!country %in% cnt_remove) %>% 
      select(-c(variables_remove_age))
  
  # S2
  if(scen == "max_W") tmp <- joined$s1_W_max %>% 
      filter(!country %in% cnt_remove) %>% 
      select(-c(variables_remove_age))
  
  if(scen == "max_A") tmp <- joined$s1_A_max %>% 
      filter(!country %in% cnt_remove) %>% 
      select(-c(variables_remove_age))
  
  if(scen == "max_D") tmp <- joined$s1_D_max %>% 
      filter(!country %in% cnt_remove) %>% 
      select(-c(variables_remove_age))
  
  tmp %>% 
    ungroup %>% 
    left_join(country_list %>% dplyr::select(-name), by = "cnt") -> tmp
  
  tmp <- pdata.frame(tmp, index=c("cnt","date"), drop.index=TRUE, row.names=TRUE)
  
  return(tmp)
}


# generate data frame for panel regression
regions_dic <- country_list$region %>% unique %>% sort


# All combinations of models
res_tab_forward <- res_tab <- expand.grid(scen = c("mid_W", "mid_A", "mid_D", 
                                                   "max_W", "max_A","max_D"), 
                                          optim_lag = c(-1, -14, -28)) %>% 
  mutate(select = NA)

# Progress bar for loop
pb <- progress::progress_bar$new(
  format = "[:bar] :current/:total (:percent in :elapsed) eta: :eta",
  total = nrow(res_tab),
  width = 80)

# Backward variable selection process
for(i in 1:nrow(res_tab)){
  
  data_tmp <- gen_regress_data(scen = res_tab$scen[i])
  
  res_tab$select[i] <- list(aicbic_select(optim_lag = res_tab$optim_lag[i], 
                                          data = data_tmp))
  pb$tick()
}

# Save as crashes are common here after
save(res_tab, res_tab_forward, file = "results/res_tab_V6.rdata")


chosen <- lapply(res_tab$select,"[[",3) %>% 
  map(mutate, rk_AIC = rank(AIC_val, ties.method = "first")) %>% 
  map(mutate, rk_BIC = rank(BIC_val, ties.method = "first")) %>% 
  map(filter, rk_AIC == 1| rk_BIC == 1) %>% #here to get R2
  map(dplyr::select, model, rk_AIC, rk_BIC) %>% 
  map(pivot_longer, cols =starts_with ("rk"), names_to = "criterion") %>% 
  map(filter, value == 1) %>% 
  bind_rows(.id = "set") %>% 
  dplyr::select(-value) %>% 
  mutate(criterion = gsub("rk_","",criterion))


# extract the p-values
p_table <- res_tab[1:3] %>% 
  mutate(set = 1:n() %>% 
           as.character) %>% 
  left_join(chosen, by = "set")

p_val <- list()

for(i in p_table$set %>% unique){
  p_table %>% 
    arrange(set, criterion) %>% 
    filter(set == i) %>% 
    pull(model) -> model_selected
  
  sum_AIC <- summary(res_tab$select[[as.numeric(i)]][["AIC_back"]]$model[model_selected[1]][[1]])
  
  sum_AIC$coefficients[,4] %>% 
    enframe %>%
    setNames(c("var","p_val")) %>% 
    mutate(p_lab = case_when(p_val > 0.05 ~ "ns",
                             p_val <= 0.05 & p_val > 0.01 ~ "*",
                             p_val <= 0.01 & p_val > 0.001 ~ "**",
                             p_val <= 0.001 ~ "***"),
           set = i,
           criterion = "AIC") -> res_1
  
  sum_BIC <- summary(res_tab$select[[as.numeric(i)]][["BIC_back"]]$model[model_selected[2]][[1]])
  
  sum_BIC$coefficients[,4] %>% 
    enframe %>%
    setNames(c("var","p_val")) %>% 
    mutate(p_lab = case_when(p_val > 0.05 ~ "ns",
                             p_val <= 0.05 & p_val > 0.01 ~ "*",
                             p_val <= 0.01 & p_val > 0.001 ~ "**",
                             p_val <= 0.001 ~ "***"),
           set = i,
           criterion = "BIC") -> res_2
  
  p_val[[i]] <- rbind(res_1, res_2)
  
}

# Bind rows of value results
p_val <- p_val %>% 
  bind_rows()

# Varaible included/ecluded in each set by criteria
lapply(lapply(res_tab$select,"[[",1),"[[","var_combo") -> tmp

AIC_panel <- lapply(1:18, function(x) tmp[[x]] %>% do.call("rbind", .)) %>% # change 1:18 to the number of sets
  map(mutate, model = 1:n()) %>% 
  bind_rows(.id = "set") 

lapply(lapply(res_tab$select,"[[",2),"[[","var_combo") -> tmp 

BIC_panel <- lapply(1:18, function(x) tmp[[x]] %>% do.call("rbind", .)) %>% 
  map(mutate, model = 1:n()) %>% 
  bind_rows(.id = "set")


# Format data for visulisation
var_select_res <- chosen[chosen$criterion=="AIC",] %>% 
  left_join(AIC_panel, by = c("set","model")) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "var") %>% 
  bind_rows(chosen[chosen$criterion=="BIC",] %>% 
              left_join(BIC_panel, by = c("set","model")) %>% 
              pivot_longer(cols = starts_with("X"), names_to = "var")) %>%  
  mutate(var = parse_number(var),
         value = factor(value)) %>% 
  left_join(joined$policy_dic %>% 
              filter(policy_code %in% policy_raw) %>% 
              mutate(var = 1:n()),
            by = "var") %>% 
  left_join(res_tab[,1:3] %>% 
              mutate(set = 1:n() %>% as.character),
            by = "set") %>% 
  mutate(lags = case_when(optim_lag == -1 ~ 1,
                          optim_lag == -14 ~ 2,
                          optim_lag == -28 ~ 3),
         scen = factor(scen, 
                       levels = c("mid_W", "mid_A", "mid_D", 
                                  "max_W", "max_A","max_D"))) %>% 
  left_join(p_val,
            by = c("policy_code" = "var",
                   "set",
                   "criterion")) %>% 
  mutate(scen_grp = case_when(grepl("mid", scen) ~ "Multi-level",
                              grepl("max", scen) ~ "Max")) %>% 
  #mutate(scen_grp = factor(scen_grp, levels = c("S1", "S2", "S3", "S4"))) %>% 
  mutate(virus = case_when(grepl("W", scen) ~ "Wild type",
                           grepl("A", scen) ~ "Alpha",
                           grepl("D", scen) ~ "Delta")) %>% 
  mutate(virus = factor(virus, levels = c("Wild type", "Alpha", "Delta")))



# Plot original timeline siginificance boxes
var_select_res %>%
  #filter(scen_grp == "Max") %>% 
  ggplot(.)  +
  geom_rect(aes(xmin = lags-0.5,
                ymin = var-0.5, 
                xmax = lags+0.5,
                ymax = var+0.5, 
                fill = value)) +
  geom_text(aes(x = lags,
                y = var,
                label = p_lab)) + 
  # facet_grid(scen ~ max_date + criterion) +
  ggh4x::facet_nested(scen_grp ~  virus + criterion) +
  geom_point(aes(x = 1, y = 5, color = cat), alpha = 0) +
  scale_color_manual(values = c('#66c2a5',
                                '#fc8d62',
                                '#8da0cb',
                                '#e78ac3',
                                '#a6d854')) +
  geom_segment(data = data.frame(y = c(0.5, 0.5, 0.5, 0.5),
                                 yend = c(15.5, 15.5, 15.5, 15.5),
                                 x = c(0.5, 1.5, 2.5,3.5),
                                 xend = c(0.5,1.5, 2.5, 3.5)),
               aes(x = x, xend = xend, y = y, yend = yend)) +
  #  geom_segment(data = data.frame(y = seq(0.5,14.5,1),
  #                                 yend = seq(0.5,14.5,1),
  #                                 x = rep(0.5,14),
  #                                 xend = rep(3.5,14)),
  #               aes(x = x, xend = xend, y = y, yend = yend)) +
  theme_cowplot() +
  scale_y_continuous(breaks = seq(1,15,1),
                     labels = joined$policy_dic %>% 
                       filter(policy_code %in% policy_raw) %>% 
                       pull(lab),
                     trans = "reverse") +
  scale_x_continuous(breaks = c(1,2, 3),
                     labels = c("-1", "-14", "-28")) +
  theme(axis.text    = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.text  = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.position = "bottom",
        strip.background = element_rect(fill = "white", 
                                        color = "black"),
        
        axis.text.y = element_text(color = c(rep('#66c2a5',7),  #'#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854'
                                             '#8da0cb',
                                             rep('#fc8d62',2),
                                             rep('#e78ac3',4),
                                             '#a6d854')),
        legend.box = "vertical",
        legend.box.just = "left",
        legend.margin=margin(),
        text = element_text(family = "Times New Roman"),
        strip.text.y = element_text(face = "italic"),
        strip.text = element_text(size = 20)) +
  scale_fill_manual(values = c("snow2","darkgrey"),
                    labels = c("Variable Excluded","Variable Chosen")) +
  labs(x = "Temporal Lags", 
       y = "", 
       fill = "", 
       color = "Intervention Category") +
  guides(color = guide_legend(override.aes = list(alpha = 1,
                                                  size = 3),
                              nrow = 2))

ggsave("figs/EURO_V6/fig4_1_all.png",
       width = 15,
       height = 10) 




#~#~#~#~# Effect sizes #~#~#~#~#

chosen_models <- lapply(1:nrow(chosen), function(i) {
  res_tab$select[[as.numeric(chosen$set[i])]] %>% 
    .[[(paste0(chosen$criterion[i], "_back"))]] %>% 
    .[["model"]] %>% 
    .[[chosen$model[i]]]
})  


eff_size <- chosen_models %>% 
  map(broom::tidy) %>% 
  bind_rows(.id = "id") %>% 
  left_join(chosen %>% mutate(id = 1:n() %>% as.character),
            by = "id") %>% 
  left_join(res_tab[,1:3] %>% mutate(set = 1:n() %>% as.character), by = "set") %>% 
  separate(., term, into = c("var","dim"), sep = "\\.") 

eff_size %>% 
  dplyr::select(id, set) %>% 
  distinct() %>% 
  left_join(res_tab[,1:3] %>% mutate(set = as.character(1:n())), by = "set")

effect_data <- eff_size %>% 
  left_join(joined$policy_dic %>% 
              filter(policy_code %in% policy_raw) %>% 
              dplyr::select(policy_code, cat, lab) %>% 
              mutate(policy_no = 1:n()),
            by = c("var" = "policy_code")) %>% 
  mutate(x = estimate - 1.96*std.error,
         xend = estimate + 1.96*std.error,
         optim_lag = factor(optim_lag,
                            levels = c(-1, -14, -28),
                            labels = c("1 day", "14 days", "28 days")),
         scen = factor(scen, 
                       levels = c("mid_W","mid_A", "mid_D",
                                  "max_W", "max_A", "max_D")),
         var = factor(var,
                      labels = joined$policy_dic %>% filter(policy_code %in% policy_raw) %>% pull(lab)),
         stat_sig = case_when(x >= 0 ~ "Significantly Positive",
                              xend <0 ~ "Significantly Negative",
                              TRUE ~ "Null Effect") %>% 
           factor(., levels = c("Significantly Negative",
                                "Null Effect",
                                "Significantly Positive"))) %>% 
  mutate(scen_grp = case_when(grepl("mid", scen) ~ "Multi-level",
                              grepl("max", scen) ~ "Max")) %>% 
  #  mutate(scen_grp = factor(scen_grp, levels = c("S1", "S2", "S3", "S4"))) %>% 
  mutate(virus = case_when(grepl("W", scen) ~ "Wild type",
                           grepl("A", scen) ~ "Alpha",
                           grepl("D", scen) ~ "Delta")) %>% 
  mutate(virus = factor(virus, levels = c("Wild type", "Alpha", "Delta")))


cluster_list <- list(hcd_W$s1_W_max, hcd_FAD$s1_A_max, hcd_FAD$s1_D_max,
                     hcd_W$s1_W_mid,  hcd_FAD$s1_A_mid, hcd_FAD$s1_D_mid)

scen_effort <- c(rep("Max", 3), rep("Multi-level", 3))

scen_virus <- rep(c("Wild type", "Alpha", "Delta"),2)

plot_virus_effect <- function(i){
  # Max wild 
  clust_max <- pvclust::pvpick(cluster_list[[i]])$clusters %>% 
    enframe() %>%
    unnest() %>% 
    left_join(., joined$policy_dic %>%
                select(policy_code, lab), by = c("value" = "policy_code")) %>% 
    mutate(cluster = paste0("Cluster - ", name)) 
  
  effect_data %>% 
    filter(scen_grp == scen_effort[[i]]) %>%
    filter(virus == scen_virus[[i]]) %>% 
    left_join(., clust_max, by = c("lab"= "lab")) %>% 
    mutate(cluster = paste0("Cluster - ", name)) %>% 
    mutate(cluster = if_else(cluster == "Cluster - NA", "No cluster", cluster)) %>% 
    ggplot(.) +
    geom_pointrange(aes(x = optim_lag,
                        y = estimate,
                        ymin = x,
                        ymax = xend,
                        color = cat,
                        shape = stat_sig),
                    size = 0.75) +
    scale_alpha_manual(values = c(1,0.5, 0.2)) +
    scale_shape_manual(values = c(16, 1, 13)) +
    ggh4x::facet_nested(virus  ~ cluster + var + criterion,
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
          legend.title = element_text(size = 15),
          strip.background = element_rect(fill = NA),
          axis.text.x = element_text(vjust = 0.5,
                                     angle = 90,
                                     hjust = 1),
          axis.text    = element_text(size = 8),
          axis.title = element_text(size = 15),
          legend.text  = element_text(size = 12),
          strip.text = element_text(size = 10)) +
    labs(y = "Effects",
         x = "Temporal lag",
         color = "Model Specification",
         shape = "Effect Type",
         title = paste0(scen_virus[[i]], " - ", scen_effort[[i]], " effort")) +
    guides(color = guide_legend(nrow = 2),
           shape = guide_legend(nrow = 2))
  
  ggsave(paste0("figs/EURO_V6/fig_5", scen_effort[[i]], "_", scen_virus[[i]], ".png"),
         width = 15,
         height = 6) 
  
  
}

map(1:length(scen_virus), plot_virus_effect)

# plot alpha 

clust_max <- pvclust::pvpick(hcd_FAD$s1_A_max)$clusters %>% 
  enframe() %>%
  unnest() %>% 
  left_join(., joined$policy_dic %>%
              select(policy_code, lab), by = c("value" = "policy_code")) %>% 
  mutate(cluster = paste0("Cluster - ", name)) 

plot_A <- effect_data %>% 
  filter(scen_grp == "Max") %>%
  filter(virus == "Alpha") %>% 
  left_join(., clust_max, by = c("lab"= "lab")) %>% 
  mutate(cluster = paste0("Cluster - ", name)) %>% 
  mutate(cluster = if_else(cluster == "Cluster - NA", "No cluster", cluster)) %>% 
  ggplot(.) +
  geom_pointrange(aes(x = optim_lag,
                      y = estimate,
                      ymin = x,
                      ymax = xend,
                      color = cat,
                      shape = stat_sig),
                  size = 0.75) +
  scale_alpha_manual(values = c(1,0.5, 0.2)) +
  scale_shape_manual(values = c(16, 1, 13)) +
  ggh4x::facet_nested(virus  ~ cluster + var + criterion,
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
        legend.title = element_text(size = 15),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(vjust = 0.5,
                                   angle = 90,
                                   hjust = 1),
        axis.text    = element_text(size = 8),
        axis.title = element_text(size = 15),
        legend.text  = element_text(size = 12),
        strip.text = element_text(size = 10)) +
  labs(y = "Effects",
       x = "Temporal lag",
       color = "Model Specification",
       shape = "Effect Type",
       title = "") +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))


# plot Delta 
clust_max <- pvclust::pvpick(hcd_FAD$s1_D_max)$clusters %>% 
  enframe() %>%
  unnest() %>% 
  left_join(., joined$policy_dic %>%
              select(policy_code, lab), by = c("value" = "policy_code")) %>% 
  mutate(cluster = paste0("Cluster - ", name)) 

plot_D <- effect_data %>% 
  filter(scen_grp == "Max") %>%
  filter(virus == "Delta") %>% 
  left_join(., clust_max, by = c("lab"= "lab")) %>% 
  mutate(cluster = paste0("Cluster - ", name)) %>% 
  mutate(cluster = if_else(cluster == "Cluster - NA", "No cluster", cluster)) %>% 
  ggplot(.) +
  geom_pointrange(aes(x = optim_lag,
                      y = estimate,
                      ymin = x,
                      ymax = xend,
                      color = cat,
                      shape = stat_sig),
                  size = 0.75) +
  scale_alpha_manual(values = c(1,0.5, 0.2)) +
  scale_shape_manual(values = c(16, 1, 13)) +
  ggh4x::facet_nested(virus  ~ cluster + var + criterion,
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
        legend.title = element_text(size = 15),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(vjust = 0.5,
                                   angle = 90,
                                   hjust = 1),
        axis.text    = element_text(size = 8),
        axis.title = element_text(size = 15),
        legend.text  = element_text(size = 12),
        strip.text = element_text(size = 10)) +
  labs(y = "Effects",
       x = "Temporal lag",
       color = "Model Specification",
       shape = "Effect Type",
       title = "") +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))



plot_w
plot_A
plot_D

#### testing

# Plot all effects - no clusters, max effort
effect_data %>% 
  filter(scen_grp == "Max") %>%
  ggplot(.) +
  geom_pointrange(aes(x = optim_lag,
                      y = estimate,
                      ymin = x,
                      ymax = xend,
                      color = cat,
                      shape = stat_sig),
                  size = 0.75) +
  scale_alpha_manual(values = c(1,0.5, 0.2)) +
  scale_shape_manual(values = c(16, 1, 13)) +
  ggh4x::facet_nested(virus  ~   var + criterion,
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
        legend.title = element_text(size = 15),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(vjust = 0.5,
                                   angle = 90,
                                   hjust = 1),
        axis.text    = element_text(size = 8),
        axis.title = element_text(size = 15),
        legend.text  = element_text(size = 12),
        strip.text = element_text(size = 10)) +
  labs(y = "Effects",
       x = "Temporal lag",
       color = "Model Specification",
       shape = "Effect Type",
       title = "") +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))


ggsave("figs/EURO_V6/fig2_0_max_effect.png",
       width = 18,
       height = 8)  

# Multi-level effort
effect_data %>% 
  filter(scen_grp == "Multi-level") %>%
  #  filter(virus == "Delta") %>% 
  #  left_join(., clust_max, by = c("lab"= "lab")) %>% 
  #  mutate(cluster = paste0("Cluster - ", name)) %>% 
  #  mutate(cluster = if_else(cluster == "Cluster - NA", "No cluster", cluster)) %>% 
  ggplot(.) +
  geom_pointrange(aes(x = optim_lag,
                      y = estimate,
                      ymin = x,
                      ymax = xend,
                      color = cat,
                      shape = stat_sig),
                  size = 0.75) +
  scale_alpha_manual(values = c(1,0.5, 0.2)) +
  scale_shape_manual(values = c(16, 1, 13)) +
  ggh4x::facet_nested(virus  ~   var + criterion,
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
        legend.title = element_text(size = 15),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(vjust = 0.5,
                                   angle = 90,
                                   hjust = 1),
        axis.text    = element_text(size = 8),
        axis.title = element_text(size = 15),
        legend.text  = element_text(size = 12),
        strip.text = element_text(size = 10)) +
  labs(y = "Effects",
       x = "Temporal lag",
       color = "Model Specification",
       shape = "Effect Type",
       title = "") +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))


ggsave("figs/EURO_V6/fig2_0_mid_effect.png",
       width = 18,
       height = 8)  


##
# Compare max and multi level
effect_data %>% 
  filter(criterion == "AIC") 
  ggplot(.) +
  geom_pointrange(aes(x = optim_lag,
                      y = estimate,
                      ymin = x,
                      ymax = xend,
                      color = cat,
                      shape = stat_sig),
                  size = 0.75) +
  scale_alpha_manual(values = c(1,0.5, 0.2)) +
  scale_shape_manual(values = c(16, 1, 13)) +
  ggh4x::facet_nested(virus  ~   var + scen_grp,
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
        legend.title = element_text(size = 15),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(vjust = 0.5,
                                   angle = 90,
                                   hjust = 1),
        axis.text    = element_text(size = 8),
        axis.title = element_text(size = 15),
        legend.text  = element_text(size = 12),
        strip.text = element_text(size = 10)) +
  labs(y = "Effects",
       x = "Temporal lag",
       color = "Model Specification",
       shape = "Effect Type",
       title = "") +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))


ggsave("figs/EURO_V6/fig2_0_effort_aic_effect.png",
       width = 18,
       height = 8)  


#### compare across VOC for AIC
effect_data %>% 
  filter(optim_lag == "14 days") %>% 
  filter(criterion == "AIC") %>% 
  ggplot(.) +
  geom_pointrange(aes(x = virus,
                      y = estimate,
                      ymin = x,
                      ymax = xend,
                      color = cat,
                      shape = stat_sig),
                  size = 0.75) +
  scale_alpha_manual(values = c(1,0.5, 0.2)) +
  scale_shape_manual(values = c(16, 1, 13)) +
  ggh4x::facet_nested(scen_grp  ~   var ,
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
        legend.title = element_text(size = 15),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(vjust = 0.5,
                                   angle = 90,
                                   hjust = 1),
        axis.text    = element_text(size = 8),
        axis.title = element_text(size = 15),
        legend.text  = element_text(size = 12),
        strip.text = element_text(size = 10)) +
  labs(y = "Effects",
       x = "Variant period",
       color = "Model Specification",
       shape = "Effect Type",
       title = "") +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))

ggsave("figs/EURO_V6/fig2_0_effort_14_day_AIC.png",
       width = 18,
       height = 8)  




# Fixed/random effects test
tmp <- joined$s1_full_max %>% 
  filter(!country %in% cnt_remove) %>% 
  select(-c(variables_remove_age))

tmp %>% 
  ungroup %>% 
  left_join(country_list %>% dplyr::select(-name), by = "cnt") -> tmp

tmp <- pdata.frame(tmp, index=c("cnt","date"), drop.index=TRUE, row.names=TRUE)

f <-  joined$policy_dic$policy_code %>% 
  .[!. %in% c("V_18_60_adj", "V_60_adj", "V_tot_adj")] %>% 
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

# P value less than 0.05, reject null hypothesis of random effects






# combinations for mapping
cluster_list <- list(hcd_W$s1_W_max, hcd_FAD$s1_A_max, hcd_FAD$s1_D_max,
                     hcd_W$s1_W_mid,  hcd_FAD$s1_A_mid, hcd_FAD$s1_D_mid)

scen_effort <- c(rep("Max", 3), rep("Multi-level", 3))

scen_virus <- rep(c("Wild type", "Alpha", "Delta"),2)

plot_cluster_mean <- function(i){
  
  # Extract clusters 
  clust_max <- pvclust::pvpick(cluster_list[[i]])$clusters %>% 
    enframe() %>%
    unnest() %>% 
    left_join(., joined$policy_dic %>%
                select(policy_code, lab), by = c("value" = "policy_code")) %>% 
    mutate(cluster = paste0("Cluster - ", name)) 
  
  effect_data %>% 
    filter(scen_grp == scen_effort[[i]]) %>%
    filter(virus == scen_virus[[i]]) %>% 
    left_join(., clust_max, by = c("lab"= "lab")) %>% 
    mutate(cluster = paste0("Cluster - ", name)) %>% 
    mutate(cluster = if_else(cluster == "Cluster - NA", "No cluster", cluster)) %>% 
    group_by(cluster, criterion, optim_lag) %>% 
    summarise(mean = mean(estimate, na.rm = TRUE),
              sd = sd(estimate, na.rm = TRUE),
              n = n()) %>%
    mutate(se = sd/ sqrt(n),
           lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>%
    mutate(virus =  scen_virus[[i]]) %>% 
    mutate(effort = scen_effort[[i]])
}


# Map all cluster mean effects  
mean_plot <-   map(1:6, plot_cluster_mean) %>% 
  bind_rows()

# R-squared data for models
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

# Plot mean cluster effect sizes
mean_plot %>% 
  left_join(., r2_data, by = c("virus" = "virus", "effort" = "scen_grp", "optim_lag" = "optim_lag")) %>%  
  mutate(virus = factor(virus, levels = c("Wild type", "Alpha", "Delta"))) %>% 
  filter(effort == "Multi-level") %>% 
  ggplot(.) +
  geom_pointrange(aes(x = optim_lag,
                      y = mean,
                      ymin = lower.ci,
                      ymax = upper.ci, 
                      color = cluster,
                      size = value)) +
  scale_alpha_manual(values = c(1,0.5, 0.2)) +
  scale_shape_manual(values = c(16, 1, 13)) +
  scale_size(range = c(0,1))+
  #  ggh4x::facet_nested(cluster ~  virus + criterion ,
  ggh4x::facet_nested(virus  ~ effort + cluster + criterion ,
                      labeller = label_wrap_gen(multi_line = T,
                                                width = 12), scales = "free_y") +
  scale_color_manual(values = c('#66c2a5',
                                '#fc8d62',
                                '#8da0cb',
                                '#e78ac3',
                                '#a6d854'))+
  theme_bw()+
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 15),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(vjust = 0.5,
                                   angle = 90,
                                   hjust = 1),
        axis.text    = element_text(size = 8),
        axis.title = element_text(size = 15),
        legend.text  = element_text(size = 12),
        strip.text = element_text(size = 10)) +
  #ylim(c(-1.5,1.5))+
  labs(y = "Mean cluster effect size",
       x = "Temporal lag",
       color = "",
       shape = "Effect Type",
       title = "",
       size = "Adjusted R2") +
  guides(color = FALSE,
         shape = guide_legend(nrow = 2)) 

ggsave("figs/EURO_V6/fig2_cluster_mean_effects.png",
       width = 6,
       height = 6)  
