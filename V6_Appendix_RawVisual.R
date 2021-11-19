library(tidyverse)
library(cowplot)
library(ggsci)
library(arrangements)

install.packages("arrangements")

# Full TS max
var_compare_remove <- c("V_all_adj", "V_18_60_adj", "V_60_adj", "V_tot_adj", "median", "lower_90", "upper_90", "lower_50", "upper_50")

tmp <- joined$s1_full_max %>% # change to $hi 
  group_by(cnt, date) %>% 
  dplyr::select(-c(var_compare_remove))

tmp <- tmp %>% 
  pivot_longer(starts_with(c("C","E","H", "V"), 
                           ignore.case = F)) %>%
  group_by(cnt, name) %>% 
  group_split() %>% 
  map(arrange, date) %>% 
  map(mutate, value_diff = c(NA, diff(value))) %>% 
  bind_rows() %>% 
  filter(value_diff != 0 & !is.na(value_diff)) %>% 
  ungroup %>% 
  mutate(marker = if_else(value_diff == 1, "Implementation", "Removal")) %>% 
  dplyr::select(-value, -value_diff)

tmp_clusters <- pvclust::pvpick(hcd_FAD$s1_full_max)$clusters %>% # change to $hi
  map(enframe) %>% 
  bind_rows(.id = "cluster_id") %>% 
  dplyr::select(-name) %>% 
  rename(policy_code = value) 

panel_grid <- arrangements::combinations(joined$policy_dic$policy_code %>% .[!. %in% c("V_all_adj", "V_18_60_adj", "V_60_adj", "V_tot_adj")],
                           2) %>% 
  as_tibble %>% 
  setNames(c("policy_code_1",
             "policy_code_2")) %>% 
  left_join(joined$policy_dic[,c("policy_code",
                                 "lab",
                                 "cat")],
            by = c("policy_code_1" = "policy_code")) %>% 
  rename(lab_1 = lab,
         cat_1 = cat) %>% 
  left_join(joined$policy_dic[,c("policy_code",
                                 "lab",
                                 "cat")],
            by = c("policy_code_2" = "policy_code")) %>% 
  rename(lab_2 = lab,
         cat_2 = cat) %>% 
  left_join(tmp_clusters, c("policy_code_1" = "policy_code")) %>% 
  rename(cluster_id_1 = cluster_id) %>% 
  left_join(tmp_clusters, c("policy_code_2" = "policy_code")) %>% 
  rename(cluster_id_2 = cluster_id) %>% 
  mutate(same_cluster = cluster_id_1 == cluster_id_2,
         same_cluster = if_else(is.na(same_cluster), F, same_cluster)) %>% 
  mutate(cluster_id = if_else(same_cluster == T, cluster_id_1, as.character(NA)))

p_list <- list()
for(i in 1:nrow(panel_grid)){
  col_tmp <- "black"
  if(panel_grid$same_cluster[i] == T) col_tmp <- "#1B9E77"
  
  tmp %>% 
    group_by(cnt,name, marker) %>% 
    mutate(a = 1) %>% 
    mutate(order = cumsum(a)) %>% 
    mutate(imp = paste(marker, order, sep = "_")) %>% 
    group_by(cnt, imp) %>% 
    pivot_wider(names_from = name,
                values_from = date) %>% 
    unnest() %>% 
    select(c("country", "cnt","marker","a", "order", "imp", panel_grid$policy_code_1[i], panel_grid$policy_code_2[i])) %>%
    drop_na %>% 
    ggplot(., aes(x = !!sym(panel_grid$policy_code_1[i]),
                  y = !!sym(panel_grid$policy_code_2[i]),
                  color = imp)) +
    theme_minimal() +
    geom_point(alpha = 0.6) +
    geom_abline(aes(intercept = 0,
                    slope = 1)) +
    labs(x = panel_grid$policy_code_1[i],
         y = panel_grid$policy_code_2[i],
         color = "Time Marker",
         title = paste0("Cluster#:", panel_grid[i,"cluster_id"] %>% pull)) +
    theme(axis.title = element_text(color = col_tmp),
          panel.border = element_rect(color = col_tmp, fill = NA),
          panel.grid = element_blank(),
          plot.title = element_text(color = col_tmp))+
    scale_color_manual(values =  c("Implementation_1" = "#006d2c", "Implementation_2" = "#31a354", "Implementation_3" = "#74c476",
                                   "Implementation_4" = "#a1d99b", "Implementation_5" = "#c7e9c0", "Implementation_6" = "#edf8e9",
                                   "Removal_1" = "#a63603", "Removal_2" = "#e6550d", "Removal_3" = "#fd8d3c",
                                   "Removal_4" = "#fdae6b", "Removal_5" = "#fdd0a2", "Removal_6" = "#feedde")) -> p_list[[i]]
}


start <- seq(1,nrow(panel_grid),20)
end <- c(start[-1] - 1, nrow(panel_grid))

for(i in 1:length(start)){
  do.call(ggpubr::ggarrange, c(p_list[start[i]:end[i]], 
                               ncol = 5,
                               nrow = 4,
                               common.legend = T)) -> p 
  ggsave(filename = paste0("figs/EURO_V6/","figS2_full_TS_max_",i,".png"),
         plot = p,
         width = 15,
         height = 10)
}





####
# Full TS Wild type
var_compare_remove <- c("V_all_adj", "V_18_60_adj", "V_60_adj", "V_tot_adj", "median", "lower_90", "upper_90", "lower_50", "upper_50")

tmp <- joined$s1_W_max %>% # change to $hi 
  group_by(cnt, date) %>% 
  dplyr::select(-c(var_compare_remove))

tmp <- tmp %>% 
  pivot_longer(starts_with(c("C","E","H", "V"), 
                           ignore.case = F)) %>%
  group_by(cnt, name) %>% 
  group_split() %>% 
  map(arrange, date) %>% 
  map(mutate, value_diff = c(NA, diff(value))) %>% 
  bind_rows() %>% 
  filter(value_diff != 0 & !is.na(value_diff)) %>% 
  ungroup %>% 
  mutate(marker = if_else(value_diff == 1, "Implementation", "Removal")) %>% 
  dplyr::select(-value, -value_diff)

tmp_clusters <- pvclust::pvpick(hcd_W$s1_W_max)$clusters %>% # change to $hi
  map(enframe) %>% 
  bind_rows(.id = "cluster_id") %>% 
  dplyr::select(-name) %>% 
  rename(policy_code = value) 

panel_grid <- arrangements::combinations(joined$policy_dic$policy_code %>% .[!. %in% c("V_all_adj", "V_18_60_adj", "V_60_adj", "V_tot_adj")],
                                         2) %>% 
  as_tibble %>% 
  setNames(c("policy_code_1",
             "policy_code_2")) %>% 
  left_join(joined$policy_dic[,c("policy_code",
                                 "lab",
                                 "cat")],
            by = c("policy_code_1" = "policy_code")) %>% 
  rename(lab_1 = lab,
         cat_1 = cat) %>% 
  left_join(joined$policy_dic[,c("policy_code",
                                 "lab",
                                 "cat")],
            by = c("policy_code_2" = "policy_code")) %>% 
  rename(lab_2 = lab,
         cat_2 = cat) %>% 
  left_join(tmp_clusters, c("policy_code_1" = "policy_code")) %>% 
  rename(cluster_id_1 = cluster_id) %>% 
  left_join(tmp_clusters, c("policy_code_2" = "policy_code")) %>% 
  rename(cluster_id_2 = cluster_id) %>% 
  mutate(same_cluster = cluster_id_1 == cluster_id_2,
         same_cluster = if_else(is.na(same_cluster), F, same_cluster)) %>% 
  mutate(cluster_id = if_else(same_cluster == T, cluster_id_1, as.character(NA)))

p_list <- list()
for(i in 1:nrow(panel_grid)){
  col_tmp <- "black"
  if(panel_grid$same_cluster[i] == T) col_tmp <- "#1B9E77"
  
  tmp %>% 
    group_by(cnt,name, marker) %>% 
    mutate(a = 1) %>% 
    mutate(order = cumsum(a)) %>% 
    mutate(imp = paste(marker, order, sep = "_")) %>% 
    group_by(cnt, imp) %>% 
    pivot_wider(names_from = name,
                values_from = date) %>% 
    unnest() %>% 
    select(c("country", "cnt","marker","a", "order", "imp", panel_grid$policy_code_1[i], panel_grid$policy_code_2[i])) %>%
    drop_na %>% 
    ggplot(., aes(x = !!sym(panel_grid$policy_code_1[i]),
                  y = !!sym(panel_grid$policy_code_2[i]),
                  color = imp)) +
    theme_minimal() +
    geom_point(alpha = 0.6) +
    geom_abline(aes(intercept = 0,
                    slope = 1)) +
    labs(x = panel_grid$policy_code_1[i],
         y = panel_grid$policy_code_2[i],
         color = "Time Marker",
         title = paste0("Cluster#:", panel_grid[i,"cluster_id"] %>% pull)) +
    theme(axis.title = element_text(color = col_tmp),
          panel.border = element_rect(color = col_tmp, fill = NA),
          panel.grid = element_blank(),
          plot.title = element_text(color = col_tmp))+
    scale_color_manual(values =  c("Implementation_1" = "#006d2c", "Implementation_2" = "#31a354", "Implementation_3" = "#74c476",
                                   "Implementation_4" = "#a1d99b", "Implementation_5" = "#c7e9c0", "Implementation_6" = "#edf8e9",
                                   "Removal_1" = "#a63603", "Removal_2" = "#e6550d", "Removal_3" = "#fd8d3c",
                                   "Removal_4" = "#fdae6b", "Removal_5" = "#fdd0a2", "Removal_6" = "#feedde")) -> p_list[[i]]
}


start <- seq(1,nrow(panel_grid),20)
end <- c(start[-1] - 1, nrow(panel_grid))

for(i in 1:length(start)){
  do.call(ggpubr::ggarrange, c(p_list[start[i]:end[i]], 
                               ncol = 5,
                               nrow = 4,
                               common.legend = T)) -> p 
  ggsave(filename = paste0("figs/EURO_V6/","figS2_Wild_max_",i,".png"),
         plot = p,
         width = 15,
         height = 10)
}



tmp %>% 
  filter(name == "C1") %>%
  mutate(date_1 = date) 

tmp %>% 
  filter(name == "C2") %>% 
  group_by(cnt, marker) %>% 
  mutate(a = 1) %>% 
  mutate(order = cumsum(a)) %>% 
  mutate(imp = paste(marker, order, sep = "_")) 

tmp %>% 
  group_by(cnt,name, marker) %>% 
  mutate(a = 1) %>% 
  mutate(order = cumsum(a)) %>% 
  mutate(imp = paste(marker, order, sep = "_")) %>% 
  group_by(cnt, imp) %>% 
  pivot_wider(names_from = name,
              values_from = date) %>% 
  unnest() %>% 
  select(c("country", "cnt","marker","a", "order", "imp", panel_grid$policy_code_1[1], panel_grid$policy_code_2[1])) %>%
  drop_na %>% 
  ggplot(., aes(x = !!sym(panel_grid$policy_code_1[1]),
                y = !!sym(panel_grid$policy_code_2[1]),
                color = imp)) +
  theme_minimal() +
  geom_point(alpha = 0.6) +
  geom_abline(aes(intercept = 0,
                  slope = 1)) +
  labs(x = panel_grid$policy_code_1[1],
       y = panel_grid$policy_code_2[1],
       color = "Time Marker",
       title = paste0("Cluster#:", panel_grid[i,"cluster_id"] %>% pull)) +
  theme(axis.title = element_text(color = col_tmp),
        panel.border = element_rect(color = col_tmp, fill = NA),
        panel.grid = element_blank(),
        plot.title = element_text(color = col_tmp))+
scale_color_manual(values =  c("Implementation_1" = "#006d2c", "Implementation_2" = "#31a354", "Implementation_3" = "#74c476",
                               "Implementation_4" = "#a1d99b", "Implementation_5" = "#c7e9c0", "Implementation_6" = "#edf8e9",
                               "Removal_1" = "#a63603", "Removal_2" = "#e6550d", "Removal_3" = "#fd8d3c",
                               "Removal_4" = "#fdae6b", "Removal_5" = "#fdd0a2", "Removal_6" = "#feedde"))


joined$s1_full_max %>% 
  

tmp %>% 
  group_by(cnt,name, marker) %>% 
  mutate(a = 1) %>% 
  mutate(order = cumsum(a)) %>% 
  mutate(imp = paste(marker, order, sep = "_")) %>% 
  group_by(cnt, imp) %>% 
  pivot_wider(names_from = name,
              values_from = date) %>% 
  unnest() %>% view()
    ggplot(., aes(x = !!sym(panel_grid$policy_code_1[1]),
                         y = !!sym(panel_grid$policy_code_2[1]),
                         color = imp)) +
    theme_minimal() +
    geom_point(alpha = 0.4) +
    geom_abline(aes(intercept = 0,
                    slope = 1)) +
    labs(x = panel_grid$policy_code_1[i],
         y = panel_grid$policy_code_2[i],
         color = "Time Marker",
         title = paste0("Cluster#:", panel_grid[i,"cluster_id"] %>% pull)) +
    theme(axis.title = element_text(color = col_tmp),
          panel.border = element_rect(color = col_tmp, fill = NA),
          panel.grid = element_blank(),
          plot.title = element_text(color = col_tmp)) +
    scale_color_manual(values = c("#D95F02","#7570B3")) 


start <- seq(1,nrow(panel_grid),20)
end <- c(start[-1] - 1, nrow(panel_grid))

for(i in 1:length(start)){
  do.call(ggpubr::ggarrange, c(p_list[start[i]:end[i]], 
                               ncol = 5,
                               nrow = 4,
                               common.legend = T)) -> p 
  ggsave(filename = paste0("figs/R2R/","figS2_lo_",i,".png"),
         plot = p,
         width = 15,
         height = 10)
}








