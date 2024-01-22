joined_list %>% 
  map(group_by, phase) %>% 
  map(group_split) -> con

for(i in 1:length(joined_list)) con[[i]][[5]] <- joined_list[[i]]

con %>%
  map(~map(., ungroup)) %>% 
  map(~map(., arrange, date)) %>% 
  map(~map(., dplyr::select, all_of(policy_dic_V$policy_code))) %>% 
  map(~map(., as.matrix)) %>% 
  map(~map(., rcorr)) %>% 
  map(~map(., ~.$r)) %>% 
  map(~map(., reshape2::melt)) -> corr_con

write_rds(corr_con, paste0(path_data, "corr_con.rds"))

con %>%
  map(~map(., ungroup)) %>% 
  map(~map(., arrange, date)) %>% 
  map(~map(., dplyr::select, all_of(policy_dic_V$policy_code))) %>% 
  map(~map(., t)) %>% 
  map(~map(., factoextra::get_dist,
           method = "euclidean")) -> distance_con

distance_con %>% write_rds(., paste0(path_data, "distance_con.rds"))

con %>%
  map(~map(., ungroup)) %>% 
  map(~map(., arrange, date)) %>% 
  map(~map(., dplyr::select, all_of(policy_dic_V$policy_code))) %>% 
  map(~map(., pvclust::pvclust,
           method.hclust = "ward.D2",
           method.dist = "euclidean",
           nboot = 5000,
           parallel = T)) -> hcd_con
  
hcd_con %>% write_rds(., paste0(path_data, "hcd_con.rds"))

con %>% 
  map(~map(., nrow))
