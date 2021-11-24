
joined_max_S1_W %>% 
  select(-var_remove) %>% 
  select(C1, C2) %>% 
  mutate(equal = if_else(C1 == C2, 1, 0)) %>% 
  mutate(total = length(equal)) %>% 
  mutate(same = sum(equal)) %>% 
  mutate(pro = same/total)



joined_max_S1_W %>% 
  select(-var_remove) %>% 
  select(C1, C2) %>% 
  mutate(equal = if_else(C1 == C2, 1, 0)) %>% 
  mutate(total = length(equal)) %>% 
  mutate(same = sum(equal)) %>% 
  mutate(pro = same/total) %>% 
  mutate(a = "C1", b = "C2") %>% 
  select(a,b,pro) %>% 
  first()

poss <- combinations(ox_var, 2) %>% 
  as_tibble()

ox_var <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6")

#poss <- expand.grid(ox_var, ox_var) %>% 
#  mutate(Var1= as.character(Var1), Var2 = as.character(Var2)) %>% 
#  mutate(same = if_else(Var1 == Var2, T, F)) %>% 
 # filter(same != TRUE) %>% 
 # select(Var1, Var2)

per_same <- function(i){
  
joined_max_S1_A %>% 
        select(-var_remove) %>% 
        select(poss[i,1] %>% pull, poss[i,2] %>% pull) %>% 
        rowSums() %>% 
  enframe() %>% 
  mutate(same_value = if_else(value %in% c(0,2), TRUE, FALSE)) %>% 
  mutate(total = length(same_value)) %>% 
  group_by(same_value) %>% 
  mutate(sum = sum(same_value)) %>% 
  mutate(pro = sum/total) %>%
  ungroup() %>% 
    filter(same_value == TRUE) %>% 
  select(pro) %>% 
  first() %>% 
  pull()

}


tmp_res_same <- map(1:nrow(poss), per_same) %>% 
  unlist()


clust_grp <- pvclust::pvpick(hcd_FAD$s1_A_max)$clusters %>% 
  enframe() %>% 
  unnest

poss %>% 
  mutate(equal = tmp_res_same) %>% 
  left_join(., clust_grp, by = c("V1" = "value")) %>% 
  mutate(clust = paste0("Cluster - ", name)) %>% 
  #mutate(V1 = factor(V1, levels = aaa %>% pull(value))) %>% 
  #mutate(V2 = factor(V2, levels = aaa %>% pull(value))) %>% 
  ggplot(aes(x= V1, y = V2, fill = equal))+
  geom_tile()

clust_grp %>% 
  group_by(name) %>% 
  expand.grid()

cx_1 <- clust_grp %>% 
  group_by(name) %>% 
  filter(name == 1) %>% 
  pull(value) %>% 
  combinations(2) %>% 
  as_tibble() %>% 
  mutate(cluster = 1)

poss %>% 
  mutate(equal = tmp_res_same) %>% 
  left_join(., cx_1, by = c("V1"= "V1", "V2"= "V2")) %>% 
  mutate(clust = paste0("Cluster - ", cluster)) %>% 
  ggplot(aes(x= V1, y = V2, fill = equal))+
  geom_tile(aes(color = clust))
