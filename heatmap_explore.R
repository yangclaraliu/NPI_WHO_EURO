
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

ox_var <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6")

poss <- expand.grid(ox_var, ox_var) %>% 
  mutate(Var1= as.character(Var1), Var2 = as.character(Var2)) %>% 
  mutate(same = if_else(Var1 == Var2, T, F)) %>% 
  filter(same != TRUE) %>% 
  select(Var1, Var2)

per_same <- function(i){
  
joined_max_S1_W %>% 
        select(-var_remove) %>% 
        select(poss[i,1], poss[i,2]) %>% 
        rowSums() %>% 
  enframe() %>% 
  mutate(ha = if_else(value %in% c(0,2), TRUE, FALSE)) %>% 
  mutate(total = length(ha)) %>% 
  group_by(ha) %>% 
  mutate(sum = sum(ha)) %>% 
  mutate(pro = sum/total) %>% 
  ungroup() %>% 
  select(pro) %>% 
  first() %>% 
  pull()

}

tmp_res_same <- map(1:nrow(poss), per_same) %>% 
  unlist()


poss %>% 
  mutate(equal = tmp_res_same) %>% 
  ggplot(aes(x= Var1, y = Var2, fill = equal))+
  geom_tile()

joined_max_S1_W %>% 
  select(-var_remove) %>% 
  select(C1, C2) %>% 
  mutate(equal = if_else(C1 == C2, 1, 0))

joined_max_S1_W %>% 
  select(-var_remove) %>% 
  select(C1, C2) %>% 
  group_by(C1, C2) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
