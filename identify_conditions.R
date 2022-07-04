
# condition 1 = are they included in the variable selection process?

joined[1:36] %>% 
  map(colnames) -> names_all

lapply(1:36, function(x) names_all[[x]][which(names_all[[x]] %in% policy_raw_discrete)]) %>% 
  map(enframe) %>% 
  setNames(names(joined[1:36])) %>% 
  bind_rows(.id = "scenario") %>% 
  dplyr::select(-name) %>% 
  mutate(VS_include = 1) %>% 
  pivot_wider(names_from = value, values_from = VS_include) %>% 
  replace(., is.na(.), 0) %>% 
  pivot_longer(cols = policy_raw_discrete,
               names_to = "policy_code",
               values_to = "VS_include") -> condition_1

# condition 2 = if they are in the variable selection process, were they selected?
effect_data %>% 
  filter(criterion == "AIC",
         var != "V_all_adj") %>% 
  mutate(var_new2 = if_else(var_nchar == 3, substr(var,1,2), var)) %>% 
  dplyr::select(var_new2, scen, optim_lag) %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = var_new2, values_from = value) %>% 
replace(., is.na(.), 0) %>% 
  pivot_longer(cols = policy_raw_discrete,
               names_to = "policy_code",
               values_to = "VS_select") -> condition_2
