tab_list <- list()
for(i in 1:5){
  comix %>% 
    rename(cctld = country,
           contact_mean = mean,
           contact_ll = lci,
           contact_ul = uci) %>% 
    mutate(cctld = toupper(cctld)) %>% 
    # filter(setting == "All_genderage") %>% 
    left_join(., country_index, by = "cctld") %>% 
    left_join(., joined_list[[i]], by = c("iso3c", "date", "cctld")) %>% 
    rename(rt_mean = mean,
           rt_median = median) %>% 
    group_by(setting, country) %>% 
    mutate(n = n()) -> tab_list[[i]]
}

tab_list[[1]] %>% 
  filter(n > 90) %>%
  pull(cctld) %>% 
  unique -> cctld_to_keep

tab_list2 <- list()
for(t in 1:5){
  tab_list[[t]] %>% 
    filter(cctld %in% cctld_to_keep) %>% 
    mutate(phase = case_when(date < voc_switch_list[[t]]$date[1] ~ "wildtype",
                             date >= voc_switch_list[[t]]$date[1] & date < voc_switch_list[[t]]$date[2] ~ voc_switch_list[[t]]$voc_name_short[1],
                             date >= voc_switch_list[[t]]$date[2] & date < voc_switch_list[[t]]$date[3] ~ voc_switch_list[[t]]$voc_name_short[2],
                             date >= voc_switch_list[[t]]$date[3] ~ voc_switch_list[[t]]$voc_name_short[3]),
           phase = factor(phase,
                          levels = c("wildtype",
                                     "Alpha",
                                     "Delta",
                                     "Omicron"))) -> tab_list2[[t]]
}

formula_all_interaction <- paste0("contact_mean ~ ", paste0(paste0(policy_dic_V$policy_code, " * phase"), collapse = " + "), "+ setting  + iso3c") 

tab_list2 %>% 
  map(~glm(data = .,
           formula = formula_all_interaction)) -> model_all_interaction_list

model_all_interaction_list %>% 
  map(summary) %>% 
  map(~.[["coefficients"]]) %>% 
  map(data.frame) %>% 
  map(mutate, sig = `Pr...t..` <= 0.05) %>% 
  map(rownames_to_column) %>% 
  map(separate, rowname, into = c("rowname_1", "rowname_2"), sep = ":") %>% 
  map(mutate,
      park_test = grepl("phase", rowname_2),
      park = if_else(park_test == FALSE, rowname_1, rowname_2),
      park2 = if_else(park_test, rowname_1, rowname_2)) %>% 
  map(dplyr::select, -rowname_1, -rowname_2) %>% 
  map(mutate,
      park3 = substr(park2, 1, 2),
      nc = nchar(park2),
      park2 = substr(park2, 3, nc),
      park3_test = is.na(park3),
      park3 = if_else(is.na(park3), park, park3),
      park3 = if_else(park3 == "pr", "prop", park3)) %>% 
  map(dplyr::select,
      -park2,
      -park3_test,
      -nc,
      -park_test) -> model_coef_list

model_coef_list %>% 
  map(dplyr::filter, 
      park3 %in% policy_dic_V$policy_code) %>% 
  map(mutate,
      park = if_else(park %in% policy_dic_V$policy_code,
                     "phaseWildtype",
                     park),
      park = gsub("phase", "", park)) %>% 
  map(rename,
      phase = park,
      policy_code = park3) %>% 
  map(mutate,
      phase = factor(phase,
                     levels = c("Wildtype",
                                "Alpha",
                                "Delta",
                                "Omicron"))) %>% 
  map(rename,
      sd = `Std..Error`) %>% 
  map(left_join,
      policy_dic_V,
      by = "policy_code") %>% 
  map(mutate,
      mu = as.numeric(NA),
      LL = as.numeric(NA),
      UL = as.numeric(NA)) %>% 
  map(group_by, policy_code) %>% 
  map(group_split)  -> model_coef_policy_list_list

for(t in 1:5){
  for(j in 1:length(model_coef_policy_list_list[[t]])){
    for(i in 1:nrow(model_coef_policy_list_list[[t]][[j]])){
      if(i == 1) {
        model_coef_policy_list_list[[t]][[j]]$mu[1] <- model_coef_policy_list_list[[t]][[j]]$Estimate[1]
        model_coef_policy_list_list[[t]][[j]]$LL[1] <- model_coef_policy_list_list[[t]][[j]]$Estimate[1] - 1.96*model_coef_policy_list_list[[t]][[j]]$sd[1]
        model_coef_policy_list_list[[t]][[j]]$UL[1] <- model_coef_policy_list_list[[t]][[j]]$Estimate[1] + 1.96*model_coef_policy_list_list[[t]][[j]]$sd[1]
      } else {
        sample_baseline <- rnorm(5000, model_coef_policy_list_list[[t]][[j]]$Estimate[1], model_coef_policy_list_list[[t]][[j]]$sd[1])
        sample_target <- rnorm(5000, model_coef_policy_list_list[[t]][[j]]$Estimate[i], model_coef_policy_list_list[[t]][[j]]$sd[i])
        model_coef_policy_list_list[[t]][[j]]$mu[i] <- mean(sample_baseline + sample_target)
        model_coef_policy_list_list[[t]][[j]]$LL[i] <- quantile(sample_baseline + sample_target, 0.025)
        model_coef_policy_list_list[[t]][[j]]$UL[i] <- quantile(sample_baseline + sample_target, 0.975)
      }
    }
  }
}

model_coef_policy_list_list %>% 
  map(bind_rows) %>% 
  map(mutate,
      sig = if_else(LL*UL > 0, T, F),
      lab = factor(lab,
                   levels = policy_dic_V$lab)) -> p_tab_list

p_tab_list %>%
  map(dplyr::filter, policy_code != "prop") %>% 
  map(mutate, policy_code = factor(policy_code,
                                   levels = policy_dic$policy_code,
                                   labels = policy_dic$lab)) %>% 
  map(~ggplot(., 
              aes(x = phase, y = mu, shape = sig, color = cat)) +
                geom_point(size = 5)+
                geom_segment(aes(x = phase, xend = phase, y = LL, yend = UL))+
                facet_wrap(~policy_code, 
                           #scales = "free", 
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
                     y = "Effects")) -> p_list

legend_shape <- get_legend(p + theme(legend.position = "bottom", legend.box = "vertical") + guides(color = FALSE))
legend_color <- get_legend(p + theme(legend.position = "bottom", legend.box = "vertical") + guides(shape = FALSE,
                                                                                                   color=guide_legend(nrow = 2,
                                                                                                                      byrow = FALSE)))
plot_grid(legend_shape, NULL, legend_color, ncol = 1, rel_heights = c(1,-1,2)) -> p_legend

for(i in 1:5){
  plot_grid(p_legend, NULL, p_list[[i]], ncol = 1, rel_heights = c(1,-0.4,10)) -> p_save
  ggsave(paste0("figs/manuscript_fig5_0.",i,".png"),
         p_save,
         height = 18, width = 10)
  
}
