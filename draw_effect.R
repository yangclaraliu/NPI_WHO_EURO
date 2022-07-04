draw_effect <- function(type_tmp = NULL, # "Any Effort", "Multi-level" OR "Max. Effort"
                        phase_df = "s1", # s1, s2, s3, OR s4
                        criterion_tmp = "AIC",# AIC or BIC
                        data = effect_data,
                        delay_tmp = c("28 days"),
                        fn = NULL# OR 1 day OR 28 days
){
  cat_tmp <- policy_dic$cat %>% unique
  cat_color <- c('#66c2a5',
                 '#fc8d62',
                 '#8da0cb')
  data %>% 
    filter(scen_phase == phase_df,
           criterion == criterion_tmp,
           optim_lag %in% delay_tmp,
           scen_val == type_tmp) -> p_tab 
  
  draw_seg <- function(m){
    p_tab %>% 
      filter(cat %in% cat_tmp[m]) %>% 
      mutate(var_new = factor(var_new,
                              levels = policy_dic %>% filter(cat %in% cat_tmp[m]) %>% pull(lab))) %>% 
      ggplot(., aes(x = virus, y = estimate)) +
      geom_pointrange(aes(x = virus,
                          y = estimate,
                          ymin = x,
                          ymax = xend,
                          color = cat,
                          shape = stat_sig),
                      size = 0.5) +
      facet_wrap(~var_new, ncol = 1, drop = F) +
      scale_color_manual(breaks = cat_tmp,
                         values = cat_color)  +
      scale_shape_manual(values = c(16, 1, 13)) +
      theme_bw()+
      geom_hline(yintercept = 0, color = "black", linetype = 2) +
      theme(panel.grid = element_blank(),
            legend.position = "none",
            legend.title = element_text(size = 12),
            strip.background = element_rect(fill = NA),
            axis.text.x = element_text(vjust = 0.5,
                                       angle = 90,
                                       hjust = 1),
            axis.text    = element_text(size = 12),
            axis.title = element_text(size = 15),
            legend.text  = element_text(size = 8),
            strip.text = element_text(size = 12)) +
      labs(y = "Effect on Rt",
           x = "VoC Phase",
           color = "PHSM category",
           shape = "Effect Type",
           title = "") -> p_tmp
    return(p_tmp)
  }

  p <- list()
  
  p[[1]] <- draw_seg(1)
  p[[2]] <- draw_seg(2:3)
  p[[3]] <- draw_seg(1:3)
  l <- get_legend(p[[3]] + theme(legend.position = "right"))
  p_low <- plot_grid(p[[2]], l, rel_heights =  c(7,2), ncol = 1)
  
  plot_grid(p[[1]], 
            p_low, 
            ncol = 2) -> p_tot
  
  ggsave(plot = p_tot,
         filename = fn,
         height = 12, width = 7)
  
}

CJ(type_tmp = c("Any Effort", "Multi-level", "Max. Effort"),
   phase = paste0("s",1:4),
   criterion = c("AIC","BIC"),
   delay_tmp = c("1 day", "14 days", "28 days")) %>% 
  mutate(fn = paste0("figs/intermediate/draw_effect/",substr(type_tmp,1,3), "_",
                     phase, "_",
                     criterion, "_",
                     substr(delay_tmp,1,2),
                     ".png")) -> draw_list

for(i in 1:nrow(draw_list)){
  draw_effect(type_tmp = draw_list$type_tmp[i],
              phase_df = draw_list$phase[i],
              criterion_tmp = draw_list$criterion[i],
              data = effect_data,
              delay_tmp = draw_list$delay_tmp[i],
              fn = draw_list$fn[i])
}
