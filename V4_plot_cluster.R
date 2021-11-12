
## All age vaccine coverage rates plot cluster 
plot_dend_all_age <- function(i){
 
  ggplot()+
    geom_hline(yintercept = c(0, 50, 100, 150),
               linetype = 3) +
    geom_segment(data =  all_age_dend[[i]]$segments,
                 aes(x = x,
                     y = y,
                     xend = xend,
                     yend = yend)) +
    geom_point(data = all_age_dend[[i]]$labels %>% 
                 left_join(joined$policy_dic, 
                           by = c("label" = "policy_code")),
               aes(x = x,
                   y = y,
                   color = cat)) +
    geom_label(data = all_age_dend[[i]]$labels %>% 
                 left_join(joined$policy_dic, 
                           by = c("label" = "policy_code")),
               aes(x = x,
                   y = y,
                   label = lab,
                   color = cat),
               hjust = -0.01,
               size = 5,
               label.padding = unit(0.55, "lines"),
               label.size = 1,
               show.legend = F) +
    #    geom_rect(data = sig_boxes[[1]],
    #              aes(xmin = xmin,
    #                  xmax = xmax,
    #                  ymin = ymin,
    #                  ymax = ymax),
    #              fill = NA,
    #              color = "firebrick",
    #              size = 1.5,
    #              linetype = 2) +
    # geom_text(data = data.frame(x = 13.55,
    #                             y = 130,
  #                             label = "Timing of \nMaximum Efforts"),
  #           aes(x = x, y = y, label = label),
  #           size = 10) +
  coord_flip() +
    scale_y_reverse(limits = c(180,-70),
                    breaks = c(150,100,50,0))+
    guides(color = guide_legend(override.aes = aes(size = 4)))+
    theme_cowplot() +
    theme(plot.title = element_text(size = 30, 
                                    face = "italic",
                                    hjust = 0.5),
          # panel.border = element_rect(color = "black",
          #                             fill = NA),
          legend.key = element_rect(fill = "white"),
          legend.text = element_text(size = 12),
          legend.position = "bottom",
          axis.line = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    # theme_dendro() +
    scale_color_manual(values = c('#a6cee3',
                                  '#1f78b4',
                                  '#b2df8a',
                                  '#33a02c',
                                  '#8856a7')) 
}


## Age specific vaccine coverage rates 
plot_dend_age <- function(i){
  
  ggplot()+
    geom_hline(yintercept = c(0, 50, 100, 150),
               linetype = 3) +
    geom_segment(data =  age_dend[[i]]$segments,
                 aes(x = x,
                     y = y,
                     xend = xend,
                     yend = yend)) +
    geom_point(data = age_dend[[i]]$labels %>% 
                 left_join(joined$policy_dic, 
                           by = c("label" = "policy_code")),
               aes(x = x,
                   y = y,
                   color = cat)) +
    geom_label(data = age_dend[[i]]$labels %>% 
                 left_join(joined$policy_dic, 
                           by = c("label" = "policy_code")),
               aes(x = x,
                   y = y,
                   label = lab,
                   color = cat),
               hjust = -0.01, 
               size = 5,
               label.padding = unit(0.55, "lines"),
               label.size = 1,
               show.legend = F) +
    #    geom_rect(data = sig_boxes[[1]],
    #              aes(xmin = xmin,
    #                  xmax = xmax,
    #                  ymin = ymin,
    #                  ymax = ymax),
    #              fill = NA,
    #              color = "firebrick",
    #              size = 1.5,
    #              linetype = 2) +
    # geom_text(data = data.frame(x = 13.55,
    #                             y = 130,
  #                             label = "Timing of \nMaximum Efforts"),
  #           aes(x = x, y = y, label = label),
  #           size = 10) +
  coord_flip() +
    scale_y_reverse(limits = c(180,-70),
                    breaks = c(150,100,50,0))+
    guides(color = guide_legend(override.aes = aes(size = 4)))+
    theme_cowplot() +
    theme(plot.title = element_text(size = 30, 
                                    face = "italic",
                                    hjust = 0.5),
          # panel.border = element_rect(color = "black",
          #                             fill = NA),
          legend.key = element_rect(fill = "white"),
          legend.text = element_text(size = 12),
          legend.position = "bottom",
          axis.line = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    # theme_dendro() +
    scale_color_manual(values = c('#a6cee3',
                                  '#1f78b4',
                                  '#b2df8a',
                                  '#33a02c',
                                  '#8856a7')) 
}
