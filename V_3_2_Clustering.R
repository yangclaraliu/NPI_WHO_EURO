#if(!exists("joined")) joined <- here("data", "joined_all.RDS") %>% readRDS

joined <- readRDS("data/joined_all_V1.RDS")

policy_raw <- joined$hi %>% 
  colnames %>% 
  str_subset("[A-Z][0-9]") %>%
  .[!.%in% c("M1","E3","E4","H4","H5")] # remove NPIs not considered

# [START]
# this needs to be ran only once, and it may take some time
# this segment conducts the temporal clustering analyses.
hcd <-  joined %>%
  .[c("hi")] %>%
  map(ungroup) %>%
  map(dplyr::select, policy_raw) %>%
  map(mutate_all, as.character) %>%
  map(mutate_all, as.numeric) %>%
  
  # this specific line below is for calculating the statistical significance of 
  # temporal clusters based on bootstrapping.
  
  map(pvclust::pvclust,
      method.hclust = "ward.D2",
      method.dist = "euclidean",
      nboot = 10000)
# 
# save(hcd, file = "results/hcd.rdata")
# [END]

#load("results/hcd.rdata")

# raw results from temporal clustering
hcd_raw <- joined$hi %>% 
  ungroup() %>% 
  select(policy_raw) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# extract cluster characteristics for the dendrograms
hcd_data <- hcd_raw %>% 
  map(dendro_data) 

# this is the significance boxes - quite manual
sig_boxes <- list()
sig_boxes[[1]] <- data.frame(xmin = c(4.5, 0.5),
                             xmax = c(13.5, 3.5),
                             ymin = c(-7.5, -7.5),
                             ymax = c(80, 47.5))
p_max <- hclust_plot_max()
p_max

sig_boxes[[2]] <- data.frame(xmin = c(0.45, 8.55),
                             xmax = c(8.45, 13.55),
                             ymin = c(-99, -99),
                             ymax = c(270, 310))
p_mid <- hclust_plot_mid()
p_mid

sig_boxes[[3]] <- data.frame(xmin = c(0.45, 4.55),
                             xmax = c(4.45, 13.55),
                             ymin = c(-54, -54),
                             ymax = c(85, 118))
p_any <- hclust_plot_any()
p_any

fig_dendogram <- ggpubr::ggarrange(p_any, p_max, ncol = 2, common.legend = T, legend = "bottom")

ggsave(filename = here("figs", "fig2_v2.png"),
       plot = fig_dendogram,
       width = 30,
       height = 10)
# 
ggsave(file = here("figs", "fig2_mid.png"),
       plot = p_mid,
       width = 20,
       height = 10)
##

## Max full

max_full <- joined$hi %>% 
  ungroup() %>% 
  select(policy_raw) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# Max original 
max_original <- joined$hi %>% 
  ungroup() %>% 
  filter(date <= as.Date("2020-11-30")) %>% 
  select(policy_raw) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# Max alpha 
max_alpha <- joined$hi %>% 
  ungroup() %>% 
  filter(date >= as.Date("2020-12-01") & date <= as.Date("2021-04-30"))%>% 
  select(policy_raw) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# Max delta 
max_delta <- joined$hi %>% 
  ungroup() %>% 
  filter(date >= as.Date("2021-05-01") & date <= as.Date("2021-09-30")) %>% 
  select(policy_raw) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

list(max_full, max_original, max_alpha, max_delta)

# Max full
ggplot()+
  geom_hline(yintercept = c(0, 50, 100, 150),
             linetype = 3) +
  geom_segment(data = max_full$segments,
               aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend)) +
  geom_point(data = max_full$labels %>% 
               left_join(joined$policy_dic, 
                         by = c("label" = "policy_code")),
             aes(x = x,
                 y = y,
                 color = cat)) +
  geom_label(data = max_full$labels %>% 
               left_join(joined$policy_dic, 
                         by = c("label" = "policy_code")),
             aes(x = x,
                 y = y,
                 label = lab,
                 color = cat),
             #hjust = -0.1,
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
  scale_y_reverse(limits = c(180,-48),
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
                                '#8856a7')) +
  labs(title = "Timing of Max. Efforts - Full Time Series",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_1/fig2_1_max_full.png",
       width = 15,
       height = 10)  

# Max Original
ggplot()+
  geom_hline(yintercept = c(0, 50, 100, 150),
             linetype = 3) +
  geom_segment(data = max_original$segments,
               aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend)) +
  geom_point(data = max_original$labels %>% 
               left_join(joined$policy_dic, 
                         by = c("label" = "policy_code")),
             aes(x = x,
                 y = y,
                 color = cat)) +
  geom_label(data = max_original$labels %>% 
               left_join(joined$policy_dic, 
                         by = c("label" = "policy_code")),
             aes(x = x,
                 y = y,
                 label = lab,
                 color = cat),
             #hjust = -0.1,
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
  scale_y_reverse(limits = c(180,-48),
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
                                '#8856a7')) +
  labs(title = "Timing of Max. Efforts - Original virus\n2020-01-01 to 2020-11-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_1/fig2_2_max_original.png",
       width = 15,
       height = 10)  


# Max Alpha
ggplot()+
  geom_hline(yintercept = c(0, 50, 100, 150),
             linetype = 3) +
  geom_segment(data = max_alpha$segments,
               aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend)) +
  geom_point(data = max_alpha$labels %>% 
               left_join(joined$policy_dic, 
                         by = c("label" = "policy_code")),
             aes(x = x,
                 y = y,
                 color = cat)) +
  geom_label(data = max_alpha$labels %>% 
               left_join(joined$policy_dic, 
                         by = c("label" = "policy_code")),
             aes(x = x,
                 y = y,
                 label = lab,
                 color = cat),
             #hjust = -0.1,
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
  scale_y_reverse(limits = c(180,-48),
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
                                '#8856a7')) +
  labs(title = "Timing of Max. Efforts - Alpha dominant period\n2020-12-01 to 2021-04-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_1/fig2_3_max_alpha.png",
       width = 15,
       height = 10)  


# Max Delta
ggplot()+
  geom_hline(yintercept = c(0, 50, 100, 150),
             linetype = 3) +
  geom_segment(data = max_delta$segments,
               aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend)) +
  geom_point(data = max_delta$labels %>% 
               left_join(joined$policy_dic, 
                         by = c("label" = "policy_code")),
             aes(x = x,
                 y = y,
                 color = cat)) +
  geom_label(data = max_delta$labels %>% 
               left_join(joined$policy_dic, 
                         by = c("label" = "policy_code")),
             aes(x = x,
                 y = y,
                 label = lab,
                 color = cat),
             #hjust = -0.1,
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
  scale_y_reverse(limits = c(180,-48),
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
                                '#8856a7')) +
  labs(title = "Timing of Max. Efforts - Delta dominant period\n2021-05-01 to 2021-09-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_1/fig2_4_max_delta.png",
       width = 15,
       height = 10)  



### 

ggplot()+
  geom_hline(yintercept = c(0, 50, 100, 150),
             linetype = 3) +
  geom_segment(data = max_delta$segments,
               aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend)) +
  geom_point(data = max_delta$labels %>% 
               left_join(joined$policy_dic, 
                         by = c("label" = "policy_code")),
             aes(x = x,
                 y = y,
                 color = cat)) +
  geom_label(data = max_delta$labels %>% 
               left_join(joined$policy_dic, 
                         by = c("label" = "policy_code")),
             aes(x = x,
                 y = y,
                 label = lab,
                 color = cat),
             #hjust = -0.1,
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
  scale_y_reverse(
    #limits = c(180,-48),
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
                                '#8856a7')) +
  labs(title = "Timing of Max. Efforts - Delta dominant period\n2021-05-01 to 2021-09-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_1/fig2_5_max_delta.png",
       width = 15,
       height = 10)  
