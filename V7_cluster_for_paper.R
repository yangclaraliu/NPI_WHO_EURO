#Version for paper 

# Cluster analysis


# Load data 
joined <- readRDS("data/joined_all_V6.RDS") 

joined$policy_dic

lab_plot <- joined %>% 
  .[[c("policy_dic")]] %>% 
  mutate(lab = if_else(lab == "Vaccine coverage - all population (OWIN)", "Vaccine coverage", lab))

# Load in HCA data 
# Wild type virus
hcd_W

#Alpha and Delta
hcd_FAD


# Wild type
plot(hcd_W$s1_W_mid)
pvrect(hcd_W$s1_W_mid, alpha=0.95, max.only = FALSE)

# Alpha 
plot(hcd_FAD$s1_A_mid)
pvrect(hcd_FAD$s1_A_mid, alpha=0.95, max.only = FALSE)

# Delta
plot(hcd_FAD$s1_D_mid)
pvrect(hcd_FAD$s1_D_mid, alpha=0.95 , max.only = FALSE)

# Full number of countries, no age stratification
policy_raw_all <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6","V_all_adj")

var_remove <- c("country", "cnt", "date","V_all_adj", "V_18_60_adj", "V_60_adj", "V_tot_adj", "median", "lower_90", "upper_90", "lower_50", "upper_50")

# wild
list_w <- joined %>%
  .[c("s1_W_mid")] %>%
  map(ungroup) %>%
  map(dplyr::select, -var_remove) %>% 
  map(t) %>% 
  map(dist, method = "euclidean") %>% 
  map(hclust, method = "ward.D2") %>% 
  map(dendro_data)

# full alpha and delta
list_FAD <- joined %>%
  .[c("s1_A_mid","s1_D_mid")] %>%
  map(ungroup) %>%
  map(dplyr::select, policy_raw_all) %>% 
  map(t) %>% 
  map(dist, method = "euclidean") %>% 
  map(hclust, method = "ward.D2") %>% 
  map(dendro_data)

# List of all age clustering 
all_age_dend <- list(list_w[[1]], list_FAD[[1]], list_FAD[[2]])


# Wild type, multi-level effort
plot(hcd_W$s1_W_mid)
pvrect(hcd_W$s1_W_mid, alpha=0.95)

# Wild type virus plot
W_plot <- ggplot()+
  geom_hline(yintercept = c(0, 50, 100, 150),
             linetype = 3) +
  geom_segment(data =  all_age_dend[[1]]$segments,
               aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend)) +
  geom_point(data = all_age_dend[[1]]$labels %>% 
               left_join(lab_plot, 
                         by = c("label" = "policy_code")),
             aes(x = x,
                 y = y,
                 color = cat)) +
  geom_label(data = all_age_dend[[1]]$labels %>% 
               left_join(lab_plot, 
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
  geom_rect(data = data.frame(xmin = c(3.65, 0.25),
                              xmax = c(14.75, 3.45),
                              ymin = c(-95, -95),
                              ymax = c(78, 56)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "red",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 125), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75, 100), 
                x = -0.2, 
                label = c("0","50","\nHeight","100")), 
            size = 5)+
  coord_flip() +
  scale_y_reverse(limits = c(125,-100),
                  breaks = c(150,100,50,0))+
  guides(color = guide_legend(override.aes = aes(size = 4)))+
  theme_cowplot() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_color_manual(values = c('#66c2a5',
                                '#fc8d62',
                                '#8da0cb',
                                '#e78ac3',
                                '#a6d854'))+
  labs(title = "Wild type virus",
       color = " ",
       y = "Height")


## Alpha plot
A_plot <- ggplot()+
  geom_hline(yintercept = c(0, 50, 100, 150),
             linetype = 3) +
  geom_segment(data =  all_age_dend[[2]]$segments,
               aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend)) +
  geom_point(data = all_age_dend[[2]]$labels %>% 
               left_join(lab_plot, 
                         by = c("label" = "policy_code")),
             aes(x = x,
                 y = y,
                 color = cat)) +
  geom_label(data = all_age_dend[[2]]$labels %>% 
               left_join(lab_plot, 
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
  geom_rect(data = data.frame(xmin = c(4.65, 0.25),
                              xmax = c(15.75, 4.45),
                              ymin = c(-95, -95),
                              ymax = c(63, 61)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "red",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 125), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75, 100), 
                x = -0.2, 
                label = c("0","50","\nHeight","100")), 
            size = 5)+
  coord_flip() +
  scale_y_reverse(limits = c(125,-100),
                  breaks = c(150,100,50,0))+
  guides(color = guide_legend(override.aes = aes(size = 4)))+
  theme_cowplot() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_color_manual(values = c('#66c2a5',
                                '#fc8d62',
                                '#8da0cb',
                                '#e78ac3',
                                '#a6d854'))+
  labs(title = "Alpha variant",
       color = " ",
       y = "Height")

# Delta plot
D_plot <- ggplot()+
  geom_hline(yintercept = c(0, 50, 100),
             linetype = 3) +
  geom_segment(data =  all_age_dend[[3]]$segments,
               aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend)) +
  geom_point(data = all_age_dend[[3]]$labels %>% 
               left_join(lab_plot, 
                         by = c("label" = "policy_code")),
             aes(x = x,
                 y = y,
                 color = cat)) +
  geom_label(data = all_age_dend[[3]]$labels %>% 
               left_join(lab_plot, 
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
  geom_rect(data = data.frame(xmin = c(3.65, 0.25),
                              xmax = c(15.75, 3.45),
                              ymin = c(-40, -40),
                              ymax = c(72, 39)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "red",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 125), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75, 100), 
                x = -0.2, 
                label = c("0","50","\nHeight","100")), 
            size = 5)+
  coord_flip() +
  scale_y_reverse(limits = c(125,-70),
                  breaks = c(150,100,50,0))+
  guides(color = guide_legend(override.aes = aes(size = 4)))+
  theme_cowplot() +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_color_manual(values = c('#66c2a5',
                                '#fc8d62',
                                '#8da0cb',
                                '#e78ac3',
                                '#a6d854'))+
  labs(title = "Delta variant",
       color = " ",
       y = "Height")



### delta big 

p1 <- plot_grid(
  W_plot + theme(legend.position = "none"),
  A_plot + theme(legend.position = "none"),
  labels = c("A", "B"),
  ncol = 2, 
  rel_heights = c(1, 1),
  align = "v",
  axis = "b")

p2 <- plot_grid(
  D_plot,
  labels = c("C"),
  ncol = 1, 
  rel_heights = c(1, 1),
  align = "v",
  axis = "b")

plot_grid(
  p1,
  NULL,
  p2,
  ncol = 1, 
  rel_heights = c(0.5,0.05, 0.5),
  align = "hv",
  axis = "b")

ggsave("figs/EURO_V6/combined_cluster.png",
       width = 18,
       height = 18)  
