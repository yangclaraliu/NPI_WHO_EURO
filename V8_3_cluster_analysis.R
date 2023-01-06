# Script for cluster analysis
require(tidyverse)
require(pvclust)
require(factoextra)

# Read in data if not already done so...
# joined <- readRDS("data/joined_all_V8.RDS")
joined <- readRDS("data/joined_v9.rds")
names(joined)

# Remove vaccination from wild type scenarios, as none occurred

# Full number of countries, no age stratification
policy_raw_wild <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6")
policy_raw_all  <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6","V_all_adj")

# [START]
# this needs to be ran only once, and it may take some time
# this segment conducts the temporal clustering analyses.

# Wild type
hcd_W <- list()
set <- names(joined)

#### bootstrapping ####
# for(s in set){
# 
#   # find <- grepl("con",s) & (grepl("A|D",s))
#   tags_tmp <- policy_raw_wild
#   # if(find) tags_tmp <- policy_raw_all
# 
#   hcd_W[[s]] <-
#     joined %>%
#     .[!grepl("full", names(.))] %>%
#     .[grepl(s, names(.))] %>%
#     map(ungroup) %>%
#     map(dplyr::select, tags_tmp) %>%
#     map(pvclust::pvclust,
#         method.hclust = "ward.D2",
#         method.dist = "euclidean",
#         nboot = 5000,
#         parallel = T)
# 
#   print(s)
# }


# hcd_W %>% flatten() %>% write_rds(., "data/hcd_W.rds")

clust_bs <- read_rds("data/hcd_W.rds")

#### calculate the euclidean distance ####
distance_all <- list()

for(s in set[1:36]){

  # find <- grepl("con",s) & (grepl("A|D",s))
  tags_tmp <- policy_raw_wild
  # if(find) tags_tmp <- policy_raw_all
  # 
  distance_all[[s]] <- joined %>%
    .[!grepl("full", names(.))] %>% 
    .[grepl(s, names(.))] %>% 
    .[[1]] %>% 
    ungroup %>% 
    select(., tags_tmp) %>% 
    t %>% 
    get_dist(., method = "euclidean")
 
  print(s)
}

#### cut trees ####
distance_all %>% 
  map(hclust) %>% 
  map(cutree, h = 50) -> clust_pruned

distance_all %<>% 
  map(as.matrix) %>% 
  map(reshape2::melt) 

#### calculate the correlation ####
require(Hmisc)
corr_all <- list()

for(s in set[1:36]){
  
  # find <- grepl("con",s) & (grepl("A|D",s))
  tags_tmp <- policy_raw_wild
  # if(find) tags_tmp <- policy_raw_all
  
  corr_all[[s]] <- joined %>%
    .[!grepl("full", names(.))] %>% 
    .[grepl(s, names(.))] %>% 
    .[[1]] %>% 
    ungroup %>% 
    select(., tags_tmp) %>% 
    as.matrix %>% 
    rcorr %>% 
    .$r %>% 
    as.matrix %>% 
    reshape2::melt()
  
  print(s)
}

#### combine measures ####
lapply(1:length(distance_all),
       function(x){
         distance_all[[x]] %>% 
           rename(distance = value) %>% 
           left_join(corr_all[[x]] %>% 
                       rename(corr = value),
                     by = c("Var1", "Var2")) %>% 
           filter(Var1 != Var2) # %>% 
           # filter(corr >= 0.7)
       }) %>% 
  bind_rows() %>% 
  tibble %>% 
  ggplot(., aes(x = distance, y = corr)) +
  geom_point()


#### generate the rectangles  ####
# source("fun_draw_clust.R")
# clust_bs %>%
#   map(gen_rect) -> rect_all
# # 
# write_rds(rect_all, "data/rect_all.rds")

rect_all <- read_rds("data/rect_all.rds")

clust_bs %>%
  map(gen_rect,
      max.only = T) -> rect_all_max

write_rds(rect_all_max, "data/rect_all_max.rds")

#### draw dendrogram ####
require(ggdendro)

for(i in 1:36){
  dhc <- as.dendrogram(clust_bs[[i]]$hclust)
  ddata <- dendro_data(dhc, type = "rectangle")
  ddata$labels %<>% 
    rename(policy_code = label) %>% 
    left_join(policy_dic_V, by = "policy_code") %>% 
    rename(label = policy_name)
  
  
  ggplot(segment(ddata)) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
    coord_flip() + 
    scale_y_reverse(expand = expansion(mult = c(0.1, 0.6)), #c(0.2, 0.1), 
                    limits = c(max(segment(ddata)$y), 0)) + 
    geom_label(data = ddata$labels, 
               aes(x = x, y = y, 
                   fill = cat, 
                   label = label), 
               size = 4,
               # vjust = 0.5, 
               hjust = 0
    ) +
    geom_hline(yintercept = c(50), size = 1.1)  +
    geom_rect(data = rect_all[[i]] %>% 
                mutate(stage = if_else(top >= 50, T, F)),
              aes(xmin = left-0.1, xmax = right+0.1,
                  ymin = 0, ymax = top+1, color = stage),
              fill = NA, linetype = 2, size = 0.4)  +
    theme_bw() +
    scale_fill_manual(values = c('#e78ac3',
                                 '#66c2a5',
                                 '#8da0cb',
                                 '#a6d854',
                                 '#fc8d62')) +
    scale_color_manual(values = c("red", "orange")) +
    labs(x = "", y = "Height") +
    theme(legend.position = "none",
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) -> p
  
  # theme(legend.position = "none") 
  ggsave(filename = paste0("figs/intermediate/dendrograms/",
                           names(joined)[i],
                           ".png"),
         plot = p,
         width = 10, height = 6)
  
}

#                                 '#fc8d62',
#                                 '#a6d854'

# save(hcd_W, file = "results/hcd_V8_W.rds")
# load("results/hcd_V8_W.rdata")

# Full TS, Alpha and Delta variants
# hcd_FAD <-  joined %>%
#   .[c("s1_full_mid", "s1_A_mid","s1_D_mid",
#       "s1_full_max", "s1_A_max","s1_D_max")] %>%
#   map(ungroup) %>%
#   map(dplyr::select, policy_raw_all) %>%
#   map(pvclust::pvclust,
#       method.hclust = "ward.D2",
#       method.dist = "euclidean",
#       nboot = 1000)
# # 
# save(hcd_FAD, file = "results/hcd_V8_FAD.rdata")
# load("results/hcd_V8_FAD.rdata")
# [END]

# Wild type
# plot(hcd_W$s1_W_mid)
# pvrect(hcd_W$s1_W_mid, max.only = F)
# pvpick(hcd_W$s1_W_mid)
# 
# # Alpha 
# plot(hcd_FAD$s1_A_mid)
# pvrect(hcd_FAD$s1_A_mid, alpha=0.95, max.only = FALSE)

# Delta
# plot(hcd_FAD$s1_D_mid)
# pvrect(hcd_FAD$s1_D_mid, alpha=0.95 , max.only = FALSE)

#
# wild
# list_w <- joined %>%
#   .[c("s1_W_mid")] %>%
#   map(ungroup) %>%
#   map(dplyr::select, policy_raw_wild) %>% 
#   map(t) %>% 
#   map(dist, method = "euclidean") %>% 
#   map(hclust, method = "ward.D2") %>% 
#   map(dendro_data)
# 
# # full alpha and delta
# list_FAD <- joined %>%
#   .[c("s1_A_mid","s1_D_mid")] %>%
#   map(ungroup) %>%
#   map(dplyr::select, policy_raw_all) %>% 
#   map(t) %>% 
#   map(dist, method = "euclidean") %>% 
#   map(hclust, method = "ward.D2") %>% 
#   map(dendro_data)
# 
# # List of all age clustering 
# all_age_dend <- list(list_w[[1]], list_FAD[[1]], list_FAD[[2]])
# 
# lab_plot <- joined %>% 
#   .[[c("policy_dic")]] %>% 
#   mutate(lab = if_else(lab == "Vaccine coverage - all population", "Vaccine coverage", lab))


# Wild type virus plot
# W_plot <- ggplot()+
#   geom_hline(yintercept = c(0, 50, 100, 150),
#              linetype = 3) +
#   geom_segment(data =  all_age_dend[[1]]$segments,
#                aes(x = x,
#                    y = y,
#                    xend = xend,
#                    yend = yend)) +
#   geom_point(data = all_age_dend[[1]]$labels %>% 
#                left_join(lab_plot, 
#                          by = c("label" = "policy_code")),
#              aes(x = x,
#                  y = y,
#                  color = cat)) +
#   geom_label(data = all_age_dend[[1]]$labels %>% 
#                left_join(lab_plot, 
#                          by = c("label" = "policy_code")),
#              aes(x = x,
#                  y = y,
#                  label = lab,
#                  color = cat),
#              hjust = -0.01,
#              size = 5,
#              label.padding = unit(0.55, "lines"),
#              label.size = 1,
#              show.legend = F) +
#   geom_rect(data = data.frame(xmin = c(3.65, 0.25),
#                               xmax = c(14.75, 3.45),
#                               ymin = c(-95, -95),
#                               ymax = c(78, 56)),
#             aes(xmin = xmin,
#                 xmax = xmax,
#                 ymin = ymin,
#                 ymax = ymax),
#             fill = NA,
#             color = "red",
#             size = 1.5,
#             linetype = 2) +
#   geom_segment(aes(x = 0, xend = 0, y = 0, yend = 125), size = 1.5) +
#   geom_text(aes(y = c(0, 50, 75, 100), 
#                 x = -0.2, 
#                 label = c("0","50","\nHeight","100")), 
#             size = 5)+
#   coord_flip() +
#   scale_y_reverse(limits = c(125,-100),
#                   breaks = c(150,100,50,0))+
#   guides(color = guide_legend(override.aes = aes(size = 4)))+
#   theme_cowplot() +
#   theme(plot.title = element_text(size = 20,
#                                   hjust = 0.5),
#         legend.key = element_rect(fill = "white"),
#         legend.text = element_text(size = 12),
#         legend.position = "bottom",
#         axis.line = element_blank(),
#         axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank()) +
#   scale_color_manual(values = c('#66c2a5',
#                                 '#fc8d62',
#                                 '#8da0cb',
#                                 '#e78ac3',
#                                 '#a6d854'))+
#   labs(title = "Wild type virus",
#        color = " ",
#        y = "Height")
# 
# ## Alpha plot
# A_plot <- ggplot()+
#   geom_hline(yintercept = c(0, 50, 100, 150),
#              linetype = 3) +
#   geom_segment(data =  all_age_dend[[2]]$segments,
#                aes(x = x,
#                    y = y,
#                    xend = xend,
#                    yend = yend)) +
#   geom_point(data = all_age_dend[[2]]$labels %>% 
#                left_join(lab_plot, 
#                          by = c("label" = "policy_code")),
#              aes(x = x,
#                  y = y,
#                  color = cat)) +
#   geom_label(data = all_age_dend[[2]]$labels %>% 
#                left_join(lab_plot, 
#                          by = c("label" = "policy_code")),
#              aes(x = x,
#                  y = y,
#                  label = lab,
#                  color = cat),
#              hjust = -0.01,
#              size = 5,
#              label.padding = unit(0.55, "lines"),
#              label.size = 1,
#              show.legend = F) +
#   geom_rect(data = data.frame(xmin = c(4.65, 0.25),
#                               xmax = c(15.75, 4.45),
#                               ymin = c(-95, -95),
#                               ymax = c(63, 61)),
#             aes(xmin = xmin,
#                 xmax = xmax,
#                 ymin = ymin,
#                 ymax = ymax),
#             fill = NA,
#             color = "red",
#             size = 1.5,
#             linetype = 2) +
#   geom_segment(aes(x = 0, xend = 0, y = 0, yend = 125), size = 1.5) +
#   geom_text(aes(y = c(0, 50, 75, 100), 
#                 x = -0.2, 
#                 label = c("0","50","\nHeight","100")), 
#             size = 5)+
#   coord_flip() +
#   scale_y_reverse(limits = c(125,-100),
#                   breaks = c(150,100,50,0))+
#   guides(color = guide_legend(override.aes = aes(size = 4)))+
#   theme_cowplot() +
#   theme(plot.title = element_text(size = 20,
#                                   hjust = 0.5),
#         legend.key = element_rect(fill = "white"),
#         legend.text = element_text(size = 12),
#         legend.position = "bottom",
#         axis.line = element_blank(),
#         axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank()) +
#   scale_color_manual(values = c('#66c2a5',
#                                 '#fc8d62',
#                                 '#8da0cb',
#                                 '#e78ac3',
#                                 '#a6d854'))+
#   labs(title = "Alpha variant",
#        color = " ",
#        y = "Height")
# 
# # Delta plot
# D_plot <- ggplot()+
#   geom_hline(yintercept = c(0, 50, 100),
#              linetype = 3) +
#   geom_segment(data =  all_age_dend[[3]]$segments,
#                aes(x = x,
#                    y = y,
#                    xend = xend,
#                    yend = yend)) +
#   geom_point(data = all_age_dend[[3]]$labels %>% 
#                left_join(lab_plot, 
#                          by = c("label" = "policy_code")),
#              aes(x = x,
#                  y = y,
#                  color = cat)) +
#   geom_label(data = all_age_dend[[3]]$labels %>% 
#                left_join(lab_plot, 
#                          by = c("label" = "policy_code")),
#              aes(x = x,
#                  y = y,
#                  label = lab,
#                  color = cat),
#              hjust = -0.01,
#              size = 5,
#              label.padding = unit(0.55, "lines"),
#              label.size = 1,
#              show.legend = F) +
#   geom_rect(data = data.frame(xmin = c(3.65, 0.25),
#                               xmax = c(15.75, 3.45),
#                               ymin = c(-40, -40),
#                               ymax = c(72, 39)),
#             aes(xmin = xmin,
#                 xmax = xmax,
#                 ymin = ymin,
#                 ymax = ymax),
#             fill = NA,
#             color = "red",
#             size = 1.5,
#             linetype = 2) +
#   geom_segment(aes(x = 0, xend = 0, y = 0, yend = 125), size = 1.5) +
#   geom_text(aes(y = c(0, 50, 75, 100), 
#                 x = -0.2, 
#                 label = c("0","50","\nHeight","100")), 
#             size = 5)+
#   coord_flip() +
#   scale_y_reverse(limits = c(125,-70),
#                   breaks = c(150,100,50,0))+
#   guides(color = guide_legend(override.aes = aes(size = 4)))+
#   theme_cowplot() +
#   theme(plot.title = element_text(size = 20,
#                                   hjust = 0.5),
#         legend.key = element_rect(fill = "white"),
#         legend.text = element_text(size = 20),
#         legend.position = "bottom",
#         axis.line = element_blank(),
#         axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank()) +
#   scale_color_manual(values = c('#66c2a5',
#                                 '#fc8d62',
#                                 '#8da0cb',
#                                 '#e78ac3',
#                                 '#a6d854'))+
#   labs(title = "Delta variant",
#        color = " ",
#        y = "Height")
# 
# 
# 
# ### Combined plot with Delta large
# p1 <- plot_grid(
#   W_plot + theme(legend.position = "none"),
#   A_plot + theme(legend.position = "none"),
#   labels = c("A", "B"),
#   ncol = 2, 
#   rel_heights = c(1, 1),
#   align = "v",
#   axis = "b")
# 
# p2 <- plot_grid(
#   D_plot,
#   labels = c("C"),
#   ncol = 1, 
#   rel_heights = c(1, 1),
#   align = "v",
#   axis = "b")
# 
# plot_grid(
#   p1,
#   NULL,
#   p2,
#   ncol = 1, 
#   rel_heights = c(0.5,0.05, 0.5),
#   align = "hv",
#   axis = "b")
# 
# ggsave("figs/figs/fig3.png",
#        width = 18,
#        height = 18)  
