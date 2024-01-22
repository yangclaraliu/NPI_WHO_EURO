read_rds(paste0(path_data, "hcd_con.rds")) %>% 
  setNames(as.character(seq(0.1,0.5,0.1))) %>% 
  map(~setNames(., c("wildtype", "Alpha", "Delta", "Omicron", "full")))-> clust_bs

read_rds(paste0(path_data, "distance_con.rds")) %>% 
  setNames(as.character(seq(0.1,0.5,0.1))) %>% 
  map(~setNames(., c("wildtype", "Alpha", "Delta", "Omicron", "full")))-> distance_all

read_rds(paste0(path_data, "corr_con.rds")) %>% 
  setNames(as.character(seq(0.1,0.5,0.1))) %>% 
  map(~setNames(., c("wildtype", "Alpha", "Delta", "Omicron", "full")))-> corr_all

policy_dic_V %>% 
  ggplot(., aes(x = policy_code, y = policy_max, color = cat, fill = cat)) +
  geom_box() +
  scale_color_manual(values = c('#e78ac3','#66c2a5','#8da0cb','#a6d854'))+
  scale_fill_manual(values = c('#e78ac3','#66c2a5','#8da0cb','#a6d854')) +
  labs(color = "", fill = "") +
  theme(legend.position = "top",
        legend.text = element_text(size = 14)) -> tmp
p_legend <- get_legend(tmp)
rm(tmp)

corr_all %>% 
  map(~map(., filter, Var1 != Var2 & value > 0.3)) %>% 
  map(~map(., nrow))

pruning_threshold <- c(50, 25, 25, 50)
clust_pruned <- list()
clust_pruned[["0.1"]] <- clust_pruned[["0.2"]] <- clust_pruned[["0.3"]] <- clust_pruned[["0.4"]] <- clust_pruned[["0.5"]] <- list()
clust_pruned[["0.1"]][["wildtype"]] <- cutree(hclust(distance_all$`0.1`$wildtype), h = pruning_threshold[1])
clust_pruned[["0.2"]][["wildtype"]] <- cutree(hclust(distance_all$`0.2`$wildtype), h = pruning_threshold[1])
clust_pruned[["0.3"]][["wildtype"]] <- cutree(hclust(distance_all$`0.3`$wildtype), h = pruning_threshold[1])
clust_pruned[["0.4"]][["wildtype"]] <- cutree(hclust(distance_all$`0.4`$wildtype), h = pruning_threshold[1])
clust_pruned[["0.5"]][["wildtype"]] <- cutree(hclust(distance_all$`0.5`$wildtype), h = pruning_threshold[1])

clust_pruned[["0.1"]][["Alpha"]] <- cutree(hclust(distance_all$`0.1`$Alpha), h = pruning_threshold[2])
clust_pruned[["0.2"]][["Alpha"]] <- cutree(hclust(distance_all$`0.2`$Alpha), h = pruning_threshold[2])
clust_pruned[["0.3"]][["Alpha"]] <- cutree(hclust(distance_all$`0.3`$Alpha), h = pruning_threshold[2])
clust_pruned[["0.4"]][["Alpha"]] <- cutree(hclust(distance_all$`0.4`$Alpha), h = pruning_threshold[2])
clust_pruned[["0.5"]][["Alpha"]] <- cutree(hclust(distance_all$`0.5`$Alpha), h = pruning_threshold[2])

clust_pruned[["0.1"]][["Delta"]] <- cutree(hclust(distance_all$`0.1`$Delta), h = pruning_threshold[3])
clust_pruned[["0.2"]][["Delta"]] <- cutree(hclust(distance_all$`0.2`$Delta), h = pruning_threshold[3])
clust_pruned[["0.3"]][["Delta"]] <- cutree(hclust(distance_all$`0.3`$Delta), h = pruning_threshold[3])
clust_pruned[["0.4"]][["Delta"]] <- cutree(hclust(distance_all$`0.4`$Delta), h = pruning_threshold[3])
clust_pruned[["0.5"]][["Delta"]] <- cutree(hclust(distance_all$`0.5`$Delta), h = pruning_threshold[3])

clust_pruned[["0.1"]][["Omicron"]] <- cutree(hclust(distance_all$`0.1`$Omicron), h = pruning_threshold[4])
clust_pruned[["0.2"]][["Omicron"]] <- cutree(hclust(distance_all$`0.2`$Omicron), h = pruning_threshold[4])
clust_pruned[["0.3"]][["Omicron"]] <- cutree(hclust(distance_all$`0.3`$Omicron), h = pruning_threshold[4])
clust_pruned[["0.4"]][["Omicron"]] <- cutree(hclust(distance_all$`0.4`$Omicron), h = pruning_threshold[4])
clust_pruned[["0.5"]][["Omicron"]] <- cutree(hclust(distance_all$`0.5`$Omicron), h = pruning_threshold[4])

distance_melt <- list()
distance_all %>% 
  map(~map(., as.matrix)) %>% 
  map(~map(., reshape2::melt)) -> distance_melt[["con"]] 

source("code/util/fun_draw_clust.R")

clust_bs %>% 
  map(~map(., gen_rect)) -> rect_all

draw_cluster <- function(threshold = "0.3", phase = "wildtype", pt = 50){

  dhc <- as.dendrogram(clust_bs[[threshold]][[phase]]$hclust)
  ddata <- dendro_data(dhc, type = "rectangle")
  ddata$labels %<>% 
    rename(policy_code = label) %>% 
    left_join(policy_dic_V, by = "policy_code") %>% 
    rename(label = policy_name)

  ggplot(segment(ddata)) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
    coord_flip() + 
    scale_y_reverse(expand = expansion(mult = c(0.1, 0.6)), #c(0.2, 0.1), 
                    limits = c(max(segment(ddata)$y), 0),
                    breaks = c(0:floor(max(segment(ddata)$y)/50))*50) + 
    geom_label(data = ddata$labels, 
               aes(x = x, y = y, 
                   fill = cat, 
                   label = label), 
               size = 4,
               # vjust = 0.5, 
               hjust = 0
    ) +
    geom_hline(yintercept = pt, size = 1.1)  +
    geom_rect(data = rect_all[[threshold]][[phase]] %>% 
                mutate(stage = if_else(top >= pt, T, F)) %>% 
                filter(stage == F),
              aes(xmin = left-0.1, xmax = right+0.1,
                  ymin = 0, ymax = top+1, color = stage),
              fill = NA, linetype = 2, size = 0.4)  +
    theme_bw() +
    scale_fill_manual(values = c('#e78ac3','#66c2a5','#8da0cb','#a6d854','#fc8d62')) +
    scale_color_manual(values = c("red", "orange")) +
    labs(x = "", y = "Height") +
    theme(legend.position = "none",
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) -> p
  return(p)
}

p1 <- draw_cluster(threshold = "0.3", phase = "wildtype", pt = pruning_threshold[1]) + labs(title = "Wildtype phase") 
p2 <- draw_cluster(threshold = "0.3", phase = "Alpha", pt = pruning_threshold[2]) + labs(title = "Alpha phase")
p3 <- draw_cluster(threshold = "0.3", phase = "Delta", pt = pruning_threshold[3]) + labs(title = "Delta phase")
p4 <- draw_cluster(threshold = "0.3", phase = "Omicron", pt = pruning_threshold[4]) + labs(title = "Omicron phase")

p <- plot_grid(p1, p2, p3, p4, align = "hv", axis = "tblr", ncol = 2)
p <- plot_grid(p, p_legend, ncol = 1, rel_heights = c(12,1))

ggsave("figs/manuscript_fig3.png",
       p,
       width = 15,
       height = 10)


for(m in c("0.1", "0.2", "0.3", "0.4","0.5")){
  p1 <- draw_cluster(threshold = m, phase = "wildtype", pt = pruning_threshold[1]) + labs(title = "Wildtype phase")
  p2 <- draw_cluster(threshold = m, phase = "Alpha", pt = pruning_threshold[2]) + labs(title = "Alpha phase")
  p3 <- draw_cluster(threshold = m, phase = "Delta", pt = pruning_threshold[3]) + labs(title = "Delta phase")
  p4 <- draw_cluster(threshold = m, phase = "Omicron", pt = pruning_threshold[4]) + labs(title = "Omicron phase")
  
  p <- plot_grid(p1, p2, p3, p4, align = "hv", axis = "tblr", ncol = 2)
  p <- plot_grid(p, p_legend, ncol = 1, rel_heights = c(12,1))
  
  ggsave(paste0("figs/manuscript_fig3_",m,".png"),
         p,
         width = 15,
         height = 10)
  
}


# p1 <- draw_cluster(list_name = "max", phase = 1) + labs(title = "Wildtype phase")
# p2 <- draw_cluster(list_name = "max", phase = 2) + labs(title = "Alpha phase")
# p3 <- draw_cluster(list_name = "max", phase = 3) + labs(title = "Delta phase")
# p4 <- draw_cluster(list_name = "max", phase = 4) + labs(title = "Omicron phase")
# 
# p <- plot_grid(p1, p2, p3, p4, align = "hv", axis = "tblr", ncol = 2)
# 
# ggsave("figs/manuscript_fig3_max.png",
#        width = 15,
#        height = 15)

# joined$any %>%
#   group_by(phase) %>%
#   group_split() -> any
# any[[5]] <- joined$any
# any %>%
#   map(ungroup) %>%
#   map(arrange, date) %>%
#   map(dplyr::select, all_of(policy_raw_all)) %>%
#   map(as.matrix) %>% 
#   map(rcorr) %>% 
#   map(~.$r) %>% 
#   map(as.matrix) %>% 
#   map(reshape2::melt) -> corr_any
# write_rds(corr_any, paste0(path_data, "corr_any.rds"))
# any %>%
#   map(ungroup) %>%
#   map(arrange, date) %>%
#   map(dplyr::select, all_of(policy_raw_all)) %>%
#   map(t) %>% 
#   map(factoextra::get_dist,
#       method = "euclidean") -> distance_any
# distance_any %>% write_rds(., paste0(path_data, "distance_any.rds"))
# any %>%
#   map(arrange, date) %>%
#   map(ungroup) %>%
#   map(dplyr::select, all_of(policy_raw_all)) %>%
#       map(pvclust::pvclust,
#           method.hclust = "ward.D2",
#           method.dist = "euclidean",
#           nboot = 5000,
#           parallel = T) -> hcd_any
# hcd_any %>% write_rds(., paste0(path_data, "hcd_any.rds"))

# joined$max %>%
#   group_by(phase) %>%
#   group_split() -> max
# max[[5]] <- joined$max
# max %>%
#   map(ungroup) %>%
#   map(arrange, date) %>%
#   map(dplyr::select, all_of(policy_raw_all)) %>%
#   map(as.matrix) %>%
#   map(rcorr) %>%
#   map(~.$r) %>%
#   map(as.matrix) %>%
#   map(reshape2::melt) -> corr_max
# write_rds(corr_max, paste0(path_data, "corr_max.rds"))
# max %>% 
#   map(ungroup) %>% 
#   map(arrange, date) %>% 
#   map(dplyr::select, all_of(policy_raw_all)) %>% 
#   map(t) %>% 
#   map(factoextra::get_dist,
#       method = "euclidean") -> distance_max
# distance_max %>% write_rds(., paste0(path_data, "distance_max.rds"))
# max %>%
#   map(arrange, date) %>%
#   map(ungroup) %>%
#   map(dplyr::select, all_of(policy_raw_all)) %>%
#   map(pvclust::pvclust,
#       method.hclust = "ward.D2",
#       method.dist = "euclidean",
#       nboot = 5000,
#       parallel = T) -> hcd_max
# hcd_max %>% write_rds(., paste0(path_data, "hcd_max.rds"))

#### combine measures ####
# lapply(1:length(distance_all),
#        function(x){
#          distance_all[[x]] %>% 
#            rename(distance = value) %>% 
#            left_join(corr_all[[x]] %>% 
#                        rename(corr = value),
#                      by = c("Var1", "Var2")) %>% 
#            filter(Var1 != Var2) # %>% 
#            # filter(corr >= 0.7)
#        }) %>% 
#   bind_rows() %>% 
#   tibble %>% 
#   ggplot(., aes(x = distance, y = corr)) +
#   geom_point()

#### draw dendrogram ####




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
