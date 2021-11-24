#if(!exists("joined")) joined <- here("data", "joined_all.RDS") %>% readRDS

source("V4_plot_cluster.R")

joined <- readRDS("data/joined_all_V6.RDS")

# Remove vaccination from wild type scenarios 
joined$s1_W_mid <- joined$s1_W_mid[,1:17]
joined$s1_W_max <- joined$s1_W_max[,1:17]


# Full number of countries, no age stratification
policy_raw_all <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6","V_all_adj")

var_remove <- c("country", "cnt", "date","V_all_adj", "V_18_60_adj", "V_60_adj", "V_tot_adj", "median", "lower_90", "upper_90", "lower_50", "upper_50")

#policy_raw_age <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6","V_18_60_adj", "V_60_adj")


# [START]
# this needs to be ran only once, and it may take some time
# this segment conducts the temporal clustering analyses.

# Full TS, Alpha and Delta variants
hcd_FAD <-  joined %>%
  .[c("s1_full_mid", "s1_A_mid","s1_D_mid",
      "s1_full_max", "s1_A_max","s1_D_max")] %>%
  map(ungroup) %>%
  map(dplyr::select, policy_raw_all) %>%
  map(pvclust::pvclust,
      method.hclust = "ward.D2",
      method.dist = "euclidean",
      nboot = 10000)
# 
save(hcd_FAD, file = "results/hcd_V6_FAD.rdata")

# Wild type
hcd_W <-  joined %>%
  .[c("s1_W_mid","s1_W_max")] %>%
  map(ungroup) %>%
  map(dplyr::select, -var_remove) %>%
  map(pvclust::pvclust,
      method.hclust = "ward.D2",
      method.dist = "euclidean",
      nboot = 10000)
# 
save(hcd_W, file = "results/hcd_V6_W.rdata")

# [END]


# Multi-level effort
# Full TS
plot(hcd_FAD$s1_full_mid)
pvrect(hcd_FAD$s1_full_mid, alpha=0.95, max.only = FALSE)

# Wild type
plot(hcd_W$s1_W_mid)
pvrect(hcd_W$s1_W_mid, alpha=0.95, max.only = FALSE)

# Alpha 
plot(hcd_FAD$s1_A_mid)
pvrect(hcd_FAD$s1_A_mid, alpha=0.95, max.only = FALSE)

# Delta
plot(hcd_FAD$s1_D_mid)
pvrect(hcd_FAD$s1_D_mid, alpha=0.95 , max.only = FALSE)


# Max effort
# Full Ts
plot(hcd_FAD$s1_full_max)
pvrect(hcd_FAD$s1_full_max, alpha=0.95, max.only = FALSE)

# Wild type
plot(hcd_W$s1_W_max)
pvrect(hcd_W$s1_W_max, alpha=0.95, max.only = FALSE)

# Alpha 
plot(hcd_FAD$s1_A_max)
pvrect(hcd_FAD$s1_A_max, alpha=0.95, max.only = FALSE)

# Delta 
plot(hcd_FAD$s1_D_max)
pvrect(hcd_FAD$s1_D_max, alpha=0.95, max.only = FALSE)


# full alpha and delta
list_FAD <- joined %>%
  .[c("s1_full_mid", "s1_A_mid","s1_D_mid",
      "s1_full_max", "s1_A_max","s1_D_max")] %>%
  map(ungroup) %>%
  map(dplyr::select, policy_raw_all) %>% 
  map(t) %>% 
  map(dist, method = "euclidean") %>% 
  map(hclust, method = "ward.D2") %>% 
  map(dendro_data)

# wild
list_w <- joined %>%
  .[c("s1_W_mid","s1_W_max")] %>%
  map(ungroup) %>%
  map(dplyr::select, -var_remove) %>% 
  map(t) %>% 
  map(dist, method = "euclidean") %>% 
  map(hclust, method = "ward.D2") %>% 
  map(dendro_data)


# List of all age clustering 
#all_age_dend <- list_FAD


all_age_dend <- list(list_FAD[[1]], list_w[[1]], list_FAD[[2]], list_FAD[[3]], 
     list_FAD[[4]], list_w[[2]], list_FAD[[5]], list_FAD[[6]])

# Full TS multi-level effort
plot(hcd_FAD$s1_full_mid)
pvrect(hcd_FAD$s1_full_mid, alpha=0.95)

plot_dend_all_age(1)+
  labs(title = "Multi-level effort - Full time series\n2020-01-01 to 2021-09-30",
       color = " ",
       y = "Height") +
  geom_rect(data = data.frame(xmin = c(4.65, 0.25),
                              xmax = c(15.75, 4.45),
                              ymin = c(-65, -65),
                              ymax = c(105, 95)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "red",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_V6/fig2_1_mid_full.png",
       width = 18,
       height = 9)  


# Wild type, multi-level effort
plot(hcd_W$s1_W_mid)
pvrect(hcd_W$s1_W_mid, alpha=0.95)

plot_dend_all_age(2)+
  labs(title = "Multi-level effort - Wild type virus dominant period",
       color = " ",
       y = "Height") +
  geom_rect(data = data.frame(xmin = c(3.65, 0.25),
                              xmax = c(14.75, 3.45),
                              ymin = c(-65, -65),
                              ymax = c(75, 50)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "red",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_V6/fig2_2_mid_wild.png",
       width = 18,
       height = 9)  

# Alpha type, multi-level effort
plot(hcd_FAD$s1_A_mid)
pvrect(hcd_FAD$s1_A_mid, alpha=0.95)

plot_dend_all_age(3)+
  labs(title = "Multi-level effort - Alpha variant dominant period",
       color = " ",
       y = "Height") +
  geom_rect(data = data.frame(xmin = c(4.65, 0.25),
                              xmax = c(15.75, 4.45),
                              ymin = c(-65, -65),
                              ymax = c(56, 55)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "red",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_V6/fig2_3_mid_alpha.png",
       width = 18,
       height = 9)  

# Alpha type, multi-level effort
plot(hcd_FAD$s1_D_mid)
pvrect(hcd_FAD$s1_D_mid, alpha=0.95)

plot_dend_all_age(4)+
  labs(title = "Multi-level effort - Delta variant dominant period",
       color = " ",
       y = "Height") +
  geom_rect(data = data.frame(xmin = c(3.65, 0.25),
                              xmax = c(15.75, 3.45),
                              ymin = c(-65, -65),
                              ymax = c(65, 35)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "red",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_V6/fig2_4_mid_delta.png",
       width = 18,
       height = 9)  

##### Max effort
# Full TS max effort
plot(hcd_FAD$s1_full_max)
pvrect(hcd_FAD$s1_full_max, alpha=0.95)

plot_dend_all_age(5)+
  labs(title = "Max effort - Full time series\n2020-01-01 to 2021-09-30",
       color = " ",
       y = "Height") +
  geom_rect(data = data.frame(xmin = c(8.65, 0.25),
                              xmax = c(15.75, 8.45),
                              ymin = c(-65, -65),
                              ymax = c(150, 105)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "red",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_V6/fig2_5_max_full.png",
       width = 18,
       height = 9)  


# Wild type, max effort
plot(hcd_W$s1_W_max)
pvrect(hcd_W$s1_W_max, alpha=0.95)

plot_dend_all_age(6)+
  labs(title = "Max effort - Wild type virus dominant period",
       color = " ",
       y = "Height") +
  geom_rect(data = data.frame(xmin = c(12.65, 10.65, 8.65, 5.65),
                              xmax = c(14.75, 12.45, 10.45, 8.45),
                              ymin = c(-45, -45, -45, -45),
                              ymax = c(80, 75, 70, 45)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "red",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_V6/fig2_6_max_wild.png",
       width = 18,
       height = 9)  

# Alpha type, max effort
plot(hcd_FAD$s1_A_max)
pvrect(hcd_FAD$s1_A_max, alpha=0.95)

plot_dend_all_age(7)+
  labs(title = "Max effort - Alpha variant dominant period",
       color = " ",
       y = "Height") +
  geom_rect(data = data.frame(xmin = c(4.65, 0.25),
                              xmax = c(15.75, 4.45),
                              ymin = c(-65, -65),
                              ymax = c(90, 65)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "red",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_V6/fig2_7_max_alpha.png",
       width = 18,
       height = 9)  

# Delta type, max effort
plot(hcd_FAD$s1_D_max)
pvrect(hcd_FAD$s1_D_max, alpha=0.95)

plot_dend_all_age(8)+
  labs(title = "Max effort - Delta variant dominant period",
       color = " ",
       y = "Height") +
  geom_rect(data = data.frame(xmin = c(12.65, 5.65, 0.25),
                              xmax = c(15.75, 12.45, 5.45),
                              ymin = c(-65, -65, -65),
                              ymax = c(60, 40, 60)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "red",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_V6/fig2_8_max_delta.png",
       width = 18,
       height = 9)  



plot(hcd_FAD$s1_D_mid)
pvrect(hcd_FAD$s1_D_mid, alpha=0.5)
