#if(!exists("joined")) joined <- here("data", "joined_all.RDS") %>% readRDS

joined <- readRDS("data/joined_all_V5.RDS")

# Full number of countries, no age stratification
policy_raw_all <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6","V_all_adj")


# [START]
# this needs to be ran only once, and it may take some time
# this segment conducts the temporal clustering analyses.
hcd_all <-  joined %>%
  .[c("s1_W", "s1_A", "s1_D",
      "s2_W", "s2_A", "s2_D",
      "s3_W", "s3_A", "s3_D",
      "s4_W", "s4_A", "s4_D")] %>%
  map(ungroup) %>%
  map(dplyr::select, policy_raw_all) %>%
  
  # this specific line below is for calculating the statistical significance of 
  # temporal clusters based on bootstrapping.
  
  map(pvclust::pvclust,
      method.hclust = "ward.D2",
      method.dist = "euclidean",
      nboot = 10000)
# 
 save(hcd, file = "results/hcd.rdata")
# [END]

#load("results/hcd.rdata")




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



# Max wild - all age
max_wild <- joined$hi_W %>% 
  select(policy_raw_all) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# Max alpha - all age
max_alpha <- joined$hi_A %>% 
  select(policy_raw_all) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# Max delta - all age
max_delta <- joined$hi_D %>% 
  select(policy_raw_all) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

##

# Mid wild - all age
mid_wild <- joined$mid_W %>% 
  select(policy_raw_all) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# Mid alpha - all age
mid_alpha <- joined$mid_A %>% 
  select(policy_raw_all) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# Mid delta - all age
mid_delta <- joined$mid_D %>% 
  select(policy_raw_all) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# List of all age clustering 
all_age_dend <- list(max_wild, max_alpha, max_delta, mid_wild, mid_alpha, mid_delta)


# Max wild virus - all ages
plot_dend_all_age(1)+
  labs(title = "Timing of Max. Efforts - Wild virus\n2020-01-01 to 2020-11-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_2/fig2_1_max_wild.png",
       width = 15,
       height = 10)  

# Max Alpha virus - all ages
plot_dend_all_age(2)+
  labs(title = "Timing of Max. Efforts - Alpha virus\n2020-12-01 to 2021-04-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_2/fig2_2_max_alpha.png",
       width = 15,
       height = 10) 

# Max Delta virus - all ages
plot_dend_all_age(3)+
  labs(title = "Timing of Max. Efforts - Delta virus\n2021-05-01 to 2021-09-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_2/fig2_3_max_delta.png",
       width = 15,
       height = 10) 


### Mid effort (scaled NPIs)

# Mid wild virus - all ages
plot_dend_all_age(4)+
  labs(title = "Timing of Scaled Efforts - Wild virus\n2020-01-01 to 2020-11-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_2/fig2_4_mid_wild.png",
       width = 15,
       height = 10)  

# Mid Alpha virus - all ages
plot_dend_all_age(5)+
  labs(title = "Timing of Scaled Efforts - Alpha virus\n2020-12-01 to 2021-04-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_2/fig2_5_mid_alpha.png",
       width = 15,
       height = 10) 

# Mid Delta virus - all ages
plot_dend_all_age(6)+
  labs(title = "Timing of Scaled Efforts - Delta virus\n2021-05-01 to 2021-09-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_2/fig2_6_mid_delta.png",
       width = 15,
       height = 10) 



### Age specific

# Max wild - all age
max_wild_age <- joined$hi_W %>% 
  select(policy_raw_age) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# Max alpha - all age
max_alpha_age <- joined$hi_A %>% 
  select(policy_raw_age) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# Max delta - all age
max_delta_age <- joined$hi_D %>% 
  select(policy_raw_age) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

##

# Mid wild - all age
mid_wild_age <- joined$mid_W %>% 
  select(policy_raw_age) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# Mid alpha - all age
mid_alpha_age <- joined$mid_A %>% 
  select(policy_raw_age) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# Mid delta - all age
mid_delta_age <- joined$mid_D %>% 
  select(policy_raw_age) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# List of all age clustering 
age_dend <- list(max_wild_age, max_alpha_age, max_delta_age, mid_wild_age, mid_alpha_age, mid_delta_age)


# Max wild virus - age specific
plot_dend_age(1)+
  labs(title = "Timing of Max. Efforts - Wild virus\n2020-01-01 to 2020-11-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_2/fig2_7_age_max_wild.png",
       width = 15,
       height = 10)  

# Max Alpha virus - age specific
plot_dend_age(2)+
  labs(title = "Timing of Max. Efforts - Alpha virus\n2020-12-01 to 2021-04-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_2/fig2_8_age__max_alpha.png",
       width = 15,
       height = 10) 

# Max Delta virus - all ages
plot_dend_age(3)+
  labs(title = "Timing of Max. Efforts - Delta virus\n2021-05-01 to 2021-09-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_2/fig2_9_age_max_delta.png",
       width = 15,
       height = 10) 


### Mid effort (scaled NPIs)

# Mid wild virus - all ages
plot_dend_age(4)+
  labs(title = "Timing of Scaled Efforts - Wild virus\n2020-01-01 to 2020-11-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_2/fig2_10_age_mid_wild.png",
       width = 15,
       height = 10)  

# Mid Alpha virus - all ages
plot_dend_age(5)+
  labs(title = "Timing of Scaled Efforts - Alpha virus\n2020-12-01 to 2021-04-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_2/fig2_11_age_mid_alpha.png",
       width = 15,
       height = 10) 

# Mid Delta virus - all ages
plot_dend_age(6)+
  labs(title = "Timing of Scaled Efforts - Delta virus\n2021-05-01 to 2021-09-30",
       color = " ",
       y = "Height") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_2/fig2_12_age_mid_delta.png",
       width = 15,
       height = 10) 
