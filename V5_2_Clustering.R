#if(!exists("joined")) joined <- here("data", "joined_all.RDS") %>% readRDS

joined <- readRDS("data/joined_all_V5.RDS")

# Full number of countries, no age stratification
policy_raw_all <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6","V_all_adj")

policy_raw_age <- c("C1","C2","C3","C4","C5","C6","C7","C8","E1","E2","H1","H2","H3","H6","V_18_60_adj", "V_60_adj")

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
 save(hcd_all, file = "results/hcd_V5.rdata")
# [END]

load("results/hcd_V5.rdata")

 plot(hcd_all$s1_D)
pvrect(hcd_all$s1_D, alpha=0.9)

hist(joined$s1_A) %>% 

plot(joined$s1_A$C1, joined$s1_A$C2)

a <- joined$s1_A$C1
b <-joined$s1_A$C2
tibble(a, b)

ggplot(data = tibble(a, b))+
  geom_point(aes(x = a, y = b))+
  abline()

# S1 cluster - wild
s1_W_dend <- joined$s1_W %>% 
  select(policy_raw_all) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# S1 cluster - Alpha
s1_A_dend <- joined$s1_A %>% 
  select(policy_raw_all) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# S1 cluster - Delta
s1_D_dend <- joined$s1_D %>% 
  select(policy_raw_all) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()


# List of all age clustering 
all_age_dend <- list(s1_W_dend, s1_A_dend, s1_D_dend)


# S1 wild virus
hcd_all$s1_W %>% plot()

plot_dend_all_age(1)+
  labs(title = "Wild virus\n2020-01-01 to 2020-11-22",
       color = " ",
       y = "Height") +
      geom_rect(data = data.frame(xmin = c(4.65, 0.25),
                                  xmax = c(15.75, 4.45),
                                  ymin = c(-65, -65),
                                  ymax = c(75, 65)),
                aes(xmin = xmin,
                    xmax = xmax,
                    ymin = ymin,
                    ymax = ymax),
                fill = NA,
                color = "firebrick",
                size = 1.5,
               linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_V5/fig2_1_wild.png",
       width = 18,
       height = 9)  

# S1 Alpha variant
hcd_all$s1_A %>% plot()

plot_dend_all_age(2)+
  labs(title = "Alpha variant\n2020-11-23 to 2021-05-10",
       color = " ",
       y = "Height") +
  geom_rect(data = data.frame(xmin = c(4.65, 0.25),
                              xmax = c(15.75, 4.45),
                              ymin = c(-65, -65),
                              ymax = c(59, 56)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "firebrick",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_V5/fig2_2_alpha.png",
       width = 18,
       height = 9)  


# S1 Delta variant
hcd_all$s1_D %>% plot()

plot_dend_all_age(3)+
  labs(title = "Delta variant\n2021-05-11 to 2021-09-30",
       color = " ",
       y = "Height") +
  geom_rect(data = data.frame(xmin = c(3.65, 0.25),
                              xmax = c(15.75, 3.45),
                              ymin = c(-65, -65),
                              ymax = c(70, 35)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "firebrick",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_V5/fig2_3_delta.png",
       width = 18,
       height = 9)  



### ---- ####  Age clustering  ### ---- ####  

hcd_age <-  joined %>%
  .[c("s1_W", "s1_A", "s1_D")] %>%
  map(ungroup) %>%
  map(filter, !country %in% cnt_remove ) %>% 
  map(dplyr::select, policy_raw_age) %>%
  
  # this specific line below is for calculating the statistical significance of 
  # temporal clusters based on bootstrapping.
  
  map(pvclust::pvclust,
      method.hclust = "ward.D2",
      method.dist = "euclidean",
      nboot = 10000)

save(hcd_age, file = "results/hcd_age_V5.rdata")



# S1 cluster - wild
s1_W_dend_age <- joined$s1_W %>% 
  filter(!country %in% cnt_remove) %>% 
  select(policy_raw_age) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()

# S1 cluster - Alpha
s1_A_dend_age <- joined$s1_A %>% 
  filter(!country %in% cnt_remove) %>% 
  select(policy_raw_age) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()


# S1 cluster - Delta
s1_D_dend_age <- joined$s1_D %>% 
  filter(!country %in% cnt_remove) %>% 
  select(policy_raw_age) %>% 
  t() %>% 
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2") %>% 
  dendro_data()



# List of all age clustering 
age_dend <- list(s1_W_dend_age, s1_A_dend_age, s1_D_dend_age)


# S1 wild virus age
hcd_age$s1_W %>% plot()

plot_dend_age(1)+
  labs(title = "Wild virus\n2020-01-01 to 2020-11-22",
       color = " ",
       y = "Height") +
  geom_rect(data = data.frame(xmin = c(6.65, 0.25),
                              xmax = c(16.75, 6.45),
                              ymin = c(-65, -65),
                              ymax = c(65, 60)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "firebrick",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_V5/fig2_1_wild_age.png",
       width = 18,
       height = 9)  


# S1 Alphavirus age
hcd_age$s1_A %>% plot()

plot_dend_age(2)+
  labs(title = "Alpha variant\n2020-11-23 to 2021-05-10",
       color = " ",
       y = "Height") +
  geom_rect(data = data.frame(xmin = c(11.65, 0.25),
                              xmax = c(16.75, 11.45),
                              ymin = c(-65, -65),
                              ymax = c(55, 50)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "firebrick",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_V5/fig2_2_alpha_age.png",
       width = 18,
       height = 9)  


# S1 Delta variant
hcd_age$s1_D %>% plot()

plot_dend_age(3)+
  labs(title = "Delta variant\n2021-05-11 to 2021-09-30",
       color = " ",
       y = "Height") +
  geom_rect(data = data.frame(xmin = c(3.65, 0.25),
                              xmax = c(16.75, 3.45),
                              ymin = c(-65, -65),
                              ymax = c(50, 30)),
            aes(xmin = xmin,
                xmax = xmax,
                ymin = ymin,
                ymax = ymax),
            fill = NA,
            color = "firebrick",
            size = 1.5,
            linetype = 2) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
  geom_text(aes(y = c(0, 50, 75,100, 150), 
                x = -0.2, 
                label = c("0","50","\nHeight","100","150")), 
            size = 5)

ggsave("figs/EURO_V5/fig2_3_delta_age.png",
       width = 18,
       height = 9)  
