#
# Markers for virus variant
markers <- tibble(marker = factor(c("first", "alpha", "delta"), 
                                  levels = c("first", "alpha", "delta"),
                                  labels = c("First case\ndetected", "Alpha becomes\ndominant variant", "Delta becomes\ndominant variant")),
                  date = c(as.Date("2020-01-20"), as.Date("2020-12-01"), as.Date("2021-05-01")))

# Read in Rt data
rt_estimates <- read_csv("data/rt_EURO.csv")

# Plot Rt
rt_estimates %>% 
  group_by(country) %>% 
  ggplot(aes(x= date))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  geom_line(aes(y = median, group = country), color = "red")+
  geom_vline(data = markers,
             aes(xintercept =  date,
                 linetype = marker))+
  facet_wrap(~country)+
  ylim(0,3)+
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")+
  labs(title = "Effective repoduction number (Rt)", y = "Rt", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8),
        legend.position = "bottom",
        legend.title = element_blank())

# Save Rt
ggsave(filename = "figs/EURO_2/fig_rt.png",
       plot = last_plot(),
       width = 15,
       height = 10)


# Mean Rt plot
rt_estimates %>% 
  group_by(date) %>% 
  summarise(mean_rt = mean(median)) %>% 
  ggplot(aes(x= date))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  geom_line(aes(y = mean_rt), color = "red")+
  geom_vline(data = markers,
             aes(xintercept =  date,
                 linetype = marker))+
  ylim(0.5,2.5)+
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")+
  labs(title = "Mean Rt", y = "Mean Rt", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8),
        legend.position = "bottom",
        legend.title = element_blank())


# International travel restrictions (scaled) compare to Rt 
joined_mid %>% 
  select(country, date, "International travel restrictions" = C8, "Rt" = median) %>% 
  pivot_longer(cols = c(3,4), names_to = "NPI", values_to = "Intensity") %>% 
  ggplot(aes(x= date))+
  geom_line(aes(y = Intensity, color = NPI))+
  geom_vline(data = markers,
             aes(xintercept =  date,
                 linetype = marker))+
  facet_wrap(~country)+
  ylim(-0.1,2)+
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")+
  labs(title = "International travel restriction & Rt compare", y = "", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8),
        legend.position = "bottom",
        legend.title = element_blank())

# Mask wearing (scaled)
joined_mid %>% 
  select(country, date, H6) %>% 
  ggplot(aes(x= date))+
  #geom_hline(yintercept = 1, linetype = "dashed")+
  #geom_ribbon(aes(ymin = lower_90, ymax = upper_90, group = country))+
  geom_line(aes(y = H6, group = country), color = "blue")+
  geom_vline(data = markers,
             aes(xintercept =  date,
                 linetype = marker))+
  facet_wrap(~country)+
  ylim(-0.1,1.25)+
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")+
  labs(title = "Mask wearing", y = "Scaled NPI strength", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8),
        legend.position = "bottom",
        legend.title = element_blank())

# Save mask wearing example
ggsave(filename = "figs/EURO_2/fig_mask.png",
       plot = last_plot(),
       width = 15,
       height = 10)

# Mask wearing (Max)
joined_hi %>% 
  select(country, date, H6) %>% 
  ggplot(aes(x= date))+
  #geom_hline(yintercept = 1, linetype = "dashed")+
  #geom_ribbon(aes(ymin = lower_90, ymax = upper_90, group = country))+
  geom_line(aes(y = H6, group = country), color = "blue")+
  geom_vline(data = markers,
             aes(xintercept =  date,
                 linetype = marker))+
  facet_wrap(~country)+
  ylim(-0.1,1.25)+
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")+
  labs(title = "Mask wearing - Max effort", y = "Scaled NPI strength", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8),
        legend.position = "bottom",
        legend.title = element_blank())

# Save mask max
ggsave(filename = "figs/EURO_2/fig_mask_max.png",
       plot = last_plot(),
       width = 15,
       height = 10)

###

# Compare mask wearing and workplace clousre examples
joined_mid %>% 
  select(country, date, "Workplace closure" = C2, "Facial covering" = H6) %>% 
  pivot_longer(cols = c(3,4), names_to = "NPI", values_to = "Intensity") %>% 
  ggplot(aes(x= date))+
  #geom_hline(yintercept = 1, linetype = "dashed")+
  #geom_ribbon(aes(ymin = lower_90, ymax = upper_90, group = country))+
  geom_line(aes(y = Intensity, color = NPI))+
  geom_vline(data = markers,
             aes(xintercept =  date,
                 linetype = marker))+
  facet_wrap(~country)+
  ylim(-0.1,1.25)+
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")+
  labs(title = "Facial covering & workplace clousure compare", y = "Scaled NPI strength", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8),
        legend.position = "bottom",
        legend.title = element_blank())
# save
ggsave(filename = "figs/EURO_2/fig_NPI_example.png",
       plot = last_plot(),
       width = 15,
       height = 10)

# Compare mask wearing and workplace clousre examples (max)
joined_hi %>% 
  select(country, date, "Workplace closure" = C2, "Facial covering" = H6) %>% 
  pivot_longer(cols = c(3,4), names_to = "NPI", values_to = "Intensity") %>% 
  ggplot(aes(x= date))+
  geom_line(aes(y = Intensity, color = NPI))+
  geom_vline(data = markers,
             aes(xintercept =  date,
                 linetype = marker))+
  facet_wrap(~country)+
  ylim(-0.1,1.25)+
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")+
  labs(title = "Facial covering & workplace clousure compare - max effort", y = "NPI strength", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8),
        legend.position = "bottom",
        legend.title = element_blank())

ggsave(filename = "figs/EURO_2/fig_NPI_example_max.png",
       plot = last_plot(),
       width = 15,
       height = 10)


# Turn plot into facet 
p_stringency <-  stringency %>%
  mutate(region = "WHO EUROPE") %>%
  filter(!is.na(region)) %>% 
  ggplot(., aes(x = date, y = stringency, group = cnt)) + 
  geom_line(aes(color = region), alpha = 0.1) +
  theme_cowplot() +
  geom_smooth(aes(group = VOC))+
  geom_segment(x = as.Date("2020-12-01"), xend =as.Date("2020-12-01"),
               y = 0, yend = 90, linetype = "dashed") +
  geom_segment(x = as.Date("2021-05-01"), xend =as.Date("2021-05-01"),
               y = 0, yend = 90, linetype = "dashed") +
  annotate(geom = "text", x = as.Date("2020-12-01"), y= 95, label = "Alpha becomes\ndominant variant")+
  annotate(geom = "text", x = as.Date("2021-05-01"), y= 95, label = "Delta becomes\ndominant variant")+
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%Y")+
  ggsci::scale_color_lancet()+
  ggsci::scale_fill_lancet()+
  labs(x = "",
       y = "Stringency Index",
       color = "Region",
       fill = "Region")  +
  theme(axis.text = element_text(size = 15),
        legend.position = "bottom")  

ggsave(filename = "figs/EURO_2/stringency_region.png",
       plot = p_stringency,
       width = 15, 
       height = 10)