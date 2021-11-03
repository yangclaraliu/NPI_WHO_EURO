#
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