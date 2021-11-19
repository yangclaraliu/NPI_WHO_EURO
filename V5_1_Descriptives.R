
# Sig box 
joined$s1_W %>% 
  select(country, date, C6, C5, C7, V_all_adj) %>% 
  pivot_longer(cols = c(3,4,5,6), names_to = "NPI", values_to = "Intensity") %>% 
  ggplot(aes(x= date))+
  #geom_hline(yintercept = 1, linetype = "dashed")+
  #geom_ribbon(aes(ymin = lower_90, ymax = upper_90, group = country))+
  geom_line(aes(y = Intensity, color = NPI))+
#  geom_vline(data = markers,
#             aes(xintercept =  date,
#                 linetype = marker))+
  facet_wrap(~country)+
  ylim(-0.1,1.25)+
  scale_x_date(date_breaks = "1 months", date_labels = "%b %Y")+
  labs(title = "", y = "Scaled NPI strength", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8),
        legend.position = "bottom",
        legend.title = element_blank())

# sig box
joined$s1_A %>% 
  select(country, date, C6, C5, C7, V_all_adj) %>% 
  pivot_longer(cols = c(3,4,5,6), names_to = "NPI", values_to = "Intensity") %>% 
  ggplot(aes(x= date))+
  #geom_hline(yintercept = 1, linetype = "dashed")+
  #geom_ribbon(aes(ymin = lower_90, ymax = upper_90, group = country))+
  geom_line(aes(y = Intensity, color = NPI))+
  #  geom_vline(data = markers,
  #             aes(xintercept =  date,
  #                 linetype = marker))+
  facet_wrap(~country)+
  ylim(-0.1,1.25)+
  scale_x_date(date_breaks = "1 months", date_labels = "%b %Y")+
  labs(title = "", y = "Scaled NPI strength", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8),
        legend.position = "bottom",
        legend.title = element_blank())

#sig box
joined$s1_D %>% 
  select(country, date, C6, C5, C7, V_all_adj) %>% 
  pivot_longer(cols = c(3,4,5,6), names_to = "NPI", values_to = "Intensity") %>% 
  ggplot(aes(x= date))+
  #geom_hline(yintercept = 1, linetype = "dashed")+
  #geom_ribbon(aes(ymin = lower_90, ymax = upper_90, group = country))+
  geom_line(aes(y = Intensity, color = NPI))+
  #  geom_vline(data = markers,
  #             aes(xintercept =  date,
  #                 linetype = marker))+
  facet_wrap(~country)+
  ylim(-0.1,1.25)+
  scale_x_date(date_breaks = "1 months", date_labels = "%b %Y")+
  labs(title = "", y = "Scaled NPI strength", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8),
        legend.position = "bottom",
        legend.title = element_blank())

# Most similar pair (Delta)
joined$s1_D %>% 
  select(country, date, C1, C2) %>% 
  pivot_longer(cols = c(3,4), names_to = "NPI", values_to = "Intensity") %>% 
  ggplot(aes(x= date))+
  #geom_hline(yintercept = 1, linetype = "dashed")+
  #geom_ribbon(aes(ymin = lower_90, ymax = upper_90, group = country))+
  geom_line(aes(y = Intensity, color = NPI))+
  #  geom_vline(data = markers,
  #             aes(xintercept =  date,
  #                 linetype = marker))+
  facet_wrap(~country)+
  ylim(-0.1,1.25)+
  scale_x_date(date_breaks = "1 months", date_labels = "%b %Y")+
  labs(title = "", y = "Scaled NPI strength", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8),
        legend.position = "bottom",
        legend.title = element_blank())


# Most similar pair including vaccination (Delta)
joined$s1_D %>% 
  select(country, date, C8, V_all_adj) %>% 
  pivot_longer(cols = c(3,4), names_to = "NPI", values_to = "Intensity") %>% 
  ggplot(aes(x= date))+
  #geom_hline(yintercept = 1, linetype = "dashed")+
  #geom_ribbon(aes(ymin = lower_90, ymax = upper_90, group = country))+
  geom_line(aes(y = Intensity, color = NPI))+
  #  geom_vline(data = markers,
  #             aes(xintercept =  date,
  #                 linetype = marker))+
  facet_wrap(~country)+
  ylim(-0.1,1.25)+
  scale_x_date(date_breaks = "1 months", date_labels = "%b %Y")+
  labs(title = "", y = "Scaled NPI strength", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8),
        legend.position = "bottom",
        legend.title = element_blank())


# Most similar pair including vaccination (Delta)
joined$s1_A %>% 
  select(country, date, H1, C3, C4) %>% 
  pivot_longer(cols = c(3,4, 5), names_to = "NPI", values_to = "Intensity") %>% 
  ggplot(aes(x= date))+
  #geom_hline(yintercept = 1, linetype = "dashed")+
  #geom_ribbon(aes(ymin = lower_90, ymax = upper_90, group = country))+
  geom_line(aes(y = Intensity, color = NPI))+
  #  geom_vline(data = markers,
  #             aes(xintercept =  date,
  #                 linetype = marker))+
  facet_wrap(~country)+
  ylim(-0.1,1.25)+
  scale_x_date(date_breaks = "1 months", date_labels = "%b %Y")+
  labs(title = "", y = "Scaled NPI strength", x = "")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8),
        legend.position = "bottom",
        legend.title = element_blank())
