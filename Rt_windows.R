
#Various Rt windows 

# Filter Rt to Yangs original timeline (up to July 5th 2020)
orig_rt <- read_csv("data/rt_estimates_2021-09-30.csv") %>% 
  filter(date < as.Date("2020-07-05"))


# Full EURO Rt timeline (till Oct 1st 2020)

RT_EURO_FULL <- read_csv("data/rt_EURO.csv")

# Rt time-series to original study timeline
RT_EURO_0 <- read_csv("data/rt_EURO.csv") %>% 
  filter(date < as.Date("2020-07-05"))


OX_EURO_FULL <- read_csv("data/NPI_EURO.csv") 

OX_EURO_0 <- read_csv("data/NPI_EURO.csv") %>% 
  filter(Date < 20200705)
  #filter(Date < as.Date("2020-07-05"))

OX_EURO_0 %>% 
  distinct(CountryCode)



