read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") |>
  filter(iso_code %in% country_index$iso3c) |> 
  dplyr::select(iso_code, location, date, 
                "total_vaccinations_per_hundred",             
                "people_vaccinated_per_hundred",             
                "people_fully_vaccinated_per_hundred") |> 
  group_by(iso_code) |> 
  group_split() |> 
  map(mutate, 
      total_vaccinations_per_hundred = imputeTS::na_interpolation(total_vaccinations_per_hundred),
      people_vaccinated_per_hundred = imputeTS::na_interpolation(people_vaccinated_per_hundred),
      people_fully_vaccinated_per_hundred = imputeTS::na_interpolation(people_fully_vaccinated_per_hundred)) |> 
  bind_rows() -> data_vac

write_csv(data_vac, "data/VAC_OWIN_v2.csv")

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

# Read in WHO raw vaccination data (Age groups included)
# Only 27 countries
cov_raw <- read_csv("data/data.csv") |> 
  mutate(iso3c = countrycode(ReportingCountry,
                                    origin = "iso2c",
                                    destination = "iso3c")) |> 
  filter(iso3c %in% country_index$iso3c) |> 
  separate(YearWeekISO, sep = "-", into = c("year", "week")) |> 
  dplyr::select(year, week, iso3c, FirstDose, TargetGroup) |> 
  group_by(year, week, iso3c, TargetGroup) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  mutate(week = parse_number(week),
         year = as.numeric(year)) |> 
  mutate(date = MMWRweek2Date(MMWRyear = year, MMWRweek = week, MMWRday = 1)) |> 
  group_by(iso3c) |> group_split()

cov_raw_cleaned <- list()

##### Austria #####
cov_raw[[1]] |> 
  dplyr::filter(!TargetGroup %in% c("Age0_4", "Age5_9", "Age10_14", "Age15_17", "ALL")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup)) |> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) |> 
  mutate(all = adults + `older adults` + children,
         unknown = 0) -> cov_raw_cleaned[[1]] 

##### Belgium #####
cov_raw[[2]] |> 
  dplyr::filter(!TargetGroup %in% c("AgeUNK", "HCW","LTCF", "ALL")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup)) |> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) |> 
  mutate(all = adults +  `older adults` + children, 
         unknown = 0) |> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[2]] 

##### Bulgaria #####
cov_raw[[3]] |> 
  dplyr::filter(!TargetGroup %in% c("HCW","LTCF", "ALL")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup)) |> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  rename(unknown = "AgeUNK") |> 
  mutate(all = adults +  `older adults` + children + unknown)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[3]] 

##### Cypress #####
cov_raw[[4]] |> 
  dplyr::filter(!TargetGroup %in% c("HCW","LTCF", "ALL")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age5_9", "Age10_14", "Age15_17"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) |> 
  mutate(all = adults +  `older adults` + children, 
         unknown = 0) |> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[4]] 

##### Czech #####
cov_raw[[5]] |> 
  dplyr::filter(!TargetGroup %in% c("Age0_4", "Age5_9", "Age10_14", "Age15_17", "ALL", "HCW", "LTCF"))|> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  mutate(all = adults +  `older adults` + children, 
         unknown = 0) |> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[5]] 

##### Germany #####
cov_raw[[6]] |>
  dplyr::filter(!TargetGroup %in% c("ALL"))|> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("1_Age<60"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("1_Age60+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  mutate(adults = adults - children,
         all = adults + children + `older adults`,
         unknown = 0)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[6]] 

##### Denmark #####
cov_raw[[7]] |> 
  dplyr::filter(!TargetGroup %in% c("HCW","LTCF", "ALL")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age5_9", "Age10_14", "Age15_17", "Age0_4"),
                               "children",
                               TargetGroup)) |> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  mutate(all = adults +  `older adults` + children, 
         unknown = 0) |> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[7]] 

##### Spain #####
cov_raw[[8]] |> 
  dplyr::filter(!TargetGroup %in% c("Age0_4", "Age5_9", "Age10_14", "Age15_17", "ALL", "HCW", "LTCF", "AgeUNK")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  mutate(all = adults +  `older adults` + children, 
         unknown = 0)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[8]] 

##### Estonia #####
cov_raw[[9]]|> 
  dplyr::filter(!TargetGroup %in% c("ALL", "HCW", "LTCF")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  rename(unknown = AgeUNK)|> 
  mutate(all = adults +  `older adults` + children + unknown)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[9]] 

##### Finland #####
cov_raw[[10]]|> 
  dplyr::filter(!TargetGroup %in% c("Age0_4", "Age5_9", "Age10_14", "Age15_17", "ALL", "HCW", "LTCF", "AgeUNK")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  mutate(all = adults +  `older adults` + children, 
         unknown = 0)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[10]] 

##### France #####
cov_raw[[11]] |> 
  dplyr::filter(!TargetGroup %in% c("ALL", "HCW", "LTCF")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  rename(unknown = AgeUNK)|> 
  mutate(all = adults +  `older adults` + children + unknown)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[11]] 

##### Croatia #####
cov_raw[[12]]|> 
  dplyr::filter(!TargetGroup %in% c("Age0_4", "Age5_9", "Age10_14", "Age15_17", "ALL", "HCW", "LTCF")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  rename(unknown = AgeUNK)|> 
  mutate(all = adults +  `older adults` + children + unknown)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[12]] 

##### Hungary #####
cov_raw[[13]] |> 
  dplyr::filter(!TargetGroup %in% c("ALL", "HCW", "LTCF")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  rename(unknown = AgeUNK)|> 
  mutate(all = adults +  `older adults` + children + unknown)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[13]] 

##### Ireland #####
cov_raw[[14]] |> 
  dplyr::filter(!TargetGroup %in% c("Age0_4", "Age5_9", "Age10_14", "Age15_17", "ALL", "HCW", "LTCF")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  rename(unknown = AgeUNK)|> 
  mutate(all = adults +  `older adults` + children + unknown)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[14]] 


#### Iceland #####
cov_raw[[15]]|> 
  dplyr::filter(!TargetGroup %in% c("ALL", "HCW", "LTCF")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age5_9", "Age10_14", "Age15_17", "Age0_4"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  rename(unknown = AgeUNK)|> 
  mutate(all = adults +  `older adults` + children + unknown)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[15]] 

##### Italy #####
cov_raw[[16]]|> 
  dplyr::filter(!TargetGroup %in% c("ALL", "HCW", "LTCF")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  mutate(all = adults +  `older adults` + children,
         unknown = 0)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[16]] 

##### Liechtenstein #####
cov_raw[[17]]|> 
  dplyr::filter(!TargetGroup %in% c("Age0_4", "Age5_9", "Age10_14", "Age15_17", "ALL", "HCW", "LTCF")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  rename(unknown = AgeUNK)|> 
  mutate(all = adults +  `older adults` + children + unknown)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[17]] 

##### Luxemburg #####
cov_raw[[18]] |> 
  dplyr::filter(!TargetGroup %in% c("ALL", "HCW", "LTCF")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age5_9", "Age10_14", "Age15_17", "Age0_4"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  mutate(all = adults +  `older adults` + children,
         unknown = 0)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[18]] 

##### Latvia #####
cov_raw[[19]] |> 
  dplyr::filter(!TargetGroup %in% c("Age0_4", "Age5_9", "Age10_14", "Age15_17", "ALL", "HCW", "LTCF")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  rename(unknown = AgeUNK)|> 
  mutate(all = adults +  `older adults` + children + unknown)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[19]] 

##### Malta #####  
cov_raw[[20]] |> 
  dplyr::filter(!TargetGroup %in% c("ALL", "HCW", "LTCF")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  rename(unknown = AgeUNK) |> 
  mutate(all = adults +  `older adults` + children + unknown)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]]))-> cov_raw_cleaned[[20]] 

##### Netherlands #####
cov_raw[[21]] |> 
  dplyr::filter(!TargetGroup %in% c("ALL", "HCW", "LTCF")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age5_9", "Age10_14", "Age15_17", "Age0_4"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  rename(unknown = AgeUNK) |> 
  mutate(all = adults +  `older adults` + children + unknown)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) -> cov_raw_cleaned[[21]] 

##### Norway #####
cov_raw[[22]] |> 
  dplyr::filter(!TargetGroup %in% c("ALL", "HCW", "LTCF")) |> 
  mutate(TargetGroup = if_else(TargetGroup %in% c("Age18_24", "Age25_49", "Age50_59"),
                               "adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age60_69", "Age70_79", "Age80+"),
                               "older adults",
                               TargetGroup),
         TargetGroup = if_else(TargetGroup %in% c("Age<18"),
                               "children",
                               TargetGroup))|> 
  group_by(year, week, iso3c, TargetGroup, date) |> 
  summarise(FirstDose = sum(FirstDose, na.rm = T)) |> 
  pivot_wider(names_from = TargetGroup,
              values_from = FirstDose) %>%
  replace(., is.na(.), 0) |> 
  mutate(all = adults +  `older adults` + children,
         unknown = 0)|> 
  dplyr::select(colnames(cov_raw_cleaned[[1]])) 







# Function to complete and interpolate vaccine TS
# Remove Germany Liechtenstein & Netherlands as only have total population with no age stratification
tmp_cnt <- cov_raw %>% 
  distinct(country) %>% 
  filter(country != "Germany" & country != "Liechtenstein" & country != "Netherlands") %>% 
  pull()

complete_vac <- function(i){
  
  tmp_1 <- cov_raw %>% 
    select(date, country, cnt, TargetGroup, cov_raw, origin) %>% 
    filter(TargetGroup == "18-60yr") %>% 
    mutate(max = max(cov_raw)) %>% 
    mutate(V_18_60_adj = cov_raw/max) %>% 
    rename(V_18_60 = cov_raw) %>% 
    select(1, 2, 3, 5, 8)
  
  tmp_2 <- cov_raw %>% 
    select(date, country, cnt, TargetGroup, cov_raw, origin) %>% 
    filter(TargetGroup == "60+yr") %>% 
    mutate(max = max(cov_raw)) %>% 
    mutate(V_60_adj = cov_raw/max) %>% 
    rename(V_60 = cov_raw) %>% 
    select(1, 2, 3, 5, 8)
  
  tmp_3 <- cov_raw %>% 
    select(date, country, cnt, TargetGroup, cov_raw, origin) %>% 
    filter(TargetGroup == "total") %>% 
    mutate(max = max(cov_raw)) %>% 
    mutate(V_tot_adj = cov_raw/max) %>% 
    rename(V_tot = cov_raw) %>% 
    select(1, 2, 3, 5, 8)
  
  tmp_ts <- full_join(tmp_1, tmp_2, by = c("country", "cnt", "date")) %>% 
    full_join(., tmp_3, by = c("country", "cnt", "date")) 
  
  tmp_ts %>% 
    filter(country == tmp_cnt[i]) %>% 
    complete(., date = seq.Date(min(date), as.Date("2021-09-30"), by = "day") ) %>% 
    
    mutate_at( vars(contains("V_")),imputeTS::na_interpolation) %>% 
    complete(., date = seq.Date(as.Date("2020-01-01"), as.Date("2021-09-30"), by = "day") )  %>% 
    mutate(country = tmp_cnt[i]) %>% 
    mutate(cnt = countrycode(country,
                             origin = "country.name", 
                             destination = "iso3c")) %>% 
    filter(date < as.Date("2021-10-01")) %>% 
    mutate_if(is.numeric, ~replace_na(., 0)) 
}

# Bind all countries together
vac <- map(1:length(tmp_cnt), complete_vac) %>% 
  bind_rows() 

# Save as .csv
write_csv(vac, "data/VAC_WHO.csv")


#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

# Compare datasets

# Plot Age group vaccine coverage (proportion of population vaccinated)
vac %>% 
  pivot_longer(cols = c(V_18_60, V_60, V_tot), names_to = "Age group", values_to = "Coverage") %>% 
  ggplot(aes(x = date))+
  geom_line(aes(y = Coverage, color = `Age group`))+
  facet_wrap(~country)+
  theme_bw()+
  scale_color_discrete(name = "Age group", labels = c("18 to 60", "60+", "Total population"))+
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")+
  labs(title = "WHO Vaccine Data", y = "Proportion of population vaccinated", x = "")+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8))

ggsave(filename = "figs/EURO_2/fig_s_WHO_vac.png",
       plot = last_plot(),
       width = 15,
       height = 10)

# Compare proprotion vaccinated between OWIN data and WHO vaccine data 
# Read in OWIN data 
OWIN <- read_csv("data/VAC_OWIN.csv") %>% 
  mutate(origin = "Our world in numbers") %>% 
  mutate(TargetGroup = "Total_OWIN")

# Plot comparison of total population coverage 
# Very similar results although OWIN n = 52, WHO n = 26
vac %>%
  mutate(origin = "WHO vaccine data") %>% 
  select(date, country, V_tot, origin) %>% 
  bind_rows(OWIN %>% 
              select(date, country, V_tot = V_all, origin)) %>% 
  ggplot(aes(x = date, y= V_tot))+
  geom_line(aes(color = origin))+
  facet_wrap(~country)+
  theme_bw()+
  scale_color_discrete(name = "Data source", labels = c("Our World in Numbers", "WHO"))+
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")+
  labs(title = "Total Population Vaccine Coverage Comparison", y = "Proportion of population vaccinated", x = "")+
  theme(axis.text.x = element_text(angle = 45,hjust=1, size = 8))

# Save
ggsave(filename = "figs/EURO_2/fig_s_cov_compare.png",
       plot = last_plot(),
       width = 15,
       height = 10)


### Function to find number of countries with data for each age grouping catagory
t_group <- who_vac_data %>% 
  select(TargetGroup) %>% 
  distinct() %>% 
  pull()

cnt_group <- function(i){
  who_vac_data %>% 
    mutate(country = countrycode(ReportingCountry,
                                 origin = "iso2c",
                                 destination = "country.name"))  %>% 
    filter(TargetGroup == t_group[i]) %>% 
    distinct(country) %>% 
    nrow() %>% 
    enframe() %>% 
    mutate(age_group = t_group[i])
}

map(1:length(t_group), cnt_group) %>% 
  bind_rows()
