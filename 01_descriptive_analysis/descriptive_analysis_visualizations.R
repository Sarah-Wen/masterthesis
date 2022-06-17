
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(countrycode)
library(viridis)
library(readxl)
library(here)
library(xtable)
library(tidyr)
library(arsenal)
library(gt)
library(magrittr)
library(webshot)
theme_set(theme_bw())


world <- ne_countries(scale = 'medium', returnclass = 'sf')

world <- world%>%
  mutate(iso = countrycode(sovereignt, origin = 'country.name', destination = 'iso3c'))

aei_bri <- read_xlsx("C:/Users/Sahara Wenzel/OneDrive/Masterarbeit/Data/05_clean_data/aei_data_bri.xlsx")

# General energy investments by country ----
investments_quant <- aei_bri%>%
  group_by(country)%>%
  summarise(investments = n())%>%
  mutate(iso = countrycode(country, origin = 'country.name', destination = 'iso3c'))

investments_quant_merge <- merge(world, investments_quant, by = "iso", all.x = T)%>%
  subset(sovereignt != "Antarctica")


ggplot(data = investments_quant_merge) +
  geom_sf(aes(fill = investments)) +
  scale_fill_viridis(na.value = "lavenderblush3")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Chinese overseas energy and transport investments", subtitle = "Investments from 2005 - 2020, Source: American Enterprise Institute") +
  labs(fill = "Cumulative number of investments" )

ggsave(here("06_tables_and_plots/map_energy_investment.png"), width = 15, height = 5, dpi = 300)

# Energy investments (in millions) by country ----
investments_quant <- aei_bri%>%
  group_by(country)%>%
  summarise(investments = sum(quantity_in_millions))%>%
  mutate(iso = countrycode(country, origin = 'country.name', destination = 'iso3c'))

investments_quant_geom <- merge(world, investments_quant, by = "iso", all.x = T)%>%
  subset(sovereignt != "Antarctica")%>%
  filter(type != "Dependency")


ggplot(data = investments_quant_geom) +
  geom_sf(aes(fill = investments)) +
  scale_fill_viridis(na.value = "lavenderblush3",
                     breaks=seq(min(investments_quant_geom$investments, na.rm = T), max(investments_quant_geom$investments, na.rm = T),
                                (max(investments_quant_geom$investments, na.rm = T)-min(investments_quant_geom$investments, na.rm = T))/4))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("Chinese overseas energy and transport investments", subtitle = "Investments from 2005 - 2020, Source: American Enterprise Institute (2021)") +
  labs(fill = "Cumulative investments in million USD")

ggsave(here("06_tables_and_plots/map_energy_and_transport_investments_usd.png"), width=15, height = 5, dpi = 300)



# Tables of investment structures in major investment destinations ----

which.max(investments_quant$investments)
investments_quant[,28]

#Selecting ten major investment destinations
max_inv <- investments_quant[with(investments_quant,order(-investments)),]
max_inv <- max_inv[1:10,]%>%
  subset(select = c(country, investments))

# Calculating investments by sector
investments_share <- aei_bri%>%
  group_by(country, subsector)%>%
  summarise(investmentshare = sum(quantity_in_millions))%>%
  filter(country %in% max_inv$country)
  

investments_share$subsector <- as.factor(investments_share$subsector)
investments_share <- spread(investments_share, key = subsector, value = investmentshare)%>%
  mutate(test = rowSums(across(where(is.numeric)), na.rm=T))
investments_share <- investments_share[with(investments_share,order(-test)),]%>%
  subset(select = -test)


investments_share_percent <- investments_share%>%
  rename(
    Country = country,
    Unknown = '<NA>'
  )%>%
  ungroup() %>%
  mutate(across(where(is.numeric))/rowSums(across(where(is.numeric)), na.rm = T))%>%
  mutate_at(vars(-Country), funs(round(., 2)))
  

col_order <- c("Country" ,"Coal", "Oil", "Gas", "Hydro", "Alternative",
               "Rail", "Shipping", "Unknown" )
investments_share_percent <- investments_share_percent[, col_order]

# plain table
print(xtable(investments_share_percent), file = "sector_percentages_by_country")


# Heatmap table ----

full_val_range <- investments_share_percent %>% 
  drop_na()%>%
  ungroup %>%
  select_if(is.numeric) %>% 
  range

tab1 <- investments_share_percent%>%
  replace(is.na(.), 0)%>%
  gt() %>% 
  data_color(
    columns = c(Alternative, Hydro, Gas, Oil, Coal, Rail, Shipping, Unknown),
    colors = scales::col_numeric(
      palette = rev(RColorBrewer::brewer.pal(
        name = "BrBG", n=11)[c(3:6)]),
      domain = full_val_range)
  )

gtsave(tab1, "./06_tables_and_plots/heattable_investments_shares.png")

test <- aei_bri %>%
  subset(country == "Brazil")%>%
  group_by(subsector)%>%
  summarise(investments = sum(quantity_in_millions))%>%
  mutate(share = investments/sum(investments))


# BRI energy investments ----
bri_investments <- aei_bri%>%
  subset(bri == 1)%>%
  group_by(country)%>%
  summarise(investments = n())%>%
  mutate(iso = countrycode(country, origin = 'country.name', destination = 'iso3c'))

bri_intensity <- merge(world, bri_investments, by = "iso", all.x = T)%>%
  subset(sovereignt != "Antarctica")

ggplot(data = bri_intensity) +
  geom_sf(aes(fill = investments)) +
  scale_fill_viridis(na.value = "lavenderblush3")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Chinese BRI energy and transport investments", subtitle = "Investments from 2014 - 2020, Source: American Enterprise Institute") +
  labs(fill = "Cumulative number of investments")

ggsave(here("06_tables_and_plots/map_bri_investment_absolute.png"), dpi = 300)






# BRI energy investments by investment size ----

bri_investments_quant <- aei_bri%>%
  subset(bri == 1)%>%
  group_by(country)%>%
  summarise(investments = sum(quantity_in_millions))%>%
  mutate(iso = countrycode(country, origin = 'country.name', destination = 'iso3c'))

bri_intensity_quant <- merge(world, bri_investments_quant, by = "iso", all.x = T)%>%
  subset(sovereignt != "Antarctica")

#This could be prettier
ggplot(data = bri_intensity_quant) +
  geom_sf(aes(fill = investments)) +
  scale_fill_viridis(na.value = "lavenderblush3",
                     breaks=seq(min(bri_intensity_quant$investments, na.rm = T), max(bri_intensity_quant$investments, na.rm = T),
                     (max(bri_intensity_quant$investments, na.rm = T)-min(bri_intensity_quant$investments, na.rm = T))/4))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("Chinese BRI energy and transport investments", subtitle = "Investments from 2014 - 2020, Source: American Enterprise Institute (2021)") +
  labs(fill = "Cumulative investments in million USD")

ggsave(here("06_tables_and_plots/map_bri_investments_usd.png"), dpi = 300)


# BR energy investments by power capacity (CGPD data) ----
bri_investments_mw <- readxl::read_xlsx(here::here("05_clean_data/cgpd_data_bri.xlsx"))%>%
  #subset(bri == 1)%>%
  dplyr::group_by(country)%>%
  summarise(capacity = sum(capacity_mw))%>%
  mutate(iso = countrycode(country, origin = 'country.name', destination = 'iso3c'))

bri_intensity_mw <- merge(world, bri_investments_mw, by = "iso", all.x = T)%>%
  subset(sovereignt != "Antarctica")

ggplot(data = bri_intensity_mw) +
  geom_sf(aes(fill = capacity)) +
  scale_fill_viridis(na.value = "lavenderblush3",
                     breaks=seq(min(bri_intensity_mw$capacity, na.rm = T), max(bri_intensity_mw$capacity, na.rm = T),
                                (max(bri_intensity_mw$capacity, na.rm = T)-min(bri_intensity_mw$capacity, na.rm = T))/4))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("Chinese BRI investments in power production", subtitle = "Plants commissioned between 2014 - 2019, Source: CGPD (2021)") +
  labs(fill = "Cumulative investments in MW capacity")

ggsave(here("06_tables_and_plots/map_bri_investments_mw.png"), width = 9, height =  7, dpi = 400)


