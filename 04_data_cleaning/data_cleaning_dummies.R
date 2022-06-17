# BRI dummy creation

library(tidyverse)
library(countrycode)
library(xtable)


# Notes ----
# Add: Yamal LNG, Kyaukpyu oil pipeline, Trans-Anatolian Pipeline (TANAP), Benban Solar Park Egypt (Hao et al. 2020)
#
# Transfrom into binary format
# create summary table

# Williams' data ----
setwd("C:/Users/Sahara Wenzel/OneDrive/Masterarbeit/Data")
# only completed projects
williams <- read.csv("./bri_energy_investments/energy_projects_williams.csv", header = TRUE, sep = ",", quote = "\"", 
                  dec = ".", fill = TRUE, comment.char = "")%>%
  as.data.frame()%>%
  filter(stage == 'Complete')#%>%
  #mutate(country = countrycode(country, origin = 'country.name', destination = 'country.name'))%>% 
  #mutate(iso = countrycode(country, origin = 'country.name', destination = 'iso3c'))%>%
  #mutate(region = countrycode(country, origin = 'country.name', destination = 'region'))


#all projects ----
williams_all <- read.csv(here::here("./bri_energy_investments/energy_projects_williams.csv"), header = TRUE, sep = ",", quote = "\"", 
                      dec = ".", fill = TRUE, comment.char = "")%>%
  as.data.frame()%>%
  rename(country = 'ï..country')%>%
  mutate(country = countrycode(country, origin = 'country.name', destination = 'country.name'))%>% 
  mutate(iso = countrycode(country, origin = 'country.name', destination = 'iso3c'))%>%
  mutate(region = countrycode(country, origin = 'country.name', destination = 'region'))



# Saving as excel to manually add years + sector variables according to research
library(writexl)
write_xlsx(williams, here::here("./bri_energy_investments/williams.clean.xlsx"))

williams <- readxl::read_xlsx("./bri_energy_investments/williams.clean.xlsx")%>%
  rename(year_op = year)%>%
  mutate(type = str_replace_all(type, "Petroleum", "Oil"))%>%
  mutate(type = str_replace_all(type, "Solar", "Alternative"))%>%
  mutate(type = str_replace_all(type, "Waste-to-energy", "Alternative"))%>%
  mutate(type = str_replace_all(type, "Wind", "Alternative"))

  
  

# AEI Data ----

list_of_sectors <- c("Energy","Transport")
list_of_subsectors <- c("Autos", "Aviation")

dummy_aei <- readxl::read_xlsx("./bri_investments/AEI-China-Global-Investment-Tracker-2020-Fall-FINAL.xlsx", sheet = "Dataset 1+2", skip = 5)%>%
  as.data.frame()%>%
  filter(BRI == 1 & Sector %in% list_of_sectors & !Subsector %in% list_of_subsectors)%>%
  subset(Year != 2020, select = -c(Greenfield, BRI))%>%
  mutate(year_op = Year +1) %>%
  rename(country = 'Country',
         region = 'Region',
         finance = 'Quantity in Millions',
         type = 'Subsector')%>%
  mutate(country = countrycode(country, origin = 'country.name', destination = 'country.name'))%>% 
  mutate(iso = countrycode(country, origin = 'country.name', destination = 'iso3c'))%>%
  mutate(region = countrycode(country, origin = 'country.name', destination = 'region'))
  

# Merge ----

dummy = plyr::rbind.fill(williams, dummy_aei)%>%
  subset(select = -c(xcoord, ycoord, source, comment, Year, Month))%>%
  mutate(region23 = countrycode(country, origin= 'country.name', destination = 'region23'))

# for sub-dummies, filter after this step

# as the dummies will change phase once one investment has been made, if there are 
# several observations of the same country, keep only the one with smallest year

# Table of investments by region

blub <- table(dummy$region23, dummy$type)[,c("Alternative","Hydro", "Electricity grid","Gas", "Oil","Coal")]
blub <- blub[order(rowSums(blub),decreasing=T),]
print(xtable(blub, type = "latex"), file = "Energy_investments_by_region.tex")

# This does not work yet
bla <- table(dummy$year_op, dummy$type)[,c("Alternative","Hydro", "Electricity grid","Gas", "Oil","Coal")]
bla <- bla[order(rowSums(bla),decreasing=T),]
print(xtable(bla, type = "latex"), file = "Energy_investments_by_year2.tex")


library(plyr)
dummy_total <- ddply(dummy, "country", function(df) return(df[df$year==min(df$year),]))

# There are still country doubles as there are some countries where multiple investments
# were made in the same year 
dummy_total <- dummy_total %>%
  filter(!is.na(country))%>%  #There was one observation with a country NA and no other values, dropping that one
  distinct(country, .keep_all = TRUE)%>%
  subset(country != "Myanmar (Burma)") # country double here, removing the one with later year of investment

# Fill in year data

dummy_total%>%
  group_by(country)%>%
  mutate(investment_dummy = ifelse(!is.na(year_op), 1,0))

#Set dummies manually because it took me too long to figure out how to do this in a pretty way


# load df back in
# check for manual mistakes with year < 2013 and !is.na(year_op) - Turkmenistan is an exception



# Create summary table -----

# This does not fucking work at all. Stargazer should be good for regression outputs though

library(sjPlot)

# retrieve value and variable labels
variables <- sji.getVariableLabels(dummy)
values <- sji.getValueLabels(dummy)
# simple frequency table
sjt.frq(efc$e42dep,
        variableLabels=variables['e42dep'],
        valueLabels=values[['e42dep']])



# Create binary dummy variables
write.csv(dummy,"C:/Users/Sahara Wenzel/OneDrive/Masterarbeit/Data/05_clean_data", row.names = FALSE)
writexl::write_xlsx(dummy_total,("./dummy_total.xlsx"))
