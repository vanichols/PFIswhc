## Get soil series (?), wt depth, and slope for each site
## used DADS class code as base

rm(list = ls())

#--packages we already know
library(sf)
library(ggplot2)
library(readr)
library(ggplot2)
library(dplyr)
library(janitor) #--a package that makes cleaning data easier
library(stringr) #--a tidyverse style package for dealing with character strings

#### KEITH ########################################################################################

# visualilze, correct? ----------------------------------------------------

srgo_shp <- st_read("data-raw/sare_soils/KeithKohler-Boone-Co/spatial/soilmu_a_aoi.shp") %>%
  clean_names() #--again, people who make spatial data like all upper-case, this just fixes that

ggplot() +
  geom_sf(data = srgo_shp, aes(fill = musym)) +
  labs(title = "Keith, Boone Co")

ggsave("data-raw/sare_soils/fig_keith-soils.png")

# Spatial_Domain:
#   Bounding_Coordinates:
#   West_Bounding_Coordinate: -94.17
# East_Bounding_Coordinate: -93.69
# North_Bounding_Coordinate: 42.21
# South_Bounding_Coordinate: 41.86



# get map unit names and amounts ------------------------------------------

#--freaking no column names ?! used dad's data
mu_datraw <- read.table("data-raw/sare_soils/KeithKohler-Boone-Co/tabular/mapunit.txt", header = F, sep = "|")
names(mu_datraw) <- names(read_csv("data-raw/sare_soils/SSURGO_mapunit-colnames.csv"))

#--what map unit dominates the area?
mu_dat <-
  mu_datraw %>%
  remove_empty("cols")  %>% #--removes empty columns
  mutate(wgt = muacres/sum(muacres)) %>%
  select(muname, musym, mukey, wgt) %>%
  arrange(-wgt)

# 2. water table depth and slopes from mapunit aggregate data (muaggatt)
wtab_datraw <- read.table("data-raw/sare_soils/KeithKohler-Boone-Co/tabular/muaggatt.txt", header = F, sep = "|")
names(wtab_datraw) <- names(read_csv("data-raw/sare_soils/SSURGO_muaggatt-colnames.csv"))

wtab_dat <-
  wtab_datraw %>%
  remove_empty("cols")  %>%
  select(musym, muname, mukey, slopegradwta, wtdepannmin)

# 3. combine them

keith <-
  mu_dat %>%
  left_join(wtab_dat) %>%
  mutate(site_name = "Central") %>%
  select(site_name, wgt, muname, slopegradwta, wtdepannmin)

keith %>%  write_csv("data-raw/sare_soils/keith.csv")


#### STOUT ########################################################################################

# visualilze, correct? ----------------------------------------------------

srgo_shp <- st_read("data-raw/sare_soils/RobStout-Washington-Co/spatial/soilmu_a_aoi.shp") %>%
  clean_names() #--again, people who make spatial data like all upper-case, this just fixes that

ggplot() +
  geom_sf(data = srgo_shp, aes(fill = musym)) +
  labs(title = "Rob, Washington Co")

ggsave("data-raw/sare_soils/fig_rob-soils.png")

# Spatial_Domain:
#   Bounding_Coordinates:
#   West_Bounding_Coordinate: -91.947
# East_Bounding_Coordinate: -91.484
# North_Bounding_Coordinate: 41.512
# South_Bounding_Coordinate: 41.161


# get map unit names and amounts ------------------------------------------

mu_datraw <- read.table("data-raw/sare_soils/RobStout-Washington-Co/tabular/mapunit.txt", header = F, sep = "|")
names(mu_datraw) <- names(read_csv("data-raw/sare_soils/SSURGO_mapunit-colnames.csv"))

#--what map unit dominates the area?
mu_dat <-
  mu_datraw %>%
  remove_empty("cols")  %>% #--removes empty columns
  mutate(wgt = muacres/sum(muacres)) %>%
  select(muname, musym, mukey, wgt) %>%
  arrange(-wgt)

# 2. water table depth and slopes from mapunit aggregate data (muaggatt)
wtab_datraw <- read.table("data-raw/sare_soils/RobStout-Washington-Co/tabular/muaggatt.txt", header = F, sep = "|")
names(wtab_datraw) <- names(read_csv("data-raw/sare_soils/SSURGO_muaggatt-colnames.csv"))

wtab_dat <-
  wtab_datraw %>%
  remove_empty("cols")  %>%
  select(musym, muname, mukey, slopegradwta, wtdepannmin)

# 3. combine them

rob <-
  mu_dat %>%
  left_join(wtab_dat) %>%
  mutate(site_name = "East") %>%
  select(site_name, wgt, muname, slopegradwta, wtdepannmin)

rob %>% write_csv("data-raw/sare_soils/rob.csv")


#### JIM ########################################################################################


# visualilze, correct? ----------------------------------------------------

srgo_shp <- st_read("data-raw/sare_soils/JimFunke-Greene-Co/spatial/soilmu_a_aoi.shp") %>%
  clean_names() #--again, people who make spatial data like all upper-case, this just fixes that

ggplot() +
  geom_sf(data = srgo_shp, aes(fill = musym)) +
  labs(title = "Jim, Greene Co")

ggsave("data-raw/sare_soils/fig_jim-soils.png")

# Spatial_Domain:
#   Bounding_Coordinates:
#   West_Bounding_Coordinate: -94.630
# East_Bounding_Coordinate: -94.164
# North_Bounding_Coordinate: 42.210
# South_Bounding_Coordinate: 41.863

# get map unit names and amounts ------------------------------------------

mu_datraw <- read.table("data-raw/sare_soils/JimFunke-Greene-Co/tabular/mapunit.txt", header = F, sep = "|")
names(mu_datraw) <- names(read_csv("data-raw/sare_soils/SSURGO_mapunit-colnames.csv"))

#--what map unit dominates the area?
mu_dat <-
  mu_datraw %>%
  remove_empty("cols")  %>% #--removes empty columns
  mutate(wgt = muacres/sum(muacres)) %>%
  select(muname, musym, mukey, wgt) %>%
  arrange(-wgt)

# 2. water table depth and slopes from mapunit aggregate data (muaggatt)
wtab_datraw <- read.table("data-raw/sare_soils/JimFunke-Greene-Co/tabular/muaggatt.txt", header = F, sep = "|")
names(wtab_datraw) <- names(read_csv("data-raw/sare_soils/SSURGO_muaggatt-colnames.csv"))

wtab_dat <-
  wtab_datraw %>%
  remove_empty("cols")  %>%
  select(musym, muname, mukey, slopegradwta, wtdepannmin)

# 3. combine them

jim <-
  mu_dat %>%
  left_join(wtab_dat) %>%
  mutate(site_name = "West") %>%
  select(site_name, wgt, muname, slopegradwta, wtdepannmin)

jim %>% write_csv("data-raw/sare_soils/jim.csv")
