## Script for getting a soil profile using R
## Agron493/593
##
## Authors: Gina Nichols (vnichols@iastate.edu) and Fernando Miguez (femiguez@iastate.edu)
## Date: 2020-03-01
##
## Goals: Illustrate how to get and work with SSURGO data in R
## Steps: 1. Create a spatail polygon for where you want soils data
##        2. Download data using the FedData package
##        3. Read in shp file and csv files (mapunit, component and chorizon)
##        4. Visualize the shp file using ggplot2
##        5. Merge csv file information with shp file
##        6. Try overlaying yield map and soils data of interest
##

#--packages we already know
library(sf)
library(ggplot2)
library(readr)
library(ggplot2)
library(dplyr)

#--these might be new to you, you may have to install them first
library(janitor) #--a package that makes cleaning data easier
library(stringr) #--a tidyverse style package for dealing with character strings

#--this is new to you! Please install it.
library(FedData)


# Step 1: Define a spatial polygon based on the ym data -------------------

## Step 1A: We create a rectangle with coordinates

# First we need to make a tibble that shows our polygon

#  point4 <--------------point3
#    |                     ^
#    |                     |
#    |                     |
#    v                     |
#   point1 ------------> point2


# Jim Funke's coordinates:

# Bounding_Coordinates:
# West_Bounding_Coordinate: -94.630
# East_Bounding_Coordinate: -94.164
# North_Bounding_Coordinate: 42.210
# South_Bounding_Coordinate: 41.863

lon1 <- -94.63 # lon min
lon2 <- -94.164 # lon max
lat2 <- 42.210 #lat max
lat1 <- 41.863 #lat min


aoi_poly <- tibble(
  point_no = c(1, 2, 3, 4, 5),
  lon = c(lon1, lon2, lon2, lon1, lon1),
  lat = c(lat1, lat1, lat2, lat2, lat1)
  )

aoi_poly


## Step 1B: We create a spatial polygon

# The sf package needs a 2 column matrix
# NOTE: the first col should be lon, the second lat
aoi_mat <- aoi_poly %>% select(-point_no) %>% as.matrix


# We use the Polygon function to turn the matrix into a polygon
pg <- Polygon(aoi_mat)

# Now we make it a spatial polygon (spg) by giving it a projection type.
# Don't worry about understanding this code
spg <- SpatialPolygons(list(Polygons(list(pg), "s1")),
                         proj4string = CRS("+proj=longlat +datum=WGS84"))

## Convert it to an sf object to use ggplot2
spg_sf <- st_as_sf(spg)

# let's see if our polygon makes a rectangle (lat/lons should apear on axes)
# ggplot() +
#   geom_sf(data = spg_sf, fill = "red")


# Step 2: Use FedData to download data ------------------------------------

# Use function 'get_ssurgo' from 'FedData' package
# we decided to call it CIA is for 'Central Iowa'. You can choose another label if you want.
# NOTE: our template is the 'SpatialPolygon' (spg) we created, NOT the spg_sf we made a figure with
# NOTE: This takes some time to run. It also might fail a couple times. That's ok, keep trying.
aoi_soil <- get_ssurgo(template = spg, label = "Funke")


# Step 3: Read in shp and csv files-----------------------------------------------------------------

## Step 3A: Read in and plot shp file using 'sf' package again
srgo_shp <- st_read("EXTRACTIONS/CIA/SSURGO/CIA_SSURGO_Mapunits.shp") %>%
  clean_names() #--again, people who make spatial data like all upper-case, this just fixes that
# Look at the data:
srgo_shp

# Visualize it using ggplot2 again
ggplot() +
  geom_sf(data = srgo_shp, aes(fill = mukey))
## Great. This looks like a soils map. But what is 403836?

############################### STOP##################################################
# Let's talk.
############################### STOP##################################################


## Step 3B: Read in 'csv' files which contain the actual soils data
# 1. mapunit descriptions
mu_dat <- read_csv("EXTRACTIONS/CIA/SSURGO/CIA_SSURGO_mapunit.csv") %>%
  remove_empty("cols") #--removes empty columns

# 2. chorizon
#  Horizon depths, sand, silt, clay, organic matter, water holding capacity, etc.
# .r means reference value, .l means low value, .h means high value
chor_dat <- read_csv("EXTRACTIONS/CIA/SSURGO/CIA_SSURGO_chorizon.csv") %>%
  remove_empty("cols")

# 3. component, tells what each mukey is 'made up of'
cmpnt_dat <- read_csv("EXTRACTIONS/CIA/SSURGO/CIA_SSURGO_component.csv") %>%
  remove_empty("cols")


# Which data has info we want?
# NOTE: mu_dat doesn't have anything we want to visualize, so we won't use it.

# Step 4: Combine shp and select info from csvs ---------------------------
# Let's add information from the csvs to the shp file so we can visualize them

## Step 4A: First we simplify the data so it is easier to work with

srgo_shp_simp <-
  srgo_shp %>%
  mutate(mukey = as.character(mukey)) #--we don't want it to be numeric

cmpnt_simp <- cmpnt_dat %>%
  select(compname, comppct.r, mukey, cokey) %>%
  mutate(mukey = as.character(mukey)) #--again, we don't want it to be numeric

#--this is where we use the stringr package function 'str_detect'
chor_simp <- chor_dat %>%
  select(hzname, om.r, cokey, chkey) %>% #--let's just do organic matter for now
  filter(str_detect("A", hzname)) #--let's only look at the A horizon (the top layer of soil)

########################STOP#################################
# NOTE: Why did we pick om.r? What does .r mean?
########################STOP#################################

chor_dat %>%
  select(hzname, om.r, om.h, om.l, cokey, chkey) %>% #--let's just do organic matter for now
  filter(str_detect("A", hzname))



srgo_shp_simp
cmpnt_simp
chor_simp

## Step 4B: Get one organic matter value for each mukey

# Combine the components with the om% data

chor_cmpnt_simp <-
  cmpnt_simp %>% #--has components of each mukey
  left_join(chor_simp) #--has organic matter for each component



# You'll notice the comppct.r doesn't add up to 100% for each mukey
# Let's just keep the component that is most dominant in the mukey

om_simp <-
  chor_cmpnt_simp %>%
  # remember how to filter out nas?
  filter(!is.na(chkey)) %>%
  group_by(mukey) %>%
  filter( comppct.r == max(comppct.r))


# Next we join the above data with our spatial data

new_srgo <-
  srgo_shp_simp %>%
  left_join(om_simp, by = "mukey")


# Step 5: Visualize soils data + yield data -------------------------------

# Now we can overlay the yield map data with the soils data

#--Yield
new_srgo %>%
  ggplot() +
  geom_sf(aes(fill = om.r)) +
  scale_fill_distiller(palette = rev("YlOrBr")) +
  geom_sf(data = yld_map, aes(color = yld_vol_dr)) +
  scale_color_distiller(palette = "Greens")

#--Grain moisture
new_srgo %>%
  ggplot() +
  geom_sf(aes(fill = om.r)) +
  scale_fill_distiller(palette = rev("YlOrBr")) +
  geom_sf(data = yld_map, aes(color = moisture)) +
  scale_color_distiller(palette = "Blues")


# Note: the yield nor the grain moisture seems to be affected by the soil type.
# Do you remember the warning the SSURGO website gave us when we got the soils for Agronomy Hall?
# What scale are we supposed to interpret the information at?
# What scale are we operating at?
# Remember 0.001 degrees is roughly 100 m
# How many meters across is our field?
