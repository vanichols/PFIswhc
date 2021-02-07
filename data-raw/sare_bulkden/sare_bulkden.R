############################
# author: Gina (vnichols@iastate.edu)
# created: Oct 10 2019
# last modified: Oct 21 2019 (do calcs, oh ASA...)
#                march 27 2020 (update based on britt's calcs)
#                march 30 2020 (made sure it runs for britt)
#                may 8 2020 (rearrange folders, make sure it still runs)
#
# purpose: calculate things in data
#
# inputs: pp_pressure-cells, rd_euIDs
#
# outputs:
#
# notes: for porosity calcs used: http://lawr.ucdavis.edu/classes/SSC100/probsets/pset01.html
#
##############################


rm(list = ls())
library(readxl)
library(dplyr)
library(tidyr)
library(readxl)

# read in data -----------------------------------------------------

plotkey <- read_csv("data-raw/sare_plotkey/sare_plotkey.csv")

datraw <-
  read_csv("data-raw/sare_pressure/sare_rawpresscells.csv") %>%
  mutate(soilvol_cm3 = 347.5,
         bulkden_gcm3 = drysoil_g / soilvol_cm3
  ) # assume volume of soil sample is 347.50 cm3 (? is this right???)


# soil data ---------------------------------------------------------------

dat_soil <-
  datraw %>%
  select(code, soilvol_cm3, drysoil_g, bulkden_gcm3)

# porosity ----------------------------------------------------------------

# via britt and mineral methods

dat_poros <-
  datraw %>%
  # calc porosity via britt
  mutate(
    soil_at_sat_g = (satsamp_g - ringpluscrap_g),
    water_at_sat_g = soil_at_sat_g - drysoil_g,
    air_cm3 = water_at_sat_g,
    poros_britt = air_cm3/soilvol_cm3,
    poros_mineral = 1 - bulkden_gcm3/myminden
    ) %>%
  select(code, drysoil_g, bulkden_gcm3,
         water_at_sat_g, air_cm3, poros_britt, poros_mineral)

#--look at it
dat_poros  %>%
  ggplot(aes(poros_mineral, poros_britt)) +
  geom_point() +
  geom_abline()

