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
library(ggplot2)

# read in data -----------------------------------------------------

plotkey <- read_csv("data-raw/sare_plotkey/sare_plotkey.csv")

datraw <-
  read_csv("data-raw/sare_pressure/sare_rawpresscells.csv") %>%
  mutate(soilvol_cm3 = 347.5,
         bulkden_gcm3 = drysoil_g / soilvol_cm3
  ) # assume volume of soil sample is 347.50 cm3 (? is this right???)


# water -------------------------------------------------------------------

# calculate water amts (subtract cylinder wgts),

dat_water <-
  datraw %>%
    mutate(   ##--# calc actual amount of water released at each pressure
    w_0cm_g = 0, ##-britt adds this to 2.5 atm value, keep a 0 as a place-holder
    w_2.5cm_g = atm - cylinder_g + satwater_g,
    w_10cm_g = `10_cm` - cylinder_g,
    w_25cm_g = `25_cm` - cylinder_g,
    w_50cm_g = `50_cm` - cylinder_g,
    w_100cm_g = `100_cm` - cylinder_g,
    w_200cm_g = `200_cm` - cylinder_g,
    w_500cm_g = `500_cm` - cylinder_g,
    w_999cm_g = sampafter500_g - ringpluscrap_g - drysoil_g
  ) %>%
  mutate(  #--total amt of water released
    w_tot_g = purrr::reduce(select(., starts_with("w")), `+`)
  ) %>%
  select(plot_id, soilvol_cm3, bulkden_gcm3, starts_with("w"))

dat_water %>%
  ggplot(aes(plot_id, w_tot_g)) +
  geom_col()


# bulk density ------------------------------------------------------------
#--just to keep things separated
dat_soil <-
  datraw %>%
  select(plot_id, soilvol_cm3, drysoil_g, bulkden_gcm3)


# porosity ----------------------------------------------------------------

# via britt and mineral methods

myminden <- 2 #--not sure what this is for...

dat_poros <-
  datraw %>%
  # calc porosity via britt
  mutate(
    soil_at_sat_g = (satsamp_g - ringpluscrap_g),
    water_at_sat_g = soil_at_sat_g - drysoil_g,
    air_cm3 = water_at_sat_g,
    poros_britt = air_cm3/soilvol_cm3,
    poros_mineral = 1 - bulkden_gcm3/myminden #--this must be an assumption
    ) %>%
  select(plot_id, drysoil_g, bulkden_gcm3,
         water_at_sat_g, air_cm3, poros_britt, poros_mineral)

#--compare the two methods for calculating porosity
dat_poros  %>%
  ggplot(aes(poros_mineral, poros_britt)) +
  geom_point() +
  geom_abline()


# soil water contents at each pressure ------------------------------------

# cumulative amount of water purged at each pressure
dat_cum <-
  dat_water %>%
  select(-w_999cm_g) %>%
  # gather into long form
  pivot_longer(w_0cm_g:w_500cm_g,
               names_to = "press_cm",
               values_to = "water_g") %>%
  separate(press_cm,
           into = c("water", "press_cm", "grams"),
           sep = "_") %>%
  # get pressure as a numeric value
  mutate(press_cm = as.numeric(str_sub(press_cm, 1, -3))) %>%
  group_by(plot_id) %>%
  arrange(plot_id, press_cm) %>%
  # get cumulative water
  mutate(cumwater_g = cumsum(water_g)) %>%
  select(plot_id, press_cm, water_g, cumwater_g)

dat_cum %>%
  ggplot(aes(press_cm, cumwater_g, color = plot_id)) +
  geom_line()

# convert this water amount to a volumetric/gravimetric water content
dat_theta <-
  dat_cum %>%
  left_join(dat_poros) %>%
  left_join(dat_soil) %>%
  mutate(
    water_inside_soil_still = water_at_sat_g - cumwater_g,
    #--calculate using gravimetric water and bulk density
    gtheta = water_inside_soil_still / drysoil_g,
    vtheta1 = gtheta * bulkden_gcm3,
    frac_water = cumwater_g/water_at_sat_g,
    vtheta2a = (1 - frac_water)*poros_britt,
    vtheta2b = (1 - frac_water)*poros_mineral
    ) %>%
  select(plot_id, press_cm, vtheta1, vtheta2a, vtheta2b)



dat_theta %>%
  pivot_longer(vtheta1:vtheta2b) %>%
  ggplot(aes(press_cm, value, group = interaction(plot_id, name))) +
  geom_line(aes(color = name)) +
  facet_grid(.~name) +
  labs(title = "gtheta method = red\nbritt method = green\nmineral method = blue")

# so vetha1 and vtheta2a are the same

#--write final values to data frame

sare_pressure <-
  dat_theta %>%
  rename(vtheta_poros1 = vtheta2a,
         vtheta_poros2 = vtheta2b)

sare_pressure %>% write_csv("data-raw/sare_pressure/sare_pressure.csv")

use_data(sare_pressure, overwrite = T)
