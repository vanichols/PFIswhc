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
library(stringr)
library(readr)
library(usethis)

# read in data -----------------------------------------------------

plotkey <- read_csv("data-raw/sare_plotkey/sare_plotkey.csv")

#note: rings are 3 inch diameter, and 3 inches tall (7.62 cm)
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
    w_0cm_g = satsamp_g - satwater_g - drysoil_g - ringpluscrap_g, #--added 5/6, is this right?
    w_3.8cm_g = atm - cylinder_g + satwater_g,
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


#--different approach 5/6
#--I have two ways of calculating water at saturation.
#--1. I can sum together all of the water released
#--2. I can take the wgt at saturation and subtract the ring, soil, crap, and water left in cup after removing it
#--sometimes the value from 1. is higher (more water released than I had to begin with)
#--someteimes the value from 2. is higher (makes more sense, I didn't measure all of the water exactly)
# what are the implications of using one over the other?
datraw %>%
  mutate(   ##--# calc actual amount of water released at each pressure
    w_0cm_g = satsamp_g - satwater_g - drysoil_g - ringpluscrap_g, #--added 5/6, is this right?
    w_3.8cm_g = atm - cylinder_g + satwater_g,
    w_10cm_g = `10_cm` - cylinder_g,
    w_25cm_g = `25_cm` - cylinder_g,
    w_50cm_g = `50_cm` - cylinder_g,
    w_100cm_g = `100_cm` - cylinder_g,
    w_200cm_g = `200_cm` - cylinder_g,
    w_500cm_g = `500_cm` - cylinder_g,
    w_999cm_g = sampafter500_g - ringpluscrap_g - drysoil_g
  ) %>%
  select(plot_id, cell_nu, starts_with("w")) %>%
    pivot_longer(w_3.8cm_g:w_999cm_g) %>%
  group_by(plot_id, cell_nu, w_0cm_g) %>%
  mutate(cumval = cumsum(value)) %>%
    filter(name == "w_999cm_g") %>%
    ggplot() +
    geom_point(aes(plot_id, w_0cm_g), color = "black") +
    geom_point(aes(plot_id, cumval), color = "red") +
    coord_flip()

#--use method #2 for now (emailed britt)
dat_water <-
  datraw %>%
    mutate(   ##--# calc actual amount of water released at each pressure
      w_0cm_g = satsamp_g - satwater_g - drysoil_g - ringpluscrap_g, #--added 5/6, is this right?
      w_3.8cm_g = atm - cylinder_g + satwater_g,
      w_10cm_g = `10_cm` - cylinder_g,
      w_25cm_g = `25_cm` - cylinder_g,
      w_50cm_g = `50_cm` - cylinder_g,
      w_100cm_g = `100_cm` - cylinder_g,
      w_200cm_g = `200_cm` - cylinder_g,
      w_500cm_g = `500_cm` - cylinder_g,
      w_999cm_g = sampafter500_g - ringpluscrap_g - drysoil_g
    ) %>%
    select(plot_id, cell_nu, starts_with("w")) %>%
    pivot_longer(w_3.8cm_g:w_999cm_g) %>%
    group_by(plot_id, cell_nu, w_0cm_g) %>%
    mutate(cumval = cumsum(value),
           water_content_g = w_0cm_g - cumval) %>%
    filter(name != "w_999cm_g") %>%
    select(-value, -cumval) %>%
    pivot_wider(names_from = name, values_from = water_content_g)  %>%
    select(-cell_nu) %>%
    pivot_longer(w_0cm_g:w_500cm_g,
             names_to = "press_cm",
             values_to = "water_g") %>%
  separate(press_cm,
           into = c("water", "press_cm", "grams"),
           sep = "_") %>%
  # get pressure as a numeric value
  mutate(press_cm = as.numeric(str_sub(press_cm, 1, -3))) %>%
  ungroup() %>%
  select(plot_id, press_cm, water_g)



dat_water %>%
  ggplot(aes(press_cm, water_g)) +
  geom_line(aes(color = plot_id))

# bulk density ------------------------------------------------------------
#--just to keep things separated
dat_soil <-
  datraw %>%
  select(plot_id, soilvol_cm3, drysoil_g, bulkden_gcm3)


# porosity ----------------------------------------------------------------

# via britt and mineral methods

myminden <- 2 #--not sure what this is for...
dat_water %>% filter(press_cm == 0)

dat_poros <-
  datraw %>%
  # calc porosity via britt
  mutate(
    soil_at_sat_g = (satsamp_g - ringpluscrap_g - satwater_g),
    water_at_sat_g = soil_at_sat_g - drysoil_g, #--same as calcs in dat_water
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

#--basically the same

# soil water contents at each pressure ------------------------------------

#--not doing this any more, using the value at saturation as baseline instead, see below


# cumulative amount of water purged at each pressure
# dat_cum <-
#   dat_water %>%
#   select(-w_999cm_g) %>%
#   # gather into long form
#   pivot_longer(w_0cm_g:w_500cm_g,
#                names_to = "press_cm",
#                values_to = "water_g") %>%
#   separate(press_cm,
#            into = c("water", "press_cm", "grams"),
#            sep = "_") %>%
#   # get pressure as a numeric value
#   mutate(press_cm = as.numeric(str_sub(press_cm, 1, -3))) %>%
#   group_by(plot_id) %>%
#   arrange(plot_id, press_cm) %>%
#   # get cumulative water
#   mutate(cumwater_g = cumsum(water_g)) %>%
#   select(plot_id, press_cm, water_g, cumwater_g)
#
# dat_cum %>%
#   ggplot(aes(press_cm, cumwater_g, color = plot_id)) +
#   geom_line()


# convert this water amount to a volumetric/gravimetric water content
# dat_theta <-
#   dat_cum %>%
#   left_join(dat_poros) %>%
#   left_join(dat_soil) %>%
#   mutate(
#     water_inside_soil_still = water_at_sat_g - cumwater_g,
#     #--calculate using gravimetric water and bulk density
#     gtheta = water_g / drysoil_g,
#     vtheta1 = gtheta * bulkden_gcm3,
#     frac_water = cumwater_g/water_at_sat_g,
#     vtheta2a = (1 - frac_water)*poros_britt,
#     vtheta2b = (1 - frac_water)*poros_mineral
#     ) %>%
#   select(plot_id, press_cm, vtheta1, vtheta2a, vtheta2b)
#
#
#
# dat_theta %>%
#   pivot_longer(vtheta1:vtheta2b) %>%
#   ggplot(aes(press_cm, value, group = interaction(plot_id, name))) +
#   geom_line(aes(color = name)) +
#   facet_grid(.~name) +
#   labs(title = "gtheta method = red\nbritt method = green\nmineral method = blue")

# so vetha1 and vtheta2a are the same


# convert this water amount to a volumetric/gravimetric water content
dat_theta <-
  dat_water %>%
  left_join(dat_poros) %>%
  left_join(dat_soil) %>%
  mutate(
    #water_inside_soil_still = water_at_sat_g - cumwater_g,
    #--calculate using gravimetric water and bulk density
    gtheta = water_g / drysoil_g,
    vtheta = gtheta * bulkden_gcm3,
    #frac_water = cumwater_g/water_at_sat_g,
    #vtheta2a = (1 - frac_water)*poros_britt,
    #vtheta2b = (1 - frac_water)*poros_mineral
  ) %>%
  select(plot_id, press_cm, vtheta)




#--write final values to data frame

#--OLD, before using saturation msmt directly
#--change 0 pressure to half core height (true pressure)
# sare_pressure <-
#   dat_theta %>%
#   mutate(press_cm = ifelse(press_cm ==0, 0.038, press_cm)) %>%
#   rename("vtheta_grav" = vtheta1,
#          "vtheta_mnrl" = vtheta2b) %>%
#   select(plot_id, press_cm, vtheta_grav, vtheta_mnrl)

#--NEW
sare_pressure <-
  dat_theta

sare_pressure %>% write_csv("data-raw/sare_pressure/sare_pressure.csv")

use_data(sare_pressure, overwrite = T)



# separate, gallons -------------------------------------------------------


#--find gallons, for blog post
sare_gallons <-
  dat_cum %>%
  left_join(dat_poros) %>%
  mutate(
    water_inside_soil_still = water_at_sat_g - cumwater_g) %>%
  select(plot_id, press_cm, water_inside_soil_still) %>%
  mutate(gal_cm3 = water_inside_soil_still * (1/347.5) * (1/3785.41),
         gal_3inac = gal_cm3 * (10000) * (4046.86)) %>%
  select(plot_id, press_cm, gal_3inac)

sare_gallons %>% write_csv("data-raw/sare_pressure/sare_gallons.csv")

use_data(sare_gallons, overwrite = T)
