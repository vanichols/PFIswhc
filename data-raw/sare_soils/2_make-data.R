## Put soils data together for easy access

rm(list = ls())

#--packages we already know
library(readr)
library(dplyr)
library(janitor)
library(usethis)


keith <- read_csv("data-raw/sare_soils/keith.csv")
rob <- read_csv("data-raw/sare_soils/rob.csv")
jim <- read_csv("data-raw/sare_soils/jim.csv")


sare_soils_all <-
  keith %>%
  bind_rows(rob) %>%
  bind_rows(jim)

sare_soils <-
  sare_soils_all %>%
  mutate(wgt_slope = wgt*slopegradwta,
         wgt_wt = wgt*wtdepannmin) %>%
  group_by(site_name) %>%
  summarise(wgt_slope = sum(wgt_slope),
            wgt_wtcm = sum(wgt_wt)) %>%
  mutate_if(is.numeric, round, 2) %>%
  left_join(
    sare_soils_all %>%
      group_by(site_name) %>%
      filter(wgt == max(wgt)) %>%
      select(site_name, muname) %>%
      rename("dom_mu" = muname)
  )


sare_soils %>% write_csv("data-raw/sare_soils/sare_soils.csv")

use_data(sare_soils, overwrite = T)
