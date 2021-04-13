## Put soils data together for easy access

rm(list = ls())

#--packages we already know
library(readr)
library(dplyr)
library(janitor)
library(usethis)


keith <- read_csv("data-raw/sare_soils/keith.csv") %>%
  mutate(site_name = "Central")
rob <- read_csv("data-raw/sare_soils/rob.csv") %>%
  mutate(site_name = "East")


sare_soils <-
  keith %>%
  bind_rows(rob) %>%
  select(site_name, wgt_slope, wgt_wtcm)

sare_soils %>% write_csv("data-raw/sare_soils/sare_soils.csv")

use_data(sare_soils, overwrite = T)
