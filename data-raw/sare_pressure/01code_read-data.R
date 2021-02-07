############################
# author: Gina (vnichols@iastate.edu)
# created: Oct 10 2019
# last modified: Oct 31 2019 (updated to Funcke from Funke)
#                Dec 19 2019 (adding boyd measurements)
#                march 27 2020 (updating based on britt's calcs)
#                may 8 2020 (changing file strucutre, cleaning up)
#                feb 7 2021 (moving to R package)
#
# purpose: read in raw pressure cell data and clean it up for calcs
#
# inputs: rd_pressure-cell-msmts
#
# outputs:
#
# notes
#
##############################


rm(list = ls())
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)



# plotkey -----------------------------------------------------------------

plotkey <- read_csv("data-raw/sare_plotkey/sare_plotkey.csv")

# read in excel sheets -----------------------------------------------------

stout <-
  read_excel("data-raw/sare_pressure/rd_pressure-cell-msmts.xlsx", sheet = "Stout", skip = 1) %>%
  filter(!is.na(code))

funcke <-
  read_excel("data-raw/sare_pressure/rd_pressure-cell-msmts.xlsx", sheet = "Funcke", skip = 1) %>%
  select(-orig_code) %>%
  filter(!is.na(code)) %>%
  mutate(atm1 = as.numeric(atm1))

boydgr <-
  read_excel("data-raw/sare_pressure/rd_pressure-cell-msmts.xlsx", sheet = "Boydgrain", skip = 1) %>%
  filter(!is.na(code)) %>%
  mutate_at(vars(contains("cm")), as.numeric) %>%
  mutate_at(vars(contains("_g")), as.numeric)

boydsil <-
  read_excel("data-raw/sare_pressure/rd_pressure-cell-msmts.xlsx", sheet = "Boydsilage", skip = 1) %>%
  filter(!is.na(code)) %>%
  mutate_at(vars(contains("cm")), as.numeric) %>%
  mutate_at(vars(contains("_g")), as.numeric) %>%
  select(-notes)


# fix things and combine ------------------------------------------------------

# funcke had two measurements at atm, combine them
funcke2 <-
  funcke %>%
  mutate(atm = (atm1 - cylinder_g) + (atm2 - cylinder_g) + cylinder_g) %>%
  select(-atm1, -atm2)

#p28 was 'clogged', eliminate it from boydsil
boydsil2 <-
  boydsil %>%
  filter(code != "B42-p28")


dat <- bind_rows(stout, funcke2, boydgr, boydsil2)

#--need to make codes the same as plotkey

sare_rawpresscells <-
  dat %>%
  separate(code, into = c("field_id", "x")) %>%
  mutate(field_id = case_when(
    field_id == "St" ~ "East",
    field_id == "B42" ~ "Central42",
    field_id == "F" ~ "West"),
    plot = case_when(
      (field_id == "East")|(field_id == "West") ~ str_sub(x, 1, 1),
      field_id == "Central42" ~ str_trim(str_sub(x, 2, 3))
    )
  ) %>%
  mutate(plot = as.numeric(plot)) %>%
  left_join(plotkey) %>%
  select(plot_id, satsamp_g:ring_g)


sare_rawpresscells %>%
  write_csv("data-raw/sare_pressure/sare_rawpresscells.csv")

use_data(sare_rawpresscells, overwrite = T)
