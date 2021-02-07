############################
# author: Gina (vnichols@iastate.edu)
# created: Oct 10 2019
# last modified: Oct 21 2019 (do calcs, oh ASA...)
#                march 27 2020 (update based on britt's calcs)
#                march 30 2020 (made sure it runs for britt)
#                may 8 2020 (rearrange folders, make sure it still runs)
#                feb 7 2021 (moved to package)
#
# purpose: calculate things in data
#
# inputs: sare_rawpresscells, rd_euIDs
#
# outputs:
#
# notes: for porosity calcs used: http://lawr.ucdavis.edu/classes/SSC100/probsets/pset01.html
#
##############################


rm(list = ls())
library(broom)
library(readxl)
library(readr)
library(dplyr)

# read in data -----------------------------------------------------

key <- read_csv("data-raw/sare_eus/rd_euIDs.csv")

sare_plotkey <-
  key %>%
  select(-site_desc) %>%
  mutate(site_name = case_when(
           site_name == "Boyd42" ~ "Central42",
           site_name == "Boyd44" ~ "Central44",
           site_name == "Funcke" ~ "West",
           site_name == "Stout" ~ "East"),
         site = gsub("[[:digit:]]", "", site_name),
         sys_trt = case_when(
           sys_trt == "sil" ~ "silage",
           sys_trt == "gr" ~ "grain"
         )) %>%
  rename("plot_id" = code, "field_id" = site_name) %>%
  select(plot_id, site, field_id, sys_trt, crop_trt, cc_trt, rep, plot, plot_id)

sare_plotkey %>% write_csv("data-raw/sare_eus/sare_plotkey.csv")

use_data(sare_plotkey, overwrite = T)
