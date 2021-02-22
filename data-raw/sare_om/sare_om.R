# created 2/18/2021
# purpose: process raw ellsworth data
# last updated:

rm(list = ls())
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(usethis)

# raw data ----------------------------------------------------------------

datraw <- read_csv("data-raw/sare_om/VIRGINIA_NICHOLS_ISU_S0215-221.csv")

dkey <- read_csv("data-raw/sare_texture/template_texture-msmts.csv") %>% select(-labeled) %>% mutate(Sample = textsamp_id)

plotkey <- read_csv("data-raw/sare_plotkey/sare_plotkey.csv")

# wrangle data ------------------------------------------------------------

sare_om <-
  datraw %>%
  rename("textsamp_id" = samp_id) %>%
  select(-starts_with("x"), -date) %>%
  left_join(dkey) %>%
  janitor::clean_names() %>%
  select(code, ph, om, potassium, phos) %>%
  rename("plot_id" = code) %>%
  select(plot_id, everything())

sare_om %>% write_csv("data-raw/sare_om/sare_om.csv")
usethis::use_data(sare_om, overwrite = T)


# viz check ---------------------------------------------------------------

dat %>%
  left_join(plotkey) %>%
  select(field_id, sys_trt, site_name, cc_trt, ph:phos) %>%
  pivot_longer(ph:phos) %>%
  #filter(name == "om") %>%
  ggplot(aes(cc_trt, value)) +
  geom_point(size = 4, aes(color = cc_trt), alpha = 0.5) +
  stat_summary(geom = "point", size = 4) +
  stat_summary(geom = "linerange") +
  facet_grid(name~field_id+sys_trt, scales = "free")
