# created 2/18/2021
# purpose: process raw ellsworth data
# last updated:

rm(list = ls())
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(usethis)

# raw data ----------------------------------------------------------------

datraw1 <- read_csv("data-raw/sare_om/VIRGINIA_NICHOLS_ISU_S0215-221.csv")
datraw2 <- read_csv("data-raw/sare_om/VIRGINIA_NICHOLS_AMES_S0416-920.csv")

dkey1 <- read_csv("data-raw/sare_texture/template_texture-msmts.csv") %>% select(-labeled) %>% mutate(Sample = textsamp_id)
dkey2 <- read_csv("data-raw/sare_texture/template_texture-msmts-new-Stout.csv") %>% select(-labeled) %>% mutate(Sample = textsamp_id)

plotkey <- read_csv("data-raw/sare_plotkey/sare_plotkey.csv")

# wrangle data ------------------------------------------------------------

sare_om1 <-
  datraw1 %>%
  rename("textsamp_id" = samp_id) %>%
  select(-starts_with("x"), -date) %>%
  left_join(dkey1) %>%
  janitor::clean_names() %>%
  select(code, ph, om, potassium, phos) %>%
  rename("plot_id" = code) %>%
  select(plot_id, everything())

sare_om2 <-
  datraw2 %>%
  rename("textsamp_id" = samp_id) %>%
  select(-starts_with("x"), -date) %>%
  left_join(dkey2) %>%
  janitor::clean_names() %>%
  select(code, ph, om, potassium, phos) %>%
  rename("plot_id" = code) %>%
  select(plot_id, everything())

sare_om1 %>%
  filter(grepl("St", plot_id)) %>%
  mutate(batch = "A") %>%
  bind_rows(sare_om2 %>% mutate(batch = "B")) %>%
  mutate(cctrt = str_sub(plot_id, -2, -1)) %>%
  ggplot(aes(cctrt, om)) +
  geom_jitter(aes(color = batch))

sare_om <-
  sare_om1 %>%
  filter(!grepl("St", plot_id)) %>%
  bind_rows(sare_om2)


sare_om %>% write_csv("data-raw/sare_om/sare_om.csv")
usethis::use_data(sare_om, overwrite = T)


# viz check ---------------------------------------------------------------

sare_om %>%
  left_join(plotkey) %>%
  select(field_id, sys_trt, site_name, cc_trt, ph:phos) %>%
  pivot_longer(ph:phos) %>%
  #filter(name == "om") %>%
  ggplot(aes(cc_trt, value)) +
  geom_point(size = 4, aes(color = cc_trt), alpha = 0.5) +
  stat_summary(geom = "point", size = 4) +
  stat_summary(geom = "linerange") +
  facet_grid(name~field_id+sys_trt, scales = "free")
