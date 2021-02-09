# created 2/4/2021
# purpose: process raw texture data from Dustin
# last updated:

rm(list = ls())
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(usethis)

# raw data ----------------------------------------------------------------

datraw <- read_excel("data-raw/sare_texture/rd-texture-hand-made-from-Dustin-sheet.xlsx")

dkey <- read_csv("data-raw/sare_texture/template_texture-msmts.csv") %>% select(-labeled) %>% mutate(Sample = textsamp_id)

plotkey <- read_csv("data-raw/sare_plotkey/sare_plotkey.csv")

# wrangle data ------------------------------------------------------------

sare_texture <- 
  datraw %>% 
  left_join(dkey) %>%
  janitor::clean_names() %>% 
  select(code, clay, silt, sand) %>% 
  rename("plot_id" = code) %>% 
  select(plot_id, everything())


sare_texture %>% 
  left_join(plotkey) %>% 
  pivot_longer(clay:sand) %>% 
  ggplot(aes(name, value)) + 
  geom_point(aes(color = cc_trt)) + 
  facet_grid(.~field_id+sys_trt)


sare_texture %>% write_csv("data-raw/sare_texture/sare_texture.csv")

use_data(sare_texture, overwrite = T)
