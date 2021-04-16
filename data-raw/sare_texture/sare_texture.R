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
library(stringr)

# raw data ----------------------------------------------------------------

plotkey <- read_csv("data-raw/sare_plotkey/sare_plotkey.csv")

# wrangle data ------------------------------------------------------------

#--run back in Jan/Feb
datrawA <- read_excel("data-raw/sare_texture/rd-texture-hand-made-from-Dustin-sheet.xlsx")
dkeyA <- read_csv("data-raw/sare_texture/template_texture-msmts.csv") %>% select(-labeled) %>% mutate(Sample = textsamp_id)

sare_textureA <-
  datrawA %>%
  left_join(dkeyA) %>%
  janitor::clean_names() %>%
  select(code, clay, silt, sand) %>%
  rename("plot_id" = code) %>%
  mutate(batch = "A") %>%
  select(batch, plot_id, everything())

#--run in April when I found original Stout samples (grrr)
datrawB <- read_excel("data-raw/sare_texture/rd-texture-hand-made-from-Dustin-sheet-new-Stout.xlsx")
dkeyB <- read_csv("data-raw/sare_texture/template_texture-msmts-new-Stout.csv") %>% select(-labeled) %>% mutate(Sample = textsamp_id)

sare_textureB <-
  datrawB %>%
  left_join(dkeyB) %>%
  janitor::clean_names() %>%
  select(code, clay, silt, sand) %>%
  rename("plot_id" = code) %>%
  mutate(batch = "B") %>%
  select(batch, plot_id, everything())



# stout samples from batch A were not the same as the water retent --------

#--do they look different?

a <- "st-5no"
str_sub(a, -2, -1)


sare_textureA %>%
  filter(grepl("St", plot_id)) %>%
  bind_rows(sare_textureB) %>%
  pivot_longer(clay:sand) %>%
  mutate(cc_trt = stringr::str_sub(plot_id, -2, -1),
         plot_id = paste(cc_trt, plot_id)) %>%
  filter(name == "clay") %>%
  ggplot(aes(plot_id, value)) +
  geom_point(aes(color = batch)) +
  facet_wrap(~name) +
  coord_flip()


sare_texture <-
  sare_textureA %>%
  filter(!grepl("St", plot_id)) %>%
  bind_rows(sare_textureB) %>%
  select(-batch) %>%
  arrange(plot_id)


sare_texture %>%
  left_join(plotkey) %>%
  pivot_longer(clay:sand) %>%
  ggplot(aes(name, value)) +
  geom_point(aes(color = cc_trt)) +
  facet_grid(.~field_id+sys_trt)


sare_texture %>% write_csv("data-raw/sare_texture/sare_texture.csv")

use_data(sare_texture, overwrite = T)
