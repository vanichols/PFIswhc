# updated march 23 2020 (changed to jarad-style files)

library(readxl)
library(dplyr)

pfi_siteinfo <-
  readxl::read_excel("data-raw/pfi_siteinfo/rd_site-locs.xlsx") %>%
  dplyr::rename(sys_trt = system) %>%
  mutate(site_name =
             case_when(
             grepl("Kohler", coop_name) ~ "Boyd",
             grepl("Funcke", coop_name) ~ "Funcke",
                grepl("Stout", coop_name) ~ "Stout")) %>%
  select(coop_name, site_name, sys_trt, lat, lon, city, county) %>%
  mutate(site_name = case_when(
    site_name == "Boyd" ~ "Central",
    site_name == "Stout" ~ "East",
    site_name == "Funcke" ~ "West"
  )) %>%
  select(-coop_name)

pfi_siteinfo %>%
  write_csv("data-raw/pfi_siteinfo/pfi_siteinfo.csv")

usethis::use_data(pfi_siteinfo, overwrite = TRUE)


