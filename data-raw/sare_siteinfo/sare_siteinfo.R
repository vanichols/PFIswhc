library(readxl)
library(dplyr)

sare_siteinfo <-
  readxl::read_excel("data-raw/sare_siteinfo/rd_site-locs.xlsx") %>%
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
  select(-coop_name) %>%
  mutate(field_id = ifelse(site_name == "Central", paste0(site_name, 42), site_name)) %>%
    filter(!(site_name == "Central" & sys_trt == "grain")) %>%
    mutate(site_desc = case_when(
      site_name == "East" ~ "Corn grain/soybean rotation, one field with 4 replicated treatments of cc/no",
      site_name == "West" ~ "Corn grain/soybean rotation, one field with 4 replicated treatments of cc/no",
      site_name == "Central" ~ "Both corn grain/soybean and corn silage/soybean rotations, field 42 was all soybean in 2019, 4 replicated treatments of cc/no in each system"
      )
    ) %>%
    select(field_id, site_name, lat, lon, county, site_desc)


sare_siteinfo %>%
  write_csv("data-raw/sare_siteinfo/sare_siteinfo.csv")

usethis::use_data(sare_siteinfo, overwrite = TRUE)


