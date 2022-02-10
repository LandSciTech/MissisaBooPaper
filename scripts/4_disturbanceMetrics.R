#Disturbance Metrics
library(caribouMetrics)
library(sf)
library(tidyverse)

# path to data
dat_pth <- "data/inputNV/Missisa/"

caribouRanges <- st_read("data/inputNV/caribouRanges/Caribou_Range_Boundary.shp", 
                         quiet = TRUE)

missisa <- caribouRanges %>%
  filter(RANGE_NAME %in% c("Missisa")) %>%
  st_transform(st_crs(raster::raster(file.path(dat_pth, "plc250.tif")))) %>% 
  select(OGF_ID)

missisa_DM <- disturbanceMetrics(
  landCover = file.path(dat_pth, "plc250.tif"),
  natDist = file.path(dat_pth, "fireAFFES2020_250.tif"),
  anthroDist = file.path(dat_pth, "harvMNRF2018_250.tif"),
  linFeat = list(roads = file.path(dat_pth, "road_ORNMNRFMiss2020.shp"),
                 rail = file.path(dat_pth, "rail.shp"),
                 utilities = file.path(dat_pth, "util2010.shp")),
  projectPoly=missisa,
  padProjPoly=TRUE,
  bufferWidth=500
)

disturbances_missisa <- missisa_DM@disturbanceMetrics

missisa_DM_RoF <- disturbanceMetrics(
  landCover = file.path(dat_pth, "plc250.tif"),
  natDist = file.path(dat_pth, "fireAFFES2020_250.tif"),
  anthroDist = file.path(dat_pth, "harvMNRF2018_250.tif"),
  linFeat = list(roads = file.path(dat_pth, "road_ROFDevelopment.shp"),
                 rail = file.path(dat_pth, "rail.shp"),
                 utilities = file.path(dat_pth, "util2010.shp")),
  projectPoly=missisa,
  padProjPoly=TRUE,
  bufferWidth=500
)

disturbances_missisa_RoF <- missisa_DM_RoF@disturbanceMetrics

missisa_DM_RoFMines <- disturbanceMetrics(
  landCover = file.path(dat_pth, "plc250.tif"),
  natDist = file.path(dat_pth, "fireAFFES2020_250.tif"),
  anthroDist = file.path(dat_pth, "mines_ras.tif"),
  linFeat = list(roads = file.path(dat_pth, "road_ROFDevelopment.shp"),
                 rail = file.path(dat_pth, "rail.shp"),
                 utilities = file.path(dat_pth, "util2010.shp")),
  projectPoly=missisa,
  padProjPoly=FALSE,
  bufferWidth=500
)

disturbances_missisa_RoFMines <- missisa_DM_RoFMines@disturbanceMetrics

# Combine results into table

disturbances_missisa_all <- bind_rows(
  Base = disturbances_missisa, 
  `Roads Only` = disturbances_missisa_RoF,
  `Roads and Mines` = disturbances_missisa_RoFMines, 
  .id = "Scenario"
) %>% 
  mutate(across(where(is.numeric), ~round(.x, 2))) %>% 
  select(-OGF_ID, -zone)

write.csv(disturbances_missisa_all, "data/disturbances_missisa_all.csv",
          row.names = FALSE)
