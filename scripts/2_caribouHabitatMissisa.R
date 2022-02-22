#This script contains the analysis for reproducing the Hornseth and Remple models from 2016 as well as plotting code. Data is stored on the Git Repo.

#Load libraries

library(sf)
library(tidyverse)
library(viridis)
library(units)
library(tmap)
library(caribouMetrics)
# raster library used but not loaded since it conflicts with tidyverse

library(osfr)

# To use the pre-prepared data skip 1_dataPrep.R and run the following
if(!dir.exists("data/Missisa")){
  # Download the zip file of prepared files from OSF
  osf_proj <- osf_retrieve_node("https://osf.io/r9mkp/")
  
  osf_ls_files(osf_proj) %>% filter(name == "Missisa.zip") %>% 
    osf_download(path = "data")
  
  # extract it to the data folder of the current project
  unzip("data/Missisa.zip", exdir = "data")
}


# ensure outputs folder exists
if(!dir.exists("outputs")){
  dir.create("outputs")
}


dat_pth <- "data/Missisa/"

#Generate/Reproduce RSF for Missisa Range

caribouRanges <- st_read("data/inputNV/caribouRanges/Caribou_Range_Boundary.shp", 
                         quiet = TRUE)

missisa <- caribouRanges %>%
  filter(RANGE_NAME %in% c("Missisa")) %>%
  st_transform(st_crs(raster::raster(file.path(dat_pth, "plc250.tif"))))

#Missisa
carHab_Mis <- caribouHabitat(
  landCover = file.path(dat_pth, "plc250.tif"),
  esker = file.path(dat_pth, "esker.shp"),
  natDist = file.path(dat_pth, "fireAFFES2010_250.tif"),
  linFeat = list(roads = file.path(dat_pth, "road_ORNMNRFMiss2010.shp"),
                 rail = file.path(dat_pth, "rail.shp"),
                 utilities = file.path(dat_pth, "util2010.shp")),
  projectPoly = missisa,
  caribouRange = "Missisa",
  #padFocal = TRUE, # assume data outside area is 0 for all variables
  padProjPoly=TRUE,
  saveOutput = "outputs/missisa_2010_HabUse.grd"
)

raster::writeRaster(carHab_Mis@processedData, "outputs/missisa_2010_procData.grd",
                    overwrite = TRUE)

#Missisa RoF

carHab_Mis_RoF <- caribouHabitat(
  landCover = file.path(dat_pth, "plc250.tif"),
  esker = file.path(dat_pth, "esker.shp"),
  natDist = file.path(dat_pth, "fireAFFES2020_250.tif"),
  linFeat = list(roads = file.path(dat_pth, "road_ROFDevelopment.shp"),
                 rail = file.path(dat_pth, "rail.shp"),
                 utilities = file.path(dat_pth, "util2020.shp")),
  projectPoly = missisa,
  caribouRange = "Missisa",
  #padFocal = TRUE, # assume data outside area is 0 for all variables
  padProjPoly=TRUE,
  saveOutput = "outputs/missisaROFDev.grd"
)

#Missisa RoF w Nipigon

# can change the caribou range and then recompute using the same data. First
# copy so don't affect the Missisa object

# Note for Nipigon re-run the model b/c it uses a different window area

carHab_Mis_RoF_Nip <- caribouHabitat(
  landCover = file.path(dat_pth, "plc250.tif"),
  esker = file.path(dat_pth, "esker.shp"),
  natDist = file.path(dat_pth, "fireAFFES2020_250.tif"),
  linFeat = list(roads = file.path(dat_pth, "road_ROFDevelopment.shp"),
                 rail = file.path(dat_pth, "rail.shp"),
                 utilities = file.path(dat_pth, "util2020.shp")),
  projectPoly = missisa,
  caribouRange = "Nipigon",
  #padFocal = TRUE, # assume data outside area is 0 for all variables
  padProjPoly=TRUE,
  saveOutput = "outputs/missisaROFDev_Nip.grd"
)

#Missisa RoF w James Bay
carHab_template <- carHab_Mis_RoF
carHab_template@attributes$caribouRange$coefRange <- "James Bay"

carHab_Mis_RoF_JB <- updateCaribou(carHab_template)

raster::writeRaster(carHab_Mis_RoF_JB@habitatUse,
                    "outputs/missisaROFDev_JB.grd", overwrite = TRUE)

#Missisa RoF w Pagwachuan
carHab_template@attributes$caribouRange$coefRange <- "Pagwachuan"

carHab_Mis_RoF_Pag <- updateCaribou(carHab_template)

raster::writeRaster(carHab_Mis_RoF_Pag@habitatUse,
                    "outputs/missisaROFDev_Pag.grd", overwrite = TRUE)

if(require(beepr, quietly = TRUE)){
  beepr::beep()
}
