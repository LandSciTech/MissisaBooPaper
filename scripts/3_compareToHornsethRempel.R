#Script compares the output from caribouHabitat to the original surfaces from Hornseth and Rempel
library(sf)
library(RColorBrewer)
library(tidyverse)
library(tmap)
library(exactextractr)
# raster library used but not loaded since it conflicts with tidyverse


#Compare to HR Original
# copied the .prj file that came with the Churchill file. Assuming they are the same
hr_1 <- read_sf("data/inputNV/Missisa_Model_Output/Missisa_4_592_1.shp")

carHab_Mis_HU <- raster::brick("outputs/missisa_2010_HabUse.grd")
carHab_Mis_PD <- raster::brick("outputs/missisa_2010_procData.grd")

caribouResult592 <- exact_extract(raster::stack(carHab_Mis_HU, 
                                                  carHab_Mis_PD),
                                    hr_1 %>% select(HEXID), 
                                    fun = "mean", append_cols = c("HEXID", "geometry"))  %>% 
  rename_with(~str_remove(.x, "mean\\.")) %>% select(-CONST, -other) %>% 
  left_join(hr_1 %>% select(HEXID), by = "HEXID") %>% 
  st_as_sf()

# Visual comparison of results #================================================

# Compare each explanatory variable and season
rempNames <- stringr::str_subset(names(hr_1), "_S6") %>% sort()

cariNames <- stringr::str_subset(names(caribouResult592),
                                 "[[:upper:]]{3}\\D+|[[:upper:]]{3}$|[[:upper:]]{2}$") %>% 
  sort() %>% stringr::str_subset("HEXID", negate = TRUE)

mapDiff <- function(x, xColVar, yColVar){
  xColVar <- xColVar
  yColVar <- yColVar
  xColVarE <- rlang::ensym(xColVar)
  yColVarE <- rlang::ensym(yColVar)
  
  x <- mutate(x, dif = !!xColVarE - !!yColVarE)
  
  brks <- seq(from = (min(x$dif, na.rm = TRUE) %>% round(2)) - 0.01,
              to = (max(x$dif, na.rm = TRUE) %>% round(2)) + 0.01,
              length.out = 10) 
  
  tm_shape(x)+
    tm_polygons(col = "dif", border.col = NULL, 
                palette=brewer.pal(n = 10, name = "RdYlBu"), midpoint=0,
                style="fixed",
                breaks=c(-1,-0.75,-0.5,-0.25,-0.05,0.05,0.25,0.5,0.75,1),
                title = paste0(xColVar," - ", yColVar))+
    tm_legend(legend.show = FALSE)+
    tm_layout(title = yColVar, title.position = c("left", "bottom"))
}

comprResults <- hr_1 %>%
  select(HEXID, contains("_S6"), contains("USE")) %>%  
  left_join(caribouResult592 %>% st_drop_geometry(), by = "HEXID")

difMapVars <- map2(rempNames, cariNames,
                   ~mapDiff(comprResults, .x, .y))

difMapRes <- map2(c("PSP_USE", "PSU_USE", "PFA_USE", "PWI_USE"),
                  c("Spring", "Summer", "Fall", "Winter"),
                  ~mapDiff(comprResults, .x, .y))

leg <- tm_shape(hr_1)+
  tm_polygons(col = "PSP_USE", border.col = NULL,  
              palette=brewer.pal(n = 10, name = "RdYlBu"), midpoint=0,
              style="fixed",
              breaks=c(-1,-0.75,-0.5,-0.25,-0.05,0.05,0.25,0.5,0.75,1),
              title = "Difference")+
  tm_layout(legend.only = TRUE)

difMapRes[[5]] <- difMapRes[[4]]
difMapRes[[4]] <- difMapRes[[3]]
difMapRes[[3]] <- leg
difMapRes <- tmap_arrange(difMapRes, ncol = 3, 
             widths = c(0.4, 0.4, 0.2))

tmap_save(difMapRes, "outputs/RempelECCCDiffMaps.png", dpi = 1200)

difMapVars[[12]] <- leg

difMapVars <- tmap_arrange(difMapVars, ncol = 4)

tmap_save(difMapVars, "outputs/RempelECCCDiffMapsVars.png", dpi = 1200, 
          width = 10.5, height = 8)

# Quantitative comparison # ====================================================

rempelResultData <- st_drop_geometry(hr_1)

caribouResultData <- st_drop_geometry(caribouResult592)

compar <- full_join(rempelResultData %>% 
                      select(HEXID, contains("_S6"), matches("P.._USE")), 
                    caribouResultData, by = "HEXID", 
                    suffix = c("_remp", "_eccc"))

comparLong <- compar %>% 
  select(HEXID, matches("P.._USE"), Spring, Summer, Fall, Winter) %>% 
  gather(season, Pred, -HEXID) %>% 
  mutate(Source = ifelse(stringr::str_detect(season, "P.._USE"), 
                         "Rempel", "ECCC"),
         season = case_when(season == "PFA_USE" ~ "Fall",
                            season == "PSP_USE" ~ "Spring",
                            season == "PSU_USE" ~ "Summer",
                            season == "PWI_USE" ~ "Winter",
                            TRUE ~ season)) %>% 
  spread(Source, Pred) 

comparLM <- comparLong %>% group_by(season) %>%
  summarise(pearsonR = cor(ECCC, Rempel) %>% round(3)) %>% 
  mutate(x = 1, y = 0.55)

lmRvsEPlot <- comparLong %>% 
  ggplot(aes(Rempel, ECCC))+
  geom_abline(slope = 1, intercept = 0, col = "grey40")+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_text(data = comparLM, aes(x, y, label = paste0("R =\n", pearsonR)), 
            hjust = "right", vjust = "top", inherit.aes = FALSE)+
  facet_wrap(~season)+
  theme_bw()

ggsave("outputs/ECCC_HR_Compare.png", lmRvsEPlot, dpi=1200)
