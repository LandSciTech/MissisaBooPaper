# Manuscript figures
library(sf)
library(tidyverse)
library(tmap)
library(grid)
library(viridis)
library(sfheaders) # to remove holes for AOU boundary

dat_pth <- "data/Missisa/"

caribouRanges <- read_sf("data/inputNV/caribouRanges/Caribou_Range_Boundary.shp", 
                         quiet = TRUE)

missisa <- caribouRanges %>%
  filter(RANGE_NAME %in% c("Missisa")) %>%
  st_transform(st_crs(raster::raster(file.path(dat_pth, "plc250.tif"))))

road_RoF <- read_sf(file.path(dat_pth, "road_ROFDevelopment.shp"))

road_2020 <- read_sf(file.path(dat_pth, "road_ORNMNRFMiss2020.shp"))
road_2010 <- read_sf(file.path(dat_pth, "road_ORNMNRFMiss2010.shp"))
province2<- raster::getData(country="Canada", level=1)

canada <- province2 %>% 
  st_as_sf() %>% 
  st_transform(st_crs(caribouRanges)) %>% 
  st_simplify(dTolerance = 10000)

# Takes a long time so use saved files
# aou <- read_sf("data/inputNV/MNR_FMUs_20220208/FOREST_MANAGEMENT_UNIT.shp") 
# 
# aou2 <- st_union(aou)
# aou2 <- st_sf(geometry = aou2) %>% 
#   sf_remove_holes(aou2)
# 
# mines_sf <- read_sf("data/inputNV/ROFDevelopment/mine_area.shp") %>% 
#   st_union() %>% 
#   st_transform(st_crs(missisa)) 
# 
# # save aou and mines union since it took a long time
# st_write(aou2, "data/inputNV/MNR_FMUs_20220208/AOU.shp")
# st_write(mines_sf, "data/inputNV/ROFDevelopment/mine_area_union.shp")

aou2 <- read_sf("data/inputNV/MNR_FMUs_20220208/AOU.shp")
mines_sf <- read_sf("data/inputNV/ROFDevelopment/mine_area_union.shp") %>% 
  st_make_valid()

aou2 <- mutate(aou2, AOU = "AOU") %>% st_transform(st_crs(caribouRanges)) %>% 
     sf_remove_holes()

# make aou just a line at the top
aouline <- aou2 %>% st_cast(to = "MULTILINESTRING") %>% 
  st_cast(to = "LINESTRING")

aouline2 <- st_intersection(aouline, caribouRanges %>% 
                              filter(!RANGE_NAME %in% c("Discontinuous Distribution", 
                                                        "Lake Superior Coast")))
aouline3 <- st_simplify(aouline2, 15000)

ecozones <- read_sf("data/inputNV/Ecozones/ecozones.shp")

hudPlBorSheild <- ecozones %>% 
  filter(ZONE_NAME %in% c("Hudson Plain", "Boreal Shield")) %>% 
  st_transform(st_crs(caribouRanges)) %>% 
  st_intersection(caribouRanges)

# Figure 1 #============================
canada_overlay <- tm_shape(canada, projection = "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")+
  tm_borders(col="black")+
  tm_shape(caribouRanges)+
  tm_fill("grey")+
  tm_layout(frame=FALSE)

boo_fig2 <- 
  tm_shape(hudPlBorSheild)+
  tm_fill(col = "ZONE_NAME", title = "Ecozone", alpha = 0.6, 
          palette = c("#f4a582", "#92c5de"))+
  tm_shape(caribouRanges, is.master = TRUE) + 
  tm_borders()+
  tm_shape(aouline3)+
  tm_lines(col = "#0571b0", lty = "dashed", lwd = 2)+
  tm_shape(mines_sf)+
  tm_fill(col = "#ca0020")+
  tm_shape(missisa)+
  tm_borders(col="black", lwd = 2)+
  tm_shape(caribouRanges, is.master = TRUE) +
  tm_text("RANGE_NAME", size= 1, fontface = "bold")+
  tm_scale_bar(position = c("left","bottom"), text.size=1)+
  tm_compass(position = c("right", "top"), size=3)+
  tm_layout(frame=F, legend.position = c(0, 0.88), 
            legend.title.size = 0.8, 
            legend.height = -0.25)+
  tm_add_legend(type = c("line"), 
                labels = c("Managed Forest\nBoundary"),
                col = c("#0571b0"), lty = "dashed", lwd = 2)+
  tm_add_legend(type = c("fill"), 
                labels = c("Mining Claims"),
                col = c("#ca0020"), 
                border.col = NA)

vp <- viewport(x=0.89, y=0.98, width = 0.3, height=0.25,
               just=c("right", "top"))

tmap_save(boo_fig2, filename = "outputs/Figure1_StudyArea.tiff",
  dpi = 300, insets_tm = canada_overlay, insets_vp = vp,
  height = 6, width = 6, units = "in")

# Figure 2 #========================================
pal <- c('#66c2a5','#fc8d62','#8da0cb')

missisa_fig <- tm_shape(missisa, legend.show=TRUE) + 
  tm_borders(col="grey70", lwd=1)+
  tm_shape(road_2020)+
  tm_lines(lty="solid", col=pal[1], lwd=2)+
  tm_scale_bar(position = c("left","BOTTOM"), text.size = 10,
               breaks = c(0, 50, 100, 150))+
  tm_compass(position = c("right", "top"), size=2)+
  tm_layout(main.title="a", main.title.size = 1)

missisa_RoF_fig <- tm_shape(missisa, legend.show=TRUE) + 
  tm_borders(col="grey70", lwd=1)+
  tm_shape(road_RoF)+
  tm_lines(col=pal[2], lwd = 2)+
  tm_shape(road_2020)+
  tm_lines(lty="solid", col= pal[1], lwd = 2)+
  tm_scale_bar(position = c("left","BOTTOM"), text.size = 10,
               breaks = c(0, 50, 100, 150))+
  tm_compass(position = c("right", "top"), size=2)+
  tm_layout(main.title="b", main.title.size = 1)

missisa_RoFmines_fig <- tm_shape(mines_sf)+
  tm_fill(col = pal[3])+
  tm_shape(missisa, legend.show=TRUE, is.master = TRUE) + 
  tm_borders(col="grey70", lwd=1)+
  tm_shape(road_RoF)+
  tm_lines(col=pal[2], lwd = 2)+
  tm_shape(road_2020)+
  tm_lines(lty="solid", col=pal[1], lwd = 2)+
  tm_scale_bar(position = c("left","BOTTOM"), text.size = 10,
               breaks = c(0, 50, 100, 150))+
  tm_compass(position = c("right", "top"), size=2)+
  tm_layout(main.title="c", main.title.size = 1)

missisa_map <- tmap_arrange(missisa_fig, missisa_RoF_fig, missisa_RoFmines_fig)

tmap_save(missisa_map, "outputs/Figure2_MissisaScenarios.tiff", dpi = 300, units="in", 
          height=3, width=6)

# Figure 3 #==============================================================
#Read in the data
missisa_boo <- raster::stack("outputs/missisa_2010_HabUse.grd")

missisa_boo <- missisa_boo[[c("Spring", "Summer", "Fall", "Winter")]]

missisa_boo_RoF <- raster::stack("outputs/missisaROFDev.grd")

missisa_boo_RoF <- missisa_boo_RoF[[c("Spring", "Summer", "Fall", "Winter")]]

missisa_boo_plot <- tm_shape(missisa_boo, 
                             raster.downsample=F)+
  tm_raster(palette=viridis(10),
            title="RSF Value", 
            legend.show = T,
            style="cont",
            breaks = seq(0,1, by=0.25))+
  tm_shape(road_2010)+
  tm_lines(col = "white")+
  tm_layout(legend.outside = TRUE, legend.outside.position = "right", 
            #main.title="Missisa - Reproduction",
            panel.label.size = 1,
            legend.text.size = 0.85,
            legend.title.size = 1, legend.outside.size = 0.15)+
  tm_facets(nrow = 1)

missisa_boo_plot

tmap_save(missisa_boo_plot, "outputs/Figure3_Missisa2010.tiff", dpi = 300,
          height = 1.5, width = 5.5)

# Figure 4 #====================================================================
missisa_boo_RoF_plot <- tm_shape(missisa_boo_RoF, 
                                 raster.downsample = F)+
  tm_raster(palette=viridis(10),
            #n=10, 
            title="RSF Value", 
            legend.show = F,
            style="cont",
            breaks = seq(0,1, by=0.25))+
  tm_shape(road_RoF)+
  tm_lines(col = "white")+
  tm_layout(legend.outside.position = "right", 
            main.title="Missisa",
            panel.label.size = 1,
            legend.text.size = 0.85,
            legend.title.size = 1,
            main.title.size = 1)+
  tm_facets(nrow = 1)

missisa_boo_RoF_plot

missisa_legend <- tm_shape(missisa_boo_RoF$Fall, raster.downsample = F)+
  tm_raster(palette=viridis(10),
            #n=10, 
            title="RSF Value", 
            legend.show = T,
            style="cont",
            breaks = seq(0,1, by=0.25))+
  tm_layout(legend.only = T, legend.width = 2, legend.height = 10, 
            legend.position = c("right", "center"),
            legend.just = c("left", "center"))

#Nipigon

missisa_boo_Nip <- raster::stack("outputs/missisaROFDev_Nip.grd")

missisa_boo_Nip <- missisa_boo_Nip[[c("Spring", "Summer", "Fall", "Winter")]]

missisa_boo_Nip_plot <- tm_shape(missisa_boo_Nip, 
                                 raster.downsample = F)+
  tm_raster(palette=viridis(10),n=10, title="RSF Value", legend.show=F)+
  tm_shape(road_RoF)+
  tm_lines(col = "white")+
  tm_layout(legend.outside.position = "right", 
            main.title="Nipigon",
            panel.label.size = 1,
            legend.text.size = 0.85,
            legend.title.size = 1,
            main.title.size = 1)+
  tm_facets(nrow = 1)

#Pagwachuan

missisa_boo_Pag <- raster::stack("outputs/missisaROFDev_Pag.grd")

missisa_boo_Pag <- missisa_boo_Pag[[c("Spring", "Summer", "Fall", "Winter")]]

missisa_boo_Pag_plot <- tm_shape(missisa_boo_Pag, 
                                 raster.downsample = F)+
  tm_raster(palette=viridis(10),n=10, title="RSF Value", legend.show = F)+
  tm_shape(road_RoF)+
  tm_lines(col = "white")+
  tm_layout(legend.outside.position = "right", 
            main.title="Pagwachuan",
            panel.label.size = 1,
            legend.text.size = 0.85,
            legend.title.size = 1,
            main.title.size = 1)+
  tm_facets(nrow = 1)

#James Bay

missisa_boo_JB <- raster::stack("outputs/missisaROFDev_JB.grd")

missisa_boo_JB <- missisa_boo_JB[[c("Spring", "Summer", "Fall", "Winter")]]

missisa_boo_JB_plot <- tm_shape(missisa_boo_JB, 
                                raster.downsample = F)+
  tm_raster(palette=viridis(10),n=10, title="RSF Value", legend.show=F)+ 
  tm_shape(road_RoF)+
  tm_lines(col = "white")+
  tm_layout(legend.outside.position = "right", 
            main.title="James Bay",
            panel.label.size = 1,
            legend.text.size = 0.85,
            legend.title.size = 1,
            main.title.size = 1)+
  tm_facets(nrow = 1)

tiff("outputs/Figure4_MissisaROFDevComapare.tiff", width = 5.5, height = 6, units = "in", res = 300)
grid.newpage()
page.layout <- grid.layout(nrow = 4, ncol = 2, widths=c(.85,.15), 
                           heights=c(0.25, 0.25, 0.25, 0.25))
pushViewport(viewport(layout = page.layout))
print(missisa_boo_RoF_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(missisa_legend, vp = viewport(layout.pos.row = 1, layout.pos.col = 2, just = "left"))
print(missisa_boo_JB_plot, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(missisa_boo_Nip_plot, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(missisa_boo_Pag_plot, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
dev.off()

# Figure S1.4 #=========================================
mod_list <- mget(ls(pattern = "missisa_boo_...?$"))

samp_pts <- st_make_grid(missisa, 
                         n = c(raster::ncol(mod_list[[1]])/10,
                               raster::nrow(mod_list[[1]])/10),
                         what = "centers") %>% 
  st_as_sf() %>% 
  filter(!st_is_empty(x))

# stk <- mod_list[[1]]
# nm <- names(mod_list)[1]

get_samp <- function(stk, nm){
  stk_samp <- raster::extract(stk, samp_pts)
  
  stk_df <- stk_samp %>% as.data.frame() %>% 
    pivot_longer(everything(), names_to = "Season", values_to = "value") %>% 
    mutate(model = nm, pt_ID = 1:n())
  
  stk_df
}

samp_dfs <- map2(mod_list, names(mod_list), get_samp)

all_samps <- bind_rows(samp_dfs[c("missisa_boo_JB", "missisa_boo_Nip", 
                                  "missisa_boo_Pag")]) %>% 
  left_join(samp_dfs["missisa_boo_RoF"][[1]], by = c("Season", "pt_ID")) %>% 
  filter(!is.na(value.x), !is.na(value.y)) %>% 
  mutate(model.x = case_when(str_detect(model.x, "JB") ~ "James Bay",
                             str_detect(model.x, "Nip") ~ "Nipigon",
                             str_detect(model.x, "Pag") ~ "Pagwachuan",))

comparR <- all_samps %>% group_by(Season, model.x) %>%
  summarise(pearsonR = cor(value.x, value.y) %>% round(3)) %>% 
  mutate(x = 0, y = 1.125)

all_samps %>% 
  ggplot(aes(value.x, value.y))+
  geom_abline(slope = 1, intercept = 0, col = "grey40")+
  geom_point(alpha = 0.2, shape = 16, size = 1)+
  geom_smooth(method = "lm")+
  geom_text(data = comparR, aes(x, y, label = paste0("R = ", pearsonR)), 
            hjust = "left", vjust = "top", inherit.aes = FALSE)+
  facet_grid(model.x ~ Season)+
  coord_fixed(ylim = c(0, 1.1))+
  scale_y_continuous(breaks = 0:4/4, labels = c(0, 0.25, 0.50, 0.75, 1) %>% 
                       as.character())+
  scale_x_continuous(breaks = 0:4/4, labels = c(0, 0.25, 0.50, 0.75, 1) %>% 
                       as.character())+
  labs(x = "Missisa range", y = "Adjacent ranges")+
  theme_bw()

ggsave("outputs/FigureS1_4_compare_adjacent_missisa_quant.tiff", width = 7, 
       height = 5, dpi = 300)
