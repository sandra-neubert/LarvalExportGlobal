#Prep GLM input
#load files with npp, temperature, lon, lat info and add movement index and dist to mpa
library(tidyverse)
library(units)

SpatialData <- "BOATSOutputEnsemble/FilesSpatial"
LatLon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


H_NAs <- read_csv(file.path(SpatialData, "GLMDatEnsMean_y164.csv")) %>%
  dplyr::select(lon, lat, val, mpa, reg) %>%
  dplyr::mutate(lon = case_when(
    lon > 180 ~ lon - 360,
    lon < 180 ~ lon
  )) %>%
  dplyr::filter(mpa=="mpa1", reg == "oa") %>%
  dplyr::select("val")

MoveInd <- read_csv(file.path(SpatialData, "MoveInd.csv"), 
                    col_names = c("lon", "lat", "moveInd"))%>%
  dplyr::mutate(lon = case_when(lon > 180 ~ lon -360,
                                lon < 180 ~ lon)) %>%
  dplyr::mutate(moveInd = case_when(is.na(H_NAs$val) ~ NA, #make land NA
                                    !is.na(H_NAs$val) ~ moveInd)) 


HDiff <- read_csv(file.path(SpatialData, "GLMDatEnsMean_y164.csv")) %>%
  #dplyr::select(lon, lat, val, mpa, reg) %>%
  dplyr::mutate(lon = case_when(
    lon > 180 ~ lon - 360,
    lon < 180 ~ lon
  )) 

df_list <- list(HDiff, MoveInd)
GLMData <- df_list %>% reduce(full_join, by=c("lon","lat"))


#Distance

MPA1_Dist <- readRDS(file.path(SpatialData, "MPA1_Dist.rds"))
MPA2_Dist <- readRDS(file.path(SpatialData, "MPA2_Dist.rds"))
MPA_Dist <- rbind(MPA1_Dist, MPA2_Dist)

df_list <- list(GLMData , MPA_Dist)
GLMData <- df_list %>% reduce(full_join, by=c("lon","lat", "mpa")) 

GLMDataFilt <- GLMData %>%
  dplyr::filter(inMPA == 0)

saveRDS(GLMDataFilt, file.path("BOATSOutputEnsemble", "FilesSpatial", "GLMDataFilt.rds"))

#plot move ind
library(patchwork)
library(rnaturalearth)
FigurePath <- "BOATSOutputEnsemble/Figures/"
cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
sf_use_s2(FALSE)

land <- rnaturalearth::ne_countries(
  scale = "large",
  returnclass = "sf"
) %>%
  # st_union() %>%
  st_crop(., c(xmin = -180, xmax = 180, ymin = -75, ymax = 75)) %>%
  sf::st_as_sf(crs = LatLon) %>%
  sf::st_transform(cCRS)


source(file.path("BOATSOutputEnsemble", "SpatialPlottingFunctions.R"))

MoveIndPlot <- MoveInd %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = LatLon)

centroids <- MoveIndPlot %>%
  dplyr::select("geometry")

cellSize <- 1
grid <- (st_bbox(centroids) + cellSize / 2 * c(-1, -1, 1, 1)) %>%
  st_make_grid(cellsize = c(cellSize, cellSize)) %>%
  st_sf() %>%
  dplyr::mutate(cellID = 1:nrow(.))

gridNew <- grid[match(unlist(st_intersects(MoveIndPlot, grid)), grid$cellID), ]

MoveIndPlot <- MoveIndPlot %>%
  st_drop_geometry() %>%
  dplyr::mutate(geometry = gridNew$geometry) %>%
  sf::st_as_sf(crs = LatLon) %>%
  sf::st_transform(cCRS)

gg_moveInd <- plot_MoveInd(MoveIndPlot) + 
  gg_add

ggsave(
  plot = gg_moveInd,
  filename = file.path(FigurePath, paste0("MoveInd.png")),
  width = 20, height = 14, dpi = 400
)

### plot npp and temp
Npp_Temp_Plot <- GLMData %>%
  dplyr::filter(mpa == "mpa1", reg == "oa") %>%
  dplyr::select("lon", "lat", "npp", "npp_ed", "temp") %>%
  na.omit() %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = LatLon) 

centroids <- Npp_Temp_Plot %>%
  dplyr::select("geometry")

cellSize <- 1
grid <- (st_bbox(centroids) + cellSize / 2 * c(-1, -1, 1, 1)) %>%
  st_make_grid(cellsize = c(cellSize, cellSize)) %>%
  st_sf() %>%
  dplyr::mutate(cellID = 1:nrow(.))

gridNew <- grid[match(unlist(st_intersects(Npp_Temp_Plot, grid)), grid$cellID), ]

Npp_Temp_Plot <- Npp_Temp_Plot %>%
  st_drop_geometry() %>%
  dplyr::mutate(geometry = gridNew$geometry) %>%
  sf::st_as_sf(crs = LatLon) %>%
  sf::st_transform(cCRS)

gg_npp_ed <- plot_predictors(Npp_Temp_Plot, colInterest = Npp_Temp_Plot$npp_ed,
                             paletteName = "viridis", legendTitle = expression("Mean NPP (mmolC m"^"-3" * "d"^"-1" * ")")) + 
  gg_add

ggsave(
  plot = gg_npp_ed,
  filename = file.path(FigurePath, paste0("NPP_ed.png")),
  width = 20, height = 14, dpi = 400
)


gg_npp <- plot_predictors(Npp_Temp_Plot, colInterest = Npp_Temp_Plot$npp,
                             paletteName = "mako") + 
  gg_add

ggsave(
  plot = gg_npp,
  filename = file.path(FigurePath, paste0("NPP.png")),
  width = 20, height = 14, dpi = 400
)

gg_temp <- plot_predictors(Npp_Temp_Plot, colInterest = Npp_Temp_Plot$temp,
                          paletteName = "magma", legendTitle = "Mean Temperature (°C)") + 
  gg_add

ggsave(
  plot = gg_temp,
  filename = file.path(FigurePath, paste0("Temperature.png")),
  width = 20, height = 14, dpi = 400
)

###### Plot of predictors

plot_preds <- (gg_distMPA1 + gg_distMPA2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')) /
  (gg_moveInd + gg_npp_ed + gg_temp)+
  plot_annotation(tag_levels = "a")

ggsave(
  plot = plot_preds,
  filename = file.path(FigurePath, paste0("GLM_predictors.png")),
  width = 40, height = 18, dpi = 400
)
