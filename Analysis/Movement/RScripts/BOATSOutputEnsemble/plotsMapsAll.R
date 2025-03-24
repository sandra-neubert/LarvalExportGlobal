# Time series plots 
# Fig. 2

library(tidyverse)
library(sf)
library(patchwork)
library(rnaturalearth)

### files and paths
source(file.path("BOATSOutputEnsemble", "SpatialPlottingFunctions.R"))
FigurePath <- "BOATSOutputEnsemble/Figures/"

# settings
LatLon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
cCRS <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
sf_use_s2(FALSE)

# prep data
# land
land <- rnaturalearth::ne_countries(
  scale = "large",
  returnclass = "sf"
) %>%
  # st_union() %>%
  st_crop(., c(xmin = -180, xmax = 180, ymin = -75, ymax = 75)) %>%
  sf::st_as_sf(crs = LatLon) %>%
  sf::st_transform(cCRS)

## to set
whichMPA <- c("mpa1", "mpa2") # either mpa1 or mpa2 (has to match with above)
regOut <- c("oa", "msy") # either msy or oa


for (i in 1:length(whichMPA)) {

if (whichMPA[i] == "mpa1") {
  limitsH <- c(0, 1.12)#c(0, 1.461)
  limitsHDiff <- c(-0.305, 0.305)#c(-0.323, 0.323)
  MPAName <- "MPA1"
} else {
  limitsH <- c(0, 1.12)
  limitsHDiff <- c(-0.305, 0.305)
  MPAName <- "MPA2"
}

FileNameH <- "HarvestSpatialEnsMean_y164"
FileNameHDiff <- "GLMDatEnsMean_y164"


# preprocess MPA data (put data into grid)
mpa <- read_csv(paste0("BOATSOutputEnsemble/FilesSpatial/", MPAName, ".csv"),
                col_names = c("lon", "lat", "mpa")
) %>%
  dplyr::mutate(lon = case_when(
    lon > 180 ~ lon - 360,
    lon < 180 ~ lon
  )) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = LatLon)
# dplyr::filter(mpa ==1) %>%
# dplyr::mutate(mpa = as.factor(mpa))

centroids <- mpa %>%
  dplyr::select("geometry")

cellSize <- 1
grid <- (st_bbox(centroids) + cellSize / 2 * c(-1, -1, 1, 1)) %>%
  st_make_grid(cellsize = c(cellSize, cellSize)) %>%
  st_sf() %>%
  dplyr::mutate(cellID = 1:nrow(.))

gridNew <- grid[match(unlist(st_intersects(mpa, grid)), grid$cellID), ]

mpa <- mpa %>%
  st_drop_geometry() %>%
  dplyr::mutate(geometry = gridNew$geometry) %>%
  sf::st_as_sf(crs = LatLon) %>%
  sf::st_transform(cCRS) %>%
  na.omit() %>%
  dplyr::filter(mpa == 1) %>%
  dplyr::mutate(mpa = as.factor(mpa))

rm(grid, gridNew, cellSize, centroids)

## raw Harvest data
H <- read_csv(paste0("BOATSOutputEnsemble/FilesSpatial/", FileNameH, ".csv")) %>%
  dplyr::mutate(lon = case_when(
    lon > 180 ~ lon - 360,
    lon < 180 ~ lon
  )) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = LatLon)

# transform into grid rather than points
centroids <- H %>%
  dplyr::select("geometry")

cellSize <- 1
grid <- (st_bbox(centroids) + cellSize / 2 * c(-1, -1, 1, 1)) %>%
  st_make_grid(cellsize = c(cellSize, cellSize)) %>%
  st_sf() %>%
  dplyr::mutate(cellID = 1:nrow(.))

for (j in 1:length(regOut)) {
  regFilter <- c(paste0("m1_", regOut[j])) # paste0("m0_", regOut[j]),
  for (k in 1:length(regFilter)) {
    HFiltered <- H %>%
      # na.omit() %>%
      dplyr::filter(
        mpa == whichMPA[i],
        reg == regFilter[k] # , ens == 4
      )
    
    gridNew <- grid[match(unlist(st_intersects(HFiltered, grid)), grid$cellID), ]
    
    HFilt_Sort <- HFiltered %>%
      st_drop_geometry() %>%
      dplyr::mutate(geometry = gridNew$geometry) %>%
      sf::st_as_sf(crs = LatLon) %>%
      sf::st_transform(cCRS) %>%
      na.omit()
    
    # clean env
    # rm(grid, gridNew, cellSize, centroids)
    
    if (regOut[j] == "msy") {
      print(paste0(
        "Harvest: ", as.numeric(stats::quantile(dplyr::pull(HFilt_Sort, val), 0.95)),
        " for ", regFilter[k]
      ))
    }
    
    
    gg_H <- plot_Harvest(HFilt_Sort, mpa, limitsH) +
      gg_add
    
    assign(paste0("gg_H_", regFilter[k], "_", whichMPA[i]), gg_H)
  }
  
  ###### Additional Harvest
  HDiff <- read_csv(paste0("BOATSOutputEnsemble/FilesSpatial/", FileNameHDiff, ".csv")) %>%
    dplyr::select(lon, lat, val, mpa, reg) %>%
    dplyr::mutate(lon = case_when(
      lon > 180 ~ lon - 360,
      lon < 180 ~ lon
    )) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = LatLon)
  
  # transform point geometry of BOATS data to a grid for better
  centroidsDiff <- HDiff %>%
    dplyr::select("geometry")
  
  cellSizeDiff <- 1
  gridDiff <- (st_bbox(centroidsDiff) + cellSizeDiff / 2 * c(-1, -1, 1, 1)) %>%
    st_make_grid(cellsize = c(cellSizeDiff, cellSizeDiff)) %>%
    st_sf() %>%
    dplyr::mutate(cellID = 1:nrow(.))
  
  HFilteredDiff <- HDiff %>%
    dplyr::filter(
      mpa == whichMPA[i],
      reg == regOut[j]
    )
  
  gridNewDiff <- gridDiff[match(unlist(st_intersects(HFilteredDiff, gridDiff)), gridDiff$cellID), ]
  
  HDiff_Filt <- HFilteredDiff %>%
    st_drop_geometry() %>%
    dplyr::mutate(geometry = gridNewDiff$geometry) %>%
    sf::st_as_sf(crs = LatLon) %>%
    sf::st_transform(cCRS) %>%
    na.omit()
  
  if (regOut[j] == "oa") {
    print(paste0(
      "Harvest Difference: ", as.numeric(stats::quantile(dplyr::pull(HDiff_Filt, val), 0.95)),
      " for ", regOut[j]
    ))
  }
  
  gg_HDiff <- plot_Diff2(HDiff_Filt, mpa, limitsHDiff) +
    gg_add
  
  assign(paste0("gg_HDiff_", regOut[j], "_", whichMPA[i]), gg_HDiff)
}
}

rm(
  HDiff_Filt, HDiff, H, HFiltered, HFilt_Sort, HFilteredDiff,
  centroids, centroidsDiff, grid, gridDiff, gridNew, gridNewDiff,
  gg_H, gg_HDiff
)

maps_row1 <- ((gg_H_m1_msy_mpa1 + 
                       ggplot2::ggtitle("Maximum Sustainable Yield \n(Economics-focused scenario)") + 
                       ggplot2::theme(
                         legend.position = "none",
                         axis.title.x = element_blank(),
                         axis.text.x = element_blank()
                       )
                      )
              + (gg_HDiff_msy_mpa1+ 
                   ggplot2::theme(
                     legend.position = "none",
                     axis.title.x = element_blank(),
                     axis.text.x = element_blank()
                   )))


maps_row2 <- ((gg_H_m1_oa_mpa1 + 
                 ggplot2::ggtitle("Open-Access \n(Economics-focused scenario)") + 
                 ggplot2::theme(
                   legend.position = "none",
                   axis.title.x = element_blank(),
                   axis.text.x = element_blank()
                 )
)
               + (gg_HDiff_oa_mpa1+ 
                    ggplot2::theme(
                      legend.position = "none",
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank()
                    )))


maps_row3 <- ((gg_H_m1_msy_mpa2 + 
                 ggplot2::ggtitle("Maximum Sustainable Yield \n(Biodiversity-focused scenario)") + 
                 ggplot2::theme(
                   legend.position = "none",
                   axis.title.x = element_blank(),
                   axis.text.x = element_blank()
                 )
              )
               + (gg_HDiff_msy_mpa2)+ 
                ggplot2::theme(
                  legend.position = "none",
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank()
                ))


maps_row4 <- ((gg_H_m1_oa_mpa2 + 
                 ggplot2::ggtitle("Open-Access \n(Biodiversity-focused scenario)") #+ 
               # ggplot2::theme(
               #   legend.position = "none",
               #   axis.title.x = element_blank(),
               #   axis.text.x = element_blank()
               # )
              ) 
               + (gg_HDiff_oa_mpa2))


maps_plots <- maps_row1 / maps_row2 / maps_row3 / maps_row4 + # plot_layout(guides = "collect")+
  plot_annotation(tag_levels = "a")

ggsave(
  plot = maps_plots,
  filename = file.path(FigurePath, paste0("H_Maps.png")),
  width = 35, height = 38, dpi = 400
)
