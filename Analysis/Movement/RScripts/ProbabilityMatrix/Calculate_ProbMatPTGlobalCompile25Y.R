library(sf)
library(tidyverse)
library(tidync)
library(ncdf4)
library(lubridate)

###### Set paths ######

## Katana
Data_path <- "/srv/scratch/z9902002/Movement/Output"
idx <- as.integer(Sys.getenv('PBS_ARRAY_INDEX'))
uniqueYears <- 1995:2019

name_suf <- "_25Y"

# Figure_path <- "../Figures"
BoatsPath <- file.path("..", "Data", "maskGlobal.csv") #dont use just ocean cells, filter the cells later (takes longer though?)
LatLon <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


###### Functions #########
get_Indices <- function(BOATSCentroid, BOATSdf) { #BOATSCentroid is index number
  #dont need to account for indexing >74.9 or <-75 bc no prob of going there anyway
  
  currBOATS <- BOATSdf[BOATSCentroid,] %>% dplyr::select(-ind) #get centroid boats info
  
  
  cellN <- currBOATS %>% dplyr::mutate(lat = lat + 1)
  cellS <- currBOATS %>% dplyr::mutate(lat = lat - 1)
  if (currBOATS$lon == 179.5 ) { 
    cellW <- currBOATS %>% dplyr::mutate(lon = lon - 1)
    cellE <- currBOATS %>% dplyr::mutate(lon = -179.5)
    cellNW <- cellN %>% dplyr::mutate(lon = lon - 1) 
    cellNE <- cellN %>% dplyr::mutate(lon = -179.5)
    cellSW <- cellS %>% dplyr::mutate(lon = lon - 1)
    cellSE <- cellS %>% dplyr::mutate(lon = -179.5)
  } else if (currBOATS$lon == -179.5 ){
    cellW <- currBOATS %>% dplyr::mutate(lon = 179.5)
    cellE <- currBOATS %>% dplyr::mutate(lon = lon + 1)
    cellNW <- cellN %>% dplyr::mutate(lon = 179.5 ) 
    cellNE <- cellN %>% dplyr::mutate(lon = lon + 1)
    cellSW <- cellS %>% dplyr::mutate(lon = 179.5 )
    cellSE <- cellS %>% dplyr::mutate(lon = lon + 1)
  } else {
    cellW <- currBOATS %>% dplyr::mutate(lon = lon - 1)
    cellE <- currBOATS %>% dplyr::mutate(lon = lon + 1)
    cellNW <- cellN %>% dplyr::mutate(lon = lon - 1) 
    cellNE <- cellN %>% dplyr::mutate(lon = lon + 1)
    cellSW <- cellS %>% dplyr::mutate(lon = lon - 1)
    cellSE <- cellS %>% dplyr::mutate(lon = lon + 1)
  }
  neighbours <- list(cellN, cellS, cellW, cellE, cellNW, cellNE, cellSW, cellSE)
  
  listNeigh <- list(BOATSCentroid)
  for (i in 1:length(neighbours)) {
    neighInd <- which((BOATSdf$lon == neighbours[[i]]$lon) & (BOATSdf$lat == neighbours[[i]]$lat))[1]
    listNeigh <- append(listNeigh, neighInd)
  }
  return(listNeigh)
}


##### Load BOATS Data #####
BOATSdf <-  read_csv(BoatsPath, col_names = c("lon", "lat", "ind")) %>%
  dplyr::filter((lat <= 75) & (lat > (-75))) #OFES only near-global

BOATSsf <- BOATSdf %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = LatLon) 



for (d in 1:length(uniqueYears)){
  print("Working on unique years")
  
  print(d)
  
  parcelsDataUsed <- readRDS(file.path(Data_path, paste0("ParcelsData_", uniqueYears[d], "_", idx, ".rds")))
  
  #get start and end particle centroid BOATS
  sP <- parcelsDataUsed %>%
    dplyr::filter(time == 0) %>%
    dplyr::select(lon, lat)
  
  eP <- parcelsDataUsed %>%
    dplyr::filter(time == 86400) %>% #end point should never be in a different month because we release in the middle of each month and dont track for that long
    dplyr::select(lon, lat)
  
  #still: sanity check
  if (nrow(sP) != nrow(eP)) {
    print("Particles don't end in same month as they start. Check PT data.")
  }
  
  
  rm(parcelsDataUsed)
  
  
  if (d == 1){
    startParticles <- sP
    endParticles <- eP
  } else {
    startParticles <- rbind(startParticles, sP)
    endParticles <- rbind(endParticles, eP)
  }
  
  rm(sP, eP)
  
  StartLoc <- startParticles %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = LatLon)
  
  #get what boats cell particles started in
  StartCentr <- sf::st_nearest_feature(StartLoc, BOATSsf) %>% 
    data.frame() %>% 
    dplyr::rename(Start = ".")
  
  rm(StartLoc)
}



uniqueStartCentr <- sort(unique(StartCentr$Start)) #how many unique start centroids? 

#init probs calculation
LonLatDf <- data.frame() #save lon lat info for calculated probs
ProbsDf <- data.frame() #save probs

print("Working on starting cells")

for (i in 1:length(uniqueStartCentr)) { #loop through all starting cells in boats
  
  MoveInd <- get_Indices(uniqueStartCentr[i], BOATSdf) #get neighbour information
  
  LonLatDf <- rbind(LonLatDf, data.frame(st_coordinates(BOATSsf[uniqueStartCentr[i],]))) #lon lat for current starting cell
  
  BOATSReduced <- BOATSsf[unlist(MoveInd), ] # only get 8 neighboring cells and current cell to reduce spatial analysis load
  
  indStart <- which(StartCentr$Start %in% uniqueStartCentr[i]) # get index of starting centroid
  
  EndLoc <- endParticles[indStart,] %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = LatLon)
  
  #get nearest neigh of end location
  EndCentr <- sf::st_nearest_feature(EndLoc, BOATSReduced) %>% 
    sf::st_drop_geometry() %>% 
    data.frame() %>% 
    dplyr::rename(End = ".")
  
  #get count how many times particles ended up in each of the end cells
  countWhereTo <- as.data.frame(table(EndCentr))
  
  #and sum that to get what to divide by when calculating probs (ideally 100: close to coast sometimes less)
  Total <- sum(countWhereTo$Freq)
  
  #calculate probs
  probs <- numeric(9) #init with all 0 (so when particles never moved to a specific neighbour, that prob stays at 0)
  for (j in 1:length(MoveInd)){ #length(testInd)
    if (j %in% countWhereTo$End) {
      dirNow <- match(j, countWhereTo$End)
      probs[j] <- (countWhereTo[dirNow,]$Freq)/Total #probs calculation
    } else {
      probs <- probs
    }
    
  }
  ProbsDf <- rbind(ProbsDf, as.data.frame(t(probs))) #save probs
  rm(probs)
}

print("Binding probability")
names(ProbsDf) <- c("Stay", "N", "S", "W", "E", "NW", "NE", "SW", "SE")
ProbsDf <- cbind(ProbsDf, LonLatDf) %>%
  dplyr::rename(lon = X, lat = Y)

if (idx < 10){ #easiest way to ensure right order in MATLAB
  currentMonth <- paste0("0", idx)
} else {
  currentMonth <- idx
}


#save probability matrix as csv to load into matlab
write.csv(ProbsDf, file.path(Data_path, paste0("ProbMatrixGlobal_", currentMonth, name_suf,".csv")), row.names=FALSE)

