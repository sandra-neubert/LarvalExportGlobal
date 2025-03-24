# Prep Biomass Timeseries Data

library(tidyverse)
InputPath <- "YOUR_PATH/OutputFiles/outputFiles/" #Path to BOATS output data in csv files
OutputPath <- "BOATSOutputEnsemble/summarisedFiles/"
params <- c("ens1", "ens2", "ens3", "ens4", "ens5")
whichMPA <- c("mpa1", "mpa2") # change as needed
whichReg <- "oa" # change as needed

for (i in 1:length(whichMPA)) {
  
  ## MPA -------------------------------------------------------------
  toLoadM1 <- c(paste0("m1_", whichReg, "_mpa0"), paste0("m1_", whichReg, "_", whichMPA)) 
  toLoadM0 <- c(paste0("m0_", whichReg, "_mpa0"), paste0("m0_", whichReg, "_", whichMPA)) 
  toLoad <- c(toLoadM1, toLoadM0)
  matchPCDiff <- unique(gsub("_mpa.", "", toLoad)) # get pattern used to calculate PC and difference
  
  #### Barvest ####
  
  ### plotting prep
  B_sub <- "B_Boats_LEx_d250_"
  #raw data
  for (j in 1:length(toLoadM1)) {
    B <- read_csv(file.path(InputPath, paste0(B_sub, toLoadM1[j], ".csv")),
                  col_names = params
    ) %>%
      mutate(
        mean_ens = pmap_dbl(across(all_of(params)), ~ mean(c(...), na.rm = TRUE)),
        sd_ens = pmap_dbl(across(all_of(params)), ~ sd(c(...), na.rm = TRUE)),
        neg_sd = .data$mean_ens - .data$sd_ens, # to filter out values <0 and set them to 0 for plotting
        pos_sd = .data$mean_ens + .data$sd_ens,
        neg_sd = ifelse(neg_sd < 0, yes = 0, no = neg_sd),
        year = 1877:2126,
        sim = rep(toLoadM1[j], 250)
      ) %>%
      dplyr::select("year", "mean_ens", "neg_sd", "pos_sd", "sim", "sd_ens") %>% #leave out for now so can caluclate PC and difference
      dplyr::filter(year > 1950, year < 2101)
    
    if (j == 1) {
      B_df <- B
    } else {
      B_df <- rbind(B_df, B)
    }
  }
  
  saveRDS(B_df, file.path(OutputPath, paste0(matchPCDiff[1], "_B.rds")))
  rm(B, B_df)
  
  for (k in 1:length(toLoadM0)) {
    B <- read_csv(file.path(InputPath, paste0(B_sub, toLoadM0[k], ".csv")),
                  col_names = params
    ) %>%
      mutate(
        mean_ens = pmap_dbl(across(all_of(params)), ~ mean(c(...), na.rm = TRUE)),
        sd_ens = pmap_dbl(across(all_of(params)), ~ sd(c(...), na.rm = TRUE)),
        neg_sd = .data$mean_ens - .data$sd_ens, # to filter out values <0 and set them to 0 for plotting
        pos_sd = .data$mean_ens + .data$sd_ens,
        neg_sd = ifelse(neg_sd < 0, yes = 0, no = neg_sd),
        year = 1877:2126,
        sim = rep(toLoadM0[k], 250)
      ) %>%
      dplyr::select("year", "mean_ens", "neg_sd", "pos_sd", "sim", "sd_ens") %>% #leave out for now so can caluclate PC and difference
      dplyr::filter(year > 1950, year < 2101)
    
    if (k == 1) {
      B_df <- B
    } else {
      B_df <- rbind(B_df, B)
    }
  }
  
  saveRDS(B_df, file.path(OutputPath, paste0(matchPCDiff[2], "_B.rds")))
  
  rm(B, B_df)
  
  ######### PC and difference
  
  B_mpa0_m0 <- read_csv(file.path(InputPath, paste0(B_sub, toLoadM0[1], ".csv")),
                        col_names = params
  )
  B_mpa1_m0 <- read_csv(file.path(InputPath, paste0(B_sub, "m0_", whichReg, "_", whichMPA[i], ".csv")),
                        col_names = params
  )
  
  B_PC_m0 <- ((B_mpa1_m0 - B_mpa0_m0) / B_mpa0_m0 * 100) %>% 
    dplyr::mutate(   year = 1877:2126) %>%
    dplyr::filter(year > 1950, year < 2101)
  
  B_Diff_m0 <- (B_mpa1_m0 - B_mpa0_m0) %>% 
    dplyr::mutate(   year = 1877:2126) %>%
    dplyr::filter(year > 1950, year < 2101)
  
  
  B_mpa0_m1 <- read_csv(file.path(InputPath, paste0(B_sub, toLoadM1[1], ".csv")),
                        col_names = params
  )
  B_mpa1_m1 <- read_csv(file.path(InputPath, paste0(B_sub, "m1_", whichReg, "_", whichMPA[i], ".csv")),
                        col_names = params
  )
  
  B_PC_m1 <- ((B_mpa1_m1 - B_mpa0_m1) / B_mpa0_m1 * 100) %>% 
    dplyr::mutate(year = 1877:2126) %>%
    dplyr::filter(year > 1950, year < 2101)
  
  B_Diff_m1 <- (B_mpa1_m1 - B_mpa0_m1) %>% 
    dplyr::mutate(   year = 1877:2126) %>%
    dplyr::filter(year > 1950, year < 2101)
  
  B_PC_offset <- (B_PC_m1 %>% select (-"year") - B_PC_m0 %>% select (-"year")) %>%
    mutate(
      mean_ens = pmap_dbl(across(all_of(params)), ~ mean(c(...), na.rm = TRUE)),
      sd_ens = pmap_dbl(across(all_of(params)), ~ sd(c(...), na.rm = TRUE)),
      neg_sd = .data$mean_ens - .data$sd_ens, # to filter out values <0 and set them to 0 for plotting
      pos_sd = .data$mean_ens + .data$sd_ens,
      year = 1951:2100)  %>%
    dplyr::select("year", "mean_ens", "neg_sd", "pos_sd", "sd_ens") 
  
  B_Diff_offset <- (B_Diff_m1 %>% select (-"year") - B_Diff_m0 %>% select (-"year")) %>%
    mutate(
      mean_ens = pmap_dbl(across(all_of(params)), ~ mean(c(...), na.rm = TRUE)),
      sd_ens = pmap_dbl(across(all_of(params)), ~ sd(c(...), na.rm = TRUE)),
      neg_sd = .data$mean_ens - .data$sd_ens, # to filter out values <0 and set them to 0 for plotting
      pos_sd = .data$mean_ens + .data$sd_ens,
      year = 1951:2100)  %>%
    dplyr::select("year", "mean_ens", "neg_sd", "pos_sd", "sd_ens") 
  
  B_PC_m1 <- B_PC_m1 %>%
    mutate(
      mean_ens = pmap_dbl(across(all_of(params)), ~ mean(c(...), na.rm = TRUE)),
      sd_ens = pmap_dbl(across(all_of(params)), ~ sd(c(...), na.rm = TRUE)),
      neg_sd = .data$mean_ens - .data$sd_ens, # to filter out values <0 and set them to 0 for plotting
      pos_sd = .data$mean_ens + .data$sd_ens,
      sim = rep(paste0("m1_", whichReg, "_", whichMPA[i]), 150))  %>%
    dplyr::select("year", "mean_ens", "neg_sd", "pos_sd", "sd_ens", "sim") 
  
  B_PC_m0 <- B_PC_m0 %>%
    mutate(
      mean_ens = pmap_dbl(across(all_of(params)), ~ mean(c(...), na.rm = TRUE)),
      sd_ens = pmap_dbl(across(all_of(params)), ~ sd(c(...), na.rm = TRUE)),
      neg_sd = .data$mean_ens - .data$sd_ens, # to filter out values <0 and set them to 0 for plotting
      pos_sd = .data$mean_ens + .data$sd_ens,
      sim = rep(paste0("m0_", whichReg, "_", whichMPA[i]), 150))  %>%
    dplyr::select("year", "mean_ens", "neg_sd", "pos_sd", "sd_ens", "sim") 
  
  B_Diff_m1 <- B_Diff_m1 %>%
    mutate(
      mean_ens = pmap_dbl(across(all_of(params)), ~ mean(c(...), na.rm = TRUE)),
      sd_ens = pmap_dbl(across(all_of(params)), ~ sd(c(...), na.rm = TRUE)),
      neg_sd = .data$mean_ens - .data$sd_ens, # to filter out values <0 and set them to 0 for plotting
      pos_sd = .data$mean_ens + .data$sd_ens,
      sim = rep(paste0("m1_", whichReg, "_", whichMPA[i]), 150))  %>%
    dplyr::select("year", "mean_ens", "neg_sd", "pos_sd", "sd_ens", "sim") 
  
  B_Diff_m0 <- B_Diff_m0 %>%
    mutate(
      mean_ens = pmap_dbl(across(all_of(params)), ~ mean(c(...), na.rm = TRUE)),
      sd_ens = pmap_dbl(across(all_of(params)), ~ sd(c(...), na.rm = TRUE)),
      neg_sd = .data$mean_ens - .data$sd_ens, # to filter out values <0 and set them to 0 for plotting
      pos_sd = .data$mean_ens + .data$sd_ens,
      sim = rep(paste0("m0_", whichReg, "_", whichMPA[i]), 150))  %>%
    dplyr::select("year", "mean_ens", "neg_sd", "pos_sd", "sd_ens", "sim") 
  
  
  saveRDS(B_Diff_m0, file.path(OutputPath, "longName_B", paste0("m0_", whichReg, "_", whichMPA[i], "_B_diff.rds")))
  saveRDS(B_PC_m0, file.path(OutputPath, "longName_B", paste0("m0_",whichReg, "_", whichMPA[i], "_B_PC.rds")))
  saveRDS(B_Diff_m1, file.path(OutputPath, "longName_B", paste0("m1_",whichReg, "_", whichMPA[i],"_B_diff.rds")))
  saveRDS(B_PC_m1, file.path(OutputPath, "longName_B", paste0("m1_",whichReg, "_", whichMPA[i], "_B_PC.rds")))
  saveRDS(B_Diff_offset, file.path(OutputPath, "longName_B", paste0(whichReg, "_", whichMPA[i], "_B_diff_offset.rds")))
  saveRDS(B_PC_offset, file.path(OutputPath, "longName_B", paste0(whichReg, "_", whichMPA[i], "_B_PC_offset.rds")))
  
}

rm(list=setdiff(ls(), c("OutputPath", "whichMPA", "whichReg")))


if (length(whichMPA) == 2) {
  # Make a vector of all your file paths
  file_paths <- list.files(path = file.path(OutputPath, "longName_B"), pattern = whichReg, full.names = TRUE)
  # Make a vector of file names
  file_names <-  gsub(pattern = "\\.rds$", replacement = "", x = basename(file_paths))
  
  for(i in 1:length(file_names)){
    assign(file_names[i],readRDS(file_paths[i]))
  }
  
  m0_B_diff <- rbind(get(paste0("m0_", whichReg, "_mpa1_B_diff")), 
                     get(paste0("m0_", whichReg, "_mpa2_B_diff")))
  m0_B_PC <- rbind(get(paste0("m0_", whichReg, "_mpa1_B_PC")), 
                   get(paste0("m0_", whichReg, "_mpa2_B_PC")))
  m1_B_diff <- rbind(get(paste0("m1_", whichReg, "_mpa1_B_diff")), 
                     get(paste0("m1_", whichReg, "_mpa2_B_diff")))
  m1_B_PC <- rbind(get(paste0("m1_", whichReg, "_mpa1_B_PC")), 
                   get(paste0("m1_", whichReg, "_mpa2_B_PC")))
  B_PC_offset <- rbind(get(paste0(whichReg, "_mpa1_B_PC_offset")), 
                       get(paste0(whichReg, "_mpa2_B_PC_offset")))
  B_diff_offset <- rbind(get(paste0(whichReg, "_mpa1_B_diff_offset")), 
                         get(paste0(whichReg, "_mpa2_B_diff_offset")))
  
  saveRDS(m0_B_diff, file.path(OutputPath, paste0("m0_", whichReg, "_B_diff.rds")))
  saveRDS(m0_B_PC, file.path(OutputPath, paste0("m0_", whichReg, "_B_PC.rds")))
  saveRDS(m1_B_diff, file.path(OutputPath, paste0("m1_", whichReg, "_B_diff.rds")))
  saveRDS(m1_B_PC, file.path(OutputPath, paste0("m1_", whichReg, "_B_PC.rds")))
  saveRDS(B_PC_offset, file.path(OutputPath, paste0(whichReg, "_B_PC_offset.rds")))
  saveRDS(B_diff_offset, file.path(OutputPath, paste0(whichReg, "_B_diff_offset.rds")))
}

