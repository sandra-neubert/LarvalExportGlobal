# prep Timeseries 2 (calculate offset directly from data)

library(tidyverse)
InputPath <- "~/MME/MA/BOATS_repos/HPC_Runs/OutputFiles/outputFiles/"
OutputPath <- "BOATSOutputEnsemble/summarisedFiles/"
params <- c("ens1", "ens2", "ens3", "ens4", "ens5")
whichMPA <- c("mpa1", "mpa2") # change as needed
whichReg <- "msy" # change as needed

for (i in 1:length(whichMPA)) {

## MPA -------------------------------------------------------------
toLoadM1 <- c(paste0("m1_", whichReg, "_mpa0"), paste0("m1_", whichReg, "_", whichMPA)) 
toLoadM0 <- c(paste0("m0_", whichReg, "_mpa0"), paste0("m0_", whichReg, "_", whichMPA)) 
toLoad <- c(toLoadM1, toLoadM0)
matchPCDiff <- unique(gsub("_mpa.", "", toLoad)) # get pattern used to calculate PC and difference

#### Harvest ####

### plotting prep
H_sub <- "H_Boats_LEx_d250_"
#raw data
for (j in 1:length(toLoadM1)) {
  H <- read_csv(file.path(InputPath, paste0(H_sub, toLoadM1[j], ".csv")),
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
    H_df <- H
  } else {
    H_df <- rbind(H_df, H)
  }
}

saveRDS(H_df, file.path(OutputPath, paste0(matchPCDiff[1], "_H.rds")))
rm(H, H_df)

for (k in 1:length(toLoadM0)) {
  H <- read_csv(file.path(InputPath, paste0(H_sub, toLoadM0[k], ".csv")),
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
    H_df <- H
  } else {
    H_df <- rbind(H_df, H)
  }
}

saveRDS(H_df, file.path(OutputPath, paste0(matchPCDiff[2], "_H.rds")))

rm(H, H_df)

######### PC and difference

H_mpa0_m0 <- read_csv(file.path(InputPath, paste0(H_sub, toLoadM0[1], ".csv")),
                      col_names = params
)
H_mpa1_m0 <- read_csv(file.path(InputPath, paste0(H_sub, "m0_", whichReg, "_", whichMPA[i], ".csv")),
                      col_names = params
)

H_PC_m0 <- ((H_mpa1_m0 - H_mpa0_m0) / H_mpa0_m0 * 100) %>% 
  dplyr::mutate(   year = 1877:2126) %>%
  dplyr::filter(year > 1950, year < 2101)

H_Diff_m0 <- (H_mpa1_m0 - H_mpa0_m0) %>% 
  dplyr::mutate(   year = 1877:2126) %>%
  dplyr::filter(year > 1950, year < 2101)


H_mpa0_m1 <- read_csv(file.path(InputPath, paste0(H_sub, toLoadM1[1], ".csv")),
                   col_names = params
)
H_mpa1_m1 <- read_csv(file.path(InputPath, paste0(H_sub, "m1_", whichReg, "_", whichMPA[i], ".csv")),
                   col_names = params
)

H_PC_m1 <- ((H_mpa1_m1 - H_mpa0_m1) / H_mpa0_m1 * 100) %>% 
  dplyr::mutate(year = 1877:2126) %>%
  dplyr::filter(year > 1950, year < 2101)

H_Diff_m1 <- (H_mpa1_m1 - H_mpa0_m1) %>% 
  dplyr::mutate(   year = 1877:2126) %>%
  dplyr::filter(year > 1950, year < 2101)

H_PC_offset <- (H_PC_m1 %>% select (-"year") - H_PC_m0 %>% select (-"year")) %>%
  mutate(
    mean_ens = pmap_dbl(across(all_of(params)), ~ mean(c(...), na.rm = TRUE)),
    sd_ens = pmap_dbl(across(all_of(params)), ~ sd(c(...), na.rm = TRUE)),
    neg_sd = .data$mean_ens - .data$sd_ens, # to filter out values <0 and set them to 0 for plotting
    pos_sd = .data$mean_ens + .data$sd_ens,
    year = 1951:2100, 
    sim = rep(paste0("offset_", whichMPA[i], "_", whichReg), 150))  %>%
  dplyr::select("year", "mean_ens", "neg_sd", "pos_sd", "sd_ens", "sim") 

H_Diff_offset <- (H_Diff_m1 %>% select (-"year") - H_Diff_m0 %>% select (-"year")) %>%
  mutate(
    mean_ens = pmap_dbl(across(all_of(params)), ~ mean(c(...), na.rm = TRUE)),
    sd_ens = pmap_dbl(across(all_of(params)), ~ sd(c(...), na.rm = TRUE)),
    neg_sd = .data$mean_ens - .data$sd_ens, # to filter out values <0 and set them to 0 for plotting
    pos_sd = .data$mean_ens + .data$sd_ens,
    year = 1951:2100, 
    sim = rep(paste0("offset_", whichMPA[i], "_", whichReg), 150))  %>%
  dplyr::select("year", "mean_ens", "neg_sd", "pos_sd", "sd_ens", "sim") 

H_PC_m1 <- H_PC_m1 %>%
  mutate(
    mean_ens = pmap_dbl(across(all_of(params)), ~ mean(c(...), na.rm = TRUE)),
    sd_ens = pmap_dbl(across(all_of(params)), ~ sd(c(...), na.rm = TRUE)),
    neg_sd = .data$mean_ens - .data$sd_ens, # to filter out values <0 and set them to 0 for plotting
    pos_sd = .data$mean_ens + .data$sd_ens,
    sim = rep(paste0("m1_", whichReg, "_", whichMPA[i]), 150))  %>%
  dplyr::select("year", "mean_ens", "neg_sd", "pos_sd", "sd_ens", "sim") 

H_PC_m0 <- H_PC_m0 %>%
  mutate(
    mean_ens = pmap_dbl(across(all_of(params)), ~ mean(c(...), na.rm = TRUE)),
    sd_ens = pmap_dbl(across(all_of(params)), ~ sd(c(...), na.rm = TRUE)),
    neg_sd = .data$mean_ens - .data$sd_ens, # to filter out values <0 and set them to 0 for plotting
    pos_sd = .data$mean_ens + .data$sd_ens,
    sim = rep(paste0("m0_", whichReg, "_", whichMPA[i]), 150))  %>%
  dplyr::select("year", "mean_ens", "neg_sd", "pos_sd", "sd_ens", "sim") 

H_Diff_m1 <- H_Diff_m1 %>%
  mutate(
    mean_ens = pmap_dbl(across(all_of(params)), ~ mean(c(...), na.rm = TRUE)),
    sd_ens = pmap_dbl(across(all_of(params)), ~ sd(c(...), na.rm = TRUE)),
    neg_sd = .data$mean_ens - .data$sd_ens, # to filter out values <0 and set them to 0 for plotting
    pos_sd = .data$mean_ens + .data$sd_ens,
    sim = rep(paste0("m1_", whichReg, "_", whichMPA[i]), 150))  %>%
  dplyr::select("year", "mean_ens", "neg_sd", "pos_sd", "sd_ens", "sim") 

H_Diff_m0 <- H_Diff_m0 %>%
  mutate(
    mean_ens = pmap_dbl(across(all_of(params)), ~ mean(c(...), na.rm = TRUE)),
    sd_ens = pmap_dbl(across(all_of(params)), ~ sd(c(...), na.rm = TRUE)),
    neg_sd = .data$mean_ens - .data$sd_ens, # to filter out values <0 and set them to 0 for plotting
    pos_sd = .data$mean_ens + .data$sd_ens,
    sim = rep(paste0("m0_", whichReg, "_", whichMPA[i]), 150))  %>%
  dplyr::select("year", "mean_ens", "neg_sd", "pos_sd", "sd_ens", "sim") 


saveRDS(H_Diff_m0, file.path(OutputPath, "longName", paste0("m0_", whichReg, "_", whichMPA[i], "_H_diff.rds")))
saveRDS(H_PC_m0, file.path(OutputPath, "longName", paste0("m0_",whichReg, "_", whichMPA[i], "_H_PC.rds")))
saveRDS(H_Diff_m1, file.path(OutputPath, "longName", paste0("m1_",whichReg, "_", whichMPA[i],"_H_diff.rds")))
saveRDS(H_PC_m1, file.path(OutputPath, "longName", paste0("m1_",whichReg, "_", whichMPA[i], "_H_PC.rds")))
saveRDS(H_Diff_offset, file.path(OutputPath, "longName", paste0(whichReg, "_", whichMPA[i], "_H_diff_offset.rds")))
saveRDS(H_PC_offset, file.path(OutputPath, "longName", paste0(whichReg, "_", whichMPA[i], "_H_PC_offset.rds")))

}

rm(list=setdiff(ls(), c("OutputPath", "whichMPA", "whichReg")))


if (length(whichMPA) == 2) {
  # Make a vector of all your file paths
  file_paths <- list.files(path = file.path(OutputPath, "longName"), pattern = whichReg, full.names = TRUE)
  # Make a vector of file names
  file_names <-  gsub(pattern = "\\.rds$", replacement = "", x = basename(file_paths))

  for(i in 1:length(file_names)){
    assign(file_names[i],readRDS(file_paths[i]))
  }
  
  m0_H_diff <- rbind(get(paste0("m0_", whichReg, "_mpa1_H_diff")), 
                     get(paste0("m0_", whichReg, "_mpa2_H_diff")))
  m0_H_PC <- rbind(get(paste0("m0_", whichReg, "_mpa1_H_PC")), 
                   get(paste0("m0_", whichReg, "_mpa2_H_PC")))
  m1_H_diff <- rbind(get(paste0("m1_", whichReg, "_mpa1_H_diff")), 
                     get(paste0("m1_", whichReg, "_mpa2_H_diff")))
  m1_H_PC <- rbind(get(paste0("m1_", whichReg, "_mpa1_H_PC")), 
                   get(paste0("m1_", whichReg, "_mpa2_H_PC")))
  H_PC_offset <- rbind(get(paste0(whichReg, "_mpa1_H_PC_offset")), 
                       get(paste0(whichReg, "_mpa2_H_PC_offset")))
  H_diff_offset <- rbind(get(paste0(whichReg, "_mpa1_H_diff_offset")), 
                       get(paste0(whichReg, "_mpa2_H_diff_offset")))
  
  saveRDS(m0_H_diff, file.path(OutputPath, paste0("m0_", whichReg, "_H_diff.rds")))
  saveRDS(m0_H_PC, file.path(OutputPath, paste0("m0_", whichReg, "_H_PC.rds")))
  saveRDS(m1_H_diff, file.path(OutputPath, paste0("m1_", whichReg, "_H_diff.rds")))
  saveRDS(m1_H_PC, file.path(OutputPath, paste0("m1_", whichReg, "_H_PC.rds")))
  saveRDS(H_PC_offset, file.path(OutputPath, paste0(whichReg, "_H_PC_offset.rds")))
  saveRDS(H_diff_offset, file.path(OutputPath, paste0(whichReg, "_H_diff_offset.rds")))
}

