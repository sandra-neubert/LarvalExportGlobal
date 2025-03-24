#Anthony Richardson and Sandra Neubert
# GAM on additional harvest due to larval export
# 10/11/23

library(tidyverse)
library(visreg)
library(splines)
library(mgcv)
library(patchwork)

FigurePath <- "BOATSOutputEnsemble/Figures/"

# snapshot model year 164 (ten years after 30% MPAs were implemented;2040)
# val is variable of interest: additional Harvest (gwB m-2 yr-1) (yearly mean)
# npp_ed (mmolC m^-3 d^-1) is npp (mmolC m^-2 s^-1) integrated over euphotic zone (yearly mean)
# temp is in °C (yearly mean)
# mpa is whether the simulation was MPA1 (economics focused) or MPA2 (biodiversity focused)
# reg is open access (oa) vs maximum sustainable yield (msy)
# moveInd is movement index, a proxy for current speed: probability (0-1) that larvae will move out of cell within one time step (yearly mean)
# distance is distance to closest MPA (km)
# inMPA was used to filter only cells outside of MPAs (where we would see additional harvest), can be -select("inMPA)

GLMDataFilt <- readRDS(file.path("BOATSOutputEnsemble", "FilesSpatial", "GLMDataFilt.rds")) %>%
  na.omit() %>%
  dplyr::mutate(val = replace(val, val >=5, 5),
                val = replace(val, val < 0, 0),
                npp_ed = replace(npp_ed, npp_ed >= 3, 3),
                moveInd = replace(moveInd, moveInd > 0.65, 0.65),
                reg = as.factor(reg),
                mpa = as.factor(mpa), 
                distance = replace(distance, distance > 1000, 1000),
                mpa = fct_recode(mpa, 
                                 "economics" = "mpa1",
                                 "biodiversity" = "mpa2"),
                reg = fct_recode(reg, 
                                 "Open-Access" = "oa",
                                 "MSY" = "msy")) 


minNonNeg <- GLMDataFilt %>%
  dplyr::filter(val >0)

minVal <- min(minNonNeg$val)

NonNeg <- minNonNeg %>%
  dplyr::filter(val < 0.0001)

GLMDataNeg <- readRDS(file.path("BOATSOutputEnsemble", "FilesSpatial", "GLMDataFilt.rds")) %>%
  na.omit() %>%
  dplyr::filter(val <0 )


DF <- 4

# #interaction with npp_ed and distance, no longitude or latitude 
m1 <- glm((val + 0.01)  ~ ns(moveInd, DF) + reg * ns(distance, DF) + 
             reg * ns(npp_ed, DF) + ns(temp, DF) + mpa,
           family = Gamma(link = "log"), GLMDataFilt)
#par(mfrow = c(3,2))
#plot(m1)
summary(m1)
plotM1 <- gg_plotPreds(m1, 
                        dist = TRUE, dist_interact = TRUE, 
                        npp_ed = TRUE, npp_ed_interact = TRUE,
                        moveInd = TRUE,
                        temp = TRUE,
                        lat = FALSE,
                        lon = FALSE,
                        mpa = TRUE,
                        reg = FALSE)

ggM1 <- (((plotM1[[1]] + gg_add + ggplot2::labs(x = "Distance to MPA (km)")) + 
             (plotM1[[2]] + scale_y_continuous(limits = c(0, 0.4))+ gg_add + ggplot2::labs(x =  parse(text='NPP~(mmolC~m^-3*~d^-1)')) + ggplot2::theme(
               axis.title.y = element_blank()
             ))+ plot_layout(guides = "collect") & theme(legend.position = 'bottom')) /
            ((plotM1[[3]] + gg_add + ggplot2::labs(x = "Current Speed Index")) +
               (plotM1[[4]] + gg_add + ggplot2::labs(x = "Temperature (°C)") + ggplot2::theme(
                 axis.title.y = element_blank()
               )) + 
               (plotM1[[7]] + scale_y_continuous(limits = c(0, 0.02)) + gg_add + ggplot2::labs(x = "MPA") + ggplot2::theme(
                 axis.title.y = element_blank()
               )))) + 
  plot_annotation(tag_levels = "a",
                  title = paste0("R2 = ", round(with(summary(M1), 1 - deviance/null.deviance), 2))
  ) #+ plot_layout(guides = "collect") & theme(legend.position = 'bottom')

ggsave(plot = ggM1,
       filename = file.path(FigurePath, "GLM", paste0("ggM1.png")),
       width = 11, height = 9, dpi = 400) 




