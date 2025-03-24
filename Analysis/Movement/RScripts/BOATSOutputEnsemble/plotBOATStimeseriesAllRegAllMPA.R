# Time series plots 
# Fig. 1

library(tidyverse)
library(patchwork)
InputPath <- "BOATSOutputEnsemble/summarisedFiles/"
FigurePath <- "BOATSOutputEnsemble/Figures/"

gg_add <- list(
  scale_x_continuous(expand = c(0, 0)),
  scale_y_continuous(expand = c(0, 0)),
  expand_limits(y=0),
  ggplot2::theme_bw(),
  ggplot2::theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    text = ggplot2::element_text(size = 14, colour = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = ggplot2::element_text(size = 14, colour = "black"),
    plot.title = ggplot2::element_text(size = 14),
    axis.title = ggplot2::element_text(size = 14, colour = "black"),
    legend.text=element_text(size = 14, colour = "black"),
    plot.margin = unit(
      c(5.5, 15, 5.5, 5.5),
      "pt"
    )
  ),
  geom_vline(xintercept = c(1995, 2015, 2030), colour = "darkgrey", linetype = "dashed", size = 0.7),
  geom_line(linewidth = 1.5),
  geom_ribbon(aes(ymin = neg_sd, ymax = pos_sd, fill = sim), alpha = 0.2, color = NA)
)


# write script so we have one plot for all scenarios: 
#4 columns (oa_mpa1; msy_mpa1, oa_mpa2, msy_mpa2) , 3 rows (B, H, PC)

whichMPA <- c("mpa1", "mpa2")
harvestType <- c("oa", "msy") # add msy when all data there

for (i in 1:length(harvestType)) {
  for (j in 1:length(whichMPA)) {
  ### Harvest raw ###
  
  m1_H <- readRDS(file.path(InputPath, paste0("m1_", harvestType[i], "_H.rds"))) %>%
    dplyr::filter(sim == paste0("m1_", harvestType[i], "_", whichMPA[j]) |
                  sim == paste0("m1_", harvestType[i], "_mpa0"),
                  year < 2051) 
  
  m0_H <- readRDS(file.path(InputPath, paste0("m0_", harvestType[i], "_H.rds"))) %>%
    dplyr::filter(sim == paste0("m0_", harvestType[i], "_", whichMPA[j]) |
                  sim == paste0("m0_", harvestType[i], "_mpa0"),
                  year < 2051) 
  
  H <- rbind(m1_H, m0_H) %>%
    dplyr::mutate(
      sim = replace(sim, str_detect(sim, "^m0\\w*mpa0$"), "-Export -MPAs"),
      sim = replace(sim, str_detect(sim, "^m0\\w*mpa[1-2]$"), paste0("-Export +MPAs")),
      sim = replace(sim, str_detect(sim, "^m1\\w*mpa0$"), "+Export -MPAs"),
      sim = replace(sim, str_detect(sim, "^m1\\w*mpa[1-2]$"), paste0("+Export +MPAs"))
    )
  
  gg_H <- ggplot(H, aes(year, mean_ens, color = sim)) +
    gg_add +
    scale_fill_manual(
      values = c("darkgrey", "#003399", "darkgrey", "coral"),
      aesthetics = c("color", "fill"),
      guide = ggplot2::guide_legend(
        nrow = 2
      )
    ) +
    ggplot2::labs(x = "Year", y = expression(paste(" Harvest (Mt ", yr^{
      -1
    }, ")")))
  
  assign(paste0("gg_H_", harvestType[i], "_", whichMPA[j]), gg_H)
  
  ### Harvest PC ###
  
  m1_HPC <- readRDS(file.path(InputPath, paste0("m1_", harvestType[i], "_H_PC.rds"))) %>%
    dplyr::filter(sim == paste0("m1_", harvestType[i], "_", whichMPA[j]) |
                  sim == paste0("m1_", harvestType[i], "_mpa0"),
                  year < 2051) 
  
  m0_HPC <- readRDS(file.path(InputPath, paste0("m0_", harvestType[i], "_H_PC.rds"))) %>%
    dplyr::filter(sim == paste0("m0_", harvestType[i], "_", whichMPA[j]) |
                  sim == paste0("m0_", harvestType[i], "_mpa0"),
                  year < 2051)
  
  offset_HPC <- readRDS(file.path(InputPath, paste0(harvestType[i], "_H_PC_offset.rds"))) %>%
    dplyr::filter(sim == paste0("offset_", whichMPA[j], "_", harvestType[i]),
                  year < 2051) 
  
  HPC <- do.call("rbind", list(m1_HPC, m0_HPC, offset_HPC)) %>%
    dplyr::mutate(
      sim = replace(sim, str_detect(sim, "^m0"), "-Export"),
      sim = replace(sim, str_detect(sim, "^m1"), "+Export"),
      sim = replace(sim, str_detect(sim, "^offset"), "Offset")
    )
  
  gg_HPC <- ggplot(HPC, aes(year, mean_ens, color = sim)) +
    gg_add +
    scale_fill_manual(values = c("#003399", "coral", "black"), aesthetics = c("color", "fill")) +
    ggplot2::labs(x = "Year", y = "Percentage Change (%)")
  
  assign(paste0("gg_HPC_", harvestType[i], "_", whichMPA[j]), gg_HPC)
  
  # clean up env
  remove(
    gg_H, m0_H, m1_H, 
    gg_HPC, m0_HPC, m1_HPC
  )
  
  ### Biomass raw ###
  
  m1_B <- readRDS(file.path(InputPath, paste0("m1_", harvestType[i], "_B.rds"))) %>%
    dplyr::filter(sim == paste0("m1_", harvestType[i], "_", whichMPA[j]) |
                  sim == paste0("m1_", harvestType[i], "_mpa0"),
                  year < 2051) 
  
  m0_B <- readRDS(file.path(InputPath, paste0("m0_", harvestType[i], "_B.rds"))) %>%
    dplyr::filter(sim == paste0("m0_", harvestType[i], "_", whichMPA[j]) |
                  sim == paste0("m0_", harvestType[i], "_mpa0"),
                  year < 2051) 
  
  B <- rbind(m1_B, m0_B) %>%
    dplyr::mutate(
      sim = replace(sim, str_detect(sim, "^m0\\w*mpa0$"), "-Export -MPAs"),
      sim = replace(sim, str_detect(sim, "^m0\\w*mpa[1-2]$"), paste0("-Export +MPAs")),
      sim = replace(sim, str_detect(sim, "^m1\\w*mpa0$"), "+Export -MPAs"),
      sim = replace(sim, str_detect(sim, "^m1\\w*mpa[1-2]$"), paste0("+Export +MPAs"))
    )
  
  gg_B <- ggplot(B, aes(year, mean_ens, color = sim)) +
    gg_add +
    scale_fill_manual(
      values = c("darkgrey", "#003399", "darkgrey", "coral"),
      aesthetics = c("color", "fill"),
      guide = ggplot2::guide_legend(
        nrow = 2
      )
    ) +
    ggplot2::labs(x = "Year", y = "Biomass (Mt)")
  
  assign(paste0("gg_B_", harvestType[i], "_", whichMPA[j]), gg_B)
  
  
  # clean up env
  remove(
    gg_B, m0_B, m1_B)
  }
}

timeseries_row1 <- ((gg_B_msy_mpa1 + 
                       ggplot2::ggtitle("Maximum Sustainable Yield \n(Economics-focused scenario)") + 
                       ggplot2::theme(
                         legend.position = "none",
                         axis.title.x = element_blank(),
                         axis.text.x = element_blank()
                       ))
                    + (gg_H_msy_mpa1 + ggplot2::theme(
                      legend.position = "none",
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank()
                    )) 
                    + (gg_HPC_msy_mpa1 + ggplot2::theme(
                      legend.position = "none",
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank()
                    )))

timeseries_row2 <- ((gg_B_oa_mpa1 + 
                       ggplot2::ggtitle("Open-Access \n(Economics-focused scenario)") + 
                       ggplot2::theme(
                         legend.position = "none",
                         axis.title.x = element_blank(),
                         axis.text.x = element_blank()
                       )) 
                    + (gg_H_oa_mpa1 + ggplot2::theme(
                      legend.position = "none",
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank()
                    ))
                    + (gg_HPC_oa_mpa1 + ggplot2::theme(
                      legend.position = "none",
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank()
                    ))
) 

timeseries_row3 <- ((gg_B_msy_mpa2 + 
                       ggplot2::ggtitle("Maximum Sustainable Yield \n(Biodiversity-focused scenario)") + 
                       ggplot2::theme(
                         legend.position = "none",
                         axis.title.x = element_blank(),
                         axis.text.x = element_blank()
                       ))
                    + (gg_H_msy_mpa2 + ggplot2::theme(
                      legend.position = "none",
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank()
                    )) 
                    + (gg_HPC_msy_mpa2 + ggplot2::theme(
                      legend.position = "none",
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank()
                    )))

timeseries_row4 <- ((gg_B_oa_mpa2 + 
                       ggplot2::ggtitle("Open-Access \n(Biodiversity-focused scenario)")) 
                    + (gg_H_oa_mpa2)
                    + (gg_HPC_oa_mpa2)
) 

timeseries_plots <- timeseries_row1 / timeseries_row2 / timeseries_row3 / timeseries_row4 + plot_annotation(tag_levels = 'a')

ggsave(
  plot = timeseries_plots,
  filename = file.path(FigurePath, paste0("TimeseriesAll_offset.png")),
  width = 12, height = 15, dpi = 300
)


# timeseries_row1 <- ((gg_B_msy_mpa1 + 
#                          ggplot2::ggtitle("Maximum Sustainable Yield (Economics-focused scenario)") + 
#                          ggplot2::theme(
#   legend.position = "none",
#   axis.title.x = element_blank(),
#   axis.text.x = element_blank()
# ))
# + (gg_B_oa_mpa1 + 
#      ggplot2::ggtitle("Open-Access (Economics-focused scenario)") + 
#      ggplot2::theme(
#        legend.position = "none",
#        axis.title.x = element_blank(),
#        axis.text.x = element_blank()
#      ))
# + (gg_B_msy_mpa2 + 
#      ggplot2::ggtitle("Maximum Sustainable Yield (Biodiversity-focused scenario)") + 
#      ggplot2::theme(
#        legend.position = "none",
#        axis.title.x = element_blank(),
#        axis.text.x = element_blank()
#      ))
# + (gg_B_oa_mpa2 + 
#      ggplot2::ggtitle("Open-Access (Biodiversity-focused scenario)") + 
#      ggplot2::theme(
#        legend.position = "none",
#        axis.title.x = element_blank(),
#        axis.text.x = element_blank()
#      )))  +
#     plot_layout(ncol = 4) 
#   
#   
#   
# timeseries_row2 <-((gg_H_msy_mpa1) # replace with MSY harvest plots
#      + (gg_H_oa_mpa1)#+ ggplot2::ggtitle("Maximum Sustainable Yield"))
#      + (gg_H_msy_mpa2)
#      + (gg_H_oa_mpa2)) +
#   plot_layout(ncol = 4) 
# 
# timeseries_row3 <-((gg_HPC_msy_mpa1) # replace with MSY harvest plots
#                    + (gg_HPC_oa_mpa1)#+ ggplot2::ggtitle("Maximum Sustainable Yield"))
#                    + (gg_HPC_msy_mpa2)
#                    + (gg_HPC_oa_mpa2)) +
#   plot_layout(ncol = 4) 
# 
#    # plot_annotation(tag_levels = 'a')) #+ ggplot2::ggtitle("Maximum Sustainable Yield")))) # ) + plot_layout(guides = "collect") & theme(legend.position = 'bottom'))
# 
# timeseries_plots1 <- timeseries_row1 / timeseries_row2 #+ plot_layout(guides = "collect") & theme(legend.position = 'bottom')
# 
# timeseries_plots <- timeseries_plots1 / timeseries_row3 + plot_annotation(tag_levels = 'a')

