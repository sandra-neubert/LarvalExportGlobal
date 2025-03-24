# Harvest difference plots
# Fig. S1

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
#4 columns (oa_mpa1; msy_mpa1, oa_mpa2, msy_mpa2) , 1 row (H_diff)

whichMPA <- c("mpa1", "mpa2")
harvestType <- c("oa", "msy") # add msy when all data there

for (i in 1:length(harvestType)) {
  for (j in 1:length(whichMPA)) {
    ### Harvest Diff ###
    
    m1_Hdiff <- readRDS(file.path(InputPath, paste0("m1_", harvestType[i], "_H_diff.rds"))) %>%
      dplyr::filter(sim == paste0("m1_", harvestType[i], "_", whichMPA[j]) |
                      sim == paste0("m1_", harvestType[i], "_mpa0"),
                    year < 2051) 
    
    m0_Hdiff <- readRDS(file.path(InputPath, paste0("m0_", harvestType[i], "_H_diff.rds"))) %>%
      dplyr::filter(sim == paste0("m0_", harvestType[i], "_", whichMPA[j]) |
                      sim == paste0("m0_", harvestType[i], "_mpa0"),
                    year < 2051)
    
    offset_Hdiff <- readRDS(file.path(InputPath, paste0(harvestType[i], "_H_diff_offset.rds"))) %>%
      dplyr::filter(sim == paste0("offset_", whichMPA[j], "_", harvestType[i]),
                    year < 2051) 
    
    Hdiff <- do.call("rbind", list(m1_Hdiff, m0_Hdiff)) %>%#, offset_Hdiff)) %>%
      dplyr::mutate(
        sim = replace(sim, str_detect(sim, "^m0"), "-Export"),
        sim = replace(sim, str_detect(sim, "^m1"), "+Export")#,
        #sim = replace(sim, str_detect(sim, "^offset"), "Offset")
      )
    
    gg_Hdiff <- ggplot(Hdiff, aes(year, mean_ens, color = sim)) +
      gg_add +
      scale_fill_manual(values = c("#003399", "coral"), aesthetics = c("color", "fill")) +
      ggplot2::labs(x = "Year", y = expression(paste(Delta, " Harvest (Mt ", yr^{
        -1
      }, ")")))
    
    assign(paste0("gg_Hdiff_", harvestType[i], "_", whichMPA[j]), gg_Hdiff)
    
    # clean up env
    remove(
      gg_H, m0_H, m1_H, 
      gg_Hdiff, m0_Hdiff, m1_Hdiff
    )}}

timeseries_row1 <- (gg_Hdiff_msy_mpa1 + 
                       ggplot2::ggtitle("Maximum Sustainable Yield \n(Economics-focused scenario)") + 
                       ggplot2::theme(
                         legend.position = "none",
                         axis.title.x = element_blank(),
                         axis.text.x = element_blank()
                       ))

timeseries_row2 <- (gg_Hdiff_oa_mpa1 + 
                       ggplot2::ggtitle("Open-Access \n(Economics-focused scenario)") + 
                       ggplot2::theme(
                         legend.position = "none",
                         axis.title.x = element_blank(),
                         axis.text.x = element_blank()
                       )) 

timeseries_row3 <- (gg_Hdiff_msy_mpa2 + 
                       ggplot2::ggtitle("Maximum Sustainable Yield \n(Biodiversity-focused scenario)") + 
                       ggplot2::theme(
                         legend.position = "none",
                         axis.title.x = element_blank(),
                         axis.text.x = element_blank()
                       ))

timeseries_row4 <- (gg_Hdiff_oa_mpa2 + 
                       ggplot2::ggtitle("Open-Access \n(Biodiversity-focused scenario)")) 
                    
timeseries_plots <- timeseries_row1 / timeseries_row2 / timeseries_row3 / timeseries_row4 + plot_annotation(tag_levels = 'a')

ggsave(
  plot = timeseries_plots,
  filename = file.path(FigurePath, paste0("TimeseriesAll_offset_Hdiff.png")),
  width = 6, height = 15, dpi = 300
)