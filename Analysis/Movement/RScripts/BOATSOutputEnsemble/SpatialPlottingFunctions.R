#Functions for spatial plots

gg_add <- list(
  ggplot2::theme_bw(),
  ggplot2::theme(
    #legend.key.width = unit(1.5, 'cm'),
    legend.position = "bottom",
    legend.direction = "horizontal",
    #legend.title = element_blank(),
    text = ggplot2::element_text(size = 30, colour = "black"), #(size = 28, colour = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text = ggplot2::element_text(size = 28, colour = "black"),#(size = 25, colour = "black"),
    plot.title = ggplot2::element_text(size = 30, colour = "black"),#(size = 28),
    axis.title = ggplot2::element_text(size = 30, colour = "black"), #size = 28, colour = "black"),
    legend.text=element_text(size = 30, colour = "black"), #28
    legend.title=element_text(size = 30, colour = "black") # 28
  )#,
  # guides(colour = guide_legend(title.position = "top"))
)

plot_Harvest <- function(H, mpa, limitsH) {
  gg_Harvest <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = H, aes(fill = val, colour = val), size = 0.5) +
    ggplot2::coord_sf(xlim = sf::st_bbox(H)$xlim, ylim = sf::st_bbox(H)$ylim) +
    ggplot2::scale_fill_distiller(
      name = expression("Harvest (gwB m"^"-1" * " y"^"-1" * ")"),
      palette = "Oranges", # "YlOrRd",#"YlGnBu",
      direction = 1,
      breaks = seq(round(limitsH[1], digits = 1),round(limitsH[2]-0.08, digits=1),length.out=5),
      # low = "#fff5eb",
      # high = "#d94801",
      aesthetics = c("colour", "fill"),
      limits = limitsH, #c(
      oob = scales::squish,
      guide = ggplot2::guide_colourbar(
        title.position = "bottom",
        title.hjust = 0.5,
        order = 1,
        #barheight = grid::unit(0.023, "npc"),
        barheight = grid::unit(0.01, "npc"),
        barwidth = grid::unit(0.25, "npc")#,
       # frame.colour = "black"
      )
    ) +
    ggnewscale::new_scale_colour() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = mpa, ggplot2::aes(fill = .data$mpa), colour = "grey70", size = 0.1, show.legend = TRUE) +
    ggplot2::scale_fill_manual(
      name = "",
      values = "grey70",
      labels = "MPA",
      # aesthetics = c("colour", "fill"),
      aesthetics = "fill",
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        order = 2,
        nrow = 1
      )
    ) +
    ggnewscale::new_scale_colour() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = land, colour = "grey50", fill = "grey15", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(mpa)$xlim, ylim = sf::st_bbox(mpa)$ylim) +
    ggplot2::scale_y_continuous(expand = c(0, 0))

  return(gg_Harvest)
}

plot_Diff1 <- function(HDiff, mpa) {
  gg_Diff <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = HDiff_Filt, aes(fill = val, colour = val), size = 0.5) +
    ggplot2::coord_sf(xlim = sf::st_bbox(HDiff_Filt)$xlim, ylim = sf::st_bbox(HDiff_Filt)$ylim) +
    # ggplot2::scale_fill_distiller(
    ggplot2::scale_fill_gradient(
      # palette = "Oranges",
      low = "#FFF5F0",
      high = "#A50611", 
      name = expression(paste(Delta, " Harvest (gwB m"^"-1" * " y"^"-1" * ")")),
      # palette = "Reds", # "YlOrRd",#"YlGnBu",
      # direction = 1,
      aesthetics = c("colour", "fill"),
      limits = c(
        0,
        as.numeric(stats::quantile(dplyr::pull(HDiff_Filt, val), 0.95))
      ),
      oob = scales::squish,
      guide = ggplot2::guide_colourbar(
        title.position = "bottom",
        title.hjust = 0.5,
        order = 1,
        barheight = grid::unit(0.03, "npc"),
        barwidth = grid::unit(0.25, "npc"),
        frame.colour = "black"
      )
    ) +
    ggnewscale::new_scale_colour() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = mpa, ggplot2::aes(fill = .data$mpa), colour = "grey70", size = 0.1, show.legend = TRUE) +
    ggplot2::scale_fill_manual(
      name = "",
      values = "grey70",
      labels = "MPA",
      # aesthetics = c("colour", "fill"),
      aesthetics = "fill",
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        order = 2,
        nrow = 1
      )
    ) +
    ggnewscale::new_scale_colour() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = land, colour = "grey50", fill = "grey15", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(mpa)$xlim, ylim = sf::st_bbox(mpa)$ylim) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) 
  return(gg_Diff)
}




plot_Diff2 <- function(HDiff, mpa, limitsHDiff) {
  gg_Diff <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = HDiff, aes(fill = val, colour = val), size = 0.5) +
    ggplot2::coord_sf(xlim = sf::st_bbox(HDiff)$xlim, ylim = sf::st_bbox(HDiff)$ylim) +
    ggplot2::scale_fill_distiller(
      palette = "RdBu",
      name = expression(paste(Delta, " Harvest (gwB m"^"-1" * " y"^"-1" * ")")),
      # palette = "Reds", # "YlOrRd",#"YlGnBu",
      # direction = 1,
      aesthetics = c("colour", "fill"),
      limits = limitsHDiff, #c(
      #  -(as.numeric(stats::quantile(dplyr::pull(HDiff_Filt, val), 0.95))),
      #  as.numeric(stats::quantile(dplyr::pull(HDiff_Filt, val), 0.95))
      #),
      oob = scales::squish,
      guide = ggplot2::guide_colourbar(
        title.position = "bottom",
        title.hjust = 0.5,
        order = 1,
        #barheight = grid::unit(0.023, "npc"),
        barheight = grid::unit(0.01, "npc"),
        barwidth = grid::unit(0.25, "npc")#,
       # frame.colour = "black"
      )
    ) +
    ggnewscale::new_scale_colour() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = mpa, ggplot2::aes(fill = .data$mpa), colour = "grey70", size = 0.1, show.legend = TRUE) +
    ggplot2::scale_fill_manual(
      name = "",
      values = "grey70",
      labels = "MPA",
      # aesthetics = c("colour", "fill"),
      aesthetics = "fill",
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        order = 2,
        nrow = 1
      )
    ) +
    ggnewscale::new_scale_colour() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = land, colour = "grey50", fill = "grey15", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(mpa)$xlim, ylim = sf::st_bbox(mpa)$ylim) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) 
  
  return(gg_Diff)
}


plot_Distance <- function(mpa_dist, mpa) {
  gg_dist <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = mpa_dist, aes(fill = distance, colour = distance), size = 0.5) +
    ggplot2::coord_sf(xlim = sf::st_bbox(mpa_dist)$xlim, ylim = sf::st_bbox(mpa_dist)$ylim) +
    # 
    scale_fill_viridis_c(
    #ggplot2::scale_fill_distiller(
      name = "Distance to MPA (km)",
      #palette = "Blues", # "YlOrRd",#"YlGnBu",
      #direction = -1,
      aesthetics = c("colour", "fill"),
      oob = scales::squish,
      guide = ggplot2::guide_colourbar(
        title.position = "bottom",
        title.hjust = 0.5,
        order = 1,
        barheight = grid::unit(0.023, "npc"),
        barwidth = grid::unit(0.25, "npc"),
        frame.colour = "black"
      ),
      limits = c(0,1500)
    ) +
    ggnewscale::new_scale_colour() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = mpa, ggplot2::aes(fill = .data$mpa), colour = "grey70", size = 0.1, show.legend = TRUE) +
    ggplot2::scale_fill_manual(
      name = "",
      values = "grey70",
      labels = "MPA",
      # aesthetics = c("colour", "fill"),
      aesthetics = "fill",
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        order = 2,
        nrow = 1
      )
    ) +
    ggnewscale::new_scale_colour() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = land, colour = "grey50", fill = "grey15", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(mpa)$xlim, ylim = sf::st_bbox(mpa)$ylim) +
    ggplot2::scale_y_continuous(expand = c(0, 0))
  
  return(gg_dist)
}


plot_Distance2 <- function(mpa_dist, mpa) {
  gg_dist <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = mpa_dist, aes(fill = distance, colour = distance), size = 0.5) +
    ggplot2::coord_sf(xlim = sf::st_bbox(mpa_dist)$xlim, ylim = sf::st_bbox(mpa_dist)$ylim) +
    scale_fill_cmocean(name = "deep",
                       #   alpha = 1,
                       
                       aesthetics = c("colour", "fill"),
                       direction = -1,
                       limits = c(NA, NA),
                       oob = scales::squish,
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.position = "bottom",
                         order = 1,
                         title.hjust = 0.5,
                         barheight = grid::unit(0.023, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black"),
                       limits = c(0,1500)) +
  ggnewscale::new_scale_colour() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = mpa, ggplot2::aes(fill = .data$mpa), colour = "grey70", size = 0.1, show.legend = TRUE) +
    ggplot2::scale_fill_manual(
      name = "",
      values = "grey70",
      labels = "MPA",
      # aesthetics = c("colour", "fill"),
      aesthetics = "fill",
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0),
        order = 2,
        nrow = 1,
        frame.colour = "black"
      )
    ) +
    ggnewscale::new_scale_colour() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = land, colour = "grey50", fill = "grey15", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(mpa)$xlim, ylim = sf::st_bbox(mpa)$ylim) +
    ggplot2::scale_y_continuous(expand = c(0, 0))
  
  return(gg_dist)
}

plot_MoveInd <- function(move_ind) {
  gg_moveInd <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = move_ind, aes(fill = moveInd, colour = moveInd), size = 0.5) +
    ggplot2::coord_sf(xlim = sf::st_bbox(move_ind)$xlim, ylim = sf::st_bbox(move_ind)$ylim) +
    # 
    scale_fill_viridis_c(
      #ggplot2::scale_fill_distiller(
      name = "Current Speed Index",
      option="magma",
      #palette = "Blues", # "YlOrRd",#"YlGnBu",
      #direction = -1,
      aesthetics = c("colour", "fill"),
      oob = scales::squish,
      guide = ggplot2::guide_colourbar(
        title.position = "bottom",
        title.hjust = 0.5,
        order = 1,
        barheight = grid::unit(0.023, "npc"),
        barwidth = grid::unit(0.25, "npc"),
        frame.colour = "black"
      )
    ) +
    ggnewscale::new_scale_colour() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = land, colour = "grey50", fill = "grey15", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(move_ind)$xlim, ylim = sf::st_bbox(move_ind)$ylim) +
    ggplot2::scale_y_continuous(expand = c(0, 0))
  
  return(gg_moveInd)
}

plot_predictors <- function(pred_df, colInterest, legendTitle = "NPP", paletteName) {
  pred_df<- pred_df %>%
    dplyr::mutate(toPlot = colInterest)
  
  gg_npp <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = pred_df, aes(fill = toPlot, colour = toPlot), size = 0.5) +
    ggplot2::coord_sf(xlim = sf::st_bbox(pred_df)$xlim, ylim = sf::st_bbox(pred_df)$ylim) +
    # 
    scale_fill_viridis_c(
      #ggplot2::scale_fill_distiller(
      name = legendTitle,
      option=paletteName,
      #palette = "Blues", # "YlOrRd",#"YlGnBu",
      direction = 1,
      limits = c(
        0,
        as.numeric(stats::quantile(dplyr::pull(pred_df, toPlot), 0.99))
        ),
      aesthetics = c("colour", "fill"),
      oob = scales::squish,
      guide = ggplot2::guide_colourbar(
        title.position = "bottom",
        title.hjust = 0.5,
        order = 1,
        barheight = grid::unit(0.023, "npc"),
        barwidth = grid::unit(0.25, "npc"),
        frame.colour = "black"
      )
    ) +
  ggnewscale::new_scale_colour() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(data = land, colour = "grey50", fill = "grey15", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    ggplot2::coord_sf(xlim = sf::st_bbox(pred_df)$xlim, ylim = sf::st_bbox(pred_df)$ylim) +
    ggplot2::scale_y_continuous(expand = c(0, 0))
  
  return(gg_npp)
}

