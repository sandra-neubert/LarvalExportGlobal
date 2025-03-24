gg_add <- list(#scale_x_continuous(expand = c(0, 0)),
               #scale_y_continuous(expand = c(0, 0)),
               ggplot2::theme_bw(),
               ggplot2::theme(
                 legend.position = "bottom",
                 legend.direction = "horizontal",
                 # nrows = 1,
                 legend.title = element_blank(),
                 text = ggplot2::element_text(size = 14, colour = "black"),
                 panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.text = ggplot2::element_text(size = 14, colour = "black"),
                 plot.title = ggplot2::element_text(size = 14),
                 axis.title = ggplot2::element_text(size = 14, colour = "black")
               ),
               labs(y=parse(text='Additional~Harvest~(g~m^-2*~yr^-1)'))
)

gg_plotPreds <- function(m,
                         dist = FALSE, dist_interact = FALSE, dist_interV = "reg",
                         npp_ed = FALSE, npp_ed_interact = FALSE, npp_ed_interV = "reg",
                         moveInd = FALSE, moveInd_interact = FALSE, moveInd_interV = "reg",
                         temp = FALSE, temp_interact = FALSE, temp_interV = "reg",
                         lat = FALSE, lat_interact = FALSE, lat_interV = "reg",
                         lon = FALSE, lon_interact = FALSE, lon_interV = "reg",
                         mpa = FALSE, reg = FALSE) {
  ggDist <- NA
  ggNPP_ed <- NA
  ggMoveInd <- NA
  ggTemp <- NA
  ggLat <- NA
  ggLon <- NA
  ggMPA <- NA
  ggReg <- NA

  if (dist == TRUE) {
    if (dist_interact == FALSE) {
      ggDist <- visreg(m, xvar = "distance", gg = TRUE, scale = "response", partial = FALSE) +
        scale_y_continuous(limits = c(0, 0.5)) 
    } else {
      ggDist <- visreg(m, xvar = "distance", by = dist_interV, gg = TRUE, scale = "response", partial = FALSE, overlay = TRUE) +
        scale_y_continuous(limits = c(0, 0.5)) 
    }
  } 
  
  if (npp_ed == TRUE) {
    if (npp_ed_interact == FALSE) {
      ggNPP_ed <- visreg(m, xvar = "npp_ed", gg = TRUE, scale = "response", partial = FALSE) +
        scale_y_continuous(limits = c(0, 0.35)) 
    } else {
      ggNPP_ed <- visreg(m, xvar = "npp_ed", by = npp_ed_interV, gg = TRUE, scale = "response", partial = FALSE, overlay = TRUE) +
        scale_y_continuous(limits = c(0, 0.35)) 
    } 
  } 
  
  if (moveInd == TRUE) {
    if (moveInd_interact == FALSE) {
      ggMoveInd <- visreg(m, xvar = "moveInd", gg = TRUE, scale = "response", partial = FALSE) +
        scale_y_continuous(limits = c(0, 0.04)) 
    } else {
      ggMoveInd <- visreg(m, xvar = "moveInd", by = moveInd_interV, gg = TRUE, scale = "response", partial = FALSE, overlay = TRUE) +
        scale_y_continuous(limits = c(0, 0.055)) 
    } 
  } 
  
  if (temp == TRUE) {
    if (temp_interact == FALSE) {
      ggTemp <- visreg(m, xvar = "temp", gg = TRUE, scale = "response", partial = FALSE) +
        scale_y_continuous(limits = c(0, 0.03)) 
    } else {
      ggTemp <- visreg(m, xvar = "temp", by = temp_interV, gg = TRUE, scale = "response", partial = FALSE, overlay = TRUE) +
        scale_y_continuous(limits = c(0, 0.03)) 
    } 
  } 
  
  if (lat == TRUE) {
    if (lat_interact == FALSE) {
      ggLat <- visreg(m, xvar = "lat", gg = TRUE, scale = "response", partial = FALSE) +
        scale_y_continuous(limits = c(0, 0.025)) 
    } else {
      ggLat <- visreg(m, xvar = "lat", by = lat_interV, gg = TRUE, scale = "response", partial = FALSE, overlay = TRUE) +
        scale_y_continuous(limits = c(0, 0.025)) 
    } 
  } 
  
  if (lon == TRUE) {
    if (lon_interact == FALSE) {
      ggLon <- visreg(m, xvar = "lon", gg = TRUE, scale = "response", partial = FALSE) +
        scale_y_continuous(limits = c(0, 0.02)) 
    } else {
      ggLon <- visreg(m, xvar = "lon", by = lon_interV, gg = TRUE, scale = "response", partial = FALSE, overlay = TRUE) +
        scale_y_continuous(limits = c(0, 0.02)) 
    } 
  } 
  
  if (mpa == TRUE) {
    ggMPA <- visreg(m, xvar = "mpa", gg = TRUE, scale = "response", partial = FALSE) +
      scale_y_continuous(limits = c(0, 0.015))
  } 
  
  if (reg == TRUE) {
    ggReg<- visreg(m, xvar = "reg", gg = TRUE, scale = "response", partial = FALSE) +
      scale_y_continuous(limits = c(0, 0.025)) 
  } 
  
  return(list(ggDist, ggNPP_ed, ggMoveInd, ggTemp, ggLat, ggLon, ggMPA, ggReg))
}

               