
plot_cv <- function(mod, predgrid, transf = "log10", study_area = F, buffer = T, effort = F, points = T, coast = coast_lr){
  mod.name = deparse(substitute(mod))
  # predgrid$height <- predgrid$width <- 2
  pred_split <- split(predgrid, 1:nrow(predgrid))

  var_split <- dsm_var_prop.x(mod, pred_split,
                              off.set = predgrid$off.set)
  predgrid$cv <- sqrt(var_split$pred.var)/unlist(var_split$pred)

  big.CV <- dsm_var_prop.x(mod, predgrid, off.set = predgrid$off.set) %>% summary()
  big.CV <- big.CV$cv %>% round(digits = 3)

  p <- ggplot() +
    geom_tile(data = predgrid, aes(x=x, y=y, fill=CV)) +
    geom_point(data = obs, aes(x = x, y = y, size = size), shape = 1, alpha = 0.7, show.legend = F) +
    # geom_sf(data = coast, fill = "grey") +
    # geom_sf(aes(fill = cv, colour = cv)) +
    coord_equal() +
    scale_fill_viridis_c(option = "E", trans = transf, name = "CV",
                         aesthetics = c("colour", "fill")) +
    ggtitle(mod.name, subtitle = big.CV)

  # if(study_area)   p <- p + geom_sf(data = study.area.can, fill = "transparent")
  # if(buffer)       p <- p + geom_sf(data = study.area.can2.crop, fill = "transparent")
  if(effort)       p <- p + geom_sf(data = seg.sf, alpha = 0.3)
  # if(points)       p <- p + geom_sf(data = subset(obs.sf, size > 0), aes(size = size),
  #                                   shape = 1, alpha = 0.8, show.legend = F)

  p <- p + guides(colour = guide_colourbar(order = 1),
                  fill = guide_colourbar(order = 1)) +
    theme(legend.text = element_blank(), axis.title = element_blank(),
          legend.position = "right", legend.key.width = unit(0.005, "npc")) +
    gg.opts + ggtitle(paste("Overall CV : ", as.character(big.CV)), subtitle = mod.name) #+ coord.aea
  p
}

####################################################
# **Plotting uncertainty**
####################################################
# dsm_nb_vargam_plot <- dsm_var_prop.x(dsm_nb_xy, predgrid.sf_var_split,
#                              off.set=predgrid.sf$off.set)
# dsm_tw_vargam_plot <- dsm_var_prop.x(dsm_tw_xy, predgrid.sf_var_split,
#                              off.set=predgrid.sf$off.set)
# # can use plot=FALSE to get the ggplot object that we can modify
# pl_nb <- plot(dsm_nb_vargam_plot, plot=FALSE, observations=FALSE)
# pl_tw <- plot(dsm_tw_vargam_plot, plot=FALSE, observations=FALSE)

# modify plot
# pl_nb <- pl_nb +
#   geom_point(aes(x=x, y=y), data=dsm_nb_xy$data, colour="lightgrey", alpha=.5, size=0.2) +
#   theme(legend.position="bottom") +  scale_fill_viridis_c()
# pl_tw <- pl_tw +
#   geom_point(aes(x=x, y=y), data=dsm_tw_xy$data, colour="lightgrey", alpha=.5, size=0.2) +
#   theme(legend.position="bottom") + scale_fill_viridis_c()
# pl_nb + pl_tw

# plot_cv(dsm_nb_xy)
# plot_cv(dsm_tw_xy)


# annotate(geom = 'text',
#          x=1070000, y=485000, label=paste("Overall CV : ", big.CV))
