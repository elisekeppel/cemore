
####################################################
# **Plotting covariates with effort/sgts**
####################################################

plot_cov <- function(effort = NULL, sightings = NULL,
                     coast = coast_lr, study.area = T,
                     by_season = NULL, cov = NULL, facet = NULL,
                     save = F, save.dir = file.path("R/8_hw_seasonal/cov plots")){
  # if(is.null(cov)) cov <- by
  if(!is.null(by_season)){
    # data <- data %>% filter(season == by_season)
    if(!is.null(sightings)) sightings <- sightings %>% filter(season == by_season)
    if(!is.null(effort)) effort <- effort %>% filter(season == by_season)
  }
  p <- ggplot() +
    geom_sf(data = predgrid.ext,
            aes(colour = .data[[cov]],
                fill   = .data[[cov]])) +
    scale_colour_viridis_c(trans = "sqrt") +
    scale_fill_viridis_c(trans   = "sqrt") +
    guides(colour = guide_colourbar(order = 1),
           fill   = guide_colourbar(order = 1)) +
    theme_minimal() + theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      title = element_text(size = 7),
      legend.position  = "right",
      legend.title = element_text(size = 6),
      legend.text = element_text(size = 5),
      legend.key.height = unit(0.05, "npc"),
      legend.key.width = unit(0.0075, "npc"))

  if(study.area)          p <- p + geom_sf(data = study.area.can2.crop, colour = "grey10", fill = NA, alpha = 0.8)
  if(!is.null(effort))    p <- p + geom_sf(data = effort, colour = "black", alpha = 0.3, linewidth = 0.2)
  if(!is.null(sightings)) p <- p + geom_sf(data = subset(sightings, size>0), aes(size = size),
                                           alpha = 0.4, linewidth = 0.2, shape = 1, show.legend = FALSE)

  if(!is.null(facet)) p <- p + facet_wrap(~.data[[facet]])

  tit <- paste("")
  if(!is.null(by_season))  tit <- paste(tit, first_up(by_season))
  if(!is.null(cov)) tit <- paste(tit, first_up(cov))

  p <- p + geom_sf(data = coast, colour = "grey60", fill="grey70") +
    coord.aea + ggtitle(tit)

  if(save){
    file <- paste(Sys.Date(), ".png")
    if(!is.null(cov)) file <- paste0(cov, "_", file)
    if(!is.null(facet)){
      file <- paste0("facet_", facet, "_", file)}
    if(!is.null(by_season)) file <- paste0(by_season, "_", file)
    ggsave(plot = p, filename = file.path(dir, save.dir, file))
    knitr::plot_crop(file.path(dir, save.dir, file))

    return(p)
  }
}


# covs <- c("depth.m",
#           "slope",
#           "rug",
#           "tidal",
#           "dist2shore.km",
#           "dist.htidal.km")
#
# # one big plot per cov
# # individual plots for each covariate with all eff and sgt (all months/seasons/years)
# p <- map(covs, ~plot_cov(sightings = obs.sf, effort = seg.sf, cov = ., save = T,
#                          coast = coast_hr))
#
# # all covs on one page for overall time - one plot per cov all on one page  (one page)
# # page with all covs plotted spatially for a time period with effort and sightings
# p[[1]] + p[[2]] + p[[3]] + p[[4]] + p[[5]] + p[[6]] + plot_layout(ncol = 2, nrow=3)
# ggsave(filename = file.path(dir, "R/8_hw_seasonal/cov plots",  paste0("spatial_plots_of_all_stat_covs_", Sys.Date(), ".png")))
# knitr::plot_crop(file.path(dir, "R/8_hw_seasonal/cov plots",  paste0("spatial_plots_of_all_stat_covs_", Sys.Date(), ".png")))
#
# # sesonal page for each cov - 4 plots per each cov on one page (6 pages)
# # individual plots for each covariate plotted with eff and sgt faceted by season
# map(covs, ~plot_cov(sightings = obs.sf, effort = seg.sf, cov = ., save = T,
#                     facet = "season",
#                     coast = coast_hr))
#
# ########################################################################
# # subset data by season
# ########################################################################
# w <- map(covs, ~plot_cov(sightings = obs.sf, effort = seg.sf,
#                          cov = ., save = T, by_season = "Winter",
#                          coast = coast_hr))
#
# w[[1]] + w[[2]] + w[[3]] + w[[4]] + w[[5]] + w[[6]] + plot_layout(ncol = 2, nrow=3)
# ggsave(filename = file.path(dir, "R/8_hw_seasonal/cov plots/Winter",  paste0("spatial_plots_of_all_stat_covs_winter", Sys.Date(), ".png")))
# knitr::plot_crop(file.path(dir, "R/8_hw_seasonal/cov plots/Winter",  paste0("spatial_plots_of_all_stat_covs_winter", Sys.Date(), ".png")))
#
# sp <- map(covs, ~plot_cov(sightings = obs.sf, effort = seg.sf,
#                           cov = ., save = T, by_season = "Spring",
#                           coast = coast_hr))
# sp[[1]] + sp[[2]] + sp[[3]] + sp[[4]] + sp[[5]] + sp[[6]] + plot_layout(ncol = 2, nrow=3)
# ggsave(filename = file.path(dir, "R/8_hwp_seasonal/cov plots/Spring",  paste0("spatial_plots_of_all_stat_covs_spr", Sys.Date(), ".png")))
# knitr::plot_crop(file.path(dir, "R/8_hwp_seasonal/cov plots/Spring",  paste0("spatial_plots_of_all_stat_covs_spr", Sys.Date(), ".png")))
#
# su <- map(covs, ~plot_cov(sightings = obs.sf, effort = seg.sf,
#                           cov = ., save = T, by_season = "Summer",
#                           coast = coast_hr))
#
# su[[1]] + su[[2]] + su[[3]] + su[[4]] + su[[5]] + su[[6]] + plot_layout(ncol = 2, nrow=3)
# ggsave(filename = file.path(dir, "R/8_hw_seasonal/cov plots/Summer",  paste0("spatial_plots_of_all_stat_covs_Summer", Sys.Date(), ".png")))
# knitr::plot_crop(file.path(dir, "R/8_hw_seasonal/cov plots/Summer",  paste0("spatial_plots_of_all_stat_covs_Summer", Sys.Date(), ".png")))
#
# f <- map(covs, ~plot_cov(sightings = obs.sf, effort = seg.sf,
#                          cov = ., save = T, by_season = "Fall",
#                          coast = coast_hr))
#
# f[[1]] + f[[2]] + f[[3]] + f[[4]] + f[[5]] + f[[6]] + plot_layout(ncol = 2, nrow=3)
# ggsave(filename = file.path(dir, "R/8_hw_seasonal/cov plots/Fall",  paste0("spatial_plots_of_all_stat_covs_Fall", Sys.Date(), ".png")))
# knitr::plot_crop(file.path(dir, "R/8_hw_seasonal/cov plots/Fall",  paste0("spatial_plots_of_all_stat_covs_Fall", Sys.Date(), ".png")))
#

