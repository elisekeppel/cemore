# new functions created to assist model comparison and plotting when running dsms

####################################################
# build a model name from model objec
####################################################

get_mod_name <- function(mod){
  fam   = strsplit(mod$family$family, "\\(")[[1]][1]
  form  = strsplit(as.character(mod$formula)[3], "\\+")[[1]][1]

  if(fam %like% "Negative Binomial"){
    fam <- "nb"
  }else if(fam %like% "Tweedie"){
    fam <- "tw"
  }
  paste(fam, form)
}

####################################################
# add model name to qq.gam
####################################################
qq.gam2 <- function(mod){
  qq.gam(mod, asp=1, rep=200, main = get_mod_name(mod), sub)
}

# reduce columns in your predgrid
# pg    <- predgrid.my5 %>% select(x, y, grid.id, monthYear, off.set, height, width, sst, sqr.disthtidal, fYear)

#####################################################
# **Plotting uncertainty**
#####################################################
pred_ab_spt <- function(mod, newdata, pred_by, res="sum"){ # res can = "sum" or "data"
  newdata$pred <- predict(object = mod, newdata = newdata, off.set = newdata$off.set)

  if(!is.null(pred_by)){
    widedata <- newdata %>%
      group_by(grid.id, x, y, off.set) %>%
      summarise(pred = mean(pred))
  }else{
    widedata <- newdata
  }

  # pivot_wider(id_cols = c(grid.id, x, y), names_from = monthYear, values_from = preds) %>%
  #   mutate(
  #     get mean pred per cell
  #     mean.2020 = rowMeans(across(c(names(data)[which(names(data) %like% "2020")]))),
  #     mean.2021 = rowMeans(across(c(names(data)[which(names(data) %like% "2021")]))),
  #     mean.2022 = rowMeans(across(c(names(data)[which(names(data) %like% "2022")]))),
  #     mean.2023 = rowMeans(across(c(names(data)[which(names(data) %like% "2023")]))),
  #     pred   = rowMeans(across(my)))

  if(res == "sum"){
    model <- deparse(substitute(mod))
    res <- data.frame(model, pred = round(sum(widedata$pred)))
  }else{
    res <- widedata
  }
  return(res)
}

# x <- pred_ab_by_my(gl_nb_fy_sst_k20, pg)
# preddata <- x[[1]]
# sumdata <- x[[2]]
# plot_pred(mod, preddata, sumdata)
# plot_pred(mod, preddata, sumdata, label=T)
# plot_pred(mod, preddata, sumdata, byYear=T)
# plot_pred(mod, preddata, sumdata, byYear=T, label=T)

plot_pred <- function(mod,
                      plotdat,
                      var = NULL,
                      # pred_by = F,
                      transf="log10",
                      # label=F,
                      # study_area = F,
                      # buffer = T,
                      coast = coast_lr,
                      points = T){
  #########################################
  # Prep yearly data
  #########################################
  # if(byYear){
  #   # set up data
  #   grid <- data[,c(1:3)]
  #   plot_data <- c()
  #   for(i in means){
  #     plot_data <- rbind(plot_data,
  #                        cbind(grid, years=i, pred = data[[i]]))
  #   }
  # }else{
  #########################################
  # Prep overall data
  #########################################
  # plot_data <- data
  # }
  #########################################
  # Prepare plot
  #########################################
  p <- ggplot(plotdat) +
    # geom_tile(aes(x=x, y=y, fill=pred)) +
    # coord_equal() +
    # geom_point(data = obs, aes(x = x, y = y, size = size), shape = 1, alpha = 0.6)
    geom_sf(data = coast, fill = "grey") +
    scale_colour_viridis_c(trans = transf, name = "Abundance\nestimate") + #, name = "Relative\ndensity"
    scale_fill_viridis_c(trans = transf, "Abundance\nestimate") + # , name = "Relative\ndensity"
    guides(colour = guide_colourbar(order = 1),
           fill = guide_colourbar(order = 1),
           size = guide_legend(title = "Group\nsize")) +
    theme(legend.text = element_blank(), axis.title = element_blank(),
          legend.position = "right", legend.key.width = unit(0.005, "npc")) +
    gg.opts

  if(is.null(var)) p <- p + geom_sf(aes(colour = pred, fill = pred))
  if(!is.null(var)) p <- p + geom_sf(aes(fill = .data[[cv]]))
  # if(study_area) p <- p + geom_sf(data = study.area.can, fill = NA)
  # if(buffer)
  p <- p + geom_sf(data = study.area.can2.crop, fill = NA) +
    # if(points)
    p + geom_sf(data = obs.sf, aes(size = size), shape = 1, alpha = 0.9)

  #########################################
  # Plot yearly
  #########################################
  # if(!is.null(pred_by)){
  #   p <- p + facet_wrap(~.data[[pred_by]])
  #   if(label){
  #     # prep abund labels
  #     means <- c("mean.2020", "mean.2021", "mean.2022", "mean.2023")
  #     dat.sum <- data.frame(period = names(data[,c(7:19)]), mean_values = colMeans(data[,c(7:19)]),
  #                           sum_values = colSums(data[,c(7:19)]))
  #     dat.sum.years <- filter(dat.sum, period %in% means) %>%
  #       mutate(sum_values=round(sum_values, digits = 0),
  #              label = paste("Est abund by year =", sum_values))
  #     p <- p + geom_text_npc(data = tibble(x=0.02, y=0.98, year = list(dat.sum.years)),
  #                            aes(npcx=x, npcy = y, label = dat.sum.years$label))
  #   }
  # }else{
  #########################################
  # Plot overall
  #########################################
  # if(label){
  # prep text
  df <- summod(mod, newdata=data, forplot = T, abund = T) %>% as.data.frame()
  df <- df[,c(1,3)]
  names(df) <- NULL
  dfnpc <- tibble(x=0.075, y=0.925, df = list(df))

  p <- p +
    geom_table_npc(data = dfnpc,
                   aes(npcx=x, npcy = y, label = df),
                   table.theme = ttheme_custom)
  # }
  # }
  p
}

plot_cv <- function(mod, predgrid.sf, transf = "log10", study_area = F, buffer = T, effort = F, points = T, coast = coast_lr){
  mod.name = deparse(substitute(mod))
  predgrid.sf$height <- predgrid.sf$width <- 2
  pred_split <- split(predgrid.sf, 1:nrow(predgrid.sf))

  var_split <- dsm_var_prop.x(mod, pred_split,
                              off.set = predgrid$off.set)
  predgrid.sf$cv <- sqrt(var_split$pred.var)/unlist(var_split$pred)

  big.CV <- dsm_var_prop.x(mod, predgrid, off.set = predgrid$off.set)
  big.CV <- summary(big.CV)$cv %>% round(digits = 3)

  p <- ggplot(predgrid.sf) +
    geom_sf(data = coast, fill = "grey") +
    geom_sf(aes(fill = cv, colour = cv)) +
    scale_fill_viridis_c(option = "E", trans = transf, name = "CV",
                         aesthetics = c("colour", "fill")) +
    ggtitle(mod.name, subtitle = big.CV)

  if(study_area)   p <- p + geom_sf(data = study.area.can, fill = "transparent")
  if(buffer)       p <- p + geom_sf(data = study.area.can2.crop, fill = "transparent")
  if(effort)       p <- p + geom_sf(data = seg.sf, alpha = 0.3)
  if(points)       p <- p + geom_sf(data = subset(obs.sf, size > 0), aes(size = size),
                                    shape = 1, alpha = 0.8, show.legend = F)

  p <- p + guides(colour = guide_colourbar(order = 1),
                  fill = guide_colourbar(order = 1)) +
    theme(legend.text = element_blank(), axis.title = element_blank(),
          legend.position = "right", legend.key.width = unit(0.005, "npc")) +
    gg.opts + ggtitle(paste("Overall CV : ", as.character(big.CV)), subtitle = mod.name)
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

