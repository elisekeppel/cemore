# # # new functions created to assist model comparison and plotting when running dsms
# #
# # ####################################################
# # # build a model name from model objec
# # ####################################################
# #
# # get_mod_name <- function(mod){
# #   fam   = strsplit(mod$family$family, "\\(")[[1]][1]
# #   form  = strsplit(as.character(mod$formula)[3], "\\+")[[1]][1]
# #
# #   if(fam %like% "Negative Binomial"){
# #     fam <- "nb"
# #   }else if(fam %like% "Tweedie"){
# #     fam <- "tw"
# #   }
# #   paste(fam, form)
# # }
# #
# # ####################################################
# # # add model name to qq.gam
# # ####################################################
# # qq.gam2 <- function(mod){
# #   qq.gam(mod, asp=1, rep=200, main=get_mod_name(mod), sub)
# # }
# #
# # # reduce columns in your predgrid
# # # pg    <- predgrid.my5 %>% select(x, y, grid.id, monthYear, off.set, height, width, sst, sqr.disthtidal, fYear)
# #
# # ####################################################
# # # **Plotting uncertainty**
# # ####################################################
# pred_ab_by_my <- function(mod, newdata = newdata, res="sum", split_by = "monthYear"){ # res can = "sum" or "data"
#   # get rid of monthYears outside of our bimonthly period
#   # newdata$monthYear <- newdata$monthYear %>% droplevels()
#   grids <- split(newdata, newdata[[split_by]])
#   # make predictions per monthyear grid
#   preds <- map(grids, ~predict(object = mod, newdata=., off.set=.$off.set))
#   names(preds) <- names(grids)
#   # make grid skeleton to append predictions to for each month/year
#   data <- newdata %>% distinct(x, y, grid.id, off.set, height, width)
#   n <- names(data)
#
#   # bind x, y, grid id with preds
#   for(i in seq_along(grids)){
#     data <-cbind.data.frame(data, preds[[i]])
#   }
#   names(data) <- c(n, names(grids))
#
#   # get mean pred per cell
#   data <- data %>%
#     mutate(
#       # mean.2020 = rowMeans(across(c(names(data)[which(names(data) %like% "2020")]))),
#       # mean.2021 = rowMeans(across(c(names(data)[which(names(data) %like% "2021")]))),
#       # mean.2022 = rowMeans(across(c(names(data)[which(names(data) %like% "2022")]))),
#       # mean.2023 = rowMeans(across(c(names(data)[which(names(data) %like% "2023")]))),
#       pred   = rowMeans(across(c(names(preds)))))
#
#   if(res == "sum"){
#     model <- deparse(substitute(mod))
#     res <- data.frame(model, pred = round(sum(data$pred)))
#   }else{
#     # print(paste("Est abund =", round(sum(data$pred))))
#     res <- data
#   }
#   res
# }
# # # x <- pred_ab_by_my(gl_nb_fy_sst_k20, pg)
# # # preddata <- x[[1]]
# # # sumdata <- x[[2]]
# # # plot_pred(mod, preddata, sumdata)
# # # plot_pred(mod, preddata, sumdata, label=T)
# # # plot_pred(mod, preddata, sumdata, byYear=T)
# # # plot_pred(mod, preddata, sumdata, byYear=T, label=T)
# #
# # plot_pred <- function(mod, data, cv = NULL, byYear = F, transf="log10", label=F, study_area = F, buffer = T, coast = coast_lr, points = T){
# #   #########################################
# #   # Prep yearly data
# #   #########################################
# #   if(byYear){
# #     # set up data
# #     grid <- data[,c(1:3)]
# #     plot_data <- c()
# #     for(i in means){
# #       plot_data <- rbind(plot_data,
# #                          cbind(grid, years=i, pred = data[[i]]))
# #     }
# #   }else{
# #     #########################################
# #     # Prep overall data
# #     #########################################
# #     plot_data <- data
# #   }
# #   #########################################
# #   # Prepare plot
# #   #########################################
# #   p <- ggplot(plot_data) +
# #     # geom_tile(aes(x=x, y=y, fill=pred)) +
# #     # coord_equal() +
# #     # geom_point(data = obs, aes(x = x, y = y, size = size), shape = 1, alpha = 0.6)
# #     geom_sf(data = coast, fill = "grey") +
# #     scale_colour_viridis_c(trans = transf, name = "Abundance\nestimate") + #, name = "Relative\ndensity"
# #     scale_fill_viridis_c(trans = transf, "Abundance\nestimate") + # , name = "Relative\ndensity"
# #     guides(colour = guide_colourbar(order = 1),
# #            fill = guide_colourbar(order = 1),
# #            size = guide_legend(title = "Group\nsize")) +
# #     theme(legend.text = element_blank(), axis.title = element_blank(),
# #           legend.position = "right", legend.key.width = unit(0.005, "npc")) +
# #     gg.opts
# #
# #   if(is.null(cv)) p <- p + geom_sf(aes(colour = pred, fill = pred))
# #   if(!is.null(cv)) p <- p + geom_sf(aes(fill = .data[[cv]]))
# #   if(study_area) p <- p + geom_sf(data = study.area.can, fill = NA)
# #   if(buffer)     p <- p + geom_sf(data = study.area.can2.crop, fill = NA)
# #   if(points)     p <- p + geom_sf(data = obs.sf, aes(size = size), shape = 1, alpha = 0.9)
# #
# #   #########################################
# #   # Plot yearly
# #   #########################################
# #   if(byYear){
# #     p <- p + facet_wrap(~years)
# #     if(label){
# #       # prep abund labels
# #       means <- c("mean.2020", "mean.2021", "mean.2022", "mean.2023")
# #       dat.sum <- data.frame(period = names(data[,c(7:19)]), mean_values = colMeans(data[,c(7:19)]),
# #                             sum_values = colSums(data[,c(7:19)]))
# #       dat.sum.years <- filter(dat.sum, period %in% means) %>%
# #         mutate(sum_values=round(sum_values, digits = 0),
# #                label = paste("Est abund by year =", sum_values))
# #       p <- p + geom_text_npc(data = tibble(x=0.02, y=0.98, year = list(dat.sum.years)),
# #                              aes(npcx=x, npcy = y, label = dat.sum.years$label))
# #     }
# #   }else{
# #     #########################################
# #     # Plot overall
# #     #########################################
# #     if(label){
# #       # prep text
# #       df <- summod(mod, newdata=data, forplot = T, abund = T) %>% as.data.frame()
# #       df <- df[,c(1,3)]
# #       names(df) <- NULL
# #       dfnpc <- tibble(x=0.075, y=0.925, df = list(df))
# #
# #       p <- p +
# #         geom_table_npc(data = dfnpc,
# #                        aes(npcx=x, npcy = y, label = df),
# #                        table.theme = ttheme_custom)
# #     }
# #   }
# #   p
# # }
