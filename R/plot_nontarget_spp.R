# #  nontarget species
#
# ################################################################################
# # baseplot
# ################################################################################
# b <- ggplot() + theme(    plot.background = element_rect(fill=NA),
#                           panel.grid.major = element_blank(),
#                           panel.grid.minor = element_blank()) +
#   geom_sf(data=survey_can, fill="white", colour="grey80") +
#   geom_sf(data = coast_file, linewidth = 0.01, fill = "grey 70", colour = "grey 40")
#
# ################################################################################
# # effort
# ################################################################################
# e <- b + geom_sf(data = all_effort_lines, linewidth = 0.1, colour = "grey40", aes(colour = "Survey effort")) +
#   # geom_sf(data = effort_data, size = 0.25, aes(colour = "Survey effort"))# +
#   # scale_colour_manual(values=c("Off effort" = "grey60", "On effort" = "black")) +
#   guides(colour = guide_legend(direction="horizontal"))
#
#
# ################################################################################
# # prep species data
# ################################################################################
# ap_sf <- all_ap_sf
# species <- sp
# if(!is.null(species) & length(species) > 1) ap_sf %<>% dplyr::filter(Species %in% species)
#
# # to not display all potential species in legend
# ap_sf$Species %<>% droplevels()
# # Capitalize species for legend
# lev <- first_up(levels(ap_sf$Species))
# ap_sf$Species <- ap_sf$Species %>%
#   as.character() %>%
#   first_up() %>%
#   factor(levels=lev)
# # to order legend symbols consistently
# sp <- unique(c(levels(ap_sf$Species)))
#
# # to size symbols by count
# ap_sf <- ap_sf %>% mutate(Count =case_when(
#   is.na(Group_Size) ~ "1",
#   Group_Size == 1 ~ "1",
#   Group_Size %in% c(2:5) ~ "2:5",
#   Group_Size >5 ~ ">5"
# ) %>% factor(levels = c("1", "2:5", ">5")))
#
# ################################################################################
# # plot species
# ################################################################################
#
# e + geom_sf(data = ap_sf, shape = 21, alpha = 0.7, colour="black", stroke=0.1,
#             aes(fill = Species, size = Count)) + facet_wrap(.~bimonth, ncol = 2) +
#   scale_size_manual(values = c(1.5, 2.5, 3.5), name="Group Size",
#                     guide = guide_legend(
#                       position = "inside",
#                       theme = theme(
#                         legend.margin = margin(c(2,2,0,2)),
#                         legend.background = element_rect(colour = "black", fill="white")))) +
# scale_fill_manual(values = nt.on, breaks = sp,
#                   guide = guide_legend(
#                     position = "bottom",
#                     title = NULL,
#                     nrow=2, order = 1,
#                     override.aes = list(size=2.5),
#                     theme = theme(legend.spacing.y = unit(0, 'mm'),
#                                   legend.key.spacing.y = unit(-1,"cm"),
#                                   legend.box.spacing = unit(-1, "cm"),
#                                   legend.margin = margin(t=-0.5, b = 0.5, unit = "cm")))) +
#   theme(legend.background = element_blank(),
#         legend.box.margin = margin(0),
#         legend.text = element_text(size=10),#size=fig_legend_size
#         legend.title = element_text(margin = margin(b=0), size=10), #change legend title font size
#         legend.key = element_blank(),
#         legend.key.size = unit(0.5,"cm"),
#         legend.justification =c(0.03,0.97)) +
#   metR::scale_x_longitude(breaks = seq(-125.,-123.,1)) +
#   metR::scale_y_latitude(breaks = seq(48.3,49.5,0.5)) +
#   coord
#
#
