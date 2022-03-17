# Plotting post-processing data (after Eva's data processing code)

# ----------------------------------------------------------------------
# ----------------- SET SURVEY DATE --------------------------------
# ----------------------------------------------------------------------
if(!exists("year")) stop("No year has been entered. Enter year of survey. For example, enter year <- 2021")
if(!exists("month")) stop("No month has been entered. Enter month of survey. For example, for October, enter month <- '10'")
# month_abb = month.abb[as.numeric(month)]
# survey_title <- paste(first_up(month_abb), year)
# folder = paste0("survey_data/raw_data/",year, month)
# surveyid = paste0("cemore_", year, "_", month)

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# ----------------- LOAD SPATIAL FILES --------------------------------
# ----------------------------------------------------------------------
message("Loading spatial files")
# bc coast for plotting
if(!exists("coast")){
coast <- sf::st_read(dsn = "shapefiles", layer = "BC_coast_UTM9")
}
# higher resolution bc coast --- takes a long time to load
if(!exists("bc_shp")){
  bc_shp <- sf::st_read(dsn = "shapefiles", "BC_AK_WA_union_polygon") %>%
  sf::st_transform(crs = 4326)
}
# Pacific Canada polygon for outlining Canadian border
# canada_shp <- st_read(dsn = "shapefiles", "CanadianEEZ") %>%
#   st_transform(crs = 3156)
# ----------------------------------------------------------------------


# ap_sf %<>% filter(grepl("Porpoise", Species))
# saveRDS(ap_sf, paste0("output/porpoise_sightings_to_",year, month, ".rds"))
coord <- ggplot2::coord_sf(xlim = c(-125.5, -123), ylim = c(48.1, 49.5), crs = sf::st_crs(4326))
pal <- RColorBrewer::brewer.pal(12, "Paired")[c(2:10, 12)]
# coord2<- ggplot2::coord_sf(xlim = c(-123.4, -123.1), ylim = c(48.32, 48.5), crs = sf::st_crs(4326)) # FOR BB

# if require inset to magnify area of intense sightings
# bb <- create_bb(-123.4, -123.1, 48.32, 48.5)
# ap_sf_cropped <- st_intersection(ap_sf, bb)

# Old way of viewing tidy but not processed sightings (would need to import raw data)
# s <- data$sightings %>%
#   dplyr::filter(!is.na(Sgt.Lat) | !is.na(Sgt.Lon), !Species == "Sea Otter") %>% #, incidential.sighting == F) %>%
#   st_as_sf(coords = c("Sgt.Lon", "Sgt.Lat"), crs = 4326) %>%
#   st_transform(crs= 3156)

#---------------------------------------------------------------
# Special case, plotting incidental sightings in Feb 2021
#---------------------------------------------------------------
# # at line 48 in Sighting Position Correction.R file:
# data <- rbind(DATA[which(DATA$Event=="Survey sighting" & DATA$status=="ON"),],DATA[which(DATA$Sgt.ID=="S5a"),]) #EK edit = to include incidentals for plotting feb 2021
# and re-run Eva's code (revert and run again after for only significant sightings in final processed data)

#---------------------------------------------------------------
# Special case, plotting large group of porpoises & incidental grey whale Apr 2021
#---------------------------------------------------------------
# ap_sf[which(ap_sf$Sgt_ID == "S28"),]$Species <- "H. Porpoise group 40-50"
# # at line 48 in Sighting Position Correction.R file:
# data <- rbind(DATA[which(DATA$Event=="Survey sighting" & DATA$status=="ON"),],DATA[which(DATA$Sgt.ID=="S34"),]) #EK edit = to include incidental grey whale sighting for plotting APR 2021
# and re-run Eva's code (revert and run again after for only significant sightings in final processed data)

#---------------------------------------------------------------
# Special case, plotting incidental sightings in May 2021
#---------------------------------------------------------------
# elf <- get_track(file_name = "20210520-LineTransect_MB.gpx", folder = "2021may") # can't use get_track as it changes it to lines
# file_name = "20210520-LineTransect_MB.gpx"
# folder = "2021may"
# dir <- file.path( "C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/survey_data/tracklines/transects") # on oyster drive on small laptop
# t <- plotKML::readGPX(file.path(dir, "gpx", folder, file_name))[[4]][[1]][[1]] %>%
#   mutate(time = time %>% lubridate::ymd_hms() %>% hms::as_hms(), Species = "KW - Transient", SurveyID = "cemore_2021may", month = 5, season = "Spring") %>%
#   filter(hour(time) == 23 & minute(time) == 15) %>%
#   filter(time == min(time)) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#   st_transform(crs = st_crs(bc_shp)) %>%
#   dplyr::select(geometry, Species,SurveyID,month, season)
# ap_sf <- rbind(ap_sf, t)
#---------------------------------------------------------------

#---------------------------------------------------------------
# Special case, plotting incidental fin whale sighting in Aug 2021
#---------------------------------------------------------------
# if(year == 2021 & month == "08" & incidentals == T){
# fin <- data.frame(SurveyID = "cemore_2021aug",
#                   # time = lubridate::ymd_hms("2021-09-02 13:23:00") %>% hms::as_hms(),
#                   Sgt_ID = "none",
#                   Species = "Fin Whale",
#                   year = 2021,
#                   month = 8,
#                   TransectID = "none",
#                   season = "Summer",
#                   month_abb = "Aug",
#                   month_year = "Aug 2021",
#                   lat = 48.43262667,
#                   lon = -124.38709000
#                  ) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326)
# ap_sf <- rbind(ap_sf, fin)
# }
#---------------------------------------------------------------


# #---------------------------------------------------------------
# # -------------- plot sighting distance histograms ---------------------
# #---------------------------------------------------------------
# # for all species
# s <- AP@data %>%
#   filter(!is.na(PSD_nm)) %>%
#   dplyr::mutate(month = as.factor(month(time_index)),
#                 beauf = as.factor(beauf),
#                 season = as.factor(case_when(
#                   month %in% c(5:9) ~ "Summer",
#                   month %in% c(1:4, 10:12) ~ "Winter"
#                 )))
# sp <- "cetacean"
# #---------------------------------------------------------------
# # for single species/group
# sp <- "Humpback"
# sp <- "Harbour Porpoise"
# sp <- "Dalls Porpoise"
# sp <- c("Harbour Porpoise", "Dalls Porpoise", "Unknown Porpoise")
# s <- AP@data %>%
#   filter(!is.na(PSD_nm), Species %in% sp) %>%
#   dplyr::mutate(month = as.factor(month(time_index)),
#                 beauf = as.factor(beauf),
#                 season = as.factor(case_when(
#                   month %in% c(5:9) ~ "Summer",
#                   month %in% c(1:4, 10:12) ~ "Winter"
#                 )))
#
# # sp <- "porpoise"
# #---------------------------------------------------------------
# # to get y value for height of plot text
# y <- ggplot(data = s) + geom_histogram(aes(PSD_nm)) + facet_grid(season ~ Species)
# y <- ggplot_build(y)
# y <- y$data[[1]]$y %>% max()
# #---------------------------------------------------------------
# # to create text for facet grid (for non-facetted plot, group by 'Event') to get only one line for plot text
# # to facet by second variable, add desired variable to 'group_by' here
# tx <- s %>%
#   # group_by(Event) %>%
#   group_by(season) %>%
#   summarise(N = n(),
#             max_dist = paste0(round(max(PSD_nm),2), " nm"),
#             med_dist = paste0(round(median(PSD_nm),2), " nm"),
#             mean_dist = paste0(round(mean(PSD_nm),2), " nm"))
# #---------------------------------------------------------------
# ggplot() +
#   geom_histogram(data = s, aes(PSD_nm, fill = beauf)) +
# # ggplot(data = s, aes(PSD_nm)) + geom_histogram() +
#   ggtitle(paste0("Distances to ", sp, " observations"), subtitle = survey_title) +
#   xlab("Distance (nm)") +
#   ylab("Count") +
#   geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = y, label = paste0("N = ", N))) +
#   geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = 0.93 * y, label = paste0("Max distance = ", max_dist))) +
#   geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = 0.85 * y, label = paste0("Median distance = ", med_dist))) +
#   geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = 0.76 * y, label = paste0("Mean distance = ", mean_dist))) +
#   facet_grid(season ~.)
#
# # for single N, max/mean/median (= non-facetted)
#   # annotate("text", label = paste0("N = ", nrow(s)), x = (0.75 * max(s$PSD_nm)), y = y) +#
#   # annotate("text", label = paste0("Max distance = ", paste0(round(max(s$PSD_nm),2), " nm")), x = (0.75 * max(s$PSD_nm)), y = 0.93 * y) +
#   # annotate("text", label = paste0("Median distance = ", paste0(round(median(s$PSD_nm),2), " nm")), x = (0.75 * max(s$PSD_nm)), y = 0.85 * y) +
#   # annotate("text", label = paste0("Mean distance = ", paste0(round(mean(s$PSD_nm),2), " nm")), x = (0.75 * max(s$PSD_nm)), y = 0.76 * y)
#
# s %>% group_by(beauf) %>% summarise(n = n())
# ggsave(paste0("output_hist/", survey_title, " ", sp, " seasonal sighting distance hist.png"))
# # ----------------------------------------------------------------------



# ----------------------------------------------------------------------
# ----------------- PLOT ON-EFFORT SIGHTINGS --------------------------------
# ----------------------------------------------------------------------

fill <- c('Humpback' = pal[1],
          'Harbour Porpoise' = pal[3],
          'Dalls Porpoise' = pal[5],
          'Unknown Porpoise' = pal[7],
          'KW - Northern Resident' = "black",
          'KW - Southern Resident' = "black",
          'KW - Transient' = "black",
          'KW - Unknown ecotype' = "black",
          'Sei Whale' = "grey 30",
          'Sperm Whale' = pal[10],
          'Cuvier\'s Whale' = pal[4],
          'Fin Whale' = pal[9],
          'Grey Whale' = " purple",
          'H. Porpoise group 40-50' = "black",
          'Minke Whale' = "orange",
          "Acoustic Recorder" = "black")

cols <- c('Humpback' = "dark blue",
          'Harbour Porpoise' = "dark green",
          'Dalls Porpoise' = "dark red",
          'Unknown Porpoise' = pal[7],
          'KW - Northern Resident' = "black",
          'KW - Southern Resident' = "black",
          'KW - Transient' = "black",
          'KW - Unknown ecotype' = "black",
          'Sei Whale' = "grey 30",
          'Sperm Whale' = pal[10],
          'Cuvier\'s Whale' = pal[4],
          'Fin Whale' = "black",
          'Grey Whale' = " purple",
          'H. Porpoise group 40-50' = "black",
          'Minke Whale' = "orange",
          "Acoustic Recorder" = "black")

shape <- c('Humpback' = 24,
           'Harbour Porpoise' = 23,
           'Dalls Porpoise' = 23,
           'Unknown Porpoise' = 23,
           'KW - Northern Resident' = 2,
           'KW - Southern Resident' = 21,
           'KW - Transient' = 19,
           'KW - Unknown ecotype' = 4,
           'Sei Whale' = 21,
           'Sperm Whale' = 21,
           "Cuvier's Whale" = 21,
           "Fin Whale" = 21,
           "Grey Whale" = 21,
           "H. Porpoise group 40-50" = 8,
           "Minke Whale" = 21,
           "Acoustic Recorder" = 8)

Save <- F

if(!Save) coast_file <- coast else coast_file <- bc_shp
# for final high-resolution map
# coast_file <-  bc_shp

#-----------------------------------------------------------
# to add in hydrophone deployment position(s)
#-----------------------------------------------------------
if(hydrophone){
x <- read.csv("hydrophone_data/hydrophone_deployment_locations.csv") #

x <- data.frame(SurveyID = surveyid,
                Sgt_ID = NA,
                Species = "Acoustic Recorder",
                year = 2021,
                month = 8,
                TransectID = NA,
                season = "summer",
                month_abb = "Aug",
                month_year = "Aug 2021",
                lat = x$lat[1],
                lon = x$lon[1]) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
ap_sf <- rbind(ap_sf, x)
}
#-----------------------------------------------------------
# to create bathymetric layer
#-----------------------------------------------------------
bathy <- getNOAA.bathy(-125.5, -122.5,48.1, 49.5,res=1, keep=TRUE) %>%
  fortify(bathy)

bathy$z[which(bathy$z >= 0)] <- 0

col <- rev(RColorBrewer::brewer.pal(9L, "Blues")[4:7])
col_ramp <- colorRampPalette(col)

# ggplot() +
#   geom_sf(data = coast_file, fill = "light yellow", colour = "grey 60") +
#   # geom_sf(data = canada_shp, colour = "black", fill = NA, linetype = "solid") +
#   # geom_sf(data = planned, size = 0.25, colour = "grey 70") + planned routes from "checking transects.." .R
#   geom_sf(data = effort_lines, size = 0.25, colour = "black") +
#   # geom_sf(data = effort_lines, size = 0.25, aes(colour = Date)) +
#   geom_sf(data = ap_sf, size = 1.5, aes(colour = Species, fill = Species, shape =  Species)) + #, stroke =0.05
#
#
#   # geom_sf(data = t, aes(colour = factor(Species), fill = factor(Species), shape =  factor(Species))) +  # added extra incidental sighting, drop=FALSE, breaks = unique(ap_sf$Species)
#   scale_shape_manual(values = shape, name = NULL, breaks  = c("Humpback", "Harbour Porpoise", "Dalls Porpoise", "Fin Whale", "Acoustic Recorder")) + ## Why is this necessary for plotting the legend correctly? Species is a factor so legend should be ordered so.
#   scale_fill_manual(values = cols, name = NULL, breaks = c("Humpback", "Harbour Porpoise", "Dalls Porpoise", "Fin Whale", "Acoustic Recorder"))   +
#   scale_color_manual(values = cols,  name = NULL, breaks = c("Humpback", "Harbour Porpoise", "Dalls Porpoise", "Fin Whale", "Acoustic Recorder")) +
#   # geom_sf(data = x, size = 1, colour = "black") +
#   # geom_sf(data = bb, colour = "black", fill = NA) +
#   coord +
#   ggtitle(paste0("CeMoRe survey cetacean sightings and hydrophone deployment locations ", survey_title)) + #gfplot:::theme_pbs() +
#   # ggtitle(paste0("CeMoRe survey cetacean sightings - High density porpoise sightings ", survey_title)) +
#   theme(panel.background= element_rect(fill = "light blue")) +
#   theme(legend.position = "right")

#--------------------------------------------------------------------------
if(single_survey){
ggplot() +
  geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  labs(fill = "Depth (m)") +
  scale_fill_gradientn(colours = col_ramp(20)) +
  ggnewscale::new_scale("fill") +
  #  + coord_map()
  # geom_contour(data = b,
  #              aes(x=X, y=Y, z=PID),
  #              breaks=c(-250),
  #              size=c(0.6),
  #              colour="grey")+
  geom_sf(data = coast_file, stroke = 0.05, fill = "light yellow", colour = "grey 60")+
  geom_sf(data = effort_lines, size = 0.25, colour = "black") +
  geom_sf(data = ap_sf, colour = "black", stroke = 0.01, size = 1.5, aes(fill = Species, shape =  Species)) +
  scale_shape_manual(values = shape, name = NULL, breaks  = c("Humpback", "Harbour Porpoise", "Dalls Porpoise", "Fin Whale", "Acoustic Recorder")) + ## Why is this necessary for plotting the legend correctly? Species is a factor so legend should be ordered so.
  scale_fill_manual(values = fill, name = NULL, breaks = c("Humpback", "Harbour Porpoise", "Dalls Porpoise", "Fin Whale", "Acoustic Recorder"))   +
  scale_color_manual(values = cols,  name = NULL, breaks = c("Humpback", "Harbour Porpoise", "Dalls Porpoise", "Fin Whale", "Acoustic Recorder")) +
  ggtitle(paste0("CeMoRe survey cetacean sightings ", survey_title)) +
  # ggtitle(paste0("CeMoRe survey cetacean sightings and acoustic recorder deployment location ", survey_title)) +
  coord +
  ylab("")+xlab("")
}



#--------------------------------------------------------------------------


# if saving plot, change coast to bc_shp for higher res basemap
if(Save){
  ggsave(paste0("output_maps/sightings_", survey_title,".png")) #, "_high_res_plus_hydrophone79
}
Save = F
# ggsave(paste0("output_maps/sightings_high_dens_porpoises_", survey_title, "_high_res.png"))
#
# -------------------------------------------------------------------------------
# ---- SPECIES DISTRIBUTION, ALL SURVEYS COMBINED
# -------------------------------------------------------------------------------

# x <- ap_sf %>% filter(Species == "H. Porpoise group 40-50")
if(!single_survey){
ggplot() + geom_sf(data = coast_file, fill = "light yellow", colour = "grey 60") +
  geom_sf(data = ap_sf, size = 1.5, aes(colour = Species, fill = Species, shape =  Species)) +
  # geom_raster(data = bath_df, aes(x = x, y = y, fill = salish_sea_20m)) +
  # geom_raster(data = sub_df, aes(x = x, y = y, fill = salish_sea_20m)) +
  # coord
# ggsave("test_w_bath_and_sub.png")

  #, stroke =0.05
  scale_shape_manual(values = shape, name = NULL) +
  scale_fill_manual(values = cols, name = NULL)   +
  scale_color_manual(values = cols,  name = NULL) +
  coord +
  ggtitle(paste0("All CeMoRe survey cetacean on effort sightings Sept 2020 - ", survey_title)) + #gfplot:::theme_pbs() +
  theme(panel.background= element_rect(fill = "light blue")) +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) #+
  facet_wrap( ~ Species)
}
# # if saving plot, change coast to bc_shp for higher res basemap
# if(save){
#   ggsave(paste0("output_maps/all on effort sightings.png"))
# }
# save = F
# #-------------------------------------------------------------------------------
# #---- SIGHTING PLOTS BY SPECIES, ALL SURVEYS COMBINED, COLOURED BY SURVEY
# #-------------------------------------------------------------------------------
# ap_sf$SurveyID %<>% factor(c("cemore_2020sept",
#                              "cemore_2020oct",
#                              "cemore_2020nov",
#                              "cemore_2021jan",
#                              "cemore_2021feb",
#                              "cemore_2021mar",
#                              "cemore_2021apr",
#                              "cemore_2021may"))
# cols <- c('cemore_2020sept' = "brown",
#           'cemore_2020oct'= "purple",
#           'cemore_2020nov' = "blue",
#           'cemore_2020dec' = "turquoise3",
#           'cemore_2021jan' = "turquoise1",
#           'cemore_2021feb' ="darkturquoise",
#           'cemore_2021mar' = "limegreen",
#           'cemore_2021apr' = "springgreen4",
#           'cemore_2021may' = "yellow",
#           'cemore_2021jun' = "orange",
#           'cemore_2021jul' = "red",
#           'cemore_2021aug' = "magenta")
# save = F
# coast_file <- if(save) bc_shp else coast
# ggplot() + geom_sf(data = coast_file, fill = "light yellow", colour = "grey 60") +
#   # geom_sf(data = canada_shp, colour = "black", fill = NA, linetype = "solid") +
#   # geom_sf(data = effort_lines, size = 0.25, colour = "grey 50") +
#   # geom_sf(data = effort_lines, size = 0.25, aes(colour = Date)) +
#
#   # geom_sf(data = ap_sf, size = 1.5, aes(colour = Species, fill = Species, shape =  Species)) + #, stroke =0.05
#   geom_sf(data = ap_sf, size = 1.5, aes(colour = season)) + #, stroke =0.05
#   # scale_shape_manual(values = shape, name = NULL) +
#   # scale_fill_manual(values = cols, name = NULL)   +
#   scale_color_manual(values = cols ,  name = NULL) +
#   coord +
#   ggtitle(paste0("CeMoRe cetacean sightings by survey")) + #gfplot:::theme_pbs() +
#   # ggtitle(paste0("CeMoRe survey cetacean sightings - High density porpoise sightings ", survey_title)) +
#   theme(panel.background= element_rect(fill = "light blue")) +
#   theme(legend.position = "right") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   facet_wrap( ~ Species)
# # if saving plot, change coast to bc_shp for higher res basemap
# if(save){
#   ggsave(paste0("output_maps/sightings by month.png"))
# }
# save = F
#
# #-------------------------------------------------------------------------------
# #---- SIGHTING PLOTS BY SPECIES, ALL SURVEYS COMBINED, COLOURED BY MONTH
# #-------------------------------------------------------------------------------
# cols <- c('Sep' = "brown",
#           'Oct'= "purple",
#           'Nov' = "blue",
#           'Dec' = "turquoise3",
#           'Jan' = "turquoise1",
#           'Feb' ="darkturquoise",
#           'Mar' = "limegreen",
#           'Apr' = "springgreen4",
#           'May' = "yellow",
#           'Jun' = "orange",
#           'Jul' = "red",
#           'Aug' = "magenta")
# save = F
# coast_file <- if(save) bc_shp else coast
# ggplot() + geom_sf(data = coast_file, fill = "light yellow", colour = "grey 60") +
#   # geom_sf(data = canada_shp, colour = "black", fill = NA, linetype = "solid") +
#   # geom_sf(data = effort_lines, size = 0.25, colour = "grey 50") +
#   # geom_sf(data = effort_lines, size = 0.25, aes(colour = Date)) +
#
#   # geom_sf(data = ap_sf, size = 1.5, aes(colour = Species, fill = Species, shape =  Species)) + #, stroke =0.05
#   geom_sf(data = ap_sf, size = 0.75, aes(colour = factor(month_abb))) + #, stroke =0.05
#   # scale_shape_manual(values = shape, name = NULL) +
#   # scale_fill_manual(values = cols, name = NULL)   +
#   scale_color_manual(values = cols,  name = NULL) +
#   coord +
#   ggtitle(paste0("CeMoRe survey cetacean sightings by month ", survey_title)) + #gfplot:::theme_pbs() +
#   # ggtitle(paste0("CeMoRe survey cetacean sightings - High density porpoise sightings ", survey_title)) +
#   theme(panel.background= element_rect(fill = "light blue")) +
#   theme(legend.position = "right") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   facet_wrap( ~ Species)
# if(save){
#   ggsave(paste0("output_maps/sightings by month.png"))
# }
# save = F
# #-------------------------------------------------------------------------------
# #---- SIGHTING PLOTS BY SPECIES, ALL SURVEYS COMBINED, COLOURED BY SEASON
# #-------------------------------------------------------------------------------
# cols <- c('Fall' = "orange",
#           'Winter'= "blue",
#           'Summer' = "red",
#           'Spring' = "limegreen")
# save = T
# coast_file <- if(save) bc_shp else coast
# ggplot() + geom_sf(data = coast_file, fill = "light yellow", colour = "grey 60") +
#   geom_sf(data = ap_sf, size = 0.8, aes(colour = factor(season))) + #, stroke =0.05
#   scale_color_manual(values = cols,  name = NULL) +
#   coord +
#   ggtitle(paste0("CeMoRe survey cetacean on-effort sightings by season. Plotted  ", survey_title)) + #gfplot:::theme_pbs() +
#   # ggtitle(paste0("CeMoRe survey cetacean sightings - High density porpoise sightings ", survey_title)) +
#   theme(panel.background= element_rect(fill = "light blue")) +
#   theme(legend.position = "right") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   facet_wrap( ~ Species)
#
# # if saving plot, change coast to bc_shp for higher res basemap
# if(save){
#   ggsave(paste0("output_maps/sightings by season ", month, year, ".png"))
# }
# save = F
#
# #-------------------------------------------------------------------------------
# #-------------------------------------------------------------------------------
# #---- CAN RUN TO HERE
# #-------------------------------------------------------------------------------
# #-------------------------------------------------------------------------------

