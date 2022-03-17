# after Eva's code
plot_survey <- function(Save = F, 
                        single_survey=T,
                        incidentals=F,
                        hydrophone=F,
                        sightings_only=F){
  
  # 
  # # ----------------------------------------------------------------------
  # # ----------------- SET SURVEY DATE --------------------------------
  # # ----------------------------------------------------------------------
  # {year = 2021
  # month = "10"
  # month_abb <- month.abb[as.numeric(month)]
  # survey_title <- paste(first_up(month_abb), year) 
  # folder = paste0("survey_data/raw_data/",year,  "-",month)
  # surveyid = paste0("cemore_", year, tolower(month_abb))}
  # # ----------------------------------------------------------------------
  
  # ----------------------------------------------------------------------
  # ----------------- LOAD SPATIAL FILES --------------------------------
  # ----------------------------------------------------------------------
  # bc coast for plotting
  if(!Save & !exists("coast")){
    coast <- sf::st_read(dsn = "shapefiles", layer = "BC_coast_UTM9", quiet = T)
    coast_file <- coast
  }
  # higher resolution bc coast --- takes a long time to load
  if(Save & !exists("bc_shp")){
    bc_shp <- sf::st_read(dsn = "shapefiles", "BC_AK_WA_union_polygon") %>%
      sf::st_transform(crs = 4326)
    coast_file <- bc_shp
  }
  # Pacific Canada polygon for outlining Canadian border
  # canada_shp <- st_read(dsn = "shapefiles", "CanadianEEZ") %>% 
  #   st_transform(crs = 4326) 
  
  #-----------------------------------------------------------
  # to create bathymetric layer
  #-----------------------------------------------------------
  {bathy <- getNOAA.bathy(-125.5, -122.5,48.1, 49.5,res=1, keep=TRUE) %>% 
    fortify(bathy) 
  
  bathy$z[which(bathy$z >= 0)] <- 0
  
  col <- rev(RColorBrewer::brewer.pal(9L, "Blues")[4:7])
  col_ramp <- colorRampPalette(col)}
  
  # ----------------------------------------------------------------------
  
  # ap_sf %<>% filter(grepl("Porpoise", Species))
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
  # Special case, incidental sightings Oct 2021
  #---------------------------------------------------------------
  # s <- bind_rows(s1,s2) %>% 
  #   mutate(month = month(time_index),
  #          SurveyID = "cemore_2021oct", 
  #          month = 10, 
  #          year = 2021,
  #          season = "Fall",
  #          TransectID = "14",
  #          month_abb = "Oct",
  #          month_year = "Oct 2021",
  #          Sgt_ID = "S99a",
  #          beauf = 1,
  #          PSD_nm = 0.01,
  #          BestNumber = Best.Cnt) %>% 
  #   tidyr::separate(GPS.Pos, into = c("lat", "lon"), sep = "N") %>% 
  #   dplyr::mutate(lon = substr(lon, 2, nchar(lon)-3)) %>% 
  #   dplyr::mutate(lat = substr(lat, 1, nchar(lat)-2)) %>% 
  #   tidyr::separate(lon, into = c("lon.deg", "lon.min"), sep = " ") %>% 
  #   tidyr::separate(lat, into = c("lat.deg", "lat.min"), sep = " ") %>% 
  #   mutate(lon = -(as.numeric(lon.deg) + as.numeric(lon.min)/60),
  #          lat = as.numeric(lat.deg) + as.numeric(lat.min)/60) %>% 
  #   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  #   dplyr::select("SurveyID", "Sgt_ID", "Species","year","month","TransectID","season","month_abb","month_year","geometry", "beauf", "PSD_nm", "BestNumber")
  
  #---------------------------------------------------------------
  # Special case, incidental sightings Nov 2021
  #---------------------------------------------------------------
  # s <- s %>% # filter(Species == "Minke Whale") %>% 
  #   mutate(month = month(time_index),
  #          SurveyID = surveyid,
  #          month = 11,
  #          year = 2021,
  #          season = "Fall",
  #          TransectID = iteration,
  #          month_abb = month_abb,
  #          month_year = survey_title,
  #          Sgt_ID = "S99a",
  #          beauf = 1,
  #          PSD_nm = 0.01,
  #          BestNumber = Best.Cnt) 
  # %>%
  #   tidyr::separate(GPS.Pos, into = c("lat", "lon"), sep = "N") %>%
  #   dplyr::mutate(lon = substr(lon, 2, nchar(lon)-3)) %>%
  #   dplyr::mutate(lat = substr(lat, 1, nchar(lat)-2)) %>%
  #   tidyr::separate(lon, into = c("lon.deg", "lon.min"), sep = " ") %>%
  #   tidyr::separate(lat, into = c("lat.deg", "lat.min"), sep = " ") %>%
  #   mutate(lon = -(as.numeric(lon.deg) + as.numeric(lon.min)/60),
  #          lat = as.numeric(lat.deg) + as.numeric(lat.min)/60) %>%
  #   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  #   dplyr::select("SurveyID", "Sgt_ID", "Species","year","month","TransectID","season","month_abb","month_year","geometry", "beauf", "PSD_nm", "BestNumber")
  
  # incidentals plotted at GPS position of ship
  if(incidentals){
    inc <- incid %>% 
      tidyr::separate(GPS.Pos, into = c("lat", "lon"), sep = "N") %>% 
      dplyr::mutate(lon = substr(lon, 2, nchar(lon)-3)) %>% 
      dplyr::mutate(lat = substr(lat, 1, nchar(lat)-2)) %>% 
      tidyr::separate(lon, into = c("lon.deg", "lon.min"), sep = " ") %>% 
      tidyr::separate(lat, into = c("lat.deg", "lat.min"), sep = " ") %>% 
      mutate(lon = -(as.numeric(lon.deg) + as.numeric(lon.min)/60),
             lat = as.numeric(lat.deg) + as.numeric(lat.min)/60,
             BestNumber = Best.Cnt, Count = Best.Cnt) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      dplyr::select("Species","BestNumber","Count", geometry)
    ap_sf <- bind_rows(ap_sf,inc)
  }
  
  
  #-----------------------------------------------------------
  # to add in hydrophone deployment position(s)
  #-----------------------------------------------------------
  # (see hydrophone/hydrophone.R)
  if(hydrophone){
    source("R/hydrophone.R")
    # acoust <- y %>% filter(deployment_year == year, deployment_month == month)
    acoust <- y %>% filter(deployment_year == 2021, month(retrieval_date) == 11)
  }
  # ap_sf <- rbind(ap_sf, x)
  
  # ----------------------------------------------------------------------
  # ----------------- PLOT ON-EFFORT SIGHTINGS --------------------------------
  # ----------------------------------------------------------------------
  coord <- ggplot2::coord_sf(xlim = c(-125.3, -123), ylim = c(48.2, 49.44), crs = sf::st_crs(4326)) 
  # pal <- RColorBrewer::brewer.pal(12, "Paired")[c(2,4,3)]
  pal <- brewer.pal(9, "Set1")[c(1:9)] #red,blue,green,purple,orange,yellow,brown,pink,grey
  
  
  cols <- c('Humpback' = pal[1],
            'Harbour Porpoise' = pal[3],
            'Dalls Porpoise' = pal[4],
            'Unknown Porpoise' = pal[9],
            'KW - Northern Resident' = "black",
            'KW - Southern Resident' = "black",
            'KW - Transient' = "black",
            'KW - Unknown ecotype' = "black",
            'Fin Whale' = pal[5],
            'Grey Whale' = pal[8],
            'Minke Whale' = pal[7])
  
  shape <- c('Humpback' = 21,
             'Harbour Porpoise' = 21,
             'Dalls Porpoise' = 21,
             'Unknown Porpoise' = 21,
             'KW - Northern Resident' = 24,
             'KW - Southern Resident' = 25,
             'KW - Transient' = 21,
             'KW - Unknown ecotype' = 22,
             "Fin Whale" = 21,
             "Grey Whale" = 21,
             "Minke Whale" = 21)
  # shape <- c('Humpback' = 24,
  #            'Harbour Porpoise' = 23,
  #            'Dalls Porpoise' = 23,
  #            'Unknown Porpoise' = 23,
  #            'KW - Northern Resident' = 24,
  #            'KW - Southern Resident' = 25,
  #            'KW - Transient' = 22,
  #            'KW - Unknown ecotype' = 23,
  #            "Fin Whale" = 21,
  #            "Grey Whale" = 21,
  #            "Minke Whale" = 21,
  #            "H. Porpoise group 40-50" = 8)
  
  
  
  
  
  # make bathy legend
  b_leg <- ggplot() +
    geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  labs(fill = "Depth (m)") +
    scale_fill_gradientn(colours = col_ramp(20)) +
    theme(legend.position = "bottom") +
    guides(fill = guide_colorbar(title.position = "left"))
  
  # leg1 <- gtable_filter(ggplot_gtable(ggplot_build(b_leg)), "guide-box") 
  # leg1Grob <- grobTree(leg1)
  
  leg1 <- cowplot::get_legend(b_leg)
  
  
  #-------------------------------------------------------
  if(single_survey){
    #-------------------------------------------------------
    # to not display all potential species in legend
    ap_sf$Species %<>% droplevels()
    # s$Species %<>% droplevels()
    
    # to order legend symbols consistently
    # sp <- unique(c(levels(ap_sf$Species), levels(s$Species)))
    sp <- unique(c(levels(ap_sf$Species)))
    
    # to size symbols by count
    ap_sf <- ap_sf %>% mutate(Count =case_when(
      BestNumber == 1 ~ "1",
      BestNumber %in% c(2:5) ~ "2:5",
      BestNumber >5 ~ ">5"
    ) %>% factor(levels = c("1", "2:5", ">5")))
    # s <- s %>% mutate(Count =case_when(
    #   Best.Cnt == 1 ~ "1",
    #   Best.Cnt %in% c(2:5) ~ "2:5",
    #   Best.Cnt >5 ~ ">5"
    # ) %>% factor(levels = c("1", "2:5", ">5")))
    
    # create object 'g' basemap with effort
    g <- ggplot() +
      geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  labs(fill = "Depth (m)") +
      scale_fill_gradientn(colours = col_ramp(20), guide = "none") +
      ggnewscale::new_scale("fill") +
      #  + coord_map()
      # geom_contour(data = b,
      #              aes(x=X, y=Y, z=PID),
      #              breaks=c(-250),
      #              size=c(0.6),
      #              colour="grey")+
      geom_sf(data = coast_file, stroke = 0.05, fill = "light yellow", colour = "grey 60")+
      geom_sf(data = effort_lines, size = 0.25, colour = "black")
    
    g
    if(hydrophone){
      g <- g +
        geom_sf(data = acoust, aes(shape = deployed)) +
        scale_shape_manual(values = 8, name = NULL) +  
        ggnewscale::new_scale("shape") 
    }
    
    g <- g + geom_sf(data = ap_sf, stroke = 0, alpha = 0.8, aes(fill = Species, shape = Species, size = Count)) +#
      # geom_sf(data = s, stroke = 0.01,alpha = 0.5, aes(fill = Species, shape = Species, size = Count)) +
      
      scale_size_manual(values = c(1.5,2,3)) +
      scale_fill_manual(values = cols, name = "Sightings", breaks = sp)   +
      scale_shape_manual(values = shape, name = "Sightings", breaks = sp) +
      
      guides(alpha= "none") +
      guides(shape = guide_legend(override.aes = list(size=2))) +
      
      # ggtitle(paste0("CeMoRe survey ", survey_title), 
      #         subtitle = "Cetacean sightings and acoustic recorder deployment locations") +
      annotation_custom(leg1, xmin=-119.1, xmax=-124.95, ymin=47.95, ymax=48.1) +
      
      coord +
      ylab("")+xlab("")
    
  }else{
    if(sightings_only){
      #--------------- SIGHTINGS ONLY ----------------------------------------
      # to not display all potential species in legend
      all_ap_sf$Species %<>% droplevels()
      
      # to order legend symbols consistently
      # sp <- unique(c(levels(all_ap_sf$Species), levels(s$Species)))
      sp <- unique(c(levels(all_ap_sf$Species)))
      
      # to size symbols by count
      all_ap_sf <- all_ap_sf %>% mutate(Count =case_when(
        BestNumber == 1 ~ "1",
        BestNumber %in% c(2:5) ~ "2:5",
        BestNumber >5 ~ ">5"
      ) %>% factor(levels = c("1", "2:5", ">5")))
      
      g <- ggplot() +
        geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  labs(fill = "Depth (m)") +
        scale_fill_gradientn(colours = col_ramp(20)) +
        ggnewscale::new_scale("fill") +
        
        geom_sf(data = coast_file, stroke = 0.05, fill = "light yellow", colour = "grey 60")+
        
        geom_sf(data = all_ap_sf, stroke = 0.01, alpha = 0.5, aes(fill = Species, shape = Species, size = Count)) +#
        
        scale_size_manual(values = c(1.5,2,3)) +
        scale_fill_manual(values = cols, name = "Sightings", breaks = sp)   +
        scale_shape_manual(values = shape, name = "Sightings", breaks = sp) +
        
        guides(alpha= "none") +
        guides(shape = guide_legend(override.aes = list(size=2))) +
        # annotation_custom(leg1, xmin=-124.5, xmax=-124.95, ymin=47.9, ymax=48.1) +
        
        ggtitle(paste0("CeMoRe survey cetacean sightings to ", survey_title)) +
        coord +
        ylab("")+xlab("") + 
        theme(axis.text.x=element_text(angle=90)) +
        # facet_grid(Species ~ season)
        facet_wrap(~ season)
      #   
      # g <- cowplot::plot_grid(g, leg1, 
      #                         align = "v", axis = "r",
      #                         ncol = 1,
      #                         rel_heights = c(1, 0.05),
      #                         rel_widths = c(1, 0.05)
      # if saving plot, change coast to bc_shp for higher res basemap
      if(Save){
        # ggsave(paste0("output_maps/sightings_", survey_title, "_low_res_plus_hydrophones31_78.png"))
        ggsave(paste0("../output_maps/sightings_", survey_title, "_high_res.png"), height = 15, width = 15, units = "cm")
      }
    }else{
      #------------- EFFORT LINES ONLY ------------------------------------------
      
      g <- ggplot() +
        geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  labs(fill = "Depth (m)") +
        scale_fill_gradientn(colours = col_ramp(20), guide = "none") +
        ggnewscale::new_scale("fill") +
        ggnewscale::new_scale_colour() +
        geom_sf(data = coast_file, stroke = 0.05, fill = "light yellow", colour = "grey 60")+
        geom_sf(data = all_effort_lines, size = 0.25) +
        scale_colour_manual(values = c("dark orange", "green4", "yellow", "dark blue"), name = "Season") +
        ggtitle("CeMoRe survey completed transects", paste0("Sept 2020 to ", survey_title)) +
        # annotation_custom(leg1Grob, xmin=-124.99, xmax=-124.95, ymin=47.89, ymax=48.15) +
        coord +
        ylab("")+xlab("") +
        theme(axis.text.x=element_text(angle=90)) +
        facet_wrap(~ season)
      
      g <- cowplot::plot_grid(g, leg1, 
                              align = "v", axis = "l",
                              ncol = 1,
                              rel_heights = c(1, 0.1),
                              rel_widths = c(1, 0.1)
      )
      # if saving plot, change coast to bc_shp for higher res basemap
      if(Save){
        # ggsave(paste0("output_maps/sightings_", survey_title, "_low_res_plus_hydrophones31_78.png"))
        ggsave(paste0("../output_maps/effortlines_", survey_title, "_high_res.png"), height = 15, width = 15, units = "cm")
      }
    } 
  }
}




# Save = F
# 
# ggsave(paste0("../output_maps/sightings_to_", survey_title, "_low_res.png"))

# 
# 
# #--------------------------------------------------------------------------
# 
# 
# 
# # ggsave(paste0("output_maps/sightings_high_dens_porpoises_", survey_title, "_high_res.png"))
# 
# # -------------------------------------------------------------------------------
# # ---- substrate
# # -------------------------------------------------------------------------------
# # Load substrate raster and create bounding box for study area
# 
# sub <- raster("covariates/salish_sea_20m/salish_sea_20m.tif")#crs = 3005
# plot(sub)
# object.size(sub) # 14464 bytes
# 
# sub_clip <- raster("covariates/sub_arc_clip.tif")#crs = 3005
# plot(sub_clip)
# object.size(sub_clip) # 14464 bytes
# 
# 
# sub_clip_df <- as.data.frame(sub_clip, xy = T)%>%
#   na.omit() 
# # sub_sf <- st_as_sf(sub_df, coords=c(lon = sub_df$x, lat = sub_df$y))
# object.size(sub_clip_df)  # 3521621736 bytes
# 
# # bb <- create_bb(-125.5, -123., 48.1, 49.5) %>% st_transform(3005)
# # tss_6km_2 <- st_read(
# #   "C:/Users/keppele/Documents/CeMoRe/Design/cemore_design/shapefiles/study_area", 
# #   "TSS_buffer6km_2") %>% 
# #   st_transform(3005)
# 
# # plot(tss_6km_2)
# # plot(bb, add=T)
# # crop raster to study area
# # sub_cropped <- crop(sub, extent(tss_6km_2))
# # object.size(sub_cropped)/1000
# 
# # convert to data.frame for plotting
# sub_clip_df2 <- sub_clip %>%
#   as.bathy() %>%
#   fortify.bathy() %>%
#   na.omit()
# object.size(sub_clip_df2) # 3521621728 bytes
# 
# ggRasterly(data = sub_clip_df2, aes(x=x, y=y, color=z),
#            show_raster = T,
#            drop_data = T)
# 
# 
# sub_mask <- raster("covariates/sub_extract_mask.tif") %>%
#   na.omit() #crs = 3005
# plot(sub_mask)
# object.size(sub_mask) # 14480 bytes bytes
# 
# saveRDS(sub_cropped_df, "covariates/substrate_cropped_df.rds")
# sub_cropped_df <- readRDS("covariates/substrate_cropped_df.rds")
# 
# sub_mask_df <- sub_mask %>%
#   as.bathy() %>%
#   fortify.bathy() %>%
#   na.omit()
# object.size(sub_clip_df2) # 3,790,000 bytes
# 
# sub_mask_df2 <- as.data.frame(sub_mask, xy = T)%>%
#   na.omit() 
# #------------------------------------------------------------------------------
# # try converting arc-clipped substrate layer to spdf, then st_as_sf
# #------------------------------------------------------------------------------
# 
# 
# sub_clip_df <- as(sub_clip,'SpatialPolygonsDataFrame')
# object.size(sub_clip_df)
# 
# sub_clip_sf <- st_as_sf(sub_clip_df)
# #------------------------------------------------------------------------------
# 
# ggplot() + #geom_sf(data = st_transform(coast_file, crs = 4326), fill = "light yellow", colour = "grey 60") +
#   # geom_sf(data = bb) +
#   geom_raster(data = sub_df, aes(x = x, y = y, fill = sub_arc_clip)) +
#   coord 
# 
# area <- st_read("C:/Users/keppele/Documents/CeMoRe/Design/cemore_design/shapefiles/study_area", "cemore_study_area_TSS_6km_can")
# tss_6km_2 <- st_read(
#   "C:/Users/keppele/Documents/CeMoRe/Design/cemore_design/shapefiles/study_area", "TSS_buffer6km_2"
#   ) %>% 
#    st_transform(3005)
# 
# 
# 
# 
# 
# ggsave("test_w_sub.png")
# x <- ap_sf %>% filter(Species == "H. Porpoise group 40-50")
# ggplot() + geom_sf(data = coast_file, fill = "light yellow", colour = "grey 60") +
#   geom_sf(data = ap_sf, size = 1.5, aes(colour = Species, fill = Species, shape =  Species)) +
#   # geom_raster(data = bath_df, aes(x = x, y = y, fill = salish_sea_20m)) +
#   # geom_raster(data = sub_df, aes(x = x, y = y, fill = salish_sea_20m)) +
#   # coord 
# # ggsave("test_w_bath_and_sub.png")
# 
#   #, stroke =0.05
#   scale_shape_manual(values = shape, name = NULL) +
#   scale_fill_manual(values = cols, name = NULL)   +
#   scale_color_manual(values = cols,  name = NULL) +
#   coord +
#   ggtitle(paste0("All CeMoRe survey cetacean on effort sightings Sept 2020 - ", survey_title)) + #gfplot:::theme_pbs() +
#   theme(panel.background= element_rect(fill = "light blue")) +
#   theme(legend.position = "right") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) #+
#   facet_wrap( ~ Species)
# # # if saving plot, change coast to bc_shp for higher res basemap
# # if(save){
# #   ggsave(paste0("output_maps/all on effort sightings.png"))
# # }
# # save = F
# # #-------------------------------------------------------------------------------
# # #---- SIGHTING PLOTS BY SPECIES, ALL SURVEYS COMBINED, COLOURED BY SURVEY
# # #-------------------------------------------------------------------------------
# # ap_sf$SurveyID %<>% factor(c("cemore_2020sept",
# #                              "cemore_2020oct",
# #                              "cemore_2020nov",
# #                              "cemore_2021jan",
# #                              "cemore_2021feb",
# #                              "cemore_2021mar",
# #                              "cemore_2021apr",
# #                              "cemore_2021may"))
# # cols <- c('cemore_2020sept' = "brown",
# #           'cemore_2020oct'= "purple",
# #           'cemore_2020nov' = "blue",
# #           'cemore_2020dec' = "turquoise3",
# #           'cemore_2021jan' = "turquoise1",
# #           'cemore_2021feb' ="darkturquoise",
# #           'cemore_2021mar' = "limegreen",
# #           'cemore_2021apr' = "springgreen4",
# #           'cemore_2021may' = "yellow",
# #           'cemore_2021jun' = "orange",
# #           'cemore_2021jul' = "red",
# #           'cemore_2021aug' = "magenta")
# # save = F
# # coast_file <- if(save) bc_shp else coast
# # ggplot() + geom_sf(data = coast_file, fill = "light yellow", colour = "grey 60") +
# #   # geom_sf(data = canada_shp, colour = "black", fill = NA, linetype = "solid") +
# #   # geom_sf(data = effort_lines, size = 0.25, colour = "grey 50") +
# #   # geom_sf(data = effort_lines, size = 0.25, aes(colour = Date)) +
# # 
# #   # geom_sf(data = ap_sf, size = 1.5, aes(colour = Species, fill = Species, shape =  Species)) + #, stroke =0.05
# #   geom_sf(data = ap_sf, size = 1.5, aes(colour = season)) + #, stroke =0.05
# #   # scale_shape_manual(values = shape, name = NULL) + 
# #   # scale_fill_manual(values = cols, name = NULL)   +
# #   scale_color_manual(values = cols ,  name = NULL) +
# #   coord +
# #   ggtitle(paste0("CeMoRe cetacean sightings by survey")) + #gfplot:::theme_pbs() +
# #   # ggtitle(paste0("CeMoRe survey cetacean sightings - High density porpoise sightings ", survey_title)) +
# #   theme(panel.background= element_rect(fill = "light blue")) +
# #   theme(legend.position = "right") +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
# #   facet_wrap( ~ Species) 
# # # if saving plot, change coast to bc_shp for higher res basemap
# # if(save){
# #   ggsave(paste0("output_maps/sightings by month.png"))
# # }
# # save = F
# # 
# # #-------------------------------------------------------------------------------
# # #---- SIGHTING PLOTS BY SPECIES, ALL SURVEYS COMBINED, COLOURED BY MONTH
# # #-------------------------------------------------------------------------------
# # cols <- c('Sep' = "brown",
# #           'Oct'= "purple",
# #           'Nov' = "blue",
# #           'Dec' = "turquoise3",
# #           'Jan' = "turquoise1",
# #           'Feb' ="darkturquoise",
# #           'Mar' = "limegreen",
# #           'Apr' = "springgreen4",
# #           'May' = "yellow",
# #           'Jun' = "orange",
# #           'Jul' = "red",
# #           'Aug' = "magenta")
# # save = F
# # coast_file <- if(save) bc_shp else coast
# # ggplot() + geom_sf(data = coast_file, fill = "light yellow", colour = "grey 60") +
# #   # geom_sf(data = canada_shp, colour = "black", fill = NA, linetype = "solid") +
# #   # geom_sf(data = effort_lines, size = 0.25, colour = "grey 50") +
# #   # geom_sf(data = effort_lines, size = 0.25, aes(colour = Date)) +
# #   
# #   # geom_sf(data = ap_sf, size = 1.5, aes(colour = Species, fill = Species, shape =  Species)) + #, stroke =0.05
# #   geom_sf(data = ap_sf, size = 0.75, aes(colour = factor(month_abb))) + #, stroke =0.05
# #   # scale_shape_manual(values = shape, name = NULL) + 
# #   # scale_fill_manual(values = cols, name = NULL)   +
# #   scale_color_manual(values = cols,  name = NULL) +
# #   coord +
# #   ggtitle(paste0("CeMoRe survey cetacean sightings by month ", survey_title)) + #gfplot:::theme_pbs() +
# #   # ggtitle(paste0("CeMoRe survey cetacean sightings - High density porpoise sightings ", survey_title)) +
# #   theme(panel.background= element_rect(fill = "light blue")) +
# #   theme(legend.position = "right") +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
# #   facet_wrap( ~ Species) 
# # if(save){
# #   ggsave(paste0("output_maps/sightings by month.png"))
# # }
# # save = F
# # #-------------------------------------------------------------------------------
# # #---- SIGHTING PLOTS BY SPECIES, ALL SURVEYS COMBINED, COLOURED BY SEASON
# # #-------------------------------------------------------------------------------
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
# # save = F
# # 
# # #-------------------------------------------------------------------------------
# # #---- SINGLE SPECIES DISTRIBUTION, ALL SURVEYS COMBINED, BY MONTH 
# # #-------------------------------------------------------------------------------
# cols <- c('Humpback' = "blue3",
#           'Harbour Porpoise' = "dark green",
#           'Dalls Porpoise' = "firebrick3",
#           'Unknown Porpoise' = "darkorange1",
#           'KW - Northern Resident' = "green3",
#           'KW - Southern Resident' = "violetred1",
#           'KW - Transient' = "black",
#           'KW - Unknown ecotype' = "black",
#           'Sei Whale' = "grey 30",
#           'Sperm Whale' = pal[6],
#           'Cuvier\'s Whale' = pal[4],
#           'Fin Whale' = pal[10],
#           'Grey Whale' = "darkorchid4",
#           'H. Porpoise group 40-50' = "black")
# 
# shape <- c('Humpback' = 24,
#            'Harbour Porpoise' = 23,
#            'Dalls Porpoise' = 23,
#            'Unknown Porpoise' = 23,
#            'KW - Northern Resident' = 2,
#            'KW - Southern Resident' = 25,
#            'KW - Transient' = 25,
#            'KW - Unknown ecotype' = 4,
#            'Sei Whale' = 21,
#            'Sperm Whale' = 21,
#            "Cuvier's Whale" = 21,
#            "Fin Whale" = 21,
#            "Grey Whale" = 21,
#            "H. Porpoise group 40-50" = 8)
# Save = T
# coast_file <- if(Save) bc_shp else coast
# 
# x <- ap_sf %>% filter(Species == "Humpback")
# x <- ap_sf %>% filter(Species == "Harbour Porpoise")
# x <- ap_sf %>% filter(Species == "Dalls Porpoise")
# x <- ap_sf %>% filter(Species %in% c("KW - Southern Resident", "KW - Transient"))
# #-------------------------------------------------------------------------------
# # for just porpoises
# #-------------------------------------------------------------------------------
# x <- ap_sf %>% dplyr::filter(grepl("Porpoise", Species, fixed = TRUE))
# #-------------------------------------------------------------------------------
# 
# 
# ggplot() + geom_sf(data = coast_file, fill = "light yellow", colour = "grey 60", size = 0.25) +
#   geom_sf(data = all_effort_lines, size = 0.1, colour = "black") +
#   geom_sf(data = x, size = 1, aes(fill = Species, shape =  Species, colour = Species)) + #, stroke =0.05
#   scale_shape_manual(values = shape, name = NULL) +
#   scale_fill_manual(values = cols, name = NULL)   +
#   scale_color_manual(values = cols,  name = NULL) +
#   coord +
#   # ggtitle(paste0("All CeMoRe survey on effort Humpback sightings - plotted ", survey_title)) + #gfplot:::theme_pbs() +
#   ggtitle(paste0("All CeMoRe survey on effort Harbour Porpoise sightings - plotted ", survey_title)) +
#   # ggtitle(paste0("All CeMoRe survey on effort Dalls Porpoise sightings - plotted ", survey_title)) +
#   # ggtitle(paste0("All CeMoRe survey on effort Killer Whale sightings - plotted ", survey_title)) +
#   theme(panel.background= element_rect(fill = "light blue")) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   theme(legend.position = "NONE") +
#  facet_wrap( ~ month_year, drop = F)
# # if saving plot, change coast to bc_shp for higher res basemap
# 
# if(save){
#   ggsave(paste0("output_maps/HW by month up to June 2021.png"))
#   ggsave(paste0("output_maps/HP by month up to June 2021.png"))
#   ggsave(paste0("output_maps/DP by month up to June 2021.png"))
#   ggsave(paste0("output_maps/KW by month up to June 2021.png"))
#   
# 
#   }
# save = F
# 
# 
# # TRY JITTERING TO SEE ALL OF THE HP SIGHTINGS
# #display.brewer.all()
# 
# 
# 
# 
# 
# 
# 
# # ------------------------------------------------------------------------------
# # plot tag deployment locations
# # ------------------------------------------------------------------------------
# x <- read.csv("tag_data/2021-08 tag deployment locations.csv") %>% 
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
#     st_transform(crs = st_crs(bc_shp)) 
#   # st_as_sf(coords = c("lon", "lat"), crs = st_crs(bc_shp)) %>% 
#   # st_transform(crs = 4326)
# 
# Save <-  T
# if(!Save) coast_file <- coast else coast_file <- bc_shp
# 
# ggplot() + 
#   geom_sf(data = coast_file, fill = "light yellow", colour = "grey 60", size = 0.25) +
#   geom_sf(data = x, size = 1, colour = "black") +
#   coord +
#   # theme(panel.background= element_rect(fill = "light blue")) +
#   ggtitle("Humpback Whale Tagging Locations August 2021") 
# 
#   
#   if(Save){
#       ggsave(paste0("tagging_maps/tagging locations 2021 August.png"))
#   }
# 
# # ------------------------------------------------------------------------------
# # plot hydrophone deployment locations
# # ------------------------------------------------------------------------------
# x <- read.csv("hydrophone_data/hydrophone_deployment_locations.csv") %>% 
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
#   st_transform(crs = st_crs(bc_shp)) 
# # st_as_sf(coords = c("lon", "lat"), crs = st_crs(bc_shp)) %>% 
# # st_transform(crs = 4326)
# 
# Save <- F
# if(!Save) coast_file <- coast else coast_file <- bc_shp
# display.brewer.all()
# 
# bathy <- getNOAA.bathy(-125.5, -123,48.1, 49.5,res=1, keep=TRUE)
# bathy <- as.raster(bathy)
# plot(bathy, col = RColorBrewer::brewer.pal(20L, "Blues"))
# 
# ggplot() + geom_sf(data = coast_file, fill = "light yellow", colour = "grey 60", size = 0.25) +
#   # geom_sf(data = x, size = 1, colour = "black") +
#   coord +
#   theme(panel.background= element_rect(fill = "light blue")) +
#   ggtitle("Hydrophone Deployment Location August 21, 2021") +
#   geom_raster(data = bathy)
# 
# bathy %<>% as.matrix() 
#   bathy %>% as.data.frame() %>%
#   rownames_to_column(var = "lon") %>%
#   gather(lat, value, -1) %>%
#   mutate_all(funs(as.numeric)) %>%
#   ggplot()+
#   geom_contour(aes(x = lon, y = lat, z = value), bins = 10, colour = "black") +
#   coord_map()
# 
# 
# if(Save){
#   ggsave(paste0("hydrophone_data/deployment locations 2021 August.png"))
# }
