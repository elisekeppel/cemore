# after Eva's code
plot_survey <- function(Save = F,
                        border = F,
                        single_survey=T,
                        incidentals=F,
                        hydrophone=F,
                        sightings_only=F,
                        N=F,
                        km=F,
                        depth=F){


  # ----------------------------------------------------------------------
  # ----------------- LOAD SPATIAL FILES --------------------------------
  # ----------------------------------------------------------------------
  # bc coast for plotting
  if(!Save & !exists("coast")){
    coast <- sf::st_read(dsn = "C:/Users/Keppele/Documents/GitHub/cemore/cemore/data", layer = "BC_coast_UTM9", quiet = T)
    coast_file <- coast
  }
  # higher resolution bc coast --- takes a long time to load
  if(Save & !exists("bc_shp")){
    bc_shp <- sf::st_read(dsn = "C:/Users/Keppele/Documents/GitHub/cemore/cemore/data", "BC_AK_WA_union_polygon", quiet = T) %>%
      sf::st_transform(crs = 4326)
    coast_file <- bc_shp
  }
  # Pacific Canada polygon for outlining Canadian border
  if(border & !exists("canada_shp")){
  canada_shp <- st_read(dsn = "C:/Users/Keppele/Documents/GitHub/cemore/cemore/data", "CanadianEEZ", quiet = T) %>%
    st_transform(crs = 4326)
  }
  if(border){
    border <- geom_sf(data = canada_shp, colour = "red", fill = NA, size = 0.1)
  }else{border <- NULL}
  #-----------------------------------------------------------
  # to create bathymetric layer
  #-----------------------------------------------------------
  {bathy <- getNOAA.bathy(-125.7, -122.5,48, 49.5,res=1, keep=TRUE) %>%
    fortify(bathy)
  bathy$z[which(bathy$z >= 0)] <- 0
  col <- rev(RColorBrewer::brewer.pal(9L, "Blues")[4:7])
  col_ramp <- colorRampPalette(col)}

  # ----------------------------------------------------------------------

  # if require inset to magnify area of intense sightings
  # bb <- create_bb(-123.4, -123.1, 48.32, 48.5)
  # ap_sf_cropped <- st_intersection(ap_sf, bb)

  # Old way of viewing tidy but not processed sightings (would need to import raw data)
  # s <- data$sightings %>%
  #   dplyr::filter(!is.na(Sgt.Lat) | !is.na(Sgt.Lon), !Species == "Sea Otter") %>% #, incidential.sighting == F) %>%
  #   st_as_sf(coords = c("Sgt.Lon", "Sgt.Lat"), crs = 4326) %>%
  #   st_transform(crs= 3156)


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
  coord <- ggplot2::coord_sf(xlim = c(-125.5, -122.9), ylim = c(48.1, 49.44), crs = sf::st_crs(4326))

  # pal <- RColorBrewer::brewer.pal(12, "Paired")[c(2,4,3)]
  pal <- brewer.pal(9, "Set1")[c(1:9)] #red,blue,green,purple,orange,yellow,brown,pink,grey


  cols <- c('Humpback Whale' = pal[1],
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

  shape <- c('Humpback Whale' = 21,
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

    # create object 'g' basemap with effort
    g <- ggplot() +
      geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  labs(fill = "Depth (m)") +
      scale_fill_gradientn(colours = col_ramp(20), guide = "none") +
      ggnewscale::new_scale("fill") +
      geom_sf(data = coast_file, stroke = 0.05, fill = "light yellow", colour = "grey 60")+
      geom_sf(data = effort_lines, size = 0.25, colour = "black")


    if(hydrophone){
      g <- g +
        geom_sf(data = acoust, aes(shape = deployed)) +
        scale_shape_manual(values = 8, name = NULL) +
        ggnewscale::new_scale("shape")
    }

    g <- g + geom_sf(data = ap_sf, stroke = 0, alpha = 0.8, aes(fill = Species, shape = Species, size = Count)) +#

      scale_size_manual(values = c(1.5,2,3)) +
      scale_fill_manual(values = cols, name = "Sightings", breaks = sp)   +
      scale_shape_manual(values = shape, name = "Sightings", breaks = sp) +

      guides(alpha= "none") +
      guides(shape = guide_legend(override.aes = list(size=2), order = 1)) +
      guides(fill = guide_legend(order = 1)) +
      guides(size = guide_legend(order = 2)) +
      annotation_custom(leg1, xmin=-119.1, xmax=-124.95, ymin=47.95, ymax=48.1) +

      coord +
      ylab("")+xlab("")
    if(Save){
      # ggsave(paste0("output_maps/sightings_", survey_title, "_low_res_plus_hydrophones31_78.png"))
      ggsave(paste0("../output_maps/summary_map_", survey_title, "_high_res.png"), height = 15, width = 15, units = "cm")
    }
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
      # if saving plot, change coast to bc_shp for higher res basemap
      if(Save){
        # ggsave(paste0("output_maps/sightings_", survey_title, "_low_res_plus_hydrophones31_78.png"))
        ggsave(paste0("../output_maps/sightings_", survey_title, "_high_res.png"), height = 15, width = 15, units = "cm")
      }
    }else{
      #------------- EFFORT LINES ONLY ------------------------------------------
      g <- ggplot() +
        geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  #labs(fill = "Depth (m)") +
        scale_fill_gradientn(colours = col_ramp(20), guide = "none") +
        ggnewscale::new_scale("fill") +
        # ggnewscale::new_scale_colour() +
        geom_sf(data = coast_file, stroke = 0.05, fill = "light yellow", colour = "grey 60")+
        border +
        geom_sf(data = all_effort_lines, size = 0.25) +
        scale_colour_manual(values = c("dark orange", "green4", "yellow", "dark blue"), name = "Season", guide = "none")
        # ggtitle("CeMoRe survey completed transects", paste0("Sept 2020 to ", survey_title))
        # annotation_custom(leg1Grob, xmin=-124.99, xmax=-124.95, ymin=47.89, ymax=48.15) +

        if(N){
          tx <- all_effort_lines %>% data.frame() %>%
            group_by(season) %>%
            dplyr::summarise(N = n_distinct(SurveyID))
          g <- g+geom_text(data = tx, aes(x = -124.3, y = 49, label = paste0(N, " surveys")), size = 3)}
      if(km){
        tx2 <- all_effort_lines %>% data.frame() %>%
          dplyr::group_by(season) %>%
          dplyr::summarise(dist = round(sum(length_km),0))
        g <- g+geom_text(data = tx2, aes(x = -124.3, y = 48.9, label = paste0(dist, " km")), size = 3)}

         g <- g+ coord +
        ylab(NULL)+xlab(NULL) +
        theme(axis.text.x=element_text(angle=90),
              plot.margin = unit(c(0,0,0,0), "cm"),
              legend.title = element_blank(),
              legend.position="none") +
        facet_wrap(~ season)

         if(depth){
      g <- cowplot::plot_grid(g, leg1, ncol = 1, rel_heights = c(1, .00001))}

      # if saving plot, change coast to bc_shp for higher res basemap
      if(Save){
        # ggsave(paste0("output_maps/sightings_", survey_title, "_low_res_plus_hydrophones31_78.png"))
        ggsave(paste0("../output_maps/effortlines_", survey_title, "_high_res.png"), height = 15, width = 15, units = "cm")
      }
    }
  }
  g
}


