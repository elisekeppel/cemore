# after Eva's code
plot_survey_old <- function(Save = F,
                        border = F,
                        single_survey=T,
                        effort=NULL, # T/F
                        effort_lines = effort_lines,
                        effort_by_day = F,
                        season=F,
                        species=NULL,
                        incidentals=F,
                        hydrophone=F,
                        effort_only=F,
                        N=F,
                        km=F,
                        depth=F,
                        coord=NULL,
                        bathy=NULL){

  # ---------------------------------------------------------------------
  # --------------------------- SET UP DATA -----------------------------
  # ---------------------------------------------------------------------
  if(is.null(effort) & single_survey) effort <- T
  if(is.null(effort) & !single_survey) effort <- F

   if(!single_survey){
    ap_sf <- all_ap_sf
    effort_lines <- all_effort_lines
    years <- unique(ap_sf$year)
    months <- unique(ap_sf$month)
  }else{
    years <- year
    months <- month
  }

  ap_sf <- ap_sf %>% dplyr::select(year,month,Species,Group_Size,season)

  # ----------------------------------------------------------------------
  # ----------------- LOAD SPATIAL FILES --------------------------------
  # ----------------------------------------------------------------------
  # bc coast for plotting
  if(!Save){
    if(!exists("coast")){
      coast <- sf::st_read(dsn = "C:/Users/Keppele/Documents/GitHub/cemore/cemore/data", layer = "BC_coast_UTM9", quiet = T)
    }
    coast_file <- coast
  }
  # higher resolution bc coast --- takes a long time to load
  if(Save){
    if(!exists("bc_shp")){
      bc_shp <- sf::st_read(dsn = "C:/Users/Keppele/Documents/GitHub/cemore/cemore/data", "BC_AK_WA_union_polygon", quiet = T) %>%
        sf::st_transform(crs = 4326)
    }
    coast_file <- bc_shp
  }

  # Pacific Canada polygon for outlining Canadian border
  if(border){
    if(!exists("canada_shp")){
      canada_shp <- st_read(dsn = "C:/Users/Keppele/Documents/GitHub/cemore/cemore/data", "CanadianEEZ", quiet = T) %>%
        st_transform(crs = 4326)
    }
    border <- geom_sf(data = canada_shp, colour = "red", fill = NA, size = 0.1)
  }else{border <- NULL}

  if(is.null(coord)){
    coord <- ggplot2::coord_sf(xlim = c(-125.5, -122.9), ylim = c(48.1, 49.44), crs = sf::st_crs(4326))
  }

  #-----------------------------------------------------------
  # to create bathymetric layer and basemap
  #-----------------------------------------------------------
  if(is.null(bathy)){bathy <- getNOAA.bathy(-125.7, -122.5,48, 49.5,res=1, keep=TRUE) %>%
    fortify(bathy)
  bathy$z[which(bathy$z >= 0)] <- 0
  col <- rev(RColorBrewer::brewer.pal(9L, "Blues")[4:7])
  col_ramp <- colorRampPalette(col)}

  # make bathy legend
  if(depth){
    b_leg <- ggplot() +
      geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  labs(fill = "Depth (m)") +
      scale_fill_gradientn(colours = col_ramp(20)) +
      theme(legend.position = "bottom",
            axis.text=element_text(size=12),
            legend.text=element_text(size=12)
      ) +
      guides(fill = guide_colorbar(title.position = "left"))

    # leg1 <- gtable_filter(ggplot_gtable(ggplot_build(b_leg)), "guide-box")
    # leg1Grob <- grobTree(leg1)
    leg1 <- cowplot::get_legend(b_leg)
  }
  # ---------------------------------------------------------------------
  # -------------------- create object 'g' basemap ----------------------
  # ---------------------------------------------------------------------
  # create object 'g' basemap
  g <- ggplot() +
    geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  #labs(fill = "Depth (m)") +
    scale_fill_gradientn(colours = col_ramp(20), guide = "none") +
    ggnewscale::new_scale("fill") +
    geom_sf(data = coast_file, size = 0.1, fill = "light yellow", colour = "grey 60")
  # ----------------------------------------------------------------------

  # if require inset to magnify area of intense sightings
  # bb <- create_bb(-123.4, -123.1, 48.32, 48.5)
  # ap_sf_cropped <- st_intersection(ap_sf, bb)

  # Old way of viewing tidy but not processed sightings (would need to import raw data)
  # s <- data$sightings %>%
  #   dplyr::filter(!is.na(Sgt.Lat) | !is.na(Sgt.Lon), !Species == "Sea Otter") %>% #, incidential.sighting == F) %>%
  #   st_as_sf(coords = c("Sgt.Lon", "Sgt.Lat"), crs = 4326) %>%
  #   st_transform(crs= 3156)

  if(!effort_only){
    # ----------------------------------------------------------------------
    # incidental sightings are plotted at GPS position of ship
    # ----------------------------------------------------------------------
    if(incidentals){
      inc <- get_incid(single_survey = single_survey) %>%
        tidyr::separate(GPS.Pos, into = c("lat", "lon"), sep = "N") %>%
        dplyr::mutate(lon = substr(lon, 2, nchar(lon)-3)) %>%
        dplyr::mutate(lat = substr(lat, 1, nchar(lat)-2)) %>%
        tidyr::separate(lon, into = c("lon.deg", "lon.min"), sep = " ") %>%
        tidyr::separate(lat, into = c("lat.deg", "lat.min"), sep = " ") %>%
        mutate(month = month(date),
               lon = -(as.numeric(lon.deg) + as.numeric(lon.min)/60),
               lat = as.numeric(lat.deg) + as.numeric(lat.min)/60,
               Group_Size = Best.Cnt, Group_Size = Best.Cnt,
               season = factor(dplyr::case_when(
                 month %in% c(1:3) ~ "Winter",
                 month %in% c(4:6) ~ "Spring",
                 month %in% c(7:9) ~ "Summer",
                 month %in% c(10:12)  ~ "Fall"
               ), levels = c("Winter", "Spring", "Summer", "Fall"))) %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        dplyr::select("Species","Group_Size", "season",geometry)
      if(!nrow(inc)<0) ap_sf <- bind_rows(ap_sf,inc)
    }

    #-----------------------------------------------------------
    # to add in hydrophone deployment position(s)
    #-----------------------------------------------------------
    # (see hydrophone/hydrophone.R)
    if(hydrophone){
      # source("R/hydrophone.R")
      y <- read.csv("C:/users/keppele/documents/cemore/analysis/cemore_analysis/acoustic_data/hydrophone_deployments.csv")
      acoust <- y %>% dplyr::filter(deployment_year %in% as.numeric(years), deployment_month %in% as.numeric(months)) %>%
        dplyr::select(-c(lat_deg,lat_dec_min,lon_deg,lon_dec_min)) %>%
        st_as_sf(coords=c("lon","lat"),crs=4326)
    }

    #-----------------------------------------------------------
    # set colours, shapes, bathymetry legend, species factor levels
    #-----------------------------------------------------------
    # pal <- RColorBrewer::brewer.pal(12, "Paired")[c(2,4,3)]
    pal <- brewer.pal(9, "Set1")[c(1:9)] #red,blue,green,purple,orange,yellow,brown,pink,grey

    cols <-   c("Humpback whale" = pal[1], #red
                "Harbour porpoise" = pal[3], #green
                "Dall\'s porpoise" = pal[4], #purple
                "Unknown porpoise" = pal[6], #yellow
                "Killer whale - northern resident" = "black",
                "Killer whale - southern resident" = "black",
                "Killer whale - Bigg\'s" = "black",
                "Killer whale - unknown ecotype" = "black",
                "Fin whale" = pal[5], #orange
                "Grey whale" = pal[9], #grey
                "Minke whale" = pal[7]) #brown

    shape <- c("Humpback whale" = 21, #circle
               "Harbour porpoise" = 21,
               "Dall\'s porpoise" = 21,
               "Unknown porpoise" = 21,
               "Killer whale - northern resident" = 24, #triangle
               "Killer whale - southern resident" = 25, #upside down triangle
               "Killer whale - Bigg\'s" = 22,           #circle
               "Killer whale - unknown ecotype" = 21,   #square
               "Fin whale" = 21,
               "Grey whale" = 21,
               "Minke whale" = 21)

    #----------------------------------------------
    # prep species data
    #----------------------------------------------
    if(!is.null(species)) ap_sf %<>% dplyr::filter(Species %in% species)
    # to not display all potential species in legend
    ap_sf$Species %<>% droplevels()
    # Capitalize species for legend
    lev <- first_up(levels(ap_sf$Species))
    ap_sf$Species <- ap_sf$Species %>%
      as.character() %>%
      first_up() %>%
      factor(levels=lev)
    # to order legend symbols consistently
    sp <- unique(c(levels(ap_sf$Species)))

    # to size symbols by count
    ap_sf <- ap_sf %>% mutate(Count =case_when(
      is.na(Group_Size) ~ "1",
      Group_Size == 1 ~ "1",
      Group_Size %in% c(2:5) ~ "2:5",
      Group_Size >5 ~ ">5"
    ) %>% factor(levels = c("1", "2:5", ">5")))

    # ----------------------------------------------------------------------
    # ----------------- PLOT ON-EFFORT SIGHTINGS ---------------------------
    # ----------------------------------------------------------------------

    #-------------------------------------------------------
    if(effort){
      if(effort_by_day){
        col <- c("black",paste0(c(RColorBrewer::brewer.pal(12, "Paired"))))
      }else{
        col <- "black"
      }
        g <- g + geom_sf(data = effort_lines, size = 0.25, colour = col)
    }
    if(hydrophone){
      g <- g +
        geom_sf(data = acoust, aes(shape = deployed)) +
        scale_shape_manual(values = 8, name = NULL) +
        ggnewscale::new_scale("shape")
    }
    #-------------------------------------------------------------------
    g <- g + geom_sf(data = ap_sf, alpha = 0.8, colour="black",stroke=0.2,
                     aes(fill = Species, shape = Species, size = Count)) +#

      scale_size_manual(values = c(1.5,2,3),name="Group Size") +
      scale_fill_manual(values = cols, breaks = sp, name = NULL)   +
      scale_shape_manual(values = shape, breaks = sp, name = NULL) +

      guides(alpha= "none",
             shape = guide_legend(ncol=1,order = 1,override.aes = list(size=2),title.position = NULL),
             fill =  guide_legend(ncol=1,order = 1),
             size =  guide_legend(ncol=1,order = 2,title.position = "left")) +
      # annotation_custom(leg1, xmin=-119.1, xmax=-124.95, ymin=47.95, ymax=48.1) +

      theme(
        # plot.margin = unit(c(0,0,0,0), "cm"),
            axis.text = element_text(size=9),
            axis.text.x = element_text(angle=90),
            legend.key = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size=9),#size=fig_legend_size
            legend.margin = margin(0)#,
            # legend.key.size = unit(0.1,"cm")
      )

    if(season) g <- g + facet_wrap(~ season) #+ # guides(shape="none",fill="none",size="none")
    # if(season & !is.null(species)) g <- g + #ggtitle(first_up(paste(species))) #
      # + guides(size="none")

    g <- g +
      coord +
      ylab("")+xlab("")

    # TO ADD TITLE WHEN FILTERED BY SPECIES (NOT CURRENTLY NEEDED SINCE LEGEND)
    # if(!is.null(species)){
#   g <- g + ggtitle(toupper(species))
# }


    # guides(fill=guide_legend(ncol=1,title.position = "top"),shape=guide_legend(ncol=1,title.position = "top"),
    #        size=guide_legend(ncol=1,title.position = "top")) +
    #   theme(legend.position = "bottom",
    #         axis.text.x = element_text(angle=90),
    #         legend.key = element_blank(),
    #         axis.text = element_text(size=9),
    #         # plot.margin = unit(c(0,0,0,0), "cm"),
    #         legend.text = element_text(size=9),#size=fig_legend_size
    #         legend.margin = margin(0)#,
    #         # legend.key.size = unit(0.1,"cm")
    #   )


    if(Save){
      if(single_survey){
        file <- paste0("C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/output_maps/summary_map_",survey_title,".png")
      }else{
        file <- paste0("C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/output_maps/all_cemore_sightings_to_",survey_title,".png")}
      ggsave(file, height = 15, width = 15, units = "cm")
    }
  }else{
    #------------- EFFORT LINES ONLY ------------------------------------------
    g <- g +
      border +
      geom_sf(data = effort_lines, size = 0.25) +
      scale_colour_manual(values = c("dark orange", "green4", "yellow", "dark blue"), name = "Season", guide = "none")
    # ggtitle("CeMoRe survey completed transects", paste0("Sept 2020 to ", survey_title))
    # annotation_custom(leg1Grob, xmin=-124.99, xmax=-124.95, ymin=47.89, ymax=48.15) +

    if(N){
      tx <- effort_lines %>% data.frame() %>%
        group_by(month_abb) %>%
        dplyr::summarise(N = n_distinct(SurveyID))
      g <- g+geom_text(data = tx, aes(x = -124.4, y = 48.9, label = paste0(N, " surveys")), size = 3)}
    if(km){
      tx2 <- effort_lines %>% data.frame() %>%
        dplyr::group_by(month_abb) %>%
        dplyr::summarise(dist = round(sum(length_km),0))
      g <- g+geom_text(data = tx2, aes(x = -124.4, y = 48.8, label = paste0(dist, " km")), size = 3)}

    g <- g+ coord +
      ylab(NULL)+xlab(NULL) +
      theme(axis.text.x=element_text(angle=90),
            axis.text = element_text(size=12),
            plot.margin = unit(c(0,0,0,0), "cm"),
            legend.title = element_blank(),
            legend.text = element_text(),#size=fig_legend_size
            legend.position="none") +
      facet_wrap(~ month_abb, ncol=3)

    if(depth){
      g <- cowplot::plot_grid(g, leg1, ncol = 1, rel_heights = c(1, .00001))}

    # if saving plot, change coast to bc_shp for higher res basemap
    if(Save){
      # ggsave(paste0("output_maps/sightings_", survey_title, "_low_res_plus_hydrophones31_78.png"))
      ggsave(paste0("../output_maps/effortlines_", survey_title, "_high_res.png"), height = 15, width = 15, units = "cm")
    }
  }

  g
}


