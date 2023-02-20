# after Eva's code
plot_survey <- function(Save = F,
                        file_name = NULL,

                        bathy=NULL,
                        border = F,
                        single_survey=T,

                        badelf = NULL,

                        plot_effort = T, # T/F
                        effort_data = effort_lines,
                        effort_by_day = F,
                        effort_by_vis = F,
                        show_transit = T,

                        N = F,
                        km = F,
                        depth = F,

                        plot_sgt = T,
                        species = NULL,
                        incidentals = F,
                        incl = F, # include hw and porps in incidentals

                        hydrophone = F,

                        monthly = F,
                        season=  F,

                        leg.pos = "bottom",
                        coord = NULL
){

  # ---------------------------------------------------------------------
  # --------------------------- SET UP DATA -----------------------------
  # ---------------------------------------------------------------------
  # if(!exists(effort) & single_survey) effort <- T
  # if(!exists(effort) & !single_survey) effort <- F

  if(!single_survey){
    ap_sf <- all_ap_sf
    effort_data <- all_effort_lines
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
  #----------------------- BATHYMETRY ------------------------
  #-----------------------------------------------------------
  if(is.null(bathy)){
    bathy <- getNOAA.bathy(-125.7, -122.5,48, 49.5,res=1, keep=TRUE) %>%
      fortify(bathy)}
  bathy$z[which(bathy$z >= 0)] <- 0
  col <- rev(RColorBrewer::brewer.pal(9L, "Blues")[4:7])
  col_ramp <- colorRampPalette(col)

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
  # --------------------------- BASEMAP ---------------------------------
  # ---------------------------------------------------------------------
  g <- ggplot() +
    geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  #labs(fill = "Depth (m)") +
    scale_fill_gradientn(colours = col_ramp(20), guide = "none") +
    ggnewscale::new_scale("fill") +
    geom_sf(data = coast_file, size = 0.1, fill = "light yellow", colour = "grey 60") +
    border

  # if require inset to magnify area of intense sightings
  # bb <- create_bb(-123.4, -123.1, 48.32, 48.5)
  # ap_sf_cropped <- st_intersection(ap_sf, bb)

  # Old way of viewing tidy but not processed sightings (would need to import raw data)
  # s <- data$sightings %>%
  #   dplyr::filter(!is.na(Sgt.Lat) | !is.na(Sgt.Lon), !Species == "Sea Otter") %>% #, incidential.sighting == F) %>%
  #   st_as_sf(coords = c("Sgt.Lon", "Sgt.Lat"), crs = 4326) %>%
  #   st_transform(crs= 3156)


  # ---------------------------------------------------------------------
  # ---------------------------- RAW TRACKS -----------------------------
  # ---------------------------------------------------------------------
  if(!is.null(badelf)){
    g <- g + geom_sf(data = badelf, size = 0.25, aes(colour = "Trackline"))
    # guides(colour = guide_legend(ncol=1, order = 3,override.aes = list(size=1),title.position=NULL))


  }

  # ---------------------------------------------------------------------
  # ------------------------------- EFFORT ------------------------------
  # ---------------------------------------------------------------------

  if(plot_effort){
    effort_lines <- effort_lines %>%
      mutate(Effort = case_when(Effort == "On Effort" ~ "Survey effort",
                                Effort == "In Transit"~ "In transit"))

    if(effort_by_day){
      col <- c(paste0(c(RColorBrewer::brewer.pal(12, "Paired"))))
      g <- g + geom_sf(data = effort_data, size = 0.25, aes(colour = as.factor(date))) +
        scale_colour_manual(name="Date",values=col)+
        guides(colour = guide_legend(ncol=1,order = 3,override.aes = list(size=1),title.position="top")) + #
        ggnewscale::new_scale("colour")
    }
    if(show_transit){
      g <- g+ geom_sf(data = effort_lines, size = 0.5, aes(linetype = Effort)) +
        scale_linetype_manual(values=c("Survey effort"="solid", "In transit" = "dashed")) +
        ggnewscale::new_scale("linetype")
    }
    # to size lines by vis
    if(effort_by_vis){
      effort_data <- effort_data %>%
        mutate(vis_line = case_when(
          Visibility == "R" ~ 0.25,
          Visibility == "P" ~ 0.5,
          Visibility == "F" ~ 1,
          Visibility == "G&E" ~ 2),
          beauf_char = as.character(Beaufort))

      # effort_data$vis_line %<>% as.factor() %>% droplevels()
      # lab <- levels(effort_data$vis_line)

      pal <- brewer.pal(9, "Set1")[c(1:9)] #red,blue,green,purple,orange,yellow,brown,pink,grey
      bf <-   c("0" = pal[2], #blue
                "1" = pal[3], #green
                "2" = pal[6], #yellow
                "3" = pal[5], #orange
                "4" = pal[1], #red
                "5" = pal[4]) #purple


      g <- g + geom_sf(data = effort_data, aes(linewidth =vis_line)) +
        scale_linewidth(name="Visibility",range=c(0.25,1), labels = c("R","P","F","G&E"), guide = "legend") +
        ggnewscale::new_scale("linewidth") +
        geom_sf(data=effort_data, aes(colour=beauf_char)) +
        scale_colour_manual(name="Beaufort", values = bf) +
        ggnewscale::new_scale("colour")

      # g + geom_sf(data = effort_data, aes(linewidth =Visibility)) +
      #   # scale_size_manual(values=c(0.25,0.5,20), guide = "legend") +
      #   ggnewscale::new_scale("linewidth") +
      #   geom_sf(data=effort_data, aes(colour=beauf_char)) +
      #   scale_colour_manual(name="Beaufort", values = bf) +
      #   ggnewscale::new_scale("colour") + coord

      # guides(linewidth = guide_legend(ncol=1,order = 3,override.aes = list(size=1),title.position="top")) + #
    }

    if(!effort_by_day & !effort_by_vis & !show_transit){
      g <- g + geom_sf(data = effort_data, size = 0.25, aes(colour = "Survey effort"))# +
      # scale_colour_manual(values=c("Off effort" = "grey60", "On effort" = "black")) +
      # guides(colour = guide_legend(ncol=1,order = 3,override.aes = list(size=1),title.position=NULL))  #    }
    }
  }
  # ---------------------------------------------------------------------
  # ------------------------- TRACKS/EFFORT LEGEND ----------------------
  # ---------------------------------------------------------------------
  if(plot_effort ==T & is.null(badelf)){
    g <- g + scale_colour_manual(values=c("Survey effort" = "black"), name=NULL) +
      guides(colour = guide_legend(ncol=1,order = 3,override.aes = list(size=1),title.position=NULL))
  }
  if(plot_effort ==F & !is.null(badelf)){
    g <- g + scale_colour_manual(values=c("Trackline" = "grey50"), name=NULL) +
      guides(colour = guide_legend(ncol=1,order = 3,override.aes = list(size=1),title.position=NULL))
  }
  if(plot_effort ==T & !is.null(badelf)){
    g <- g + scale_colour_manual(values=c("Trackline" = "grey50", "Survey effort" = "black"), name=NULL) +
      guides(colour = guide_legend(ncol=1,order = 3,override.aes = list(size=1),title.position=NULL))
  }
  g <- g + ggnewscale::new_scale("colour")

  # ---------------------------------------------------------------------
  # ------------------------- TRACKS/EFFORT LABELS ----------------------
  # ---------------------------------------------------------------------
  if(N){
    tx <- effort_data %>% data.frame() %>%
      group_by(season) %>%
      dplyr::summarise(N = n_distinct(SurveyID))
    g <- g+geom_text(data = tx, aes(x = -124.4, y = 48.9, label = paste0(N, " surveys")), size = 3)}
  if(km){
    tx2 <- effort_data %>% data.frame() %>%
      mutate(length_km=st_length(geometry)) %>%
      dplyr::group_by(season) %>%
      # dplyr::summarise(dist = round(sum(length_km),0))

      # prettyNum(round(x,2),
      #           big.mark=",")
      dplyr::summarise(dist = prettyNum(round(sum(length_km),0),
                                        big.mark=","))

    g <- g+geom_text(data = tx2, aes(x = -124.4, y = 48.8, label = paste0(dist, " km")), size = 3)}

  # legacy from effort_only
  # g <- g+ coord +
  #   ylab(NULL)+xlab(NULL) +
  #   theme(axis.text.x=element_text(angle=90),
  #         axis.text = element_text(size=12),
  #         plot.margin = unit(c(0,0,0,0), "cm"),
  #         legend.title = element_blank(),
  #         legend.text = element_text(),#size=fig_legend_size
  #         legend.position="none") +
  #   facet_wrap(~ month_abb, ncol=3)

  if(depth){
    g <- cowplot::plot_grid(g, leg1, ncol = 1, rel_heights = c(1, .00001))}

  # ----------------------------------------------------------------------
  # ----------------------------- SIGHTINGS ------------------------------
  # ----------------------------------------------------------------------

  # ----------------------------------------------
  # prep sightings data
  # ----------------------------------------------
  # ----------------------------------------------------------------------
  # **incidental sightings are plotted at GPS position of ship
  # ----------------------------------------------------------------------
  if(plot_sgt){
    if(incidentals){
      inc <- get_incid(single_survey = single_survey, include_hw_porps = incl) %>%
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

    #-----------------------------------------------------------
    # set colours, shapes, species factor levels
    #-----------------------------------------------------------
    # pal <- RColorBrewer::brewer.pal(12, "Paired")[c(2,4,3)]
    pal <- brewer.pal(9, "Set1")[c(1:9)] #red,blue,green,purple,orange,yellow,brown,pink,grey

    cols <-   c("Pacific white-sided dolphin" = pal[2], #blue
                "Humpback whale" = pal[1], #red
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

    shape <- c("Pacific white-sided dolphin" = 23, #diamond
               "Humpback whale" = 21, #circle
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


    #-------------------------------------------------------------------
    g <- g + geom_sf(data = ap_sf, alpha = 0.8, colour="black",stroke=0.2,
                     aes(fill = Species, shape = Species, size = Count)) +#

      scale_size_manual(values = c(1.5,2.25,3),name="Group Size") +
      scale_fill_manual(values = cols, breaks = sp, name = "Species")   +
      scale_shape_manual(values = shape, breaks = sp, name = "Species") +

      guides(alpha= "none",
             shape = guide_legend(ncol=1,order = 1,override.aes = list(size=2),title.position = "top"),
             fill =  guide_legend(ncol=1,order = 1),
             size =  guide_legend(ncol=1,order = 2,title.position = "top"))
    # annotation_custom(leg1, xmin=-119.1, xmax=-124.95, ymin=47.95, ymax=48.1) +


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

    g <- g +
      geom_sf(data = acoust, aes(shape = deployed)) +
      scale_shape_manual(values = 8, name = NULL) +
      ggnewscale::new_scale("shape")
  }
  #-----------------------------------------------------------

  # ----------------------------------------------------------------------
  # ----------------------------- FINAL PLOT -----------------------------
  # ----------------------------------------------------------------------
  if(season) g <- g + facet_wrap(~ season) #+ # guides(shape="none",fill="none",size="none")
  # if(season & !is.null(species)) g <- g + #ggtitle(first_up(paste(species))) #
  # + guides(size="none")
  if(monthly) g <- g + facet_wrap(~ month) #+ # guides(shape="none",fill="none",size="none")

  g <- g +
    theme(
      # plot.margin = unit(c(0,0,0,0), "cm"),
      axis.text = element_text(size=9),
      axis.text.x = element_text(angle=90),
      legend.key = element_blank(),
      legend.position = leg.pos,
      legend.text = element_text(size=9),#size=fig_legend_size
      legend.margin = margin(0)#,
      # legend.key.size = unit(0.1,"cm")
    ) +
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

  # ----------------------------------------------------------------------
  # ----------------------------- SAVING ---------------------------------
  # ----------------------------------------------------------------------
  if(Save){
    if(single_survey){
      if(is.null(file_name)) file_name <- paste0("C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/output_maps/summary_map_",survey_title,".png")
    }else{
      if(is.null(file_name)) file_name <- paste0("C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/output_maps/all_cemore_sightings_to_",survey_title,".png")
    }
    ggsave(file_name, height = 15, width = 15, units = "cm")
  }

  g
}

