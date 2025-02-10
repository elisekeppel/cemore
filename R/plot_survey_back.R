# after Eva's code
plot_survey_back <- function(sgt_data = NULL,
                        data.source="cemore",
                        bath = T,
                        set_lat_scale=NULL, # ie. seq(-132,-123,4)
                        set_lon_scale=NULL,
                        survey_area=F,
                        survey_area_colour="grey93",
                        background="white",
                        high_res = F,
                        save = F,
                        file_name = NULL,
                        xmin=NULL,
                        xmax=NULL,
                        ymin=NULL,
                        ymax=NULL,
                        border = F,
                        single_survey=T,
                        badelf = NULL,
                        plot_effort = T, # T/F
                        effort_colour = "black",
                        effort_data = NULL,
                        effort_by_day = F,
                        effort_by_bf = F,
                        effort_by_vis = F,
                        colour_month = F,
                        colour_year = F,
                        # show_transit = F,
                        N = F,
                        km = F,
                        depth = F,
                        legend = T,
                        sp_leg=F,
                        plot_sgt = T,
                        set.alpha=0.9,
                        species = NULL,
                        spec_order=F,
                        incidentals = F,
                        incl_porps = F, # include hw and porps in incidentals
                        plot_grp_sz=NULL,# c(1.4, 2.4, 3)

                        specify_pt_size=NULL,
                        sgt_colours=NULL,
                        set_shape=NULL,
                        hydrophone = F,
                        facet_month = F,
                        facet_year = F,
                        facet_season=  F,
                        facet_seasonYear=F,
                        facet_yearSeason=F,
                        cols,
                        text_size,
                        label_size,
                        axis_angle=0,
                        rare_spp=F,
                        leg.pos = "bottom",
                        leg.box.spacing=NULL,
                        legend_justification=NULL,
                        leg_key_spacing_x = unit(0.25, "cm"),
                        leg_key_spacing_y = unit(0.25, "cm"),

                        legend_placement =NULL,
                        legend_spacing=NULL,
                        legend_margin=NULL,
                        leg.title.pos="top",
                        leg_dir = NULL,
                        grid_label = TRUE,
                        coord = NULL,
                        print = T,
                        title=NULL
){
  # ---------------------------------------------------------------------
  # --------------------------- SET UP DATA -----------------------------
  # ---------------------------------------------------------------------
  if(single_survey){
    if(is.null(sgt_data)) {sgt_data <- ap_sf}
    if(is.null(effort_data)) {effort_data <- effort_lines}
    years <- unique(effort_data$year)
    months <- unique(effort_data$month)
    survey_title <- paste(month.abb[month], year)
  }else{
    if(is.null(sgt_data)) {sgt_data <- all_ap_sf}
    if(is.null(effort_data)) {effort_data <- all_effort_lines}
    years <- year
    months <- month
    survey_title <- paste("All surveys to ", month.abb[month], year)
  }
  ap_sf <- sgt_data %>% dplyr::select(year,month,month_abb, Species,Group_Size,season,seasonYear)
  if(leg.pos=="bottom") {
    leg_dir <- "horizontal"
    leg_box <- "horizontal"

  }else{
    leg_dir <- "vertical"
    leg_box <- "vertical"
  }

  if(is.null(legend_margin)) legend_margin <- 0

  # ----------------------------------------------------------------------
  # ----------------- LOAD SPATIAL FILES --------------------------------
  # ----------------------------------------------------------------------
  # bc coast for plotting
  if(!high_res){
    if(!exists("coast")){
      coast <- sf::st_read(dsn = "C:/Users/Keppele/Documents/GitHub/cemore/cemore/data", layer = "BC_coast_UTM9", quiet = T) %>%
        sf::st_transform(crs = 4326)
    }
    coast_file <- coast
  }
  # higher resolution bc coast --- takes a long time to load
  if(high_res){
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
    # border <- geom_sf(data = canada_shp, colour = "red", fill = NA, size = 0.1)
    border <- geom_sf(data = canada_shp, colour = "black", fill = NA, linewidth = 0.05, lty = 1, alpha=0.1)
  }else{border <- NULL}

  if(is.null(coord)){
    coord <- ggplot2::coord_sf(xlim = c(-125.5, -122.9), ylim = c(48.1, 49.44), crs = sf::st_crs(4326))
  }

  if(survey_area & !exists("survey_can")){
    survey_area <- sf::st_read(dsn = "data", layer = "Full_study_area_UTM9N") %>% st_union() %>%  st_transform(crs = 4326)
    survey_can <- st_intersection(survey_area,canada_shp)
    # canada_shp %<>% st_cast("MULTILINESTRING")
  }

  #-----------------------------------------------------------
  #----------------------- BATHYMETRY ------------------------
  #-----------------------------------------------------------
  if(bath){
    if(data.source=="cemore"){
      bathy <- getNOAA.bathy(-125.7, -122.5,48, 49.5,res=1, keep=TRUE, path = "C:/Users/KeppelE/Documents/CeMoRe/Analysis/cemore_analysis/bath") %>% fortify(bathy)
    }else{
      bathy <- getNOAA.bathy(xmin, xmax, ymin, ymax, res=1, keep=TRUE, path= "C:/Users/KeppelE/Documents/CeMoRe/Analysis/cemore_analysis/bath") %>% fortify(bathy)
    }
    bathy$z[which(bathy$z >= 0)] <- 0
    col <- rev(RColorBrewer::brewer.pal(9L, "Blues")[4:7])
    col_ramp <- colorRampPalette(col)

    # make bathy legend
    if(depth){
      b_leg <- ggplot() +
        geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  labs(fill = "Depth (m)") +
        scale_fill_gradientn(colours = col_ramp(20)) +
        theme(legend.position = "bottom",
              axis.text=element_text(size=text_size),
              legend.text=element_text(size=text_size)
        ) +
        guides(fill = guide_colorbar(title.position = "left"))

      # leg1 <- gtable_filter(ggplot_gtable(ggplot_build(b_leg)), "guide-box")
      # leg1Grob <- grobTree(leg1)
      leg1 <- cowplot::get_legend(b_leg)
    }
  }
  # ---------------------------------------------------------------------
  # --------------------------- BASEMAP ---------------------------------
  # ---------------------------------------------------------------------
  {g <- ggplot()
  if(bath){
    g <- g +
      geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  #labs(fill = "Depth (m)") +
      scale_fill_gradientn(colours = col_ramp(20), guide = "none") +
      ggnewscale::new_scale("fill")
  }else{
    # col <- RColorBrewer::brewer.pal(9L, "Blues")[2]
    # bl <- col
    g <- g +  theme(    plot.background = element_rect(fill=background),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank())
  }
  if(survey_area) g <- g + geom_sf(data=survey_can, fill=survey_area_colour, colour="grey80")
  # if(survey_area) g <- g + geom_sf(data=survey_can, colour="grey80")

  g <- g +
    # geom_sf(data = coast_file, size = 0.1, fill = "light yellow", colour = "grey 60") +
    geom_sf(data = coast_file, linewidth = 0.01, fill = "grey 70", colour = "grey 40") +
    border
  }
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
    # g <- g + geom_sf(data = badelf, size = 0.25, aes(colour = "Trackline"))
    g <- g + geom_sf(data = badelf, linewidth = 0.25, aes(colour = "Trackline"))
    # guides(colour = guide_legend(ncol=1, order = 3,override.aes = list(size=1),title.position=NULL))


  }

  # ---------------------------------------------------------------------
  # ------------------------------- EFFORT ------------------------------
  # ---------------------------------------------------------------------

  if(plot_effort){
    # effort_data <- effort_data %>%
    #   mutate(Effort = case_when(status == "On Effort" ~ "Survey effort",
    #                             status == "In Transit"~ "In transit"))
    # if(plot_effort & plot_sgt){
    #   effort_col <- "grey80"
    # }else{
    #     effort_col <- "black"
    #   }

    if(effort_by_day){
      col <- c(paste0(c(RColorBrewer::brewer.pal(12, "Paired"))))
      g <- g +
        # geom_sf(data = effort_data, size = 0.25, aes(colour = as.factor(date))) +
        geom_sf(data = effort_data, linewidth = 0.25, aes(colour = as.factor(date))) +
        scale_colour_manual(name="Date",values=col)+
        guides(colour = guide_legend(ncol=1,order = 3,override.aes = list(size=1),title.position=leg.title.pos)) + #
        ggnewscale::new_scale("colour")
    }
    # if(show_transit){
    #   g <- g+ geom_sf(data = effort_data, size = 0.5, aes(linetype = status)) +
    #     scale_linetype_manual(values=c("Survey effort"="solid", "In transit" = "dashed")) +
    #     ggnewscale::new_scale("linetype")
    # }
    # to size lines by vis
    if(effort_by_vis | effort_by_bf){
      effort_data <- effort_data %>%
        mutate(
          beauf_char = as.character(Beaufort))

      pal <- brewer.pal(9, "Set1")[c(1:9)] #red,blue,green,purple,orange,yellow,brown,pink,grey
      bf <-   c("0" = pal[2], #blue
                "1" = pal[3], #green
                "2" = pal[6], #yellow
                "3" = pal[5], #orange
                "4" = pal[1], #red
                "5" = pal[4]) #purple
      if(effort_by_vis){

        g <- g + geom_sf(data = effort_data, aes(linewidth =Visibility,colour=beauf_char)) +
          scale_linewidth_manual(name="Visibility",values=c(1.75, 1.3, 0.5), labels = c("G&E","Moderate","P"), guide = "legend") +
          ggnewscale::new_scale("linewidth") +
          scale_colour_manual(name="Beaufort", values = bf, guide="legend") +
          guides(colour = guide_legend(override.aes = list(linewidth=1),title.position=NULL, direction=leg_dir)) +
          guides(linewidth = guide_legend(title.position=NULL, direction=leg_dir)) +
          ggnewscale::new_scale("colour")
      }
      if(effort_by_bf){
        g <- g + geom_sf(data = effort_data, aes(colour=beauf_char)) +
          scale_colour_manual(name="Beaufort", values = bf) +
          guides(colour = guide_legend(override.aes = list(linewidth=1),title.position=NULL, direction=leg_dir)) +
          ggnewscale::new_scale("colour")
      }
    }

    if(!effort_by_day & !effort_by_vis & !effort_by_bf){ #  & !show_transit

      if(colour_month){
        # mpal <- c(brewer.pal(9, "Purples")[c(3,9)],  # light purple, dark purple
        #           brewer.pal(4, "Dark2")[4], # pink purple
        #           brewer.pal(9, "YlGn")[c(3,9)],  # light green, dark green
        #           brewer.pal(5, "Dark2")[5],  # bright green
        #           brewer.pal(9, "Reds")[c(3,6,9)],  # yellow, orange, red, pink
        #           brewer.pal(9, "Oranges")[c(3,6,9)])[c(1,3,2,4,6,5,7,8,9,10,11,12)] # light orange, brown
        mpal <- c(
          brewer.pal(8, "Set1"),#[c(5,7,9)],       #
          brewer.pal(6, "GnBu")[6],  # light blue
          brewer.pal(5, "Dark2")[c(4,5)],  # dark pink, light green
          brewer.pal(5, "RdPu")[7],  # purple
          brewer.pal(9, "YlOrRd")[9])#,       # burgundy
        # brewer.pal(7, "Set3")[7],  # bright green
        # brewer.pal(9, "RdPu")[c(3,6,9)])  # pink, magenta, purple
        # brewer.pal(9,"Oranges")[c(4,6,9)]) # light orange, orange, brown
        month_col <- c("Jan" = mpal[1], #red
                       "Feb" = mpal[11], # green
                       "Mar" = "grey80", #
                       "Apr" = mpal[2],
                       "May" = mpal[5],
                       "Jun" = mpal[6],
                       "Jul" = mpal[12],
                       "Aug" = mpal[11],
                       "Sep" = mpal[10],
                       "Oct" = mpal[7],
                       "Nov" = mpal[8],
                       "Dec" = mpal[9])
        # if(facet_seasonYear) month_col <- rep(c("green4", "#00008B", "#FF4040"), 4)
        if(facet_seasonYear) month_col <- cols

        g <- g +
          geom_sf(data = effort_data, linewidth = 0.5, aes(colour = month_abb)) +
          scale_colour_manual(values=month_col, name=NULL) + # c("Winter", "Spring", "Summer", "Fall")
          guides(colour = guide_legend(ncol=2, order = 3,override.aes = list(linewidth=1.25),title.position=leg.title.pos, direction="horizontal")) + #coord
          ggnewscale::new_scale("colour")

      }else{
        if(colour_year){

          ypal <- c(brewer.pal(8, "Dark2"))

          year_col <-  c("2020" = ypal[1],
                         "2021" = ypal[2],
                         "2022" = ypal[3],
                         "2023" = ypal[4],
                         "2024" = ypal[5],
                         "2025" = ypal[6],
                         "2026" = ypal[7],
                         "2027" = ypal[8])

          g <- g +
            geom_sf(data = effort_data, linewidth = 0.25, aes(colour = as.character(year))) +
            scale_colour_manual(values=year_col, name="Survey effort") +
            guides(colour = guide_legend(ncol=1,order = 3,override.aes = list(linewidth=1),title.position=NULL,direction="horizontal")) + #coord
            ggnewscale::new_scale("colour")

        }else{
          g <- g +
            geom_sf(data = effort_data, linewidth = 0.1, colour = effort_colour, aes(colour = "Survey effort")) +
            # geom_sf(data = effort_data, size = 0.25, aes(colour = "Survey effort"))# +
            # scale_colour_manual(values=c("Off effort" = "grey60", "On effort" = "black")) +
            guides(colour = guide_legend(direction="horizontal"))  #    }
        }
      }
    }
  }

  # ---------------------------------------------------------------------
  # ------------------------- TRACKS/EFFORT LEGEND ----------------------
  # ---------------------------------------------------------------------
  if(plot_effort ==T & is.null(badelf)){
    g <- g + scale_colour_manual(values=c("Survey effort" = effort_colour), name=NULL) +
      guides(colour = guide_legend(ncol=2,order = 3,override.aes = list(size=1),title.position=NULL))
  }
  if(plot_effort ==F & !is.null(badelf)){
    g <- g + scale_colour_manual(values=c("Trackline" = effort_colour), name=NULL) +
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
    if(facet_season & !facet_seasonYear){
      tx <- effort_data %>% data.frame() %>%
        group_by(season)}
    if(facet_month){
      tx <- effort_data %>% data.frame() %>%
        group_by(month_abb)}
    if(facet_seasonYear){
      tx <- effort_data %>% data.frame() %>%
        group_by(season, year)}
    if(single_survey)  tx <- effort_data %>% data.frame() %>% group_by(SurveyID)

    tx %<>% dplyr::summarise(N = n_distinct(SurveyID))
    g <- g+geom_text(data = tx, aes(x = -124.3, y = 48.9, label = paste0(N, ifelse(N==1, " survey", " surveys"))), size = text_size)
  }

  if(km){
    if(facet_season & !facet_seasonYear){
      tx2 <- effort_data %>% data.frame() %>%
        dplyr::group_by(season)}
    if(facet_month){
      tx2 <- effort_data %>% data.frame() %>%
        dplyr::group_by(month_abb)}
    if(facet_seasonYear){
      tx2 <- effort_data %>% data.frame() %>%
        dplyr::group_by(season, year)}
    if(single_survey)  tx2 <- effort_data %>% data.frame() %>% group_by(SurveyID)

    tx2 %<>% dplyr::summarise(dist = prettyNum(round(sum(length_km),0),
                                               big.mark=","))
    g <- g+geom_text(data = tx2, aes(x = -124.35, y = 48.75, label = paste0(dist, " km")), size = text_size)}

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
      inc <- get_incid(single_survey = single_survey, include_hw_porps = incl_porps, Year=year, Month=month) %>%
        tidyr::separate(GPS.Pos, into = c("lat", "lon"), sep = "N") %>%
        dplyr::mutate(lon = substr(lon, 2, nchar(lon)-3)) %>%
        dplyr::mutate(lat = substr(lat, 1, nchar(lat)-2)) %>%
        tidyr::separate(lon, into = c("lon.deg", "lon.min"), sep = " ") %>%
        tidyr::separate(lat, into = c("lat.deg", "lat.min"), sep = " ") %>%
        mutate(month = month(date),
               lon = -(as.numeric(lon.deg) + as.numeric(lon.min)/60),
               lat = as.numeric(lat.deg) + as.numeric(lat.min)/60,
               Group_Size = Best.Cnt,
               season = factor(dplyr::case_when(
                 month %in% c(1:3) ~ "Winter",
                 month %in% c(4:6) ~ "Spring",
                 month %in% c(7:9) ~ "Summer",
                 month %in% c(10:12)  ~ "Fall"
               ), levels = c("Winter", "Spring", "Summer", "Fall"))
        ) %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        dplyr::select("Species","Group_Size", "season",geometry)
      if(!nrow(inc)<0) ap_sf <- bind_rows(ap_sf,inc)
    }

    if(!is.null(species) & length(species) == 1) {
      ap_sf %<>% dplyr::filter(Species %like% species)
      # species <- tolower(unique(ap_sf$Species))
    }

    if(!is.null(species) & length(species) > 1) ap_sf %<>% dplyr::filter(Species %in% species)

    ap_sf %<>% mutate(Species = factor(gsub(pattern="killer whale",replacement="KW",.$Species)))
    species <- gsub("killer whale", "KW", species)


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
    if(!is.null(plot_grp_sz)){
      ap_sf <- ap_sf %>% mutate(Count =case_when(
        is.na(Group_Size) ~ "1",
        Group_Size == 1 ~ "1",
        Group_Size %in% c(2:5) ~ "2:5",
        Group_Size >5 ~ ">5"
      ) %>% factor(levels = c("1", "2:5", ">5")))
    }else{
      ap_sf <- ap_sf %>% mutate(Count = as.factor("1"))
    }
    if(!is.null(specify_pt_size)) ap_sf$Count <- as.factor(specify_pt_size)
    #-----------------------------------------------------------
    # set colours, shapes, species factor levels
    #-----------------------------------------------------------
    # pal <- RColorBrewer::brewer.pal(12, "Paired")[c(2,4,3)]
    pal <- RColorBrewer::brewer.pal(9, "Set1")[c(1:9)] #red,blue,green,purple,orange,yellow,brown,pink,grey
    # hw <-  RColorBrewer::brewer.pal(8, "Reds")[8]
    # hp <-  RColorBrewer::brewer.pal(8, "Greens")[8]
    # dp <-  RColorBrewer::brewer.pal(8, "Purples")[8]

    if(!is.null(sgt_colours)) {
      cols <- sgt_colours
    }else{
      cols <-   c("Pacific white-sided dolphin" = pal[8], #pink
                  # "Humpback whale" = hw, #red
                  "Humpback whale" = pal[1], #red
                  # "Harbour porpoise" = hp, #green
                  "Harbour porpoise" = pal[3], #green
                  # "Dall\'s porpoise" = dp, #purple
                  "Dall\'s porpoise" = pal[4], #purple
                  "Unknown porpoise" = pal[7], #yellow
                  "KW - northern resident" = "black",
                  "KW - southern resident" = "black",
                  "KW - Bigg\'s" = "black",
                  "KW - unknown ecotype" = "black",
                  "Fin whale" = pal[5], #orange
                  "Fin whale - off effort" = "red",
                  "Grey whale - off effort" = "blue", #grey
                  "Grey whale" = "blue", #grey
                  "Minke whale - off effort" = pal[6], #brown
                  "Minke whale" = pal[6]) #brown
    }

    if(!is.null(species)){
      shape <- c("Pacific white-sided dolphin" = 23, #diamond
                 "Humpback whale" = 21, #circle
                 "Harbour porpoise" = 21,
                 "Dall\'s porpoise" = 21,
                 "Unknown porpoise" = 21,
                 # "KW - northern resident" =21,#= 24, #triangle
                 # "KW - southern resident"=21,# = 25, #upside down triangle
                 # "KW - Bigg\'s" = 21,#,           #circle
                 # "KW - unknown ecotype"=21,# = 21,   #square
                 "Fin whale" = 23,
                 "Fin whale - off effort" = 23,
                 "Grey whale - off effort" = 23,
                 "Grey whale" = 23,
                 "Minke whale - off effort" = 23,
                 "Minke whale" = 23,
                 "KW - all ecotypes" = 21)
    }

    if(!is.null(species) & (any(species %like% "KW") | any(species %like% "killer"))){
      shape <- c(
        "Pacific white-sided dolphin" = 23, #diamond
        "Humpback whale" = 21, #circle
        "Harbour porpoise" = 21,
        "Dall\'s porpoise" = 21,
        "Unknown porpoise" = 21,
        "Fin whale" = 23,
        "Fin whale - off effort" = 23,
        "Grey whale - off effort" = 23,
        "Grey whale" = 23,
        "Minke whale - off effort" = 23,
        "Minke whale" = 23,

        "KW - northern resident" =24, #triangle
        "KW - southern resident"=25, #upside down triangle
        "KW - Bigg\'s" = 22,#,           #circle
        "KW - unknown ecotype"=21)# = 21,   #square
    }
    if(!is.null(set_shape)) shape <- set_shape
    #-------------------------------------------------------------------

    n.sp <- length(unique(sp))
    if(leg_dir=="vertical"){
      shp = guide_legend(ncol=1,order = 1,override.aes = list(size=2),title.position = "top", direction = leg_dir)
      fl =  guide_legend(ncol=1,order = 1, direction = leg_dir)
    }else{
      shp = guide_legend(ncol=n.sp,order = 1,override.aes = list(size=2),title.position = "top", direction = leg_dir)
      fl =  guide_legend(ncol=n.sp,order = 1, direction = leg_dir)
    }
    if(!sp_leg) shp <- fl <- "none"

    ####################################################################
    ####################################################################
    # PLOT BY GIVEN SPECIES ORDER AND/OR GROUP SIZE
    ####################################################################
    ####################################################################

    #################### spec_order ##############################
    #################### grp sz ##############################
    if(spec_order){
      if(!is.null(plot_grp_sz)){
        for(i in species){
          x <- ap_sf %>% filter(Species==i)
          g <- g + geom_sf(data = x, alpha = set.alpha, colour="black",stroke=0.1,
                           aes(fill = Species, shape = Species, size = Count)) +
            scale_fill_manual(values = cols, breaks = sp, name = NULL, guide=fl )   +
            scale_shape_manual(values = shape, breaks = sp, name = NULL, guide=shp) +
            scale_size_manual(values = plot_grp_sz, name="  Group Size") +
            ggnewscale::new_scale("shape") +
            ggnewscale::new_scale("fill") +
            guides(alpha= "none",
                   shape=shp,
                   colour="none",
                   fill=fl,
                   size=guide_legend(direction = leg_dir))
        }
      }else{
        #################### NOT grp sz ##############################

        for(i in species){
          x <- ap_sf %>% filter(Species==i)
          g <- g + geom_sf(data = x, alpha = set.alpha, colour="black",stroke=0.1,
                           aes(fill = Species, shape = Species, size = 0.75)) +
            scale_fill_manual(values = cols, breaks = sp, name = NULL, guide=fl )   +
            scale_shape_manual(values = shape, breaks = sp, name = NULL, guide=shp) +
            ggnewscale::new_scale("shape") +
            ggnewscale::new_scale("fill") +
            guides(alpha= "none",
                   shape=shp,
                   colour="none",
                   fill=fl,
                   size="none")
        }
      }
    }else{
      #################### NOT spec_order ##############################
      #################### grp sz ##############################

      if(!is.null(plot_grp_sz)){
        g <-      g + geom_sf(data = ap_sf, alpha = set.alpha, colour="black",stroke=0.1,
                              aes(fill = Species, shape = Species, size = Count)) +#
          # scale_size_manual(values = c(1,2,3), name="Group Size") +
          scale_fill_manual(values = cols, breaks = sp, name = NULL, guide=fl )   +
          scale_shape_manual(values = shape, breaks = sp, name = NULL, guide=shp) +
          scale_size_manual(values = plot_grp_sz, name="  Group Size") +
          ggnewscale::new_scale("shape") +
          ggnewscale::new_scale("fill") +
          guides(alpha= "none",
                 shape=shp,
                 colour="none",
                 fill=fl,
                 size=guide_legend(direction = leg_dir))
      }else{
        #################### NOT grp sz ##############################
        if(is.null(specify_pt_size)) specify_pt_size <- 1
        g <-      g + geom_sf(data = ap_sf, alpha = set.alpha, colour="black",stroke=0.1,size=specify_pt_size,
                              aes(fill = Species, shape = Species)) +#
          # scale_size_manual(values = c(1,2,3), name="Group Size") +
          scale_fill_manual(values = cols, breaks = sp, name = NULL, guide=fl )   +
          scale_shape_manual(values = shape, breaks = sp, name = NULL, guide=shp) +
          scale_size_manual(values = c(2), name="") +
          ggnewscale::new_scale("shape") +
          ggnewscale::new_scale("fill") +
          guides(alpha= "none",
                 shape=shp,
                 colour="none",
                 fill=fl,
                 size="none")
      }

    }
    ###################################################################
    ###################################################################
  }

  #-----------------------------------------------------------
  # to add in hydrophone deployment position(s)
  #-----------------------------------------------------------
  # (see hydrophone/hydrophone.R)
  if(hydrophone){
    # source("R/hydrophone.R")
    # this csv is Lisa's master spreadsheet for mooring deployments on sharepoint
    am <- read.csv("C:/users/keppele/documents/cemore/analysis/cemore_analysis/acoustic_data/moorings.csv")[,c(1,13,16,21,22,24,25)]
    names(am) <- c(am[1,1],"deployed", "retrieved", am[1,4:7])
    acoust <- am[2:nrow(am),] %>%
      mutate(lat=as.numeric(lat_deg)+as.numeric(lat_min)/60,
             lon=-as.numeric(lon_deg)-as.numeric(lon_min)/60,
             deployed = lubridate::date(mdy_hm(deployed)),
             retrieved = lubridate::date(mdy_hm(retrieved))) %>%
      filter(!is.na(lat), !is.na(lon)) %>%
      dplyr::select(-c(lat_deg,lat_min,lon_deg,lon_min)) %>%
      tidyr::pivot_longer(cols=c(deployed, retrieved),names_to = "action", values_to="date") %>%
      mutate(year=year(date), month=month(date)) %>%
      st_as_sf(coords=c("lon","lat"),crs=4326)


    if(single_survey){
      acoust <- acoust %>% dplyr::filter(year %in% as.numeric(years) & month %in% as.numeric(months))
    }
    g <- g +
      geom_sf(data = acoust, shape=8,aes(colour = action), size = 2) +
      scale_colour_manual(values = c("yellow","orange"), name = "Acoustic recorders") +
      ggnewscale::new_scale("colour")
  }
  #-----------------------------------------------------------

  # ----------------------------------------------------------------------
  # ----------------------------- FINAL PLOT -----------------------------
  # ----------------------------------------------------------------------

  g <- g +
    theme(
      # plot.margin = unit(c(0,0,0,0), "cm"),
      # legend.box.justification=leg_box_just, # not working
      plot.background = element_rect(fill='transparent'),
      # plot.background = element_rect(fill='transparent', color=NA),
      panel.background = element_rect(fill=background, color=NA),

      # panel.background = element_blank(),

      legend.position=leg.pos,
      legend.key = element_blank(),
      legend.key.spacing.x=leg_key_spacing_x,
      legend.key.spacing.y=leg_key_spacing_y,

      legend.direction = leg_dir,
      legend.box=leg_box,
      legend.box.background = element_rect(colour = "black", fill="white"),

      legend.text = element_text(size=label_size),#size=fig_legend_size
      legend.title = element_text(size=label_size), #change legend title font size
      legend.margin = margin(legend_margin),
      legend.background = element_blank(),
      legend.box.margin = margin(legend_margin),
      legend.box.spacing = leg.box.spacing,
      legend.spacing.y = unit(0, 'mm'),

      legend.key.size = unit(1, 'cm'),
      axis.text.x = element_text(angle=axis_angle,vjust=0.7, size=label_size),
      axis.text.y = element_text(angle=axis_angle, size=label_size),
    )

  if(!is.null(set_lat_scale)){
    g <- g + metR::scale_x_longitude(breaks = set_lat_scale)
  }
  if(!is.null(set_lon_scale)){
    g <- g +metR::scale_y_latitude(breaks = set_lon_scale)
  }
  if(leg.pos=="inside"){
    g <- g +
      theme(
        legend.position.inside = legend_placement,
        legend.justification =legend_justification      )
  }

  if(!grid_label) g <- g+theme(axis.text.x = element_blank (),
                               axis.ticks.x = element_blank (),
                               axis.text.y = element_blank (),
                               axis.ticks.y = element_blank ())


  g <- g +
    coord +
    ylab("")+xlab("")

  if(facet_season) g <- g + facet_wrap(~ season) #+ # guides(shape="none",fill="none",size="none")
  if(facet_month) g <- g + facet_wrap(~ month_abb,nrow=4) #+ # guides(shape="none",fill="none",size="none")
  if(facet_year) g <- g + facet_wrap(~ year) #+ # guides(shape="none",fill="none",size="none")

  if(!legend) g <- g + theme(legend.position = "none")
  if(!is.null(title)) g <- g + ggtitle(title)

  if(facet_seasonYear & !rare_spp) {
    g <- g + facet_grid(season ~ year)
    if(facet_yearSeason){g <- g + facet_grid(year ~ season)}

    # Get ggplot grob
    g1 = ggplotGrob(g)
    # g1$layout

    # Remove the grobs
    #  and the relevant row in the layout data frame needs to be removed
    pos <- grepl(pattern = "panel-1-1", g1$layout$name)
    g1$grobs <- g1$grobs[!pos]
    g1$layout <- g1$layout[!pos, ]
    pos <- grepl(pattern = "panel-2-1", g1$layout$name)
    g1$grobs <- g1$grobs[!pos]
    g1$layout <- g1$layout[!pos, ]

    # # Draw the plot - below

  }

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

  if(depth){
    g <- cowplot::plot_grid(g, leg1, ncol = 1, rel_heights = c(1, .00001))}

  if(save & !facet_seasonYear){
    if(single_survey){
      if(is.null(file_name)) file_name <- paste0("C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/output_maps_cemore/summary_map_",survey_title,".png")
    }else{
      if(is.null(file_name)) file_name <- paste0("C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/output_maps_cemore/all_cemore_sightings_to_",survey_title,".png")
    }
    ggsave(file_name, height = 15, width = 15, units = "cm")
  }

  if(save & facet_seasonYear & !rare_spp){
    if(is.null(file_name)) file_name <- paste0("C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/output_maps/seasonal_track_to_",survey_title,".png")
    png(file_name);grid.newpage; grid.draw(g1); dev.off()}

  if(save & facet_seasonYear & rare_spp){
    if(is.null(file_name)) file_name <- paste0("C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/output_maps/seasonal_track_to_",survey_title,".png")
    ggsave(file_name, height = 15, width = 15, units = "cm")
  }

  if(!facet_seasonYear & print) {return(g); print(g)}
  if(facet_seasonYear & print & !rare_spp) { return(g1); grid.newpage(); grid.draw(g1)}
  if(facet_seasonYear & print & rare_spp) {return(g); print(g)}
  if(facet_seasonYear & !print & !rare_spp) g <- g1
}

