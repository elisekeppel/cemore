# after Eva's code
plot_survey_summary <- function(
    col_land = "grey 70",
    sgt_data = NULL,
    surveyid=NULL,
    data.source="cemore",
    bath = T,
    set_lat_scale=NULL, # ie. seq(-132,-123,4)
    set_lon_scale=NULL,
    survey_area="all",
    survey_area_colour="grey93",
    background="white",
    high_res = F,
    save = F,
    file_name = NULL,
    xmin=NULL,
    xmax=NULL,
    ymin=NULL,
    ymax=NULL,
    # axis_labels = T,
    # axis_text_size = NULL,
    border = F,
    single_survey=T,
    badelf = NULL,

    plot_effort = T, # T/F
    eff_size = 1,
    effort_colour = "black",
    effort_data = NULL,
    effort_by_day = F,
    effort_by_bf = F,
    effort_by_vis = F,
    effort_by_vis_and_bf = F,
    colour_month = F,
    colour_season = F,
    colour_year = F,
    month_col = NULL,
    season_col = NULL,
    year_col = NULL,
    # show_transit = F,
    N = F,
    count_transects = F,
    km = F,
    depth = F,
    legend = T,
    sp_leg=F,
    plot_sgt = T,
    no_on_eff_sp=F,
    date_cut_off=NULL,

    set.alpha=0.9,
    set.shape.outline.colour = "grey30",
    species = NULL,
    spec_order=F,
    incidentals = F,
    exclude_sp = NULL,
    incl_porps = F, # include hw and porps in incidentals
    plot_grp_sz=NULL,# c(1.4, 2.4, 3)

    specify_pt_size=NULL,
    sgt_colours=NULL,
    set_shape=NULL,
    hydrophone = F,
    facet_month = F,
    facet_year = F,
    facet_season=  F,
    facet_bimonth = F,
    facet_seasonYear=F,
    facet_yearSeason=F,
    facet_month_ncol = 2,
    facet_season_ncol = 2,
    facet_year_ncol = 2,
    strip_size=10,
    cols,
    # text_size, # for axis text and geom_text ie.  10 * 25.4 / 72.27 (scaling ratio)
    # label_size, # in theme(), like legend text and legend title ie. 10
    axis_angle=0,
    rare_spp=F,
    leg.pos = "bottom",
    leg.pos2 = "bottom",
    # ncols = 1,
    leg.box.spacing=NULL,
    legend_justification=NULL,
    leg_key_spacing_x = unit(0.25, "cm"),
    leg_key_spacing_y = unit(0.25, "cm"),
    # leg_key_size = unit(1, "cm"),
    legend_placement =NULL,
    legend_spacing=NULL,
    legend_margin=NULL,
    leg.title.pos="top",
    leg_box = "vertical",
    leg.just = "left",
    # leg_dir,
    leg_dir_eff = "vertical",
    leg_dir_spp = "vertical",
    leg_dir_siz = "vertical",
    leg_nrow_eff = NULL,
    leg_ncol_eff = NULL,
    leg_nrow_spp = NULL,
    leg_ncol_spp = NULL,
    grid_label = TRUE,
    coord = NULL,
    print = T,
    title=NULL,
    us_spelling=F
    # sgt_data = NULL
    # data.source="cemore"
    # bath = T
    # set_lat_scale=NULL  # ie. seq(-132,-123,4
    # set_lon_scale=NULL
    # survey_area="all" # all or ca
    # survey_area_colour="grey93"
    # background="white"
    # high_res = F
    # save = F
    # file_name = NULL
    # xmin=NULL
    # xmax=NULL
    # ymin=NULL
    # ymax=NULL
    # border = F
    # single_survey=T
    # badelf = NULL
    # plot_effort = T # T/
    # effort_colour = "black"
    # effort_data = NULL
    # effort_by_day = F
    # effort_by_bf = F
    # effort_by_vis = F
    # colour_month = F
    # colour_year = F
    # # show_transit = F
    # N = F
    # km = F
    # depth = F
    # legend = T
    # sp_leg=F
    # plot_sgt = T
    # no_on_eff_sp=F
    # date_cut_off=NULL
    #
    # set.alpha=0.9
    # set.shape.outline.colour = "grey30"
    # species = NULL
    # spec_order=F
    # incidentals = F
    # exclude_sp = NULL
    # incl_porps = F # include hw and porps in in
    # plot_grp_sz=NULL # c(1.4, 2.4, 3)
    #
    # specify_pt_size=NULL
    # sgt_colours=NULL
    # set_shape=NULL
    # hydrophone = F
    # facet_month = F
    # facet_year = F
    # facet_season=  F
    # facet_bimonth = F
    # facet_seasonYear=F
    # facet_yearSeason=F
    # strip_size=10
    # cols
    # text_size # for axis text and geom_text ie.72.27 (scaling ratio)
    # label_size # in theme(), like legend text ae ie. 10
    # axis_angle=0
    # rare_spp=F
    # leg.pos = "bottom"
    # leg.pos2 = "bottom"
    # ncols = 1
    # leg.box.spacing=NULL
    # legend_justification=NULL
    # leg_key_spacing_x = unit(0.25, "cm")
    # leg_key_spacing_y = unit(0.25, "cm")
    # # leg_key_size = unit(1, "cm")
    # legend_placement =NULL
    # legend_spacing=NULL
    # legend_margin=NULL
    # leg.title.pos="top"
    # leg_box = "vertical"
    # leg_dir_spp = "vertical"
    # leg_dir_sz = "vertical"
    # grid_label = TRUE
    # coord = NULL
    # print = T
    # title=NULL
    # us_spelling=F
){
  # ---------------------------------------------------------------------
  # --------------------------- SET UP DATA -----------------------------
  # ---------------------------------------------------------------------
  if(is.null(sgt_data)) sgt_data <- all_ap_sf
  if(is.null(effort_data)) effort_data <- all_effort_lines

  if(single_survey){
    sgt_data = sgt_data %>% filter(SurveyID == surveyid)
    effort_data = effort_data %>% filter(SurveyID == surveyid)

    years <- unique(effort_data$year)
    months <- unique(effort_data$month)
    survey_title <- paste(month.abb[month], year)
  }else{
    years <- year
    months <- month
    survey_title <- paste("All surveys to ", month.abb[month], year)
  }

  survey_abbrev <- paste(year, month, year)

  ap_sf <- sgt_data %>% dplyr::select(year,month,month_abb,
                                      # bimonth,
                                      Species,Group_Size,season,seasonYear)
  if(no_on_eff_sp) ap_sf <- ap_sf[0,]

  # if(is.null(legend_margin)) legend_margin <- 1

  # ----------------------------------------------------------------------
  # ----------------- LOAD SPATIAL FILES --------------------------------
  # ----------------------------------------------------------------------
  # bc coast for plotting
  {
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

    if(survey_area =="can" & !exists("survey_can")){
      survey_can <- read_sf("C:/Users/KeppelE/Documents/CeMoRe/Analysis/cemore_sdm/shapefiles/study.area.can2.shp")
    }
    if(survey_area =="all" & !exists("survey_full")){
      survey_full <- sf::read_sf("C:/Users/KeppelE/Documents/github/cemore/cemore/data/Full_study_area_UTM9N.shp") %>% st_union() %>%  st_transform(crs = 4326)
      # survey_can <- st_intersection(survey_area,canada_shp)
      # canada_shp %<>% st_cast("MULTILINESTRING")
    }
  }
  #-----------------------------------------------------------
  #----------------------- BATHYMETRY ------------------------
  #-----------------------------------------------------------
  {
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
                # axis.text=element_text(size=text_size),
                # legend.text=element_text(size=label_size)
          ) +
          guides(fill = guide_colorbar(title.position = "left"))
        ggnewscale::new_scale("fill")

        # leg1 <- gtable_filter(ggplot_gtable(ggplot_build(b_leg)), "guide-box")
        # leg1Grob <- grobTree(leg1)
        leg1 <- cowplot::get_legend(b_leg)
      }
    }
  }
  # ---------------------------------------------------------------------
  # --------------------------- BASEMAP ---------------------------------
  # ---------------------------------------------------------------------
  {base <- ggplot()
  if(bath){
    base <- base +
      geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  #labs(fill = "Depth (m)") +
      scale_fill_gradientn(colours = col_ramp(20), guide = "none") +
      ggnewscale::new_scale("fill")
  }else{
    # col <- RColorBrewer::brewer.pal(9L, "Blues")[2]
    # bl <- col
    base <- base +  theme(    plot.background = element_rect(fill=background),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank())
  }
  if(survey_area == "all") base <- base + geom_sf(data=survey_full, fill=survey_area_colour, colour="grey50", alpha = 0.4)
  if(survey_area == "can") base <- base + geom_sf(data=survey_can, fill=survey_area_colour, colour="grey50")
  # if(survey_area) g <- g + geom_sf(data=survey_can, colour="grey80")

  g <- base +
    # geom_sf(data = coast_file, size = 0.1, fill = "light yellow", colour = "grey 60") +
    geom_sf(data = coast_file, linewidth = 0.01, fill = col_land, colour = "grey 40") +
    border
  }

  # ---------------------------------------------------------------------
  # ------------------------------- EFFORT ------------------------------
  # ---------------------------------------------------------------------

  if(plot_effort){
    if(effort_by_day){
      col <- c(paste0(c(RColorBrewer::brewer.pal(9, "Set1"))), paste0(c(RColorBrewer::brewer.pal(8, "Dark2"))))
      g <- g +
        # geom_sf(data = effort_data, size = 0.25, aes(colour = as.factor(date))) +
        geom_sf(data = effort_data, linewidth = eff_size, aes(colour = as.factor(date))) +
        scale_colour_manual(name="Date", values=col) +
        guides(colour = guide_legend(ncol=leg_ncol_eff,order = 1, nrow = leg_nrow_eff)) +

        # guides(colour = guide_legend(ncol=leg_ncol_eff,order = 1,override.aes = list(size=1),position = "bottom", title.position=leg.title.pos)) + #
        ggnewscale::new_scale("colour")
    }
    # to size lines by vis
    if(effort_by_vis | effort_by_bf | effort_by_vis_and_bf){
      effort_data <- effort_data %>%
        mutate(
          beauf_char = as.character(Beaufort),
          Visib = factor(
            case_when(
              Visib == "G&E" ~ "Excellent/Good",
              Visib == "Moderate" ~ "Moderate",
              Visib == "P" ~ "Poor"),
            levels = c("Excellent/Good", "Moderate", "Poor")))

      pal <- brewer.pal(9, "Set1")[c(1:9)] #red,blue,green,purple,orange,yellow,brown,pink,grey
      bf <-   c("0" = pal[2], #blue
                "1" =  "turquoise",
                "2" = pal[3], #green
                "3" = pal[6], #yellow
                "4" = pal[5], #orange
                "5" = pal[1], #red
                "6" = pal[4], #purple
                "7" = "black")
      if(effort_by_vis){
        g <- g + geom_sf(data = effort_data, aes(colour=Visib)) +
          scale_colour_manual(name="Visibility", values = c("light blue", "blue", "red")) +
          guides(colour = guide_legend(ncol=leg_ncol_eff, override.aes = list(linewidth=1),title.position=NULL, direction=leg_dir_eff, nrow = leg_nrow_eff)) +
          ggnewscale::new_scale("colour")
      }

      if(effort_by_bf){
        g <- g + geom_sf(data = effort_data, aes(colour=beauf_char)) +
          scale_colour_manual(name="Beaufort", values = bf) +
          guides(colour = guide_legend(ncol=leg_ncol_eff, override.aes = list(linewidth=1),title.position=NULL, direction=leg_dir_eff, nrow = leg_nrow_eff)) +
          ggnewscale::new_scale("colour")
      }

      if(effort_by_vis_and_bf){


        g <- g + geom_sf(data = effort_data, aes(linewidth =Visib, colour=beauf_char)) +
          scale_colour_manual(name="Beaufort", values = bf, guide="legend") +
          scale_linewidth_manual(name="Visibility",
                                 values=c(1.75, 1.3, 0.5), labels = c("Excellent/Good","Moderate","Poor"),
                                 guide = "legend") +
          guides(colour = guide_legend(ncol=leg_ncol_eff, order = 1, override.aes = list(linewidth=1),title.position=NULL, direction=leg_dir_eff, nrow = leg_nrow_eff)) +
          guides(linewidth = guide_legend(ncol=leg_ncol_eff, title.position=NULL, direction=leg_dir_eff, nrow = leg_nrow_eff)) +
          ggnewscale::new_scale("linewidth") +
          ggnewscale::new_scale("colour")

      }
    }

    if(!effort_by_day & !effort_by_vis & !effort_by_bf & !effort_by_vis_and_bf){ #  & !show_transit

      if(colour_month){
        if(is.null(month_col)){
          # mpal <- c(
          #   brewer.pal(8, "Set1"),#[c(5,7,9)],       #
          #   brewer.pal(6, "GnBu")[6],  # light blue
          #   brewer.pal(5, "Dark2")[c(4,5)],  # dark pink, light green
          #   brewer.pal(5, "RdPu")[7],  # purple
          #   brewer.pal(9, "YlOrRd")[9])#,       # burgundy
          # month_col <- c("Jan" = mpal[1], #red
          #                "Feb" = mpal[11], # green
          #                "Mar" = "grey80", #
          #                "Apr" = mpal[2],
          #                "May" = mpal[5],
          #                "Jun" = mpal[6],
          #                "Jul" = mpal[12],
          #                "Aug" = mpal[11],
          #                "Sep" = mpal[10],
          #                "Oct" = mpal[7],
          #                "Nov" = mpal[8],
          #                "Dec" = mpal[9])}
          month_col <- c(brewer.pal(n=12, "Paired")[2:12], "black")}

        # if(facet_seasonYear) month_col <- rep(c("green4", "#00008B", "#FF4040"), 4)
        # if(facet_seasonYear) month_col <- cols

        g <- g +
          geom_sf(data = effort_data, linewidth = eff_size, aes(colour = month_abb)) +
          scale_colour_manual(values=month_col, name=NULL) + # c("Winter", "Spring", "Summer", "Fall")
          guides(colour = guide_legend(ncol=leg_ncol_eff, nrow = leg_nrow_eff, order = 3,override.aes = list(linewidth=1),title.position=leg.title.pos, direction=leg_dir_eff)) + #coord
          ggnewscale::new_scale("colour")

      }else if(colour_year){
          if(is.null(year_col)){
            # ypal <- c(brewer.pal(8, "Dark2"))

            # year_col <-  c("2020" = ypal[1],
            #                "2021" = ypal[2],
            #                "2022" = ypal[3],
            #                "2023" = ypal[4],
            #                "2024" = ypal[5],
            #                "2025" = ypal[6],
            #                "2026" = ypal[7],
            #                "2027" = ypal[8])}

            year_col <- viridis::turbo(n=length(unique(effort_data$year)) + 2)}
            # scales::show_col(v)

          g <- g +
            geom_sf(data = effort_data, linewidth = eff_size, aes(colour = as.character(year))) +
            scale_colour_manual(values=year_col, name="Survey effort") +
            guides(colour = guide_legend(ncol=leg_ncol_eff, nrow = leg_nrow_eff, order = 3,override.aes = list(linewidth=1),title.position=NULL,direction=leg_dir_eff)) + #coord
            ggnewscale::new_scale("colour")
}else if(colour_season){
            if(is.null(season_col)){
              ok <-  palette.colors(palette = "Okabe-Ito") # not a palette in brewer pal
              # scales::show_col(ok)
              # spal <- ok[c(1,4,5,7)]
              #
              # season_col <-  c("Winter" = spal[1],
              #                  "Spring" = spal[2],
              #                  "Summer" = spal[3],
              #                  "Fall" = spal[4])}
              season_col <- viridis::turbo(n=4)}

            g <- g +
              geom_sf(data = effort_data, linewidth = eff_size, aes(colour = as.character(season))) +
              scale_colour_manual(values=season_col, name="Survey effort") +
              guides(colour = guide_legend(ncol=leg_ncol_eff, nrow = leg_nrow_eff, order = 3,override.aes = list(linewidth=1),title.position=NULL,direction=leg_dir_eff)) + #coord
              ggnewscale::new_scale("colour")

        }else{
          g <- g +
            geom_sf(data = effort_data, linewidth = 0.1, colour = effort_colour, aes(colour = "Survey effort")) +
            # geom_sf(data = effort_data, size = 0.25, aes(colour = "Survey effort"))# +
            # scale_colour_manual(values=c("Off effort" = "grey60", "On effort" = "black")) +
            guides(colour = guide_legend(ncol=leg_ncol_eff, nrow = leg_nrow_eff, direction=leg_dir_eff))
          ggnewscale::new_scale("colour")
          #    }
        }
      }
    }


  # ---------------------------------------------------------------------
  # ------------------------- TRACKS/EFFORT LEGEND ----------------------
  # ---------------------------------------------------------------------
  if(plot_effort ==T & is.null(badelf)){
    g <- g + scale_colour_manual(values=c(effort_colour), name="Survey effort") +
      guides(colour = guide_legend(ncol=leg_ncol_eff, nrow = leg_nrow_eff,order = 3,override.aes = list(size=1),title.position=NULL))
  }
  if(plot_effort ==F & !is.null(badelf)){
    g <- g + scale_colour_manual(values=c("Trackline" = effort_colour), name=NULL) +
      guides(colour = guide_legend(ncol=leg_ncol_eff, nrow = leg_nrow_eff,order = 3,override.aes = list(size=1),title.position=NULL))
  }
  if(plot_effort ==T & !is.null(badelf)){
    g <- g + scale_colour_manual(values=c("Trackline" = "grey50", "Survey effort" = "black"), name=NULL) +
      guides(colour = guide_legend(ncol=leg_ncol_eff, nrow = leg_nrow_eff,order = 3,override.aes = list(size=1),title.position=NULL))
  }
  g <- g + ggnewscale::new_scale("colour")

  # ---------------------------------------------------------------------
  # ------------------------- TRACKS/EFFORT LABELS ----------------------
  # ---------------------------------------------------------------------
  if(N | count_transects | km){
    if(facet_season & !facet_seasonYear){
      tx <- effort_data %>% data.frame() %>%
        group_by(season)}
    if(facet_month){
      tx <- effort_data %>% data.frame() %>%
        group_by(month_abb)}
    if(facet_seasonYear){
      tx <- effort_data %>% data.frame() %>%
        group_by(year, season)}
    if(single_survey)  tx <- effort_data %>% data.frame() %>% group_by(SurveyID)

    if(!facet_season & !facet_seasonYear & !facet_month){
      tx <- effort_data %>% data.frame()}

    tx %<>%
      dplyr::summarise(N = n_distinct(SurveyID),
                       transects = length(unique(TransectID)),
                       dist = prettyNum(round(sum(length_km), 0), big.mark=","))

    if(N){
      g <- g+geom_text(data = tx, aes(x = -124.3, y = 48.95, label = paste0(N, ifelse(N==1, " survey", " surveys"))))#, size = text_size)
    }

    if(count_transects){
      g <- g+geom_text(data = tx, aes(x = -124.3, y = 48.82, label = paste0(transects, ifelse(transects==1, " transect", " transects"))))#, size = text_size)
    }

    if(km){
      g <- g+geom_text(data = tx, aes(x = -124.3, y = 48.7, label = paste0(dist, " km")))
    }
  }
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
        dplyr::select("Species","Group_Size", date, "season",geometry)
      if(!is.null(date_cut_off)) inc <- inc %>% filter(date<date_cut_off)

      if(!nrow(inc)<0) ap_sf <- bind_rows(ap_sf,inc)

    }

    if(!is.null(species) & length(species) == 1) {
      ap_sf %<>% dplyr::filter(Species %like% species)
      # species <- tolower(unique(ap_sf$Species))
    }

    if(!is.null(species) & length(species) > 1) ap_sf %<>% dplyr::filter(Species %in% species)

    # ap_sf %<>% mutate(Species = factor(gsub(pattern="killer whale",replacement="KW",.$Species)))
    # species <- gsub("killer whale", "KW", speciec()

    if(us_spelling){
      levels(ap_sf$Species) <- c(levels(ap_sf$Species), "Gray whale", "gray whale")
      ap_sf[which(ap_sf$Species == "grey whale"),]$Species <- "gray whale"
      ap_sf[which(ap_sf$Species == "Grey whale"),]$Species <- "Gray whale"

      levels(ap_sf$Species) <- c(levels(ap_sf$Species), "Harbor porpoise", "harbor porpoise")
      ap_sf[which(ap_sf$Species == "harbor porpoise"),]$Species <- "harbor porpoise"
      ap_sf[which(ap_sf$Species == "Harbor porpoise"),]$Species <- "Harbor porpoise"
    }

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
        Group_Size %in% c(2:5) ~ "2-5",
        Group_Size >5 ~ ">5"
      ) %>% factor(levels = c("1", "2-5", ">5")))
    }else{
      ap_sf <- ap_sf %>% mutate(Count = as.factor("1"))
    }

    #-----------------------------------------------------------
    # REMOVE SPECIES FROM PLOTS
    #-----------------------------------------------------------
    if(!is.null(exclude_sp)){
      ap_sf %<>% filter(!Species %in% exclude_sp)
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

    if(is.null(sgt_colours)) {
      # cols <- sgt_colours
      # }else{
      sgt_colours <-   c("Pacific white-sided dolphin" = pal[8], #pink
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
                         "Killer whale - northern resident" = "black",
                         "Killer whale - southern resident" = "black",
                         "Killer whale - Bigg\'s" = "black",
                         "Killer whale - unknown ecotype" = "black",
                         "Fin whale" = pal[5], #orange
                         "Fin whale - off effort" = "red",
                         "Grey whale - off effort" = "blue", #grey
                         "Grey whale" = "blue", #grey
                         "Minke whale - off effort" = pal[6], #brown
                         "Minke whale" = pal[6]) #brown
    }

    # if(!is.null(species)){
    shape <- c("Pacific white-sided dolphin" = 21, #diamond
               "Humpback whale" = 21, #circle
               "Harbour porpoise" = 21,
               "Dall\'s porpoise" = 21,
               "Unknown porpoise" = 21,
               "KW - northern resident" =24,#= 24, #triangle
               "KW - southern resident"=25,# = 25, #upside down triangle
               "KW - Bigg\'s" = 21,#,           #circle
               "KW - unknown ecotype"=22,# = 21,   #square
               "Killer whale - northern resident" =24,#= 24, #triangle
               "Killer whale - southern resident"=25,# = 25, #upside down triangle
               "Killer whale - Bigg\'s" = 21,#,           #circle
               "Killer whale - unknown ecotype"=22,# = 21,   #square
               "Fin whale" = 21,
               "Fin whale - off effort" = 21,
               "Grey whale - off effort" = 21,
               "Grey whale" = 21,
               "Minke whale - off effort" = 21,
               "Minke whale" = 21,
               "KW - all ecotypes" = 21)
    # }

    # if(!is.null(species) & (any(species %like% "KW") | any(species %like% "killer"))){
    #   shape <- c(
    #     "Pacific white-sided dolphin" = 21, #diamond
    #     "Humpback whale" = 21, #circle
    #     "Harbour porpoise" = 21,
    #     "Dall\'s porpoise" = 21,
    #     "Unknown porpoise" = 21,
    #     "Fin whale" = 21,
    #     "Fin whale - off effort" = 21,
    #     "Grey whale - off effort" = 21,
    #     "Grey whale" = 21,
    #     "Minke whale - off effort" = 21,
    #     "Minke whale" = 21,
    #
    #     "KW - northern resident" =24, #triangle
    #     "KW - southern resident"=25, #upside down triangle
    #     "KW - Bigg\'s" = 21,#,           #circle
    #     "KW - unknown ecotype"=22)# = 21,   #square
    # }
    # if(!is.null(set_shape)) shape <- set_shape
    #-------------------------------------------------------------------

    n.sp <- length(unique(sp))
    # if(leg_dir=="vertical"){
    #   shp = guide_legend(ncol=leg_nrow_spp, order = 1,override.aes = list(size=2),title = NULL, direction = leg_dir)
    #   fl =  guide_legend(ncol=leg_nrow_spp, order = 1, direction = leg_dir, title = NULL)
    # }else{
    #   shp = guide_legend(ncol=leg_nrow_spp,order = 1,override.aes = list(size=2),title = NULL, direction = leg_dir)
    #   fl =  guide_legend(ncol=leg_nrow_spp,order = 1, direction = leg_dir, title = NULL)
    # }
    # if(!sp_leg) shp <- fl <- "none"

    ####################################################################
    ####################################################################
    # PLOT BY GIVEN SPECIES ORDER AND/OR GROUP SIZE
    ####################################################################
    ####################################################################

    #################### spec_order ##############################
    #################### grp sz ##############################
    # if(spec_order){
    #
    #
    #   if(leg_dir=="vertical"){
    #     shp = guide_legend(ncol=leg_nrow_spp, order = 1,override.aes = list(size=2),title = NULL, direction = leg_dir)
    #     fl =  guide_legend(ncol=leg_nrow_spp, order = 1, direction = leg_dir, title = NULL)
    #   }else{
    #     shp = guide_legend(ncol=leg_nrow_spp, order = 1,override.aes = list(size=2),title = NULL, direction = leg_dir)
    #     fl =  guide_legend(ncol=leg_nrow_spp, order = 1, direction = leg_dir, title = NULL)
    #   }
    #   if(!sp_leg) shp <- fl <- "none"
    #
    #   if(!is.null(plot_grp_sz)){
    #     for(i in species){
    #       x <- ap_sf %>% filter(Species==i)
    #       g <- g + geom_sf(data = x, alpha = set.alpha, colour=set.shape.outline.colour,stroke=0.1,
    #                        aes(fill = Species, shape = Species, size = Count)) +
    #         scale_fill_manual(values = cols, breaks = sp, name = NULL, guide=fl )   +
    #         scale_shape_manual(values = shape, breaks = sp, name = NULL, guide=shp) +
    #         scale_size_manual(values = plot_grp_sz, name="  Group Size") +
    #         ggnewscale::new_scale("shape") +
    #         ggnewscale::new_scale("fill") +
    #         guides(alpha= "none",
    #                shape=shp,
    #                colour="none",
    #                fill=fl,
    #                size=guide_legend(direction = leg_dir))
    #     }
    #   }else{
    #     #################### NOT grp sz ##############################
    #
    #     for(i in species){
    #       x <- ap_sf %>% filter(Species==i)
    #       g <- g + geom_sf(data = x, alpha = set.alpha, colour=set.shape.outline.colour,stroke=0.1,
    #                        aes(fill = Species, shape = Species, size = 0.75)) +
    #         scale_fill_manual(values = cols, breaks = sp, name = NULL, guide=fl )   +
    #         scale_shape_manual(values = shape, breaks = sp, name = NULL, guide=shp) +
    #         ggnewscale::new_scale("shape") +
    #         ggnewscale::new_scale("fill") +
    #         guides(alpha= "none",
    #                shape=shp,
    #                colour="none",
    #                fill=fl,
    #                size="none")
    #     }
    #   }
    # }else{
    #################### NOT spec_order ##############################
    #################### grp sz ##############################

    if(!is.null(plot_grp_sz)){
      # if(n.sp==1){
      #   g  <- g +
      #     geom_sf(data=ap_sf, shape=21, alpha=set.alpha, colour=set.shape.outline.colour, stroke=0.1, fill = sgt_colours, aes(size=Count)) +
      #     scale_fill_manual(values = rep(sgt_colours,3), labels=c("1","2-5",">5"), name = "Group Size") +
      #     scale_size_manual(values = plot_grp_sz,        labels=c("1","2-5",">5"), name = "Group Size") +
      #     guides(fill=guide_legend(title.position=leg.title.pos),
      #            size=guide_legend(title.position=leg.title.pos))
      #
      # }else{
      # works for colour and shape and size!!
      g <- g + geom_sf(data = ap_sf,
                       # shape = 21,
                       alpha = set.alpha, colour=set.shape.outline.colour, stroke=0.1,
                       aes(fill = Species,
                           shape = Species,
                           size = Count)) +#
        scale_fill_manual(values = sgt_colours, breaks = sp,
                          guide=guide_legend(title = "Species",
                                             legend.position = leg.pos,
                                             ncol=leg_ncol_spp,
                                             nrow=leg_nrow_spp,
                                             order = 1, direction = leg_dir_spp,
                                             override.aes = list(size=2.4)) )   +
        scale_shape_manual(values = shape, breaks = sp,
                           guide=guide_legend(title = "Species",
                                              legend.position = leg.pos,
                                              ncol=leg_ncol_spp,
                                              nrow=leg_nrow_spp,
                                              order = 1, direction = leg_dir_spp,
                                              override.aes = list(size=2.4))) +
        # scale_size_manual(values = plot_grp_sz, name="Group Size") +
        scale_size_manual(values = plot_grp_sz,
                          guide = guide_legend(title = "Group Size",
                                               direction = leg_dir_siz,
                                               legend.position = leg.pos,
                                               ncol=leg_ncol_spp,
                                               nrow=leg_nrow_spp,
                                               order = 2)) +
        ggnewscale::new_scale("shape") +
        ggnewscale::new_scale("fill") +
        guides(alpha= "none",
               # shape= guide_legend(direction = leg_dir_spp),
               colour="none"
               # fill= guide_legend(direction = leg_dir_spp),
               )
      # }
    }else{
      #################### NOT grp sz ##############################
      # works for colour and shape
      if(is.null(specify_pt_size)) specify_pt_size <- 1
      g <- g + geom_sf(data = ap_sf, alpha = set.alpha, colour=set.shape.outline.colour, stroke=0.1, size=specify_pt_size,
                       aes(fill = Species, shape = Species)) +#
        scale_fill_manual(values = sgt_colours, breaks = sp, name = NULL,
                          guide=guide_legend(ncol=leg_ncol_spp, order = 1, direction = leg_dir_spp,
                                             nrow=leg_nrow_spp,
                                             override.aes = list(size=2.5),title.position = "top") )   +
        scale_shape_manual(values = shape, breaks = sp,
                           guide=guide_legend(title = NULL,
                                              legend.position = leg.pos2,
                                              ncol=leg_ncol_spp, order = 1, direction = leg_dir_spp,
                                              nrow=leg_nrow_spp,
                                              override.aes = list(size=2.5))) +
        # scale_size_manual(values = plot_grp_sz, name="Group Size") +
        ggnewscale::new_scale("shape") +
        ggnewscale::new_scale("fill") +
        guides(alpha= "none",
               # shape=shp,
               colour="none",
               # fill=fl,
               size="none",
               fill=guide_legend(ncol=leg_ncol_spp, nrow=leg_nrow_spp, order=1, direction = leg_dir_spp, title.position=leg.title.pos),
               shape=guide_legend(ncol=leg_ncol_spp, nrow=leg_nrow_spp, order=1, direction = leg_dir_spp, title.position=leg.title.pos))
    }


    # }
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
      # legend.key.spacing.x=leg_key_spacing_x,
      legend.key.spacing.y=leg_key_spacing_y,

      # legend.direction = leg_dir,
      legend.box=leg_box,
      legend.box.background = element_rect(colour = "black", fill="white"),
      legend.box.just = "left",
      legend.justification = leg.just,
      # legend.text = element_text(size=label_size),#size=fig_legend_size
      # legend.title = element_text(margin = margin(b=0), size=label_size), #change legend title font size
      legend.title.position = "top",
      # legend.title = element_text(size=label_size), #change legend title font size
      # legend.margin = margin(c(10,10,10,10)),
      legend.background = element_blank(),
      legend.box.margin = margin(legend_margin),
      legend.box.spacing = leg.box.spacing,
      legend.spacing.y = unit(0, 'mm')
      # text = element_text(size = text_size),
      # legend.key.size = leg_key_size,
    )

  #   if(axis_labels){
  #     g <- g + theme(
  #       axis.text.x = element_text(angle=axis_angle,vjust=0.7, size=axis_text_size),
  #       axis.text.y = element_text(angle=axis_angle, size=axis_text_size))
  #   }else{
  #     g <- g + theme(axis.text = element_blank())
  # }
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

  if(!grid_label) g <- g+theme(axis.text.x = element_blank(),
                               axis.ticks.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.ticks.y = element_blank())


  g <- g +
    coord +
    ylab("")+xlab("")

  # if(facet_bimonth) g <- g + facet_wrap(~ bimonth, ncol = 2) + theme(strip.text = element_text(size = strip_size))
  if(facet_season) g <- g + facet_wrap(~ season, ncol = facet_season_ncol) + theme(strip.text = element_text(size = strip_size))
  #+ # guides(shape="none",fill="none",size="none")
  if(facet_month) g <- g + facet_wrap(~ month_abb,ncol=facet_month_ncol) #+ # guides(shape="none",fill="none",size="none")
  if(facet_year) g <- g + facet_wrap(~ year, ncol = facet_year_ncol) #+ # guides(shape="none",fill="none",size="none")

  if(!legend) g <- g + theme(legend.position = "none")
  if(!is.null(title)) g <- g + ggtitle(title)

  if(facet_seasonYear & !rare_spp) {
    g <- g + facet_grid(year~season) + theme(strip.text = element_text(size = strip_size))
    if(facet_yearSeason){g <- g + facet_grid(year ~ season)} + theme(strip.text = element_text(size = strip_size))

    # Get ggplot grob
    # g1 = ggplotGrob(g)
    # # g1$layout
    #
    # # Remove the grobs
    # #  and the relevant row in the layout data frame needs to be removed
    # pos <- grepl(pattern = "panel-1-1", g1$layout$name)
    # g1$grobs <- g1$grobs[!pos]
    # g1$layout <- g1$layout[!pos, ]
    # pos <- grepl(pattern = "panel-2-1", g1$layout$name)
    # g1$grobs <- g1$grobs[!pos]
    # g1$layout <- g1$layout[!pos, ]

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
    ggsave(file_name, height = 8.5, width = 8.5, units = "in")
  }

  if(save & facet_seasonYear & !rare_spp){
    if(is.null(file_name)) file_name <- paste0("C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/output_maps/seasonal_track_to_",survey_title,".png")
    # png(file_name, height = 8.5, width = 8.5, units = "in", res = 72);grid.newpage; grid.draw(g); dev.off()
    ggsave(file_name, height = 8.5, width = 8.5, units = "in")
  }

  if(save & facet_seasonYear & rare_spp){
    if(is.null(file_name)) file_name <- paste0("C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/output_maps/seasonal_track_to_",survey_title,".png")
    ggsave(file_name, height = 8.5, width = 8.5, units = "in")
  }

  if(!facet_seasonYear & print) {return(g); print(g)}
  if(facet_seasonYear & print & !rare_spp) { return(g); grid.newpage(); grid.draw(g)}
  if(facet_seasonYear & print & rare_spp) {return(g); print(g)}
  # if(facet_seasonYear & !print & !rare_spp) g <- g
  # if(facet_seasonYear & !print & !rare_spp) return(g)# <- g
}

