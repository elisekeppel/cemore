# after Eva's code
plot_effort <- function(Save = F,
                        border = F,
                        single_survey=T,
                        facet=NULL, # by month
                        N=T,
                        km=T,
                        depth=F,
                        coord=NULL,
                        bathy=NULL){

    # if(is.null(effort) & single_survey) effort <- T
    # if(is.null(effort) & !single_survey) effort <- F

    if(!single_survey){
      effort_lines <- all_effort_lines
    }else{
      years <- year
      months <- month
    }
    effort_lines$year <- effort_lines$year %>% as.factor()


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

    coord <- ggplot2::coord_sf(xlim = c(-125.5, -122.9), ylim = c(48.1, 49.44), crs = sf::st_crs(4326))

    #-----------------------------------------------------------
    # to create bathymetric layer and basemap
    #-----------------------------------------------------------
    if(is.null(bathy)){bathy <- getNOAA.bathy(-125.7, -122.5,48, 49.5,res=1, keep=TRUE, path="bath") %>%
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

    # g <- g + geom_sf(data = effort_lines, size = 0.25, colour = "black")




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




    #------------- EFFORT LINES ONLY ------------------------------------------
    g <- g +
      border +
      geom_sf(data = effort_lines, size = 0.25, aes(colour=year)) +
      # FOR SEASONS # scale_colour_manual(values = c("dark orange", "green4", "yellow", "dark blue"), name = "Season", guide = "none")
      scale_colour_manual(values = c("black", "yellow", "blue", "green","red"), name = "Year")
    # ggtitle("CeMoRe survey completed transects", paste0("Sept 2020 to ", survey_title))
    # annotation_custom(leg1Grob, xmin=-124.99, xmax=-124.95, ymin=47.89, ymax=48.15) +

    if(N){
      tx <- effort_lines %>% data.frame() %>%
        group_by(month) %>%
        dplyr::summarise(N = n_distinct(SurveyID))
      g <- g+geom_text(data = tx, aes(x = -124.4, y = 48.9, label = paste0(N, " surveys")), size = 3)}
    if(km){
      tx2 <- effort_lines %>% data.frame() %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(dist = round(sum(length_km),0))
      g <- g+geom_text(data = tx2, aes(x = -124.4, y = 48.8, label = paste0(dist, " km")), size = 3)}

    g <- g+ coord +
      ylab(NULL)+xlab(NULL) +
      theme(axis.text.x=element_text(angle=90),
            axis.text = element_text(size=12),
            plot.margin = unit(c(0,0,0,0), "cm"),
            legend.title = element_blank(),
            legend.text = element_text(),#size=fig_legend_size
            legend.position="none")

    if(!is.null(facet)) g <- g +
      facet_grid(month_abb ~ .)


    if(depth){
      g <- cowplot::plot_grid(g, leg1, ncol = 1, rel_heights = c(1, .00001))}

    # if saving plot, change coast to bc_shp for higher res basemap
    if(Save){
      # ggsave(paste0("output_maps/sightings_", survey_title, "_low_res_plus_hydrophones31_78.png"))
      ggsave(paste0("../output_maps/effortlines_", survey_title,"_", facet,".png"), height = 15, width = 15, units = "cm")
    }


    g
  }

