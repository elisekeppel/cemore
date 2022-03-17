coast <- sf::st_read(dsn = "shapefiles", layer = "BC_coast_UTM9")
coord <- ggplot2::coord_sf(xlim = c(-125.5, -123), ylim = c(48.1, 49.5), crs = sf::st_crs(4326)) 

#------------------------------------------
# plot gpx survey track lines from bad elf
#------------------------------------------
transect_dir <- "survey_data/tracklines/transects/gpx/2020oct/"
files <- list.files(transect_dir)
transects_oct <- purrr::map_df(files, get_track, folder = "2020oct") 

transect_dir <- "./survey_data/tracklines/transects/gpx/2020nov/"
files <- list.files(transect_dir)
transects_nov <- purrr::map_df(files, get_track, folder = "2020nov") 

transect_dir <- "./survey_data/tracklines/transects/gpx/2020sept/"
files <- list.files(transect_dir)
transects_sept <- purrr::map_df(files, get_track, folder = "2020sept") 

transect_dir <- "./survey_data/tracklines/transects/gpx/2021mar/"
files <- list.files(transect_dir)
transects_mar <- purrr::map_df(files, get_track, folder = "2021mar") 

transect_dir <- "./survey_data/tracklines/transects/gpx/2021feb/"
files <- list.files(transect_dir)
transects_feb <- purrr::map_df(files, get_track, folder = "2021feb") 

transect_dir <- "./survey_data/tracklines/transects/gpx/2021jan/"
files <- list.files(transect_dir)
transects_jan <- purrr::map_df(files, get_track, folder = "2021jan") 

# transect_dir <- "./survey_data/tracklines/transects/gpx/sept2020/"
# files <- list.files(transect_dir)
# transects_sept <- purrr::map_df(files, get_track, folder = "sept2020/") 

plot_track(transects_oct, save = F)
plot_track(transects_nov, save = F)
plot_track(transects_sept, save = F)
plot_track(transects_jan, save = F)
plot_track(transects_feb, save = F)
plot_track(transects_mar, save = F)

# plot all survey lines by month
transects <- rbind(transects_oct, transects_nov, transects_sept,
                   transects_jan, transects_feb, transects_mar) #%>% 
  # group_by(date) %>% 
  # summarize(do_union=FALSE) %>% st_cast("LINESTRING")
ggsave("output_maps/badelf_all_surveys.pdf")



#------------------------------------------
# plot gpx survey intended survey transects
#------------------------------------------
x1 <- tidy_transects(dir = "survey_data/survey transects/2020 transects/gpx", file_name = "t18km_can_1_2020_sept.gpx", iteration = 1)
x2 <- tidy_transects(dir = "survey_data/survey transects/2020 transects/gpx", file_name = "t18km_can_2_2020_oct.gpx", iteration = 2)
x3 <- tidy_transects(dir = "survey_data/survey transects/2020 transects/gpx", file_name = "t18km_can_3_2020_nov.gpx", iteration = 3)

x6 <- tidy_transects(dir = "survey_data/survey transects/2021 transects/gpx", file_name = "t18km_iteration_6_can_2021jan.gpx", iteration = 6)
x7 <- tidy_transects(dir = "survey_data/survey transects/2021 transects/gpx", file_name = "t18km_iteration_7_can_2021feb.gpx", iteration = 7)
x8 <- tidy_transects(dir = "survey_data/survey transects/2021 transects/gpx", file_name = "t18km_iteration_8_can_2021mar.gpx", iteration = 8)

y <- rbind(x1, x2, x3, x6, x7, x8)
ggplot() +  geom_sf(data = coast, fill = "light yellow") +
  # geom_sf(data = y, colour = y$iteration) +
  geom_sf(data = y, colour = y$iteration) +
  scale_colour_manual(values = c("red", "yellow", "orange", "green", "black", "blue")) +
    # geom_sf(data = transects_sept, colour = "blue") +
  coord
ggsave("output_maps/all planned transects to 2021 March.png")




#------------------------------------------
# plot mysti tracks from csv - unprocessed = transects + transitting
#------------------------------------------
transect_dir <- "./survey_data/tracklines/transects/csv/oct2020/"
files <- list.files(transect_dir)
transects_oct_mysti <- purrr::map_df(files, get_track, file_type = "csv", folder = "oct2020/") 

transect_dir <- "./survey_data/tracklines/transects/csv/nov2020/"
files <- list.files(transect_dir)
transects_nov_mysti <- purrr::map_df(files, get_track, file_type = "csv", folder = "nov2020/") 

transect_dir <- "./survey_data/tracklines/transects/csv/2020sept/"
files <- list.files(transect_dir)
transects_sept_mysti <- purrr::map_df(files, get_track, folder = "sept2020/") 

plot_track(transects_oct_mysti, save = F)
plot_track(transects_nov_mysti, save = F)
plot_track(transects_sept_mysti, save = F)


transect_dir <- "./survey_data/tracklines/transects/csv/2021jan/"
files <- list.files(transect_dir)
transects_jan2021 <- purrr::map_df(files, get_track, file_type = "csv", folder = "jan2021/") 
plot_track(transects_jan2021, save = T)


# combine gpx and mysti data to plot
transects <- rbind(transects_oct, transects_oct_mysti) #%>% 
# group_by(date) %>% 
# summarize(do_union=FALSE) %>% st_cast("LINESTRING")
plot_track(transects, save = F) + facet_wrap(vars(type))
