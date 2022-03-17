library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(magrittr)

# bc coast for plotting
coast <- sf::st_read(dsn = "shapefiles", layer = "BC_coast_UTM9")

# higher resolution bc coast --- takes a long time to load
bc_shp <- st_read(dsn = "shapefiles", "BC_AK_WA_union_polygon") %>%
  st_transform(crs = 3156)

# Pacific Canada polygon for outlining Canadian border
canada_shp <- st_read(dsn = "shapefiles", "CanadianEEZ") %>% 
  st_transform(crs = 3156) 


#---------------- read in track-------------------
# get_track <- function(file_name, folder = "oct2020/")  {
#   dir <- paste0( "./survey_data/tracklines/transects/", folder)
#   date <- as.POSIXct(paste0(substring(file_name, 1, 4), "-", substring(file_name, 5, 6), "-", substring(file_name, 7, 8)))
#   t <- plotKML::readGPX(paste0(dir, file_name))[[4]][[1]][[1]] %>%
#     mutate(date = date) 
# }
# 
# 
# get_track <- function(file_name, file_type = "gpx", folder = "oct2020/")  {
#   dir <- file.path( "survey_data/tracklines/transects", file_type, folder)
#   # dir <- paste0( "./survey_data/tracklines/transects/", folder) # on oyster drive on small laptop
# 
#   # date <- as.POSIXct(paste0(substring(file_name, 1, 4), "-", substring(file_name, 5, 6), "-", substring(file_name, 7, 8)))
#   if(substring(file_name[1], nchar(file_name[1]) -3, nchar(file_name[1])) == ".gpx") {
#     t <- plotKML::readGPX(paste0(dir, file_name))[[4]][[1]][[1]] %>% 
#       mutate(type = "gpx")
#   }else{
#     t <- read.csv(file.path(dir, file_name)) %>% 
#       dplyr::rename(lat = Latitude, lon = Longitude, time = Time.Created..UTC.) %>% 
#       dplyr::select(lat, lon, time) %>% 
#       mutate(type = "mysti")
#   }
#   date <- as.POSIXct(substring(t$time[[1]], 1, 10))
#   t %<>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
#     mutate(date = date) %>% 
#     dplyr::select(time, date, type, geometry) %>% 
#     group_by(date) %>% 
#     st_cast("LINESTRING")
#   
# }

# ----------------------------------------------------------------------
# -------------- IMPORT, COLLATE AND PLOT RAW TRACKLINE DATA --------------
# ----------------------------------------------------------------------
# get trackline data for each day and combine into one object for plotting or analysis

# # ------------- From bad elf gpx files ------------------------------
# file_type = "gpx"
# files_gpx <- list.files(paste("./survey_data/tracklines/transects", file_type = file_type, folder, sep = "/"))
# transects_gpx <- purrr::map_df(files_gpx, get_track, file_type = "gpx", folder = folder)
# plot_track(transects_gpx, save = F)

# ------------- From mysti csv files ----------------------------------
# file_type = "csv"
folder = "2020oct"
files_csv <- list.files(file.path("survey_data/tracklines/transects/csv",folder))
transects_csv <- purrr::map_df(files_csv, get_track,  folder = folder) # %>% filter(date == "2020-11-25")
plot_track(transects_csv, save = F)
# ----------------------------------------------------------------------



oct <- get_track(file_name = "20201014-LineTransect_MB.gpx", file_type = "gpx", folder = "oct2020/")
nov <- get_track(file_name = "20201125-LineTransect_RB.gpx", file_type = "gpx", folder = "nov2020/")
sept <- get_track(file_name = "Track CeMoRe vessel  Started at 20200828 0814554 PDT CeMoRe vessel.csv", file_type = "csv", folder = "2020sept/")
plot_track(sept, save = F ,plot_by = "day")
# ---------- script to follow for line transect tracks-----------------------------
transect_dir <- "./survey_data/tracklines/transects/gpx/oct2020/"

files <- list.files(transect_dir)
#files <- "20201015-LineTransect_MB.gpx" # for one survey day/file only
transects_gpx <- purrr::map_df(files, get_track, file_type = "gpx", folder = "oct2020/") #%>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) #%>% dplyr::select(date, geometry)
# group_by(date) %>% 
#   summarize(do_union=FALSE) %>% st_cast("LINESTRING")



#---------trying to use code from gpx lines for csv line transect tracks from mysti----------------

# --------------------- test get_track for mystitracks ------------------
# t <- get_track(file_name = "20201015 0805583 PDT line transect mb.csv", folder = "mystitracks/oct2020/")

# test get_track for mystitracks
transect_dir <- "survey_data/tracklines/transects/csv/2020sept/"
files <- list.files(transect_dir)
#files <- "20201015-LineTransect_MB.gpx"

transects_mysti <- purrr::map_df(files, get_track, file_type = "csv", folder = "2020sept")# %>%
  # st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% #dplyr::select(date, geometry)
  # group_by(date) %>% 
  # summarize(do_union=FALSE) %>% st_cast("LINESTRING")
# --------------------- end test get_track for mystitracks ------------------

  
transect_dir <- "D:/CeMoRe/Analysis/cemore_analysis/survey_data/tracklines/transects/mystitracks/oct2020/"

filename <- list.files(transect_dir)

transects2 <- read.csv(paste0(transect_dir, filename)) %>% 
  dplyr::select(Latitude, Longitude) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  mutate(date = date)

transects <- rbind(transects1, transects2) %>% 
  group_by(date) %>% 
  summarize(do_union=FALSE) %>% st_cast("LINESTRING")


plot_track <- function(transects, save = TRUE, plot_by = "day", month = "sept"){
  n <- length(unique(transects$date))
  cols <- paste0(c(RColorBrewer::brewer.pal(8, "Set3")))[1:n] #[c(1:5,7,8,9)][1:n]
  coord <- coord_sf(xlim = c(-125.5, -123), ylim = c(48.1, 49.5), crs = sf::st_crs(4326)) 
  
  # one month, multiple survey days
  # if(n>1 & length(unique(month(transects$date)))==1) {
    if(plot_by =="day") {
      
    
    title <- paste0("CeMoRe survey trackline ", first_up(month), " ", year(transects$date))
    legend <- theme(legend.position = "right")
    # more than one survey day over multiple months
  } else if(n>1 & !length(unique(month(transects$date)))==1){
    title <- paste0("CeMoRe survey trackline ", year(transects$date))
    legend <- theme(legend.position = "right") 
    # only one survey day
  }else{
    title <- paste0("CeMoRe survey trackline ", month.abb[month(transects$date)], " ", day(transects$date), " ", year(transects$date))
    legend <- theme(legend.position = "none") 
  }
  
  # if(length(unique(month(transects$date)))==1){
    colour_by <- transects$date
    png_name <- paste0(month, unique(year(transects$date)), ".png")
    
    # png_name <- paste0(month.abb[unique(month(transects$date))], unique(year(transects$date)), ".png")
  # }else{
  #   colour_by <- month(transects$date)
  #   png_name <- paste0(unique(year(transects$date)), ".png")
  # }
     
  if(save){coastline <-  bc_shp}else{coastline <-  coast}
  plot <- ggplot() + geom_sf(data = coastline, fill = "light yellow") +
    geom_sf(data = transects, size = 0.5, aes(colour = as.factor(colour_by))) +
    coord +
    ggtitle(title) +
    scale_color_manual(values = c(cols), name = NULL) +
    guides(color = guide_legend(override.aes = list(size = 3))) +
    legend
  if(save) ggsave(paste0("output_maps/survey_track_", png_name))
  plot
}

plot_track(transects_mysti, save = FALSE)
# --------------------for whale tagging tracks-------------------
tagging_dir <- "./survey_data/tracklines/tagging"

files <- list.files(tagging_dir)
n <- length(files)
cols <- paste0(c(RColorBrewer::brewer.pal(n, "Set1")))
coord <- coord_sf(xlim = c(-125.5, -123), ylim = c(48.1, 49.5), crs = sf::st_crs(4326)) 

tagging <- purrr::map_df(files, get_track, transects = FALSE, folder = "sept2020/") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  group_by(date) %>% 
  summarize(do_union=FALSE) %>% st_cast("LINESTRING")
plot_track(tagging)

ggplot() + geom_sf(data = coast, fill = "light yellow") +
  geom_sf(data = tagging, size = 0.5, aes(colour = as.factor(date))) +
  coord +
  ggtitle("CeMoRe HW tracking Fall 2020") +
  scale_color_manual(values = c(cols), name = NULL) 
ggsave("output_maps/hw_tagging_fall2020.png")

#TO DO: map mysti tracks so can see only 'on effort' portions

#----- for future reference/attempts  -------------------------------------------

plots = map(x, ~ggplot() +
    geom_sf(data = .data[[.x]]) # haven't worked out purrr with ggplot yet
)

g + geom_sf(data = tss6_utm9, aes(fill = Names)) +
  coord + scale_fill_manual(values = RColorBrewer::brewer.pal(8L, "Pastel1")[c(1,2,3,4,5,6,8)])
#-------------------------------------------------------------------------------



#------------------
for(i in seq_along(effort$time_index..pdt.)){
  effort$time_diff <-   difftime(effort$time_index..pdt[i], effort$time_index..pdt.[i+1])
}

# plot gps trackline from survey against planned transects
transect_dir <- "D:/CeMoRe/Analysis/cemore_analysis/survey_data/gpx/transects/"

files <- list.files(transect_dir)
n <- length(files)
cols <- paste0(c(RColorBrewer::brewer.pal(n, "Set1")))
coord <- coord_sf(xlim = c(-125.5, -123), ylim = c(48.1, 49.5), crs = sf::st_crs(4326)) 

transects <- purrr::map_df(files, get_track, dir = transect_dir) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  group_by(date) %>% 
  summarize(do_union=FALSE) %>% st_cast("LINESTRING")
ggplot() + geom_sf(data = coast, fill = "light yellow") +
  geom_sf(data = transects, size = 0.5, aes(colour = as.factor(date))) +
  geom_sf(data = st_read("../../Design/cemore_design/shapefiles/transects/for Oct 2020 survey/t18km_iteration_1_can.shp"), linetype = 2, aes(colour = "planned transects")) +
  coord +
  ggtitle("CeMoRe transects Fall 2020") +
  scale_discrete_manual(aesthetics = c("colour", "linetype"), values = c(cols, "black"), name = NULL)
scale_linetype_manual(values = c(1, 2), name = NULL)
