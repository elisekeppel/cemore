

#' get_track
#' Get survey trackline for one day (pre-processed data). Used with
#' purrr::map to import data from all tracklines (mysticetus .csv files)
#' from all days of a survey. Reads in survey trackline data (as .gpx from
#' Bad Elf or .csv from Mysticetus) and formats as LINESTRING.
#'
#' @param dir
#' @param year
#' @param month
#' @param file_name Name of gpx or csv trackline file
#'
#' @return
#' @export
#'
#' @examples test <- get_track(file_name = "Track CeMoRe vessel  Started at 20210920 0903435 PDT CeMoRe vessel.csv", year = 2021, month = "09")
#' transect_dir <- "./survey_data/tracklines/transects/gpx/2021-08/"
#' files <- list.files(transect_dir)
#' transects_sept <- purrr::map_df(files, get_track, year = 2021, month = "08")
get_track <- function(file_name,
                      dir = NULL,
                      year,
                      month)  {
  if(!is.null(dir)){
    dir = dir
  }else{
    dir <- file.path( "C:/Users/keppele/Documents/GitHub/cemore/cemore/survey_data/tracklines/transects")
  }

  folder <- paste0(year, "-", month)
  if(substring(file_name[1], nchar(file_name[1]) -3, nchar(file_name[1])) == ".gpx") {
    t <- plotKML::readGPX(file.path(dir, "gpx", folder, file_name))[[4]][[1]][[1]] %>%
      dplyr::mutate(type = "gpx")
  }else{
    t <- read.csv(file.path(dir, "csv", folder, file_name)) %>%
      dplyr::rename(lat = Latitude, lon = Longitude, time = Time.Created..PDT.) %>% # NOTE - sometimes PDT, UTC seems to not always be exported correctly, missing date and hour
      dplyr::select(lat, lon, time) %>%
      dplyr::mutate(type = "mysti")
  }
  date <- as.POSIXct(substring(t$time[[1]], 1, 10))
  t$hour <- lubridate::hour(t$time)
  t %<>% sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    dplyr::mutate(date = date) %>%
    dplyr::select(time, date, type, geometry, hour) %>%
    dplyr::group_by(date, hour) %>%
    dplyr::summarize(do_union=FALSE) %>%
    sf::st_cast("LINESTRING")
}


#' Tidy transects
#' Read in gpx file of planned survey transects (as either route or track) and convert to sf LINESTRING object
#' @param file_name
#' @param dir
#' @param iteration
#' @param route true/false whether the object is a route (= linestring object)
#'
#' @return
#' @export
#'
#' @examples ## Not run:: test <- tidy_transects(file_name = "18km_full_2.gpx", iteration = 2, route = FALSE)
tidy_transects <- function(file_name,
                           dir = NULL,
                           depth_cutoff_object=NULL#,
                           # iteration,
                           # route = TRUE
)  {
  if(!is.null(dir)){
    dir = dir
  }else{
    dir <- file.path( "C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/survey_data/survey transects/gpx") # on oyster drive on small laptop
  }
  # if(is.null(depth_cutoff_object)) {
  #   d <- st_read("shapefiles/less_than_5m.shp") %>%
  #     st_transform(crs = 4326)
  # }else{
  #   d <- depth_cutoff_object
  # }
  # if(is.null(iteration)){
  #   iteration = substring(file_name, 11,12)
  # }
  # output <- map_df(t, ~{
  #   bind_rows(.x) %>%
  #     st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  #     summarize(do_union=FALSE) %>%
  #     st_cast("LINESTRING")
  #       })
  if(substr(file_name, 1,5) == 'track'){
    t <- plotKML::readGPX(file.path(dir,  file_name))[[4]]
  }else{
    t <- plotKML::readGPX(file.path(dir,  file_name))[[5]]
  }

  iterations <- data.frame(cbind(year = c(rep(2020,3), rep(2021, 11), rep(2022,3)),
                                 month_abb =c("Sep", "Oct", "Nov", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                                 iteration = c(1,2,3,6,7,8,9,10,11,12,13,14,15,16,17,18,19)))
  year <- case_when(
    grepl('2020', file_name) == T ~ '2020',
    grepl('2021', file_name) == T ~ '2021',
    grepl('2022', file_name) == T ~ '2022'
  )    # NOT WORKING

  month <- case_when(
    grepl('jan', file_name) == T ~ 1,
    grepl('feb', file_name) == T ~ 2,
    grepl('mar', file_name) == T ~ 3,
    grepl('apr', file_name) == T ~ 4,
    grepl('may', file_name) == T ~ 5,
    grepl('jun', file_name) == T ~ 6,
    grepl('jul', file_name) == T ~ 7,
    grepl('aug', file_name) == T ~ 8,
    grepl('sep', file_name) == T ~ 9,
    grepl('oct', file_name) == T ~ 10,
    grepl('nov', file_name) == T ~ 11,
    grepl('dec', file_name) == T ~ 12)
  month_abb <- month.abb[month]
  if(year == 2020 & month_abb == "Aug") month_abb <- "Sep"
  iteration <- iterations$iteration[which(iterations$year == year & iterations$month == month.abb[month])]


  output <- purrr::map_df(t, get_lines) %>%
    dplyr::mutate(year = year, month = month, month_abb = month_abb, iteration = iteration)# %>% st_difference(d)

  output
}

# if(grep('2020', "route18km_iteration_1_can_2020sept.gpx") == 1) T
#' get_lines
#' Bind together list of survey transect lines brought in as gpx (readGPX) into one
#' linestring object. Used with tidy_transects() to import multiple lines in
#' .gpx file.
#' @param x a list of survey transect lines imported from a .gpx file (readGPX()).
#'
#' @return
#' @export
#'
#' @examples
get_lines <- function(x){
  dplyr::bind_rows(x) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    dplyr::summarize(do_union=FALSE) %>%
    sf::st_cast("LINESTRING")
}

#' plot_track
#' Plot survey tracklines (from get_effort_lines(effort from processed survey data))
#'
#' @param transects LINESTRING object from get_effort_lines
#' @param save Logical. Whether or not to save plot.
#' @param coastline Coastline object (ie. bc_shp for high-resolution or
#'   coast for low-resolution)
#'
#' @return
#' @export
#'
#' @examples plot_track(transects, save = FALSE)

plot_track <- function(transects, save = FALSE, coast_file){
  n <- length(unique(transects$date))
  # cols <- paste0(c(RColorBrewer::brewer.pal(9, "Set3")))[c(1,3:5,7,8,9)][1:n]
  cols <- paste0(c(RColorBrewer::brewer.pal(12, "Paired")))
  coord <- coord_sf(xlim = c(-125.5, -123), ylim = c(48.1, 49.5), crs = sf::st_crs(4326))

  # one month, multiple survey days
  if(n>1 & length(unique(month(transects$date)))==1) {
    title <- paste0("CeMoRe survey trackline ", month.abb[month(transects$date)], " ", year(transects$date))
    legend <- theme(legend.position = "right")
    # more than one survey day over multiple months
  } else if(n>1 & !length(unique(month(transects$date)))==1){
    title <- paste0("CeMoRe survey trackline ", min(year(transects$date)),":",max(year(transects$date)))
    legend <- theme(legend.position = "right")
    # only one survey day
  }else{
    title <- paste0("CeMoRe survey trackline ", month.abb[month(transects$date)], " ", day(transects$date), " ", year(transects$date))
    legend <- theme(legend.position = "none")
  }

  if(length(unique(month(transects$date)))==1){
    colour_by <- transects$date
    png_name <- paste0(month.abb[unique(month(transects$date))], unique(year(transects$date)), ".png")
  }else{
    colour_by <- month_year
    png_name <- paste0(unique(year(transects$date)), ".png")
  }

  plot <- ggplot(data = transects) + geom_sf(data = coast_file, fill = "light yellow") +
    geom_sf(size = 0.5, aes(colour = as.factor(colour_by))) +
    coord +
    ggtitle(title) +
    scale_color_manual(values = c(cols), name = NULL) +
    guides(color = guide_legend(override.aes = list(size = 1.5))) +
    legend
  if(save) ggsave(paste0("output_maps/survey_track_", png_name))
  plot
}


#' get_obs_data
#'
#' @param data_folder
#'
#' @return
#' @export
#'
#' @examples data <- get_obs_data(data_folder = "sept2020/"); saveRDS(data, "survey_data/cemore_survey_data_oct2020.rds")
get_obs_data <- function(year, month){
  folder <- paste0(year, "-", month)
  dir <- file.path("survey_data","raw_data",folder, "observations")
  files <- list.files(dir)

  s <- list()
  for(i in seq_along(files)){
    s[[i]] <- read.csv(file.path(dir, files[[i]], "Sighting.csv"), header=TRUE, stringsAsFactors = FALSE)
    if(!is.null(s[[i]]$Incidential.Sighting)) s[[i]] %<>% dplyr::rename(Incidental.Sighting =Incidential.Sighting)
    s[[i]] %<>%
      dplyr::rename(time_index = names(s[[i]][grep("Index", names(s[[i]]))]),
                    time_local = names(s[[i]][grep("local", names(s[[i]]))])) %>%
      dplyr::mutate(sighting_distance = ifelse(is.na(Sgt.Dist..m.), Distance..m., Sgt.Dist..m.),
                    time_index = lubridate::ymd_hms(time_index),
                    time_local = lubridate::ymd_hms(time_local)) %>%
      dplyr::mutate_at(c('Photos', 'Incidental.Sighting', 'Sighting.Complete'), as.logical) %>%
      dplyr::mutate_at(c('Bearing', 'time_index', 'time_local', 'GPS.Pos', 'Sgt.Id', 'Horizon_Certainty',
                         'Reticle.Instr', 'Side', 'Obs', 'Species', 'Comments', 'Sgt.Pos', 'QA.QC.Comments'), as.character) %>%
      dplyr::filter(!Species == "######MISTAKE/CANCEL#####", !is.null(Species)) %>%
      dplyr::filter(Species %in% c("Humpback Whale",
                                   "Harbour Porpoise",
                                   "Dall's Porpoise",
                                   "Unidentified Porpoise",
                                   "Killer Whale - Southern Resident",
                                   "Southern Resident Killer Whale",
                                   "Killer Whale - Northern Resident",
                                   "Killer Whale - Transient",
                                   "Killer Whale - Unknown ecotype",
                                   "Grey Whale",
                                   "Fin Whale",
                                   "Minke Whale"))
  }

  sightings <- dplyr::bind_rows(s) %>%
    dplyr::mutate(date = lubridate::date(time_index))
  sightings %<>%  autofill_side()
  sightings$Bearing<- gsub("-", "", sightings$Bearing)
  sightings$Species<- gsub("Southern Resident Killer Whale", "Killer Whale - Southern Resident", sightings$Species)
  sightings$Species<- gsub("Unidentified Porpoise", "Unknown Porpoise", sightings$Species)
  sightings$Species<- gsub("Dall's Porpoise", "Dalls Porpoise", sightings$Species)
  sightings$Species = factor(sightings$Species, levels = c("Humpback Whale",
                                                           "Harbour Porpoise",
                                                           "Dalls Porpoise",
                                                           "Unknown Porpoise",
                                                           "Killer Whale - Northern Resident",
                                                           "Killer Whale - Southern Resident",
                                                           "Killer Whale - Transient",
                                                           "Killer Whale - Unknown ecotype",
                                                           "Grey Whale",
                                                           "Fin Whale",
                                                           "Minke Whale"))

  multispecies <- list()
  for(i in seq_along(files)){
    multispecies[[i]] <- read.csv(file.path(dir, files[[i]], "xMultiSpecies.csv"), header=TRUE, stringsAsFactors = FALSE)
    multispecies[[i]] %<>%
      dplyr::rename(time_index = names(multispecies[[i]][grep("Index", names(multispecies[[i]]))]),
                    time_local = names(multispecies[[i]][grep("local", names(multispecies[[i]]))]))
  }
  multispecies <- dplyr::bind_rows(multispecies)


  effort <- list()
  for(i in seq_along(files)){
    effort[[i]] <- read.csv(file.path(dir, files[[i]], "EffortEnv.csv"), header=TRUE, stringsAsFactors = FALSE)
    effort[[i]] %<>%
      dplyr::rename(time_index = names(effort[[i]][grep("Index", names(effort[[i]]))]),
                    time_local = names(effort[[i]][grep("local", names(effort[[i]]))])) %>%
      dplyr::mutate_at(c("time_index", "time_local", "Action", "Status", "Platform", "PORT.Observer",
                         "STBD.Observer", "Effort_Instrument", "Data.Recorder", "PORT.Visibility",
                         "STBD.Visibility", "Swell", "Glare", "Cloud.Cover", "Precipitation", "Comments",
                         "Locked.from.Editing", "QA.QC_Comments"), as.character)
  }
  effort <- dplyr::bind_rows(effort)

  c <- list()
  for(i in seq_along(files)){
    c[[i]] <- read.csv(file.path(dir, files[[i]], "zComments.csv"), header=TRUE, stringsAsFactors = FALSE)
    c[[i]] %<>%
      dplyr::rename(time_index = names(c[[i]][grep("Index", names(c[[i]]))]),
                    time_local = names(c[[i]][grep("local", names(c[[i]]))])) %>%
      dplyr::mutate_at(c("time_index", "time_local", "Position", "Notes", "Lock.from.Editing"), as.character)

  }
  comments <- dplyr::bind_rows(c)

  df <- list(effort, sightings, multispecies, comments)
  names(df) <- c("effort", "sightings", "multispecies", "comments")

  data_file <- file.path("survey_data","raw_data", "collated_rds", paste0("cemore_survey_raw_data_", year,"_", month, ".rds"))
  saveRDS(df, data_file)
  cat("Saving raw data .rds file")
  df
}

#' create_bb
#' Make a bounding box to clip plots by or create magnified inset
#' @param xmin
#' @param xmax
#' @param ymin
#' @param ymax
#'
#' @return
#' @export
#'
#' @examples
#' bb <- create_bb(-123.4, -123.1, 48.32, 48.5)
#' cropped_sf <- st_intersection(sf, bb)
create_bb<- function(xmin, xmax, ymin, ymax){
  raster::extent(xmin, xmax, ymin, ymax) %>%
    as("SpatialPolygons") %>%
    sf::st_as_sf(st_crs(4326))
}

#' get_effort_lines
#'
#' @param effort
#'
#' @return
#' @export
#'
#' @examples
#' dir <- file.path("OUTPUT FILES","dataEffort table")
#' effort <- read.delim(file.path(dir, paste0("dataEffortcemore_", year, "-",month, ".txt")))
#' effort_lines <- get_effort_lines(effort)
get_effort_lines <- function(effort){
  effort %<>%
    dplyr::filter(Status == "ON") %>%
    dplyr::select(Vessel,year, month, month_abb, day, TransectID, Latitude, Longitude, ONSEQ_ID, SurveyID, season, CloudCover, Beaufort=Bf,Visibility=Port.Vis,
                  Swell, Glare, Precip) %>%
    dplyr::mutate(Glare = ifelse(!Glare == "None", "y","n")) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    dplyr::group_by(Vessel, ONSEQ_ID, SurveyID, year, month, month_abb, day, TransectID, CloudCover, season, Beaufort,Visibility,
                    Swell, Glare, Precip) %>%
    dplyr::summarize(do_union=FALSE) %>%
    sf::st_cast("LINESTRING")
}

load_effort <- function(year, month, single_survey = T, vessel=NULL){
  dir <- file.path("C:/Users/KeppelE/Documents/CeMoRe/Analysis/cemore_analysis/OUTPUT FILES/dataEffort table")
  if(single_survey){
    effort <- read.delim(file.path(dir, paste0("cemore_Effort_", year,"_",month,".txt")))
  }else{
    effort_files <- list.files(dir)
    effort <- purrr::map_df(file.path(dir, effort_files), read.delim)
  }
  effort %<>% dplyr::mutate(year = year(GpsT),
                            month = month(GpsT),
                            day = day(GpsT),
                            month_abb = factor(month.abb[month], levels = month.abb[1:12]),
                            season = factor(dplyr::case_when(
                              month %in% c(1:3) ~ "Winter",
                              month %in% c(4:6) ~ "Spring",
                              month %in% c(7:9) ~ "Summer",
                              month %in% c(10:12)  ~ "Fall"
                            ), levels = c("Winter", "Spring", "Summer", "Fall"))) %>%
    rename(TransectID=Final.T.ID)

  if(!is.null(vessel)) effort %<>% filter(Vessel == vessel)

  effort$month_abb[which(effort$month_abb == "Aug" & effort$year == 2020)] <- "Sep"
  effort%<>% arrange(year, month)
  lev <- unique(effort$SurveyID )
  effort$SurveyID %<>% factor(levels = lev)

  effort
}

load_sightings <- function(year, month, single_survey = T, vessel=NULL){
  dir <- "C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/OUTPUT FILES/dataSightings_True Positions"
  month_abb <- month.abb[as.numeric(month)]
  if(single_survey){
    AP <- rgdal::readOGR(file.path(dir, paste0("cemore_Sightings", "_truePositions_WGS84_UTM9N_",year,"_", month,".shp")), verbose = F)
  }else{
    files <- list.files(path = dir, pattern = "\\.shp$")
    AP <- purrr::map(file.path(dir,files), rgdal::readOGR, verbose = F)
    AP <- do.call(rbind, AP)
    survey_title <- paste0("All CeMoRe surveys from Sep 2020 - ", survey_title)
  }

  ap_sf <- AP %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::filter(!is.na(PSD_nm), !SightedBy=="SHrushowy") %>%
    dplyr::mutate() %>%
    dplyr::transmute(SurveyID, Vessel=vessel,
                     year = lubridate::year(time_index), month = lubridate::month(time_index),
                     month_abb = factor(month.abb[month], levels = month.abb[1:12]),
                     Sgt_ID=paste(year,month,Sgt_ID,sep="-"),
                     SD_nm,
                     time_index,
                     Species = factor(Species, levels = c('Humpback Whale',
                                                          'Harbour Porpoise',
                                                          'Dalls Porpoise',
                                                          'Unknown Porpoise',
                                                          'Killer Whale - Northern Resident',
                                                          'Killer Whale - Southern Resident',
                                                          'Killer Whale - Transient',
                                                          'Killer Whale - Unknown ecotype',  # added for special account in Apr 2021
                                                          'Fin Whale',
                                                          "Grey Whale",
                                                          "Minke Whale")),
                     Group_Size=BestNumber,
                     PSD_nm, distance = units::set_units(PSD_nm * 1.852,"km"),
                     Beaufort = as.numeric(as.character(beauf)),
                     Glare = ifelse(!glare == "None", yes = "y",no = "n"),
                     Visibility = port_visib,
                     swell, precip, SightedBy, Reticle,
                     season = factor(dplyr::case_when(
      month %in% c(7:9) ~ "Summer",
      month %in% c(10:12)  ~ "Fall",
      month %in% c(1:3) ~ "Winter",
      month %in% c(4:6) ~ "Spring"), levels = c("Winter","Spring","Summer","Fall"))) %>%
    sf::st_transform(crs = sf::st_crs(4326)) %>%
    arrange(year, month)
  if(!is.null(vessel)) ap_sf %<>% filter(Vessel == vessel)

    lev <- unique(ap_sf$SurveyID)
    ap_sf$SurveyID %<>% factor(levels = lev)
  ap_sf

}

# utils

first_up <- function(x){
  paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
}

# all raw sightings in all surveys (including incidentals)
get_all_raw_sgt <- function(){
  dir <- file.path("c:/users/keppele/documents/cemore/analysis/cemore_analysis/survey_data","raw_data", "collated_rds")
  files <- list.files(path = dir)
  for(i in seq_along(files)){
    x <- readRDS(file.path(dir, files[i]))
    if(i==1){
      s <- x$sightings
    }else{
      s <- bind_rows(s,x$sightings) %>%
        mutate(Species = gsub(pattern="KW",replacement="Killer Whale",.$Species))
    }
  }
  s %>%
    mutate(Species = factor(Species, levels = c('Humpback Whale',
                                                'Harbour Porpoise',
                                                'Dalls Porpoise',
                                                'Unknown Porpoise',
                                                'Killer Whale - Northern Resident',
                                                'Killer Whale - Southern Resident',
                                                'Killer Whale - Transient',
                                                'Killer Whale - Unknown ecotype',  # added for special account in Apr 2021
                                                'Fin Whale',
                                                "Grey Whale",
                                                "Minke Whale")))
}
