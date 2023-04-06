

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
#' transects_sept <- purrr::map_df(files, get_track, year = 2021, month = "09")
get_track <- function(file_name,
                      dir = NULL,
                      year,
                      month)  {
  if(!is.null(dir)){
    dir = dir
  }else{
    dir <- file.path("C:\\Users\\KeppelE\\Documents\\CeMoRe\\Analysis\\cemore_analysis\\survey_data\\raw_data",year)
  }

  folder <- paste0(year, "-", month)
  if(substring(file_name[1], nchar(file_name[1]) -3, nchar(file_name[1])) == ".gpx") {
    t <- plotKML::readGPX(file.path(dir, folder, "badelf", file_name))[[4]][[1]][[1]] %>%
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
    dplyr::mutate(date = date, file=file_name) %>%
    dplyr::select(file, time, date, type, geometry, hour) %>%
    dplyr::group_by(date, hour,file) %>%
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
                           type="route",
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
  if(substr(file_name, 1,5) == 'track' | type== "track"){
    t <- plotKML::readGPX(file.path(dir,  file_name))[[4]]
  }else{
    t <- plotKML::readGPX(file.path(dir,  file_name))[[5]]
  }

  # surveys <- data.frame(cbind(year = c(rep(2020,3), rep(2021, 11), rep(2022,3)),
  #                                month_abb =c("Sep", "Oct", "Nov", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
  #                                iteration = c(1,2,3,6,7,8,9,10,11,12,13,14,15,16,17,18,19)))
  # year <- case_when(
  #   grepl('2020', file_name) == T ~ '2020',
  #   grepl('2021', file_name) == T ~ '2021',
  #   grepl('2022', file_name) == T ~ '2022'
  # )    # NOT WORKING
  #
  # month <- case_when(
  #   grepl('jan', file_name) == T ~ 1,
  #   grepl('feb', file_name) == T ~ 2,
  #   grepl('mar', file_name) == T ~ 3,
  #   grepl('apr', file_name) == T ~ 4,
  #   grepl('may', file_name) == T ~ 5,
  #   grepl('jun', file_name) == T ~ 6,
  #   grepl('jul', file_name) == T ~ 7,
  #   grepl('aug', file_name) == T ~ 8,
  #   grepl('sep', file_name) == T ~ 9,
  #   grepl('oct', file_name) == T ~ 10,
  #   grepl('nov', file_name) == T ~ 11,
  #   grepl('dec', file_name) == T ~ 12)
  # month_abb <- month.abb[month]
  # if(year == 2020 & month_abb == "Aug") month_abb <- "Sep"
  # iteration <- surveys$iteration[which(surveys$year == year & surveys$month == month.abb[month])]


  output <- purrr::map_df(t, get_lines) %>%
    dplyr::mutate(year = year, month = month, month_abb = month_abb)#, iteration = iteration)# %>% st_difference(d)

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
    group_by(date, file) %>%
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

plot_track <- function(transects, save = FALSE, coord = NULL){
  n <- length(unique(transects$date))
  # cols <- paste0(c(RColorBrewer::brewer.pal(9, "Set3")))[c(1,3:5,7,8,9)][1:n]
  cols <- c(paste0(c(RColorBrewer::brewer.pal(12, "Paired"))),"black")
  if(is.null(coord)){
  coord <- coord_sf(xlim = c(-125.5, -123), ylim = c(48.1, 49.5), crs = sf::st_crs(4326))
  }

  if(!exists("coast")){
    coast <- sf::st_read(dsn = "C:/Users/Keppele/Documents/GitHub/cemore/cemore/data", layer = "BC_coast_UTM9", quiet = T)
  }

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
    colour_by <- transects$file
    png_name <- paste0(month.abb[unique(month(transects$date))], unique(year(transects$date)), ".png")
  }else{
    colour_by <- paste0(transects$month,"_",transects$year)
    png_name <- paste0(unique(year(transects$date)), ".png")
  }

  plot <- ggplot(data = transects) + geom_sf(data = coast, fill = "light yellow") +
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
get_obs_data <- function(year, month, data.source = "cemore", vessel = "MB"){

  if(data.source == "cemore") main.dir <- "survey_data"
  if(data.source == "mmcp") main.dir <- "mmcp_data"

  folder <- paste0(year, "-", month,"/", vessel)
  dir <- file.path(main.dir, "raw_data",year,folder, "observations")
  files <- list.files(dir)

  s <- list()
  for(i in seq_along(files)){
    s[[i]] <- read.csv(file.path(dir, files[[i]], "Sighting.csv"), header=TRUE, stringsAsFactors = FALSE)
    if(!is.null(s[[i]]$Incidential.Sighting)) s[[i]] %<>% dplyr::rename(Incidental.Sighting =Incidential.Sighting)
    if(is.null(s[[i]]$Porpoise.Behaviour)) {
      df <- data.frame(matrix(ncol = 1, nrow = 0))
      colnames(df) <- c('Porpoise.Behaviour')
      s[[i]] <- bind_rows(s[[i]],df) }

    if(!is.null(s[[i]]$Porpoise.approaching)) s[[i]] %<>% dplyr::mutate(Porpoise.approaching = as.character(Porpoise.approaching))

    s[[i]] %<>%
      dplyr::rename(time_index = names(s[[i]][grep("Index", names(s[[i]]))]),
                    time_local = names(s[[i]][grep("local", names(s[[i]]))])) %>%
      dplyr::mutate(sighting_distance = ifelse(is.na(Sgt.Dist..m.), Distance..m., Sgt.Dist..m.),
                    time_index = lubridate::ymd_hms(time_index),
                    time_local = lubridate::ymd_hms(time_local)) %>%
      dplyr::mutate_at(c('Photos', 'Incidental.Sighting', 'Sighting.Complete'), as.logical) %>%
      dplyr::mutate_at(c('Bearing', 'time_index', 'time_local', 'GPS.Pos', 'Sgt.Id', 'Horizon_Certainty',
                         'Reticle.Instr', 'Side', 'Obs', 'Species', 'Comments', 'Sgt.Pos', 'QA.QC.Comments','Porpoise.Behaviour'), as.character) %>%
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
                                   "Minke Whale",
                                   "Pacific White-sided Dolphin"))
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
                                                           "Minke Whale",
                                                           "Pacific White-sided Dolphin"))

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

  folder <- file.path(main.dir,"raw_data", "collated_rds")
  if(!file.exists(folder)) dir.create(folder)

  if(data.source == "cemore") data_file <- file.path(paste0("cemore_survey_raw_data_", year,"_", month, ".rds"))
  if(data.source == "mmcp") data_file <- file.path(paste0("mmcp_survey_raw_data_", year,"_", month, ".rds"))

  saveRDS(df, file.path(folder, data_file))
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
    sf::st_as_sf(crs=4326)
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
    dplyr::select(Vessel,date,year, month, month_abb, day, GpsT, transect_no, status, TransectID, Latitude, Longitude, ONSEQ_ID, SurveyID, season, CloudCover, Beaufort=Bf,Visibility=Port.Vis,
                  Swell, Glare, Precip, Port.Obs, Stbd.Obs) %>%
    dplyr::mutate(date=lubridate::date(GpsT),
                  seasonYear = paste0(tolower(season), year),
                  seasonYear = factor(seasonYear, levels = unique(seasonYear))) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    dplyr::group_by(
      Vessel,
      ONSEQ_ID,
      SurveyID,
      date,
      year, month, month_abb, day, transect_no,
                    TransectID
                    , CloudCover, season, seasonYear, Beaufort, Visibility,
                    Swell, Glare, Precip, Port.Obs, Stbd.Obs,status
                    ) %>%
    dplyr::summarize(do_union=FALSE) %>%
    sf::st_cast("LINESTRING")

  effort %>% arrange(date) %>% mutate(length = st_length(geometry), length_km = as.numeric(units::drop_units(length))/1000)
}

load_effort <- function(year, month, single_survey = T, vessel=NULL,dir=NULL){
  if(is.null(dir)){dir <- file.path("C:/Users/KeppelE/Documents/CeMoRe/Analysis/cemore_analysis/OUTPUT FILES/dataEffort table")}else{dir=dir}
  if(single_survey){
    effort <- read.delim(file.path(dir, paste0("cemore_Effort_", year,"_",month,".txt")))
  }else{
    effort_files <- list.files(dir)
    effort <- purrr::map_df(file.path(dir, effort_files), read.delim)
  }
  effort %<>% dplyr::mutate(date = lubridate::date(GpsT),
                            year = year(GpsT),
                            month = month(GpsT),
                            day = day(GpsT),
                            month_abb = factor(month.abb[month], levels = month.abb[1:12]),
                            season = factor(dplyr::case_when(
                              month %in% c(1:3) ~ "Winter",
                              month %in% c(4:6) ~ "Spring",
                              month %in% c(7:9) ~ "Summer",
                              month %in% c(10:12)  ~ "Fall"
                            ), levels = c("Winter", "Spring", "Summer", "Fall")),
                            transect_no = as.numeric(gsub(SurveyID, "", Final.T.ID)),
                            status = ifelse(transect_no <100,"On Effort","In Transit")) %>%
    dplyr::rename(TransectID=Final.T.ID)

  if(!is.null(vessel)) effort %<>% filter(Vessel == vessel)

  effort$month_abb[which(effort$month_abb == "Aug" & effort$year == 2020)] <- "Sep"
  effort%<>% arrange(year, month)
  lev <- unique(effort$SurveyID )
  effort$SurveyID %<>% factor(levels = lev)

  effort
}

load_sightings <- function(year, month, single_survey = T, vessel=NULL,dir=NULL){
  if(is.null(dir)){dir <- "C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/OUTPUT FILES/dataSightings_True Positions"}else{dir=dir}
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
    dplyr::rename(Observer=SightedBy) %>%
    dplyr::mutate() %>%
    dplyr::transmute(SurveyID, Vessel=vessel,
                     date = lubridate::date(time_index),
                     year = lubridate::year(time_index), month = lubridate::month(time_index),
                     month_abb = factor(month.abb[month], levels = month.abb[1:12]),
                     Sgt_ID=paste(year,month,Sgt_ID,sep="-"),
                     TransectID=Final_T_ID,#paste(year,month,Final_T_ID,sep="-"),
                     speed,
                     SD_nm,
                     time_index,
                     Species =  tolower(as.character(Species)),
                     Group_Size=BestNumber,
                     PSD_nm, distance = PSD_nm * 1.852,
                     Beaufort = as.numeric(as.character(beauf)),
                     # Glare = ifelse(!glare == "None", yes = "y",no = "n"),
                     # Glare = ifelse(glare %in% -45:45, yes = "y",no="n"),
                     glare,
                     l_glare,
                     r_glare,
                     Visibility = port_visib,
                     swell, precip, Observer, Reticle, Bearing_R,
                     season = factor(dplyr::case_when(
                       month %in% c(7:9) ~ "Summer",
                       month %in% c(10:12)  ~ "Fall",
                       month %in% c(1:3) ~ "Winter",
                       month %in% c(4:6) ~ "Spring"), levels = c("Winter","Spring","Summer","Fall"))) %>%
    dplyr::mutate(Species = gsub(pattern="dalls",replacement="Dall's",.$Species)) %>%
    dplyr::mutate(Species = gsub(pattern="pacific",replacement="Pacific",.$Species)) %>%
    dplyr::mutate(Species = gsub(pattern="transient",replacement="Bigg's",.$Species),
           Species = factor(Species, levels =
                              c("humpback whale",
                                "harbour porpoise",
                                "Dall's porpoise" ,
                                "unknown porpoise",
                                "killer whale - northern resident",
                                "killer whale - southern resident",
                                "killer whale - Bigg's",
                                "killer whale - unknown ecotype",
                                "grey whale",
                                "fin whale",
                                "minke whale",
                                "Pacific white-sided dolphin"
                              ))) %>%
    sf::st_transform(crs = sf::st_crs(4326)) %>%
    dplyr::arrange(year, month)
  if(!is.null(vessel)) ap_sf %<>% filter(Vessel == vessel)

    lev <- unique(ap_sf$SurveyID)
    ap_sf$SurveyID %<>% factor(levels = lev)
  ap_sf

}

# utils

first_up <- function(x){
  paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
}

# get incidental sightings from raw data for one survey
get_incid <- function(single_survey=T,
                      Year=NULL,
                      Month=NULL,
                      include_hw_porps=F){
  if(single_survey){
    if(is.null(Year)) Year=year
    if(is.null(Month)) Month=month}
  x <- get_all_raw_sgt(single=single_survey, Year,Month)
  x %<>%
    filter(Incidental.Sighting == T)
  if(!include_hw_porps){
    x %<>% filter(!Species %like% c("humpback") & !Species %like% "porpoise")
  }
  x
}

# all raw sightings in all surveys (including incidentals)
get_all_raw_sgt <- function(single = T,
                            Year = NULL,
                            Month = NULL){
  dir <- file.path("c:/users/keppele/documents/cemore/analysis/cemore_analysis/survey_data","raw_data", "collated_rds")

  if(single){
    if(is.null(Year)) Year=year
    if(is.null(Month)) Month=month
    if(nchar(Month)==1) Month <- paste0("0",Month)

    x <- readRDS(file.path(dir,paste0("cemore_survey_raw_data_",Year,"_",Month,".rds")))
    s <- x$sightings
     }else{
    files <- list.files(path = dir)
    for(i in seq_along(files)){
      x <- readRDS(file.path(dir, files[i]))
      if(i==1){
        s <- x$sightings
      }else{
        s <- bind_rows(s,x$sightings)
      }
    }}
  s %>%
    mutate(Species = gsub(pattern="KW",replacement="Killer Whale",.$Species)) %>%
    mutate(Species = tolower(Species)) %>%
    mutate(Species = gsub(pattern="dalls",replacement="Dall's",.$Species)) %>%
    dplyr::mutate(Species = gsub(pattern="pacific",replacement="Pacific",.$Species)) %>%
    mutate(Species = gsub(pattern="transient",replacement="Bigg's",.$Species))%>%
    mutate(Species = factor(Species, levels = c("humpback whale",
                                                "harbour porpoise",
                                                "Dall's porpoise" ,
                                                "unknown porpoise",
                                                "killer whale - northern resident",
                                                "killer whale - southern resident",
                                                "killer whale - Bigg's",
                                                "killer whale - unknown ecotype",
                                                "grey whale",
                                                "fin whale",
                                                "minke whale",
                                                "Pacific white-sided dolphin")))
}

survey_summary <- function(single=T,
                           Year=year,
                           Month = month,
                           save=F
){

  if(!single) {
    ap_sf = all_ap_sf
    effort_lines = all_effort_lines
  }
  year <- Year
  month <- Month
  month_abb <- month.abb[as.numeric(month)]
  s <- ap_sf %>% as.data.frame()
  #---------------- for august 2020 -------------------------------------
  # filter for only Aug survey days
  if(year == 2020 & tolower(month_abb) == "aug"){
    s <- s %>% filter(day(time_index) %in% c(19,26,27,28))
  }
  #---------------- for september 2020 -----------------------------------------
  # filter for only survey days, not tagging days
  if(year == 2020 & tolower(month_abb) == "sep"){
    s <- s %>% filter(day(time_index) %in% c(2,3,10,13, 28)) # subtract the killer whale incidental sighting while not on effort... note 28th = Aug
  }

  summary <- list()

  # total count cetacean sightings; manually check any NA values
  summary[[1]] <- s %>% dplyr::group_by(SurveyID) %>% #Year = year(GpsT), Month = month(GpsT)
    dplyr::summarise(number_sightings = n(), number_individuals = sum(Group_Size))
  if(save) write.csv(summary[[1]] , paste0("output/sightings_summary/cemore_sightings_summary_", year, tolower(month_abb), ".csv"), row.names = FALSE)

  # counts by month and species
  summary[[2]] <- s %>%
    dplyr::group_by(SurveyID, Species) %>% #Year = year(GpsT), Month = month(GpsT)
    dplyr::summarise(number_sightings = n(), number_individuals = sum(Group_Size))
  if(save) write.csv(summary[[2]], paste0("output/sightings_summary/cemore_species_summary_", year, tolower(month), ".csv"), row.names = FALSE)
  rm(s)
  # count cetacean sightings by species by day
  # s %>%  dplyr::group_by(month(time_index), day(time_index), Species) %>% dplyr::summarise(number_sightings = n(), number_indivduals = sum(Group_Size))

# ------------- to summarise effort in field work -----------------------
  x <- effort_lines %>%  mutate(length=st_length(geometry)) %>%
    as.data.frame() %>% dplyr::select(-geometry) %>%
    dplyr::group_by(SurveyID, date, TransectID) %>% #Year = year(GpsT), Month = month(GpsT)
    dplyr::summarise(
      distance_km=round(sum(as.numeric(length))/1000,0))

  y <- x %>% dplyr::group_by(SurveyID) %>% #Year = year(GpsT), Month = month(GpsT)
    dplyr::summarise(TransectID = length(unique(x$TransectID)),
                     date = length(unique(x$date)),
                     distance_km=round(sum(distance_km)))
  y$TransectID <- paste0("Number of transects = ", y$TransectID)
  y$date <- paste0("Number of on-effort days = ", y$date)
  y$distance_km <- paste0("Total km surveyed = ", y$distance_km)
  x %<>% mutate(date=as.character(date), distance_km = as.character(distance_km))
  summary[[3]] <- rbind(x,y)
   # summary[[3]] <- effort_lines %>%  mutate(length=st_length(geometry)) %>%
   #  as.data.frame() %>% dplyr::select(-geometry) %>%
   #  dplyr::group_by(SurveyID) %>% #Year = year(GpsT), Month = month(GpsT)
   #  dplyr::summarise(on_effort_days = n_distinct(date),
   #                   transects = n_distinct(TransectID),
   #                   distance_km=round(sum(as.numeric(length))/1000,0))

  if(single){
    survey_data <- read.table(file.path("C:\\Users\\KeppelE\\Documents\\CeMoRe\\Analysis\\cemore_analysis/survey_data/tidy_data",year,tolower(month_abb),paste0("cemore_",year,tolower(month_abb),"_dataSurveyID.txt")))
   }else{
     k <- 0
     survey_data <- list()
     surveys %<>% filter(!iteration==99)
     for(i in unique(surveys$year)){
       # year <- surveys[[i]]$year
       for(j in unique(surveys %>% filter(year==i))$month){
         # month_abb <- j
         k <- k+1
         survey_data[[k]] <- read.table(file.path("C:\\Users\\KeppelE\\Documents\\CeMoRe\\Analysis\\cemore_analysis/survey_data/tidy_data",i,tolower(j),paste0("cemore_",i,tolower(j),"_dataSurveyID.txt")))
       }
     }
     survey_data <- suppressMessages(survey_data %>% reduce(full_join))
   }
  # ------------- to summarise overall details of field work -----------------------

  summary[[4]] <- survey_data %>% dplyr::select(SurveyID, Vessel_code, Date_Start_GMT, Date_End_GMT) %>%
    dplyr::mutate(Date_Start_GMT=lubridate::date(Date_Start_GMT),
                  Date_End_GMT=lubridate::date(Date_End_GMT),
                  start_month = lubridate::month(Date_Start_GMT),
                  end_month = lubridate::month(Date_End_GMT),
                  year=lubridate::year(Date_Start_GMT),
                  start_day = lubridate::day(Date_Start_GMT),
                  end_day = lubridate::day(Date_End_GMT),
                  field_days = (Date_End_GMT - Date_Start_GMT + 1) %>% as.numeric(),
                  firstday = paste(month.name[start_month], start_day, sep=" "),
                  lastday = paste(month.name[end_month], end_day, sep=" "))
names(summary) <- c("sightings","species","effort","survey")
  return(summary)
}
