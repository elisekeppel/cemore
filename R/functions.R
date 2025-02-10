

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
  coord <- coord_sf(xlim = c(-125.3, -123), ylim = c(48.1, 49.5), crs = sf::st_crs(4326))
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
    colour_by <- transects$date
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
  # plot
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

  if(data.source == "cemore") {
    main.dir <- "survey_data"
    folder <- paste0(year, "-", month,"/")
  }
  if(data.source == "mmcp") {
    main.dir <- "mmcp_data"
    folder <- paste0(year, "-", month,"/", vessel)
  }

  dir <- file.path(main.dir, "raw_data",year,folder, "observations")
  files <- list.files(dir)

  s <- list()
  for(i in seq_along(files)){
    s[[i]] <- read.csv(file.path(dir, files[[i]], "Sighting.csv"), header=TRUE, stringsAsFactors = FALSE)
    if(!is.null(s[[i]]$Incidential.Sighting)) s[[i]] %<>% dplyr::rename(Incidental.Sighting =Incidential.Sighting)
    if(!is.null(s[[i]]$Sgt.Dist..km.)) s[[i]] %<>% mutate(Sgt.Dist..km.=Sgt.Dist..km.*1000) %>% dplyr::rename(Sgt.Dist..m. = Sgt.Dist..km.)
    if(!is.null(s[[i]]$Psd..km.)) s[[i]] %<>% mutate(Psd..km.=Psd..km.*1000) %>% dplyr::rename(Psd..m. = Psd..km.)
    if(!is.null(s[[i]]$Distance..km.)) s[[i]] %<>% dplyr::rename(Distance..m. = Distance..km.) # %<>% mutate(Distance..km.=Distance..km.*1000) %>%

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

  # multispecies <- list()
  # for(i in seq_along(files)){
  #   multispecies[[i]] <- read.csv(file.path(dir, files[[i]], "xMultiSpecies.csv"), header=TRUE, stringsAsFactors = FALSE)
  #   multispecies[[i]] %<>%
  #     dplyr::rename(time_index = names(multispecies[[i]][grep("Index", names(multispecies[[i]]))]),
  #                   time_local = names(multispecies[[i]][grep("local", names(multispecies[[i]]))]))
  # }
  # multispecies <- dplyr::bind_rows(multispecies)


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

  # c <- list()
  # for(i in seq_along(files)){
  #   c[[i]] <- read.csv(file.path(dir, files[[i]], "zComments.csv"), header=TRUE, stringsAsFactors = FALSE)
  #   c[[i]] %<>%
  #     dplyr::rename(time_index = names(c[[i]][grep("Index", names(c[[i]]))]),
  #                   time_local = names(c[[i]][grep("local", names(c[[i]]))])) %>%
  #     dplyr::mutate_at(c("time_index", "time_local", "Position", "Notes", "Lock.from.Editing"), as.character)
  #
  # }
  # comments <- dplyr::bind_rows(c)

  df <- list(effort, sightings)#, multispecies, comments)
  names(df) <- c("effort", "sightings")#, "multispecies", "comments")

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
get_effort_lines <- function(effort, rare_obs=NULL, data.source="cemore"){
  if(is.null(effort$line.code)){
    effort$line.code <- "NA"
  }
  effort %<>%
    dplyr::filter(Status == "ON") %>%
    dplyr::select(Vessel, SurveyID, GPSIndex, GpsT, # ONSEQ_ID,
                                 date,year, month, month_abb, day, GpsT,
                                 # transect_no,
                  Status, TransectID,
                                 season,
                                 Beaufort=Bf,
                  Port.Vis, Stbd.Vis, Visib,
                  Port.Obs, Stbd.Obs,
                                 Swell, Glare, L.G.Limit, R.G.Limit, Precip, CloudCover,
                                 line.code,
                                 Latitude, Longitude) %>%
    arrange(GpsT) %>%

    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    dplyr::group_by(Vessel, SurveyID, # ONSEQ_ID,
                                   date, year, month, month_abb, #day, #GpsT,
                                   # transect_no,
                    Status, TransectID,
                                   season,
                                   Beaufort,
                    Port.Vis, Stbd.Vis, Visib,
                    # Port.Obs, Stbd.Obs,
                                   Swell, Glare,
                    # L.G.Limit, R.G.Limit,
                    Precip,
                    CloudCover,
                                   line.code
                    ) %>%
    dplyr::summarize(min.GPSInd=min(GPSIndex),max.GPSInd=max(GPSIndex), do_union=FALSE) %>%
    sf::st_cast("LINESTRING") %>% ungroup() %>% as.data.frame() %>%
    sf::st_as_sf()

  effort %<>% arrange(date, TransectID) %>% mutate(length = st_length(geometry), length_km = as.numeric(units::drop_units(length))/1000) %>%
    filter(length_km >0)

  effort$line.code <- 1:nrow(effort)

  # if(data.source=="cemore"){
  #   effort %<>% mutate(Visibility=ifelse(Visibility=="F","Moderate",Visibility))
  # }

  # if(!is.null(rare_obs)){
  #   effort %<>% mutate( # halve the effort when SH was observing as we have too few observations by them and time with them on effort to include
  #   length_km= case_when(
  #     (Port.Obs %in% c(rare_obs) | Stbd.Obs %in% c(rare_obs)) ~ length_km/2,
  #     !(Port.Obs %in% c(rare_obs) | Stbd.Obs %in% c(rare_obs)) ~ length_km
  #   )
  # )}
    return(effort)
}

load_effort <- function(year, month, single_survey = T, vessel=NULL,dir=NULL,data.source = "cemore"){
  if(is.null(dir)){dir <- file.path("C:/Users/KeppelE/Documents/CeMoRe/Analysis/cemore_analysis",paste0("OUTPUT FILES ",data.source),"dataEffort table")}else{dir=dir}
  if(single_survey){
    if(data.source=="cemore"){
      effort <- read.delim(file.path(dir, paste0(data.source,"_Effort_", year,"_",month,".txt")))
    }else{
      effort <- read.delim(file.path(dir, paste0(data.source,"_Effort_", year,"_",month,"_",vessel,".txt")))
    }
  }else{
    effort_files <- list.files(dir)
    # effort <- purrr::map_df(file.path(dir, effort_files), read.delim)
    effort <- purrr::map(file.path(dir, effort_files), read.delim)
    if(data.source=="mmcp")  effort <- do.call(rbind,effort)
    if(data.source=="cemore") effort <- purrr::map_df(effort, mutate, iteration=as.character(iteration))
  }
    effort %<>% dplyr::mutate(date = lubridate::date(GpsT))
    if(month<12){
      end_date <- lubridate::make_date(year=year, month=(as.numeric(month)+1))
    }else{
      end_date <- lubridate::make_date(year=year+1, month=as.numeric(1))
    }
    effort <- effort %>% filter(date<end_date)



  effort %<>% dplyr::mutate(
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
    transect_no = as.numeric(stringr::str_remove_all(Final.T.ID,SurveyID)), #, #Raw.T.ID
    status = ifelse(transect_no <100,"On Effort","In Transit")) %>%
    dplyr::mutate(TransectID=Final.T.ID) #paste(SurveyID,transect_no, sep="_"))
  if(!is.null(vessel)) effort %<>% filter(Vessel == vessel)

  # ##### EK edit ###########
  #Create final Visibility field that combines the STBD and PORT Visibility fields (using the worse one) # EK edit
  if(data.source=="cemore"){
    effort %<>% mutate(Port.Vis = case_when(
      Port.Vis == "F" ~ "Moderate",
      !Port.Vis == "F" ~ Port.Vis),
      Stbd.Vis = case_when(
        Stbd.Vis == "F" ~ "Moderate",
        !Stbd.Vis == "F" ~ Stbd.Vis
      ))
  }
  effort$Port.Vis <- factor(effort$Port.Vis , levels=c("R","P","Moderate","F", "G&E"))
  effort$Stbd.Vis <- factor(effort$Stbd.Vis , levels=c("R","P","Moderate","F", "G&E"))
  col <- c("Port.Vis","Stbd.Vis")

  v <- which(!effort$Port.Vis==effort$Stbd.Vis)

  effort$Visib <- effort$Port.Vis
  for(i in v){
    # if(sum(is.na(effort[i,col]))!=2){
    x <- which.min(as.numeric(c(effort[i,]$Port.Vis,effort[i,]$Stbd.Vis)))
    effort[i,]$Visib <- effort[i,col[x]] %>% as.character()
  }
  # View(effort[v,c("Port.Vis","Stbd.Vis","Visib")])
  # #################################

  if(length(effort$month_abb[which(effort$month_abb == "Aug" & effort$year == 2020)])>0){
    effort$month_abb[which(effort$month_abb == "Aug" & effort$year == 2020)] <- "Sep"}
  effort%<>% arrange(year, month)
  lev <- unique(effort$SurveyID )
  effort$SurveyID %<>% factor(levels = lev)
  effort
}

load_sightings <- function(year, month, single_survey = T, vessel=NULL,dir=NULL, data.source = "cemore"){
  if(is.null(dir)){dir <- paste0("C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/OUTPUT FILES ", data.source,"/dataSightings_True Positions")}else{dir=dir}
  month_abb <- month.abb[as.numeric(month)]
  if(single_survey){
    if(data.source=="cemore"){
      AP <- sf::st_read(file.path(dir, paste0(data.source,"_WGS84_UTM9N_",year,"_", month,".shp")))
    }else{
      AP <- sf::st_read(file.path(dir, paste0(data.source,"_Sightings_truePositions_WGS84_UTM9N_",year,"_", month,"_",vessel,".shp")))
    }
  }else{
    # if(data.source=="cemore"){
    files <- list.files(path = dir, pattern = "\\.shp$")
    # }else{files <- list.files(path = dir, pattern = "\\.shp$")
    AP <- purrr::map(file.path(dir,files), sf::st_read)
    AP <- do.call(rbind, AP)
    survey_title <- paste0("All ", data.source," surveys to`` - ", survey_title)
  }

  AP %<>%     sf::st_as_sf() %>%
    dplyr::mutate(date = lubridate::date(tind_date))
  if(month<12){
    end_date <- lubridate::make_date(year=year, month=(as.numeric(month)+1))
  }else{
    end_date <- lubridate::make_date(year=year+1, month=as.numeric(1))
  }
  AP %<>% filter(date<end_date)

  ap_sf <- AP %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::filter(!is.na(PSD_nm), !SightedBy=="SHrushowy") %>%
    dplyr::rename(Observer=SightedBy) %>%
    dplyr::mutate() %>%
    dplyr::transmute(SurveyID=paste0(data.source,"_", sid),
                     Vessel=vessel,
                     date,
                     year = lubridate::year(tind_date), month = lubridate::month(tind_date),
                     month_abb = factor(month.abb[month], levels = month.abb[1:12]),
                     GPSIndex,
                     Sgt_ID=paste(year,month,Sgt_ID,sep="-"),
                     ONSEQ_ID=onseq_id,
                     TransectID=Final_T_ID,#paste(year,month,Final_T_ID,sep="-"),
                     speed,
                     SD_nm,
                     # time=tind_time,
                     time_index=as.POSIXct(paste(tind_date, tind_time), format= "%Y-%m-%d %H_%M_%S", tz="America/Los_Angeles"),
                     gps_time=as.POSIXct(paste(gpsdate, gpstime), format= "%Y-%m-%d %H_%M_%S", tz="UTC"),
                     Species =  tolower(as.character(Species)),
                     Group_Size=BestNumber,
                     PSD_nm, distance = PSD_nm * 1.852,
                     Beaufort = as.numeric(as.character(beauf)),
                     # Glare = ifelse(!glare == "None", yes = "y",no = "n"),
                     # Glare = ifelse(glare %in% -45:45, yes = "y",no="n"),
                     glare,
                     l_glare,
                     r_glare,
                     port_visib,
                     stbd_visib,
                     Visibility = case_when(
                       Side=="Port" ~  port_visib,
                       Side=="Starboard" ~ stbd_visib),
                     swell, precip, Observer, Reticle, Bearing_R,
                     cloudcover,
                     season = factor(dplyr::case_when(
                       month %in% c(7:9) ~ "Summer",
                       month %in% c(10:12)  ~ "Fall",
                       month %in% c(1:3) ~ "Winter",
                       month %in% c(4:6) ~ "Spring"), levels = c("Winter","Spring","Summer","Fall")),
                     seasonYear = paste0(tolower(season), year),
                     seasonYear = factor(seasonYear, levels = unique(seasonYear))) %>%
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

  if(data.source=="cemore"){
    ap_sf %<>% mutate(Visibility=ifelse(Visibility=="F","Moderate",Visibility))
    ap_sf %<>% mutate(port_visib=ifelse(port_visib=="F","Moderate",port_visib))
    ap_sf %<>% mutate(stbd_visib=ifelse(stbd_visib=="F","Moderate",stbd_visib))
  }
  ap_sf$port_visib <- factor(ap_sf$port_visib , levels=c("P","Moderate", "G&E"))
  ap_sf$stbd_visib <- factor(ap_sf$stbd_visib , levels=c("P","Moderate", "G&E"))
  col <- c("port_visib","stbd_visib")

  v <- which(!ap_sf$port_visib==ap_sf$stbd_visib)

  ap_sf$Visib <- ap_sf$Visibility
  for(i in v){
    # if(sum(is.na(effort[i,col]))!=2){
    x <- which.min(as.numeric(c(ap_sf[i,]$port_visib,ap_sf[i,]$stbd_visib)))
    ap_sf[i,]$Visib <- st_drop_geometry(ap_sf)[i,col[x]] %>% as.character()
  }

  lev <- unique(ap_sf$SurveyID)
  ap_sf$SurveyID %<>% factor(levels = lev)


  #########################################################
  # KW ecotype updates as determined by Lisa Spaven after looking at mysti comments and BCCSN records
  # updated May 21, 2024
  #########################################################
  # if(!single_survey){
    # ap_sf[which(ap_sf$time_index=="2021-03-02 15:38:02"),]$Species <- "killer whale - Bigg's"
    # ap_sf[which(ap_sf$time_index=="2021-10-13 13:25:26"),]$Species <- "killer whale - southern resident"
    # ap_sf[which(ap_sf$time_index=="2022-04-27 09:53"),]$Species <- "killer whale - Bigg's" ### NOT YET ON EFFORT FOR THE DAY

    # update kw ecotypes
    # nov 28, 2024

  # 2021
  if(year==2021){
    if(month==1){
      ap_sf[which(ap_sf$time_index=="2021-01-20 14:30:09"),]$Species <- "killer whale - southern resident"
      ap_sf[which(ap_sf$time_index=="2021-01-20 14:58:26"),]$Species <- "killer whale - southern resident"
    }else if(month==3){
      ap_sf[which(ap_sf$time_index=="2021-03-02 15:38:02"),]$Species <- "killer whale - Bigg's"
    }else if(month==10){
      ap_sf[which(ap_sf$time_index=="2021-10-13 13:25:26"),]$Species <- "killer whale - southern resident"
    }else if(month==11){
      ap_sf[which(ap_sf$time_index=="2021-11-18 15:39:52"),]$Species <- "killer whale - southern resident"
    }
  }else if(year==2022){
    if(month==1){
      ap_sf[which(ap_sf$time_index=="2022-01-22 13:33:51"),]$Species <- "killer whale - southern resident"
    }else if(month==6){
      ap_sf[which(ap_sf$time_index=="2022-06-11 10:49:20"),]$Species <- "killer whale - Bigg's"
    }else if(month==8){
      ap_sf[which(ap_sf$time_index=="2022-08-23 17:03:11"),]$Species <- "killer whale - Bigg's"
    }else if(month==10){
      ap_sf[which(ap_sf$time_index=="2022-10-12 11:18:51"),]$Species <- "killer whale - Bigg's"
    }
  }
  # ap_sf[which(ap_sf$time_index=="2022-04-26 11:52:53"),]$Species <- STILL UNKNOWN
  # ap_sf[which(ap_sf$time_index=="2022-04-27 09:53:X"),]$Species <- "killer whale - Bigg's" INCIDENTAL ### NOT YET ON EFFORT FOR THE DAY
  # ap_sf[which(ap_sf$time_index=="2022-07-25 15:23:xx"),]$Species <- STILL UNKNOWN
  # ap_sf[which(ap_sf$time_index=="2022-10-14 09:47:xx"),]$Species <- STILL UNKNOWN

  ap_sf

}

# utils

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
first_up <- function(x){
  paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
}

# get incidental sightings from raw data for one survey
get_incid <- function(single_survey=T,
                      Year=NULL,
                      Month=NULL,
                      include_hw_porps=F){
    if(is.null(Year)) Year=year
    if(is.null(Month)) Month=month
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
                           save=F,
                           data.source="cemore",
                           Vessel=NULL
){
  if(data.source == "cemore") main.dir <- "survey_data"
  if(data.source == "mmcp") main.dir <- "mmcp_data"

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
  if(save) write.csv(summary[[1]] , paste0("output_", data.source,"/sightings_summary/",data.source,"_sightings_summary_", year, tolower(month_abb), ".csv"), row.names = FALSE)

  # counts by month and species
  summary[[2]] <- s %>%
    dplyr::group_by(year, month, season, SurveyID, Species) %>% #Year = year(GpsT), Month = month(GpsT)
    dplyr::summarise(number_sightings = n(), number_individuals = sum(Group_Size))
  if(save) write.csv(summary[[2]], paste0("output_", data.source,"/sightings_summary/",data.source,"_species_summary_", year, tolower(month), ".csv"), row.names = FALSE)
  # rm(s)
  # count cetacean sightings by species by day
  # s %>%  dplyr::group_by(month(time_index), day(time_index), Species) %>% dplyr::summarise(number_sightings = n(), number_indivduals = sum(Group_Size))

  # ------------- to summarise effort in field work -----------------------
  x <- as.data.frame(effort_lines)
  if(nrow(x[which(x$year == 2020 & x$month == 8),])>0){
    x[which(x$year == 2020 & x$month == 8),]$month <- 9}
  if(nrow(x[which(x$year == 2022 & x$month == 5),])>0){
    x[which(x$year == 2022 & x$month == 5),]$month <- 4}

  y <- x %>%
    dplyr::select(-geometry) %>%
    dplyr::group_by(SurveyID, year, month, season) %>%
    dplyr::summarise(transects = length(unique(TransectID)),
                     days = length(unique(date)),
                     distance_km=round(sum(length_km)))

  # y$transects <- paste0("Number of transects = ", y$transects)
  # y$days <- paste0("Number of on-effort days = ", y$days)
  # y$distance_km <- paste0("Total km surveyed = ", y$distance_km)
  # x %<>% mutate(date=as.character(date), distance_km = as.character(distance_km))
  summary[[3]] <- y #rbind(x,y)
  # summary[[3]] <- effort_lines %>%  mutate(length=st_length(geometry)) %>%
  #  as.data.frame() %>% dplyr::select(-geometry) %>%
  #  dplyr::group_by(SurveyID) %>% #Year = year(GpsT), Month = month(GpsT)
  #  dplyr::summarise(on_effort_days = n_distinct(date),
  #                   transects = n_distinct(TransectID),
  #                   distance_km=round(sum(as.numeric(length))/1000,0))

  if(single){
    if(data.source=="cemore"){
      survey_data <- read.table(file.path("C:\\Users\\KeppelE\\Documents\\CeMoRe\\Analysis\\cemore_analysis",main.dir,"tidy_data",year,tolower(month_abb),paste0(data.source,"_",year,tolower(month_abb),"_dataSurveyID.txt")))
    }else{
      survey_data <- read.table(file.path("C:\\Users\\KeppelE\\Documents\\CeMoRe\\Analysis\\cemore_analysis",main.dir,"tidy_data",year,tolower(month_abb),Vessel,paste0(data.source,"_",year,tolower(month_abb),"_",Vessel,"_dataSurveyID.txt")))
    }
  }else{
    k <- 0
    survey_data <- list()
    # if(!is.null(Vessel)) surveys %<>% filter(vessel %in% Vessel)
    surveys %<>% filter(!iteration==99, vessel == "MB")
    for(i in unique(surveys$year)){
      # year <- surveys[[i]]$year
      for(j in unique((surveys %>% filter(year==i))$month)){
        # month_abb <- j
        # if(data.source=="cemore"){
          k <- k+1
          survey_data[[k]] <- read.table(file.path("C:\\Users\\KeppelE\\Documents\\CeMoRe\\Analysis\\cemore_analysis",main.dir,"tidy_data",i,tolower(j),paste0(data.source,"_",i,tolower(j),"_dataSurveyID.txt")))
        # }else{
        #   vessels <- list.dirs(file.path(main.dir,"tidy_data",i,tolower(j)),full.names=F)
        #   vessels <- vessels[!vessels %in% c("")]
        #   for(v in vessels){
        #     k <- k+1
        #     survey_data[[k]] <- read.table(file.path("C:\\Users\\KeppelE\\Documents\\CeMoRe\\Analysis\\cemore_analysis",main.dir,"tidy_data",i,tolower(j),v,paste0(data.source,"_",i,tolower(j),"_", v,"_dataSurveyID.txt")))
        #
        #   }
        # }
      }
    }
    survey_data <- map_df(survey_data, rbind)
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
  summary[[5]] <- s %>% group_by(Species) %>% summarise("# Sightings" = length(Group_Size), "# Individuals" = sum(Group_Size))

  names(summary) <- c("tot_sgt_by_survey","tot_sgt_by_sp&survey","effort","survey", "tot_sgt_by_sp")
  if(data.source == "cemore"){
    saveRDS(summary, paste0("C:/users/keppele/documents/cemore/analysis/cemore_analysis/summary_",data.source,"/",data.source,year,"_",month,".rds"))
  }else{
    saveRDS(summary, paste0("C:/users/keppele/documents/cemore/analysis/cemore_analysis/summary_",data.source,"/",data.source,year,"_",month,".rds"))
  }
  return(summary)
}
