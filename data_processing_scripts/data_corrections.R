# ----------------------------------------------------------------------
# -------------------- DATA CORRECTIONS - ALL MONTHS -------------------
# ----------------------------------------------------------------------
if(is.null(data$effort$Beaufort)){
  data$effort <- data$effort %>% rowwise() %>% mutate(Beaufort= min(PORT.Beaufort, STBD.Beaufort))
  data$effort$PORT.Beaufort <- NULL
  data$effort$STBD.Beaufort <- NULL
}

data$effort$Action %<>% tolower() %>% first_up()
data$sightings <- autofill_side(data$sightings) # fill either side or bearing (l/r) if missing
data$sightings;autofill_side(data$sightings)
data$effort$Swell[which(data$effort$Swell == "")] <- "No swell"
data$effort$Swell[which(data$effort$Swell == "None")] <- "No swell"

data$effort %<>% mutate(gsub("<25%", "0%-25%", Cloud.Cover))
data$effort %<>% mutate(iteration = iteration)

if(!survey$vessel == "FR") data$effort$Franklin.Hut <- NA
#data$effort[which(!is.na(data$effort$Transect.ID)),] %<>% mutate(Transect.ID = paste(iteration, Transect.ID, sep = "-"))


if(nrow(data$effort[which(data$effort$Status == "ON EFFORT" & data$effort$Data.Recorder==""),])>0){
  data$effort[which(data$effort$Status == "ON EFFORT" & data$effort$Data.Recorder==""),]$Data.Recorder <- "unknown"
}

# if(nrow(data$sightings[which(nchar(data$sightings$time_index)>19),])>0){
data$sightings$time_index <- substr(data$sightings$time_index, 1, 19)
data$sightings$time_local <- substr(data$sightings$time_local, 1, 19)

data$effort$time_index <- substr(data$effort$time_index, 1, 19)
data$effort$time_local <- substr(data$effort$time_local, 1, 19)
# }

##########################################
# Trackline mysti csv files field name changed
if(data.source == "cemore") track.path <- file.path(getwd(),main.dir, "tracklines", "transects", "csv", paste0(year, "-", month))
if(data.source == "mmcp") track.path <- file.path(getwd(), main.dir, "tracklines", "transects", "csv", paste0(year, "-", month),vessel)
track.files <- list.files(track.path,include.dirs = FALSE,full.names = TRUE)
track.list <- as.list(track.files)
for(i in 1:length(track.list)) { #upload all GPS tables
  x <- read.csv(track.files[i], header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","na","n/a","N/A",""))

  # fix jul 16:18 2023 where there were two utc time fields and no pdt time fields exported in mysti
  if(!is.null(x$Time.Created..UTC..1)) {
    x <- x %>%
      mutate(
        Time.Created..UTC..1 = as.POSIXct(
          strptime(Time.Created..UTC..1, format = "%Y-%m-%dT%H:%M:%S"), tz = "GMT"),
        Time.Created..UTC..1 = with_tz(Time.Created..UTC..1, "America/Vancouver"),
        Time.Created..UTC..1 = format(Time.Created..UTC..1, "%Y-%m-%dT%H:%M:%S")
      ) %>%
      dplyr::rename(Time.Created..PDT. = Time.Created..UTC..1)
  }

  if(!is.null(x$Distance.From.Previous..km.)) {
    x$Distance.From.Previous..m. <- x$Distance.From.Previous..km.*1000
    x$Distance.From.Previous..km. <- NULL
  }
  if(!is.null(x$Speed.Over.Ground..kph.)) {
    x$Speed.Over.Ground..kts. <- x$Speed.Over.Ground..kph./1.852
    x$Speed.Over.Ground..kph. <- NULL
  }
  if(!is.null(x$Distance.From.Start..km.)) {
    x$Distance.From.Start..m. <- x$Distance.From.Start..km.*1000
    x$Distance.From.Start..km. <- NULL
  }
  #################################
  # When importing tracklines from the Integrated Effort csv:
  if(!is.null(x$TrkTime..PDT.)){
    x$Time.Created..PDT. <- x$TrkTime..PDT.
    x$Time.Created..UTC. <- with_tz(strptime(x$TrkTime..PDT., format = "%Y-%m-%dT%H:%M:%S", tz = "America/Vancouver"), tz = "GMT")
    x$TrkTime..PDT. <- NULL
  }
  if(!is.null(x$TrkTime..PST.)) {
    x$Time.Created..PST. <- x$TrkTime..PST.
    x$TrkTime..PST. <- NULL
  }
  if(!is.null(x$TrkLatitude)) {
    x$Latitude <- x$TrkLatitude
    x$TrkLatitude <- NULL
  }
  if(!is.null(x$TrkLongitude)) {
    x$Longitude <- x$TrkLongitude
    x$TrkLongitude <- NULL
  }
  if(!is.null(x$HeadingPlatTrue..T.)) {
    x$Course.Over.Ground..T. <- x$HeadingPlatTrue..T.
    x$HeadingPlatTrue..T. <- NULL
  }
  if(!is.null(x$PlatformSpeed..kts.)) {
    x$Speed.Over.Ground..kts. <- x$PlatformSpeed..kts.
    x$PlatformSpeed..kts. <- NULL
  }
  if(!is.null(x$TrkDist..m.)) {
    x$Distance.From.Previous..m. <- x$TrkDist..m.
    x$TrkDist..m. <- NULL
  }
  #################################

  write.csv(x, track.files[i], row.names=F)
}

# ----------------------------------------------------------------------
# -------------------- DATA CORRECTIONS - BY SURVEY --------------------
# ----------------------------------------------------------------------

cat("Applying data corrections")

# ----------------------------------------------------------------------
# -------------------------------- 2025 --------------------------------
# ----------------------------------------------------------------------

if(year == 2025 & tolower(month_abb) == "oct"){
  data$effort[which(data$effort$PORT.Observer=="Stacey Hrushowy"),]$PORT.Observer <- "Johanna Withers"
  data$effort[which(data$effort$STBD.Observer=="Stacey Hrushowy"),]$STBD.Observer <- "Johanna Withers"
  data$effort[which(data$effort$Data.Recorder=="Stacey Hrushowy"),]$Data.Recorder <- "Johanna Withers"
  # Comment : "2025-10-30T10:32:58.6 Might be below 5 knots" - Nope, fine :)
  # Comment : "2025-10-30T10:37:52.9 Nope" - We aborted after 5 min, but maybe fine to keep in
  # data$effort[which(data$effort$time_index=="2025-10-30T10:32:58"),]$Comments <- "Way too bouncy - aborted" - maybe it's fine to keep that 5 minutes in?
  # Comment : "2025-10-30T15:01:48.4 Fix time stamp -  - Went off effort at 15:00 because of vessel wake" - Fixed
  data$effort[which(data$effort$time_index=="2025-10-30T15:01:48"),]$time_local <- "2025-10-30T15:01:22"
  data$effort[which(data$effort$time_index=="2025-10-30T15:01:48"),]$time_index <- "2025-10-30T15:01:22"

  data$sightings[which(data$sightings$Obs=="Stacey Hrushowy"),]$Obs <- "Johanna Withers"

}

if(year == 2025 & tolower(month_abb) == "sep"){
  data$effort[which(data$effort$time_index=="2025-09-09T14:26:09"),]$Transect.ID <- 35
  data$effort[which(data$effort$time_index=="2025-09-11T15:20:52"),]$time_local <- "2025-09-11T15:19:11"
  data$effort[which(data$effort$time_index=="2025-09-11T15:20:52"),]$time_index <- "2025-09-11T15:19:11"
  data$effort[which(data$effort$PORT.Observer=="Stacey Hrushowy"),]$PORT.Observer <- "Tasli Shaw"
  data$effort[which(data$effort$STBD.Observer=="Stacey Hrushowy"),]$STBD.Observer <- "Tasli Shaw"

  data$sightings[which(data$sightings$Obs=="Stacey Hrushowy"),]$Obs <- "Tasli Shaw"
}
if(year == 2025 & tolower(month_abb) == "aug"){
  data$effort[which(data$effort$time_index=="2025-08-08T16:20:55"),]$time_local <- "2025-08-08T16:20:55"
}
if(year == 2025 & tolower(month_abb) == "jun"){
  # Remove non-effort
  data$effort <- data$effort[which(!data$effort$Transect.ID %in% 98),]

  data$effort[which(data$effort$Status=="CLOSING"),]$Transect.ID <- NA
  # data$effort[which(data$effort$Status=="OFF"),]$Transect.ID <- NA
  data$effort[which(data$effort$time_index=="2025-06-16T07:35:29"),]$Left.Glare.Limit <- 55
  data$effort[which(data$effort$time_index=="2025-06-16T07:35:29"),]$Right.Glare.Limit <- 75

  data$sightings <- data$sightings %>% filter(!time_index=="2025-06-18 09:22:30")

  # Fix cemore slow speed segments
  # adjust timing of going off effort to fix slow speed segment that still had on effort status (transect 41)
  data$effort[which(data$effort$time_index=="2025-06-23T09:22:47"),]$time_local <- "2025-06-23T09:15:32"
  data$effort[which(data$effort$time_index=="2025-06-23T09:22:47"),]$time_index <- "2025-06-23T09:15:32"

  # add 2 rows to effort data to create off effort segment due to quickly reduced speed section (transect 40)
  x <- nrow(data$effort)+1
  x2 <- nrow(data$effort)+2

  data$effort[x,] <- data$effort[which(data$effort$time_index == "2025-06-23T11:59:40"),] # another line changing effort status to off
  data$effort[x,]$time_index <- "2025-06-23T11:10:12"
  data$effort[x,]$time_local <- "2025-06-23T11:10:12"

  data$effort[x2,] <- data$effort[which(data$effort$time_index == "2025-06-23T11:00:01"),] # previous timestamp with the proper effort info (ie. observers, conditions, etc.)
  data$effort[x2,]$time_local <- "2025-06-23T11:15:02"
  data$effort[x2,]$time_index <- "2025-06-23T11:15:02"
  data$effort[x2,]$Action <-  "Changing effort status"

  # fix non-cemore slow speed segments
  # 99 effort
  data$effort[which(data$effort$time_index=="2025-06-13T13:24:09"),]$time_local <- "2025-06-13T13:22:30"
  data$effort[which(data$effort$time_index=="2025-06-13T13:24:09"),]$time_index <- "2025-06-13T13:22:30"

  # tid 506
  # add 2 rows to effort data to create off effort segment due to quickly reduced speed section (transect 40)
  x <- nrow(data$effort)+1
  x2 <- nrow(data$effort)+2

  data$effort[x,] <- data$effort[which(data$effort$time_index == "2025-06-16T12:13:56"),] # another line changing effort status to off
  data$effort[x,]$time_index <- "2025-06-16T08:25:05"
  data$effort[x,]$time_local <- "2025-06-16T08:25:05"

  data$effort[x2,] <- data$effort[which(data$effort$time_index == "2025-06-16T08:34:49"),] # previous timestamp with the proper effort info (ie. observers, conditions, etc.)
  data$effort[x2,]$time_local <- "2025-06-16T08:38:35"
  data$effort[x2,]$time_index <- "2025-06-16T08:38:35"
  data$effort[x2,]$Action <-  "Changing effort status"

  data$effort[which(data$effort$time_index=="2025-06-16T08:27:16"),]$Status <- "OFF EFFORT"
  data$effort[which(data$effort$time_index=="2025-06-16T08:34:49"),]$Status <- "OFF EFFORT"

  # tid 1112
  data$effort[which(data$effort$time_index=="2025-06-18T13:59:41"),]$time_local <- "2025-06-18T13:13:05"
  data$effort[which(data$effort$time_index=="2025-06-18T13:59:41"),]$time_index <- "2025-06-18T13:13:05"
  data$effort[which(data$effort$time_index=="2025-06-18T13:31:03"),]$Status <- "OFF EFFORT"
  data$effort[which(data$effort$time_index=="2025-06-18T13:31:03"),]$Transect.ID <- NA

  # tid 5758
  # add 2 rows to effort data to create off effort segment due to quickly reduced speed section
  x <- nrow(data$effort)+1
  x2 <- nrow(data$effort)+2

  data$effort[x,] <- data$effort[which(data$effort$time_index == "2025-06-17T18:36:00"),] # another line changing effort status to off
  data$effort[x,]$time_index <- "2025-06-17T16:11:13"
  data$effort[x,]$time_local <- "2025-06-17T16:11:13"

  data$effort[x2,] <- data$effort[which(data$effort$time_index == "2025-06-17T16:02:37"),] # previous timestamp with the proper effort info (ie. observers, conditions, etc.)
  data$effort[x2,]$time_local <- "2025-06-17T16:14:23"
  data$effort[x2,]$time_index <- "2025-06-17T16:14:23"
  data$effort[x2,]$Action <-  "Changing effort status"



}

# ----------------------------------------------------------------------
# -------------------------------- 2024 --------------------------------
# ----------------------------------------------------------------------

if(year == 2024 & tolower(month_abb) == "dec"){
  data$effort[which(data$effort$Action=="Changing effort status" & data$effort$Status == "ON EFFORT" & is.na(data$effort$Transect.ID)),]$Transect.ID <- 39
}
if(year == 2024 & tolower(month_abb) == "oct"){
  data$effort[which(data$effort$STBD.Visibility=="Excellent/GoodModerate"),]$STBD.Visibility <- "Excellent/Good"
  data$effort[which(data$effort$PORT.Visibility=="Excellent/GoodModerate"),]$PORT.Visibility <- "Excellent/Good"
}
if(year == 2024 & tolower(month_abb) == "aug"){
  data$effort <- data$effort[which(!data$effort$time_index=="2024-08-15T14:53:32"),]
  data$effort <- data$effort[which(!data$effort$time_index=="2024-08-15T15:02:20"),]
  data$effort[which(data$effort$time_index=="2024-08-15T09:24:27"),]$Action <- "Changing effort status"
  data$effort[which(data$effort$time_index=="2024-08-15T09:24:27"),]$Status <- "OFF"
  data$effort[which(data$effort$time_local == "2024-08-16T10:42:28"),]$Transect.ID <- 3

  data$sightings[which(is.na(data$sightings$time_local)),]$time_local <- data$sightings[which(is.na(data$sightings$time_local)),]$time_index
}
if(year == 2024 & tolower(month_abb) == "jun"){
  data$effort[which(data$effort$time_local==""),]$time_local <- data$effort[which(data$effort$time_local==""),]$time_index
  data$effort[which(data$effort$time_index=="2024-06-18T12:30:37"),]$time_local <- "2024-06-18T12:30:37"
  data$effort[which(data$effort$time_index=="2024-06-17T10:39:40"),]$Beaufort <- 2 # Stace covering data, Lisa down w migraine, continuing transect that had bf=2

  data$sightings[which(data$sightings$time_index=="2024-06-12 17:18:58"),]$time_local <- "2024-06-12 7:18:58"
  data$sightings[which(data$sightings$time_index=="2024-06-16 11:45:08"),]$time_local <- "2024-06-16 11:45:08"
  data$sightings[which(data$sightings$time_index=="2024-06-12 17:18:58"),]$time_local <- "2024-06-12 17:18:58"
  data$sightings[which(data$sightings$time_index=="2024-06-16 11:23:32"),]$time_local <- "2024-06-16 11:23:32"
}
if(year == 2024 & tolower(month_abb) == "mar"){
  # use badelf to recover lost gps from March 18, 2024 ~15:53 to 17:33
  data$effort[which(data$effort$time_index == "2024-03-21T14:14:02"),]$time_local <- "2024-03-21T14:14:02"
  data$effort[which(data$effort$time_index == "2024-03-18T13:01:40"),]$time_local <- "2024-03-18T13:01:40"
  data$effort[which(data$effort$Left.Glare.Limit==-27),]$Right.Glare.Limit <- 0

  # march 26, 2024 - appears someone tried to update bf to 1 and instead overwrote transect id
  data$effort[which(data$effort$time_index == "2024-03-26T13:52:52"),]$Beaufort <- 1
  data$effort[which(data$effort$time_index == "2024-03-26T13:52:52"),]$Transect.ID <- 3

  data$sightings[which(is.na(data$sightings$time_local)),]$time_local <- data$sightings[which(is.na(data$sightings$time_local)),]$time_index
  data$sightings[which(data$sightings$time_index == "2024-03-26 12:10:50"),]$time_local <- "2024-03-26 12:10:50"
}

# ----------------------------------------------------------------------
# -------------------------------- 2023 --------------------------------
# ----------------------------------------------------------------------

if(year == 2023 & tolower(month_abb) == "nov"){
  data$effort[which(data$effort$time_index == "2023-11-21T11:40:15"),]$time_local <- "2023-11-21T11:40:15"
  data$effort[which(data$effort$time_index == "2023-11-28T09:43:09"),]$time_local <- "2023-11-28T09:43:09"
  data$effort[which(data$effort$time_index == "2023-11-28T10:43:42"),]$time_local <- "2023-11-28T10:43:42"
  data$effort <- data$effort[which(!data$effort$time_local == "2023-11-20T14:18:06"),]
  data$effort %<>% arrange(time_index)
}
if(year == 2023 & tolower(month_abb) == "sep"){
  data$effort[nrow(data$effort) +1,] <- c("2023-09-12T10:43:53",
                                          "2023-09-12T10:43:53",
                                          "Changing effort status",
                                          "OFF EFFORT", "",
                                          "Fujinon_ManyberriesBow", "", "", "7x50 Fujinons", "",
                                          "", "", "", # BF, VIS
                                          "", "", "", "", #  swell, glare
                                          "", "", "", FALSE, "",   # cloud... comments
                                          rep("", 3))
  data$effort %<>% mutate(iteration = 29)
  data$effort %<>% arrange(time_index)
  data$sightings[which(is.na(data$sightings$time_local)),]$time_local <- data$sightings[which(is.na(data$sightings$time_local)),]$time_index
  data$sightings[which(data$sightings$time_index == "2023-09-12 10:37:18"),]$time_local <- "2023-09-12 10:37:18"
}
if(year == 2023 & tolower(month_abb) == "jul"){
  data$effort[which(data$effort$time_index == "2023-07-20T14:26:30"),]$time_local <- "2023-07-20T14:26:30"
  data$effort[which(data$effort$time_index == "2023-07-19T15:41:18"),]$Status <- "OFF EFFORT"
  data$effort[which(data$effort$time_index == "2023-07-19T15:42:54"),]$Status <- "ON EFFORT"
  data$effort <- data$effort %>%
    mutate(new = as.POSIXct(strptime(time_index, format = "%Y-%m-%dT%H:%M:%S"), tz = "GMT"))
  data$effort[which(day(data$effort$new) %in% c(16:18)),]$time_index <-
    data$effort[which(day(data$effort$new) %in% c(16:18)),]$new %>% with_tz("America/Vancouver") %>%
    format("%Y-%m-%dT%H:%M:%S")
  data$effort[which(day(data$effort$new) %in% c(16:18)),]$time_local <-
    data$effort[which(day(data$effort$new) %in% c(16:18)),]$time_index
  data$effort <- data$effort %>% select(-new)

  data$sightings[which(is.na(data$sightings$time_local)),]$time_local <- data$sightings[which(is.na(data$sightings$time_local)),]$time_index
  data$sightings[which(data$sightings$time_index=="2023-07-16 20:19:12"),]$Distance..m. <- data$sightings[which(data$sightings$time_index=="2023-07-16 20:19:12"),]$Distance..m./1000
  # fix jul 16:18 2023 where there were two utc time fields and no pdt time fields exported in mysti
  data$sightings <- data$sightings %>%
    mutate(new = as.POSIXct(strptime(time_index, format = "%Y-%m-%d %H:%M:%S"), tz = "GMT"))
  data$sightings[which(day(data$sightings$new) %in% c(16:18)),]$time_index <-
    data$sightings[which(day(data$sightings$new) %in% c(16:18)),]$new %>% with_tz("America/Vancouver") %>%
    format("%Y-%m-%d %H:%M:%S")
  data$sightings[which(day(data$sightings$new) %in% c(16:18)),]$time_local <-
    data$sightings[which(day(data$sightings$new) %in% c(16:18)),]$time_index
  data$sightings <- data$sightings %>% select(-new)
}
if(year == 2023 & tolower(month_abb) == "feb"){
  data$effort[which(data$effort$PORT.Visibility == "Fair (CCG)"),]$PORT.Visibility <- "Moderate"
  data$effort[which(data$effort$STBD.Visibility == "Fair (CCG)"),]$STBD.Visibility <- "Moderate"
}
if(year == 2023 & tolower(month_abb) == "jan"){
  data$sightings$Reticle.Instr <- "Fujinon_FranklinMI"
  data$effort$Platform <- "Fujinon_FranklinMI"
  data$effort$iteration <- 1001
  data$effort[which(data$effort$time_index == "2023-01-21T10:10:30"),]$time_local <- "2023-01-21T10:10:30"
  data$effort[which(data$effort$time_index == "2023-01-18T16:15:32"),]$time_local <- "2023-01-18T16:14:44"
  data$effort[which(data$effort$time_index == "2023-01-18T16:15:32"),]$time_index <- "2023-01-18T16:14:44"
  t <- read.csv("survey_data/tracklines/transects/csv/2023-01/Vehicle   Track CeMoRe vessel  Started at 20230116 PST.csv") %>%
    filter(lubridate::date(Time.Created..PST.)=="2023-01-16") %>%
    write.csv("survey_data/tracklines/transects/csv/2023-01/Vehicle   Track CeMoRe vessel  Started at 20230116 PST.csv", row.names = F)
  t <- read.csv("survey_data/tracklines/transects/csv/2023-01/Vehicle   Track CeMoRe vessel  Started at 20230117 PST.csv") %>%
    filter(lubridate::date(Time.Created..PST.)=="2023-01-17") %>%
    write.csv("survey_data/tracklines/transects/csv/2023-01/Vehicle   Track CeMoRe vessel  Started at 20230117 PST.csv", row.names = F)
}

# ----------------------------------------------------------------------
# -------------------------------- 2022 --------------------------------
# ----------------------------------------------------------------------

if(year == 2022 & tolower(month_abb) == "oct"){
  data$effort[which(data$effort$time_index == "2022-10-13T13:13:07"),]$Transect.ID <- 28
  data$effort[which(data$effort$time_index == "2022-10-13T13:35:01"),]$Transect.ID <- 28
  data$effort[which(data$effort$time_index == "2022-10-13T13:46:53"),]$Transect.ID <- 27
  data$effort[which(data$effort$time_index == "2022-10-13T13:46:53"),]$Precipitation <- "Smoke"
}
if(year == 2022 & tolower(month_abb) == "sep"){
  data$effort[which(data$effort$time_index == "2022-09-20T11:33:13"),]$Transect.ID <- 2
}
if(year == 2022 & tolower(month_abb) == "aug"){
  req.conditions <- c("Platform","Transect.ID","Effort_Instrument","PORT.Visibility","Beaufort","STBD.Visibility","Swell","Glare","Cloud.Cover","Precipitation")
  data$effort[which(data$effort$time_index == "2022-08-24T11:24:46"),req.conditions] <- data$effort[which(data$effort$time_index == "2022-08-24T11:00:14"),req.conditions]
}
if(year == 2022 & tolower(month_abb) == "jul"){
  data$effort[which(data$effort$time_index == "2022-07-19T14:35:46"),]$time_local <- "2022-07-19T14:35:46"
}
if(year == 2022 & tolower(month_abb) == "apr"){
  data$effort[which(data$effort$Platform == "Fujinon_TanuMonkey"),]$Platform <-  "Fujinon_MBBow"
  data$effort[which(data$effort$time_index == "2022-04-26T15:07:13"),]$Glare <- "None"
  data$effort[which(data$effort$time_index == "2022-04-26T11:55:25"),]$PORT.Observer <- "CMcMillan"  #speed slowed, so data are correct to go off effort for a minute
  data$effort[which(data$effort$time_index == "2022-04-26T11:55:25"),]$STBD.Observer <- "EKeppel"  #speed slowed, so data are correct to go off effort for a minute
  data$effort[which(data$effort$time_index == "2022-04-26T11:55:25"),]$Transect.ID <-  4 # but have to restate transect id when back on effort
  data$effort[which(data$effort$time_index == "2022-04-26T11:55:25"),]$PORT.Visibility <-  "Excellent/Good"
  data$effort[which(data$effort$time_index == "2022-04-26T11:55:25"),]$Beaufort <-  4
  data$effort[which(data$effort$time_index == "2022-04-26T11:55:25"),]$STBD.Visibility <-  "Excellent/Good"
  data$effort[which(data$effort$time_index == "2022-04-26T11:55:25"),]$Cloud.Cover <-  "50% to 75%"
  data$sightings[which(date(data$sightings$time_index) == "2022-04-25"),]$Incidental.Sighting <- T
  data$sightings[which(data$sightings$Reticle.Instr == "Fujinon_TanuMonkey"),]$Reticle.Instr <- "Fujinon_MBBow"
}
if(year == 2022 & tolower(month_abb) == "mar"){
  data$effort[which(data$effort$PORT.Observer == "SHrushowy"),]$PORT.Observer <-  "JHildering"
  data$effort[which(data$effort$STBD.Observer == "SHrushowy"),]$STBD.Observer <-  "JHildering"
  data$effort[which(data$effort$Data.Recorder == "SHrushowy"),]$Data.Recorder <-  "JHildering"
  data$effort[which(data$effort$PORT.Visibility == "Fair (CCG)"),]$PORT.Visibility <-  "Fair"
  data$effort[which(data$effort$STBD.Visibility == "Fair (CCG)"),]$STBD.Visibility <-  "Fair"
  data$effort[which(data$effort$PORT.Visibility == "Moderate"),]$PORT.Visibility <-  "Fair"
  data$effort[which(data$effort$STBD.Visibility == "Moderate"),]$STBD.Visibility <-  "Fair"
  data$effort[which(data$effort$Platform == "Fujinon_MBBow"),]$Platform <-  "Fujinon_TanuMonkey"
  data$effort[which(data$effort$time_index == "2022-03-25T15:07:57"),]$Transect.ID <-  1
  data$effort[which(data$effort$time_index == "2022-03-25T15:28:23"),]$Action <-  "Weather update"
  data$effort[which(data$effort$time_index == "2022-03-28T17:38:00"),]$Beaufort <-  2
  data$sightings[which(data$sightings$Sgt.Id == "S1"),]$Species <-  "Humpback Whale"
}

# ----------------------------------------------------------------------
# -------------------------------- 2021 --------------------------------
# ----------------------------------------------------------------------

if(year == 2021 & tolower(month_abb) == "nov"){
  data$effort[which(data$effort$time_index == "2021-11-17T14:05:22"),]$Right.Glare.Limit <-  0
}
if(year == 2021 & tolower(month_abb) == "oct"){
  data$effort[which(data$effort$time_index == "2021-10-16T16:56:55"),]$Status <-  "OFF EFFORT"
  data$sightings[which(data$sightings$time_index == "2021-10-14 09:28:22"),]$Incidental.Sighting <- T
  data$sightings[which(data$sightings$time_index == "2021-10-14 10:55:09"),]$Incidental.Sighting <- T
  data$sightings[which(data$sightings$Species == "Killer Whale - Southern Resident"),]$Species <- "Killer Whale - Unknown ecotype"
}
if(year == 2021 & tolower(month_abb) == "aug"){
  data$sightings[which(as.character(data$sightings$date) %in% c("2021-08-23",
                                                                "2021-08-24",
                                                                "2021-08-26",
                                                                "2021-08-27",
                                                                "2021-08-28",
                                                                "2021-08-30")),]$Incidental.Sighting <- T
  data$sightings[which(data$sightings$Sgt.Id == "S1"),]$Species <- "Killer Whale - Southern Resident"
  data$sightings[which(data$sightings$Sgt.Id == "S1"),]$Comments <- "spread out all around Everest - noted to SRKiller Whale team"
}
if(year == 2021 & tolower(month_abb) == "jul"){
  data$sightings[which(data$sightings$time_index == "2021-07-09 17:29:12"),]$Species <-  "Killer Whale - Transient"
}
if(year == 2021 & tolower(month_abb) == "jun"){
  data$sightings[which(data$sightings$time_index == "2021-06-07 10:51:08"),]$Incidental.Sighting <-  T
  data$sightings[which(data$sightings$time_index == "2021-06-07 12:27:44"),]$Incidental.Sighting <-  T
  data$sightings[which(data$sightings$time_index == "2021-06-08 15:00:15"),]$Incidental.Sighting <-  T
  data$sightings[which(data$sightings$time_index == "2021-06-04 14:18:08"),]$Reticles <-  0.75
  data$sightings[which(data$sightings$time_index == "2021-06-04 13:01:20"),]$Reticles <-  1
}
if(year == 2021 & tolower(month_abb) == "may"){
  # add 1 row to effort data to end transect 30 (between 30 and 31 where we didn't stop while surveying in the traffic lanes on May 17, 2021)
  x <- nrow(data$effort)+1
  data$effort[x,] <- data$effort[which(data$effort$time_index == "2021-05-17T10:00:11"),] # another line changing effort status to off
  data$effort[x,]$time_index <- "2021-05-17T09:53:02"
  data$effort[x,]$time_local <- "2021-05-17T09:53:02"

  data$sightings[which(data$sightings$time_index == "2021-05-15 17:01:09"),]$Incidental.Sighting <-  T
}
if(year == 2021 & tolower(month_abb) == "apr"){
  data$effort <- data$effort %>% filter(!Action == "")
  data$effort[which(data$effort$Transect.ID == 37),]$Cloud.Cover <- "100%"
  data$effort[which(data$effort$Transect.ID == 37),]$Precipitation <- "Light Rain"
  data$effort[which(data$effort$time_index == "2021-04-23T15:08:48.6"),]$time_index <- "2021-04-23T15:07:47"
  data$effort[which(data$effort$time_local == "2021-04-23T15:08:48.6"),]$time_local <-  "2021-04-23T15:07:47"
}
if(year == 2021 & tolower(month_abb) == "mar"){
  data$effort <- data$effort %>% filter(!Action == "")
  data$effort[which(data$effort$time_index == "2021-03-03T15:34:24.9"),]$time_index <-  "2021-03-03T15:32:01"
  data$effort[which(data$effort$time_local == "2021-03-03T15:34:24.9"),]$time_local <-  "2021-03-03T15:32:01"
  data$effort[which(data$effort$Status =="CLOSING"),]$Status <- "OFF EFFORT"
}
#temporary only; TO DO: fix 2021-01-22T10:05:23.4 entry - see comments
if(surveyid == "cemore_2021jan"){
  data$effort %<>% filter(!time_index == "2021-01-23T10:36:38", !Action == "NANA")
  # data$effort[which(data$effort$time_index == "2021-01-23T10:36:38"),]$Action <- "2021-01-20T16:17:15"

  data$effort[which(data$effort$time_index == "2021-01-20T16:18:21"),]$time_local <- "2021-01-20T16:17:15"
  data$effort[which(data$effort$time_index == "2021-01-20T16:18:21"),]$time_index <- "2021-01-20T16:17:15"
  data$effort[which(data$effort$time_index == "2021-01-22T13:45:32"),]$time_local <- "2021-01-22T13:45:08"
  data$effort[which(data$effort$time_index == "2021-01-22T13:45:32"),]$time_index <- "2021-01-22T13:45:08"
  # change 1 row to effort data to end transect 30 (between 30 and 31 where we didn't stop mysti effort while transitting to transect 31 Jan 22, 2021)
  data$effort[which(data$effort$time_index == "2021-01-22T10:05:23"),]$Status <-  "OFF EFFORT"
  data$effort[which(data$effort$time_index == "2021-01-22T10:05:23"),]$time_local <- "2021-01-22T09:49:47"
  data$effort[which(data$effort$time_index == "2021-01-22T10:05:23"),]$time_index <- "2021-01-22T09:49:47"


  data$sightings[which(data$sightings$time_index == "2021-01-20 14:30:09"),]$Reticles <- 0.9
}

# ----------------------------------------------------------------------
# -------------------------------- 2020 --------------------------------
# ----------------------------------------------------------------------

if(year == 2020 & tolower(month_abb) == "nov"){  # TO DO - EDIT PLATFORM TO RBflybridge
  data$effort[which(data$effort$time_index == "2020-11-23T09:52:18.1"),]$time_local <-  "2020-11-23T09:52:18.1"
  data$effort[which(data$effort$time_index == "2020-11-23T13:28:27.4"),]$Action <-  "Weather update"
  data$effort[which(data$effort$time_index == "2020-11-25T09:58:42.1"),]$Status <-  "OFF EFFORT"

  #--------------------------
  # add in Transect.ID
  ind <- which(with(data$effort, c(FALSE, Status[-1L] != Status[-length(Status)]))) #each time Status switches
  on <- which(data$effort$Status=="ON EFFORT") #all ON effort records
  ON.start <- c(1, on[which(on %in% ind)]) #each time ON effort segment begins
  n <- length(ON.start)

  data$effort$Transect.ID <- NA
  data$effort$Transect.ID[ON.start] <- c(seq(1:n))
  data$effort$Transect.ID[which(data$effort$Status == "ON EFFORT")] %<>% fill()
  #--------------------------

  data$effort[which(data$effort$Transect.ID == 1),]$Platform <- "RBFly_sitting"
  data$effort[which(is.na(data$effort$Transect.ID)),]$Platform <- "RBFly_sitting"
  data$effort[which(!data$effort$Transect.ID %in% c(1, NA)),]$Platform <- "RBFly_standing"
  data$effort[which(data$effort$time_index == "2020-11-23T13:37:54.0"),]$time_local <-  "2020-11-23T13:37:27.0"
  data$effort[which(data$effort$time_index == "2020-11-23T13:37:54.0"),]$time_index <-  "2020-11-23T13:37:27.0"

  data$sightings$Reticle.Instr <- "Fujinon_RBFly"
}

if(year == 2020 & tolower(month_abb) == "oct"){
  #--------------------------
  # add in Transect.ID
  ind <- which(with(data$effort, c(FALSE, Status[-1L] != Status[-length(Status)]))) #each time Status switches
  on <- which(data$effort$Status=="ON EFFORT") #all ON effort records
  ON.start <- c(1, on[which(on %in% ind)]) #each time ON effort segment begins
  n <- length(ON.start)

  data$effort$Transect.ID <- NA
  data$effort$Transect.ID[ON.start] <- c(seq(1:n))
  data$effort$Transect.ID[which(data$effort$Status == "ON EFFORT")] %<>% fill()
  #--------------------------

  # combine separate bf in first half of survey with overall bf in second half
  data$effort <- data$effort %>% rowwise() %>% mutate(Beaufort=case_when(
    is.na(Beaufort) ~ min(PORT.Beaufort, STBD.Beaufort),
    !is.na(Beaufort) ~ Beaufort))
  data$effort$PORT.Beaufort <- NULL
  data$effort$STBD.Beaufort <- NULL

  data$effort[which(data$effort$Platform == "Fujinon_RBbridge"),]$Platform <- "MBBow"
  data$effort[which(data$effort$Status == "CLOSING"),]$Status <- "OFF EFFORT"
  data$effort$Beaufort[74] <- 1 # rest of that day Beaufort was 1 for all effort entries

  data$sightings[which(data$sightings$time_index == "2020-10-15 11:24:38.7"),]$Species <-  "Killer Whale - Southern Resident"
  data$sightings[which(data$sightings$time_index == "2020-10-20 07:58:05.7"),]$Species <-  "Harbour Porpoise"
  data$sightings[which(data$sightings$time_index == "2020-10-15 16:05:38"),]$Bearing <-  "43l"
  data$sightings[which(data$sightings$time_index == "2020-10-15 15:44:12.9"),]$Bearing <-  "41r"
  data$sightings[which(data$sightings$time_index == "2020-10-19 16:48:13.1"),]$Incidental.Sighting <-  T
}

if(year == 2020 & tolower(month_abb) == "sep"){
  # add in Transect.ID
  ind <- which(with(data$effort, c(FALSE, Status[-1L] != Status[-length(Status)]))) #each time Status switches
  on <- which(data$effort$Status=="ON EFFORT") #all ON effort records
  ON.start <- c(1, on[which(on %in% ind)]) #each time ON effort segment begins
  n <- length(ON.start)

  data$effort$Transect.ID <- NA
  data$effort$Transect.ID[ON.start] <- c(seq(1:n))
  data$effort$Transect.ID[which(data$effort$Status == "ON EFFORT")] %<>% fill()

  data$sightings[which(data$sightings$Reticles == 200),]$Distance..m. <- 200
  data$sightings[which(data$sightings$Reticles == 200),]$sighting_distance <- 200
  data$sightings[which(data$sightings$Reticles == 200),]$Reticles <- NA
  data$sightings <- data$sightings %>% filter(!Incidental.Sighting == F | !is.na(Bearing),
                                              !is.na(Reticles) | !is.na(Distance..m.), as.character(date) %in% c("2020-09-02", "2020-09-03", "2020-09-10", "2020-09-13", "2020-08-28"))
  data$effort <- data$effort %>% filter(as.character(date(time_index)) %in% c("2020-09-02", "2020-09-03", "2020-09-10", "2020-09-13", "2020-08-28"))
  data$effort[which(data$effort$time_index == "2020-08-28T12:17:31.7"),]$Action <-  "Weather update"
  data$effort[which(data$effort$time_index == "2020-08-28T12:17:31.7"),]$Action <-  "Weather update"

  data$effort[which(lubridate::ymd_hms(data$effort$time_local) %>% substr(12,16) == "15:46"),]$PORT.Observer <- "CMcMillan"
  data$effort[which(lubridate::ymd_hms(data$effort$time_local) %>% substr(12,16) == "15:46"),]$STBD.Observer <- "LSpaven"
  data$effort[which(lubridate::ymd_hms(data$effort$time_local) %>% substr(12,16) == "15:46"),]$PORT.Visibility <- "Excellent/Good"
  data$effort[which(lubridate::ymd_hms(data$effort$time_local) %>% substr(12,16) == "15:46"),]$STBD.Visibility <- "Excellent/Good"
  data$effort[which(lubridate::ymd_hms(data$effort$time_local) %>% substr(12,16) == "15:46"),]$Beaufort <- 1
  data$effort[which(lubridate::ymd_hms(data$effort$time_local) %>% substr(12,16) == "15:46"),]$Data.Recorder <- "SHrushy"
  data$effort[which(lubridate::ymd_hms(data$effort$time_local) %>% substr(12,16) == "15:46"),]$Swell <- "No swell"
  data$effort[which(lubridate::ymd_hms(data$effort$time_local) %>% substr(12,16) == "15:46"),]$Glare <- "None"
  data$effort[which(lubridate::ymd_hms(data$effort$time_local) %>% substr(12,16) == "15:46"),]$Cloud.Cover <- "<25%"
  data$effort[which(lubridate::ymd_hms(data$effort$time_local) %>% substr(12,16) == "15:46"),]$Precipitation <- "Clear"
  data$effort[which(lubridate::ymd_hms(data$effort$time_local) %>% substr(12,16) == "15:46"),]$Effort_Instrument <- "7x50 Fujinons"
  data$effort[which(lubridate::ymd_hms(data$effort$time_local) %>% substr(12,16) == "15:27"),]$time_local <- "2020-09-02 15:28:21"
  data$effort[which(lubridate::ymd_hms(data$effort$time_index) %>% substr(12,16) == "15:27"),]$time_index <- "2020-09-02 15:28:21"
  data$effort[which(lubridate::ymd_hms(data$effort$time_local) %>% substr(12,16) == "12:21"),]$time_local <- "2020-09-10 12:16:21"
  data$effort[which(lubridate::ymd_hms(data$effort$time_index) %>% substr(12,16) == "12:21"),]$time_index <- "2020-09-10 12:16:21"
  data$effort[which(lubridate::ymd_hms(data$effort$time_local) %>% substr(12,16) == "09:14"),]$time_local <- "2020-09-10 09:13:56"
  data$effort[which(lubridate::ymd_hms(data$effort$time_index) %>% substr(12,16) == "09:14"),]$time_index <- "2020-09-10 09:13:56"
  data$effort$Platform <- "Fujinon_MBbow"
  data$effort[which(data$effort$Status == "ON EFFORT" & is.na(data$effort$Data.Recorder)),]$Data.Recorder <- "SHrushy"

  data$sightings[which(data$sightings$time_index == "2020-09-02 14:04:53"),]$time_local <-  "2020-09-02 14:04:53"
  data$sightings[which(data$sightings$time_index == "2020-09-10 09:55:29.2"),]$time_local <-  "2020-09-10 09:55:29.2"
  data$sightings[which(data$sightings$time_index == "2020-09-13 10:34:29.4"),]$time_local <-  "2020-09-13 10:34:29.4"

  data$sightings[which(as.character(date(data$sightings$time_index)) == "2020-09-03" & data$sightings$Sgt.Id == 'S51'),]$Bearing <- '0l'
  data$sightings[which(as.character(date(data$sightings$time_index)) == "2020-09-13" & data$sightings$Sgt.Id %in% c("S5", "S6", "S7", "S8")),]$Incidental.Sighting <- T
  data$sightings$Reticle.Instr <- "Fujinon_MBbow"
}

# ----------------------------------------------------------------------
# ---------- SAVE TIDY, COLLATED DATA FOR INPUT TO EVA'S CODE ----------
# ----------------------------------------------------------------------
effort <- data$effort
sightings <- data$sightings
multispecies <- data$multispecies
comments <- data$comments
# SEE DATA_EDITS !!!
write.csv(effort, file.path("survey_data","tidy_data", year, tolower(month_abb), paste0("cemore_", year, tolower(month_abb), "_EffortEnv.csv")), row.names = F)
write.csv(sightings, file.path("survey_data","tidy_data", year, tolower(month_abb), paste0("cemore_", year, tolower(month_abb), "_Sighting.csv")), row.names = F)
write.csv(multispecies, file.path("survey_data","tidy_data", year, tolower(month_abb), paste0("cemore_", year, tolower(month_abb), "_xMultiSpecies.csv")), row.names = F)
write.csv(comments, file.path("survey_data","tidy_data", year, tolower(month_abb), paste0("cemore_", year, tolower(month_abb), "_zComments.csv")), row.names = F)
# ----------------------------------------------------------------------
# SAVE TIDY DATA .RDS
df <- list(effort, sightings, multispecies, comments)
names(df) <- c("effort", "sightings", "multispecies", "comments")

data_file <- file.path("survey_data","tidy_data", "collated_rds", paste0("cemore_survey_tidy_data_", year, "_",month, ".rds"))
saveRDS(df, data_file)
cat(". Saving tidy data .rds file")
