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

#data$effort[which(!is.na(data$effort$Transect.ID)),] %<>% mutate(Transect.ID = paste(iteration, Transect.ID, sep = "-"))

# ----------------------------------------------------------------------
# -------------------- DATA CORRECTIONS - BY MONTH ---------------------
# ----------------------------------------------------------------------

cat("Applying data corrections")
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
  # data$effort$Transect.ID <- NA
  # data$effort[which(data$effort$Status == "ON EFFORT"),]$Transect.ID <- 1
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
  data$effort[which(data$effort$Platform == "Fujinon_RBbridge"),]$Platform <- "Fujinon_MBbow"



  # data$sightings[which(as.character(date(time_index) == "2020-09-13" & Sgt.Id %in% c(S3)))]
  data$sightings[which(as.character(date(data$sightings$time_index)) == "2020-09-03" & data$sightings$Sgt.Id == 'S51'),]$Bearing <- '0l'
  # data$sightings[which(as.character(date(data$sightings$time_index)) == "2020-09-03" & data$sightings$Sgt.Id == 'S19'),]$Distance..m. <- 200
  # data$sightings[which(as.character(date(data$sightings$time_index)) == "2020-09-03" & data$sightings$Sgt.Id == 'S19'),]$sighting_distance <- 200
  # data$sightings[which(as.character(date(data$sightings$time_index)) == "2020-09-03" & data$sightings$Sgt.Id == 'S19'),]$Reticles <- NA
  data$sightings[which(as.character(date(data$sightings$time_index)) == "2020-09-13" & data$sightings$Sgt.Id %in% c("S5", "S6", "S7", "S8")),]$Incidental.Sighting <- T

}

if(year == 2020 & tolower(month_abb) == "oct"){
  # data$effort$Transect.ID <- c(NA,1,NA,2,2,NA,NA,3,3,3,NA,4,4,4,NA,5,NA,6,6,6,NA,7,7,7,NA,8,8,8,NA,9,NA,10,NA,NA,11,11,NA,12,NA,13,13,NA,14,14,14,NA,
  #                              15,15,NA,16,NA,17,17,17,NA,18,18,18,NA,19,NA,20,NA,NA,21,NA,22,NA,23,23,NA,24,NA,25,25,NA,26,NA,27,NA,NA,28,NA,
  #                              29,NA,30,30,NA,31,NA,32,NA,33,NA)

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
  # data$effort$Beaufort[1:32] <- data$effort$PORT.Beaufort[1:32]
  data$effort$Beaufort[74] <- 1 # rest of that day Beaufort was 1 for all effort entries

  data$sightings[which(data$sightings$time_index == "2020-10-15 11:24:38"),]$Species <-  "Killer Whale - Southern Resident"
  data$sightings[which(data$sightings$time_index == "2020-10-20 07:58:05"),]$Species <-  "Harbour Porpoise"
  data$sightings[which(data$sightings$time_index == "2020-10-19 16:48:13"),]$Incidental.Sighting <-  T
  data$sightings[which(data$sightings$time_index == "2020-10-15 16:05:38"),]$Bearing <-  "43l"
  data$sightings[which(data$sightings$time_index == "2020-10-15 15:44:12"),]$Bearing <-  "41r"
  data$sightings[which(data$sightings$time_index == "2020-10-19 16:48:13"),]$Incidental.Sighting <-  T
}

if(year == 2020 & tolower(month_abb) == "nov"){  # TO DO - EDIT PLATFORM TO RBflybridge
  data$effort[which(data$effort$time_index == "2020-11-23T09:52:18.1"),]$time_local <-  "2020-11-23T09:52:18.1"
  data$effort[which(data$effort$time_index == "2020-11-23T13:28:27.4"),]$Action <-  "Weather update"
  data$effort[which(data$effort$time_index == "2020-11-25T09:58:42.1"),]$Status <-  "OFF EFFORT"
  # data$effort$Transect.ID <- c(NA,1,1,1,NA,2,2,NA,3,NA,4,4,NA,5,NA,NA,NA,6,NA,7,NA,8,NA,9,9,9,NA,10,NA,11,NA)

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


#temporary only; TO DO: fix 2021-01-22T10:05:23.4 entry - see comments
if(surveyid == "cemore_2021jan"){
  data$effort %<>% filter(!time_index == "2021-01-23T10:36:38.8", !Action == "NANA")
  data$effort[which(data$effort$time_index == "2021-01-20T16:18:21.6"),]$time_local <- "2021-01-20T16:17:15"
  data$effort[which(data$effort$time_index == "2021-01-20T16:18:21.6"),]$time_index <- "2021-01-20T16:17:15"
  data$effort[which(data$effort$time_index == "2021-01-22T13:45:32.1"),]$time_local <- "2021-01-22T13:45:08"
  data$effort[which(data$effort$time_index == "2021-01-22T13:45:32.1"),]$time_index <- "2021-01-22T13:45:08"
  data$sightings[which(data$sightings$time_index == "2021-01-20 14:30:09"),]$Reticle <- 0.9
}


# EK edit temp fix for data with "-" in Bearing column. Fixed in my data processing code
# if(surveyid == "cemore_feb_2021"){
#   sightings[which(sightings$Sgt.ID == "S2d"),]$Bearing <- '45L'
# }

if(year == 2021 & tolower(month_abb) == "mar"){
  data$effort <- data$effort %>% filter(!Action == "")
  data$effort[which(data$effort$time_index == "2021-03-03T15:34:24.9"),]$time_index <-  "2021-03-03T15:32:01"
  data$effort[which(data$effort$time_local == "2021-03-03T15:34:24.9"),]$time_local <-  "2021-03-03T15:32:01"
  data$effort[which(data$effort$Status =="CLOSING"),]$Status <- "OFF EFFORT"
}

if(year == 2021 & tolower(month_abb) == "apr"){
  data$effort <- data$effort %>% filter(!Action == "")
  data$effort[which(data$effort$Transect.ID == 37),]$Cloud.Cover <- "100%"
  data$effort[which(data$effort$Transect.ID == 37),]$Precipitation <- "Light Rain"
  data$effort[which(data$effort$time_index == "2021-04-23T15:08:48.6"),]$time_index <- "2021-04-23T15:07:47"
  data$effort[which(data$effort$time_local == "2021-04-23T15:08:48.6"),]$time_local <-  "2021-04-23T15:07:47"
  # data$sightings[which(data$sightings$Species == "Grey Whale"),]$Incidental.Sighting <- "FALSE" # TEMPORARY FOR PLOTTING
  # data$sightings[which(data$sightings$Species == "Grey Whale"),]$Side <- "Port" # TEMPORARY FOR PLOTTING
  # data$sightings[which(data$sightings$Species == "Grey Whale"),]$Bearing <- "0L" # TEMPORARY FOR PLOTTING
}

if(year == 2021 & tolower(month_abb) == "may"){
  data$sightings[which(data$sightings$time_index == "2021-05-15 17:01:09"),]$Incidental.Sighting <-  T
  # data$sightings[which(data$sightings$time_index == "2021-05-17 09:33:30"),]$Distance..m. <-  0.053995680
}

if(year == 2021 & tolower(month_abb) == "jun"){
  data$sightings[which(data$sightings$time_index == "2021-06-07 10:51:08"),]$Incidental.Sighting <-  T
  data$sightings[which(data$sightings$time_index == "2021-06-07 12:27:44"),]$Incidental.Sighting <-  T
  data$sightings[which(data$sightings$time_index == "2021-06-08 15:00:15"),]$Incidental.Sighting <-  T
  data$sightings[which(data$sightings$time_index == "2021-06-04 14:18:08"),]$Reticle <-  0.75
  data$sightings[which(data$sightings$time_index == "2021-06-04 13:01:20"),]$Reticle <-  1
  data$effort[which(data$effort$time_index == "2021-06-17T10:21:10.0"),]$time_index <- "2021-06-17T10:21:10.1"

}

if(year == 2021 & tolower(month_abb) == "jul"){
  data$sightings[which(data$sightings$time_index == "2021-07-09 17:29:12"),]$Species <-  "Killer Whale - Transient"
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

if(year == 2021 & tolower(month_abb) == "oct"){
  data$effort[which(data$effort$time_index == "2021-10-16T16:56:55.5"),]$Status <-  "OFF EFFORT"
  data$sightings[which(data$sightings$time_index == "2021-10-14 09:28:22"),]$Incidental.Sighting <- T
  data$sightings[which(data$sightings$time_index == "2021-10-14 10:55:09"),]$Incidental.Sighting <- T
  data$sightings[which(data$sightings$Species == "Killer Whale - Southern Resident"),]$Species <- "Killer Whale - Unknown ecotype"
}

if(year == 2021 & tolower(month_abb) == "nov"){
  data$effort[which(data$effort$time_index == "2021-11-17T14:05:22.9"),]$Right.Glare.Limit <-  0
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
  data$sightings[which(data$sightings$Sgt.Id == "S1"),]$Species <-  "Humpback Whale"

  data$effort[which(data$effort$time_index == "2022-03-25T15:07:57.0"),]$Transect.ID <-  1
  data$effort[which(data$effort$time_index == "2022-03-25T15:28:23.8"),]$Action <-  "Weather update"
  data$effort[which(data$effort$time_index == "2022-03-28T17:38:00.1"),]$Beaufort <-  2

}

if(year == 2022 & tolower(month_abb) == "apr"){
  data$effort[which(data$effort$Platform == "Fujinon_TanuMonkey"),]$Platform <-  "Fujinon_MBBow"
  data$effort[which(data$effort$time_index == "2022-04-26T15:07:13.1"),]$Glare <- "None"
  # data$effort <- data$effort[which(!data$effort$time_index == "2022-04-26T11:55:25.6"),] speed slowed, so data are correct to go off effort for a minute
  # data$effort <- data$effort[which(!data$effort$time_index == "2022-04-26T11:53:59.2"),] perhaps captain passing shades or cloths to obs's since no data person
  data$effort[which(data$effort$time_index == "2022-04-26T11:55:25.6"),]$Transect.ID <-  4 # but have to restate transect id when back on effort
  data$effort[which(data$effort$time_index == "2022-04-26T11:55:25.6"),]$PORT.Visibility <-  "Excellent/Good"
  data$effort[which(data$effort$time_index == "2022-04-26T11:55:25.6"),]$Beaufort <-  4
  data$effort[which(data$effort$time_index == "2022-04-26T11:55:25.6"),]$STBD.Visibility <-  "Excellent/Good"
  data$effort[which(data$effort$time_index == "2022-04-26T11:55:25.6"),]$Cloud.Cover <-  "50% to 75%"
  # data$effort[which(!data$effort$time_index == "2022-04-26 11:55:25"),]$Transect.ID <-  4

  data$sightings[which(date(data$sightings$time_index) == "2022-04-25"),]$Incidental.Sighting <- T
  data$sightings[which(data$sightings$Reticle.Instr == "Fujinon_TanuMonkey"),]$Reticle.Instr <- "Fujinon_MBBow"
}

if(year == 2022 & tolower(month_abb) == "jul"){
  data$effort[which(data$effort$time_index == "2022-07-19T14:35:46.9"),]$time_local <- "2022-07-19T14:35:46.9"
}

if(year == 2022 & tolower(month_abb) == "aug"){
  req.conditions <- c("Platform","Transect.ID","Effort_Instrument","PORT.Visibility","Beaufort","STBD.Visibility","Swell","Glare","Cloud.Cover","Precipitation")

  data$effort[which(data$effort$time_index == "2022-08-24T11:24:46.8"),req.conditions] <- data$effort[which(data$effort$time_index == "2022-08-24T11:00:14.2"),req.conditions]
}

if(year == 2022 & tolower(month_abb) == "sep"){
  data$effort[which(data$effort$time_index == "2022-09-20T11:33:13.7"),]$Transect.ID <- 2
}

if(year == 2022 & tolower(month_abb) == "oct"){
  data$effort[which(data$effort$time_index == "2022-10-13T13:13:07.4"),]$Transect.ID <- 28
  data$effort[which(data$effort$time_index == "2022-10-13T13:35:01.8"),]$Transect.ID <- 28
  data$effort[which(data$effort$time_index == "2022-10-13T13:46:53.6"),]$Transect.ID <- 27
  data$effort[which(data$effort$time_index == "2022-10-13T13:46:53.6"),]$Precipitation <- "Smoke"
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
