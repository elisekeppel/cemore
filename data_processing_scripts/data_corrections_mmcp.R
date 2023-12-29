# ----------------------------------------------------------------------
# -------------------- DATA CORRECTIONS - ALL MONTHS -------------------
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# -------------------- DATA CORRECTIONS - BY MONTH ---------------------
# ----------------------------------------------------------------------

if(year == 2022 & tolower(month_abb) == "jul" & vessel=="GN"){
  data$effort[which(data$effort$Action ==""),]$Action <- "Weather update" # cannot have empty Action field - there was a comment added after effort ended for day
  data$sightings[which(data$sightings$Bearing %like% "N/A"),]$Bearing <- "" # cannot have NA in bearing field
}

if(year == 2022 & tolower(month_abb) == "aug" & vessel=="GN"){
  data$effort[which(data$effort$time_index =="2022-08-05T08:46:17.5"),]$time_local <- "2022-08-05T08:46:17.5" # time_index and time_local off by a few milliseconds
  data$effort[which(data$effort$time_index =="2022-08-05T12:01:04.3"),]$time_local <- "2022-08-05T12:01:04.3" # time_index and time_local off by a few milliseconds
  data$effort[which(data$effort$time_index =="2022-08-11T11:57:27.0"),]$time_local <- "2022-08-11T11:57:27.0" # time_index and time_local off by a few milliseconds
  data$effort[which(data$effort$time_index =="2022-08-11T14:08:56.6"),]$time_local <- "2022-08-11T14:08:56.6" # time_index and time_local off by a few milliseconds

  data$effort[which(data$effort$time_index =="2022-08-05T15:42:32.6"),]$time_local <- "2022-08-05T15:41:32.6" # effort ended sooner as evident by slowed ship speed
  data$effort[which(data$effort$time_index =="2022-08-05T15:42:32.6"),]$time_index <- "2022-08-05T15:41:32.6" # effort ended sooner as evident by slowed ship speed

  data$sightings[which(data$sightings$Sgt.Id == "S12"),]$Reticle.Instr <-  "Fujinon_GreatNorthern" # incidental sgt's with no ret instr
  data$sightings[which(data$sightings$Sgt.Id == "S13"),]$Reticle.Instr <-  "Fujinon_GreatNorthern" # incidental sgt's with no ret instr
}
if(year == 2022 & tolower(month_abb) == "aug" & vessel=="CC"){
  data$effort[which(data$effort$time_index =="2022-08-11T11:07:09.4"),]$time_local <- "2022-08-11T11:07:09.4" # time_index and time_local off by a few milliseconds
  data$effort[which(data$effort$time_index =="2022-08-11T14:56:40.3"),]$time_local <- "2022-08-11T14:56:40.3" # time_index and time_local off by a few milliseconds
  data$effort[which(data$effort$Action ==""),]$Action <- "Weather update" # cannot have empty Action field - there was a comment aded after effort ended for day
  data$effort[which(data$effort$time_index =="2022-08-11T14:56:40.3"),]$time_local <- "2022-08-11T14:45:52.3" # effort ended sooner as evident by slowed ship speed
  data$effort[which(data$effort$time_index =="2022-08-11T14:56:40.3"),]$time_index <- "2022-08-11T14:45:52.3" # effort ended sooner as evident by slowed ship speed

  data$sightings[which(data$sightings$Sgt.Id == "S28"),]$Incidental.Sighting <-  T

}

if(year == 2022 & tolower(month_abb) == "sep" & vessel=="GN"){
  data$effort[which(data$effort$time_index =="2022-09-17T11:52:45.2"),]$time_local <- "2022-09-17T11:52:45.2" # time_index and time_local off by a few milliseconds
  data$effort[which(data$effort$Action ==""),]$Action <- "Weather update" # cannot have empty Action field - there was a comment aded after effort ended for day
}
if(year == 2022 & tolower(month_abb) == "sep" & vessel=="TJ"){
  data$effort[which(data$effort$time_index =="2022-09-17T13:50:34.8"),]$time_local <- "2022-09-17T13:50:34.8" # time_index and time_local off by a few milliseconds
  data$sightings[which(is.na(data$sightings$Reticle.Instr)),]$Reticle.Instr <-  "Fujinon_TitanJunior" # incidental sgt's with no ret instr
}

# Trackline mysti csv files field name changed
if(data.source == "cemore") track.path <- file.path(getwd(),main.dir, "tracklines", "transects", "csv", paste0(year, "-", month))
if(data.source == "mmcp") track.path <- file.path(getwd(), main.dir, "tracklines", "transects", "csv", paste0(year, "-", month),vessel)
track.files <- list.files(track.path,include.dirs = FALSE,full.names = TRUE)
track.list <- as.list(track.files)
for(i in 1:length(track.list)) { #upload all GPS tables
  x <- read.csv(track.files[i], header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","na","n/a","N/A",""))

  if(!is.null(x$Distance.From.Previous..km.)) {
    x %<>% rename(Distance.From.Previous..m.=Distance.From.Previous..km.) %>%
      mutate(Distance.From.Previous..m.*1000)
    # x$Distance.From.Previous..m. <- x$Distance.From.Previous..km.*1000
    # x$Distance.From.Previous..km. <- NULL
  }
  if(!is.null(x$Speed.Over.Ground..kph.)) {
    x %<>% rename(Speed.Over.Ground..kts.=Speed.Over.Ground..kph.) %>%
      mutate(Speed.Over.Ground..kts./1.852)
    # x$Speed.Over.Ground..kts. <- x$Speed.Over.Ground..kph./1.852
    # x$Speed.Over.Ground..kph. <- NULL
  }
  if(!is.null(x$Distance.From.Start..km.)) {
    x %<>% rename(Distance.From.Start..m.=Distance.From.Start..km.) %>%
      mutate(Distance.From.Start..m.*1000)
    # x$Distance.From.Start..m. <- x$Distance.From.Start..km.*1000
    # x$Distance.From.Start..km. <- NULL
  }
  write.csv(x, track.files[i], row.names=F)
}

if(vessel=="CC" & nrow(data$sightings[which(data$sightings$Reticle.Instr==""),])>0){
  data$sightings[which(data$sightings$Reticle.Instr==""),]$Reticle.Instr <- "Fujinon_CharleyCBow (CharleyC)"
}

if(year == 2023 & tolower(month_abb) == "aug" & vessel=="CC"){
  data$effort[which(data$effort$time_index =="2023-08-21T10:28:53.1"),]$time_local <- "2023-08-21T10:29:28" # effort start entry before up to speed
  data$effort[which(data$effort$time_index =="2023-08-21T10:28:53.1"),]$time_index <- "2023-08-21T10:29:28" # effort start entry before up to speed
  data$effort[which(data$effort$time_index =="2023-08-21T16:34:27.8"),]$time_local <- "2023-08-21T16:34:08" # effort end entry after slowed down speed
  data$effort[which(data$effort$time_index =="2023-08-21T16:34:27.8"),]$time_index <- "2023-08-21T16:34:08" # effort end entry after slowed down speed
}
if(year == 2023 & tolower(month_abb) == "aug" & vessel=="GN"){
  data$effort[which(data$effort$time_index =="2023-08-21T17:16:27.4"),]$Action <- "Weather update" # effort entry at end of day with blank 'action' field - must be populated
}
cat("Applying data corrections")

# ----------------------------------------------------------------------
# ---------- SAVE TIDY, COLLATED DATA FOR INPUT TO EVA'S CODE ----------
# ----------------------------------------------------------------------
effort <- data$effort
sightings <- data$sightings
multispecies <- data$multispecies
comments <- data$comments
# SEE DATA_EDITS !!!
write.csv(effort,      file.path(main.dir,"tidy_data", year, tolower(month_abb), vessel, paste0(data.source,"_", year, tolower(month_abb), "_", vessel, "_EffortEnv.csv")), row.names = F)
write.csv(sightings,   file.path(main.dir,"tidy_data", year, tolower(month_abb), vessel, paste0(data.source,"_", year, tolower(month_abb), "_", vessel, "_Sighting.csv")), row.names = F)
write.csv(multispecies,file.path(main.dir,"tidy_data", year, tolower(month_abb), vessel, paste0(data.source,"_", year, tolower(month_abb), "_", vessel, "_xMultiSpecies.csv")), row.names = F)
write.csv(comments,    file.path(main.dir,"tidy_data", year, tolower(month_abb), vessel, paste0(data.source,"_", year, tolower(month_abb), "_", vessel, "_zComments.csv")), row.names = F)
# ----------------------------------------------------------------------
# SAVE TIDY DATA .RDS
df <- list(effort, sightings, multispecies, comments)
names(df) <- c("effort", "sightings", "multispecies", "comments")

data_file <- file.path(main.dir,"tidy_data", "collated_rds", paste0(data.source,"_survey_tidy_data_", year, "_",month,"_",vessel, ".rds"))
saveRDS(df, data_file)
cat(". Saving tidy data .rds file")
