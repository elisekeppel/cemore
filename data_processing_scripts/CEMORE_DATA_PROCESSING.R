#################################################################################
## SHIP SURVEY DATA PROCESSING SOURCE CODE ADAPTED FOR CEMORE ANALYSIS BY EK
#################################################################################
# Author: Eva Stredulinsky
# Please do not change anything in this code without consulting Eva first!!!
# Adapted (with E. Stredulinsky permission and guidance) by Elise Keppel to process
# the CeMoRe survey data in preperation for analysis

cat("\n\n\n
    #################################################################################
    ## SHIP SURVEY DATA PROCESSING
    #################################################################################")
cat("\n\n****** DO NOT MAKE ANY CHANGES TO THE SOURCE CODE BEFORE CONSULTING WITH ELISE PLEASE!!! ******\n\n")

cat("\nFYI: Modifications made to data within this process will not alter the imported files. Use the modified files exported from this process for analyses.\n\n")

cat("FYI: This process will produce dataEffort, dataSightings, dataSurveyID and sighting summary tables for the Master Survey database, as well as shapefiles of corrected sighting positions, effort buffers, effort tracklines, and effort points for the Survey Geodatabase. All these files will be saved in the OUTPUT FILES folder.\n\n")

#Load required packages # EK edit - not necessary as these are loaded in the cemore package
#====================================
# suppressMessages(library(plyr))
# suppressMessages(library(rgeos))
# suppressMessages(library(sp))
# suppressMessages(library(sf))
# suppressMessages(library(maptools))
# suppressMessages(library(raster))
# suppressMessages(library(rgdal))
# suppressMessages(library(dplyr))
# suppressMessages(library(lubridate))
suppressMessages(library(beepr))
suppressMessages(library(stringr))
# suppressMessages(library(magrittr))


#Set working directory:
#====================================
# setwd(path) # EK edit - remove

#Set working variables - already set in 0_Setup.R in cemore_analysis
# survey_title <- paste(first_up(month), year)
# surveyid = paste0("cemore_", year, tolower(month_abb))

#Set warning environment
#====================================
#This will make all warnings/errors/issues stop the process and produce an error message
options(warn=2, error=NULL)

#Load data files
#====================================
#Load survey data files:
cat("\n\n  LOADING SURVEY DATA FILES")
cat("\n-------------------------------------\n\n")

tryCatch(effort <- read.csv(paste(getwd(),u,data_path,u,surveyid,"_EffortEnv.csv", sep=""),header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","na","n/a","N/A","")), error = function(e) {paste("Oops! Are you sure '",surveyid,"_EffortEnv.csv' is saved in the 'Survey data' folder?",sep="")}, warning = function(w) {paste("Oops! Are you sure '",surveyid,"_EffortEnv.csv' is saved in the 'Survey data' folder?",sep="")})
if(exists("effort")){
  cat("Effort file upload successful!\n\n")
} else {
  beep(10)
  stop(paste("Effort file upload unsuccessful: Are you sure '",surveyid,"_EffortEnv.csv' is saved in the 'Survey data' folder?",sep=""), call. = FALSE)
}
iteration <- unique(effort$iteration)

tryCatch(sightings <- read.csv(paste(getwd(),u,data_path,u,surveyid,"_Sighting.csv", sep=""),header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","na","n/a","N/A","")), error = function(e) {paste("Oops! Are you sure '",surveyid,"_Sighting.csv' is saved in the 'Survey data' folder?",sep="")}, warning = function(w) {paste("Oops! Are you sure '",surveyid,"_Sighting.csv' is saved in the 'Survey data' folder?",sep="")})
if(exists("sightings")){
  cat("Sightings file upload successful!\n\n")
} else {
  beep(10)
  stop(paste("Sightings file upload unsuccessful: Are you sure '",surveyid,"_Sighting.csv' is saved in the 'Survey data' folder?",sep=""), call. = FALSE)
}

# tryCatch(survey <- read.table(paste(getwd(),u,"Survey data",u,surveyid,"_dataSurveyID.txt", sep=""), sep=",", header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","na","n/a","N/A","")), error = function(e) {paste("Oops! Are you sure '",surveyid,"_dataSurveyID.txt' is saved in the 'Survey data' folder?",sep="")}, warning = function(w) {paste("Oops! Are you sure '",surveyid,"_dataSurveyID.txt' is saved in the 'Survey data' folder?",sep="")})
tryCatch(survey <- read.table(paste(getwd(),u,data_path,u,surveyid,"_dataSurveyID.txt", sep=""), header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","na","n/a","N/A","")), error = function(e) {paste("Oops! Are you sure '",surveyid,"_dataSurveyID.txt' is saved in the 'Survey data' folder?",sep="")}, warning = function(w) {paste("Oops! Are you sure '",surveyid,"_dataSurveyID.txt' is saved in the 'Survey data' folder?",sep="")})

if(exists("survey")){
  cat("Survey info file upload successful!\n\n")
} else {
  beep(10)
  stop(paste("Survey info file upload unsuccessful: Are you sure '",surveyid,"_dataSurveyID.txt' is saved in the 'Survey data' folder?",sep=""), call. = FALSE)
}

tryCatch(ship <- read.table(paste(getwd(),u,"Required information tables",u,"dataHOE.txt", sep=""), sep=",", header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","na","n/a","N/A","")), error = function(e) {"Oops! Are you sure 'dataHOE.txt' is saved in the 'Required information tables' folder?"}, warning = function(w) {"Oops! Are you sure 'dataHOE.txt' is saved in the 'Required information tables' folder?"})
if(exists("ship")){
  cat("Vessel info file upload successful!\n\n")
} else {
  beep(10)
  stop("Vessel info file upload unsuccessful: Are you sure 'dataHOE.txt' is saved in the 'Required information tables' folder?", call. = FALSE)
}

tryCatch(DST <- read.csv(paste(getwd(),u,"Required information tables",u,"DST.csv", sep=""), sep=",", header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","na","n/a","N/A","")), error = function(e) {"Oops! Are you sure 'DST.csv' is saved in the 'Required information tables' folder?"}, warning = function(w) {"Oops! Are you sure 'DST.csv' is saved in the 'Required information tables' folder?"})
if(exists("DST")){
  cat("Daylight Savings info file upload successful!\n\n")
} else {
  beep(10)
  stop("Daylight Savings info file upload unsuccessful: Are you sure 'DST.csv' is saved in the 'Required information tables' folder?", call. = FALSE)
}

#Import GPS data
cat("Importing GPS data... ")
if(data.source == "cemore") track.path <- paste(getwd(), u,main.dir,u, "tracklines",u, "transects", u, "csv",u, year, "-", month,sep="")
if(data.source == "mmcp") track.path <- paste(getwd(), u,main.dir,u, "tracklines",u, "transects", u, "csv",u, year, "-", month,u,vessel,sep="")
track.files <- list.files(track.path,include.dirs = FALSE,full.names = TRUE)
track.list <- as.list(track.files)
for(i in 1:length(track.list)) { #upload all GPS tables
  data <- read.csv(track.files[i], header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","na","n/a","N/A",""))
  if(exists("data")){
    cat("TrackLines file upload successful!\n\n")
  } else {
    stop(paste0("TrackLine upload was unsuccessful: ", basename(track.files[i])))
  }
  # Get rid of rows where Distance.From.Previous..m. == 0
  data <- data[which(data$Distance.From.Previous..m.!=0 | is.na(data$Distance.From.Previous..m.)),] # why are all of our variables in "m" not "nm"?

  rownames(data)<-1:length(data$Distance.From.Previous..m.)
  data <- dplyr::rename(data, Time.Created = grep("Time.Created..P", names(data), value = T))
  dates <- sort(unique(as.Date(str_extract(data$Time.Created, "^.{10}"), format="%Y-%m-%d"))) # EK edit changed to PST when required
  d <- as.numeric(diff(dates))
  #Check to ensure that file name dates match the date/time stamps within the file (i.e. look for non-consecutive dates):
  if(sum(d!=1)==0){
    cat("All date stamps in gps are consecutive!\n\n")
  } else {
    beep(10)
    stop(paste("Problematic non-consecutive date called in from Tracklines file: ", track.files[i],". Please check this file and make corrections if needed. See Robin as this is likely a Mysticetus related issue."), call. = FALSE)
  }
  track.list[[i]] <- data #Store dataframe in list
  data<-NULL
}

gps <- do.call("rbind",track.list) #append all GPS data together
gps$Time.Created..UTC. <- as.POSIXct(strptime(gps$Time.Created..UTC., format = "%Y-%m-%d %H:%M:%S"),tz="GMT") # Make sure GPS data is in GMT!
gps$Time.Created <- as.POSIXct(strptime(gps$Time.Created,format = "%Y-%m-%dT%H:%M:%S"),tz="America/Vancouver") #EK edit
# gps$Time.Created..PST. <- as.POSIXct(strptime(gps$Time.Created..PST.,format = "%Y-%m-%dT%H:%M:%S"),tz="America/Vancouver")

#Remove redundant records
gps <- gps[order(gps$Time.Created),]
rownames(gps) <- c(1:nrow(gps))
gps$duplicate.DT <- duplicated(gps$Time.Created)
gps <- gps[which(gps$duplicate.DT==FALSE),]
gps$duplicate.DT <- NULL
rownames(gps) <- c(1:nrow(gps))
if(nrow(gps)==0){
  stop("GPS file uploads unsuccessful: Are you sure all GPS data are saved as '.csv' files in the 'TrackLines' folder?", call. = FALSE)
} else {
  cat("GPS file uploads successful!\n\n")
}

#Make list of dataframes
#------------------------
dfList <- list(effort, sightings, survey, ship, DST, gps)

cat("\n\n\n
  DATA CLEANING
#################################################################################")

cat("\n\n  FORMATTING DATA TABLES")
cat("\n-------------------------------------\n\n")

#Remove blank rows from files
#==========================================
dfList <- lapply(dfList, function(x) x <- x[rowSums(is.na(x)) != ncol(x),])
effort <- as.data.frame(dfList[[1]])
sightings <- as.data.frame(dfList[[2]])
survey <- as.data.frame(dfList[[3]])
ship <- as.data.frame(dfList[[4]])
DST <- as.data.frame(dfList[[5]])
gps <- as.data.frame(dfList[[6]])

#Make sure all dataframes are formatted properly
#=================================================
cat("Checking dataframe column names...\n\n")

#Effort fields depend on the vessel platform # EK edit
if(is.null(effort$Franklin.Hut)) effort$Franklin.Hut <- NA
nn <- c("time_index", "time_local","Action","Status","Platform","Franklin.Hut","PORT.Observer","STBD.Observer","Effort_Instrument","Data.Recorder","Beaufort", "PORT.Visibility","STBD.Visibility","Swell","Glare","Left.Glare.Limit","Right.Glare.Limit","Cloud.Cover","Precipitation","Comments", "Locked.from.Editing","QA.QC_Comments")
if(length(which(nn %ni% colnames(effort)))!=0){
  beep(10)
  stop(paste("Column name(s) in EffortEnv.csv are missing or misspelled. We are looking for:", nn[which(nn %ni% colnames(effort))], sep = " "), call. = FALSE)
}

if(is.null(sightings$Porpoise.Behaviour)) sightings$Porpoise.Behaviour <- NA
nn <- c("time_index", "time_local","GPS.Pos","Sgt.Id","Bearing","Reticles","Horizon_Certainty","Reticle.Instr","Distance..m.","Side","Obs","Species","Min.Cnt","Max.Cnt","Best.Cnt","Photos","Comments", "Incidental.Sighting","Bearing.Abs", "Sighting.Complete","Sgt.Dist..m.", "Sgt.Pos","Sgt.Lat","Sgt.Lon","Psd..m.", "QA.QC.Comments", "sighting_distance", "date", "Porpoise.Behaviour")
if(length(which(nn %ni% colnames(sightings)))!=0){
  beep(10)
  stop(paste("Column name(s) in Sightings.csv are missing or misspelled. We are looking for:", toString(nn[which(nn %ni% colnames(sightings))]), sep = " "), call. = FALSE)
}

nn <- c("SurveyID","Tasking","Vessel_code","VesselID","Analysis_Status","Analytical_Approach","Date_Start_GMT","Date_End_GMT","Total_Days","TotalONEFFAreakmsqd","TotalONEFFDistancekm","TotalONEFFTimehr","TotalOFFTimehr","TotalONCefforts","TotalONCTimehr","TotalONEFFsurveysightings","TotalONEFFanimalcount","Totalothersightings","Totalotheranimalcount","Date_Analysed")
if(length(which(nn %ni% colnames(survey)))!=0){
  beep(10)
  stop(paste("Column name(s) in dataSurveyID.txt are missing or misspelled. We are looking for:", nn[which(nn %ni% colnames(survey))], sep = " "), call. = FALSE)
}

nn <- c("Time.Created..UTC.","Time.Created","Latitude","Longitude","Altitude..m.","Speed.Over.Ground..kts.","Course.Over.Ground..T.","Distance.From.Previous..m.","Course.Over.Ground.from.Start..T.","Distance.From.Start..m.","Water.Temperature..C.","Water.Depth..m.")
if(length(which(nn %ni% colnames(gps)))!=0){
  beep(10)
  stop(paste("Column name(s) in Gps Data are missing or misspelled. We are looking for:", toString(nn[which(nn %ni% colnames(gps))], sep = " ")), call. = FALSE)
}

cat("\nDONE")


#Remove any spaces in no-space variables
#----------------------------------------
cat("\n\nRemoving spaces from character strings in dataframes...")

#Remember Sighting #s will likely be character strings (due to 'a,b,c' labelling), so should be included in this step
index<-which(names(effort) %in% c("PORT.Observer","STBD.Observer","Data.Recorder","PORT.Bigeyes","STBD.Bigeyes","PORT.Visibility","STBD.Visibility","Franklin.Hut"))
for(j in index) {effort[,j]<-gsub("[[:space:]]","",effort[,j])}
# index<-which(names(ship) %in% c("Ship_code","Platform"))
# for(j in index) {ship[,j]<-gsub("[[:space:]]","",ship[,j])}
index<-which(names(sightings) %in% c("Sgt.Id","Bearing","Reticle.Instr","Side","Obs"))
for(j in index) {sightings[,j]<-gsub("[[:space:]]","",sightings[,j])}
index<-which(names(survey) %in% c("SurveyID","Vessel_Code","Tasking"))
for(j in index) {survey[,j]<-gsub("[[:space:]]","",survey[,j])}
cat("DONE")


#Format variables
#---------------------------------------
cat("\n\nClassifying variables...")

################GPS DATAFRAME
gps <- gps[,c("Time.Created..UTC.","Time.Created","Latitude","Longitude","Speed.Over.Ground..kts.","Course.Over.Ground..T.")]
#Numeric variables
nv <- c("Latitude","Longitude","Speed.Over.Ground..kts.","Course.Over.Ground..T.")
index <- which(names(gps) %in% nv)
#Find any non-numeric values in columns that should be numeric
for(j in index) {
  y <- which.nonnum(gps[,j])
  if(length(y)!=0) {
    beep(10)
    stop(paste("Oops! There is a non-numeric value in the GPS Data table's", names(gps)[j], "column:", gps[y,j], "  Please correct this record in table and re-run the data processing code.", sep = " "), call. = FALSE)
  }
}
gps[,index]<-lapply(index, function(x) as.numeric(as.character(gps[,x])))

#Make sure all GPS index entries are unique
if(length(unique(gps$Time.Created))!=nrow(gps)){
  rep.index <- as.data.frame(table(gps$Time.Created))
  rep.index <- rep.index[which(rep.index$Freq>1),]
  beep(10)
  stop(paste("Oops! It appears that there was a GPS malfunction at some point during the survey that resulted in repeated GPS Index entries:", toString(rep.index[1,])," Please examine the GPS Data table and, if needed, recreate the GPS track and re-run this code.", sep = " "), call. = FALSE)
}

#Date-time variables
gps$Time.Created <- gsub("T"," ", gps$Time.Created)
gps$Time.Created <- as.POSIXct(strptime(gps$Time.Created,format = "%Y-%m-%d %H:%M:%S"),tz="America/Vancouver")

if(sum(is.na(gps$Time.Created))!=0){
  beep(10)
  stop("Oops! The Date and Time format in the GPS Data table is not correct. Make sure that the Regional settings on your computer have 'short date' format set to yyyy-MM-dd and 'long time' format set as HH:mm:ss, re-save the GPS Data table, and re-run this code.", call. = FALSE)
}

#Create GPS index
#this will be the unique key for the GPS and Effort dataframes. The unique key for the Sighting table will be GPSIndex-SightingNo
gps <- gps[order(gps$Time.Created),]
rownames(gps) <- c(1:nrow(gps))
gps$GPSIndex <- c(1:nrow(gps))

#################EFFORT DATAFRAME
#Make sure that the (hidden) index time is the same as the display time. If not, spit out error.
if(sum(effort$time_index==effort$time_local)!=nrow(effort)){
  stop(paste("Oops! There appear to be inconsistent Time entries in the  Effort table. Time_Index: ", toString(as.character(effort[which(effort$time_index!=effort$time_local),]$time_index))," Please correct these entries (Time_Index and Time_local should be identical) and run this code again.",sep = " "), call. = FALSE)
}

effort <- effort[,c("time_index","time_local","Action","Status","Transect.ID","Platform","Franklin.Hut","PORT.Observer","STBD.Observer","Effort_Instrument","Data.Recorder", "PORT.Visibility","Beaufort","STBD.Visibility","Swell","Glare","Left.Glare.Limit","Right.Glare.Limit","Cloud.Cover","Precipitation", "Comments","Locked.from.Editing","QA.QC_Comments")]


#Numeric variables
nv <- c("Beaufort","Left.Glare.Limit","Right.Glare.Limit")
index <- which(names(effort) %in% nv)
#Find any non-numeric values in columns that should be numeric
for(j in index) {
  y <- which.nonnum(effort[,j])
  if(length(y)!=0) {
    beep(10)
    stop(paste("Oops! There is a non-numeric value in the Effort table's", names(effort)[j], "column:", effort[y,j], "  Please correct this record in the Effort table and re-run the data processing code.", sep = " "), call. = FALSE)
  }
}
effort[,index]<-lapply(index, function(x) as.numeric(as.character(effort[,x])))

#Character variables
cv <- c("Action","Status","Transect.ID","Platform","Franklin.Hut","PORT.Observer","STBD.Observer","Effort_Instrument","Data.Recorder","PORT.Visibility","STBD.Visibility","Swell","Glare","Cloud.Cover","Precipitation","Comments","QA.QC_Comments")

index <- which(names(effort) %in% cv)
effort[,index]<-lapply(index, function(x) as.character(effort[,x]))

#Date-time variables EK edit
effort$time_index <- gsub("T"," ", effort$time_index)
effort$time_index <- as.POSIXct(strptime(effort$time_index,format = "%Y-%m-%d %H:%M:%S"),tz="America/Vancouver")
effort <- effort[order(effort$time_index),]
rownames(effort) <- c(1:nrow(effort))

#Match effort dataframe records to nearest GPS record
effort$GPSIndex <- NA
for (i in 1:nrow(effort)){
  tdif <- abs(difftime(effort[i,]$time_index,gps$Time.Created,units="mins"))
  h <- which(tdif==min(tdif))[1]
  effort[i,]$GPSIndex <- gps[h,]$GPSIndex
}

if(sum(is.na(effort$GPSIndex))!=0){
  beep(10)
  stop(paste("Oops! There are some missing Date-times either in the GPS Data or Effort table. Please rectify this and re-run this code.", sep=" "), call. = FALSE)
}


#################SIGHTINGS TABLE
#Make sure that the (hidden) index time is the same as the display time. If not, spit out error.
if(sum( substr(sightings$time_index, 1, nchar(sightings$time_index)-2)==substr(sightings$time_local, 1, nchar(sightings$time_local)-2))!=nrow(sightings)){
  stop(paste("Oops! There appear to be inconsistent Time entries in the Sighting table. Time_Index: ", toString(as.character(sightings[which(sightings$time_index!=sightings$time_local),]$time_index))," Please correct these entries (Time_Index and Time_local should be identical) and run this code again.",sep = " "), call. = FALSE)
}

# -------------------- EK insert ---------------------
sightings %<>% mutate(Distance..nm. = Distance..m./1852,
                      Sgt.Dist..nm. = Sgt.Dist..m./1852,
                      Psd..nm. = Psd..m./1852) %>%
  dplyr::select(-c(Distance..m., Sgt.Dist..m., Psd..m.))

sightings <- sightings[,c("time_index","time_local", "GPS.Pos","Sgt.Id","Bearing","Reticles","Horizon_Certainty","Reticle.Instr","Distance..nm.","Side","Obs","Species","Min.Cnt","Max.Cnt","Best.Cnt","Photos","Comments","Incidental.Sighting","Bearing.Abs","Sighting.Complete","Sgt.Dist..nm.","Sgt.Pos","Sgt.Lat", "Sgt.Lon","Psd..nm.", "QA.QC.Comments", "Porpoise.Behaviour")]

#Numeric variables
nv <- c("Reticles","Distance..nm.","Min.Cnt","Max.Cnt","Best.Cnt","Bearing.Abs","Sgt.Dist..nm.","Sgt.Lat","Sgt.Lon","Psd..nm.")
index <- which(names(sightings) %in% nv)
#Find any non-numeric values in columns that should be numeric
for(j in index) {
  y <- which.nonnum(sightings[,j])
  if(length(y)!=0) {
    ##beep(10)
    stop(paste("Oops! There is a non-numeric value in the Sightings table's", names(sightings)[j], "column:", sightings[y,j], "  Please correct this record in the Sightings table and re-run the data processing code.", sep = " "), call. = FALSE)
  }
}
sightings[,index]<-lapply(index, function(x) as.numeric(as.character(sightings[,x])))

#Character variables
cv <- c("Sgt.Id","Bearing","Reticle.Instr","Side","Obs","Species", "Porpoise.Behaviour")
index <- which(names(sightings) %in% cv)
sightings[,index]<-lapply(index, function(x) as.character(sightings[,x]))
#Date-time variables
sightings$time_index <- gsub("T"," ", sightings$time_index)
sightings$time_index <- as.POSIXct(strptime(sightings$time_index,format = "%Y-%m-%d %H:%M:%S"),tz="America/Vancouver")

#Match sightings dataframe records to nearest GPS record
sightings$GPSIndex <- NA
sightings$M.int <- as.numeric(rep(NA,nrow(sightings))) #M.int = match interval = time difference between sighting time and GPS match time (in seconds)
for (i in 1:nrow(sightings)){
  tdif <- abs(difftime(sightings[i,]$time_index,gps$Time.Created,units="mins"))
  h <- which(tdif==min(tdif))[1]
  sightings[i,]$GPSIndex <- gps[h,]$GPSIndex
  sightings[i,]$M.int <- difftime(sightings[i,]$time_index,gps[h,]$Time.Created,units = "secs")
}

if(sum(is.na(sightings$GPSIndex))!=0){
  ##beep(10)
  stop(paste("Oops! There are some missing Date-times in the Sightings table. Please rectify this and re-run this code.", sep=" "), call. = FALSE)
}

##################DST TABLE
DST$DST.start_GMT <- as.POSIXct(strptime(DST$DST.start_GMT,format = "%y-%m-%d %H:%M"),tz="GMT")
DST$DST.end_GMT <- as.POSIXct(strptime(DST$DST.end_GMT,format = "%y-%m-%d %H:%M"),tz="GMT")

##################SURVEYID TABLE
#Numeric variables
nv <- c("Total_Days","TotalONEFFTimehr","TotalONEFFDistancekm","TotalONEFFAreakmsqd","TotalONCefforts","TotalONCTimehr","TotalOFFTimehr","TotalONEFFsurveysightings","Totalothersightings","TotalONEFFanimalcount","Totalotheranimalcount")
index <- which(names(survey) %in% nv)
#Find any non-numeric values in columns that should be numeric
for(j in index) {
  y <- which.nonnum(survey[,j])
  if(length(y)!=0) {
    ##beep(10)
    stop(paste("Oops! There is a non-numeric value in the dataSurveyID table's", names(survey)[j], "column:", survey[y,j], "  Please correct this record in the dataSurveyID table and re-run the data processing code.", sep = " "), call. = FALSE)
  }
}
survey[,index]<-lapply(index, function(x) as.numeric(as.character(survey[,x])))
#Character variables
cv <- c("SurveyID","VesselID","Vessel_Code","Tasking","Analysis_Status", "Analytical_Approach", "Date_Analysed")
index <- which(names(survey) %in% cv)
survey[,index]<-lapply(index, function(x) as.character(survey[,x]))
#Date-time variables
survey$Date_Start_GMT <- as.POSIXct(strptime(survey$Date_Start_GMT,format = "%Y-%m-%d %H:%M:%S"),tz="GMT")
survey$Date_End_GMT <- as.POSIXct(strptime(survey$Date_End_GMT,format = "%Y-%m-%d %H:%M:%S"),tz="GMT")
if(sum(is.na(survey$Date_Start_GMT))==nrow(survey) | sum(is.na(survey$Date_End_GMT))==nrow(survey)){
  #beep(10)
  stop("Oops! The Date and Time format in the dataSurveyID table is not correct. Make sure that the Regional settings on your computer have 'short date' format set to yyyy-MM-dd and 'long time' format set as HH:mm:ss, re-save the dataSurveyID table, and re-run this code.", call. = FALSE)
}
if(data.source == "mmcp") surveyid <- paste0(surveyid, "_", vessel)
#Rename field names (to be consistent with previous survey data) # EK edit
#----------------------------------------------------------------
names(effort) <- c("time_index","time_local","Action","Status","Transect.ID","Platform","Franklin.Hut","Port.Observer","Starboard.Observer","Effort_Instrument","DataRecorder","PORT.Visibility","Beaufort","STBD.Visibility","Swell","Glare","Left.Glare.Limit","Right.Glare.Limit","Cloud.Cover","Precipitation","Comments","Locked.from.Editing","QAQC_Comments","GPSIndex")
names(gps) <- c("GpsTime.UTC","GpsTime","Latitude","Longitude","Speed","Heading","GPSIndex")
names(sightings) <- c("time_index","time_local","GPS.Pos","Sgt.ID","Bearing","Reticle","Horizon_Certainty","Reticle.Instr","Distance","Side","SightedBy","Species","MinNumber","MaxNumber","BestNumber","Photos","Comments","Incidental.Sighting","MYST_Bearing.abs","Sighting.Complete","MYST_SgtDist.nm","MYST.Pos","MYST_Sgt.Lat","MYST_Sgt.Lon","MYST_PSD.nm","QAQC_Comments", "Porpoise.Behaviour","GPSIndex","M.int")
cat("DONE")

#Store survey information
#=============================
cat("\n\n\n  STORING SURVEY INFO")
cat("\n-------------------------------------\n\n")
#SURVEY ID TABLE
#Make sure there is a record for the survey
if(nrow(survey[which(survey$SurveyID %in% surveyid),])!=1){
  if(nrow(survey[which(survey$SurveyID %in% surveyid),])==0){
    ##beep(10)
    stop(paste("Oops! There are no records for", surveyid, "in the SurveyID table. Please ensure there is a record for each survey in this table and run this code again.", sep = " "), call. = FALSE)
  } else {
    ##beep(10)
    stop(paste("Oops! There are multiple records for", surveyid, "in the SurveyID table. Please ensure there is only one record for each survey in this table and run this code again.", sep = " "), call. = FALSE)
  }
}
#Store the vessel code name for the survey #EK edit
if(survey[which(survey$SurveyID %in% surveyid),]$Vessel_code %ni% c("MB", "RB", "VE","TA", "FR", "CC", "GN", "TJ")){
  ##beep(10)
  stop(paste("Oops! The vessel code assigned to", surveyid, "in the SurveyID table isn't recognized:" , vessel,"Please make sure that vessel code in the SurveyID table is correct and run this code again.", sep = " "), call. = FALSE)
} else {
  vessel <- survey[which(survey$SurveyID %in% surveyid),]$Vessel_code
}
#     #Make sure the vessel is present in the HOE table
if(vessel %ni% ship$Ship_code){
  ##beep(10)
  stop(paste("Oops! The vessel," , vessel,"isn't present in the dataHOE table. Please enter the vessel and its height of eye information into the dataHOE table and run this code again.", sep = " "), call. = FALSE)
}

#Store year and month of survey
year <- year(min(gps$GpsTime))
# month <- month.abb(min(gps$GpsTime)) # TO DO
m <- month(min(gps$GpsTime))
# if(month==m){
#   m <- NULL
#   beep(10)
#   x <- readline(prompt = cat(paste("Please confirm that", surveyid, "took place in", month.name[month], year, "  [click here & type Yes or No & hit Enter]    \n\n"), sep = " "))
# } else {
#   beep(10)
#   x <- readline(prompt = cat(paste("Please confirm that", surveyid, "took place in", month.name[month],"-", month.name[m], year, "  [click here & type Yes or No & hit Enter]    \n\n"), sep = " "))
# }
x <- "y"
if(x %in% c("NO","no","No","N","n")){
  ##beep(10)
  stop("Please correct the date-time data in the GPS Index table and re-run this code.", call. = FALSE)
}

#Store the Tasking for the survey
if(survey[which(survey$SurveyID %in% surveyid),]$Tasking %ni% c("Dedicated","Opportunistic","Photo-ID","Dedicated/Opportunistic")){
  beep(10)
  stop(paste("Oops! The Tasking assigned to", surveyid, "in the SurveyID table isn't recognized. Please make sure that Tasking in the SurveyID table is either 'Dedicated', 'Opportunistic', 'Dedicated/Opportunistic', or 'Photo-ID', and run this code again.", sep = " "), call. = FALSE)
} else {
  task <- survey[which(survey$SurveyID %in% surveyid),]$Tasking
}

#Prompt to make sure that the Tasking for the survey is what is wanted
#x <- readline(prompt = cat(paste("\nPlease confirm that", surveyid, "is a", task, "survey. Keep in mind that only dedicated or dedicated/opportunistic surveys will produce corrected animal positions.  [click here & type Yes or No & hit Enter]    \n\n"), sep = " "))
x <- "y"
if(x %in% c("NO","no","No","N","n")){
  beep(10)
  x <- readline(prompt = cat(paste("Okay. Please state what type of survey", surveyid, "is: (1) Dedicated, (2) Opportunistic, (3) Dedicated/Opportunistic, or (4) Photo-ID   [click here & type 1, 2, 3, or 4 & hit Enter]", sep = " ")))
  t <- c("Dedicated","Opportunistic","Dedicated/Opportunistic","Photo-ID")
  task <- t[as.numeric(x)]
  survey[which(survey$SurveyID %in% surveyid),]$Tasking <- t[as.numeric(x)]
}
#Make sure the survey year is in the DST table
if(sum(DST$Year==year)!=1){
  beep(10)
  stop(paste("Oops! We don't have the start date-times of Daylight Savings time in the 'DST.csv' table for the survey year, ", year,". Please enter these data in the table, save it in the 'Required information tables' folder, then re-run this code.", sep = ""), call. = FALSE)
}

# TO DO set up survey transect id column in survey txt file
#If there is a survey design, the design, based on the vessel identifier, will be loaded.
if(vessel %in% c("RB","MB","VE","TA","FR","CC","GN","TJ")) {
  # beep(10)
  # x <- readline(prompt = cat(paste("How many transects were completed in survey", surveyid, "?   [click here & type number of transects & hit Enter]    \n\n"), sep = " "))
  # transect.name.list.MASTER <- seq(1:x)
  transect.name.list.MASTER <- unique(effort$Transect.ID[which(!is.na(effort$Transect.ID))])
  transect.name.list.directional <- transect.name.list.MASTER
} else {
  stop("Looks like we don't have a design for the given survey. Please fix this and try again.", call.=FALSE)
}

#Make sure all data entries make sense
#---------------------------------------
cat("\n\n  CHECKING DATA ENTRIES")
cat("\n-------------------------------------\n\n")

#Find Status entries that are not recognized
if(length(which(effort$Status %ni% c("ON EFFORT", "ON", "CLOSING")))!=0){
  index <- which(effort$Status %ni% c("ON EFFORT", "ON", "CLOSING"))
  effort[index,]$Status <- "OFF"
  effort[index,]$Transect.ID <- NA
}

if(length(which(effort$Status %in% c("ON EFFORT (Visual only)", "ON EFFORT (Visual&Acoustic Array)", "ON EFFORT")))!=0){
  effort[which(effort$Status %in% c("ON EFFORT (Visual only)","ON EFFORT (Visual&Acoustic Array)", "ON EFFORT")),]$Status <- "ON"
}

if(sum(effort$Status %ni% c(NA,"ON","OFF","CLOSING"))!=0){
  beep(10)
  stop(paste("Oops! There are Status entries in the Effort table that aren't recognized:", toString(unique(effort[which(effort$Status %ni% c(NA,"ON","OFF")),]$Status)),"Please correct these entries and run this code again. If any of these are valid Status entries, contact Eva about incorporating the new Activities into this automated process.",sep = " "), call. = FALSE)
}

#Check transect IDS are appropriately assigned
if(length(which(effort$Status!="ON" & !is.na(effort$Transect.ID)))!=0){
  stop(paste("There are some effort records that have Transect ID inappropriately assigned (i.e. we are not ON effort). Please clean it up:",toString(effort[which(effort$Status!="ON" & !is.na(effort$Transect.ID)),]$Time.PDT)),call.=FALSE)
}

#Check to make sure all transect IDs make sense
if(length(which(!is.na(effort$Transect.ID) & effort$Transect.ID %ni% transect.name.list.directional))!=0){
  stop(paste("There are some effort records that have incorrect Transect IDs assigned:",toString(effort[which(!is.na(effort$Transect.ID) & effort$Transect.ID %ni% transect.name.list.directional),]$Time.PDT)),call.=FALSE)
}

if(sum(effort$Action %ni% c("Changing effort status","Observer rotation","Transect ID change","Weather update"))!=0){
  ##beep(10)
  stop(paste("Oops! There are Action entries in the Effort table that aren't recognized or missing (EVERY effort record must have an Action assigned): ", toString(unique(effort[which(effort$Action %ni% c("Changing effort status","Observer rotation","Transect ID change","Weather update")),]$Action)),".  Please correct these entries and run this code again. If any of these are valid Action entries, contact Eva about incorporating the new Action(s) into this automated process.",sep = " "), call. = FALSE)
}

#Find Platform entries that are not recognized
# if(length(which(effort$Platform=="Monkey's Island"))!=0){
#   effort[which(effort$Platform=="Monkey's Island"),]$Platform <- "Mo"
# }
if(length(which(effort$Platform == "Bridge"))!=0){
  effort[which(effort$Platform=="Bridge"),]$Platform <- "Br"
}
if(length(which(effort$Platform %in% c("MBBow", "Fujinon_MBBow", "Fujinon_MBbow")))!=0){
  effort[which(effort$Platform %in% c("MBBow", "Fujinon_MBBow", "Fujinon_MBbow")),]$Platform <- "Bo"
}
if(length(which(effort$Platform =="RBFly_sitting"))!=0){
  effort[which(effort$Platform =="RBFly_sitting"),]$Platform <- "RBFly_sit"
}
if(length(which(effort$Platform =="RBFly_standing"))!=0){
  effort[which(effort$Platform =="RBFly_standing"),]$Platform <- "RBFly_stand"
}
if(length(which(effort$Platform =="Fujinon_RBbridge"))!=0){
  effort[which(effort$Platform =="Fujinon_RBbridge"),]$Platform <- "Br"
}


if(length(which(effort$Platform %in% c("Fujinon_VecBridge","Fujinon_TanuBridge")))!=0){
  effort[which(effort$Platform %in% c("Fujinon_VecBridge","Fujinon_TanuBridge")),]$Platform <- "Br"
}
if(length(which(effort$Platform %in% c("Fujinon_TanuMonkey")))!=0){
  effort[which(effort$Platform %in% c("Fujinon_TanuMonkey")),]$Platform <- "Mo"
}
if(length(which(effort$Platform %in% c("Fujinon_FranklinMI")))!=0){
  effort[which(effort$Platform %in% c("Fujinon_FranklinMI")),]$Platform <- "FR"
}
if(length(which(effort$Platform %in% c("Fujinon_CharleyC")))!=0){
  effort[which(effort$Platform %in% c("Fujinon_CharleyC")),]$Platform <- "CC"
}
if(length(which(effort$Platform %in% c("Fujinon_GreatNorthern")))!=0){
  effort[which(effort$Platform %in% c("Fujinon_GreatNorthern")),]$Platform <- "GN"
}
if(length(which(effort$Platform %in% c("Fujinon_TitanJunior")))!=0){
  effort[which(effort$Platform %in% c("Fujinon_TitanJunior")),]$Platform <- "TJ"
}

#Look in ship table and make sure that all platform entries match the vessel's design
vessel.platforms <- sort(unique(ship[which(ship$Ship_code==vessel),]$Platform))
if(length(vessel.platforms)==1){
  effort$Platform <- vessel.platforms
} else {
  if(sum(effort$Platform %ni% c(NA,vessel.platforms))!=0){
    beep(10)
    stop(paste("Oops! There are Platform entries in the Effort table that aren't recognized:", toString(unique(effort[which(effort$Platform %ni% c(NA,vessel.platforms)),]$Platform)),"We are looking for 'Monkey's Island' or 'Bridge'. Please correct these entries and run this code again.",sep = " "), call. = FALSE)
  }
}

#Find Instrument entries that are not recognized
if(length(which(effort$Effort_Instrument=="7x50 Fujinons"))!=0){
  effort[which(effort$Effort_Instrument=="7x50 Fujinons"),]$Effort_Instrument <- "Bi"
}
if(length(which(effort$Effort_Instrument %in% c("Naked Eye", " Naked Eye")))!=0){
  effort[which(effort$Effort_Instrument %in% c("Naked Eye", " Naked Eye")),]$Effort_Instrument <- "NE"
}

if(sum(effort$Effort_Instrument %ni% c(NA,"NE","BE","Bi"))!=0){
  ##beep(10)
  stop(paste("Oops! There are Instrument entries in the Effort table that aren't recognized:", toString(unique(effort[which(effort$Effort_Instrument %ni% c(NA,"NE","BE","Bi")),]$Instrument)),"Please correct these entries and run this code again.",sep = " "), call. = FALSE)
}

#Find Beaufort entries that don't make sense
if(sum(effort$Beaufort %ni% c(NA,0:9))!=0){
  ##beep(10)
  stop(paste("Oops! There are Beaufort entries in the Effort table that don't make sense:", toString(unique(effort[which(effort$Beaufort %ni% c(NA,0:9)),]$Beaufort)),"Please correct these entries and run this code again.",sep = " "), call. = FALSE)
}

#Find Visibility entries that don't make sense. For PRISMM: had to add restricted category and pooled Good & Excellent from initial portion of the survey.
#example of how to condense the code above so save processing time
if(length(which(effort$PORT.Visibility=="Restricted"))!=0){
  effort[which(effort$PORT.Visibility=="Restricted"),]$PORT.Visibility <- "R"
}
if(length(which(effort$STBD.Visibility=="Restricted"))!=0){
  effort[which(effort$STBD.Visibility=="Restricted"),]$STBD.Visibility <- "R"
}
if(length(which(effort$PORT.Visibility=="Poor"))!=0){
  effort[which(effort$PORT.Visibility=="Poor"),]$PORT.Visibility <- "P"
}
if(length(which(effort$STBD.Visibility=="Poor"))!=0){
  effort[which(effort$STBD.Visibility=="Poor"),]$STBD.Visibility <- "P"
}
if(length(which(effort$PORT.Visibility=="Fair"))!=0){
  effort[which(effort$PORT.Visibility=="Fair"),]$PORT.Visibility <- "F"
}
if(length(which(effort$STBD.Visibility=="Fair"))!=0){
  effort[which(effort$STBD.Visibility=="Fair"),]$STBD.Visibility <- "F"
}
if(length(which(effort$PORT.Visibility%in% c("Good","Excellent","Excellent/Good")))!=0){
  effort[which(effort$PORT.Visibility %in% c("Good","Excellent","Excellent/Good")),]$PORT.Visibility <- "G&E"
}
if(length(which(effort$STBD.Visibility%in% c("Good","Excellent","Excellent/Good")))!=0){
  effort[which(effort$STBD.Visibility %in% c("Good","Excellent","Excellent/Good")),]$STBD.Visibility <- "G&E"
}
if(sum(effort$PORT.Visibility %ni% c(NA,"R","G&E","F","P","Moderate"))!=0){
  beep(10)
  stop(paste("Oops! There are PORT.Visibility entries in the Effort table that don't make sense:","Please correct these entries and run this code again.",sep = " "), call. = FALSE)
}

if(sum(effort$STBD.Visibility %ni% c(NA,"R","G&E","F","P", "Moderate"))!=0){
  beep(10)
  stop(paste("Oops! There are STBD.Visibility entries in the Effort table that don't make sense:","Please correct these entries and run this code again.",sep = " "), call. = FALSE)
}

#Create final Visibility field that combines the STBD and PORT Visibility fields (using the worse one) # EK edit
vis <- c("R","P","F","G&E", "Moderate")
col <- c("PORT.Visibility","STBD.Visibility")
effort$Visib <- NA
for(i in 1:nrow(effort)){
  if(sum(is.na(effort[i,col]))!=2){
    effort[i,]$Visib <- effort[i,col[which.min(as.numeric(factor(c(effort[i,]$PORT.Visibility,effort[i,]$STBD.Visibility),vis)))]]
  }
}

#GPS DATA TABLE

#Make sure that ship speed is filled in and realistic
#First, impute Speeds for initial GPS records (if next hit is within a minute)
if(sum(is.na(gps$Speed))!=0){
  index = which(is.na(gps$Speed))
  for(i in index){
    if(as.numeric(difftime(gps[i+1,"GpsTime.UTC"],gps[i,"GpsTime.UTC"], unit="secs"))<60)
      gps[i,]$Speed <- gps[i+1,]$Speed
  }
}

####################################################################
#The sampling rate on the badelf was 1Hz, and random datapoints show erroneous speeds.  My solution is to delete these erroneous speeds, then impute the speed from the following record.  As speed is not used beyond the effort definition (and not even here), this has very little consequence on the data.  Exploring the data shows that 95% of data is between 9.5 and 10.3 knots, however to be conservative I choose 15Knots as the cut off.
summary(gps$Speed)

#Find Distances that don't make sense - ALL DISTANCES SHOULD BE IN NAUTICAL MILES
  if((vessel == "MB" & max(gps$Speed) >27) |
     (vessel == "RB" & max(gps$Speed) >28) |
     (vessel == "TA" & max(gps$Speed) >20) |
     (vessel == "VE" & max(gps$Speed) >20) |
     (vessel == "FR" & max(gps$Speed) >25)){
    x <- readline(prompt = cat(paste("\nThere are some suspiciously high speeds (", max(gps$Speed), ") in the gps data. Do any of these need to be corrected?   [click here & type Yes or No & hit Enter]    \n\n", sep=" ")))
    if(x %ni% c("NO","no","No","N","n")){
      beep(10)
      stop("Please make your corrections in the gps table and re-run this code.", call. = FALSE)
    }
  }
## if(sum(which(gps$Speed>25))!=0){
#   index=which(gps$Speed>25)
#   for(i in index){
#     if(as.numeric(difftime(gps[i,"GpsTime.UTC"],gps[i-1,"GpsTime.UTC"], unit="secs"))<60)
#       gps[i,]$Speed <- gps[i-1,]$Speed
#   }
# }
######################################################################################

if(sum(gps$Latitude==0 | is.na(gps$Latitude) | gps$Longitude==0 | is.na(gps$Longitude))){
  beep(10)
  stop("Oops! It appears that there was a GPS malfunction at some point during the survey that resulted in at least one record with no coordinates. Please examine the GPS Data table for missing or erroneous coordinates & fix. If needed, recreate the GPS track and re-run this code.", call. = FALSE)
}
#Make sure there are at least 4 digits for latitude and longitude data
if(sum(sapply(gps$Latitude, nchar)>4)==0){
  beep(10)
  stop("Oops! Latitude does not have adequate amount of digits in at least one record. Make sure that the Regional settings on your computer allow for the maximum number of digits in Numbers, then re-save the GPS Data table to the 'Survey data' folder, and run this code again.", call. = FALSE)
}
if(sum(sapply(gps$Longitude, nchar)>4)==0){
  beep(10)
  stop("Oops! Longitude does not have adequate amount of digits in at least one record. Make sure that the Regional settings on your computer allow for the maximum number of digits in Numbers, then re-save the GPS Data table to the 'Survey data' folder, and run this code again.", call. = FALSE)
}

#Make sure Headings are actually true (0-360)
#Added is.na code to take speed from next record if less then 10 seconds earlier.  If NA is more then 10 seconds from the next record, NA should remain as heading can change quickly, and the value is used directly by sighting position corrections.  At this point taking a gamble that there were no sightings associated with these particlar gpsIndex records, may have to revisit.

if(sum(is.na(gps$Heading))!=0){
  index = which(is.na(gps$Heading))
  for(i in index){
    if(as.numeric(difftime(gps[i+1,"GpsTime.UTC"],gps[i,"GpsTime.UTC"], unit="secs"))<10)
      gps[i,]$Heading <- gps[i+1,]$Heading
  }
}

if(sum(!is.na(gps$Heading)&(gps$Heading<0 |gps$Heading >360))!=0){
  beep(10)
  index=which(is.na(gps$Heading) | gps$Heading<0 |gps$Heading >360)
  stop("Oops! There is at least one record with missing or erroneous Heading. Please examine the GPS Data table for missing or erroneous headings (i.e. not between 0-360). If needed, recreate the GPS track and re-run this code. Remember: this should be TRUE heading (0-360), not relative heading.", call. = FALSE)
}

#Assign 'abc's to non-unique Sighting ID entries
cat("\n\nAmending Sighting ID #s...")
numb.rep <- as.data.frame(table(sightings$Sgt.ID))
sight.rep <- sort(as.character(numb.rep[which(numb.rep$Freq!=1),]$Var1))
if(length(sight.rep)!=0){ #if there are repeated Sighting #s
  for(i in seq_along(sight.rep)){ #Run through each repeated Sighting #
    index <- which(sightings$Sgt.ID==sight.rep[i]) #positions of each repeated Sighting #
    n <- length(index) #Number of times Sighting # is repeated
    new.number <- paste(sight.rep[i],letters[1:n],sep = "")
    sightings[index,]$Sgt.ID <- c(new.number)
  }
}

#Make sure all Sighting ID #s are unique (multispecies sightings should be dealt with prior to running the code)
if(any(duplicated(sightings$Sgt.ID))){
  stop("Oops! It appears that there are some sightings records with duplicate 'Sgt ID' values: ", toString(sort(unique(sightings[which(duplicated(sightings$Sgt.ID)),]$Sgt.ID," Please correct these entries in the Sightings table and run this code again. REMEMBER: all multispecies sightings should have been manually edited to have completely redundant fields with the original sighting entry, EXCEPT for Sgt ID - which should be split as: e.g. S150a, S150b, S150c, etc.", call. = FALSE))))
}

cat("DONE")

#Check values of Sighting table fields
#--------------------------------------
#Check species name entries
cat("\n\nChecking Species names...")
###>>>>THIS IS A SECTION THAT NEEDS TO BE GENERALIZED (i.e. have a master list of species names that are spelled correctly and throw error if any entries aren't from the list)


#Find Species entries that are not recognized
#in Myst. this is a dropdown menu, so should we just be looking for blanks?
if(sum(is.na(sightings$Species))!=0){
  beep(10)
  stop(paste("Oops! There appears to be at least one sighting missing Species information in the Sightings table: Sighting #", toString(sightings[which(is.na(sightings$Species)),]$Sgt.ID),". Please correct this in the Sightings table and re-run this code.", sep = " "), call. = FALSE)
}
# to remove sightings missing required
#sightings %<>% filter(Sgt.ID == toString(sightings[which(is.na(sightings$Species)),]$Sgt.ID))

#Remove apostrophes from species names (as they disrupt text file interpretation) #EK edit: add back in after processing for Dall's
if(length(grep("'",sightings$Species))!=0){
  sightings$Species <- gsub("'","",sightings$Species)
}

cat("DONE")

cat("\n\nFilter out non-target sightings...")
#Remove all Sightings with Non-Target species
if(length(grep("NON-TARGET",sightings$Species))!=0){
  sightings <- sightings[-grep("NON-TARGET",sightings$Species),]
  rownames(sightings) <- c(1:nrow(sightings))
}

#Remove all Mola Mola
if(length(grep("Mola",sightings$Species))!=0){
  sightings <- sightings[-grep("Mola",sightings$Species),]
  rownames(sightings) <- c(1:nrow(sightings))
}

#Remove all Sightings with Mistake/Cancel
if(length(grep("MISTAKE|CANCEL",sightings$Species))!=0){
  sightings <- sightings[-grep("MISTAKE|CANCEL",sightings$Species),]
  rownames(sightings) <- c(1:nrow(sightings))
}

cat("DONE")

cat("\n\nChecking other sightings field values...")

#Find Bearings that don't make sense
sightings$Bearing <- toupper(sightings$Bearing)
#Look for Bearings without 'L' or 'R' assigned:
#20190110 - The following code highlighted 20 instances that needed attention.  As these were a case by case basis, and not likely to introduce bias, I corrected them in Mysticetus and reexported. Action noted in the QA_QC_Comments. 75% of issues were simply missing Directions on sightings reported at 0. In the remaining cases, defaulted to the side of the observor.
if(length(which(!is.na(sightings$Bearing) & !grepl("L|R",sightings$Bearing)))!=0){
  beep(10)
  stop(paste("Oops! There is at least one sighting with no side assigned in its Bearing (e.g '5L', '36R'). Sighting #: ", toString(sightings[which(!is.na(sightings$Bearing) & !grepl("L|R",sightings$Bearing)),]$Sgt.ID), ". Please correct these entries in the Sightings table and run this code again. Remember, even sightings at a Bearing of zero have to have a direction assigned.",sep=""), call. = FALSE)
}

sightings$angle <- as.character(gsub("[a-zA-Z]","",sightings$Bearing))
#Check for non-sensical angles:
if(length(grep("[^0-9\\.]",sightings$angle))!=0){
  stop(paste0("Oops! There are sightings with Bearings that do not make sense. Sightings #: ", toString(sightings[grep("[^0-9\\.]",sightings$angle),]$Sgt.ID), ". Please correct these entries in the Sightings table and run this code again."), call. = FALSE)
}
sightings$angle <- floor(as.numeric(sightings$angle))

sightings[grep("L",sightings$Bearing),]$angle <- sightings[grep("L",sightings$Bearing),]$angle *-1
if(sum(!is.na(sightings$angle) & (sightings$angle < -180 | sightings$angle > 180))!=0){
  beep(10)
  stop("Oops! There is at least one sighting with a non-sensical bearing: ", toString(sightings[which(!is.na(sightings$angle) & (sightings$angle < -180 | sightings$angle > 180)),]$Bearing),". Please correct these entries in the Sightings table and run this code again.", call. = FALSE)
}

sightings$Bearing <- sightings$angle
sightings$angle <- NULL

#Find Reticles that don't make sense
if(sum(!is.na(sightings$Reticle) & sightings$Reticle < 0)!=0){
  ##beep(10)
  stop("Oops! There is at least one sighting with negative Reticle reported. Please correct these entries in the Sightings table and run this code again.", call. = FALSE)
}

#Find Distances that don't make sense - ALL DISTANCES SHOULD BE IN NAUTICAL MILES
if(sum(sightings[which(!is.na(sightings$Distance)),]$Distance>0.5 | sightings[which(!is.na(sightings$Distance)),]$Distance<0)!=0){
  issues <- which(!is.na(sightings$Distance) & (sightings$Distance>0.5|sightings$Distance<0))
  beep(10)
  x <- readline(prompt = cat(paste("\nThere are some suspicious distance estimates (i.e. very large or negative) in the Sightings table: Sighting #", toString(sightings[issues,]$Sgt.ID),". REMEMBER: PRIOR TO RUNNING THIS CODE, ALL DISTANCES SHOULD BE CHECKED TO MAKE SURE THEY ARE IN NAUTICAL MILES. Please examine these sightings closely before answering: Do any of these estimated distances need to be corrected?   [click here & type Yes or No & hit Enter]    \n\n", sep=" ")))
  if(x %ni% c("NO","no","No","N","n")){
    #beep(10)
    stop("Please make your corrections in the Sightings table and re-run this code.", call. = FALSE)
  }
}

#Find Side data that don't make sense
if(sum(sightings[which(sightings$Incidental.Sighting==FALSE),]$Side %ni% c("Port","Starboard"))!=0){
  issues<- which((sightings$Incidental.Sighting==FALSE) & sightings$Side %ni% c("Port","Starboard"))
  #beep(10)
  stop(paste("Oops! There is at least one sighting with missing or unrecognized Side reported. We are looking for 'Port' or 'Starboard', and all records of non-incidental sightings MUST have the Side field completed.Sightings #", toString(sightings[issues,]$Sgt.ID)," Please correct these entries in the Sightings table and run this code again.", sep = " "), call. = FALSE)
}

#Find Method entries that are not recognized
if(sum(is.na(sightings[which(!is.na(sightings$Reticle)),]$Reticle.Instr))!=0){
  stop("Oops! There are some sightings with Reticle estimates that don't have a Reticle.Instr assigned. Please correct this in the Sightings table and re-run this code.")
}

sightings$Method <- sightings$Reticle.Instr


#Assign a platform to sightings according to the Method entry
#unique(sightings$Method)
sightings$Platform <- NA

if(nrow(sightings[which(sightings$Method %in% c("Fujinon_MBBow", "Fujinon_MBbow")),])!=0){
  sightings[which(sightings$Method %in% c("Fujinon_MBBow", "Fujinon_MBbow")),]$Platform <- "Bo"
}

if(nrow(sightings[which(sightings$Method %in% c("Fujinon_bridge", "Fujinon_MBBridge")),])!=0){
  sightings[which(sightings$Method%in% c("Fujinon_bridge", "Fujinon_MBBridge")),]$Platform <- "Br"
}

# for Roller Bay
if(nrow(sightings[which(sightings$Method %in% c("RBFly_sit")),])!=0){
  sightings[which(sightings$Method%in% c("RBFly_sit")),]$Platform <- "RBFly_sit"
}
if(nrow(sightings[which(sightings$Method %in% c("RBFly_stand")),])!=0){
  sightings[which(sightings$Method%in% c("RBFly_stand")),]$Platform <- "RBFly_stand"
}

# for Franklin
if(nrow(sightings[which(sightings$Method %in% c("Fujinon_FranklinMI")),])!=0){
  sightings[which(sightings$Method%in% c("Fujinon_FranklinMI")),]$Platform <- "Fujinon_FranklinMI"
}
# for CharleyC
if(nrow(sightings[which(sightings$Method %in% c("Fujinon_CharleyC")),])!=0){
  sightings[which(sightings$Method%in% c("Fujinon_CharleyC")),]$Platform <- "Fujinon_CC"
}# for GreatNorthern
if(nrow(sightings[which(sightings$Method %in% c("Fujinon_GreatNorthern")),])!=0){
  sightings[which(sightings$Method%in% c("Fujinon_GreatNorthern")),]$Platform <- "Fujinon_GN"
}# for Franklin
if(nrow(sightings[which(sightings$Method %in% c("Fujinon_TitanJunior")),])!=0){
  sightings[which(sightings$Method%in% c("Fujinon_TitanJunior")),]$Platform <- "Fujinon_TJ"
}

#If there is a reported distance, change the defaulted method (originally Reticle Instrument) to NE
if(sum(!is.na(sightings$Distance))!=0){
  sightings[which(!is.na(sightings$Distance)),]$Method <- "NE"
}
# TO DO: add in Platform creation from Method for Vector and Tanu
# unique(sightings$Platform)

#Adjust Method entries so they will work with our pre-written functions ('Bi', 'BE', 'NE'):
if(sum(sightings$Method %in% c("Fujinon_bridge", "Fujinon_MBBow", "Fujinon_MBbow", "Fujinon_MBBridge", "Fujinon_RBbridge", "Fujinon_RBFly","Fujinon_VecBridge","Fujinon_TanuMonkey","Fujinon_TanuBridge", "Fujinon_FranklinMI","Fujinon_CharleyC","Fujinon_GreatNorthern", "Fujinon_TitanJunior"))!=0){
  sightings[which(sightings$Method %in% c("Fujinon_bridge","Fujinon_MBbow", "Fujinon_MBBow", "Fujinon_MBBridge", "Fujinon_RBbridge", "Fujinon_RBFly", "Fujinon_VecBridge","Fujinon_TanuMonkey","Fujinon_TanuBridge", "Fujinon_FranklinMI","Fujinon_CharleyC", "Fujinon_GreatNorthern", "Fujinon_TitanJunior")),]$Method <- "Bi"
}



#At this point all sightings should have a Method (Bi, BE, or NE)
#unique(sightings$Method)
if(sum(sightings$Method %ni% c("Bi","BE","NE"),na.rm=TRUE)!=0){
  issues<- which(sightings$Method %ni% c("Bi","BE","NE"))
  #beep(10)
  stop(paste("Go fix your sighting records that do not have either a bearing, reticle, reticle instrument, etc.  Sighting #", toString(sightings[issues,]$Sgt.ID)),call. = FALSE)
}

cat("DONE")


cat("\n\n\n All survey data entries appear satisfactory.")


#Create final Sightings table
#======================================================
cat("\n\n\n  DEALING WITH SIGHTINGS")
cat("\n-------------------------------------")

#Remove Mistake Cancel sightings
#------------------------------------------------------
cat("\n\nRemoving Mistake/Cancel sightings...")
#Look for 'cancel' or 'mistake' in sighting Comments
comments <- tolower(sightings$Comments)  #make Comments all lower case (to ease the search)
issues <- sort(unique(c(grep("ancel",comments), grep("istake",comments),grep("ccident",comments),grep("test",comments))))
if(length(issues)!=0){
  beep(10)
  x <- readline(prompt = cat(paste("\nBased on sighting Comments, Sgt ID: ", toString(sightings[issues,]$Sgt.ID)," may be Mistake/Cancel sightings. Please examine these sightings closely before answering: Are any of these sightings mistakes (i.e. should be cancelled)?   [click here & type Yes or No & hit Enter]    \n\n", sep=" ")))
  if(x %ni% c("NO","no","No","N","n")){
    beep(10)
    stop("Please make sure that the mistake/cancel sightings have 'MISTAKE/CANCEL' Species designation in the Sightings table and re-run this code.", call. = FALSE)
  }
}

if(nrow(sightings)!=0){
  rownames(sightings) <- c(1:nrow(sightings))
}

cat("DONE")


cat("\n\nMaking sure Observer data is filled in in the Sightings table...")
#Make sure all Observer data is filled in -- if Observer is blank, assume it is the on-effort observer on that side at the time. This has to happen after Mistake/Cancel sightings have been removed (as they usually don't have Observer info filled in)

if(nrow(sightings[which(sightings$Incidental.Sighting==FALSE & is.na(sightings$SightedBy)),])!=0){
  issues <- which(sightings$Incidental.Sighting==FALSE & is.na(sightings$SightedBy))
  gc.maybe <- NA
  for(i in seq_along(issues)){
    if(is.na(sightings[issues[i],]$Side)){
      gc.maybe <- c(gc.maybe, sightings[issues[i],]$Sighting.)
    } else {
      if(sightings[issues[i],]$Side=="Port"){
        port.effort <- effort[max(which(!is.na(effort$Port.Observer) & effort$time_index <= sightings[issues[i],]$time_index)),]$Port.Observer
        sightings[issues[i],]$SightedBy <- port.effort
      }
      if(sightings[issues[i],]$Side=="Starboard"){
        stbd.effort <- effort[max(which(!is.na(effort$Starboard.Observer) & effort$time_index <= sightings[issues[i],]$time_index)),]$Starboard.Observer
        sightings[issues[i],]$SightedBy <- stbd.effort
      }
    }
  }
  gc.maybe <- gc.maybe[-is.na(gc.maybe)]
  if(length(gc.maybe)!=0){
    stop(paste("There is at least one record in the Sightings table where both SightedBy and Side are not filled in: Sgt ID ", toString(gc.maybe), "  Please examine each of these records, fill SightedBy and Side in the Sightings table with your best guess and re-run this code.", sep = " "))  #Myst does not have protocol for high density/area counts if this is the reason for the lack of information...
  }
}
cat("DONE")

cat("\n\nLooking for incidental sightings...")

#1) BY SPECIES NAME
#Make sure all seal. sea lion, and sea otter sightings are incidental
if(length(grep("Sea",sightings$Species))!=0){
  sightings[grep("Sea",sightings$Species),]$Incidental.Sighting <- TRUE
  rownames(sightings) <- c(1:nrow(sightings))
}

#2) BY Bearings > +90 or < -90:
if(length(which((sightings$Bearing > 90 | sightings$Bearing < -90) & sightings$Incidental.Sighting==FALSE))!=0){
  #beep(10)
  sightings[which((sightings$Bearing > 90 | sightings$Bearing < -90) & sightings$Incidental.Sighting==FALSE),]$Incidental.Sighting <- TRUE
}

#3) Check Comments that suggest incidental sighting
#This doesn't take into consideration sightings that are marked incidental already.
xyz<-sightings[which(sightings$Incidental.Sighting=="False"),]
issues <-sort(unique(c(grep("dental",xyz$Comments), grep("off",xyz$Comments), grep("OFF",xyz$Comments), grep("Off",xyz$Comments), grep("clos",xyz$Comments), grep("ONC",xyz$Comments),grep("Clos",xyz$Comments),grep("onc",xyz$Comments))))

if(length(issues)!=0){
  beep(10)
  x <- readline(prompt = cat(paste("\nBased on sighting Comments, Sightings #", toString(xyz[issues,]$Sgt.ID),"may be incidental sightings. Please examine these sightings closely before answering: Are any of these sightings incidental?   [click here & type Yes or No & hit Enter]    \n\n", sep=" ")))
  if(x %ni% c("NO","no","No","N","n")){
    beep(10)
    stop("Please make sure that if these are incidental sightings that they are infact assigned the value 'TRUE' in the Incidental.Sighting field in Sightings table and re-run this code.", call. = FALSE)
  }
}
remove(xyz)
cat("DONE")

cat("\n\nAdding survey ID...")
sightings$SurveyID <- surveyid
sightings <- sightings[,c(ncol(sightings),1:(ncol(sightings)-1))]
head(sightings)
cat("DONE")

cat("\n\nChecking for unique sighting keys...")
#check to make sure that surveyid-gpsindex-sighting# provides unique key for all sightings
sightings$key <- paste(sightings$SurveyID,sightings$GPSIndex,as.character(sightings$Sgt.ID), sep = "-")
rep.sight <- as.data.frame(table(sightings$key))
if(nrow(rep.sight[which(rep.sight$Freq!=1),])!=0){
  #beep(10)
  stop("Oops! There are some non-unique sighting keys in our sightings table. At this point in processing, this should be impossible... So if this message appears, call Eva to sort this out.", call. = FALSE)
}

cat("DONE")



cat("\n\nFilling in all best count fields...")
#Make sure all best # fields are populated.
if(length(which(is.na(sightings$BestNumber)))!=0){
  index <- which(is.na(sightings$BestNumber)) #positions with blank Best Number
  for(i in seq_along(index)){
    if(is.na(sightings[index[i],]$MinNumber) & is.na(sightings[index[i],]$MaxNumber)){
      sightings[index[i],]$BestNumber <- 1 #if nothing is filled in best/min/max, make Best Number = 1
    } else {
      sightings[index[i],]$BestNumber <- floor(mean(c(sightings[index[i],]$MinNumber,sightings[index[i],]$MaxNumber),na.rm=TRUE)) #If min/max are both filled in, take the mean (rounded down to nearest whole number). If only one of min/max are filled out, use that number
    }
  }
}
cat("DONE")


#Create final Effort table
#======================================================
cat("\n\n\n  DEALING WITH EFFORT")
cat("\n-------------------------------------")

#Examine Activity
#--------------------------------
cat("\n\nExamining Activity entries...")
#Look at Effort entries with no Activity (Status) assigned
if(length(which(is.na(effort$Status)))!=0){
  issues<-which(is.na(effort$Status))
  beep(10)
  stop(paste("\nOops! There are some Effort entries where Status is not filled in. Please make sure all Effort records have an Status assigned and re-run this code.  Row ",toString(issues),"has issues, Please correct this in the effort table and re-run this code.", sep = " "), call. = FALSE)
}


#Check interval between Effort entries
#--------------------------------------------------
cat("\n\nChecking time interval between event entries...")

#Make sure that Effort table is ordered by GPSIndex, with consecutive row names
effort <- effort[order(effort$GPSIndex),]
rownames(effort) <- c(1:nrow(effort))

#Event entries will only be considered valid for this analysis if Observers, Beaufort, Visib, Instrument and Platform has been entered
cols <- c("Platform","Port.Observer","Starboard.Observer","Effort_Instrument","PORT.Visibility","Beaufort","STBD.Visibility")
ind <- which(with(effort, c(FALSE, Status[-1L] != Status[-length(Status)]))) #each time Status switches

# To include first effort entry correctly when the first effort entry of survey is ON EFFORT - as in June 2022
if(effort$Status[1]=="ON") ind <- c(1,ind)

#When Status is switching, Action should be 'Changing effort status':
if(length(which(effort[ind,]$Action %in% c("Weather update","Observer rotation")))!=0){
  effort[ind[which(effort[ind,]$Action %in% c("Weather update","Observer rotation"))],]$Action <- "Changing effort status"
}

on <- which(effort$Status=="ON") #all ON effort records
ON.start <- on[which(on %in% ind)] #each time ON effort segment begins
time.issues <- NA
for(i in seq_along(ON.start)){
  ON.end <- min(ind[which(ind > ON.start[i])]) #get start of next activity
  times <- effort[ON.start[i]:ON.end,]$time_index[which(as.data.frame(rowSums(is.na(effort[ON.start[i]:ON.end,paste(cols)])))[,1] == 0)]
  if(length(which(as.numeric(diff(times)/60)>45))!=0){ #diff(times) provides difference in seconds - we want the difference in minutes
    x <- effort[ON.start[i]:ON.end,]$time_index[which(as.data.frame(rowSums(is.na(effort[ON.start[i]:ON.end,paste(cols)])))[,1] == 0)][which(as.numeric(diff(times)/60)>45)+1]
    time.issues <- c(time.issues,as.character(x))
  }
}
time.issues <- time.issues[-is.na(time.issues)]
if(length(time.issues)!=0){
  beep(10)
  stop(paste("Oops! There are some effort entries that are entered more than 45 minutes after the previous entry*: Time_local (PDT) ", toString(time.issues)," Please investigate and correct this in the Effort table and re-run this code. *Only counting entries with Observers, Beaufort, Visib, Instrument and Platform filled in -- this means that a previous entry within 45 minutes of the flagged entry may simply be missing one of these attributes and the error can be reconciled by filling it in."), call. = FALSE)
}

cat("DONE")

#Merge effort & gps tables
#--------------------------------------------------
cat("\n\nMerging Effort & GPS tables...")
effort <- merge(gps, effort, by="GPSIndex",all.x=TRUE,all.y=TRUE)
effort <- effort[order(effort$GPSIndex),]
rownames(effort) <- c(1:nrow(effort))
cat("DONE")

#Complete Vessel information
#--------------------------------------------------
effort$Vessel <- vessel

#Fill in Status
#--------------------------------------------------
cat("\n\nFilling down Status...")
fe <- min(which(!is.na(effort$Status))) #first effort entry
if(fe!=1){ #Anything before first effort entry will have OFF effort
  effort[1:(fe-1),]$Status <- "OFF"
}
effort$Status <- fill(effort$Status)
cat("DONE")

#Fill down conditions
#--------------------------------------------------
cat("\n\nFilling down survey conditions...")
#Fill in conditions for ON effort only
#Find row numbers of the start of all ON effort segments
ind <- which(with(effort, c(FALSE, Status[-1L] != Status[-length(Status)]))) #each time Status switches

# To include first effort entry correctly when the VERY first effort entry of survey is ON EFFORT - as in June 2022 when trackline wasn't logging for first hour or so
if(effort$Status[1]=="ON") ind <- c(1,ind)

on <- which(effort$Status=="ON") #all ON effort records
ON.start <- on[which(on %in% ind)] #each time ON effort segment begins
#Required conditions:
#Conditions required for analysis -- these MUST be filled out for all ON-effort records
#Speed is a required condition but has already been checked for completion
req.conditions <- c("Platform","Transect.ID","Effort_Instrument","PORT.Visibility","Beaufort","STBD.Visibility","Swell","Glare","Left.Glare.Limit","Right.Glare.Limit","Cloud.Cover","Precipitation")
# ADD IN ???
#Find any ON-effort start records with at least one empty condition required for analysis
#Fix missing effort data
effort$Glare[ON.start] <- ifelse(is.na(effort$Glare[ON.start]), "None", effort$Glare[ON.start])
effort$Left.Glare.Limit <- ifelse(effort$Glare=="None", "None", effort$Left.Glare.Limit)
effort$Right.Glare.Limit <- ifelse(effort$Glare=="None", "None", effort$Right.Glare.Limit)
effort$Precipitation[ON.start] <- ifelse(is.na(effort$Precipitation[ON.start]), "Clear", effort$Precipitation[ON.start])

rec.bl <- which(as.data.frame(rowSums(is.na(effort[ON.start,paste(req.conditions)])))[,1] != 0)
# TO DO
# EK edit for Oct 2020 data, remove these = expect extra lines entered in Mysti when arrows hit on keyboard
# if(year == 2020 & m == 10){
#   effort <- effort[which(!effort$GPSIndex %in% effort[ON.start[rec.bl],]$GPSIndex),]
#   length(rec.bl) <- 0
# }
if(length(rec.bl) != 0){
  beep(10)
  stop(paste("Oops! There are important Effort records that are missing required data: GPS_Index = ", toString(as.character(effort[ON.start[rec.bl],]$GPSIndex))," Please make sure that the Effort table has Platform,Transect.ID,Effort_Instrument,PORT.Visibility,Port.Beaufort,STBD.Visibility,STBD.Beaufort,Swell,Glare,Cloud.Cover,Precipitation completed for these records and re-run this code.", sep = " "), call. = FALSE)
}

#Pull down all required conditions for all ON-effort segments
#Since all initial values are filled in, we will use the fill function
effort[which(effort$Status=="ON"),paste(req.conditions)] <- lapply(effort[which(effort$Status=="ON"),paste(req.conditions)], fill)
#Other conditions:
#Pull down conditions within ON-effort segments
conditions <- c("Port.Observer","Starboard.Observer","DataRecorder")

#As not all initial values are necessarily filled in, we will use the fillNAgaps function and go segment by segment
for(i in seq_along(ON.start)){ #loop through each segment
  ON.end <- min(ind[which(ind > ON.start[i])])-1 #get the last row of the segment
  effort[ON.start[i]:ON.end, paste(conditions)] <- lapply(effort[ON.start[i]:ON.end, paste(conditions)], fillNAgaps)
}
cat("DONE")

#Examine Speed
#--------------------------------
cat("\n\nExamining vessel speed...\n")
effort <- effort[order(effort$GPSIndex),]
rownames(effort) <- c(1:nrow(effort))
#Look at segments of low speed
#if any involve ONC, the entire slow segment is assigned Status 'ONC'. If they don't, the slow segment is assigned Status 'OFF'. If the segment involves ON, ONC, and OFF, assigning ONC or OFF depends on the order; in this case, we will throw an error for those segments to be handled manually.
effort$speed.category <- NA
effort[which(effort$Speed<5),]$speed.category <- "slow"
effort[which(is.na(effort$speed.category)),]$speed.category <- "at-speed"
ind <- with(effort, c(FALSE, speed.category[-1L] != speed.category[-length(speed.category)])) #everytime speed category changes
sp.start <- c(1,which(ind)) #start of speed segments
sp.end <- c(which(ind)-1,nrow(effort)) #end of speed segments
slow.start <- sp.start[which(effort[sp.start,]$speed.category=="slow")] #start of slow segments
slow.end <- sp.end[which(effort[sp.start,]$speed.category=="slow")] #end of slow segments

#Look at all slow speed segments that include ON effort status
temp.activity <- data.frame(slow.seg.start.index=slow.start, slow.seg.end.index=slow.end, activity=rep(NA,length(slow.start)))
for(i in 1:length(slow.start)){
  temp.activity[i,]$activity <- toString(rle(effort[slow.start[i]:slow.end[i],]$Status)$values)
}

#Filter for slow effort segments that include ON effort status (*ONC might be confused in this for multi-status segments)
temp.activity<-temp.activity[grep("ON",temp.activity$activity),]

if(nrow(temp.activity) > 0){ # Only run this alteration if there are instances of slow speed while ON EFFORT
  rownames(temp.activity) <- c(1:nrow(temp.activity))
  slow.segment.information <- data.frame(start.time=as.POSIXct(rep(NA,length(temp.activity$activity)),tz="America/Los_Angeles"), end.time=as.POSIXct(rep(NA,length(temp.activity$activity)),tz="America/Los_Angeles"), total.time.min=rep(NA,length(temp.activity$activity)), Status = temp.activity$activity)
  slow.segment.issues.start.index <- temp.activity$slow.seg.start.index
  slow.segment.issues.end.index <- temp.activity$slow.seg.end.index
  for(i in 1:length(slow.segment.issues.start.index)){
    slow.segment.information[i,]$start.time <- as.POSIXct(as.character(effort[slow.segment.issues.start.index[i],]$GpsTime),tz="America/Los_Angeles")
    slow.segment.information[i,]$end.time <- as.POSIXct(as.character(effort[slow.segment.issues.end.index[i],]$GpsTime),tz="America/Los_Angeles")
  }
  slow.segment.information$total.time.min <- as.numeric(difftime(slow.segment.information$end.time,slow.segment.information$start.time,units = "mins"))

  if(length(which(slow.segment.information$total.time.min>1))!=0){
    print(slow.segment.information[which(slow.segment.information$total.time.min>1),])
    x <- readline(prompt = cat(paste("Do any of these slow speed intervals need Status adjustments?   [click here & type Yes or No & hit Enter]    \n\n"), sep = " "))
    if(x %in% c("YES","yes","Yes","Y","y")){
      stop("Take a look at the data and fix it. There are instances where Speed drops below 5 knots for more than 1 minute, while ON effort. This requires inspection to rule out major issues.", call. = FALSE)
    }
  }
}
#The following loop is not needed for PRISMM (it automatically assigns Status changes based on non-PRISMM rules)
for(i in 1:length(slow.start)){
  act <- rle(effort[slow.start[i]:slow.end[i],]$Status)$values
  if(sum(act %ni% c("OFF"))!=0){  #slow speeds are acceptable when ONC or OFF effort. If this statement is true, it means ON effort is occurring sometime during the slow speed segment
    if(length(act)==1){  #length(act)==1, then whole slow speed segment is ON effort --> change segment to OFF effort
      effort[slow.start[i]:slow.end[i],]$Status <- "OFF"
    } else {
      if(length(unique(act))==2){ #if there are only 2 unique Status values it means that only ON and one of OFF/ONC are present, then slow speed segment is composed of ON/ONC or ON/OFF --> change segment to non-ON effort value in 'act'
        effort[slow.start[i]:slow.end[i],]$Status <- unique(act[which(act %in% c("OFF"))])
      } else { #if there are more than 2 unique Status values in the slow speed segment it means that OFF, ONC and ON are present in the slow speed segment. There are too many combinations for me to code for -- will throw error and have the situation dealt with manually...
        #beep(10)
        stop(paste("Oops! There is on-effort Status when the ship's speed is under 5 knots. Please adjust Status entries in the Effort table made between ", effort[slow.start[i],]$Time.PDT , " and ", effort[slow.end[i],]$Time.PDT , " to either ONC or OFF Status, according to adjacent entries, then re-run this code.", sep=""), call. = FALSE)
      }
    }
  }
}
cat("DONE")

effort$speed.category <- NULL


#Assign SEQUENCE
#--------------------------------
#We are using the term SEQUENCE/SEQ_ID to identify segments of continuous track where effort status and Transect ID are the same
#Pre-PRISMM this was called Segment_ID, however, we do not want to confuse the term "segment" with what one might call a leg/subset of a given transect
cat("\n\nCreating Sequence field...")
effort <- effort[order(effort$GPSIndex),]
rownames(effort) <- c(1:nrow(effort))
#SEQ_ID will change every time Status and/or Transect ID changes
# effort$dummy.stat.trans <- paste(effort$Status,effort$subTransect.ID,sep="-") EK edit
effort$dummy.stat.trans <- paste(effort$Status,effort$subTransect.ID,sep="-")
n <- rle(effort$dummy.stat.trans)$length #get number of records in each Status segment
effort$SEQ_ID <- rep(c(1:length(n)),n)
effort$SEQ_ID <- paste(surveyid,effort$SEQ_ID,sep="_")
effort$dummy.stat.trans <- NULL
lev <- unique(effort$SEQ_ID)
effort$SEQ_ID %<>% factor(levels = lev)
cat("DONE")

#Add Final.Transect.ID field.
effort %<>% arrange(GPSIndex) #year(GpsTime),month(GpsTime),day(GpsTime),Transect.ID
effort$Final.Transect.ID <- NA
effort[which(!is.na(effort$Transect.ID)),]$Final.Transect.ID <- paste(surveyid, effort[which(!is.na(effort$Transect.ID)),]$Transect.ID,sep="")
lev <- unique(effort$Final.Transect.ID)
effort$Final.Transect.ID %<>% factor(levels = lev)

#Check to make sure no effort falls on land
#---------------------------------------------------------
cat("\n\nMaking sure trackline doesn't intercept land (this may take several minutes)...")
#Import coast shapefile:

# load land shapefile
#-------- EK edit (for diff shapefile, and using sf instead of sp---------------
if(!exists("bc_coast")){
  # bc_coast <- readOGR("C:\\Users\\keppele\\Documents\\ArcGIS\\basemaps\\CoastLand.shp", verbose = FALSE) #Load in CHS coastline shapefile (in WGS84)
  # bc_coast <- sf::st_read(dsn="C:\\Users\\keppele\\Documents\\ArcGIS\\basemaps\\CoastLand.shp") %>%
  # st_as_sf() %>%
  #   st_transform(crs = 4326) #Load in CHS coastline shapefile (in WGS84)
  # bc_coast <- st_transform(bc_coast, CRSobj = "+proj=utm +zone=9N +datum=WGS84 +towgs84=0,0,0")
  # EK edit: change to using sf package
  bc_coast <- sf::st_read(dsn="C:\\Users\\keppele\\Documents\\ArcGIS\\basemaps\\CoastLand.shp") %>%
    st_transform(crs = 3005) %>% dplyr::select(geometry)
}
cat("\n - Land shapefile loaded")
#Make trackpoints
# BP <- SpatialPointsDataFrame(cbind(effort$Longitude,effort$Latitude), data=effort, proj4string=CRS("+proj=longlat"))
# BP <- spTransform(BP, CRSobj = "+proj=utm +zone=9N +datum=WGS84 +towgs84=0,0,0")
# EK edit: change to using sf package
BP <- st_as_sf(effort, coords = c("Longitude","Latitude"),crs=4326) %>%
  st_transform(BP, crs=3005)

cat("\n - Track points constructed")
#Clip land by extent of trackpoints
if(!exists("bc_clip")){
  # bc_clip <- gClip(bc_coast,BP)
  bc_clip <- st_crop(bc_coast, BP)
}
cat("\n - Land extent clipped")
#Find any points that fall on land
LAND <- BP[bc_clip,]
# if(nrow(LAND@data[which(LAND@data$Status!="OFF"),])!=0){
# EK edit : edited for sf functionality as no longer an S4 object
if(nrow(LAND[which(LAND$Status!="OFF"),])!=0){

  beep(10)
  stop(paste("\nOops! Some points from our GPS track fall on land. Time Created (UTC): ", toString(as.character(LAND@data$GpsTime.UTC)), "Please adjust these positions in the GPS Data table and re-run this code.", sep = ""), call. = FALSE)
} else {
  cat("\nDONE")
}

#Extend ON-effort SEQ_ID to include end point of all ON effort segments
#----------------------------------------------------------------------------
#i.e. the first record proceeding the last ON-effort segment record
cat("\n\nIncorporating end points of ON-effort segments...")
effort$ONSEQ_ID <- NA
effort[which(effort$Status=="ON"),]$ONSEQ_ID <- effort[which(effort$Status=="ON"),]$SEQ_ID
sdf <- as.data.frame(cbind(rle(effort$ONSEQ_ID)$values,rle(effort$ONSEQ_ID)$lengths))
sdf$V3 <- cumsum(sdf$V2) #V1 = ONsegID, V2 = number of records in segment, V3 = row number of last record in given segment
sdf <- sdf[which(sdf$V1 %in% sort(unique(effort$ONSEQ_ID))),]
rownames(sdf) <- c(1:nrow(sdf))
for(i in 1:nrow(sdf)){
  if(is.na(effort[sdf[i,]$V3+1,]$ONSEQ_ID)){  #check to see if proceeding row has ONSEQ_ID assigned (if not, the record is OFF|ONC and we simply assign the transect ID and SEQID of our transect of interest)
    effort[sdf[i,]$V3+1,]$ONSEQ_ID <- effort[sdf[i,]$V3,]$ONSEQ_ID
    effort[sdf[i,]$V3+1,]$Final.Transect.ID <- effort[sdf[i,]$V3,]$Final.Transect.ID
  } else { #if ONSEQ_ID is assigned, this is a switch in transects - we do not want to override the transect ID or SEQ_ID. Instead we will make a duplicate of this row to assign values to the endpoint of our transect of interest.
    effort <- rbind(effort,effort[sdf[i,]$V3+1,])
    rownames(effort) <- c(1:nrow(effort))
    effort[nrow(effort),]$ONSEQ_ID <- effort[sdf[i,]$V3,]$ONSEQ_ID
    effort[nrow(effort),]$Final.Transect.ID <- effort[sdf[i,]$V3,]$Final.Transect.ID
  }
}
lev <- unique(effort$ONSEQ_ID)
effort$ONSEQ_ID %<>% factor(levels = lev)

#Sort effort table so that endpoint rows are stored in chronological order
effort <- effort[order(effort$GpsTime.UTC,effort$ONSEQ_ID),]
rownames(effort) <- c(1:nrow(effort))
cat("DONE")

#Limit effort data to ON-effort only
#------------------------------------
cat("\n\nLimiting effort data to ON-effort segments only...")
ONeffort <- effort[which(!is.na(effort$ONSEQ_ID)),]
rownames(ONeffort) <- c(1:nrow(ONeffort))


#Filter effort table to fields of interest only for exporting table # EK edit - remove stbd/port beaufort; GpsTime.PDT changed to PST temporarily - need to address this
#--------------------------------------------------------------------
#I haven't run fill on instrument, platform, etc. IF run into issues will have to address this.
if(survey$Vessel_code == "FR"){
  Effort.Final<- ONeffort[,c("Vessel","GPSIndex","GpsTime.UTC","GpsTime","Latitude","Longitude","Speed","Heading","Status","Transect.ID","Final.Transect.ID","ONSEQ_ID","Platform","Franklin.Hut","Port.Observer","Starboard.Observer","Effort_Instrument","DataRecorder","PORT.Visibility","Beaufort","STBD.Visibility","Swell","Glare","Left.Glare.Limit","Right.Glare.Limit","Cloud.Cover","Precipitation")]
  names(Effort.Final)<-c("Vessel","GPSIndex","GpsT.UTC","GpsT","Latitude","Longitude","Speed","Heading","Status","Raw.T.ID","Final.T.ID","ONSEQ_ID","Platform","FR.Hut","Port.Obs", "Stbd.Obs","E_Instr","Data","Port.Vis","Bf","Stbd.Vis","Swell","Glare","L.G.Limit","R.G.Limit","CloudCover","Precip") # EK edit - combine bf and remove windspeed
}else{
  Effort.Final<- ONeffort[,c("Vessel","GPSIndex","GpsTime.UTC","GpsTime","Latitude","Longitude","Speed","Heading","Status","Transect.ID","Final.Transect.ID","ONSEQ_ID","Platform","Port.Observer","Starboard.Observer","Effort_Instrument","DataRecorder","PORT.Visibility","Beaufort","STBD.Visibility","Swell","Glare","Left.Glare.Limit","Right.Glare.Limit","Cloud.Cover","Precipitation")]
  names(Effort.Final)<-c("Vessel","GPSIndex","GpsT.UTC","GpsT","Latitude","Longitude","Speed","Heading","Status","Raw.T.ID","Final.T.ID","ONSEQ_ID","Platform","Port.Obs", "Stbd.Obs","E_Instr","Data","Port.Vis","Bf","Stbd.Vis","Swell","Glare","L.G.Limit","R.G.Limit","CloudCover","Precip") # EK edit - combine bf and remove windspeed
}
Effort.Final$SurveyID <- surveyid
Effort.Final$iteration <- iteration
cat("\n\n\n
       SIGHTING POSITION CORRECTION
    #################################################################################")


#Sighting position correction (for ON-effort survey sightings on dedicated surveys)
#-----------------------------------------------------------------------------------------
cat("\n\nRe-formatting incidental sightings...")
#Re-format Incidental Sightings field to conform with traditional 'Event' field
sightings$Event <- NA

sightings$Incidental.Sighting<-toupper(sightings$Incidental.Sighting)

if(length(unique(sightings$Incidental.Sighting))!=1){
  sightings[which(sightings$Incidental.Sighting==TRUE),]$Event <- "Incidental sighting"
  sightings[which(sightings$Incidental.Sighting==FALSE),]$Event <- "Survey sighting"
} else {
  sightings$Event <- "Survey sighting"
}
sightings$Incidental.Sighting <- NULL
cat("DONE")

source(paste(cemore,u,"data_processing_scripts",u,"SIGHTING POSITION CORRECTION_WORKING_CEMORE.R",sep=""))

######################################################
#        EXPORT DATA
######################################################
cat("\n\n\nEXPORTING FINAL PRODUCTS...")

#Export Effort Table
#--------------------------------------
cat("\n\n\n Effort Table...")
#dataEffort Tables

#Remove files already present in the export folder
# if(length(list.files(paste(getwd(),u,"OUTPUT FILES",u,"dataEffort table",u,"dataEffort", sep=""), full.names = TRUE))!=0){
#   file.remove(list.files(paste(getwd(),u,"OUTPUT FILES",u,"dataEffort table",u,"dataEffort", sep=""), full.names = TRUE))
# }
#Final Effort Table
# write.table(Effort.Final,paste(getwd(),u,"OUTPUT FILES",u,"dataEffort table",u,"PRISMM_dataEffort",surveyID.abbrev, ".txt", sep = ""), sep="\t",row.names=F)
if(data.source=="cemore"){
write.table(Effort.Final,paste(getwd(),u,paste0("OUTPUT FILES ",data.source),u,"dataEffort table",u,data.source,"_Effort_",year,"_",month,".txt", sep = ""), sep="\t",row.names=F)
  cat(paste("\n Saved as: '",data.source,"_dataEffort",year,"_",month, ".txt'", sep = ""))
}
if(data.source=="mmcp"){
write.table(Effort.Final,paste(getwd(),u,paste0("OUTPUT FILES ",data.source),u,"dataEffort table",u,data.source,"_Effort_",year,"_",month,"_",vessel,".txt", sep = ""), sep="\t",row.names=F)
  cat(paste("\n Saved as: '",data.source,"_dataEffort",year,"_",month,"_",vessel, ".txt'", sep = ""))
}


#Export transit table for review
#--------------------------------------
# cat("\n\n\n TRANSIT Review Table...")
# write.table(review.transects,paste(getwd(),u,"OUTPUT FILES",u,"dataEffort table",u,"PRISMM_REVIEW_transects",surveyID.abbrev, ".csv", sep = ""), sep=",",row.names=F)
# cat(paste("\n Saved as: 'PRISMM_REVIEW_transects",surveyID.abbrev, ".csv'", sep = ""))


#Export transit shapefiles for review
#--------------------------------------
# cat("\n\n\n TRANSIT Review Shapefiles...")
# if(length(list.files(paste(getwd(),u,"OUTPUT FILES",u,"Review transect segments", sep=""), full.names = TRUE))!=0){
#   file.remove(list.files(paste(getwd(),u,"OUTPUT FILES",u,"Review transect segments", sep=""), full.names = TRUE))
# }
# #
#  Review.Index<-sort(as.numeric(as.character(unlist(strsplit(review.transects$SEQIDS,split=",")))))
# #
# if(length(Review.Index)!=0){
#   for(i in 1:length(Review.Index)){
#     BP <- SpatialPointsDataFrame(cbind(Effort.Final[which(Effort.Final$ONSEQ_ID==Review.Index[i]),]$Longitude,Effort.Final[which(Effort.Final$ONSEQ_ID==Review.Index[i]),]$Latitude), data=Effort.Final[which(Effort.Final$ONSEQ_ID==Review.Index[i]),], proj4string=CRS("+proj=longlat"))
#     BP <- spTransform(BP, CRSobj = "+proj=utm +zone=9N +datum=WGS84 +towgs84=0,0,0")
#     writeOGR(BP, dsn = paste(getwd(),u,"OUTPUT FILES",u,"Review transect segments", sep=""), layer = paste(surveyid,"ONSEQID", Review.Index[i], "Points", sep = "_"), driver = "ESRI Shapefile")
#   }
# }
# cat("DONE")

#Export Replicated Transect Table
#--------------------------------------
cat("\n\n\n Replicated Transect Table...")
#Replicated Transect Table
# EK edit not needed for CeMoRe write.table(replicated.transect,paste(getwd(),u,"OUTPUT FILES",u,"dataEffort table",u,"PRISMM_replicated_transect",surveyID.abbrev, ".csv", sep = ""), sep=",",row.names=F)
# cat(paste("\n Saved as: 'PRISMM_replicated_transect",surveyID.abbrev, ".csv'", sep = ""))

#
# #Export Replicated Transect Shapefiles - EK edit - not needed for CeMoRe
# #--------------------------------------
# cat("\n\n\n Replicated Transect Shapefiles...")
# if(length(list.files(paste(getwd(),u,"OUTPUT FILES",u,"Duplicate transect segments", sep=""), full.names = TRUE))!=0){
#   file.remove(list.files(paste(getwd(),u,"OUTPUT FILES",u,"Duplicate transect segments", sep=""), full.names = TRUE))
# }
#
# Dup.Index<-sort(as.numeric(as.character(unlist(strsplit(replicated.transect$SEQIDS,split=",")))))
#
# if(length(Dup.Index)!=0){
#   for(i in 1:length(Dup.Index)){
#     BP <- SpatialPointsDataFrame(cbind(Effort.Final[which(Effort.Final$ONSEQ_ID==Dup.Index[i]),]$Longitude,Effort.Final[which(Effort.Final$ONSEQ_ID==Dup.Index[i]),]$Latitude), data=Effort.Final[which(Effort.Final$ONSEQ_ID==Dup.Index[i]),], proj4string=CRS("+proj=longlat"))
#     BP <- spTransform(BP, CRSobj = "+proj=utm +zone=9N +datum=WGS84 +towgs84=0,0,0")
#     writeOGR(BP, dsn = paste(getwd(),u,"OUTPUT FILES",u,"Duplicate transect segments", sep=""), layer = paste(surveyid,"ONSEQID", Dup.Index[i], "Points", sep = "_"), driver = "ESRI Shapefile")
#   }
# }
# cat("DONE")

#Export Sightings Table
#---------------------------------
cat("\n\n\n Sightings Table...")
#Remove files already present in the export folder
# if(length(list.files(paste(getwd(),u,"OUTPUT FILES",u,"dataSightings table", sep=""), full.names = TRUE))!=0){
#   file.remove(list.files(paste(getwd(),u,"OUTPUT FILES",u,"dataSightings table", sep=""), full.names = TRUE))
# }
# write.table(positions,paste(getwd(),u,"OUTPUT FILES",u,"dataSightings table",u,"PRISMM_dataSightings",surveyID.abbrev, ".txt", sep = ""), sep="\t",row.names=F)
if(data.source=="cemore"){
  write.table(positions,paste(getwd(),u,paste0("OUTPUT FILES ",data.source),u,"dataSightings table",u,data.source,"_Sightings_",year,"_",month,".txt", sep = ""), sep="\t",row.names=F)
  cat(paste("\n Saved as: '",data.source,"_dataSightings",year,"_",month,".txt'", sep = ""))
}
if(data.source=="mmcp"){
    write.table(positions,paste(getwd(),u,paste0("OUTPUT FILES ",data.source),u,"dataSightings table",u,data.source,"_Sightings_",year,"_",month, "_", vessel,".txt", sep = ""), sep="\t",row.names=F)
    cat(paste("\n Saved as: '",data.source,"_dataSightings",year,"_",month, "_",vessel,".txt'", sep = ""))
}

#Export Sightings shapefile
#---------------------------------
cat("\n\n\n Sightings Corrected Position Shapefile...")
#Export shapefile (true positions):
# EK edit: writeOGR deprecated, switch to st_write which has more restrictions on field types and length of file/field names, etc.
AP <- SpatialPointsDataFrame(cbind(positions$"final.lon",positions$final.lat), data=positions, proj4string=CRS("+proj=longlat")) %>%
  st_as_sf() %>% st_transform(crs="+proj=utm +zone=9N +datum=WGS84 +towgs84=0,0,0")
AP <- AP %>% mutate(gpsdate=date(gpstimeutc),
                    gpstime=paste(hour(gpstimeutc),minute(gpstimeutc),second(gpstimeutc),sep="_"),
                    tind_date=date(time_index),
                    tind_time=paste(hour(time_index),minute(time_index),second(time_index),sep="_"),
                    sid=substr(SurveyID,8,17)) %>%
  dplyr::select(-c(SurveyID, gpstimeutc, time_index))
# AP <- spTransform(AP, CRSobj = proj4string(bc_coast))
# AP <- spTransform(AP, CRSobj = "+proj=utm +zone=9N +datum=WGS84 +towgs84=0,0,0")
#Remove files already present in the export folder
# if(length(list.files(paste(getwd(),u,"OUTPUT FILES",u,"dataSightings_True Positions", sep=""), full.names = TRUE))!=0){
#   file.remove(list.files(paste(getwd(),u,"OUTPUT FILES",u,"dataSightings_True Positions", sep=""), full.names = TRUE))
# }

# if(data.source=="cemore"){
#   # writeOGR(AP, dsn = paste(getwd(),u,paste0("OUTPUT FILES ",data.source),u,"dataSightings_True Positions",u,data.source,"_Sightings_truePositions_WGS84_UTM9N_",year,"_",month,".shp", sep=""), layer = paste("dataSightings",year,"_",month,"_truePositions_WGS84_UTM9N_",vessel, sep = ""), driver = "ESRI Shapefile", overwrite_layer = T)
#   st_write(paste(getwd(), u,
#   paste0("OUTPUT FILES ",data.source), u,
#   "dataSightings_True Positions", u,
#   data.source,"_Sightings_truePositions_WGS84_UTM9N_",year,"_",month,".shp", sep=""), append=F)}
# if(!data.source=="cemore"){
#   st_write(AP, dsn = paste(getwd(),u,paste0("OUTPUT FILES ",data.source),u,"dataSightings_True Positions",u,data.source,"_Sightings_truePositions_WGS84_UTM9N_",year,"_",month,"_",vessel,".shp", sep=""), driver = "ESRI Shapefile")
# }
if(data.source=="cemore"){
  # writeOGR(AP, dsn = paste(getwd(),u,paste0("OUTPUT FILES ",data.source),u,"dataSightings_True Positions",u,data.source,"_Sightings_truePositions_WGS84_UTM9N_",year,"_",month,".shp", sep=""), layer = paste("dataSightings",year,"_",month,"_truePositions_WGS84_UTM9N_",vessel, sep = ""), driver = "ESRI Shapefile", overwrite_layer = T)
  # st_write(AP, dsn="OUTPUT FILES cemore/TEST.shp", driver="ESRI Shapefile",append=F)
  file.name <- paste0("OUTPUT FILES cemore/dataSightings_True Positions/cemore_WGS84_UTM9N_",year,"_",month,".shp") #prev file name format too long
  st_write(AP, dsn=file.name, driver="ESRI Shapefile",append=F)
}
if(!data.source=="cemore"){
  # writeOGR(AP, dsn = paste(getwd(),u,paste0("OUTPUT FILES ",data.source),u,"dataSightings_True Positions",u,data.source,"_Sightings_truePositions_WGS84_UTM9N_",year,"_",month,"_",vessel,".shp", sep=""), layer = paste("dataSightings",year,"_",month,"_truePositions_WGS84_UTM9N_",vessel, sep = ""), driver = "ESRI Shapefile", overwrite_layer = T)
  st_write(AP, dsn=paste0("OUTPUT FILES ", data.source, "/dataSightings_True Positions/", data.source,"_Sightings_truePositions_WGS84_UTM9N_",year,"_",month,"_",vessel,".shp"), driver="ESRI Shapefile",append=F)
}

cat(paste("\n Saved in: '",data.source,"_Sightings_truePositions_WGS84_UTM9N'", sep = ""))

cat(paste("\n\n\n******************************************************\n*** ",surveyid," CEMORE SURVEY DATA PROCESSING COMPLETE! ***\n****************************************************\n\n\n",sep=""))
beep(8)



###################################################
#######################################################
