###########################################################
# SHIP SURVEY SIGHTING POSITION CORRECTION
###########################################################

#Note: this code works from 'effort' & 'sightings' dataframes created in the 'SHIP SURVEY SOURCE CODE' process.

#Make all Effort headings lower case
#---------------------------------------
#This will let us discern effort columns from sightings columns when the two dataframes are merged
names(effort) <- tolower(names(effort))
# surveyid <- "cemore_feb_2021"
#add survey ID to effort table
effort$surveyid<-as.character(surveyid)
sightings$SurveyID<-as.character(surveyid)


#Merge sightings with effort data
#=================================================
DATA <- merge(sightings,effort,by.x=c("SurveyID","GPSIndex"),by.y=c("surveyid","gpsindex"),all.x=TRUE,all.y=FALSE)
DATA <- DATA[order(DATA$GPSIndex,DATA$Sgt.ID),]
rownames(DATA) <- c(1:nrow(DATA))

#Check that all sightings have unique key.  Duplication can be caused with this merge if a sighting lands on the exact gpsIndex which is repeated to generate the end points of each SEQ ID.  If this happens have to go back to original data and alter time ever so slightly so it lands on one of the gpsindexes not duplicated.
rep.sight <- as.data.frame(table(DATA$key))
rep.sight.list<-rep.sight[which(rep.sight$Freq>1),]
if(nrow(rep.sight[which(rep.sight$Freq!=1),])!=0){
  #beep(10)
  stop("Oops! A duplicate sightingID: ", toString(paste(rep.sight.list$Var1)), " has been created by the merge of the sighting and effort data as a sighting fell on the exact gpsIndex which is repeated to generate the end points of each SEQ ID.  If this happens have to go back to original data and alter time ever so slightly so it lands on one of the gpsindexes not duplicated.", call. = FALSE)
}

#Add data quality attributes
#=================================================
#See data quality spreadsheet (will make these binary columns, fill them out and then create a data quality text column that will put them together as a string)
qual <- c("OK","IA","ED","UB","UP","ZR","FC","UED","AB","ER","OM","EA")
DATA[,qual] <- NA
index <- which(names(DATA) %in% qual)
for(j in index) {as.numeric(DATA[,j])}

#Filter sightings data
#====================================
cat("\n\nFiltering for on-effort survey sightings...")
#We are only correcting positions for on-effort survey sightings
# temp allowance for non-survey sightings for plotting
# DATA[which(DATA$Sgt.ID == "S1f"),]$status <- "ON"
data <- DATA[which(DATA$Event=="Survey sighting" & DATA$status=="ON"),]

rownames(data) <- c(1:nrow(data))
cat("DONE")

#Look for on-effort survey sighting that are in fact high density counts sightings
#==================================================================================
#<<<<<MYST DOES NOT HAVE PROTOCOL FOR HIGH DENSITY/AREA COUNTS>>>>>
cat("\n\nLooking for on-effort survey sightings that are actually density counts...")
comments <- tolower(data$Comments) #make comments all lower case (to help with search)
dens.comments <- sort(unique(c(grep("density",comments), grep("area",comments),grep("count",comments))))
issues <- dens.comments[which(data[dens.comments,]$status=="ON")]
if(length(issues)!=0){
  beep(10)
  x <- readline(prompt = cat(paste("\nThere are some on-effort sightings whose Comments imply that we are doing a high density area count of animals. This activity requires that Effort's Activity is ONC (not ON) at the time: inGPS Index # (see data object)", toString(sort(data[issues,]$GPSIndex)),"  Please examine these sightings and the Effort table at the time of the sightings closely before answering: Are any of these sightings during high density area counts?   [click here & type Yes or No & hit Enter]    \n\n ****NOTE**** If this does not apply to your data (see data$commentsn), check for key words in comments (density, count, area) which trigger this error  \n\nv", sep=" ")))
  if(x %ni% c("NO","no","No","N","n")){
    #beep(10)
    stop("Please make sure that throughout the ENTIRE duration of high density area counts, Effort Activity is ONC. Change Effort entries accordingly and re-run this code.", call. = FALSE)
  }
}
cat("DONE")
#
# if(length(issues)!=0){
#   beep(10)
#   x <- readline(prompt = cat(paste("\nThere are some on-effort sightings whose Comments imply that we are doing a high density area count of animals. This activity requires that Effort's Activity is ONC (not ON) at the time: GPS Index #", toString(sort(data[issues,]$GPSIndex)),"  Please examine these sightings and the Effort table at the time of the sightings closely before answering: \n Are any of these sightings during high density area counts?   [click here & type Yes or No & hit Enter]    \n\n", sep=" ")))
#   if(x %ni% c("NO","no","No","N","n")){
#     #beep(10)
#     stop("Please make sure that throughout the ENTIRE duration of high density area counts, Effort Activity is ONC. Change Effort entries accordingly and re-run this code. \n\n
#          ****NOTE**** If this does not apply to your data, check for key words in comments (density, count, area) which trigger this error", call. = FALSE)
#
#   }
# }

#Deal with lat-longs
#====================================
#The GPS.Position and Bearing of the closest gpshit will be the position used to correct sighting positions. This is because accuracy of the sightings gps data is lost in the character string when its exported.  MInt shows the difference in time of the sighting creation and the gps-hit.
cat("\n\nFinalizing ship's position at time of sightings...")
data$SHIPLAT <- data$latitude
data$SHIPLON <- data$longitude
#This calculates the true bearing to the sighting using the ships heading at closest gps point, and the reported relative bearing to animal (+/-90)
data$BEARING <-tru.bearing(data$heading,data$Bearing)

#This takes mysticetus calculation of the true bearing of the sighting, taking the heading at the time the record was created (from the gps position/heading/time stamp).  As we are defaulting the the gps latitude/longitude closest in time to the sighting creation time, im wondering if it may be more consistent for all sightings to use the gpsheading associated with the gps lat/long, rather then the sighting heading associated with the sighting lat long.  In exploring the data, it shows that it makes very little difference (median=0 degrees diff, Mean=-1 degree difference).  Im going to to with the gps bearing to get away from using the unknown mysticetus calculations
#data$TRUE.BEARING <- data$MYST_Bearing.abs
#data$EXPLORE.BEARING<-tru.bearing(data$heading,data$Bearing)
#data$DIFF.BEARING<-data$MYST_Bearing.abs-data$EXPLORE.BEARING

#If for some reason there is a missing bearing at this point its either a gpshit didnt' calculate a heading, or no bearing was reported.  This code will use the MYST_Bearing to populate the BEARING field if the issue is a missing gps heading.  If no bearing was ever reported, then nothing can be done.
if(length(which(is.na(data$SHIPLAT)|is.na(data$SHIPLON)|is.na(data$BEARING)))!=0){
  index <- which(is.na(data$SHIPLAT)|is.na(data$SHIPLON)|is.na(data$BEARING))
  data[index,]$BEARING<-data[index,]$MYST_Bearing.abs
}

cat("DONE")

#Add final bearing to sighting data....as switched to using gps heading, don't need this chunk of code.  If the sighting doesn't have a bearing at this point, its not getting one and its because no bearing was reported.
#====================================
# cat("\n\nCalculating final bearing of sightings...")
# if(length(index)!=0){
#   data[index,]$TRUE.BEARING <- tru.bearing(data[index,]$heading,data[index,]$Bearing)
# }
# data$TRUE.BEARING <- round(data$TRUE.BEARING,0)
# cat("DONE")


#Deal with distances
#====================
#Previously in the code If there is a reported distance (regardless if there is also a reticle),  the method was changed to NE
#For sightings with distance, reticle and bearing, with binocular method, remove the Distance. For those NE method, use Distance as the measure:
if(length(which(!is.na(data$Distance) & !is.na(data$Reticle) & (data$Method %in% c("Bi", "BE"))))!=0){
  data[which(!is.na(data$Distance) & !is.na(data$Reticle) & (data$Method %in% c("Bi", "BE"))),]$Distance <- NA
}
if(length(which(!is.na(data$Distance) & !is.na(data$Reticle) & (data$Method %in% c("NE"))))!=0){
  data[which(!is.na(data$Distance) & !is.na(data$Reticle) & (data$Method %in% c("NE"))),]$Reticle <- NA
}

#Assign data quality to sighting data
#====================================
cat("\n\nAssign data quality to sighting data...")
#Assign adequate sightings (OK)
if(length(which(!is.na(data$Reticle) & !is.na(data$BEARING) & (data$Method %in% c("Bi","BE"))))!=0){
  data[which(!is.na(data$Reticle) & !is.na(data$BEARING) & (data$Method %in% c("Bi","BE"))),]$OK <- 1
}
if(length(which(!is.na(data$BEARING) & !is.na(data$Distance) & data$Method=="NE"))!=0){
  data[which(!is.na(data$BEARING) & !is.na(data$Distance) & data$Method=="NE"),]$OK <- 1
}
#Assign inadequate information to sightings (IA)
if(length(which(is.na(data$heading)))!=0){
  data[which(is.na(data$heading)),]$IA <- 1
}
#if(length(which(data$Nominal.lat.long=="YES"))!=0){
#  data[which(data$Nominal.lat.long=="YES"),]$IA <- 1
#}
if(length(which(is.na(data$Reticle) & is.na(data$Distance)))!=0){
  data[which(is.na(data$Reticle) & is.na(data$Distance)),]$IA <- 1
}
#need to switch this code to capture use of BEARING
#if(length(which(is.na(data$TRUE.BEARING)))!=0){
# data[which(is.na(data$TRUE.BEARING)),]$IA <- 1
#}
if(length(which(is.na(data$BEARING)))!=0){
  data[which(is.na(data$BEARING)),]$IA <- 1
}

#Assign at-boat sightings (AB)
if(length(which(data$Distance==0))!=0){
  data[which(data$Distance==0),]$AB <- 1
}

#Anything that is an at-boat sighting is not IA (even though they are both treated the same way, ultimately)
if(length(which(data$AB==1))!=0){
  data[which(data$AB==1),]$IA <- NA
}
#Assign Estimated distance (ED)
if(length(which(!is.na(data$Distance)))!=0){
  data[which(!is.na(data$Distance)),]$ED <- 1
}
#Uncertain binoculars (UB)
if(length(which((data$Method %ni% c("Bi","BE")) & !is.na(data$Reticle) & !is.na(data$BEARING)))!=0){
  data[which((data$Method %ni% c("Bi","BE")) & !is.na(data$Reticle) & !is.na(data$BEARING)),]$UB <- 1
}
#Uncertain platform (UP)
#>>>>This would be assigned to all sightings because Platform is generated from the Effort table...
#if(length(which(is.na(data$Platform)))!=0){
#  data[which(is.na(data$Platform)),]$UP <- 1
#}
#Observer mismatch (OM)
#look at all sightings where the observer is not one of the assigned on-effort observers, still -- we assume these are meal-time substitutions & substitutions when the small boat is deployed
if(length(which((data$SightedBy!=data$port & data$SightedBy!=data$starboard) | is.na(data$SightedBy)))!=0){
  data[which((data$SightedBy!=data$port & data$SightedBy!=data$starboard) | is.na(data$SightedBy)),]$OM <- 1
}
#Estimated angle (EA)
#for any surveys with Tasking "Dedicated/Opportunistic"
# beep(10)
# x <- readline(prompt = cat(paste("\nDid this survey use an instrument to measure bearings of sightings?   [click here & type Yes or No & hit Enter]    \n\n", sep=" ")))
# if(x %in% c("NO","no","No","N","n")){
#   data$EA <- 1
# }


#Interpolate Methods & Platforms from effort table
#==================================================
#Interpolating Method
#------------------------
cat("\n\nInterpolating sighting Method...")
#nrow(data[which(data$UB==1),])
#nrow(data[which(data$Method=="NE" & !is.na(data$Reticle) & !is.na(data$Bearing)),]) #Most UBs are due to "NE" entries in Methods when Reticles and Bearing are given... the rest have nothing filled in Methods
if(length(which(data$UB==1 & is.na(data$Method)))!=0){
  data[which(data$UB==1 & is.na(data$Method)),]$Method <- data[which(data$UB==1 & is.na(data$Method)),]$instrument
}
#At this point all sightings with reticle and bearing data should have a Method assigned to them (whether "Bi", "BE", or "NE")
if(length(which(!is.na(data$Reticle) & !is.na(data$BEARING) & (data$Method %ni% c("Bi", "BE", "NE"))))!=0){
  prob <- as.character(data[which(!is.na(data$Reticle) & !is.na(data$BEARING) & (data$Method %ni% c("Bi", "BE", "NE"))),]$Time.PDT)
  stop(paste("There are bearing-reticle sightings with no Method assigned: Time.local (PDT) ", toString(as.character(prob)),"  At this point this really should be impossible. See Eva."), call. = FALSE)
}
#Assign Estimated reticles (ER)- an artifact of old code - This is a swarofski thing!!  Not needed for PRISMM

# if(length(which(!is.na(data$Reticle) & !is.na(data$TRUE.BEARING) & data$Method=="NE" & data$instrument=="NE"))!=0){
#   data[which(!is.na(data$Reticle) & !is.na(data$TRUE.BEARING) & data$Method=="NE" & data$instrument=="NE"),]$ER <- 1
# }

#Find all remaining records with no Method assigned
#All that should be left are ED and IA records
if(length(which(is.na(data$Method) & data$ED!=1 & data$IA!=1))!=0){
  stop("At this point all records with no Method assigned should be ED and/or IA records. See Eva.")
}
cat("DONE")

#Interpolating Platform
#------------------------
#Override uncertain platform records
cat("\n\nInterpolating sighting Platform...")
#data[which(data$UP==1),]$Platform <- data[which(data$UP==1),]$platform
#Find all remaining records with no Platform assigned
# data$platform <- "Fujinon_MBBow" # EK temp fix for erroneous platform in Oct 2020 effort data
if(length(which(is.na(data$Platform)))!=0){
  data[which(is.na(data$Platform)),]$Platform <- data[which(is.na(data$Platform)),]$platform
}
#Make sure that method and platform from sightings table and effort table match
sum(data$Method != data$effort_instrument,na.rm=TRUE)  #it's likely that Method and Instrument will not match up totally -- that's OK.
nrow(data[which(data$Method!="NE" & (data$Method != data$effort_instrument)),])  #it's likely that Method and Instrument will not match up totally -- that's OK. Method will be what is used for the position correction
sum(data$Platform != data$platform,na.rm=TRUE) #all platform data matches
data$effort_instrument <- NULL
data$platform <- NULL
cat("DONE")

#Create Ret_line field
#----------------------
#Only reticle-bearing sightings should have horizon noted
data$Ret_line <- NA

#Merge sighting data with ship specifications
#---------------------------------------------
#This cannot occur sooner because all records must have a Platform assigned to them
data <- merge(data, ship[,c("Ship_code","Platform","Height_of_eye")], by.x=c("vessel","Platform"), by.y=c("Ship_code","Platform"),all.x=TRUE,all.y=FALSE)


#######################################################################
# Position correction
#######################################################################

#Calculate maximum distance to horizon
#==========================================
cat("\n\nCalculating maximum distance to horizon...")
#For reticle & bearing sightings only
data$horizon.dist.nmi <- NA
data[which(!is.na(data$BEARING) & !is.na(data$Reticle)),]$horizon.dist.nmi <- mapply(ret.dist, data[which(!is.na(data$BEARING) & !is.na(data$Reticle)),]$Height_of_eye, data[which(!is.na(data$BEARING) & !is.na(data$Reticle)),]$Method, reticles=0)


#Calculate lat-long at maximum horizon
#==========================================
#For reticle & bearing sightings only
data$TOY <- NA
data[which(!is.na(data$BEARING) & !is.na(data$Reticle)),]$TOY <- mapply(NewPosLat, data[which(!is.na(data$BEARING) & !is.na(data$Reticle)),]$SHIPLAT, data[which(!is.na(data$BEARING) & !is.na(data$Reticle)),]$SHIPLON, data[which(!is.na(data$BEARING) & !is.na(data$Reticle)),]$BEARING, data[which(!is.na(data$BEARING) & !is.na(data$Reticle)),]$horizon.dist.nmi)

data$TOX <- NA
data[which(!is.na(data$BEARING) & !is.na(data$Reticle)),]$TOX <- mapply(NewPosLon, data[which(!is.na(data$BEARING) & !is.na(data$Reticle)),]$SHIPLAT, data[which(!is.na(data$BEARING) & !is.na(data$Reticle)),]$SHIPLON, data[which(!is.na(data$BEARING) & !is.na(data$Reticle)),]$BEARING, data[which(!is.na(data$BEARING) & !is.na(data$Reticle)),]$horizon.dist.nmi)
cat("DONE")

#Plot sight lines on map (to find true horizon)
#=====================================================

#Plot sight lines
#--------------------
cat("\n\nCalculating true horizon (this may take several minutes)...")

#Import coast shapefile:
# bc_coast = readOGR(paste(getwd(),u,"Required shapefiles/CHS/CHS_WGS_84.shp", sep = ""), verbose = FALSE) #Load in CHS coastline shapefile (in WGS84)
# if(!exists("bc_coast")){

bc_coast <- read_sf("C:\\Users\\keppele\\Documents\\ArcGIS\\basemaps\\Coastland\\CoastLand.shp") %>%
  st_transform(crs = 3005) %>% dplyr::select(geometry)# }

# bc_coast <- sf::st_transform(bc_coast, crs = 4326)
cat("\n - Land shapefile loaded")

#Get start and end points for reticle sightings
bino.begin <- data.frame(id = data[which(!is.na(data$TOY)),]$key, x = data[which(!is.na(data$TOY)),]$longitude, y = data[which(!is.na(data$TOY)),]$latitude)
bino.end <- data.frame(id = data[which(!is.na(data$TOY)),]$key, x = data[which(!is.na(data$TOY)),]$TOX, y = data[which(!is.na(data$TOY)),]$TOY)
bino.begin$id <- as.character(bino.begin$id)
bino.end$id <- as.character(bino.end$id)

#Make initial points spatial objects
# xy <- bino.begin[2:3]
# initPos <- SpatialPointsDataFrame(coords=xy, data=bino.begin, proj4string=CRS("+proj=longlat"))
# #initPos <- st_as_sf()
# #initPos <- spTransform(initPos, CRSobj = proj4string(bc_coast))
# initPos <- spTransform(initPos, CRSobj = "+proj=utm +zone=9N +datum=WGS84 +towgs84=0,0,0")
# cat("\n - Ship's position converted to spatial points")

# EK edit - use sf instead of sp
xy <- bino.begin[2:3]
initPos <- st_as_sf(xy, coords = c("x","y"),crs=4326) %>%
  st_transform(BP, crs=3005)
cat("\n - Ship's position converted to spatial points")



#Convert lines into spatial line data frame
#Create a list of line coordinates
l <- vector("list", nrow(bino.begin))
for (i in seq_along(l)) {
  l[[i]] <- Lines(list(Line(rbind(bino.end[i,2:3], bino.begin[i,2:3]))), ID=bino.begin[i,1])
}
#Convert to spatial lines
spl <- SpatialLines(l, proj4string = CRS("+proj=longlat"))
#Alter bino.begin so row names match list names in spatial line object
row.names(bino.begin) <- bino.begin$id
#Create spatial line data frame
spl <- SpatialLinesDataFrame(spl, data = bino.begin)
#Transform to WGS84 UTM 9N
#spl <- spTransform(spl, CRSobj = proj4string(bc_coast))
spl <- spTransform(spl, CRSobj = "+proj=utm +zone=9N +datum=WGS84 +towgs84=0,0,0")

#Clip BC polygon to spatial extent of sight lines
# bc_clip <- gClip(bc_coast,spl) - not working EK
b_poly <- as(raster::extent(spl), "SpatialPolygons")
proj4string(b_poly) <- "+proj=utm +zone=9N +datum=WGS84 +towgs84=0,0,0"

cat("\n - Coastline clipped by extent of sighting lines")

#Clipping by bounding box
#bb <- matrix(c(650000,5618000,655000,5621000),nrow=2)
#rownames(bb) <- c("lon","lat")
#colnames(bb) <- c('min','max')
#bboxMat <- rbind(c(bb['lon','min'],bb['lat','min']), c(bb['lon','min'],bb['lat','max']), c(bb['lon','max'],bb['lat','max']), c(bb['lon','max'],bb['lat','min'])) # clockwise, 5 points to close it
#bboxSP <- SpatialPolygons( list(Polygons(list(Polygon(bboxMat)),"bbox")), proj4string=CRS("+proj=utm +zone=9N +datum=WGS84"))
#system.time(isl_clip <- gClip(bc_coast,bboxSP))

#Plot objects
#plot(spl, col="blue",ylim=c(5600000,5625000),xlim=c(650000,660000),axes=TRUE) #plot sight lines
#points(initPos, pch=20, col="red") #plot initial position
#plot(bc_clip, add=TRUE) #plot coastline -- this takes awhile

# Cut sight lines at BC polygon edge
#--------------------------------------------------
# if(gIntersects(spl,bc_clip)){ #only split if there is at least one intersection between the sight lines and the coast polygon
#   edge_lines <- raster::intersect(spl,bc_clip) #Takes 1.5 seconds for 40 lines; Takes 4 minutes for 1461 lines
#   cat("\n - Sighting lines split by BC coast shapefile")
#   # Get ID number of each line that intersects the polygon
#   #--------------------------------------------------------
#   nrow(edge_lines@data) #number of lines with at least one land intercept
#   length(edge_lines@lines) #number of lines with at least one land intercept
#   ints.index <- sapply(slot(edge_lines, "lines"), function(x) slot(x, "ID")) #get polygon-line indexes
#   length(unique(edge_lines@data$id)) #number of sighting lines that intercept land
#   ints <- sort(unique(edge_lines@data$id)) #sighting key for all lines with land intercepts
#     # Extract edge points from lines
#   #--------------------------------------------------------
#   edge_lines@data$Edge_x_utm <- NA
#   edge_lines@data$Edge_y_utm <- NA
#   #This will extract the utm coordinates of intercept points for each polygon-line:
#   for (i in 1:length(edge_lines@lines)){     # i-th list element, 2 and 4 are edge co-ords (1 and 3 are end point)
#     z <- length(edge_lines@lines[[i]]@Lines) #number of segments within the polygon-line -- it appears the last set of coordinates of the last segment is always the first line-land intercept
#     edge_lines@data[i,]$Edge_x_utm <- coordinates(edge_lines)[[i]][[z]][2]
#     edge_lines@data[i,]$Edge_y_utm <- coordinates(edge_lines)[[i]][[z]][4]
#   }
#   #Convert intercept points into lat-long
#   landpts <- SpatialPointsDataFrame(cbind(edge_lines@data$Edge_x_utm,edge_lines@data$Edge_y_utm), data=edge_lines@data, proj4string=CRS("+proj=utm +zone=9N +datum=WGS84"))
#   landpts <- spTransform(landpts, CRSobj = "+proj=longlat")
#   landpts@data <- cbind(landpts@data,coordinates(landpts)) #id = line/sighting key, xy = initial position latlon coordinates, Edge_utm = intercept utm coordinates, coords = intercept latlon coordinates
#   #Calculate distance (nmi) between initial point and intercept point
#   landpts@data$dist <- mapply(PosDist,landpts@data$y,landpts@data$x,landpts@data$coords.x2,landpts@data$coords.x1)
#   #Keep the record for each ints for the shore intercept closest to the initial position
#   lpts <- as.data.frame(landpts@data %>% group_by(id) %>% slice(which.min(dist))) #fyi - this will automatically order dataframe by 'id'
#   if(nrow(lpts)!=length(ints)){ #if this isn't true, something has gone awry
#     stop("This message shouldn't come up... See Eva. [123]", call. = FALSE)
#   }
#   cat("\n - True horizon found for all on-effort survey sightings")
#     #Add intercept points to plot
#   #points(lpts$Edge_x_utm, lpts$Edge_y_utm, col="green", pch=16, cex=0.9)
# }

#Correct horizon data
#--------------------------
cat("\n\nCorrecting horizon data...")
#All lines that intersect coastline should have Ret_line=="LA", those that don't intersect anything should have Ret_line=="HO", Sightings without reticles (i.e. with Distance) should have Ret_line==NA
if(exists("ints")){
  if(length(which(data$key %in% ints))!=0){
    data[which(data$key %in% ints),]$Ret_line <- "LA"
  }
  if(length(which(data$key %ni% ints & !is.na(data$horizon.dist.nmi)))!=0){
    data[which(data$key %ni% ints & !is.na(data$horizon.dist.nmi)),]$Ret_line <- "HO"
  }
} else {
  data[which(!is.na(data$horizon.dist.nmi)),]$Ret_line <- "HO"
}

# EK edit - add if clause
if(nrow(data[which(is.na(data$Reticle) | is.na(data$BEARING)),]!=0)){
  data[which(is.na(data$Reticle) | is.na(data$BEARING)),]$Ret_line <- NA
}
#Override horizon distance for records with Ret_line=="LA" with distance to shore (in nmi)
if(exists("ints")){
  for(i in 1:nrow(lpts)){
    data[which(data$key==lpts[i,]$id),]$horizon.dist.nmi <- lpts[i,]$dist
  }
}

#Assign zero land horizon (ZR)
if(length(which(data$Ret_line=="LA" & data$Reticle==0))!=0){
  data[which(data$Ret_line=="LA" & data$Reticle==0),]$ZR <- 1
  data[which(data$Ret_line=="LA" & data$Reticle==0),]$Reticle <- 0.1
}
cat("DONE")

#Assign Fair conditions (FC) data code using horizon certainty rather then visibility
cat("\n\nChecking for fair conditions...")
#First look for sightings where horizon_certainty wasn't populated.  In those cases use the visibility on the side the sighting was reported on to populate horizon certainty.  If port/stbd vis = F or R, horizon certainty is estimated, otherwise if visibility = G&E have to assume the horizon was unobstructed.

#Standardize horizon certainty entries (capitalization introduced some variability)

data$Horizon_Certainty<-tolower(data$Horizon_Certainty)

#Populate missing horizon certainty values
if(sum(is.na(data$Horizon_Certainty))!=0){ #there should be no blank Horizon certainty entries (ultimately only important for reticle and bearing sightings)
  if(sum(which(is.na(data$Horizon_Certainty) & data$Side=="Port" & data$port.visibility=="F"))!=0){
    data[which(is.na(data$Horizon_Certainty) & data$Side=="Port" & data$port.visibility=="F"),]$Horizon_Certainty<-"estimated"
  }#Making assumption that horizon is obscured due to reported visibility
  else
    if(sum(which(is.na(data$Horizon_Certainty) & data$Side=="Starboard" & data$stbd.visibility=="F"))!=0){
      data[which(is.na(data$Horizon_Certainty) & data$Side=="Starboard" & data$stbd.visibility=="F"),]$Horizon_Certainty<-"estimated"
    }#Making assumption that horizon is obscured due to reported visibility
  else
    if(sum(which(is.na(data$Horizon_Certainty) & data$Side=="Port" & data$port.visibility=="R"))!=0){
      data[which(is.na(data$Horizon_Certainty) & data$Side=="Port" & data$port.visibility=="R"),]$Horizon_Certainty<-"estimated"
    } #Making assumption that horizon is obscured due to reported visibility
  else
    if(sum(which(is.na(data$Horizon_Certainty) & data$Side=="Starboard" & data$stbd.visibility=="R"))!=0){
      data[which(is.na(data$Horizon_Certainty) & data$Side=="Starboard" & data$stbd.visibility=="R"),]$Horizon_Certainty<-"estimated"
    } else #Making assumption that horizon is obscured due to reported visibility
      if(sum(which(is.na(data$Horizon_Certainty) & data$Side=="Port" & data$port.visibility=="G&E"))!=0){
        data[which(is.na(data$Horizon_Certainty) & data$Side=="Port" & data$port.visibility=="G&E"),]$Horizon_Certainty<-"unobstructed"
      } #Making assumption that horizon is unobstructed due to reported visibility
  else
    if (sum(which(is.na(data$Horizon_Certainty) & data$Side=="Starboard" & data$stbd.visibility=="G&E"))!=0){
      data[which(is.na(data$Horizon_Certainty) & data$Side=="Starboard" & data$stbd.visibility=="G&E"),]$Horizon_Certainty<-"unobstructed"
    }  #Making assumption that horizon is unobstructed due to reported visibility
}

#If Horizon is obscured, but there is a reticle reported AND the horizon distance is greater then 3 nmi, Data error code FC is applied.
if(length(which(data$Horizon_Certainty=="estimated" & !is.na(data$Reticle) & data$horizon.dist.nmi>3))!=0){
  data[which(data$Horizon_Certainty=="estimated" & !is.na(data$Reticle) & data$horizon.dist.nmi>3),]$FC <- 1 #only for reticle readings and not applicable if distance to shore is less than or equal to 3 nmi
}
cat("DONE")


#Calculate corrected sighting positions
#=========================================

#Calculate distance to animal (in nmi)
#---------------------------------------
cat("\n\nCalculating distance to animal...")
#FOR ESTIMATED DISTANCES
data$SD_nm <- data$Distance

#FOR RETICLE READINGS FROM LAND
if(length(which(data$Ret_line=="LA"))!=0){
  data[which(data$Ret_line=="LA"),]$SD_nm <- mapply(ret.dist.terra, data[which(data$Ret_line=="LA"),]$Height_of_eye, data[which(data$Ret_line=="LA"),]$Method, data[which(data$Ret_line=="LA"),]$Reticle, data[which(data$Ret_line=="LA"),]$horizon.dist.nmi)
}
#FOR RETICLE READINGS FROM HORIZON
if(length(which(data$Ret_line=="HO"))!=0){
  data[which(data$Ret_line=="HO"),]$SD_nm <- mapply(ret.dist, data[which(data$Ret_line=="HO"),]$Height_of_eye, data[which(data$Ret_line=="HO"),]$Method, data[which(data$Ret_line=="HO"),]$Reticle)
}
cat("DONE")

#Calculate perpendicular sighting distance to animal (in nmi)
#-------------------------------------------------------------
cat("\n\nCalculating perpendicular sighting distance...")
#sin function is looking for radians, so bearings in degrees have to be converted by *pi/180
data$PSD_nm <- sin(abs(data$Bearing)*pi/180)*data$SD_nm
cat("DONE")

#Calculate lat-lon of animal's position
#---------------------------------------
cat("\n\nCalculating corrected sighting positions...")
data$final.lat <- mapply(NewPosLat,data$SHIPLAT,data$SHIPLON,data$BEARING,data$SD_nm)
data$final.lon <- mapply(NewPosLon,data$SHIPLAT,data$SHIPLON,data$BEARING,data$SD_nm)
cat("DONE")


#Data quality post-correction estimation
#=========================================
cat("\n\nChecking that corrected sightings don't fall on land...")
#Unrealistic estimated distance (UED)
#-------------------------------------
data1 <- data[which(!is.na(data$final.lon)),] #Filter data to corrected positions only
AP <- SpatialPointsDataFrame(cbind(data1$final.lon,data1$final.lat), data=data1, proj4string=CRS("+proj=longlat"))
# AP <- spTransform(AP, CRSobj = proj4string(bc))
# AP <- spTransform(AP, CRSobj = "+proj=utm +zone=9N +datum=WGS84 +towgs84=0,0,0") # EK edit
AP <- st_as_sf(AP) %>% st_transform(crs = st_crs(bc_coast)) # EK edit to use sf

#Find any points that fall on land
#plot(bc, axes=TRUE)
#plot(AP, add=TRUE, col="red", cex=1.5, pch=16)
LAND <- AP[bc_coast,]
nrow(LAND) #number of points on land
#Estimated distances that fall on land get data quality 'UED'
if(length(which(data$ED==1 & is.na(data$IA) & data$key %in% LAND$key))!=0){
  data[which(data$ED==1 & is.na(data$IA) & data$key %in% LAND$key),]$UED <- 1
}
#Other sightings that fall on land get data quality 'IA'? This in theory would never happen - there are no other reasons I can think of that the corrected sighting would fall on land.
if(length(which(is.na(data$IA) & is.na(data$ED) & data$key %in% LAND$key))!=0){
  data[which(is.na(data$IA) & is.na(data$ED) & data$key %in% LAND$key),]$IA <- 1
}
cat("DONE")

#Any data that has any data quality notation other than OK, cannot have notation OK:
cat("\n\nFinalizing data quality columns...")
q <- qual[-1]
data$q <- rowSums(data[,q],na.rm=TRUE)
if(length(which(data$q!=0 & data$OK==1))!=0){
  data[which(data$q!=0 & data$OK==1),]$OK <- NA
}
#this means that the only OK data are reticle binocular readings with bearing
data$q <- NULL
cat("DONE")

# TEMPORARY EDIT TO PLOT SIGHITNGS AFTER ERROR IN PLOTTING FOR SEPT 2020 SURVEY ONLY --- # EK EDIT
# data <- data %>% dplyr::mutate(
#   final.lat = case_when(
#     is.na(final.lat) ~ MYST_Sgt.Lat,
#     !is.na(final.lat) ~ final.lat),
#   final.lon = case_when(
#     is.na(final.lon) ~ MYST_Sgt.Lon,
#     !is.na(final.lon) ~ final.lon
#   ))
# View(data %>% dplyr::select(time_local.x, OK, IA, AB, final.lat, final.lon, Sgt.ID, Bearing, Reticle, Distance, SightedBy, Species, BestNumber, Event, MYST_Sgt.Lat))

cat("\n\nAssigning final coordinates to sightings...")
#Make sure that all sightings (other than inadequate or at-boat sightings) have a final lat-lon [this should be a stop gap in the auto-code]
if(sum(is.na(data[which(is.na(data$IA) & is.na(data$AB)),]$final.lat))!=0){
  stop("There are some sightings that are missing final coordinates, but that don't have Data Quality IA or AB. This shouldn't be possible. See Eva.", call. = FALSE)
}

#Override final lat-lon with ship's position according to data quality
#======================================================================
if(length(which(data$IA==1 | data$FC==1 | data$UED==1 | data$AB==1))!=0){
  data[which(data$IA==1 | data$FC==1 | data$UED==1 | data$AB==1),]$final.lon <- data[which(data$IA==1 | data$FC==1 | data$UED==1 | data$AB==1),]$SHIPLON
  data[which(data$IA==1 | data$FC==1 | data$UED==1 | data$AB==1),]$final.lat <- data[which(data$IA==1 | data$FC==1 | data$UED==1 | data$AB==1),]$SHIPLAT
}
#Make sure that all sightings have a final lat-lon [this should be a stop gap in the auto-code]
if(sum(is.na(data$final.lat))!=0){
  stop("There are some sightings that are missing final coordinates. This shouldn't be possible. See Eva.", call. = FALSE)
}
cat("DONE")

#Assign position type to each sighting
cat("\n\nAssigning Position Type to sightings...")
#notes whether the final position is corrected or defaulted to ship's position
#check in geodbase to see what the variable should be called
data$Position.type <- NA
if(length(which(data$IA==1 | data$FC==1 | data$UED==1 | data$AB ==1))!=0){
  data[which(data$IA==1 | data$FC==1 | data$UED==1 | data$AB ==1),]$Position.type <- "Ship"
}
if(length(which(is.na(data$Position.type)))){
  data[which(is.na(data$Position.type)),]$Position.type <- "Corrected"
}
cat("DONE")

#Make sure that all position.type='ship' final lat-longs=initial lat-long and that position.type='corrected' final lat-longs!=initial lat-long
data$diff <- mapply(PosDist,data$SHIPLAT,data$SHIPLON,data$final.lat,data$final.lon)
if(sum(unique(data[which(data$Position.type=='Corrected'),]$diff)==0)!=0){
  stop("This message shouldn't come up. If it does, contact Eva. [777]", call. = FALSE)
}
if(sum(unique(data[which(data$Position.type=='Ship'),]$diff)!=0)!=0){
  stop("This message shouldn't come up. If it does, contact Eva. [778]", call. = FALSE)
}
data$diff <- NULL

#Plot all final corrected positions
#====================================
#Check whether any final positions fall on land
cat("\n\nChecking that final sightings positions don't fall on land...")
AP <- SpatialPointsDataFrame(cbind(data$final.lon,data$final.lat), data=data, proj4string=CRS("+proj=longlat")) %>%
  st_as_sf() # EK edit : change to sf
# AP <- spTransform(AP, CRSobj = "+proj=utm +zone=9N +datum=WGS84 +towgs84=0,0,0")
AP <- st_transform(AP, crs = st_crs(bc_coast)) # EK edit : change to sf

LAND <- AP[bc_coast,]
nrow(LAND) #number of corrected positions on land. There should be nothing on land at this point
if(nrow(LAND)!=0){
  stop("There are some final sightings positions that fall on land. This should be impossible at this point... See Eva.", call. = FALSE)
}
cat("DONE")

#Check whether sightings were inside or outside of the EEZ to fill in Waters column
cat("\n\nAssigning Waters to sightings (this may take a couple of minutes)...")
# eez = readOGR(paste(getwd(),u,"Required shapefiles/EEZ/SDE_Boundaries_patched_WGS84_UTM9N.shp", sep = ""), verbose = FALSE) #Load in EEZ shapefile (in WGS84)
eez = st_read(paste(getwd(),u,"shapefiles/CanadianEEZ.shp", sep = "")) #Load in EEZ shapefile (in WGS84)
eez <- st_transform(eez, crs = st_crs(bc_coast))
cat("\n - EEZ shapefile loaded")
#plot(eez, axes=TRUE)
CANADA <- AP[eez,]
nrow(CANADA) #number of corrected positions in Canada
#plot(AP[which(AP@data$key %ni% CANADA@data$key),], add=TRUE, col="red", pch=16) #plot sightings outside of Canada
data$Waters <- NA
if(length(which(data$key %in% CANADA$key))!=0){
  data[which(data$key %in% CANADA$key),]$Waters <- "CANADA"
}
if(length(which(data$key %ni% CANADA$key))!=0){
  data[which(data$key %ni% CANADA$key),]$Waters <- "US"
}
cat("\n - Sightings assigned to Waters")

#Fix up columns
#====================================
#Create data quality string
#----------------------------
cat("\n\nFinalizing sightings' Data Quality attribute...")
data$Data.Quality <- NA
for(i in 1:nrow(data)){
  vec <- data[i,qual]
  data[i,]$Data.Quality <- gsub(",","",toString(qual[which(vec==1)]))
}
#Check for any blanks in data quality (EVERY record should have at least one notation for data quality...)
if(length(which(is.na(data$Data.Quality)))!=0){
  stop("Some sightings do not have any Data Quality code assigned to them. This should be impossible at this point. See Eva.", call. = FALSE)
}

#Remove binary data quality columns
data[,qual]<-NULL
cat("DONE")

#Column names & order
#----------------------------
cat("\n\nFormatting sightings shapefile attribute table...")
#Create table for shapefile:
# positions <- data[,c("SurveyID","GPSIndex","Sgt.ID","key","gpstime.utc","time_index.x","M.int","Event","SHIPLAT","SHIPLON","speed","port.visibility","beaufort","stbd.visibility","precipitation","status","final.transect.id","onseq_id","vessel","Platform","Side","Method","SightedBy","Horizon_Certainty","Bearing","BEARING","Reticle","Distance","Ret_line","SD_nm","PSD_nm","final.lat","final.lon","Position.type","Waters","Data.Quality","Species","BestNumber","MinNumber","MaxNumber","Multispecies","Detect.Min","Detect.Max","Detect.Best")]
# names(positions) <- c("SurveyID","GPSIndex","Sgt.ID","key","gpstimeutc","time_index","M.int","Event","SHIPLAT","SHIPLON","speed","port.visib","beauf","stbd.visib","precip","status","Final.T.ID","onseq_id","vessel","Platform","Side","Method","SightedBy","Horizon_C","Bearing.R","Bearing.T","Reticle","Distance","Ret_line","SD_nm","PSD_nm","final.lat","final.lon","Pos.type","Waters","Data.Qual","Species","BestNumber","MinNumber","MaxNumber","M.species","Det.Min","Det.Max","Det.Best")
# EK edit - remove fields associated with multispecies sightings
positions <- data[,c("SurveyID","GPSIndex","Sgt.ID","key","gpstime.utc","time_index.x","M.int","Event","SHIPLAT","SHIPLON","speed","port.visibility","beaufort","stbd.visibility","swell","glare", "left.glare.limit", "right.glare.limit","precipitation","cloud.cover", "status","final.transect.id","onseq_id","vessel","Platform","Side","Method","SightedBy","Horizon_Certainty","Bearing","BEARING","Reticle","Distance","Ret_line","SD_nm","PSD_nm","final.lat","final.lon","Position.type","Waters","Data.Quality","Species","BestNumber","MinNumber","MaxNumber")]
names(positions) <- c("SurveyID","GPSIndex","Sgt.ID","key","gpstimeutc","time_index","M.int","Event","SHIPLAT","SHIPLON","speed","port.visib","beauf","stbd.visib","swell","glare", "l.glare", "r.glare","precip","cloudcover","status","Final.T.ID","onseq_id","vessel","Platform","Side","Method","SightedBy","Horizon_C","Bearing.R","Bearing.T","Reticle","Distance","Ret_line","SD_nm","PSD_nm","final.lat","final.lon","Pos.type","Waters","Data.Qual","Species","BestNumber","MinNumber","MaxNumber")

positions <- positions[order(positions$GPSIndex),]
rownames(positions) <- c(1:nrow(positions))
cat("DONE")


######################################################
#EXPORT DATA.....Moved to Ship Survey Source Code with other exports
######################################################


