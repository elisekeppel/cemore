# These are Eva's functions from the beginning of her Ship Survey Data
# Processing Source Code

'%ni%' = Negate('%in%')

#Convert decimal degrees to radians
radians <- function(x) {pi*x/180}

#Calculate the bearing of a sighting [relative to true North], taking into account ship's true heading and relative bearing of sighting
tru.bearing <- function(heading,bearing){
  return((heading+bearing)-(360*floor((heading+bearing)/360)))
}

#Calculate distance to object (in nmi) according to reticles from true horizon. Requires HOE (in m), binocular type ("Bi"=7x50 or "BE"), and # reticles from horizon.
ret.dist <- function(height.of.eye,bino.type,reticles){
  if(bino.type!="BE"){
    rpr <- 0.00497
  } else {
    rpr <- 0.00136
  }
  x <- sqrt(2 * 6366 * height.of.eye / 1000 + (height.of.eye / 1000)^2)
  if(reticles==0){
    retdist = x/1.852
  } else {
    angle <- atan(x/6366)
    retdist = (1 / 1.852) * ((6366 + height.of.eye / 1000) * sin(angle + reticles * rpr) - sqrt(6366^2 - ((6366 + height.of.eye / 1000) * cos(angle + reticles * rpr))^2))
  }
  return(retdist)
}

#Calculate distance to object (in nmi) according to reticles from land. Requires HOE (in m), binocular type ("Bi"=7x50 or "BE"), # reticles from land, and distance to shore (in nmi)
ret.dist.terra <- function(height.of.eye,bino.type,reticles,DTS){
  if(bino.type!="BE"){
    rpr <- 0.00497
  } else {
    rpr <- 0.00136
  }
  RetDist <- rep(NA, length(reticles))#don't get why vector is needed
  if(reticles==0){
    RetDist = DTS
  } else {
    h <- height.of.eye/1000 #put HOE into units of km
    gama <- DTS/3437.36 #Earth's radius = 6366 km = 3437.36 nm
    theta <- reticles * rpr
    L0 <- sqrt(6366^2 + (6366 + h)^2 - 2 * 6366 * (6366 + h) * cos(gama))	#km
    beta <- acos((2 * h * 6366 + h^2 + L0^2)/(2 * (6366 + h) * L0)) - theta
    RetDist <- (1/1.852) * ((6366 + h) * cos(beta) - sqrt((6366 + h)^2 * cos(beta)^2 - (2 * h * 6366 + h^2))) # D0 (in nmi) - line-of-sight distance to animal
    RetDist <- sqrt(RetDist^2 - (h/1.852)^2) # D (in nmi) - distance to animal along surface of Earth
  }
  return(RetDist)
}

#Calculate latitude of a new position at a particular initial (true) bearing and distance (in nmi) from an initial position. Returns latitude in decimal degrees.
NewPosLat <- function(lat1, lon1, bearing, distance){
  if(is.na(lat1) | is.na(lon1) | is.na(bearing) | is.na(distance)) {
    NewPosLat = NA
  } else {
    if(bearing %in% c(0,180,360)){
      if(bearing==180){
        NewPosLat = lat1 - distance / 60
      } else {
        NewPosLat = lat1 + distance / 60
      }
    } else {
      a = radians(90 - lat1)
      RLon1 = radians(lon1)
      b = pi * distance / (60 * 180)
      CAngle = radians(bearing)
      APlusB = 2 * atan(cos((a - b) / 2) / (cos((a + b) / 2) * tan(CAngle / 2)))
      AMinusB = 2 * atan(sin((a - b) / 2) / (sin((a + b) / 2) * tan(CAngle / 2)))
      BAngle = (APlusB - AMinusB) / 2
      Aangle = (AMinusB + APlusB) / 2
      if(lat1 < 0){
        NewPosLat = (180 * asin(sin(b) * sin(CAngle) / sin(BAngle)) / pi) - 90
      } else {
        NewPosLat = abs((180 * asin(sin(b) * sin(CAngle) / sin(BAngle)) / pi) - 90)
      }
    }
    if(NewPosLat > 90){
      NewPosLat = NewPosLat - 90
    } else {
      if(NewPosLat < -90){
        NewPosLat = -(NewPosLat + 180)
      }
    }
  }
  return(NewPosLat)
}

#Calculate longitude of a new position at a particular initial (true) bearing and distance (in nmi) from an initial position. Returns longitude in decimal degrees.
NewPosLon <- function(lat1, lon1, bearing, distance){
  if(is.na(lat1) | is.na(lon1) | is.na(bearing) | is.na(distance)) {
    NewPosLon = NA
  } else {
    if(bearing==0){bearing <- 360}
    a = radians(90 - lat1)
    Rlon1 = radians(lon1)
    b = pi * distance / (60 * 180)
    CAngle = radians(bearing)
    APlusB = 2 * atan(cos((a - b) / 2) / (cos((a + b) / 2) * tan(CAngle / 2)))
    AMinusB = 2 * atan(sin((a - b) / 2) / (sin((a + b) / 2) * tan(CAngle / 2)))
    BAngle = (APlusB - AMinusB) / 2
    Aangle = (AMinusB + APlusB) / 2
    NewPosLon = lon1 + BAngle * 180 / pi
    if(NewPosLon > 180){
      NewPosLon = NewPosLon - 360
    } else {
      if(NewPosLon < -180){
        NewPosLon = NewPosLon + 360
      }
    }
  }
  return(NewPosLon)
}

#Calculate the distance between two positions in nautical miles
PosDist <- function(Lat1,Lon1,Lat2,Lon2){
  if(Lat1==Lat2 & Lon1==Lon2){
    Posdist = 0
  } else {
    Rlat1 = radians(Lat1)
    Rlat2 = radians(Lat2)
    Rlon = radians(Lon2 - Lon1)
    Posdist = 60 * (180 / pi) * acos(sin(Rlat1) * sin(Rlat2) + cos(Rlat1) * cos(Rlat2) * cos(Rlon))
  }
  return(Posdist)
}

#Clipping polygons
gClip <- function(shp, bb){
  #---------------
  #This bit no longer needed in sf with st_crop
  #---------------
  # if(class(bb) == "matrix"){
  #   b_poly <- as(raster::extent(as.vector(t(bb))), "SpatialPolygons")
  #   proj4string(b_poly) <- proj4string(shp)
  # } else {
    # b_poly <- as(raster::extent(bb), "SpatialPolygons")
  #   # proj4string(b_poly) <- proj4string(bb)
  #   proj4string(b_poly) <- "+proj=utm +zone=9N +datum=WGS84 +towgs84=0,0,0"
  # }
  #---------------
  # if(proj4string(shp)!=proj4string(b_poly)){
  # b_poly <- spTransform(b_poly, CRSobj = proj4string(bb))
  # b_poly <- spTransform(b_poly, CRSobj = "+proj=utm +zone=9N +datum=WGS84 +towgs84=0,0,0")
  # }
  #---------------
  # change to sf
  #---------------
  # shp <- rgeos::gBuffer(shp, byid=TRUE, width=0) # to fix Ring self-intersection
  # b_poly <- rgeos::gBuffer(b_poly, byid=TRUE, width=0)
  # rgeos::gIntersection(shp, b_poly, byid = TRUE)
  #---------------
  # shp <- sf::st_buffer(shp, dist=0) # to fix Ring self-intersection
  # b_poly <- sf::st_buffer(bb, dist=0)
  st_crop(shp, bb)
}

#Find non-numeric values
which.nonnum <- function(x) {
  badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
  which(badNum & !is.na(x))
}

#Fill blank values with previous value (this doesn't allow for leading blanks).  Not good to use if initial value(s) are NA.
fill <- function(x, blank = is.na) {
  # Find the values
  if(is.function(blank)) {
    isnotblank <- !blank(x)
  } else {
    isnotblank <- x != blank
  }
  # Fill down
  x[which(isnotblank)][cumsum(isnotblank)]
}

#Fill blank values with previous value (this allows for leading blanks).
fillNAgaps <- function(x, firstBack=FALSE) {
  ## NA's in a vector or factor are replaced with last non-NA values
  ## If firstBack is TRUE, it will fill in leading NA's with the first non-NA value. If FALSE, it will not change leading NA's.
  # If it's a factor, store the level labels and convert to integer
  lvls <- NULL
  if (is.factor(x)) {
    lvls <- levels(x)
    x    <- as.integer(x)
  }
  goodIdx <- !is.na(x)
  # These are the non-NA values from x only
  # Add a leading NA or take the first good value, depending on firstBack
  if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
  else             goodVals <- c(NA,            x[goodIdx])
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  x <- goodVals[fillIdx]
  # If it was originally a factor, convert it back
  if (!is.null(lvls)) {
    x <- factor(x, levels=seq_along(lvls), labels=lvls)
  }
  x
}

#Call Effort Segment creation --> Buffer creation --> Buffer overlap check
sourcePartial <- function(fn=paste(path,u,"Source code - DO NOT OPEN",u,"SHIP SURVEY SOURCE CODE.R",sep=""),startTag='#neverdeletethistag1',endTag='#neverdeletethistag2') {
  lines <- scan(fn, what=character(), sep="\n", quiet=TRUE)
  st<-grep(startTag,lines)
  en<-grep(endTag,lines)
  tc <- textConnection(lines[(st+1):(en-1)])
  source(tc)
  close(tc)
}

#Generate separate buffers on each side of a line
#Know the start and ends of your line to know which side is object [1] and which is object [2]
#In our case, because our perpendicular start lines begin -90 degrees to the ship's heading and end +90 degrees to the ship's heading, the object of interest for us, is object [2] -- as we want to clip the polygon prior to the start of the trackline
#If you want both polygons returned, remove [2] in last line of function
TwoBuf <- function(line,width,minEx){
  Buf0 <- rgeos::gBuffer(line,width=minEx,capStyle="SQUARE")
  Buf1 <- rgeos::gBuffer(line,width=width,capStyle="FLAT")
  Buf <- disaggregate(gDifference(Buf1,Buf0))
  return(disaggregate(gDifference(Buf1,Buf0))[2])
}

#To take directionality out of unique Transect IDs to facilitate identification of duplication.(TULLY&TANU)
rename.transect <- function(original.transect.id){
  if(!is.na(original.transect.id) & original.transect.id!="TRANSIT"){
    #Split Transect ID into its two components
    A <- substring(original.transect.id, unlist(gregexpr(pattern = "[[:alpha:]][[:digit:]]",original.transect.id))[1], unlist(gregexpr(pattern = "[[:alpha:]][[:digit:]]",original.transect.id))[2]-1)
    B <- substring(original.transect.id, unlist(gregexpr(pattern = "[[:alpha:]][[:digit:]]",original.transect.id))[2], nchar(original.transect.id))
    #List our two components in alphabetical order
    C <- gsub(", ", "", toString(c(A,B)[order(c(A,B))]))
    return(C)
  } else {
    return("TRANSIT")
  }
}

#To take directionality out of unique Transect IDs to facilitate identification of duplication.(TANU)
rename.transectV2 <- function(original.transect.id){
  if(!is.na(original.transect.id) & original.transect.id!="TRANSIT"){
    A <- substring(original.transect.id,1,nchar(original.transect.id)/2)
    B <- substring(original.transect.id,(nchar(original.transect.id)/2)+1,nchar(original.transect.id))
    if(sum(grepl("^[[:alpha:]]*$",A),grepl("^[[:digit:]]*$",A),grepl("^[[:alpha:]]*$",B),grepl("^[[:digit:]]*$",B))==0){ #Parse apart paired transect names vs solo transect names (e.g. 29SC28SC vs PSU15) -- this if statement makes sure both components of the transect name are alphanumerics
      #List our two components of Transect ID in alphabetical order
      C <- gsub(", ", "", toString(c(A,B)[order(c(A,B))]))
    } else {
      C <- original.transect.id
    }
    return(C)
  } else {
    return("TRANSIT")
  }
}


#Generates the proper transect name for all possible transects from a point of origin (x). This requires a schematic of the stratum design (tbl) - see Stratum1_Offshore.csv for an example.
get.relevant.transect.IDs <- function(x,tbl){
  r <- as.numeric(as.character(which(tbl==x,arr.ind = TRUE)[,1]))
  c <- as.numeric(as.character(which(tbl==x,arr.ind = TRUE)[,2]))
  adjacent.values <- unlist(c(tbl[r,c-1],tbl[r,c+1],tbl[r-1,c],tbl[r+1,c]))
  if(sum(!is.na(adjacent.values))!=0){ #only run if the cell of interest has neighbours
    #Split all values (for point of origin and its neighbours) into all their components
    x1 <- unlist(strsplit(x,"-"))
    adjacent.values <- unlist(strsplit(adjacent.values,"-"))
    #Filter for relevant neighbours
    relevant <- rep(list(NA),length(x1))
    for(f in 1:length(x1)){
      #Find letter(s) in cell of interest
      letter <- str_extract(x1[f], regex("[[:alpha:]]+", ignore_case = TRUE))
      #Find number(s) in cell of interest
      number <- str_extract(x1[f], regex("[[:digit:]]+", ignore_case = TRUE))
      #Store neighbours that share letters/numbers in common with cell of interest
      if(!is.na(letter)){
        if(!is.na(number)){
          #if both letter and number exists --> NUMBERS & LETTERS
          relevant[[f]] <- adjacent.values[grep(paste(c(letter,number),collapse="|"),adjacent.values)]
        } else {
          #if letter but no number exists --> LETTERS ONLY
          relevant[[f]] <- adjacent.values[grep(letter,adjacent.values)]
        }
      } else {
        #if no letter exists (a number has to exist) --> NUMBERS ONLY
        relevant[[f]] <- adjacent.values[grep(number,adjacent.values)]
      }
      relevant[[f]] <- paste(x1[f],relevant[[f]],sep="")
    }
    neighbours <- unlist(relevant)
    return(neighbours)
  } else {
    return(x) #if the cell of interest has no neighbours, return the cell value name as a valid transect ID
  }
}
