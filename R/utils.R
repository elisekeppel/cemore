names_lower <- function(x){
  names(x) <- tolower(names(x))
  x
}


# ensure bearing column has "r" or "l" (and in lowercase)
autofill_side <- function(s){
  for (i in 1:nrow(s)){
    s$Bearing[[i]] <- dplyr::if_else(
      tolower(substr(s$Bearing[[i]], nchar(s$Bearing[[i]]), nchar(s$Bearing[[i]]))) %in% c("r", "l"),
      tolower(s$Bearing[[i]]),
      dplyr::case_when(
        s$Side[[i]] == "Port" ~ paste0(s$Bearing[[i]], "l"),
        s$Side[[i]] == "Starboard" ~ paste0(s$Bearing[[i]], "r")))
  }

  for (i in 1:nrow(s)){
    s$Side[[i]] <- dplyr::if_else(
      tolower(s$Side[[i]]) %in% c("port", "starboard"),
      s$Side[[i]],
      dplyr::case_when(
        substr(s$Bearing[[i]], nchar(s$Bearing[[i]]), nchar(s$Bearing[[i]])) == "r" ~ paste0("Starboard"),
        substr(s$Bearing[[i]], nchar(s$Bearing[[i]]), nchar(s$Bearing[[i]])) == "l" ~ paste0("Port")))
  }
  s
}

standardize <- function(x) {
  if(length(unique(x))>2){
    return((x-mean(x, na.rm=T))/(2*sd(x, na.rm=T)))
  } else {
    return(x)
  }
}

binned <- function(x, breaks = NULL, min = NULL, max = NULL, by = NULL, dec=2){
  if(is.null(breaks)){
    if(is.null(min)) min <- min(x)
    if(is.null(max)) max <- max(x)
    if(is.null(by)) by <- (abs(min-max))/sqrt(mean(x))/2
    # br = seq(min, max, by)
    breaks = pretty(seq(min, max, by))
  }
  # if(!max(br) == max) br = c(seq(min, max, by), max)
  ranges = paste(head(breaks,-1), breaks[-1], sep=" - ")
  freq   = hist(x, breaks=breaks, include.lowest=TRUE, plot=FALSE)
  data.frame(range = ranges, frequency = freq$counts)
}

col_name_diff <- function(dat1, dat2){
  names(dat1)[which(!names(dat1) %in% names(dat2))]
}
