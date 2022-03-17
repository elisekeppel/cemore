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
