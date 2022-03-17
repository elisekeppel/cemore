# merge 2 mysticetus data sets

#' Combine Mysticetus observations
#' Combine Mysticetus observations for multiple days over a survey
#' @param year
#' @param month
#' @param day
#'
#' @return
#' @export
#'
#' @examples
#' ##Not run: combine_mysti_obs(year = 2021, month = "09",day = 20)
#' ##End(Not run)
combine_mysti_obs <- function(year, month, day){
  dir = paste0("survey_data/raw_data/",year,"-",month,"/observations/")
  folder <- paste0("obs", year, "-", month, "-", day)
  newdir <- paste0(dir, folder)
  if(!exists(newdir)) dir.create(newdir, showWarnings = F)
  file = c("EffortEnv", "RF Sighting", "Sighting", "xMultiSpecies", "zComments")

  combine <- function(file){
    file = file
    x <- read.csv(paste0(newdir, "a/", file, ".csv"))
    y <- read.csv(paste0(newdir, "b/", file, ".csv"))
    z <- rbind(x,y)
    write.csv(z, paste0(newdir,"/", file, ".csv"), row.names = F)
  }

  purrr::map(file, combine)
  unlink(paste0(newdir, "a"), recursive = T, force = T)
  unlink(paste0(newdir, "b"), recursive = T, force = T)
}



#' Combine mysticetus tracks
#' Combine mysticetus tracklines for multiple days over a survey
#' @param year
#' @param month
#' @param day
#'
#' @return
#' @export
#'
#' @examples
#' ## Not run:
#' combine_mysti_tracks(2021, "09", 20)
#' ## End(Not run)

combine_mysti_tracks <- function(year, month, day){
  date <- paste0(year, month, day)
  folder <- paste0(year, "-", month)
  dir = file.path("survey_data/tracklines/transects/csv",folder)
  files <- list.files(dir)[grep(date,list.files(dir))]

  x <- purrr::map_df(files, ~ read.csv(file.path(dir, .x))) %>%
   dplyr::arrange(lubridate::date(Time.Created..UTC.))

  unlink(file.path(dir, files[1]), recursive = T, force = T)
  unlink(file.path(dir, files[2]), recursive = T, force = T)

  write.csv(x, file.path(dir, files[1]),row.names = F)
}

