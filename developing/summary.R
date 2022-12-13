# ----------------------------------------------------------------------
# -------------------- SUMMARIZE SURVEY SIGHTINGS ----------------------
# ----------------------------------------------------------------------

# To summarise non-incidental sightings data for survey report

# ----------------------------------------------------------------------
# ----------------- SUMMARIZE --------------------------------
# ----------------------------------------------------------------------
# survey_summary <- function(single=T,
#                         year = year,
#                         month=month,
#                         month_abb = month_abb){

if(!single) {
  ap_sf = all_ap_sf
  effort_data = all_effort
}

s <- ap_sf %>% as.data.frame()
#---------------- for august 2020 -------------------------------------
# filter for only Aug survey days
if(year == 2020 & tolower(month_abb) == "aug"){
  s <- s %>% filter(day(time_index) %in% c(19,26,27,28))
}
#---------------- for september 2020 -----------------------------------------
# filter for only survey days, not tagging days
if(year == 2020 & tolower(month_abb) == "sept"){
  s <- s %>% filter(day(time_index) %in% c(2,3,10,13, 28)) # subtract the killer whale incidental sighting while not on effort... note 28th = Aug
}

cat(paste0("Number of transects completed in ", month_abb, " ", year, ": ", length(unique(effort$TransectID))))

summary <- list()
# total number of survey days and total count cetacean sightings; manually check any NA values
summary[[1]] <- s %>% dplyr::group_by(year = year(time_index), month = month(time_index)) %>%
  dplyr::summarise(number_sightings = n(), number_indivduals = sum(Group_Size))
print(summary[[1]] )
write.csv(summary[[1]] , paste0("output/sightings_summary/cemore_sightings_summary_", year, tolower(month_abb), ".csv"), row.names = FALSE)

# counts by month and species
summary[[2]] <- s %>%
  dplyr::group_by(year = year(time_index), month = month(time_index), Species) %>%
  dplyr::summarise(number_sightings = n(), number_indivduals = sum(Group_Size))
print(summary[[2]])
write.csv(summary[[2]], paste0("output/sightings_summary/cemore_species_summary_", year, tolower(month), ".csv"), row.names = FALSE)
rm(s)
# count cetacean sightings by species by day
# s %>%  dplyr::group_by(month(time_index), day(time_index), Species) %>% dplyr::summarise(number_sightings = n(), number_indivduals = sum(Group_Size))


# # ------------- to check days in field work that were on effort -----------------------
# e <- read.delim(file.path("OUTPUT FILES","dataEffort table", paste0("dataEffortcemore_", year, tolower(month_abb), ".txt")))

summary[[3]] <- effort %<>% filter(Status == "ON") %>%
  dplyr::group_by(Year = year(GpsT), Month = month(GpsT)) %>%
  dplyr::summarise(on_effort_days = n_distinct(day(GpsT)),
                  transects = n_distinct(TransectID))
print(summary[[3]])

cat(paste("Total km surveyed in",month_abb, year,"survey :", round(sum(st_length(effort_lines))/1000,0) %>% units::set_units(NULL)))
# }
