# ----------------------------------------------------------------------
# -------------------- SUMMARIZE SURVEY SIGHTINGS ----------------------
# ----------------------------------------------------------------------

# To summarise non-incidental sightings data for survey report

# ----------------------------------------------------------------------
# ----------------- SUMMARIZE --------------------------------
# ----------------------------------------------------------------------
s <- ap_sf
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

cat(paste0("Number of transects completed in ", month_abb, " ", year, ": ", length(unique(effort$Final.T.ID))))

# total number of survey days and total count cetacean sightings; manually check any NA values
summary <- s %>% dplyr::group_by(year = year(time_index), month = month(time_index)) %>%
  dplyr::summarise(number_sightings = n(), number_indivduals = sum(BestNumber))
print(summary)
write.csv(summary, paste0("output/sightings_summary/cemore_sightings_summary_", year, tolower(month_abb), ".csv"), row.names = FALSE)

# counts by month and species
summary <- s %>%  dplyr::group_by(year = year(time_index), month = month(time_index), Species) %>%
  dplyr::summarise(number_sightings = n(), number_indivduals = sum(BestNumber))
print(summary)
write.csv(summary, paste0("output/sightings_summary/cemore_species_summary_", year, tolower(month), ".csv"), row.names = FALSE)

# count cetacean sightings by species by day
# s %>%  dplyr::group_by(month(time_index), day(time_index), Species) %>% dplyr::summarise(number_sightings = n(), number_indivduals = sum(BestNumber))


# # ------------- to check days in field work that were on effort -----------------------
# e <- read.delim(file.path("OUTPUT FILES","dataEffort table", paste0("dataEffortcemore_", year, tolower(month_abb), ".txt")))
e <- effort
e %<>% filter(Status == "ON") %>%
  dplyr::group_by(Year = year(GpsT), Month = month(GpsT)) %>%
  dplyr::summarise(Lon_effort_days = n_distinct(day(GpsT)),
                  transects = n_distinct(Final.T.ID))
print(e)

