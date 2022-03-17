# effort

# first run plot_tracks code to get 
effort <- data$effort %>% mutate(date = date(time_index..pdt.), start_time = ymd_hms(time_index..pdt.)) %>% 
  filter(date >= "2020-08-28")
# start_time = format(ymd_hms(time_index..pdt.), "%H:%M")
for(i in seq_along(effort$time_index..pdt.)){
  effort$end_time[i] <- format(ymd_hms(effort$time_index..pdt.[i + 1]))
  # effort$start_time[i] <- ymd_hms(effort$time_index..pdt.[i])
  # effort$time_diff <-   difftime(effort$time_index..pdt[i], effort$time_index..pdt.[i+1])
}

# TO DO create time diff column
# effort <- effort %>% group_split(date) %>% map_df(mutate, time_diff = difftime(effort$time_index..pdt[i], effort$end_time))

effort %<>% filter(status == "ON EFFORT", date == "2020-10-14")

# full gpx trackline from full
files <- "20201014-LineTransect_MB.gpx"
transects <- purrr::map_df(files, get_track, dir = transect_dir) 
