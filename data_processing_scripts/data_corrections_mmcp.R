# ----------------------------------------------------------------------
# -------------------- DATA CORRECTIONS - ALL MONTHS -------------------
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# -------------------- DATA CORRECTIONS - BY MONTH ---------------------
# ----------------------------------------------------------------------

cat("Applying data corrections")
#
# if(year == 2023 & tolower(month_abb) == "feb"){
#   data$effort[which(data$effort$PORT.Visibility == "Fair (CCG)"),]$PORT.Visibility <- "Moderate"
#   data$effort[which(data$effort$STBD.Visibility == "Fair (CCG)"),]$STBD.Visibility <- "Moderate"
# }


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
