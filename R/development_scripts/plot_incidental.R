# add incidental sighings to plot

# 1. Run "workflow" script
# 2. Run to_run_evas script
# 3. Run evas script
# 4. take gg object from workflow and add incidental sightings

s5a <- sightings[5,]
AP <- SpatialPointsDataFrame(cbind(s5a$"final.lon",positions$final.lat), data=positions, proj4string=CRS("+proj=longlat"))
