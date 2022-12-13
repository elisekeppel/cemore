# x <- read.csv("hydrophone_data/hydrophone_deployment_locations.csv")
# Aug 2021
# x <- readOGR(dsn = "hydrophone",
#              layer = "hydrophones_2021aug") %>% st_as_sf() %>% st_transform(crs = 4326) %>% as("Spatial")
# y <- x %>% st_as_sf()
# rgdal::writeOGR(x, dsn = "hydrophone/hydrophones_2021aug.gpx", layer="points", driver="GPX",  dataset_options= "GPX_USE_EXTENSIONS=yes", overwrite_layer = T)
# y <- plotKML::readGPX("hydrophone/hydrophones_2021aug.gpx")[[3]]
#

# x <- read.csv("hydrophone_data/hydrophone_deployment_locations.csv") #
#
# x <- data.frame(SurveyID = surveyid,
#                 Sgt_ID = NA,
#                 Species = "Acoustic Recorder",
#                 year = 2021,
#                 month = 8,
#                 TransectID = NA,
#                 season = "summer",
#                 month_abb = "Aug",
#                 month_year = "Aug 2021",
#                 lat = x$lat[1],
#                 lon = x$lon[1]) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#   st_transform(crs = st_crs(bc_shp))


library(dplyr)
library(lubridate)
# library(magrittr)
library(rgdal)
library(sf)
library(magrittr)
# Oct 2021
x <- readOGR("data/hydrophone_deployment_wgs84.shp", verbose = F) %>% st_as_sf() %>% st_transform(crs = 4326)
y <- x %>% mutate(location = Deployed,
                  lat=POINT_Y,
                  lon=POINT_X,
                  deployment_date = c("2021-08-21","2021-08-21",NA,"2021-10-17","2021-10-16"),
                  deployment_year = year(deployment_date),
                  deployment_month = month(deployment_date),
                  retrieval_date = c("2021-10-16","2021-08-21",NA,"2021-11-20","2021-11-22"),
                  address <- factor(c(79,80,NA,31,78)),
                  deployed = "Acoustic Recorder")
y %>% as.data.frame() %>%
  dplyr::select(-c(POINT_Y,POINT_X,Deployed,geometry))%>%
  write.csv("data/first5_hydrophone_deployments.csv",row.names=F)
# ggplot() +
#   geom_sf(data = coast, fill = "light yellow") +
#   geom_sf(data = y, aes(colour = address)) +
#   coord
# View(x)

# x <- acoust %>% mutate(SurveyID = surveyid,
#                        Sgt_ID = NA,
#                        Species = "Acoustic Recorder",
#                        year = 2021,
#                        month = 10,
#                        TransectID = NA,
#                        season = "summer",
#                        month_abb = "Aug",
#                        month_year = "Aug 2021",
#                        beauf = NA,
#                        PSD_nm = NA,
#                        BestNumber = 1) %>%
#   dplyr::select(names(ap_sf))
