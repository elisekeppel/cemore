# -------------------------------------------------------------------------------
# ---- substrate
# -------------------------------------------------------------------------------

# bath <- data("covariates/multibeam_4b_zone10/MULTIBEAM_4B_ZONE101.tif")
# # gc()
# bath <-   as.bathy(bath)
#
#
# st_crs(bath)
# bath_utm9n <- rgdal:::gdaltransform(s_srs = "EPSG:32610", t_srs = "ESPG:3156", coords = bath)
#
# ggplot() + geom_sf(data = coast_file, fill = "light yellow", colour = "grey 60") +
#   # geom_sf(data = ap_sf, size = 1.5, aes(colour = Species, fill = Species, shape =  Species)) +
#   geom_raster(data = bath@data, aes(x = x, y = y, fill = salish_sea_20m)) +
#   coord
# ggsave("test_w_bath.png")
# Load substrate raster and create bounding box for study area

# sub <- raster("covariates/salish_sea_20m/salish_sea_20m.tif")#crs = 3005
# plot(sub)
# object.size(sub) # 14464 bytes

sub_clip <- raster("covariates/sub_arc_clip.tif")#crs = 3005
plot(sub_clip)
object.size(sub_clip) # 14464 bytes


sub_clip_df <- as.data.frame(sub_clip, xy = T)%>%
  na.omit()
# sub_sf <- st_as_sf(sub_df, coords=c(lon = sub_df$x, lat = sub_df$y))
object.size(sub_clip_df)  # 3521621736 bytes

# bb <- create_bb(-125.5, -123., 48.1, 49.5) %>% st_transform(3005)
# tss_6km_2 <- st_read(
#   "C:/Users/keppele/Documents/CeMoRe/Design/cemore_design/shapefiles/study_area",
#   "TSS_buffer6km_2") %>%
#   st_transform(3005)

# plot(tss_6km_2)
# plot(bb, add=T)
# crop raster to study area
# sub_cropped <- crop(sub, extent(tss_6km_2))
# object.size(sub_cropped)/1000

# convert to data.frame for plotting
sub_clip_df2 <- sub_clip %>%
  as.bathy() %>%
  fortify.bathy() %>%
  na.omit()
object.size(sub_clip_df2) # 3521621728 bytes

ggRasterly(data = sub_clip_df2, aes(x=x, y=y, color=z),
           show_raster = T,
           drop_data = T)


sub_mask <- raster("covariates/sub_extract_mask.tif") %>%
  na.omit() #crs = 3005
plot(sub_mask)
object.size(sub_mask) # 14480 bytes bytes

saveRDS(sub_cropped_df, "covariates/substrate_cropped_df.rds")
sub_cropped_df <- readRDS("covariates/substrate_cropped_df.rds")

sub_mask_df <- sub_mask %>%
  as.bathy() %>%
  fortify.bathy() %>%
  na.omit()
object.size(sub_clip_df2) # 3,790,000 bytes

sub_mask_df2 <- as.data.frame(sub_mask, xy = T)%>%
  na.omit()
#------------------------------------------------------------------------------
# try converting arc-clipped substrate layer to spdf, then st_as_sf
#------------------------------------------------------------------------------


sub_clip_df <- as(sub_clip,'SpatialPolygonsDataFrame')
object.size(sub_clip_df)

sub_clip_sf <- st_as_sf(sub_clip_df)
#------------------------------------------------------------------------------

ggplot() + #geom_sf(data = st_transform(coast_file, crs = 4326), fill = "light yellow", colour = "grey 60") +
  # geom_sf(data = bb) +
  geom_raster(data = sub_df, aes(x = x, y = y, fill = sub_arc_clip)) +
  coord

area <- st_read("C:/Users/keppele/Documents/CeMoRe/Design/cemore_design/shapefiles/study_area", "cemore_study_area_TSS_6km_can")
tss_6km_2 <- st_read(
  "C:/Users/keppele/Documents/CeMoRe/Design/cemore_design/shapefiles/study_area", "TSS_buffer6km_2"
) %>%
  st_transform(3005)

ggsave("test_w_sub.png")

