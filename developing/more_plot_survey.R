# MORE PLOTTING IN DEVELOPMENT - CAN'T SOURCE FROM CEMORE_ANALYSIS YET
# ------------------------------------------------------------------------------
# plot tag deployment locations
# ------------------------------------------------------------------------------
x <- read.csv("tag_data/2021-08 tag deployment locations.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(bc_shp))
# st_as_sf(coords = c("lon", "lat"), crs = st_crs(bc_shp)) %>%
# st_transform(crs = 4326)

Save <-  T
if(!Save) coast_file <- coast else coast_file <- bc_shp

ggplot() +
  geom_sf(data = coast_file, fill = "light yellow", colour = "grey 60", size = 0.25) +
  geom_sf(data = x, size = 1, colour = "black") +
  coord +
  # theme(panel.background= element_rect(fill = "light blue")) +
  ggtitle("Humpback Whale Tagging Locations August 2021")


if(Save){
  ggsave(paste0("tagging_maps/tagging locations 2021 August.png"))
}

# ------------------------------------------------------------------------------
# plot hydrophone deployment locations
# ------------------------------------------------------------------------------
x <- read.csv("hydrophone_data/hydrophone_deployment_locations.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(bc_shp))
# st_as_sf(coords = c("lon", "lat"), crs = st_crs(bc_shp)) %>%
# st_transform(crs = 4326)

Save <- F
if(!Save) coast_file <- coast else coast_file <- bc_shp
display.brewer.all()

bathy <- getNOAA.bathy(-125.5, -123,48.1, 49.5,res=1, keep=TRUE)
bathy <- as.raster(bathy)
plot(bathy, col = RColorBrewer::brewer.pal(20L, "Blues"))

ggplot() + geom_sf(data = coast_file, fill = "light yellow", colour = "grey 60", size = 0.25) +
  # geom_sf(data = x, size = 1, colour = "black") +
  coord +
  theme(panel.background= element_rect(fill = "light blue")) +
  ggtitle("Hydrophone Deployment Location August 21, 2021") +
  geom_raster(data = bathy)

bathy %<>% as.matrix()
bathy %>% as.data.frame() %>%
  rownames_to_column(var = "lon") %>%
  gather(lat, value, -1) %>%
  mutate_all(funs(as.numeric)) %>%
  ggplot()+
  geom_contour(aes(x = lon, y = lat, z = value), bins = 10, colour = "black") +
  coord_map()


if(Save){
  ggsave(paste0("hydrophone_data/deployment locations 2021 August.png"))
}
