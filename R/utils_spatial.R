# helper funtions for spatial objects and plotting
coord_fr_extent <- function(sf){
  coord_sf(c(xlim=extent(sf)[1:2]), ylim= c(extent(sf)[3:4]))
}

get_sf_ext <- function(sf){
  sf <- sf %>% st_union() %>% st_sf()
  print(extent(sf %>% st_transform(4326)))
}
