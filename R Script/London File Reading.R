##########################################
##  Loading Files for further Analysis  ##
##########################################

library(sf)
shp_name <- "London History Shapefile/London_History.shp"
London_sf <- st_read(shp_name)
London_sf$TS <- as.Date(London_sf$TS)

shp_name <- "London Shapefile/London_Ward_CityMerged.shp"
London_polygons <- st_read(shp_name)

## Ploting Map of London
ggplot() + geom_sf(data = London_polygons)

## Check if the crs version is consistent
st_crs(London_sf) == st_crs(London_polygons)

st_crs(London_polygons)
st_crs(London_sf)

lp_WGS84 <- st_transform(London_polygons,4326)
st_crs(lp_WGS84)

st_crs(London_sf) == st_crs(lp_WGS84)

## Ploting Map of London
ggplot() + geom_sf(data = lp_WGS84)

# intersection
intersection <- st_intersects(lp_WGS84,London_sf)

# Subesetting
intersection <- London_sf[unlist(intersection),]

plot(st_geometry(lp_WGS84), border="#aaa")

lp_WGS84$pt_count <- lengths(st_intersects(lp_WGS84,London_sf))

ggplot() + geom_sf(data = lp_WGS84, aes(fill = pt_count))

summary(lp_WGS84$pt_count)


