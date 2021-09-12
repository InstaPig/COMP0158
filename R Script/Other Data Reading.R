library(sf)
library(units)
## IMD Score (2019)
IMD_2019 <- read.csv("IMD_ENG.csv")

#Read Greater London Edit History
Manchester_history <- read_csv("London History Shapefile/Manchester_Data_v6.csv") %>% clean_names()
WM_history <- read_csv("London History Shapefile/West_Midlands_Data_v6.csv") %>% clean_names()

#Transform the dataframe with edit info into a sf object
Manchester_sf <-  st_as_sf(Manchester_history, coords = c("lon", "lat"), 
                       crs = 4326, agr = "constant")
WM_sf <-  st_as_sf(WM_history, coords = c("lon", "lat"), 
                           crs = 4326, agr = "constant")

shp_name <- "Manchester/england_cmwd_2011.shp"
Manchester_polygons <- st_read(shp_name)
Manchester_polygons <- st_transform(Manchester_polygons,4326)

shp_name <- "West_Midlands/england_cmwd_2011.shp"
WM_polygons <- st_read(shp_name)
WM_polygons <- st_transform(WM_polygons,4326)

colnames(Manchester_polygons)[4] <- "GSS_CODE"
colnames(WM_polygons)[4] <- "GSS_CODE"

## Area Size ##
Manchester_polygons$area <- drop_units(st_area(Manchester_polygons)/1000000)
WM_polygons$area <- drop_units(st_area(WM_polygons)/1000000)

## Ploting Map of London
ggplot() + geom_sf(data = Manchester_polygons)
ggplot() + geom_sf(data = WM_polygons)


# intersection
intersection <- st_intersects(WM_polygons,WM_sf)

# Subesetting
intersection <- WM_sf[unlist(intersection),]

plot(st_geometry(WM_polygons), border="#aaa")

WM_polygons$pt_count <- lengths(st_intersects(WM_polygons,WM_sf))
Manchester_polygons$pt_count <- lengths(st_intersects(Manchester_polygons,Manchester_sf))

ggplot() + geom_sf(data = WM_polygons, aes(fill = pt_count))
ggplot() + geom_sf(data = Manchester_polygons, aes(fill = pt_count))

summary(Manchester_polygons$pt_count)


