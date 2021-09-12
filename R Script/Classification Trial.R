#Read Greater London Edit History
London_history <- read_csv("London History Shapefile/London_Data_v6.csv") %>% clean_names()

#Transform the dataframe with edit info into a sf object
London_sf <-  st_as_sf(London_history, coords = c("lon", "lat"), 
                                    crs = 4326, agr = "constant")

# London_sf$ts <- as.Date(London_sf$ts)
rm(London_history)

#TEMP <- selectByDate(lp_WGS84, start = "1/1/1999", end = "31/12/1999")

####################
##  New Business  ##
####################
new_business_in_range <- function(history, date1, date2){
  new_busi <- with(history, history[(ts >= date1 & ts <= date2 & version == 1) |
                                    (ts >= date1 & ts <= date2 & closure_tag != 'False'), ])
  return(new_busi)
}

##############
##  Update  ##
##############
updates_in_range <- function(history, date1, date2){
  new_updates <- with(history, history[(ts >= date1 & ts <= date2 & version != 1 & closure_tag == 'False'), ])
  return(new_updates)
}

############
##  PoIs  ##
############
poi_in_range <- function(history, date){
  # return all distinct pois in the specified timeframes
  poi <- with(history, history[(ts <= date), ])
  unique_poi <- distinct(poi, id, .keep_all = TRUE)
  return(unique_poi)
}


################ 
## Statistics ##
################
pre_new <- new_business_in_range(London_sf, "2019-04-01", "2020-03-31")
post_new <- new_business_in_range(London_sf, "2020-04-01", "2021-03-31")

pre_update <- updates_in_range(London_sf, "2019-04-01", "2020-03-31")
post_update <- updates_in_range(London_sf, "2020-04-01", "2021-03-31")

pre_poi <- poi_in_range(London_sf, "2020-03-31")
post_poi <- poi_in_range(London_sf, "2021-03-31")


#######  Census Data  #######

London_Census <- read_csv("London_Census.csv") %>% clean_names()
colnames(London_Census)[2] <- "GSS_CODE"
London_Census <- na.replace(London_Census, 0)

#############
##  Pivot  ##
#############

pt_in_polygons <- function(df1,df2){
  a <- df1
  b <- df2
  a$pt_count <- lengths(st_intersects(a,b))
  return(a)
}

poi_in_polygons <- function(df1,df2){
  a <- df1
  b <- df2
  a$poi <- lengths(st_intersects(a,b))
  return(a)
}

pre_new_data <- pt_in_polygons(lp_WGS84,pre_new)
pre_new_data <- poi_in_polygons(pre_new_data,pre_poi)
pre_new_data <- merge(pre_new_data, London_Census, by = "GSS_CODE", all.x = TRUE)
pre_new_data$new_rate <- pre_new_data$pt_count/pre_new_data$area
pre_new_data$log_new_rate <- log(pre_new_data$pt_count/pre_new_data$area)
pre_new_data$log_new_rate[pre_new_data$log_new_rate == -Inf] <- log(0.01)

post_new_data <- pt_in_polygons(lp_WGS84,post_new)
post_new_data <- poi_in_polygons(post_new_data,post_poi)
post_new_data <- merge(post_new_data, London_Census, by = "GSS_CODE", all.x = TRUE)
post_new_data$new_rate <- post_new_data$pt_count/post_new_data$area
post_new_data$log_new_rate <- log(post_new_data$pt_count/post_new_data$area)
post_new_data$log_new_rate[post_new_data$log_new_rate == -Inf] <- log(0.01)


## Summary Statistics ##
summary(pre_new_data$pt_count)
summary(post_new_data$pt_count)

# economics_factor <- c("median_house_price",
#                       "number_of_jobs_in_area",
#                       "median_household_income_estimate",
#                       "imd_score")

## Choropleth Map
ggplot() + geom_sf(data = pre_new_data, aes(fill = log_new_rate))
ggplot() + geom_sf(data = post_new_data, aes(fill = log_new_rate))

