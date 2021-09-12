### Chow Test ###
library(strucchange)
library(cowplot)

### Data ###
history <- London_sf
df_poly <- lp_WGS84
# Census <- London_Census
drop <- c("ward","gss_code_x","gss_code_y","log_median_house_price","log_properties_sold")
Census = London_Census[,!(names(London_Census) %in% drop)]

pre_start_date <- "2019-04-01"
pre_end_date <- "2020-03-31"
post_start_date <- "2020-04-01"
post_end_date <- "2021-03-31"


pre_start_date <- "2018-04-01"
pre_end_date <- "2019-03-31"
post_start_date <- "2019-04-01"
post_end_date <- "2020-03-31"

# pre_start_date <- "2017-04-01"
# pre_end_date <- "2018-03-31"
# post_start_date <- "2018-04-01"
# post_end_date <- "2019-03-31"

# pre_start_date <- "2016-04-01"
# pre_end_date <- "2017-03-31"
# post_start_date <- "2017-04-01"
# post_end_date <- "2018-03-31"

# pre_start_date <- "2015-04-01"
# pre_end_date <- "2016-03-31"
# post_start_date <- "2016-04-01"
# post_end_date <- "2017-03-31"

# Data

pts_in_polygons <- function(df1,df2,new_field){
  a <- df1
  b <- df2
  a[new_field] <- lengths(st_intersects(a,b))
  return(a)
}

## trial on computing neighbouring effect
nb_sc_compute <- function(df,type){
  
  #We coerce the sf object into a new sp object
  TEMP_sp <- as(df, "Spatial")
  
  #Then we create a list of neighbours using the Queen criteria
  w <- poly2nb(TEMP_sp)
  
  if(type == 1){
    df$sc_max <- rep(0,nrow(df))
    df$sc_avg <- rep(0,nrow(df))
    for(i in 1:nrow(df)){
      df$sc_max[i] <- max(df$new_rate[w[[i]]])
      df$sc_avg[i] <- mean(df$new_rate[w[[i]]])
    }
  }else if(type == 2){
    df$sc_max <- rep(0,nrow(df))
    df$sc_avg <- rep(0,nrow(df))
    for(i in 1:nrow(df)){
      df$sc_max[i] <- max(df$update_rate[w[[i]]])
      df$sc_avg[i] <- mean(df$update_rate[w[[i]]])
    }
  }
  
  return(df)
}

get_all_data <- function(history,df_poly,Census,
                         pre_start_date,pre_end_date,
                         post_start_date,post_end_date){
  ## new business
  pre_new <- new_business_in_range(history, pre_start_date,pre_end_date)
  post_new <- new_business_in_range(history, post_start_date,post_end_date)
  
  ## user activities
  # pre_update <- updates_in_range(history, pre_start_date,pre_end_date)
  # post_update <- updates_in_range(history, post_start_date,post_end_date)
  
  ## total number of poi
  # pre_poi <- poi_in_range(history, pre_end_date)
  # post_poi <- poi_in_range(history, post_end_date)
  
  # return(list("pre_new" = pre_new,
  #             "post_new" = post_new,
  #             "pre_update" = pre_update,
  #             "post_update" = post_update))
  #             #,"pre_poi" = pre_poi,
  #             #"post_poi" = post_poi))
  
  new_data <- pts_in_polygons(df_poly,pre_new,"pre_new")
  new_data <- pts_in_polygons(new_data,post_new,"new")
  
  # left join geodemographic data
  new_data <- merge(new_data, Census, by = "GSS_CODE", all.x = TRUE)
  
  # data for previous year (self reinforcement)
  new_data$self_reinforcement <- new_data$pre_new/new_data$area
  # new_data$pre_log_new_rate <- log(new_data$pre_new/new_data$area)
  # new_data$pre_log_new_rate[new_data$pre_log_new_rate == -Inf] <- log(0.01)
  
  # data for current year
  new_data$new_rate <- new_data$new/new_data$area
  # new_data$new_rate[new_data$new_rate==0] <- 0.01/new_data$area[new_data$new_rate==0]
  new_data$log_new_rate <- log(new_data$new_rate)
  new_data$log_new_rate[new_data$log_new_rate == -Inf] <- log(0.01)
  
  # spatial correlation
  nb_sc_compute(new_data,1)
}

pre_start_date <- "2019-04-01"
pre_end_date <- "2020-03-31"
post_start_date <- "2020-04-01"
post_end_date <- "2021-03-31"

# pre_start_date <- "2017-04-01"
# pre_end_date <- "2018-03-31"
# post_start_date <- "2018-04-01"
# post_end_date <- "2019-03-31"

new_data <- get_all_data(history,df_poly,Census,
                         pre_start_date,pre_end_date,
                         post_start_date,post_end_date)

pre_start_date <- "2018-04-01"
pre_end_date <- "2019-03-31"
post_start_date <- "2019-04-01"
post_end_date <- "2020-03-31"

new_data_2 <- get_all_data(history,df_poly,Census,
                           pre_start_date,pre_end_date,
                           post_start_date,post_end_date)

post_map <- ggplot() + geom_sf(data = new_data, aes(fill = log_new_rate))+ scale_fill_viridis_c(option = "D")
pre_map <- ggplot() + geom_sf(data = new_data_2, aes(fill = log_new_rate))+ scale_fill_viridis_c(option = "D")


plot_grid(pre_map, post_map, labels = "AUTO", rel_widths = c(0.6, 1))

#TEMP <- get_all_data(London_sf,pre_start_date,pre_end_date,post_start_date,post_end_date)

# Model
stg_data_1 <- as.data.frame(new_data)

m1 <- lm(log_new_rate ~ bame_rate + population + median_house_price 
                  + mean_age + median_age + employment_rate 
                  + number_of_jobs_in_area + median_household_income_estimate
                  + imd_score + dist_to_centre, data = stg_data_1)
summary(m1)
stg_data_2 <- as.data.frame(new_data_2)
m2 <- lm(log_new_rate ~ bame_rate + population + median_house_price 
                  + mean_age + median_age + employment_rate 
                  + number_of_jobs_in_area + median_household_income_estimate
                  + imd_score + dist_to_centre, data = stg_data_2)

kable(anova(m1, m2), 
      caption="Chow test for 'new business' before and after outbreak of pandemic")



### Chow Test 2 ###
all_factors <- c("bame_rate", "population",
                 "median_house_price", "mean_age", "median_age", "employment_rate",
                 "number_of_jobs_in_area", "median_household_income_estimate",
                 "imd_score", "dist_to_centre")

x1 <- as.matrix(stg_data_1[all_factors])
x2 <- as.matrix(stg_data_2[all_factors])
y1 <- stg_data_1$new_rate
y2 <- stg_data_2$new_rate


chow.test(y1,x1,y2,x2)




step.model <- stepAIC(full.model, direction = "both", trace = FALSE)

summary(step.model)

# Result

# plot(step.model$residuals)

plot_model(arm::standardize(full.model), breakLabelsAt = 30) + ggtitle("Full::Pre-Pandemic New")

# ggplot(stg_data[stg_data$new>=20,], aes(x = pre_new_rate, y = phat, colour = BOROUGH)) +
#   #geom_point(aes(y = new_rate), alpha=.5, position=position_jitter(h=.2)) +
#   geom_line(size = 1) +
#   labs(x = "Previous New Rate", y = "Expected New Rate")

new_data_2$predict <- predict(full.model,newdata = new_data_2)

predict(m1,newdata = new_data_2,type = "zero")>0.5


