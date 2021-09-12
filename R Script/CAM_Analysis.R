## Cellular Automata Model Analysis ##

library(gap)

## New ##
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
  new_data$pre_new_rate <- new_data$pre_new/new_data$area
  new_data$self_reinforcement <- log(new_data$pre_new/new_data$area)
  new_data$self_reinforcement[new_data$self_reinforcement == -Inf] <- log(0.05)
  
  # data for current year
  new_data$new_rate <- new_data$new/new_data$area
  new_data$log_new_rate <- log(new_data$new/new_data$area)
  new_data$log_new_rate[new_data$log_new_rate == -Inf] <- log(0.05)
  
  # spatial correlation
  nb_sc_compute(new_data,1)
}

## Update ##
get_all_data <- function(history,df_poly,Census,
                         pre_start_date,pre_end_date,
                         post_start_date,post_end_date){
  ## new business
  # pre_new <- new_business_in_range(history, pre_start_date,pre_end_date)
  # post_new <- new_business_in_range(history, post_start_date,post_end_date)
  
  ## user activities
  pre_update <- updates_in_range(history, pre_start_date,pre_end_date)
  post_update <- updates_in_range(history, post_start_date,post_end_date)
  
  ## total number of poi
  # pre_poi <- poi_in_range(history, pre_end_date)
  # post_poi <- poi_in_range(history, post_end_date)
  
  # return(list("pre_new" = pre_new,
  #             "post_new" = post_new,
  #             "pre_update" = pre_update,
  #             "post_update" = post_update))
  #             #,"pre_poi" = pre_poi,
  #             #"post_poi" = post_poi))
  
  new_data <- pts_in_polygons(df_poly,pre_update,"pre_update")
  new_data <- pts_in_polygons(new_data,post_update,"update")
  
  # left join geodemographic data
  new_data <- merge(new_data, Census, by = "GSS_CODE", all.x = TRUE)
  
  # data for previous year (self reinforcement)
  new_data$self_reinforcement <- log(new_data$pre_update/new_data$area)
  new_data$self_reinforcement[new_data$self_reinforcement == -Inf]  <- log(0.05)
  
  # data for current year
  new_data$update[new_data$update==0] <-0.1
  new_data$update_rate <- new_data$update/new_data$area
  new_data$log_update_rate <- log(new_data$update/new_data$area)
  new_data$log_update_rate[new_data$log_update_rate == -Inf] <- log(0.05)
  
  # spatial correlation
  nb_sc_compute(new_data,2)
}

pre_start_date <- "2019-04-01"
pre_end_date <- "2020-03-31"
post_start_date <- "2020-04-01"
post_end_date <- "2021-03-31"

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

stg_data_1 <- as.data.frame(new_data)
stg_data_2 <- as.data.frame(new_data_2)

m1 <- lm(log_new_rate ~ dist_to_centre, data = stg_data_1)
m2 <- lm(log_new_rate ~ dist_to_centre, data = stg_data_2)

summary(m1)
summary(m2)

# y1 <- stg_data_1$log_new_rate
# y2 <- stg_data_2$log_new_rate
y1 <- stg_data_1$log_update_rate
y2 <- stg_data_2$log_update_rate



x1 <- as.matrix(stg_data_1[c("median_house_price",
                             "number_of_jobs_in_area",
                             "median_household_income_estimate",
                             "bame_rate", "population",
                             "mean_age", "median_age", "employment_rate",
                             "imd_score","dist_to_centre")])

x2 <- as.matrix(stg_data_2[c("median_house_price",
                             "number_of_jobs_in_area",
                             "median_household_income_estimate",
                             "bame_rate", "population",
                             "mean_age", "median_age", "employment_rate",
                             "imd_score","dist_to_centre")])

Geodemographic <- chow.test(y1,x1,y2,x2)

x1 <- as.matrix(stg_data_1[c("self_reinforcement")])
x2 <- as.matrix(stg_data_2[c("self_reinforcement")])

Self_Reinforcement <- chow.test(y1,x1,y2,x2)

x1 <- as.matrix(stg_data_1[c("sc_avg","sc_max")])
x2 <- as.matrix(stg_data_2[c("sc_avg","sc_max")])

Spatial_Correlation <- chow.test(y1,x1,y2,x2)

x1 <- as.matrix(stg_data_1[c("median_house_price",
                             "number_of_jobs_in_area",
                             "median_household_income_estimate",
                             "bame_rate", "population",
                             "mean_age", "median_age", "employment_rate",
                             "imd_score","dist_to_centre",
                             "self_reinforcement",
                             "sc_avg","sc_max")])

x2 <- as.matrix(stg_data_2[c("median_house_price",
                             "number_of_jobs_in_area",
                             "median_household_income_estimate",
                             "bame_rate", "population",
                             "mean_age", "median_age", "employment_rate",
                             "imd_score","dist_to_centre",
                             "self_reinforcement",
                             "sc_avg","sc_max")])

All_Spatio_Temporal <- chow.test(y1,x1,y2,x2)

# chow_table <- as.data.frame(rbind(Geodemographic,Self_Reinforcement,Spatial_Correlation,All_Spatio_Temporal))
chow_table_2 <- as.data.frame(rbind(Geodemographic,Self_Reinforcement,Spatial_Correlation,All_Spatio_Temporal))

chow_table$`P value` <- formatC(chow_table$`P value`, format = "e", digits = 2)
chow_table_2$`P value` <- formatC(chow_table_2$`P value`, format = "e", digits = 2)
agg_chow_table <- cbind(chow_table,chow_table_2)

xtable(agg_chow_table[c(1,4,5,8)])
