history <- London_sf
df_poly <- lp_WGS84
Census <- London_Census


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
      df$sc_max[i] <- max(df$self_reinforcement[w[[i]]])
      df$sc_avg[i] <- mean(df$self_reinforcement[w[[i]]])
    }
  }else if(type == 2){
    df$sc_max <- rep(0,nrow(df))
    df$sc_avg <- rep(0,nrow(df))
    for(i in 1:nrow(df)){
      df$sc_max[i] <- max(df$self_reinforcement[w[[i]]])
      df$sc_avg[i] <- mean(df$self_reinforcement[w[[i]]])
    }
  }
  
  return(df)
}

## New ##
get_all_data <- function(history,df_poly,Census,
                         pre_start_date,pre_end_date,
                         post_start_date,post_end_date){
  ## new business
  pre_new <- new_business_in_range(history, pre_start_date,pre_end_date)
  post_new <- new_business_in_range(history, post_start_date,post_end_date)
  
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

pre_start_date <- "2019-04-01"
pre_end_date <- "2020-03-31"
post_start_date <- "2020-04-01"
post_end_date <- "2021-03-31"

new_data <- get_all_data(history,df_poly,Census,
                         pre_start_date,pre_end_date,
                         post_start_date,post_end_date)

stg_data_1 <- as.data.frame(new_data)

economic_factors <- c("log_new_rate","median_house_price",
                      "number_of_jobs_in_area",
                      "median_household_income_estimate")

TEMP_1 <- stg_data_1[economic_factors]
colnames(TEMP_1)[1] <- "log_BA"
colnames(TEMP_1) <- c("log_BA","median_house_price","number_of_jobs","median_household_income")

m1 <- lm(log_BA ~ median_house_price+number_of_jobs+median_household_income, data=TEMP_1)

economic_factors <- c("self_reinforcement","median_house_price",
                      "number_of_jobs_in_area",
                      "median_household_income_estimate")

TEMP_2 <- stg_data_1[economic_factors]
colnames(TEMP_2) <- c("log_BA","median_house_price","number_of_jobs","median_household_income")

m2 <- lm(log_BA ~ median_house_price+number_of_jobs+median_household_income, data=TEMP_2)

plot1 <- plot_model(arm::standardize(m2), breakLabelsAt = 30, axis.lim = c(4,-2)) + ggtitle("Pre-Pandemic (log New Openings)")
plot2 <- plot_model(arm::standardize(m1), breakLabelsAt = 30, axis.lim = c(4,-2)) + ggtitle("Post-Pandemic (log New Openings)")
grid.arrange(plot1, plot2, nrow=2)
ggsave("econ_New.png", arrangeGrob(plot1, plot2))

demographic_factors <- c("log_new_rate","bame_rate", "population",
                          "median_age", "employment_rate",
                         "imd_score")

TEMP_1 <- stg_data_1[demographic_factors]
colnames(TEMP_1)[1] <- "log_BA"

m1 <- lm(log_BA ~ bame_rate+population+median_age+employment_rate+imd_score, data=TEMP_1)

demographic_factors <- c("self_reinforcement","bame_rate", "population",
                          "median_age", "employment_rate",
                         "imd_score")

TEMP_2 <- stg_data_1[demographic_factors]
colnames(TEMP_2)[1] <- "log_BA"

m2 <- lm(log_BA ~ bame_rate+population+median_age+employment_rate+imd_score, data=TEMP_2)

plot1 <- plot_model(arm::standardize(m2), breakLabelsAt = 30, axis.lim = c(2,-4)) + ggtitle("Pre-Pandemic (log New Openings)")
plot2 <- plot_model(arm::standardize(m1), breakLabelsAt = 30, axis.lim = c(2,-4)) + ggtitle("Post-Pandemic (log New Openings)")
        
grid.arrange(plot1, plot2, nrow=2)
ggsave("demographic_New.png", arrangeGrob(plot1, plot2))


all_factors <- c("log_new_rate","self_reinforcement", "dist_to_centre",
                 "median_house_price",
                 "number_of_jobs_in_area",
                 "median_household_income_estimate",
                 "bame_rate", "population",
                 "median_age", "employment_rate",
                 "imd_score")

TEMP_1 <- stg_data_1[all_factors]
m2 <- lm(log_new_rate ~ dist_to_centre+median_house_price+number_of_jobs_in_area+median_household_income_estimate+
                        bame_rate+population+median_age+employment_rate+imd_score, data=TEMP_1)
m1 <- lm(self_reinforcement ~ dist_to_centre+median_house_price+number_of_jobs_in_area+median_household_income_estimate+
                              bame_rate+population+median_age+employment_rate+imd_score, data=TEMP_1)

summary(m1)
summary(m2)


pre_start_date <- "2019-04-01"
pre_end_date <- "2020-03-31"
post_start_date <- "2020-04-01"
post_end_date <- "2021-03-31"

new_data <- get_all_data(history,df_poly,Census,
                         pre_start_date,pre_end_date,
                         post_start_date,post_end_date)

stg_data_1 <- as.data.frame(new_data)

pre_start_date <- "2018-04-01"
pre_end_date <- "2019-03-31"
post_start_date <- "2019-04-01"
post_end_date <- "2020-03-31"

new_data <- get_all_data(history,df_poly,Census,
                         pre_start_date,pre_end_date,
                         post_start_date,post_end_date)

stg_data_2 <- as.data.frame(new_data)

summary(m1<-lm(log_new_rate~self_reinforcement,stg_data_2))
summary(m2<-lm(log_new_rate~self_reinforcement,stg_data_1))

summary(m1<-lm(log_new_rate~sc_avg+sc_max,stg_data_2))
summary(m2<-lm(log_new_rate~sc_avg+sc_max,stg_data_1))

plot1 <- plot_model(arm::standardize(m1), breakLabelsAt = 30, axis.lim = c(4,-2)) + ggtitle("Pre-Pandemic (log New Openings)")
plot2 <- plot_model(arm::standardize(m2), breakLabelsAt = 30, axis.lim = c(4,-2)) + ggtitle("Post-Pandemic (log New Openings)")

grid.arrange(plot1, plot2, nrow=2)
ggsave("sc_new.png", arrangeGrob(plot1, plot2))

summary(m1<-lm(log_new_rate~dist_to_centre+median_house_price+number_of_jobs_in_area+median_household_income_estimate+
                 bame_rate+population+median_age+employment_rate+imd_score+self_reinforcement+sc_avg+sc_max,stg_data_2))
summary(m2<-lm(log_new_rate~dist_to_centre+median_house_price+number_of_jobs_in_area+median_household_income_estimate+
                 bame_rate+population+median_age+employment_rate+imd_score+self_reinforcement+sc_avg+sc_max,stg_data_1))



### Prediction ###

pre_start_date <- "2017-04-01"
pre_end_date <- "2018-03-31"
post_start_date <- "2018-04-01"
post_end_date <- "2019-03-31"

new_data <- get_all_data(history,df_poly,Census,
                         pre_start_date,pre_end_date,
                         post_start_date,post_end_date)

stg_data <- as.data.frame(new_data)

pre_start_date <- "2019-04-01"
pre_end_date <- "2020-03-31"
post_start_date <- "2020-04-01"
post_end_date <- "2021-03-31"

new_data <- get_all_data(history,df_poly,Census,
                         pre_start_date,pre_end_date,
                         post_start_date,post_end_date)

stg_data_1 <- as.data.frame(new_data)

pre_start_date <- "2018-04-01"
pre_end_date <- "2019-03-31"
post_start_date <- "2019-04-01"
post_end_date <- "2020-03-31"

new_data <- get_all_data(history,df_poly,Census,
                         pre_start_date,pre_end_date,
                         post_start_date,post_end_date)

stg_data_2 <- as.data.frame(new_data)


full.new <-lm(log_new_rate~dist_to_centre+median_house_price+number_of_jobs_in_area+median_household_income_estimate+
                             bame_rate+population+median_age+employment_rate+imd_score+
                             self_reinforcement+sc_avg+sc_max,stg_data)

step.new <- stepAIC(full.new, direction = "both", trace = FALSE)

summary(step.new)

post_hat <- predict(step.new, newdata = stg_data_1)
pre_hat <- predict(step.new, newdata = stg_data_2)


pre.hat <- (pre_hat>=0) 
pre.true <- (stg_data_2$log_new_rate >= 0)
post.hat <- (post_hat>=0 )
post.true <- (stg_data_1$log_new_rate >= 0)


BA_result  <- data.frame(pre.hat,pre.true,post.hat,post.true)

print("The accuracy for pre-pandemic period is:")
print((sum(BA_result$pre.hat==BA_result$pre.true))/625)
print((sum(BA_result$pre.hat[BA_result$pre.true==TRUE]
           ==BA_result$pre.true[BA_result$pre.true==TRUE]))/sum(BA_result$pre.true==TRUE))
print((sum(BA_result$pre.hat[BA_result$pre.true==FALSE]
           ==BA_result$pre.true[BA_result$pre.true==FALSE]))/sum(BA_result$pre.true==FALSE))

print("The accuracy for post-pandemic period is:")
print((sum(BA_result$post.hat==BA_result$post.true))/625)
print((sum(BA_result$post.hat[BA_result$post.true==TRUE]
           ==BA_result$post.true[BA_result$post.true==TRUE]))/sum(BA_result$post.true==TRUE))
print((sum(BA_result$post.hat[BA_result$post.true==FALSE]
           ==BA_result$post.true[BA_result$post.true==FALSE]))/sum(BA_result$post.true==FALSE))
