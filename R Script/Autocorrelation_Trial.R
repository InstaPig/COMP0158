###########################
## Autocorrelation Trial ##
###########################

library(sp)
library(spdep)

#We coerce the sf object into a new sp object
post_new_data_sp <- as(post_new_data, "Spatial")


#Then we create a list of neighbours using the Queen criteria
w <- poly2nb(post_new_data_sp)#, row.names=post_new_data_sp$GSS_CODE)
post_new_data$nb_list <- lapply(w, TEMP)

TEMP <- st_coordinates(st_centroid(st_geometry(lp_WGS84$geometry)))
plot(w, TEMP,  col="red")
summary(w)

wm <- nb2mat(w, style='B')
rwm <- mat2listw(wm, style='W')

lm.morantest(post.full.model, rwm, alternative="two.sided")

## density plots of residuals by Borough
ggplot(post_new_data, aes(x = post.step.model$residuals, colour = as.factor(BOROUGH))) + geom_density() 

plot(post.full.model$residuals)


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
      df$sc_max[i] <- max(df$log_new_rate[w[[i]]])
      df$sc_avg[i] <- mean(df$log_new_rate[w[[i]]])
    }
  }else if(type == 2){
    df$sc_max <- rep(0,nrow(df))
    df$sc_avg <- rep(0,nrow(df))
    for(i in 1:nrow(df)){
      df$sc_max[i] <- max(df$log_update_rate[w[[i]]])
      df$sc_avg[i] <- mean(df$log_update_rate[w[[i]]])
    }
  }
  
  return(df)
}

TEST <- nb_sc_compute(post_new_data,1)

ggplot(TEST, aes(x=sc_max, y=log_new_rate)) + geom_point(shape=18, color="blue") + geom_smooth(method=lm,  linetype="dashed",color="darkred", fill="blue")
