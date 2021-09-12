#################################################
## Experiment Trial of Lienar Model for London ##
#################################################

## Import Library  ##
library(MASS)
library(QuantPsyc)


all_factors <- c("log_new_rate", "bame_rate", "population",
                 "median_house_price", "mean_age", "median_age", "employment_rate",
                 "number_of_jobs_in_area", "median_household_income_estimate",
                 "imd_score", "dist_to_centre")

socio_demographic_factor <-c("log_new_rate", "bame_rate", "population",
                             "mean_age", "median_age", "employment_rate",
                             "imd_score")

economics_factor <- c("log_new_rate", "median_house_price",
                      "number_of_jobs_in_area",
                      "median_household_income_estimate")


## Fit the full model ##
pre_stg_data <- as.data.frame(pre_new_data)[all_factors]
pre.full.model <- lm(log_new_rate ~ bame_rate + population + median_house_price 
                                  + mean_age + median_age + employment_rate 
                                  + number_of_jobs_in_area + median_household_income_estimate
                                  + imd_score + dist_to_centre, data = pre_stg_data)

post_stg_data <- as.data.frame(na.omit(post_new_data))[all_factors]
post.full.model <- lm(log_new_rate ~ bame_rate + population + median_house_price 
                     + mean_age + median_age + employment_rate 
                     + number_of_jobs_in_area + median_household_income_estimate
                     + imd_score + dist_to_centre, data = post_stg_data)

summary(pre.full.model)
summary(post.full.model)

## Stepwise regression model ##
pre.step.model <- stepAIC(pre.full.model, direction = "both", trace = FALSE)
post.step.model <- stepAIC(post.full.model, direction = "both", trace = FALSE)

summary(pre.step.model)
summary(post.step.model)

arm::standardize(pre.step.model)
TEMP <- arm::standardize(pre.full.model)

lm.beta(pre.step.model)
lm.beta(post.step.model)

#######################
## Regression Result ##
#######################

# plot_model(pre.full.model, breakLabelsAt = 30) + ggtitle("Pre-Pandemic New Business")
plot1 <- plot_model(arm::standardize(pre.full.model), breakLabelsAt = 30) + ggtitle("Full::Pre-Pandemic New Business")
plot2 <- plot_model(arm::standardize(post.full.model), breakLabelsAt = 30) + ggtitle("Full::Post-Pandemic New Business")
grid.arrange(plot1, plot2, ncol=2)
ggsave("full_new_lm_std.jpg", arrangeGrob(plot1, plot2))

plot1 <- plot_model(arm::standardize(pre.step.model), breakLabelsAt = 30) + ggtitle("Stepwise::Pre-Pandemic New Business")
plot2 <- plot_model(arm::standardize(post.step.model), breakLabelsAt = 30) + ggtitle("Stepwise::Post-Pandemic New Business")
grid.arrange(plot1, plot2, ncol=2)
ggsave("step_new_lm_std.jpg", arrangeGrob(plot1, plot2))


tab_model(pre.step.model, dv.labels = "Pre-Covid New Business")
tab_model(post.step.model, dv.labels = "Post-Covid New Business")

tab_model(arm::standardize(pre.full.model), dv.labels = "Full::Pre-Covid New Business")
tab_model(arm::standardize(post.full.model), dv.labels = "Full::Post-Covid New Business")
tab_model(arm::standardize(pre.step.model), dv.labels = "Stepwise::Pre-Covid New Business")
tab_model(arm::standardize(post.step.model), dv.labels = "Stepwise::Post-Covid New Business")
