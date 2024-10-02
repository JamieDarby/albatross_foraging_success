
# Load in data and prep script
source("scripts/1_prep.R")
load("data/cleaned/waal_gps_bi.RData")
load("data/cleaned/waal_act_bi.RData")
load("data/cleaned/waal_ing_bi.RData")

# Create an adjusted date variable to populate later
waal_ing_bi$date_time_ad <- waal_ing_bi$date_time

# Loop through ingestion df
for(i in 1:nrow(waal_ing_bi)){
  
  # See if there is an activity associated with 
  if(!is.na(waal_ing_bi$act[i])){
    
    # Look for ingestions that take place soon after takeoff
    if(waal_ing_bi$act[i] == "dry" &
       waal_ing_bi$which_nearest[i] == "prior" &
       waal_ing_bi$act_prior[i] < 120){
      # Adjust the date_time so that they take place when the bird is on water
      waal_ing_bi$date_time_ad[i] <-
        waal_ing_bi$date_time[i] - waal_ing_bi$nearest_act[i]}
    
    
    # Look for ingestions that take place soon before landing
    if(waal_ing_bi$act[i] == "dry" &
       waal_ing_bi$which_nearest[i] == "prox" &
       waal_ing_bi$act_prox[i] < 120){
      # Adjust the date_time so that they take place when the bird is on water
      waal_ing_bi$date_time_ad[i] <-
      waal_ing_bi$date_time[i] + waal_ing_bi$nearest_act[i]}
  }
}

# Read in list of sexes
sexes <- read.csv("data/WAAL_2009/sexes_weights_ages.csv")

# Split into a list by identity
waal_act_bi <- split(waal_act_bi, waal_act_bi$id) %>%
  # Append ingestion data to the activity list
  lapply(., act_ingest_append, ing = waal_ing_bi, mass_var = "mass") %>%
  # Append sex and weight data to activity list
  lapply(., function(x){
    info <- sexes[which(sexes$id == x$id[1]), ]
    x[, c("weight", "sex", "age")] <- info[1, c("weight", "sex", "age")]
    x
  }) %>%
  # Bind back into dataframe
  bind_rows()

# Remove dataframe of sexes and weights
rm(sexes)

# Model ingestions per landing --------------------------------------------

# Subset model data
act_mod_data <- waal_act_bi %>%
  # Remove points with no nearby locations and one extreme outlier
  filter(loc_offset < 7200,
         !is.na(ing_bool),
         # id != "5144771",
         !is.na(Latitude),
         !is.na(zsd)) %>%
  # Get a factor of id, and cube-root the dwell time
  mutate(id = as.factor(id),
         dwell_trans = seconds ^ (1/3),
         rain = rain * 1000,
         ing_mass = ifelse(ing_mass > 3000, 3000, ing_mass),
         daynight = as.factor(ifelse(sun_angle > -6, "day", "night")))

# Create ARStart variable to group data into individual tracks for AR1 structure
act_mod_data$ARStart <- F
for(i in 2:nrow(act_mod_data)){
  if(act_mod_data$id[i] != act_mod_data$id[i = 1]){
    act_mod_data$ARStart[i] <- T
  }
}

# Binomial foraging success model -----------------------------------------

# Model ingested mass against environmental data
waal_ing_mod_bin <- gam(data = act_mod_data,
           formula = ing_bool ~
             s(wind_sp, bs = "ts") +
             s(zsd, bs = "ts", by = daynight) +
             daynight +
             s(rain, bs = "ts") +
             s(dwell_trans, bs = "ts") +
             s(Longitude, Latitude, bs = "tp") +
             s(id, bs = "re"),
           gamma = 2,
           select = T,
           family = binomial(),
           na.action = "na.fail")

# Summarise
summary(waal_ing_mod_bin)
anova(waal_ing_mod_bin)

# Check AR coefficient, all good
acf(residuals(waal_ing_mod_bin))[1]

# Test model residuals for binomial model
testResiduals(simulateResiduals(waal_ing_mod_bin))

# Check concurvity
concurvity(waal_ing_mod_bin)

# Some performance stats for the binomial model ---------------------------

# Predict model over the same dataset
pr <- as.numeric(predict(waal_ing_mod_bin, act_mod_data, type = "response"))            

# Compare predicted values to actual values
pred <- prediction(pr, act_mod_data$ing_bool)


ROCR::performance(pred, measure="auc")@y.values

# Print AUC
ROCR::performance(pred, measure="f")@x.values[[1]][
  which.max(ROCR::performance(pred, measure="f")@y.values[[1]])
  ]

# Create ROC
perf <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr")         

# Plot out ROC
plot(perf, colorize = TRUE, print.cutoffs.at = c(0.1,0.2,0.3,0.4,0.5))

# Get coordinates for the ROC
y <- unlist(perf@y.values)
x <- unlist(perf@x.values)

# Get the index of the furthest point from a diagonal to the ROCR
ind <- which.max(sqrt(x^2+y^2) * sin(atan(y/x) - pi/4))

# to identify the threshold that corresponds to the maximum prediction accuracy
perf@alpha.values[[1]][ind]

# Confusion matrix based
confusionMatrix(as.factor(ifelse(pr > 0.12, T, F)),
                as.factor(act_mod_data$ing_bool),
                mode = "everything",
                positive="TRUE")

# Get rid of some temporary data
rm(pr, pred, perf, x, y, ind)

# All plotting done here now ----------------------------------------------

# Effect plot for wind
p1 <- effect_plot(waal_ing_mod_bin, wind_sp, interval = T) +
  labs(y = "P(ingestion)", x = "Wind speed (m/s)", title = "C") +
  geom_rug(data = act_mod_data, aes(x = wind_sp, y = 0), sides = "b") +
  scale_y_continuous(limits = c(0, 0.6)) + 
  scale_x_continuous(limits = c(0, 23))

# Effect plot for rain
p2 <- effect_plot(waal_ing_mod_bin, rain, interval = T) +
  labs(y = "P(ingestion)", x = "Rain (mm)", title = "D") +
  geom_rug(data = act_mod_data, aes(x = rain, y = 0), sides = "b") +
  scale_y_continuous(limits = c(0, 0.6)) +
  scale_x_continuous(limits = c(0, 2.25))

# Arrange model plots
waal_bi_ing_plot_c <- plot_grid(p1, p2, nrow = 1)

# Test
waal_bi_ing_plot_c

# Model for time spent on water in given conditions -----------------------

# Test the dwell time
waal_dwell_mod <- gam(data = act_mod_data,
                        formula = seconds ~
                        s(wind_sp, bs = "ts") +
                        daynight +
                        ing_bool +
                        s(Longitude, Latitude, bs = "tp") +
                        s(id, bs = "re"),
                      gamma = 2,
                      select = T,
                        family = nb(link = "log"),
                        na.action = "na.fail")

# Check AR coefficient
acf(residuals(waal_dwell_mod))[1]

# Rerun model with AR(1) structure
waal_dwell_mod <- bam(data = act_mod_data,
                      formula = seconds ~
                        s(wind_sp, bs = "ts") +
                        s(rain, bs = "ts") +
                        daynight +
                        ing_bool +
                        s(Longitude, Latitude, bs = "tp")+
                        s(id, bs = "re"),
                      gamma = 2,
                      select = T,
                      family = nb(),
                      method = "fREML",
                      discrete = T,
                      AR.start = ARStart,
                      na.action = "na.fail",
                      rho = 0.24)

# Summarise
summary(waal_dwell_mod)
anova(waal_dwell_mod)

# Test model residuals for begative binomial model
testResiduals(simulateResiduals(waal_dwell_mod))

# Test concurvity
concurvity(waal_dwell_mod)

# Plot out dwell time as a response to wind
waal_dwell_plot <- 
effect_plot(waal_dwell_mod, wind_sp, interval = T) +
  geom_rug(data = act_mod_data, aes(x = wind_sp, y = 0), sides = "b") +
  scale_x_continuous(limits = c(0,23)) +
  labs(title = "B", x = "Wind speed (m/s)", y = "Time on the water per landing (s)")

# Quick test for effect of rain
effect_plot(waal_dwell_mod, rain, interval = T)

