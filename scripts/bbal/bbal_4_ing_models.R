# Load in required files
load(file = "data/cleaned/bbal_act.RData")
load(file = "data/cleaned/bbal_ing.RData")
load(file = "data/cleaned/bbal_int.RData")
source(file = "scripts/1_prep.R")

# Split into a list by identity
bbal_act <- split(bbal_act, bbal_act$id) %>%
  # Append ingestion data to the activity list
  lapply(., act_ingest_append, ing = bbal_ing, mass_var = "mass") %>%
  ## Append sex and weight data to activity list
  ## lapply(., function(x){
  ##   info <- sexes[which(sexes$id == x$id[1]), ]
  ##   x[, c("weight", "sex", "age")] <- info[1, c("weight", "sex", "age")]
  ##   x
  ## }) %>%
  # Bind back into dataframe
  bind_rows()

# Subset model data
act_mod_data <- bbal_act %>%
  # Remove points with no nearby locations and one extreme outlier
  filter(loc_offset < 7200,
         !is.na(ing_bool)) %>%
  # Get a factor of id, and cube-root the dwell time
  mutate(id = as.factor(id),
         dwell_trans = seconds ^ (1/3),
         daynight = as.factor(ifelse(sun_angle > -6, "day", "night")))

# Create ARStart variable to group data into individual tracks for AR1 structure
act_mod_data$ARStart <- F
for(i in 2:nrow(act_mod_data)){
  if(act_mod_data$id[i] != act_mod_data$id[i = 1]){
    act_mod_data$ARStart[i] <- T
  }
}

# Run a model, within a wrapper function to time it
system.time(bbal_ing_mod_bin <-
              gam(data = act_mod_data,
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
                  na.action = "na.fail"))


# Summarise
summary(bbal_ing_mod_bin)
anova(bbal_ing_mod_bin)

# Check AR coefficient
acf(residuals(bbal_ing_mod_bin))[1]

# Test model structure
testResiduals(simulateResiduals(bbal_ing_mod_bin))

# Check model concurvity
concurvity(bbal_ing_mod_bin)

# Model tests -------------------------------------------------------------

# Predict model over the same dataset
pr <- as.numeric(predict(bbal_ing_mod_bin, act_mod_data, type = "response"))            

# Compare predicted values to actual values
pred <- prediction(pr, act_mod_data$ing_bool)

# Print AUC
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
confusionMatrix(as.factor(ifelse(pr > 0.18, T, F)),
                as.factor(act_mod_data$ing_bool),
                mode = "everything",
                positive = "TRUE")

# Get rid of some temporary data
rm(pr, pred, perf, x, y, ind)

# Plotting ----------------------------------------------------------------
p1 <- effect_plot(bbal_ing_mod_bin, wind_sp, interval = T) +
  labs(y = "P(ingestion)", x = "Wind speed (m/s)", title = "A") +
  geom_rug(data = act_mod_data, aes(x = wind_sp, y = 0), sides = "b") +
  scale_y_continuous(limits = c(0, 0.6)) + 
  scale_x_continuous(limits = c(0, 23))

p2 <-
  interact_plot(bbal_ing_mod_bin, zsd, modx = daynight, interval = T,
                legend.main = "", modx.labels = c("Day", "Night")) +
  labs(y = "P(ingestion)", x = "ZSD (m)", title = "B") +
  scale_y_continuous(limits = c(0, 0.6))

# Arrange model plots
bbal_ing_plot_c <- plot_grid(p1, p2, nrow = 1)

# Test
bbal_ing_plot_c

# Test the time spent on water by environment -----------------------------

# Test the dwell time
bbal_dwell_mod <- gam(data = act_mod_data,
                      formula = seconds ~
                        s(wind_sp, bs = "ts") +
                        s(rain, bs = "ts") +
                        daynight +
                        ing_bool +
                        s(Longitude, Latitude, bs = "tp") +
                        s(id, bs = "re"),
                      gamma = 2,
                      select = T,
                      family = nb(link = "log"),
                      na.action = "na.fail")

# Check AR coefficient
acf(residuals(bbal_dwell_mod))[1]

# Rerun as BAM with AR(1) coefficient
bbal_dwell_mod <- bam(data = act_mod_data,
                      formula = seconds ~
                        s(wind_sp, bs = "ts") +
                        s(rain, bs = "ts") +
                        daynight +
                        ing_bool +
                        s(Longitude, Latitude, bs = "tp") +
                        s(id, bs = "re"),
                      gamma = 2,
                      select = T,
                      family = nb(),
                      method = "fREML",
                      discrete = T,
                      AR.start = ARStart,
                      na.action = "na.fail",
                      rho = 0.3)

# Summarise
summary(bbal_dwell_mod)
anova(bbal_dwell_mod)

# Test model residuals for binomial model
testResiduals(simulateResiduals(waal_dwell_mod))

# Test concurvity
concurvity(bbal_dwell_mod)

# Plot out response to wind speed
bbal_dwell_plot <- 
  effect_plot(bbal_dwell_mod, wind_sp, interval = T) +
  geom_rug(data = act_mod_data, aes(x = wind_sp, y = 0), sides = "b") +
  scale_x_continuous(limits = c(0,23)) +
  labs(title = "A", x = "Wind speed (m/s)", y = "Time on the water per landing (s)")
