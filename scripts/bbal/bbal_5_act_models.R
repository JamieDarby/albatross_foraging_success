# Load in required files
load(file = "data/cleaned/bbal_act.RData")
load(file = "data/cleaned/bbal_ing.RData")
load(file = "data/cleaned/bbal_int.RData")
source(file = "scripts/1_prep.R")

# Big long awful function to append all the data --------------------------

# Split df into list for appending act and ing data
bbal_int <- split(bbal_int, bbal_int$id) %>%
  # Loop around and append number of associated landings and ingestions
  lapply(., function(x){
    # Subset ing and act data to only the relevant id
    ing <- bbal_ing[which(bbal_ing$id == x$id[1]), ]
    act <- bbal_act[which(bbal_act$id == x$id[1]), ]
    
    # Create variables to populate
    x[, c("ingestions", "ing_mass", "landings", "landings_long", "fully_wet")] <- NA
    
    # Check that ing actually has values
    if(nrow(ing) > 0){
      # Loop around x to append ing data
      for(i in 1:nrow(x)){
        # Create an interval 10 minutes before and after a location
        time_seq <- c(x$date_time[i] - 899, x$date_time[i] + 900)
        
        # Create a subset of ing data
        ing_sub <- ing[which(ing$date_time >= time_seq[1] &
                               ing$date_time <= time_seq[2]), ]
        
        # Count ingestions in subdata
        x$ingestions[i] <- nrow(ing_sub)
        
        # Calculate total ingested mass if ingestions > 0
        if(x$ingestions[i] > 0){
          x$ing_mass[i] <- sum(ing_sub$mass, na.rm = T)
        }else{x$ing_mass[i] <- 0}
      }
    }
    
    # Check that act actually has data
    if(nrow(act) > 0){
      # Loop around x to append act data
      for(i in 1:nrow(x)){
        # Create an interval 10 minutes before and after a location
        time_seq <- c(x$date_time[i] - 899, x$date_time[i] + 900)
        
        # Create a subset of act data
        act_sub <- act[which(act$date_time >= time_seq[1] &
                               act$date_time <= time_seq[2]), ]
        
        # Count the number of landings
        x$landings[i] <- nrow(act_sub[which(act_sub$act == "wet"), ])
        
        # Count the number of landings
        x$landings_long[i] <- nrow(act_sub[which(act_sub$act == "wet" &
                                                   act_sub$seconds >= 15), ])
        
        # Check if any landings/takeoffs occured in this time
        if(nrow(act_sub) > 0){
          x$fully_wet[i] <- F
        }else{
          # Find whether the last activity switch was landing or takeoff
          act_prior <- act[which(act$date_time < x$date_time[i]), ]
          
          # Make sure there are some act data in this
          if(nrow(act_prior > 0)){
            # Subset to nearest act_prior
            act_prior <- act_prior[which.max(act_prior$date_time), ]
            # If last activity switch was landing, fix is fully wet
            x$fully_wet[i] <- ifelse(act_prior$act == "wet", T, F)
          }else{x$fully_wet[i] <- F}
        }
      }
    }
    x
  }) %>%
  # Bind into dataframe
  bind_rows()

# Subset model data, filter the id with no landings
mod_data <- bbal_int %>% filter(id != "W763") %>%
  # Get a factor of id and HMM state
  mutate(id = as.factor(id),
         hmm_state = as.factor(hmm_state),
         land_bool = ifelse(landings > 0, 1, 0),
         land_long_bool = ifelse(landings_long > 0, 1, 0),
         land_trans = sqrt(landings),
         ing_bool = ifelse(ingestions > 0, 1, 0),
         wet_bool = ifelse(land_bool | fully_wet, 1, 0),
         rain = rain * 1000,
         daynight = as.factor(ifelse(sun_angle > -6, "day", "night"))) %>%
  filter(!fully_wet, col_dist > 5)

# Create ARStart variable to group data into individual tracks for AR1 structure
mod_data$ARStart <- F
for(i in 2:nrow(mod_data)){
  if(mod_data$id[i] != mod_data$id[i = 1]){
    mod_data$ARStart[i] <- T
  }
}

# GAMM of landings against environmental covariates
bbal_act_mod <- gam(data = mod_data,
                    formula = landings_long ~
                      s(wind_sp, bs = "ts") +
                      s(zsd, bs = "ts", by = daynight) +
                      daynight +
                      s(rain, bs = "ts") +
                      s(Longitude, Latitude, bs = "tp") +
                      s(id, bs = "re"),
                    gamma = 2,
                    select = T,
                    family = nb(),
                    na.action = "na.fail")

# Check for autocorrelation
acf(residuals(bbal_act_mod))[1]

# Rerun as BAM with AR(1)
bbal_act_mod <- bam(data = mod_data,
                    formula = landings_long ~
                      s(wind_sp, bs = "ts") +
                      s(zsd, bs = "ts", by = daynight) +
                      daynight +
                      s(rain, bs = "ts") +
                      s(Longitude, Latitude, bs = "tp") +
                      s(id, bs = "re"),
                    gamma = 2,
                    select = T,
                    family = nb(),
                    method = "fREML",
                    discrete = T,
                    AR.start = ARStart,
                    na.action = "na.fail",
                    rho = 0.33)

# Summarise
summary(bbal_act_mod)
anova(bbal_act_mod)

# Check AR coefficient
acf(residuals(bbal_act_mod))[1]

# Test model structure
testResiduals(simulateResiduals(bbal_act_mod))
# Use bootstrapping to account for binomial family when testing for outliers
testOutliers(simulateResiduals(bbal_act_mod), type = "bootstrap")

# Test model concurvity
concurvity(bbal_act_mod)

# Create plots using a dummy df and predicitons ---------------------------

# Create dummy df
dummy_df <- data.frame(wind_sp = rep(mean(mod_data$wind_sp), 100),
                       rain = mean(mod_data$rain),
                       zsd =  mean(mod_data$zsd),
                       Latitude = mean(mod_data$Latitude),
                       Longitude = mean(mod_data$Longitude),
                       daynight = "day",
                       id = "#04")

# Split into new df with range of ZSD values
dummy_df_zsd <- dummy_df %>% mutate(zsd = seq(0, 33, length.out = 100))

# Get model link function
ilink <- family(bbal_act_mod)$linkinv

# Get model predicitons
dummy_df_zsd[, c("pred", "se")] <-
  predict.gam(bbal_act_mod, newdata = dummy_df_zsd,
              se.fit = T, type = "link")

# Transform predictions using link function
dummy_df_zsd <- transform(dummy_df_zsd,
                           lwr_ci = ilink(pred - (2 * se)),
                           upr_ci = ilink(pred + (2 * se)),
                           fitted = ilink(pred))

# Plot out predicitons
p1 <- ggplot(dummy_df_zsd, aes(x = zsd)) +
  geom_line(aes(y = fitted*(2/3)), linewidth = 1) +
  geom_ribbon(aes(ymin = lwr_ci*(2/3), ymax = upr_ci*(2/3)), alpha = 0.2) +
  scale_y_continuous(limits = c(0, 1.6), breaks = c(0, 1, 2, 3)) +
  labs(y = "Landings (per 20 minutes)", x = "ZSD (m, daytime)", title = "B") +
  geom_rug(data = mod_data, aes(x = zsd, y = 0), sides = "b") +
  theme_nice()

# Spliut into new df with a range of rain values
dummy_df_rain <- dummy_df %>% mutate(rain = seq(0, 2.8, length.out = 100))

# Get model predicitons
dummy_df_rain[, c("pred", "se")] <-
  predict.gam(bbal_act_mod, newdata = dummy_df_rain,
              se.fit = T, type = "link")

# Transform model predicitons
dummy_df_rain <- transform(dummy_df_rain,
                           lwr_ci = ilink(pred - (2 * se)),
                           upr_ci = ilink(pred + (2 * se)),
                           fitted = ilink(pred))

# Plot model predicitons
p2 <- ggplot(dummy_df_rain, aes(x = rain, y = fitted*(2/3))) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr_ci*(2/3), ymax = upr_ci*(2/3)), alpha = 0.2) +
  scale_y_continuous(limits = c(0, 1.6), breaks = c(0, 1, 2, 3)) +
  scale_x_continuous(limits = c(0, 3)) +
  labs(y = "Landings (per 20 minutes)", x = "Rain (mm/hr)", title = "A") +
  geom_rug(data = mod_data, aes(x = rain, y = 0), sides = "b") +
  theme_nice()

# Arrange model plots
bbal_act_plot_c <- plot_grid(p2, p1, nrow = 2)

# Test
bbal_act_plot_c

# Ingested mass per time models -------------------------------------------

# Subset model data
mod_data <- bbal_int %>%
  # Get a factor of id and HMM state
  mutate(id = as.factor(id),
         hmm_state = as.factor(hmm_state),
         land_bool = ifelse(landings > 0, 1, 0),
         land_long_bool = ifelse(landings_long > 0, 1, 0),
         wet_bool = ifelse(land_bool | fully_wet, 1, 0),
         rain = rain * 1000,
         daynight = as.factor(ifelse(sun_angle > -6, "day", "night"))) %>%
  filter(fully_wet | landings > 0, col_dist > 5,
         !is.na(zsd), !is.na(ing_mass)) %>%
  mutate(ing_bool = ifelse(ingestions > 0, 1, 0))

# Create ARStart variable to group data into individual tracks for AR1 structure
mod_data$ARStart <- F
for(i in 2:nrow(mod_data)){
  if(mod_data$id[i] != mod_data$id[i = 1]){
    mod_data$ARStart[i] <- T
  }
}

# Run model
mod <-
  gam(data = mod_data,
      formula = ing_mass ~
        s(wind_sp, bs = "ts") +
        s(rain, bs = "ts") +
        s(zsd, bs = "ts") +
        daynight +
        s(Longitude, Latitude, bs = "tp") +
        s(id, bs = "re"),
      family = tw(),
      na.action = "na.fail")

# Check ACF for autocorrelation
acf(residuals(mod))[1]

# Summarise
summary(mod)
anova(mod)

# Test residuals
testResiduals(simulateResiduals(mod))

# Test concurvity
concurvity(mod)

# Plot out model outputs for ingestions per time --------------------------

effect_plot(mod, wind_sp, interval = T, rug = T, rug.sides = "b") +
  scale_y_continuous(limits = c(0, 130))

effect_plot(mod, rain, interval = T, rug = T, rug.sides = "b") +
  scale_y_continuous(limits = c(0, 130))

