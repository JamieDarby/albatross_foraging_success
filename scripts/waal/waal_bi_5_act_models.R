
# Do some honest-to-God modelling -----------------------------------------

# Load in data# Load in data and prep script
source("scripts/1_prep.R")
load("data/cleaned/waal_gps_bi.RData")
load("data/cleaned/waal_act_bi.RData")
load("data/cleaned/waal_ing_bi.RData")
load("data/cleaned/waal_int_bi.RData")

# Subset model data
mod_data <- waal_int_bi %>%
  # Get a factor of id and HMM state
  mutate(id = as.factor(id),
         hmm_state = as.factor(hmm_state),
         land_bool = ifelse(landings > 0, 1, 0),
         land_long_bool = ifelse(landings_long > 0, 1, 0),
         wet_bool = ifelse(land_bool | fully_wet, 1, 0),
         rain = rain * 1000,
         gust_diff = gust - wind_sp,
         daynight = as.factor(ifelse(sun_angle > -6, "day", "night"))) %>%
  filter(!fully_wet, col_dist > 5, !is.na(zsd))

# Create ARStart variable to group data into individual tracks for AR1 structure
mod_data$ARStart <- F
for(i in 2:nrow(mod_data)){
  if(mod_data$id[i] != mod_data$id[i = 1]){
    mod_data$ARStart[i] <- T
  }
}

# GAMM of landings against environmental covariates
waal_bi_act_mod <- gam(data = mod_data,
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

# Check ACF for autocorrelation
acf(residuals(waal_bi_act_mod))[1]

# Rerun with AR(1) structure
waal_bi_act_mod <- bam(data = mod_data,
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
    rho = 0.35)

# Summarise again
summary(waal_bi_act_mod)
anova(waal_bi_act_mod)

# Residual diagnostics, dispersion and KS test check out
testResiduals(simulateResiduals(waal_bi_act_mod))

# Check concurvity of the model
concurvity(waal_bi_act_mod)

# Using dummy data and predictions to plot effects ------------------------

# Create a dummy data frame
dummy_df <- data.frame(wind_sp = rep(mean(mod_data$wind_sp), 100),
                       rain = mean(mod_data$rain),
                       zsd = mean(mod_data$zsd),
                       Latitude = mean(mod_data$Latitude),
                       Longitude = mean(mod_data$Longitude),
                       daynight = "day",
                       id = "5056443")

# Split into new df with range of wind values
dummy_df_wind <- dummy_df %>% mutate(wind_sp = seq(0, 22.5, length.out = 100))

# Get model link function
ilink <- family(waal_bi_act_mod)$linkinv

# Predict
dummy_df_wind[, c("pred", "se")] <-
  predict.gam(waal_bi_act_mod, newdata = dummy_df_wind,
              se.fit = T, type = "link")

# Tranform based oin link function
dummy_df_wind <- transform(dummy_df_wind,
                           lwr_ci = ilink(pred - (2 * se)),
                           upr_ci = ilink(pred + (2 * se)),
                           fitted = ilink(pred))

# Plot out effect
p1 <- ggplot(dummy_df_wind, aes(x = wind_sp, y = fitted)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci), alpha = 0.2) +
  labs(y = "Landings (per 20 minutes)", x = "Wind speed (m/s)", title = "E") +
  geom_rug(data = mod_data, aes(x = wind_sp, y = 0), sides = "b") +
  theme_nice()

# Split df with a range of rain values
dummy_df_rain <- dummy_df %>% mutate(rain = seq(0, 3, length.out = 100))

# Predict
dummy_df_rain[, c("pred", "se")] <-
  predict.gam(waal_bi_act_mod, newdata = dummy_df_rain,
              se.fit = T, type = "link")

# Tranform
dummy_df_rain <- transform(dummy_df_rain,
                           lwr_ci = ilink(pred - (2 * se)),
                           upr_ci = ilink(pred + (2 * se)),
                           fitted = ilink(pred))

# Plot effect
p2 <- ggplot(dummy_df_rain, aes(x = rain, y = fitted)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci), alpha = 0.2) +
  scale_y_continuous(limits = c(0, 1.6), breaks = c(0, 1)) +
  scale_x_continuous(limits = c(0, 3.2)) +
  labs(y = "Landings (per 20 minutes)", x = "Rain (mm/hr)", title = "C") +
  geom_rug(data = mod_data, aes(x = rain, y = 0), sides = "b") +
  theme_nice()

# Split out df with a range of zsd values
dummy_df_zsd <- dummy_df %>% mutate(zsd = seq(0, 38, length.out = 100))

# Predict
dummy_df_zsd[, c("pred", "se")] <-
  predict.gam(waal_bi_act_mod, newdata = dummy_df_zsd,
              se.fit = T, type = "link")

# Tranform
dummy_df_zsd <- transform(dummy_df_zsd,
                           lwr_ci = ilink(pred - (2 * se)),
                           upr_ci = ilink(pred + (2 * se)),
                           fitted = ilink(pred))

# Plot
p3 <- ggplot(dummy_df_zsd, aes(x = zsd, y = fitted)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lwr_ci, ymax = upr_ci), alpha = 0.2) +
  scale_y_continuous(limits = c(0, 1.6), breaks = c(0, 1)) +
  labs(y = "Landings (per 20 minutes)", x = "ZSD (m, daytime)", title = "D") +
  geom_rug(data = mod_data, aes(x = zsd, y = 0), sides = "b") +
  theme_nice()

# Arrange model plots
waal_bi_act_plot_c <- plot_grid(plot_grid(p2, p3, nrow = 2), p1, nrow = 1)

# Test
waal_bi_act_plot_c

# Models with ingested mass per time --------------------------------------

# Subset model data
mod_data <- waal_int_bi %>%
  # Get a factor of id and HMM state
  mutate(id = as.factor(id),
         hmm_state = as.factor(hmm_state),
         land_bool = ifelse(landings > 0, 1, 0),
         land_long_bool = ifelse(landings_long > 0, 1, 0),
         wet_bool = ifelse(land_bool | fully_wet, 1, 0),
         rain = rain * 1000,
         gust_diff = gust - wind_sp,
         daynight = as.factor(ifelse(sun_angle > -6, "day", "night"))) %>%
  filter(col_dist > 5,
         fully_wet | landings > 0,
         !is.na(zsd), !is.na(ing_mass)) %>%
  mutate(ing_bool = ifelse(ingestions > 0, 1, 0))

# Create ARStart variable to group data into individual tracks for AR1 structure
mod_data$ARStart <- F
for(i in 2:nrow(mod_data)){
  if(mod_data$id[i] != mod_data$id[i = 1]){
    mod_data$ARStart[i] <- T
  }
}

# Run model of ingested mass per time
mod <-
  gam(data = mod_data,
      formula = ing_mass ~
        s(wind_sp, bs = "ts") +
        s(rain, bs = "ts") +
        s(zsd, bs = "ts") +
        daynight +
        s(Longitude, Latitude, bs = "tp") +
        s(id, bs = "re"),
      select = T,
      family = tw(),
      na.action = "na.fail")

# Check ACF for autocorrelation
acf(residuals(mod))[1]

# Summarise
summary(mod)
anova(mod)

# Test residuals
testResiduals(simulateResiduals(mod))

# Check concurvity
concurvity(mod)

# Effect plot for wind
p1 <- effect_plot(mod, wind_sp, interval = T, rug = T, rug.sides = "b") +
  scale_y_continuous(limits = c(0, 130)) +
  labs(y = "Ingested mass (g)", x = "Wind speed (m/s)", title = "E") + 
  scale_x_continuous(limits = c(0, 23))
  
# Effect plot for rain
p2 <- effect_plot(mod, rain, interval = T, rug = T, rug.sides = "b") +
  scale_y_continuous(limits = c(0, 130)) +
  labs(y = "Ingested mass (g)", x = "Rain (mm)", title = "F") +
  scale_x_continuous(limits = c(0, 2.25))

# Combine these
waal_bi_ing_plot_d <- plot_grid(p1, p2, nrow = 1)

# Test
waal_bi_ing_plot_d
