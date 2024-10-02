
# Load in data and prep script
source("scripts/1_prep.R")
load("data/cleaned/waal_gps_bi.RData")
load("data/cleaned/waal_act_bi.RData")
load("data/cleaned/waal_ing_bi.RData")
load("data/cleaned/waal_int_bi.RData")

# Create dummy data frame with wind speed ranges
wind_df <- data.frame(wind = (0:11) * 2,
                      n = NA,
                      landings = NA,
                      ing_mass = NA,
                      ing_mass_sd = NA)

# Populate this df with calues from interpolated data frame
for(i in 1:12){
  waal_subset <- waal_int_bi %>% filter(wind_sp > ((i * 2) - 2),
                                            wind_sp <= (i * 2),
                                        landings > 0 | fully_wet,
                                        !is.na(ing_mass))
  
  wind_df$n[i] <- nrow(waal_subset)
  
  wind_df$landings[i] <- sum(waal_subset$landings)
  
  wind_df$ing_mass[i] <- mean(waal_subset$ing_mass)
  
  wind_df$ing_mass_sd[i] <- sd(waal_subset$ing_mass)
}

# Get CIs for ingested masses per wind speed
wind_df$lci <- wind_df$ing_mass - wind_df$ing_mass_sd
wind_df$lci <- ifelse(wind_df$lci < 0, wind_df$lci <- 0, wind_df$lci)
wind_df$uci <- wind_df$ing_mass + wind_df$ing_mass_sd

# Plot out trend of ingested mass per unit time
waal_ing_time_plot <-
  plot_grid(ggplot(wind_df, aes(x = wind)) +
  theme_nice() +
    theme(legend.position = "none") +
  geom_point(data = waal_int_bi %>%
               filter(landings > 0 | fully_wet) %>%
               mutate(ing_mass_ad = ifelse(ing_mass == 0, 0.5, ing_mass)),
             aes(x = wind_sp, y = ing_mass_ad),
             colour = "#707070", alpha = 0.5, size = 0.8) +
  geom_line(aes(y = ing_mass), linewidth = 1.2, linetype = 2) +
  geom_line(aes(y = uci), linewidth = 0.8, linetype = 3) +
  geom_point(aes(y = ing_mass, size = landings, colour = n/3)) +
  scale_colour_viridis_c(end = 0.8, limits = c(0, 190)) +
  scale_size_continuous(limits = c(0, 650)) +
  scale_y_continuous(trans = "log", limits = c(0.5, 2000),
                     breaks = c(0.5, 1, 10, 100, 1000),
                     labels = c(0, 1, 10, 100, 1000)) +
    scale_x_continuous(limits = c(0, 23)) +
    geom_hline(aes(yintercept = 0.7), linetype = 5, linewidth = 1,
               alpha = 0.6, colour = "darkred") +
    labs(x = "Wind speed (m/s)", y = "Ingested mass (g/location)", title = "E")) + 
  panel_border(size = 0.5, colour = "black")
