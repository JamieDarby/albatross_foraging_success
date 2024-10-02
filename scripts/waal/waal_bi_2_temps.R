
# This whole section is just data visualising -----------------------------

# Can change the filename hereto view other temp logger data
stm <- read.csv(file = "data/WAAL_2009/stm_data/1_MT103_5147731.TXT",
                  header = F, sep = "") %>%
  # Tidy up variables
  mutate(date_time = dmy_hms(paste(V1, V2)),
         temp = V3)

# Plot temp data against act data, remember to match temp and act files
ggplot(stm) +
  geom_line(aes(x = date_time, y = c(0, diff(temp)),
                colour = abs(c(0, diff(temp))))) +
  scale_colour_viridis_c(option = "D", trans = "sqrt") +
  geom_step(data = waal_act_bi %>% filter(deploy == "1"),
            aes(x = date_time, y = ifelse(status == "SUSPECT", -1,
                                          ifelse(state == "dry", 0, 1)))) +
  scale_x_datetime(limits = ymd_hm(c("2009-05-08 00:00", "2009-05-12 00:00")))
                      
# Creating ingestion dataframe --------------------------------------------

# Target filenames
filename <- c("1_MT103_5147731_res", "2_MT110_R876_res", "3_MT104_5116375_res",
              "4_MT106_R291_res", "5_MT107_5117599_res", "6_MT109_5109766_res",
              "7_MT104_5116353_res", "8_MT103_5222685_res", "9_MT106_5144771_res",
              "10_MT107_BA88_res", "11_MT109_5127237_res", "13_MT106_5146273_res",
              "14_MT103_5143083_res", "18_MT109_5146279_res", "20_MT103_5147358_res",
              "21_MT105_5116432_res", "22_MT106_4002403_res", "23_MT108_RA94_res",
              "24_MT109_4003028_res", "25_MT105_5187159_res", "26_MT108_5116480_res")

# Create empty list
ing_ls <- list()

# Populate this list from ingestion CSVs
for(i in 1:length(filename)){
  ing_ls[[i]] <- read.csv(paste("data/WAAL_2009/stm_data/", 
                                filename[i], sep = ""), sep = "") %>%
  mutate(onset = dmy_hms(paste(Onset, MinTemp)),
         min_temp = Integral,
         integral = X.SmallDrops,
         small_drops = Onset.1,
         onset_temp = temp.,
         duration = Duration,
         deployment = filename[i]) %>%
  select(onset, min_temp, integral, small_drops, onset_temp, duration, deployment)
  # Split the filename in 3 to get id, logger, and deployment number
  ing_ls[[i]][, c("deploy", "logger", "id")] <- 
  str_split(ing_ls[[i]]$deployment,
            pattern = "_", simplify = T)[, 1:3]
}

# Put the list together and remove very small ingestions
waal_ing_bi <- bind_rows(ing_ls) %>%
  filter(min_temp < 37) %>%
  mutate(date_time = onset)

# Get rid of the list
rm(ing_ls)

# Convert ingestions to mass following Rory Wilson's work
slope <- 4

# PDER to mass formula
waal_ing_bi$mass <- (waal_ing_bi$integral /
  (slope * 4 * (waal_ing_bi$onset_temp - 5.5))) * 1000

# Take a look at the densities of different ingested masses
ggplot(waal_ing_bi) + geom_density(aes(x = mass))
ggplot(waal_gps %>% filter(mass < 3000)) + geom_density(aes(x = mass)) +
  geom_density(data = waal_ing_bi, aes(x = mass), colour = "darkred")

# Check how many very small ingestions occurred
sum(waal_gps$mass <20, na.rm = T)

# Rename state to act in the activity dataframe
waal_act_bi$act <- waal_act_bi$state

# This function appends activity information to ingestions
waal_ing_bi <- split(waal_ing_bi, waal_ing_bi$id) %>%
  lapply(., ingest_act_append, act = waal_act_bi) %>%
  bind_rows()

# Append environmental data -----------------------------------------------

# Environmental variables to append to ingestion data
vars <- c("Latitude", "Longitude", "swell",
          "wave", "cloud", "cloud_hi",
          "cloud_lo", "wind_sp", "rain", "zsd",
          "depth", "iso500")

# Write NAs to these variables in the ingestion dataframe
waal_ing_bi[, vars] <- NA

# Write placeholder location offset and solar angle variables
waal_ing_bi$loc_offset <- NA
waal_ing_bi$sun_angle <- NA

# Loop around the ingestion dataframe and append environment from locations
for(i in 1:nrow(waal_ing_bi)){
  # Sunset gps data to only the id in question
  gps_sub <- waal_gps_bi %>% filter(id == waal_ing_bi$id[i])
  # Check that this subset actually has data
  if(nrow(gps_sub) > 0){
    # Find the closest timestamp in the subset to the ingestion
    index <- which.closest(gps_sub$date_time, waal_ing_bi$date_time[i])
    # Write across the variables from this
    waal_ing_bi[i, vars] <- gps_sub[index, vars]
    # Get time difference to the nearest location
    waal_ing_bi$loc_offset[i] <- 
      abs(
        as.numeric(
          difftime(waal_ing_bi$date_time[i],
                   gps_sub$date_time[index],
                   units = "secs")))
    # Get the solar angle at the time of ingestion
    waal_ing_bi$sun_angle[i] <-
      sunAngle(t = waal_ing_bi$date_time[i],
               longitude = waal_ing_bi$Longitude[i],
               latitude = waal_ing_bi$Latitude[i])$altitude
  }
}

# Remove unnecessary variables
rm(i, gps_sub, index)

# Save off this dataframe
save(waal_ing_bi, file = "data/cleaned/waal_ing_bi.RData")

# Some non-related data exploration ---------------------------------------

waal_ing_bi %>%
  filter(nearest_act > 180 & act == "dry") %>%
  select(wind_sp, integral, deployment, loc_offset, sun_angle, nearest_act)

waal_ing_bi %>% filter(nearest_act > 600 & act == "dry")

waal_ing_bi %>% filter(nearest_act < 20 & act == "wet")

waal_ing_bi %>% filter(act_prior < 20 & act == "wet")


waal_ing_bi %>% filter(deploy == "9")

labels_facets <- c(prior = "A) Closest activity switch is before ingestion",
                   prox = "B) Closest activity switch is after ingestion")

# act_ing_plot_waal_bi <- 
  ggplot(waal_ing_bi %>% filter(!is.na(which_nearest)),
         aes(x = nearest_act,
             y = integral,
             colour = as.factor(act))) +
  geom_point() +
  scale_x_continuous(trans = "sqrt") +
  geom_vline(aes(xintercept = 120), colour = "darkblue") +
  scale_colour_viridis_d(begin = 0.2, end = 0.8, labels = c("Dry", "Wet")) +
  labs(y = "Mass ingested (g)",
       x = "Time to nearest activity switch (s)",
       colour = "Activity\nat ingestion") +
  facet_wrap(facets = ~which_nearest, nrow = 2,
             labeller = labeller(which_nearest = labels_facets))

waal_ing_bi %>% filter(sun_angle < -6) %>% select(integral) %>% summary()
waal_ing_bi %>% filter(sun_angle < -6) %>% select(integral) %>% nrow()
waal_ing_bi %>% filter(sun_angle > -6) %>% select(integral) %>% summary()
waal_ing_bi %>% filter(sun_angle > -6) %>% select(integral) %>% nrow()

waal_ing_bi$daynight <- ifelse(waal_ing_bi$sun_angle > -6, "day", "night")

ggplot(waal_ing_bi) +
  geom_(aes(x = id, y = integral, fill = daynight))
