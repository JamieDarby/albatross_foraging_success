
# Load in tracking data
load(file = "data/cleaned/bbal_int.RData")

# Load in csv and rename/clean some variables
bbal_ing <- read.csv(file = "data/South Georgia Data/BBAL EWakefield/PDER_events.csv") %>%
  mutate(id = paste("#", ifelse(BIRD_ID == 4, "04", BIRD_ID), sep = ""),
         date_time = dmy_hms(paste(ONSET_DATE, ONSET_TIME)),
         mass = ifelse(is.na(MEAL_MASS), ITEM_MASS, MEAL_MASS),
         event = 1,
         event.number = EVENT)
  
# Filter to distinct ingestion events
bbal_ing <- bbal_ing %>%
  mutate(event_id = paste(id, EVENT, sep = "_")) %>%
  distinct(event_id, .keep_all = TRUE)

# Append ingestion events to locations, with a maximum time offset of 1 hour
bbal_int_ing <- 
  bbal_int %>%
  filter(id %in% bbal_ing$id) %>%
  split(., .$id) %>%
  lapply(.,
         ingest_append,
         ing = bbal_ing,
         time_lim = 3600) %>%
  bind_rows()

# Plot out some of these data
bbal_int_ing %>%
  ggplot(aes(x = date_time, y = hmm_state)) + geom_step() +
  facet_wrap(facets = ~ id, ncol = 1, scales = "free_x") +
  geom_point(aes(y = 1.1, x = date_time,
                 size = mass),
             colour = "darkred", alpha = 0.6)

# Prepare HMM data
hmm_data <- bbal_int_ing %>%
  # Rename trip_id to ID for the HMM function
  mutate(ID = section) %>%
  select(-step, -angle) %>%
  
  # MomentuHMM's data prep funtion
  prepData(., type = "LL", coordNames = c("Longitude", "Latitude")) %>%
  
  # Sort out the 0 steos and the NAs
  mutate(step = ifelse(step == 0, 0.1, step),
         step = ifelse(is.na(step), 0.1, step),
         angle = ifelse(is.na(angle), 0, angle),
         event = ifelse(is.na(event), F, T),
         mass = sqrt(ifelse(is.na(mass), 0, mass)))

# Run HMM on the prepared data
system.time(
  bbal_hmm_test <-
    fitHMM(
      data = hmm_data,
      nbStates = 3,
      dist = list(step = "gamma",
                  angle = "vm"),
      Par0 = list(step = c(1, 8, 20,
                           1, 5, 4),
                  angle = c(5, 1, 5)),
      formula = ~1))

# Run HMM on the prepared data
system.time(
  bbal_hmm_ing <-
    fitHMM(
      data = hmm_data,
      nbStates = 3,
      dist = list(step = "gamma",
                  angle = "vm"),
      Par0 = list(step = c(1, 8, 20,
                           1, 5, 4),
                  angle = c(5, 1, 5)),
      formula = ~event))

# Run HMM on the prepared data
system.time(
  bbal_hmm_mass <-
    fitHMM(
      data = hmm_data,
      nbStates = 3,
      dist = list(step = "gamma",
                  angle = "vm"),
      Par0 = list(step = c(1, 8, 20,
                           1, 5, 4),
                  angle = c(5, 1, 5)),
      formula = ~mass))

# Compare HMMs
AIC(bbal_hmm_test, bbal_hmm_ing, bbal_hmm_mass)

plot(bbal_hmm_ing)
plot(bbal_hmm_mass)

bbal_int_ing$hmm_state_ing <- viterbi(bbal_hmm_ing)

save(bbal_int_ing, file = "data/cleaned/bbal_int_ing.RData")
save(bbal_hmm_ing, file = "data/cleaned/bbal_hmm_ing.RData")

# Append environmental data -----------------------------------------------

# Environmental variables to append to ingestion data
vars <- c("Latitude", "Longitude", "swell",
          "wave", "cloud", "cloud_hi",
          "cloud_lo", "wind_sp", "rain", "zsd")

# Write NAs to these variables in the ingestion dataframe
bbal_ing[, vars] <- NA

# Write placeholder location offset and solar angle variables
bbal_ing$loc_offset <- NA
bbal_ing$sun_angle <- NA

# Loop around the ingestion dataframe and append environment from locations
for(i in 1:nrow(bbal_ing)){
  # Sunset gps data to only the id in question
  gps_sub <- bbal_int %>% filter(id == bbal_ing$id[i])
  # Check that this subset actually has data
  if(nrow(gps_sub) > 0){
    # Find the closest timestamp in the subset to the ingestion
    index <- which.closest(gps_sub$date_time, bbal_ing$date_time[i])
    # Write across the variables from this
    bbal_ing[i, vars] <- gps_sub[index, vars]
    # Get time difference to the nearest location
    bbal_ing$loc_offset[i] <- 
      abs(
        as.numeric(
          difftime(bbal_ing$date_time[i],
                   gps_sub$date_time[index],
                   units = "secs")))
    # Get the solar angle at the time of ingestion
    bbal_ing$sun_angle[i] <-
      sunAngle(t = bbal_ing$date_time[i],
               longitude = bbal_ing$Longitude[i],
               latitude = bbal_ing$Latitude[i])$altitude
  }
}
