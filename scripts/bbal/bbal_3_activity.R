
# Read in activity data and combine ---------------------------------------

# List of activity files to read in
acts2read <- c("4363_02_310308  0.act", "4372_04_130308  0.act",
               "4360_03_310308  0.act", "4368_04_310308  0.act",
               "4372_05_170308  0.act", "4364_04_310308  0.act",
               "4365_04_310308  0.act", "4367_02_310308  0.act",
               "4361_01_240208  0.act", "4372_03_240208  0.act",
               "4370_02_240208  0.act", "4374_03_310308  0.act",
               "4373_03_240208  0.act", "4361_02_310308  0.act",
               "4370_03_020308  0.act", "4364_03_130308  0.act",
               "4371_03_130308  0.act", "4369_03_150308  0.act",
               "4366_04_170308  0.act", "4370_04_310308  0.act",
               "4371_04_310308  0.act", "4362_03_310308  0.act",
               "4365_03_170308  0.act", "4369_04_310308  0.act",
               "4373_05_310308  0.act", "4366_05_310308  0.act",
               "4372_06_310308  0.act", "4360_01_220108  0.act",
               "4373_01_220108  0.act", "4364_01_220108  0.act",
               "4365_02_130208  0.act", "4365_01_220108  0.act",
               "4366_01_180108  0.act", "4362_02_130208  0.act",
               "4369_02_130208  0.act", "4362_01_190108  0.act",
               "4373_02_130208  0.act", "4369_01_180108  0.act",
               "4360_02_130208  0.act", "4372_01_220108  0.act",
               "4371_01_220108  0.act", "4368_01_180108  0.act",
               "4368_02_220108  0.act", "4363_01_190108  0.act",
               "4372_02_130208  0.act", "4373_02_130208  0.act",
               "4367_01_180108  0.act", "4374_01_180108  0.act",
               "4366_02_220108  0.act")

# Corresponding IDs for these activity files
actIDs <- c("#04", "#10", "#18", "#19", "#21", "#22", "#27", "#01", "#02",
            "#05", "#06", "#07", "#08", "#09", "#11", "#12", "#14", "#15",
            "#16", "#17", "#20", "#23", "#24", "#25", "#26", "#28", "#29",
            "BF11", "BF83", "NA20", "O043", "O093", "O193", "O294", "O431",
            "O473", "O545", "O585", "O598", "O607", "O656", "O773", "U073",
            "W718", "W731", "W763", "W779", "YB09", "YB36")

# Create an empty list to fill
act_ls <- list()

# Loop through the above and read each activity file
for(i in 1:length(acts2read)){#
  # Create the filepath
  filepath <- paste("data/South Georgia Data/BBAL EWakefield/act_data/",
                    acts2read[i], sep = "")
  
  # Read in as CSV and append column names, do some cleaning
  act_ls[[i]] <- 
    read.csv(filepath,
             col.names = c("status", "date_time",
                           "julian", "seconds", "act")) %>%
    # POSIXct of timestamp, rename id, get a boole for activity
    mutate(date_time = dmy_hms(date_time),
           id = actIDs[i],
           act_bool = ifelse(act == "wet", 1, 0)) %>%
    # Trim off last timestamp from some, which seems to be non-sensible
    filter(date_time < ymd("2008-03-27"))
}

# Add roughly 34 minutes to the act file for #18, which was clearly offset
act_ls[[3]]$date_time <- act_ls[[3]]$date_time + 2076



# Bind these activity files into one dataframe
bbal_act <- bind_rows(act_ls)

# Create a keep variable to filter out subsequent points with the same activity
bbal_act$keep <- T

# Populate this "keep" variable
for(i in 2:nrow(bbal_act)){
  if((bbal_act$act[i] == bbal_act$act[i - 1]) &
     (bbal_act$id[i] == bbal_act$id[i - 1])){
    bbal_act$seconds[i - 1] <- sum(bbal_act$seconds[(i - 1):i])
    bbal_act$keep[i] <- F
  }
}

# Subset 
bbal_act <- bbal_act[bbal_act$keep, ]

# Create an end time for each activity
bbal_act$end_time <- bbal_act$date_time + bbal_act$seconds

# Get rid of baggage
rm(acts2read, actIDs, act_ls, filepath)

# Plot activity and ingestion time series together
bbal_act %>% filter(id %in% bbal_ing$id) %>%
  ggplot(aes(x = date_time, y = act_bool)) + geom_step() +
  facet_wrap(facets = ~ id, ncol = 1, scales = "free_x") +
  scale_size_continuous() +
  scale_y_continuous(breaks = c(0, 1), labels = c("dry", "wet")) +
  geom_point(data = bbal_ing,
             aes(y = 1.1, x = date_time,
                 size = mass),
             colour = "darkred", alpha = 0.6)

# Combine ingestion and activity ------------------------------------------

# This function appends activity information to ingestions
bbal_ing <- split(bbal_ing, bbal_ing$id) %>%
  lapply(., ingest_act_append, act = bbal_act) %>%
  bind_rows()

# Create an adjusted date variable to populate later
bbal_ing$date_time_ad <- bbal_ing$date_time

# Loop through ingestion df
for(i in 1:nrow(bbal_ing)){
  
  # See if there is an activity associated with 
  if(!is.na(bbal_ing$act[i])){
    
    # Look for ingestions that take place soon after takeoff
    if(bbal_ing$act[i] == "dry" &
       bbal_ing$which_nearest[i] == "prior" &
       bbal_ing$act_prior[i] < 120){
      # Adjust the date_time so that they take place when the bird is on water
      bbal_ing$date_time_ad[i] <-
        bbal_ing$date_time[i] - bbal_ing$nearest_act[i]}
    
    
    # Look for ingestions that take place soon before landing
    if(bbal_ing$act[i] == "dry" &
       bbal_ing$which_nearest[i] == "prox" &
       bbal_ing$act_prox[i] < 120){
      # Adjust the date_time so that they take place when the bird is on water
      bbal_ing$date_time_ad[i] <-
        bbal_ing$date_time[i] + bbal_ing$nearest_act[i]}
  }
}

# Save off ingestions data
save(bbal_ing, file = "data/cleaned/bbal_ing.RData")

# Append environmental data -----------------------------------------------

# Variables to transfer from locations to activity
vars <- c("Latitude", "Longitude", "swell",
          "wave", "cloud", "cloud_hi",
          "cloud_lo", "wind_sp", "rain", "zsd")

# Write NAs to these variables
bbal_act[, vars] <- NA

# Create dummy location offset and solar angle
bbal_act$loc_offset <- NA
bbal_act$sun_angle <- NA

# Find the nearest location to an activity switch and append variables
for(i in 1:nrow(bbal_act)){
  # Subset GPS data to only the target id
  gps_sub <- bbal_int %>% filter(id == bbal_act$id[i])
  # Check that there are GPS data for this
  if(nrow(gps_sub) > 0){
    # Get the closest time stamp to the ith row of act data
    index <- which.closest(gps_sub$date_time, bbal_act$date_time[i])
    # Append variables
    bbal_act[i, vars] <- gps_sub[index, vars]
    # Get the time difference between the location and the activity
    bbal_act$loc_offset[i] <- 
      abs(
        as.numeric(
          difftime(bbal_act$date_time[i],
                   gps_sub$date_time[index],
                   units = "secs")))
    # Calculate solar angle of activity switch
    bbal_act$sun_angle[i] <-
      sunAngle(t = bbal_act$date_time[i],
               longitude = bbal_act$Longitude[i],
               latitude = bbal_act$Latitude[i])$altitude
  }
}

# Remove some variables
rm(vars, i, gps_sub, index)

# Save this off
save(bbal_act, file = "data/cleaned/bbal_act.RData")

# Plotting out ingestion timings versus activity --------------------------

# Look at ingestion events with low time iun activity
bbal_ing[which(bbal_ing$time_in_act < 13 & bbal_ing$act == "wet"),
             "time_in_act"]

# Strip out ingestion events that are "dry"
bbal_ing_act_0 <- bbal_ing[which(bbal_ing$act == "dry"), ]

# View the metrics for these "dry" ingestions
bbal_ing_act_0[, c("act", "act_prior", "act_prox", "nearest_act",
                   "time_in_act", "id", "date_time", "mass")]

# Plot out one ingestion that takes place when it is completely dry
time_range <- c(ymd_hms("2008-02-17 03:00:00"), ymd_hms("2008-02-17 15:00:00"))

# Visualise the time series of immersion data around this ingestion event
bbal_act %>% filter(id == "#04") %>%
  ggplot(aes(x = date_time, y = act_bool)) + geom_step() +
  facet_wrap(facets = ~ id, ncol = 1) +
  scale_x_datetime(limits = time_range,) +
  scale_size_continuous() +
  geom_point(data = bbal_ing_act %>% filter(id == "#04"),
             aes(y = 1.1, x = date_time,
                 size = mass),
             colour = "darkred", alpha = 0.6)

# Create labels for the plot below
labels_facets <- c(prior = "A) Closest activity switch is before ingestion",
                   prox = "B) Closest activity switch is after ingestion")

# Create a plot showing time to or from the nearest activity switch
act_ing_plot_bbal <- 
  ggplot(bbal_ing_act %>% filter(!is.na(which_nearest)),
         aes(x = nearest_act,
             y = mass,
             colour = as.factor(act))) +
  geom_point() +
  scale_x_continuous(trans = "sqrt") +
  geom_vline(aes(xintercept = 40), colour = "darkblue") +
  scale_colour_viridis_d(begin = 0.2, end = 0.8, labels = c("Dry", "Wet")) +
  labs(y = "Mass ingested (g)",
       x = "Time to nearest activity switch (s)",
       colour = "Activity\nat ingestion") +
  facet_wrap(facets = ~which_nearest, nrow = 2,
             labeller = labeller(which_nearest = labels_facets))

# Save this off
ggsave(act_ing_plot_bbal, filename = "plots/act_ing_plot_bbal.png",
       width = 8, height = 8, dpi = 500)

# Take a look at the distribution of time spent on the water
bbal_ing_act_0 %>% ggplot(aes(x = act_prior)) +
  geom_histogram() +
  scale_x_continuous(trans = "sqrt")

