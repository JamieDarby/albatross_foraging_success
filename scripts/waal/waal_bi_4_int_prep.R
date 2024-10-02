
# Explore data and see how best to standardise ----------------------------

# Create an interval variable for exploring data
waal_gps_bi$interval <- NA
# Get time between track points
for(i in 2:nrow(waal_gps_bi)){
  if(waal_gps_bi$id[i] == waal_gps_bi$id[i - 1]){
    waal_gps_bi$interval[i] <- 
      abs(
        as.numeric(
          difftime(waal_gps_bi$date_time[i],
                   waal_gps_bi$date_time[i - 1],
                   units = "secs")))
  }
}

# Look at the individuals with fine resolution GPS data
waal_gps_bi %>% filter(interval < 600) %>% select(id) %>% table()

# Summarise the track intervals for coarser data
waal_gps_bi %>% filter(!id %in% c("5116480", "5187159")) %>%
  select(interval) %>% summary()

# Interpolate data --------------------------------------------------------

# Split up the df
waal_int_bi <- waal_gps_bi %>%
  
  split(., .$id) %>%
  
  # Cut trip sections after more than 60 minute gaps
  lapply(., trip_cleaner, t = 60) %>%
  
  # Rearrange list so that it's split by section
  do.call(rbind, .) %>%
  split(., .$section) %>%
  
  # Remove trips of 36 or fewer points (up to 12 hours for coarser data)
  .[sapply(
    ., function(x) dim(x)[1]) > 36] %>%
  
  # Interpolate tracks to 20 minute intervals along a linear trajectory
  lapply(., FUN = LinStepR,
         t = 1200,
         extras = c("id", "section")) %>%
  
  # Remove trips of 36 or fewer points (12 hours)
  .[sapply(
    ., function(x) dim(x)[1]) > 36] %>%
  
  # Convert back to a dataframe
  bind_rows()

# Check that all individuals are represented by at least one section
unique(waal_int_bi$section)
unique(waal_gps_bi$id)

# Rename id to differentiate in MoveBank
waal_int_bi$id <- paste("int", waal_int_bi$id, sep = "_")

# Save off to append environmental variables
write.csv(waal_int_bi, file = "data/cleaned/waal_int_bi.csv")

# Load in the new dataset with appended environmental data
waal_int_bi <- read.csv(file = "data/cleaned/waal_int_bi_env.csv") %>%
  mutate(date_time = dmy_hms(date_time))

# Rename id to remove the "int" from the beginning
waal_int_bi$id <- str_split(waal_int_bi$id,
                            pattern = "_", simplify = T)[, 2]

# Read in list of sexes
sexes <- read.csv("data/WAAL_2009/sexes_weights_ages.csv")

# Big long function thing to append all the data --------------------------

# Split df into list for appending act and ing data
waal_int_bi <- split(waal_int_bi, waal_int_bi$id) %>%
  # Loop around and append number of associated landings and ingestions
  lapply(., function(x){
    # Subset ing and act data to only the relevant id
    ing <- waal_ing_bi[which(waal_ing_bi$id == x$id[1]), ]
    act <- waal_act_bi[which(waal_act_bi$id == x$id[1]), ]
    
    # Create variables to populate
    x[, c("ingestions", "ing_mass", "landings", "fully_wet", "activity")] <- NA
    
    # Check that ing actually has values
    if(nrow(ing) > 0){
      # Loop around x to append ing data
      for(i in 1:nrow(x)){
        # Create an interval 10 minutes before and after a location
        time_seq <- c(x$date_time[i] - 599, x$date_time[i] + 600)
        
        # Create a subset of ing data
        ing_sub <- ing[which(ing$date_time >= time_seq[1] &
                               ing$date_time <= time_seq[2]), ]
        
        # Count ingestions in subdata
        x$ingestions[i] <- nrow(ing_sub)
        
        # Calculate total ingested mass if ingestions > 0
        if(x$ingestions[i] > 0){
          x$ing_mass[i] <- sum(ing_sub$integral, na.rm = T)
        }else{x$ing_mass[i] <- 0}
      }
    }
    
    # Check that act actually has data
    if(nrow(act) > 0){
      # Loop around x to append act data
      for(i in 1:nrow(x)){
        # Create an interval 10 minutes before and after a location
        time_seq <- c(x$date_time[i] - 599, x$date_time[i] + 600)
        
        # Create a subset of act data
        act_sub <- act[which(act$date_time >= time_seq[1] &
                               act$date_time <= time_seq[2]), ]
        
        x$activity[i] <- nrow(act_sub)
        
        # Count the number of landings
        x$landings[i] <- nrow(act_sub[which(act_sub$state == "wet"), ])
        x$landings_long[i] <-
          nrow(act_sub[which(act_sub$state == "wet" &
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
            x$fully_wet[i] <- ifelse(act_prior$state == "wet", T, F)
          }else{x$fully_wet[i] <- F}
        }
      }
    }
    x
  }) %>%
  # Sort out environmental data
  lapply(., env_append) %>%
  # Append sex and weight data to activity list
  lapply(., function(x){
    info <- sexes[which(sexes$id == x$id[1]), ]
    x[, c("weight", "sex", "age")] <- info[1, c("weight", "sex", "age")]
    x
  }) %>%
  # Bind into dataframe
  bind_rows()

# HMM ---------------------------------------------------------------------

# Prepare data for hmm
hmm_data <- waal_int_bi %>%
  # Rename trip_id to ID for the HMM function
  mutate(ID = section) %>%
  # filter(!is.na(ing_mass)) %>%
  
  # MomentuHMM's data prep funtion
  prepData(., type = "LL", coordNames = c("Longitude", "Latitude")) %>%
  
  # Sort out the 0 steos and the NAs
  mutate(step = ifelse(step == 0, 0.01, step),
         step = ifelse(is.na(step), 0.01, step),
         angle = ifelse(is.na(angle), 0, angle))

# Plot out step distributions
ggplot(hmm_data) +
  geom_density(aes(x = step), fill = "darkblue", alpha = 0.5)

# Run HMM on the prepared data
system.time(
  waal_bi_hmm <-
    fitHMM(
      data = hmm_data,
      nbStates = 3,
      dist = list(step = "gamma",
                  angle = "vm"),
      Par0 = list(step = c(2, 8, 16,
                           2, 8, 4),
                  angle = c(5, 1, 5)),
      stateNames = c("Rest", "ARS", "Transit"),
      formula = ~1))

# Plot out HMM outputs
# plot(waal_bi_hmm)

# Gets rid of pesky plotting prompt
# par(ask=F)

# Get step length and turning angle from HMM
waal_int_bi[, c("hmm_step", "hmm_angle")] <- hmm_data[, c("step", "angle")]

# Extract state sequence from the HMM
waal_int_bi$hmm_state <- viterbi(waal_bi_hmm)

# Get distance to colony
waal_int_bi$col_dist <-
  raster::pointDistance(p1 = waal_int_bi[, c("Longitude", "Latitude")],
                        p2 = c(-38.069227, -54.015432), lonlat = T) / 1000

# Load in bathymetric rasters and append
load("data/bathy_rstr.RData")
load("data/iso500.RData")

# Append depth
waal_int_bi$depth <-
  raster::extract(bathy_rstr,
                  y = waal_int_bi[, c("Longitude", "Latitude")],
                  method = "bilinear") / 1000

# Append distance to contour
waal_int_bi$iso500 <-
  raster::extract(iso500,
                  y = waal_int_bi[, c("Longitude", "Latitude")],
                  method = "bilinear")

# Get dates for which data are required
date_series <- sort(unique(as.Date(waal_int_bi$date_time)))

# Create a ZSD variable
waal_int_bi$zsd <- NA

# Loop through unique dates in the dataset and append ZSD values to locations
for(i in 1:length(date_series)){
  # Read in the correct raster
  zsd_rstr <- raster(paste("data/copernicus/zsd_", date_series[i], ".nc", sep = ""))
  # Index for track points in the target date
  index <- which(as.Date(waal_int_bi$date_time) == date_series[i])
  # Convert to spatial points
  xysp <- SpatialPoints(waal_int_bi[index, c("Longitude", "Latitude")],
                        proj4string = CRS("+proj=longlat +datum=WGS84"))
  # Extract zsd data to points
  waal_int_bi$zsd[index] <-
    raster::extract(x = zsd_rstr,
                    y = xysp,
                    method = "bilinear")
  # Print progress
  print(i)
}

# Save off this dataframe
save(waal_int_bi, file = "data/cleaned/waal_int_bi.RData")
