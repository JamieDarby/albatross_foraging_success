
# Load in raw GPS data and process ----------------------------------------

bbal_gps <-
  read.csv("data/BBAL_2008/BBA_low_res_corrected.csv") %>%
  rename(Latitude = LAT,
         Longitude = LON,
         id = DARVIC) %>%
  mutate(date_time = dmy_hms(TIME)) 

# Order data by id and time
bbal_gps <- with(bbal_gps, bbal_gps[order(id, date_time),])

# Write out a CSV to append data
write.csv(bbal_gps, file = "data/cleaned/bbal_gps.csv")

# Read in environmental data
bbal_env <- read.csv("data/cleaned/bbal_env.csv") %>%
  mutate(date_time = dmy_hm(date_time))

# Append environmental data
bbal_env <- split(bbal_env, bbal_env$id) %>%
  lapply(., env_append) %>%
  bind_rows()

# Split up the df
bbal_int <-
  split(bbal_gps, bbal_gps$id) %>%
  
  # Cut trip sections after more than 10 minute gaps
  lapply(., trip_cleaner, t = 240) %>%
  
  # Rearrange list so that it's split by section
  bind_rows() %>%
  split(., .$section) %>%
  
  # Remove trips of 100 or fewer points
  .[sapply(
    ., function(x) dim(x)[1]) > 20] %>%
  
  # Interpolate tracks to 1 minute intervals along a linear trajectory
  lapply(., FUN = LinStepR,
                t = 1800,
                extras = c("id", "section")) %>%
  
  # Remove trips of 100 or fewer points
  .[sapply(
    ., function(x) dim(x)[1]) > 20] %>%
  
  # Convert back to a dataframe
  bind_rows()

# Plot out trips
map <- get_stamenmap(bbox = c(left = -64, bottom = -68,
                              right = -2, top = -41),
                     zoom = 4, maptype = "toner")

bbal_map <- 
  ggmap(map) +
  theme(panel.background = element_blank(),
        legend.position = "none") +
  scalebar(location = "bottomleft",
           y.min = -64, y.max = -58,
           x.min = -50, x.max = -30,
           dist_unit = "km", dist = 500,
           st.size = 4, height = 0.05,
           st.bottom = T, transform = T,
           st.dist = .1,
           box.color = "white",
           box.fill = c("grey", "black"),
           st.color = "white") +
  labs(x = "Longitude", y = "Latitude") +
  geom_path(data = bbal_int,
            aes(x = Longitude,
                y = Latitude,
                colour = section),
            alpha = 1)

# HMM ---------------------------------------------------------------------

# Extract these variables
hmm_data <- bbal_int %>%
  # Rename trip_id to ID for the HMM function
  mutate(ID = section) %>%
  
  # MomentuHMM's data prep funtion
  prepData(., type = "LL", coordNames = c("Longitude", "Latitude")) %>%
  
  # Sort out the 0 steos and the NAs
  mutate(step = ifelse(step == 0, 0.1, step),
         step = ifelse(is.na(step), 0.1, step),
         angle = ifelse(is.na(angle), 0, angle))

# Run HMM on the prepared data
system.time(
  bbal_hmm <-
    fitHMM(
      data = hmm_data,
      nbStates = 3,
      dist = list(step = "gamma",
                  angle = "vm"),
      Par0 = list(step = c(1, 8, 20,
                           1, 5, 4),
                  angle = c(5, 1, 5)),
      formula = ~1))

# PLot HMM ouputs
plot(bbal_hmm)

# Save off the HMM
save(bbal_hmm, file = "data/cleaned/bbal_hmm.RData")

# Extract states to the overall dataframe
bbal_int$hmm_state <- viterbi(bbal_hmm)

# Save the processed 
bbal_int[, c("step", "angle")] <- hmm_data[, c("step", "angle")]

# Give separate id name for MoveBank
bbal_int$id_int <- paste(bbal_int$id, "int", sep = "_")

# Save off CSV
write.csv(bbal_int, file = "data/cleaned/bbal_int.csv")

# Read in MoveBank data
bbal_int_env <- read.csv("data/cleaned/bbal_env_int.csv") %>%
  mutate(date_time = dmy_hms(date_time))

# Check alignment
all.equal(bbal_int_env$Latitude, bbal_int$Latitude)

# Variables to carry over from env data
vars <- c("swell", "wave", "cloud", "cloud_hi",
          "cloud_lo", "wind_u", "wind_v", "rain")

# Carry over from env data
bbal_int[, vars] <- bbal_int_env[, vars]

# Get rid of environmental data source
rm(bbal_int_env)

# Calculate wind speed and sun, moon, and flight angles
bbal_int <- split(bbal_int, bbal_int$id) %>%
  lapply(., env_append) %>%
  bind_rows()

# Get dates for which data are required
date_series <- sort(unique(as.Date(bbal_int$date_time)))

# Loop through API request
for(i in 1:length(date_series)){
  copernicus_download_motu(
    username = "jdarby",
    password = "Reagrove12",
    destination = paste("data/copernicus/bbal_zsd_", date_series[i], ".nc", sep = ""),
    product = "OCEANCOLOUR_GLO_BGC_L4_MY_009_104",
    layer = "cmems_obs-oc_glo_bgc-transp_my_l4-gapfree-multi-4km_P1D",
    sub_variables = "ZSD",
    output = "netcdf",
    region = c(-60, -66, -4, -40),
    timerange = c(paste(date_series[i], "00:00:00", sep = " "),
                  paste(date_series[i], "23:59:59", sep = " ")),
    overwrite = T
  )}

# Create a ZSD variable
bbal_int$zsd <- NA

# Loop through unique dates in the dataset and append ZSD values to locations
for(i in 1:length(date_series)){
  # Read in the correct raster
  zsd_rstr <- raster(paste("data/copernicus/bbal_zsd_", date_series[i], ".nc", sep = ""))
  # Index for track points in the target date
  index <- which(as.Date(bbal_int$date_time) == date_series[i])
  # Convert to spatial points
  xysp <- SpatialPoints(bbal_int[index, c("Longitude", "Latitude")],
                        proj4string = CRS("+proj=longlat +datum=WGS84"))
  # Extract zsd data to points
  bbal_int$zsd[index] <-
    raster::extract(x = zsd_rstr,
                    y = xysp,
                    method = "bilinear")
  # Print progress
  print(i)
}

# Get distance to colony
bbal_int$col_dist <-
  raster::pointDistance(p1 = bbal_int[, c("Longitude", "Latitude")],
                        p2 = c(-38.069227, -54.015432), lonlat = T) / 1000

# Save off file
save(bbal_int, file = "data/cleaned/bbal_int.RData")
