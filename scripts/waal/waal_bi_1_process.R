
# Collate all raw tracking data -------------------------------------------

# Create an object containing all the deployment names
files <- c("G171_5108648_27", "G171_5108666_33", "G171_5109491_19",
           "G171_5116353_7", "G171_5146273_13", "G171_5147731_1",
           "G172_R876_2", "G173_5116375_3", "G173_5144771_9",
           "G173_5146279_18", "G173_5147358_20", "G173_5156460_28",
           "G174_5116432_21", "G174_5143083_14", "G174_5145073_29",
           "G174_5187159_25", "G175_4002403_22", "G175_5144948_15",
           "G175_5145003_35", "G175_5222562_30", "G176_5116480_26",
           "G176_5122454_16", "G176_RA94_23", "G177_R291_4",
           "G178_4003028_24", "G178_5116409_36", "G178_5117599_5",
           "G178_5127237_11", "G178_5216938_17", "G178_5056443_31",
           "G180_5109766_6", "G180_B211_32", "G180_GA97_12")

# Empty list
track_ls <- list()

# Loop through the files and write each to a list element
for(i in 1:length(files)){
  # Write the filepath from the list above
  filepath <- paste("data/WAAL_2009/loc_data/", files[i], ".csv", sep = "")
  
  # Read this in, format the date, deployment id, and choose columns to keep
  track_ls[[i]] <- read.csv(file = filepath) %>%
    mutate(date_time = dmy_hms(paste(date, time)),
           deployment = files[i]) %>%
    select(deployment, speed, qc, lat, lon, date_time)
}

# Bind the list elements into one df
waal_gps_bi <- bind_rows(track_ls)

# Split up the "deployment" variable
waal_gps_bi[, c("logger", "id", "deploy")] <- 
  str_split(waal_gps_bi$deployment,
            pattern = "_", simplify = T)

# Write out CSV for environmental variables
write.csv(waal_gps_bi, file = "data/cleaned/waal_gps_bi.csv")

# Read in environmental data ----------------------------------------------

# This file needs to be created using EnvMove on MoveBank
waal_bi_env <- read.csv("data/cleaned/waal_bi_env.csv") %>%
  mutate(date_time = dmy_hms(date_time))

# Order the data by time and id
waal_bi_env <- with(waal_bi_env, waal_bi_env[order(id, date_time),])
waal_gps_bi <- with(waal_gps_bi, waal_gps_bi[order(id, date_time),])

# Check results of ordering to make sure they're the same
all.equal(waal_gps_bi$lat, waal_bi_env$Latitude)

# Variables to be appended
vars <- c("swell", "wave", "cloud", "cloud_hi",
          "cloud_lo", "wind_u", "wind_v", "rain")

# Append them
waal_gps_bi[, vars] <- waal_bi_env[, vars]

# Remove the environmental variables brought in from EnvMove process
rm(waal_bi_env)

# Change the location variable names
waal_gps_bi <- rename(waal_gps_bi, Latitude = lat, Longitude = lon)

# Run this function to process some environmental variables
waal_gps_bi <- split(waal_gps_bi, waal_gps_bi$id) %>%
  lapply(., env_append) %>%
  bind_rows()

# Turbidity data ----------------------------------------------------------

# Get dates for which data are required
date_series <- sort(unique(as.Date(waal_gps_bi$date_time)))

# Loop through API request
for(i in 1:length(date_series)){
  copernicus_download_motu(
    username = "jdarby",
    password = "Reagrove12",
    destination = paste("data/copernicus/zsd_", date_series[i], ".nc", sep = ""),
    product = "OCEANCOLOUR_GLO_BGC_L4_MY_009_104",
    layer = "cmems_obs-oc_glo_bgc-transp_my_l4-gapfree-multi-4km_P1D",
    sub_variables = "ZSD",
    output = "netcdf",
    region = c(-65, -60, -30, -28),
    timerange = c(paste(date_series[i], "00:00:00", sep = " "),
                  paste(date_series[i], "23:59:59", sep = " ")),
    overwrite = T
  )}

# Plot out an example for a look
raster("data/copernicus/zsd_2009-05-08.nc") %>% plot()

# Create a ZSD variable
waal_gps_bi$zsd <- NA

# Loop through unique dates in the dataset and append ZSD values to locations
for(i in 1:length(date_series)){
  # Read in the correct raster
  zsd_rstr <- raster(paste("data/copernicus/zsd_", date_series[i], ".nc", sep = ""))
  # Index for track points in the target date
  index <- which(as.Date(waal_gps_bi$date_time) == date_series[i])
  # Convert to spatial points
  xysp <- SpatialPoints(waal_gps_bi[index, c("Longitude", "Latitude")],
                        proj4string = CRS("+proj=longlat +datum=WGS84"))
  # Extract zsd data to points
  waal_gps_bi$zsd[index] <-
    raster::extract(x = zsd_rstr,
                    y = xysp,
                    method = "bilinear")
  # Print progress
  print(i)
}

# Get depth and distance to shelf -----------------------------------------

# Read in marmap
require(marmap)

# Get some bathymetry data
bathy_rstr <- marmap::getNOAA.bathy(lon1 = -91, lat1 = -71,
                                    lon2 = 10, lat2 = -27,
                                    resolution = 1)

# Turn it into a basic raster
bathy_rstr <- as.raster(bathy_rstr)

# Plot it, it looks nice
plot(abs(bathy_rstr))

# Plot out the 500m depth contour
lines <- rasterToContour(bathy_rstr, levels = c(-500))
plot(lines)

# Read in rgeos
require(rgeos)

# Function to get distance to 500m contour
dd = gDistance(lines, as(bathy_rstr,"SpatialPoints"), byid = T)

# Create a raster of distance to contour
iso500 <- bathy_rstr
iso500[] = apply(dd,1,min)

# Take a look
plot(iso500)

# Append depth to tracks
waal_gps_bi$depth <-
  raster::extract(bathy_rstr,
                  y = waal_gps_bi[, c("Longitude", "Latitude")],
                  method = "bilinear") / 1000

# Append distance to contour to tracks
waal_gps_bi$iso500 <-
  raster::extract(iso500,
                  y = waal_gps_bi[, c("Longitude", "Latitude")],
                  method = "bilinear")

# Save off
save(bathy_rstr, file = "data/bathy_rstr.RData")
save(iso500, file = "data/iso500.RData")

# Read in activity data ---------------------------------------------------

# Create an object containing all the deployment names
files <- c("AR18_5109491_19", "AR18_5146273_13", "AR18_5147731_1",
           "AR19_5143083_14", "AR19_5147358_20", "AR19_R876_2",
           "AR20_5116375_3", "AR20_5116432_21", "AR20_5144948_15",
           "AR21_4002403_22", "AR21_5056443_31", "AR21_5122454_16",
           "AR21_R291_4", "AR22_5117599_5", "AR22_5216938_17",
           "AR22_B211_32", "AR22_RA94_23", "AR23_4003028_24",
           "AR23_5108666_33", "AR23_5109766_6", "AR23_5146279_18",
           "AR24_5116353_7", "AR24_5216493_34", "AR25_5145003_35",
           "AR25_5222685_8", "AR26_5116409_36", "AR26_5144771_9",
           "AR27_5108648_27", "AR27_BA88_10", "AR28_5127237_11",
           "AR28_5156460_28", "AR29_5145073_29", "AR29_5187159_25",
           "AR29_GA97_12", "AR30_5116480_26", "AR30_5222562_30")

# Empty list
act_ls <- list()

# Loop through the files and write each to a list element
for(i in 1:length(files)){
  # Write the filepath from the list above
  filepath <- paste("data/WAAL_2009/act_data/", files[i], ".csv", sep = "")
  
  # Read this in, format the date and deployment id
  act_ls[[i]] <- read.csv(file = filepath) %>%
    mutate(date_time = dmy_hms(date_time),
           deployment = files[i])
}


# Bind list elements into one dataframe
waal_act_bi <- bind_rows(act_ls)

# Split up the "deployment" variable
waal_act_bi[, c("logger", "id", "deploy")] <- 
  str_split(waal_act_bi$deployment,
            pattern = "_", simplify = T)

# Adjust some activity timings slightly based on ingestions
waal_act_bi[which(waal_act_bi$deploy == "3"), "date_time"] <-
  waal_act_bi[which(waal_act_bi$deploy == "3"), "date_time"] +
  3600

waal_act_bi[which(waal_act_bi$deploy == "6"), "date_time"] <-
  waal_act_bi[which(waal_act_bi$deploy == "6"), "date_time"] +
  (60 * 60 * 24 * 365.25 * 9) - 21600

waal_act_bi[which(waal_act_bi$deploy == "13"), "date_time"] <-
  waal_act_bi[which(waal_act_bi$deploy == "13"), "date_time"] +
  (60 * 60 * 24 * 365.25 * 2) + 43200

# Create a keep variable to filter out subsequent points with the same activity
waal_act_bi$keep <- T

# Populate this "keep" variable
for(i in 2:nrow(waal_act_bi)){
  if(waal_act_bi$state[i] == waal_act_bi$state[i - 1]){
    waal_act_bi$seconds[i - 1] <- sum(waal_act_bi$seconds[(i - 1):i])
    waal_act_bi$keep[i] <- F
  }
}

# Subset 
waal_act_bi <- waal_act_bi[waal_act_bi$keep, ]

# Create an "end time" variable for each bout of wet or dry
waal_act_bi$end_time <- waal_act_bi$date_time + waal_act_bi$seconds

# Append environmental data -----------------------------------------------

# Variables to transfer from locations to activity
vars <- c("Latitude", "Longitude", "swell",
          "wave", "cloud", "cloud_hi",
          "cloud_lo", "wind_sp", "rain", "zsd",
          "depth", "iso500")

# Write NAs to these variables
waal_act_bi[, vars] <- NA

# Create dummy location offset and solar angle
waal_act_bi$loc_offset <- NA
waal_act_bi$sun_angle <- NA

# Find the nearest location to an activity switch and append variables
for(i in 1:nrow(waal_act_bi)){
  # Subset GPS data to only the target id
  gps_sub <- waal_gps_bi %>% filter(id == waal_act_bi$id[i])
  # Check that there are GPS data for this
  if(nrow(gps_sub) > 0){
    # Get the closest time stamp to the ith row of act data
    index <- which.closest(gps_sub$date_time, waal_act_bi$date_time[i])
    # Append variables
    waal_act_bi[i, vars] <- gps_sub[index, vars]
    # Get the time difference between the location and the activity
    waal_act_bi$loc_offset[i] <- 
      abs(
        as.numeric(
          difftime(waal_act_bi$date_time[i],
                   gps_sub$date_time[index],
                   units = "secs")))
    # Calculate solar angle of activity switch
    waal_act_bi$sun_angle[i] <- sunAngle(t = waal_act_bi$date_time[i],
                                         longitude = waal_act_bi$Longitude[i],
                                         latitude = waal_act_bi$Latitude[i])$altitude
  }
}

# Remove some variables
rm(vars, i, gps_sub, index)

# Save this off
save(waal_act_bi, file = "data/cleaned/waal_act_bi.RData")

# Append activity to locations --------------------------------------------

# Figure out whether GPS fixes were taken when wet or dry
waal_gps_bi <- split(waal_gps_bi, waal_gps_bi$id) %>%
  # Loop around and append number of associated activity
  lapply(., function(x){
    # Create variable for activity to fill
    x$act <- NA
    # Subset ing and act data to only the relevant id
    act <- waal_act_bi[which(waal_act_bi$id == x$id[1]), ]
    
    for(i in 1:nrow(x)){
    # Find whether the last activity switch was landing or takeoff
    act_prior <- act[which(act$date_time < x$date_time[i]), ]
    
    # Make sure there are some act data in this
    if(nrow(act_prior > 0)){
      # Subset to nearest act_prior
      act_prior <- act_prior[which.max(act_prior$date_time), ]
      # If last activity switch was landing, fix is wet
      x$act[i] <- ifelse(act_prior$state == "wet", "wet", "dry")
    }}
    # Return x
    x
  }) %>%
  bind_rows()

# Get distance to colony
waal_gps_bi$col_dist <-
  raster::pointDistance(p1 = waal_gps_bi[, c("Longitude", "Latitude")],
                        p2 = c(-38.069227, -54.015432), lonlat = T) / 1000

# Save this off
save(waal_gps_bi, file = "data/cleaned/waal_gps_bi.RData")

# Data exploration --------------------------------------------------------

# Plot out the densities of the fine-scale GPS data
ggplot(waal_gps_bi %>% filter(!is.na(act), id %in% c("5116480", "5187159"), speed > 0)) +
  geom_density(aes(x = speed, fill = act), position = "stack") +
  scale_x_continuous(limits = c(0, 125)) +
  facet_wrap(facets = ~act, nrow = 2)

waal_gps_bi %>% filter(act == "wet", id %in% c("5116480", "5187159"), speed > 0) %>%
  select(speed) %>% summary()

waal_gps_bi %>% filter(act == "wet", speed > 0) %>%
  ggplot() + geom_point(aes(x = speed, y = wind_sp))

# Save this off
save(waal_gps_bi, file = "data/cleaned/waal_gps_bi.RData")
