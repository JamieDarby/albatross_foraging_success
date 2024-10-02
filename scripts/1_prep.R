
require(ggmap)
require(sf)
require(raster)
require(tidyverse)
require(lubridate)
require(EMbC)
require(ggsn)
require(future.apply)
require(PathInterpolatR)
require(momentuHMM)
require(birk)
require(cowplot)
require(fossil)
require(oce)
require(TwGeos)
require(mgcv)
require(gratia)
require(DHARMa)
require(performance)
require(ROCR)
require(caret)
require(jtools)
require(CopernicusMarine)

load("data/land_df_wgs.RData")

# trip_cleaner ------------------------------------------------------------

trip_cleaner <- function(x, t = 60)
{
  ind <- 1
  
  x$section <- paste(x$id, ind, sep = "_")
  
  for(i in 2:(nrow(x)))
  {
    diff <- abs(as.numeric(difftime(time1 = x$date_time[i],
                                    time2 = x$date_time[i - 1],
                                    units = "mins")))
    
    if(diff > t){ind <- ind + 1}
    
    x$section[i] <- paste(x$id[i], ind, sep = "_")
  }
  x
}

# LinStepR ----------------------------------------------------------------

LinStepR <- function(df, t = 60, extras = NULL)
{
  time_range <- seq(from = (df$date_time[1]),
                    to = df$date_time[nrow(df)],
                    by = t)
  
  out <- data.frame(date_time = time_range,
                    Longitude = rep(df$Longitude[1], length(time_range)),
                    Latitude = rep(df$Latitude[1], length(time_range)))
  
  out[extras] <- df[1, extras]
  
  for(i in 2:nrow(out))
  {
    index <- birk::which.closest(vec = df$date_time,
                                 x = out$date_time[i])
    
    if(df$date_time[index] > out$date_time[i]){index <- index - 1}
    
    indices <- (index):(index + 1)
    
    if(indices[1] < 1){indices[1] <- 1}
    
    if(indices[2] > nrow(df)){indices[2] <- nrow(df)}
    
    xyt <- df[indices, c("Longitude", "Latitude", "date_time")] %>%
      mutate(date_time = as.numeric(date_time))
    
    t.slice <- as.numeric(out$date_time[i])
    
    if(t.slice > xyt$date_time[2]){t.slice -> xyt$date_time[2]}
    if(t.slice < xyt$date_time[1]){t.slice -> xyt$date_time[1]}
    
    out[i, c("Longitude", "Latitude")] <-
      (linear(xyt = xyt,
              t.slice = t.slice))[,(1:2)]
    
    if(anyNA(out[i, c("Longitude", "Latitude")])){
      out[i, c("Longitude", "Latitude")] <-
        df[index, c("Longitude", "Latitude")]
    }
  }
  out
}

# ingest_append -----------------------------------------------------------

ingest_append <- function(x, ing,
                          time_lim = 300,
                          transfers = c("event", "event.number", "mass")){
  x[, transfers] <- NA
  
  ing <- ing[which(ing$id == x$id[1]), ]
  
  for(i in 1:nrow(ing)){
    date_min <- ing$date_time[i] - time_lim
    date_max <- ing$date_time[i] + time_lim
    
    y <- x[which(x$date_time >= date_min & x$date_time <= date_max), ]
    
    if(nrow(y) > 0){
      index <- which.closest(y$date_time, ing$date_time[i])[1]
      
      y[index, transfers] <- ing[i, transfers]
      
      x[which(x$date_time >= date_min & x$date_time <= date_max), ] <- y
    }
  }
  x
}

# env_append --------------------------------------------------------------

env_append <- function(x)
{
  x$wind_sp <- NA
  x$wind_dir <- NA
  x$traj <- rep(0, nrow(x))
  x$wind_offset <- rep(0, nrow(x))
  
  if(nrow(x) > 0)
  {
    for(i in 1:nrow(x)){
      x$wind_sp[i] <- sqrt((x$wind_v[i] ^ 2) + (x$wind_u[i] ^ 2))
      
      x$wind_dir[i] <-
        (((atan2((x$wind_u[i]/x$wind_sp[i]),
                 (x$wind_v[i]/x$wind_sp[i]))) * (180 / pi)) + 180)
      
      if(x$wind_dir[i] > 180)
      {x$wind_dir[i] <- - 360 + abs(x$wind_dir[i])}
      
      if(i < nrow(x))
      {
        x$traj[i] <- earth.bear(x$Longitude[i],
                                x$Latitude[i],
                                x$Longitude[i + 1],
                                x$Latitude[i + 1])
        
        if(x$traj[i] > 180)
        {x$traj[i] <- (360 - abs(x$traj[i])) * -1}
        
        diff_dir <- x$traj[i] - x$wind_dir[i]
        
        if(diff_dir > 180)
        {diff_dir <- (360 - diff_dir) * -1}
        if(diff_dir < -180)
        {diff_dir <- (360 - abs(diff_dir))}
        
        x$wind_offset[i] <- diff_dir
      }
    }
    
    # Fixed all this because of a stupud update in "oce"
    # Need to change back if they fix it
    
    x[, c("sun_angle", "moon_angle", "moon_frac")] <- NA
    
    for(i in 1:nrow(x)){
    x$sun_angle[i] <- (sunAngle(t = x$date_time[i],
                             lat = x$Latitude[i],
                             lon = x$Longitude[i]))$altitude
    
    x$moon_angle[i] <- (moonAngle(t = x$date_time[i],
                               lat = x$Latitude[i],
                               lon = x$Longitude[i]))$altitude
    
    x$moon_frac[i] <- (moonAngle(t = x$date_time[i],
                              lat = x$Latitude[i],
                              lon = x$Longitude[i]))$illuminatedFraction
    }
    
    x$moon_total <- ifelse(x$moon_angle < 0, 0, x$moon_frac)
  }
  x
}

# ing_act -----------------------------------------------------------------

ingest_act_append <- function(ing, act){
  
  ing$act <- NA
  ing$act_prior <- NA
  ing$act_prox <- NA
  ing$time_in_act <- NA
  ing$nearest_act <- NA
  ing$which_nearest <- NA
  
  act <- act[which(act$id == ing$id[1]), ]
  
  for(i in 1:nrow(ing)){
    
    act_prior <- act[which(act$date_time <= ing$date_time[i]), ]
    act_prior <- act_prior[which.max(act_prior$date_time), ]
    
    act_prox <- act[which(act$date_time > ing$date_time[i]), ]
    act_prox <- act_prox[which.min(act_prox$date_time), ]
    
    if(nrow(act_prior) > 0){
      ing$act[i] <- act_prior$act
      
      ing$act_prior[i] <- abs(difftime(act_prior$date_time,
                                       ing$date_time[i],
                                       units = "secs"))
    }
    
    if(nrow(act_prox) > 0){
      ing$act_prox[i] <- abs(difftime(act_prox$date_time,
                                      ing$date_time[i],
                                      units = "secs"))
    }else{if(nrow(act_prior) > 0){
      ing$which_nearest[i] <- "prior"}}
    
    if(nrow(act_prior) > 0 & nrow(act_prox) > 0){
      ing$nearest_act[i] <- min(ing[i, c("act_prox", "act_prior")])
      
      ing$time_in_act[i] <- abs(difftime(act_prior$date_time,
                                         act_prox$date_time,
                                         units = "secs"))
      
      ing$which_nearest[i] <- ifelse(ing$act_prior[i] < ing$act_prox[i], "prior", "prox")
    }
  }
  ing
}

# act_ingest_append -------------------------------------------------------

act_ingest_append <- function(act, ing, mass_var = "integral"){
  
  ing <- ing[which(ing$id == act$id[1]), ]
  
  act[, c("ing_bool", "ing_count", "ing_mass", "ing_max")] <- NA
  
  if(nrow(ing) > 0){
    for(i in 1:nrow(act)){
      if(act$act[i] == "wet"){
        ing_sub <- ing[which(ing$date_time_ad >= act$date_time[i] &
                               ing$date_time_ad <= act$end_time[i]), ]
        
        act$ing_bool[i] <- nrow(ing_sub) > 0
        act$ing_count[i] <- nrow(ing_sub)
        
        if(nrow(ing_sub) > 0){
          act$ing_mass[i] <- sum(ing_sub[, mass_var])
          act$ing_max[i] <- max(ing_sub[, mass_var], na.rm = T)
        }else{act$ing_mass[i] <- 0}
      }
    }
  }
  act
}

