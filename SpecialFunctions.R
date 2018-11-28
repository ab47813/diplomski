library(suncalc)
library(sirad)

#Function for calculating sunrise and sunset time by date and location
sunset_sunrise_calc <- function(date, lat, lon) {
  return(getSunlightTimes(as.Date(date), lat, lon, tz = Sys.timezone()))
  
}

#Function for cloudiness calculation
cloudiness_calc <- function(day_length, daily_sun) {
  c <- list()
  c <-
    append(c, (as.numeric(day_length) - as.numeric(daily_sun)) / as.numeric(day_length))
  for (i in 1:length(c)) {
    if (c[i] < 0) {
      c[i] <- 0
    }
  }
  return(c)
}

#Calculating extrat radiation with sirad library
radiation_extrat <- function(date, lat) {
  return(extrat(dayOfYear(date), radians(lat)))
}