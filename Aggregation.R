library(chron)
library(expss)
library(lubridate)

file <- "Postaja_Institut_20171017-20181017.csv"

# FUNCTIONS

#Function for csv reading
readFile <- function(file) {
  return(
    read.csv(
      file = gsub("\"", "", file),
      header = TRUE,
      sep = ";",
      row.names = NULL,
      fileEncoding = "UTF-8-BOM"
    )
  )
}

#Subsetting part of data set that is needed for calculations
subset_data <- function(start_date, end_date, data, parameter) {
  calc_data <- list()
  calc_data <-
    subset(data[parameter],
           subset = (data$Timestamp >= start_date &
                       data$Timestamp <= end_date))
  return(simplify2array(calc_data))
}

#Average value of parameter from start date till end date
average_data <- function(dataset) {
  return(mean(dataset, na.rm = TRUE))
}

#Range function -> max - min value
range_data <- function(dataset) {
  max <- max(dataset, na.rm = TRUE)
  min <- min(dataset, na.rm = TRUE)
  return(max - min)
}

#Sum of parameter from start_date till end_date
sum_data <- function(dataset) {
  return(sum(dataset, na.rm = TRUE))
}

#Count all the rows that satisfy the statement
count_data <- function(dataset,
                       statement) {
  n <- count_if(statement, dataset)
  return(n)
}

#Save rows from data frame that satisfy the statement
saveif_data <- function(start_date, end_date, dataset,
                        statement) {
  n <- 0
  savedData <- data.frame()
  calc_data <- list()
  calc_data <-
    subset(data,
           subset = (data$Timestamp >= start_date &
                       data$Timestamp <= end_date))
  param_data <- dataset
  for (i in 1:length(param_data)) {
    n <- count_if(statement, param_data[i])
    if (n == 1) {
      savedData <- rbind(savedData, calc_data[i,])
    }
  }
  return(savedData)
}

#Timestamps by hours
hours_frame <- function(start_date, end_date) {
  hourly <- list()
  h <- start_date
  hourly <- append(hourly, as.POSIXct(h))
  while (h < end_date) {
    hour(h) <- hour(h) + 1
    if (h <= end_date) {
      hourly <- append(hourly, as.POSIXct(h))
    }
  }
  return(hourly)
}

#Timestamps by days
days_frame <- function(start_date, end_date) {
  daily <- list()
  d <- start_date
  daily <- append(daily, as.POSIXct(d))
  while (d < end_date) {
    day(d) <- day(d) + 1
    if (d <= end_date) {
      daily <- append(daily, as.POSIXct(d))
    }
  }
  return(daily)
}

#Timestamps by months
months_frame <- function(start_date, end_date) {
  start_date <-
    as.POSIXct(start_date, format = "%Y-%m-%d")
  end_date <-
    as.POSIXct(end_date, format = "%Y-%m-%d")
  monthly <- list()
  m <- start_date
  monthly <- append(monthly, as.POSIXct(m))
  while (m < end_date) {
    month(m) <- month(m) + 1
    if (m <= end_date) {
      monthly <- append(monthly, as.POSIXct(m))
    }
  }
  return(monthly)
}

# MAIN
data <- readFile(file)

#Date format for data

data$Timestamp <-
  paste(data$Mjesec,
        "/",
        data$Dan,
        "/",
        data$Godina,
        " ",
        data$Sat,
        ":",
        data$Min)
data$Timestamp <- gsub("[[:space:]]", "", data$Timestamp)
data$Timestamp <-
  as.POSIXct(data$Timestamp, format = "%m/%d/%Y %H:%M", tz = Sys.timezone())
