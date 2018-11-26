library(chron)
library(expss)

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

#Average value of parameter from start date till end date
average <- function(start_date, end_date, parameter) {
  calc_data <- list()
  calc_data <-
    subset(data[parameter],
           subset = (data$Timestamp >= start_date &
                       data$Timestamp <= end_date))
  return(mean(simplify2array(calc_data)))
}

#Min, max and range function
range <- function(start_date, end_date, parameter) {
  calc_data <- list()
  calc_data <-
    subset(data[parameter],
           subset = (data$Timestamp >= start_date &
                       data$Timestamp <= end_date))
  calc_data <- simplify2array(calc_data)
  max <- max(calc_data)
  min <- min(calc_data)
  return(c(max, min, max - min))
}

#Sum of parameter from start_date till end_date
sum_data <- function(start_date, end_date, parameter) {
  calc_data <- list()
  calc_data <-
    subset(data[parameter],
           subset = (data$Timestamp >= start_date &
                       data$Timestamp <= end_date))
  calc_data <- simplify2array(calc_data)
  return(sum(calc_data))
}

#Count all the rows that satisfy the statement
count_data <- function(start_date,
                       end_date,
                       parameter,
                       statement) {
  calc_data <- list()
  calc_data <-
    subset(data[parameter],
           subset = (data$Timestamp >= start_date &
                       data$Timestamp <= end_date))
  calc_data <- simplify2array(calc_data)
  n <- count_if(statement, calc_data)
  return(n)
}

#Save rows from data frame that satisfy the statement
save_data_if <- function(start_date,
                         end_date,
                         parameter,
                         statement) {
  calc_data <- list()
  n <- 0
  savedData <- data.frame()
  calc_data <-
    subset(data,
           subset = (data$Timestamp >= start_date &
                       data$Timestamp <= end_date))
  param_data <- simplify2array(calc_data[parameter])
  for (i in 1:length(param_data)) {
    n <- count_if(statement, param_data[i])
    if (n == 1) {
      savedData <- rbind(savedData, calc_data[i, ])
    }
  }
  return(savedData)
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

#Average function call example
m <-
  average(
    as.POSIXct("2017-10-20 19:35", format = "%Y-%m-%d %H:%M"),
    as.POSIXct("2017-10-20 23:21", format = "%Y-%m-%d %H:%M"),
    "Tlak.zraka"
  )

#Range function call example
r <-
  range(
    as.POSIXct("2017-10-20 19:35", format = "%Y-%m-%d %H:%M"),
    as.POSIXct("2017-10-20 23:21", format = "%Y-%m-%d %H:%M"),
    "Tlak.zraka"
  )

#Sum_dates function call example
s <-
  sum_data(
    as.POSIXct("2017-10-20 19:35", format = "%Y-%m-%d %H:%M"),
    as.POSIXct("2017-10-20 23:21", format = "%Y-%m-%d %H:%M"),
    "Brzina.vjetra"
  )

#Count_if function call example
c <-
  count_data(
    as.POSIXct("2017-10-20 19:35", format = "%Y-%m-%d %H:%M"),
    as.POSIXct("2017-10-20 23:21", format = "%Y-%m-%d %H:%M"),
    "Temperatura.zraka",
    as.criterion(gt(17) & le(18))
  )

#Save_data_if function call example
d <-
  save_data_if(
    as.POSIXct("2017-10-20 19:35", format = "%Y-%m-%d %H:%M"),
    as.POSIXct("2017-10-20 23:21", format = "%Y-%m-%d %H:%M"),
    "Temperatura.zraka",
    as.criterion(gt(18) & le(18.5))
  )