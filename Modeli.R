library("caret")
library(jsonlite)

# ***** PARAMETERS *****
file <- "data.csv"
separator <- ","
jsonfile <- "model.json"


# ***** FUNCTIONS *****

# Call function for rf and lm model
model_call <- function(formula, data, args, trControl, tuneGrid) {
  model <- train(
    formula,
    data,
    method = args$method,
    trControl = trControl,
    tuneGrid = tuneGrid
  )
  return(model)
}

# Counting RRMSE for specific model
rrmse_count <- function(mean_y, model) {
  return(model$results$RMSE / mean_y)
}

# ***** MAIN *****

#Data for models
data <- na.omit(read.csv(file, sep = separator))

#Read JSON file
json <- read_json(jsonfile)

#List of all models and list of all rrmse values
model_list <- list()
rrmse_list <- list()
json_report <- list()

for (i in 1:length(json)) {
  j <- json[[i]] #taking one model from list of models
  a <- paste("model", i, sep = "")
  json_report[[a]] <- j #saving parameters to report json
  formula <-
    as.formula(j$train$formula) #separate formula from train values
  j$train <-
    subset(j$train, subset = names(j$train) != "formula") #remove formula from $train
  
  mtry <-
    subset(j$train, subset = names(j$train) == "mtry") #putting mtry value in tuneGrid if it exists
  if (length(mtry) != 0) {
    tuneGrid = data.frame(mtry = j$train$mtry)
    j$train <- subset(j$train, subset = names(j$train) != "mtry")
  } else {
    tuneGrid = NULL
  }
  
  #Train parameters without formula and mtry
  args <- list()
  args[names(j$train)] <- j$train
  
  #TrainControl parameters in one list
  ctrl <- list()
  ctrl[names(j$trControl)] <- j$trControl
  #TrainControl model that we pass to train function
  trControl <- data.frame()
  trControl <- do.call(trainControl, ctrl)
  
  #Creating model
  model <- model_call(formula, data, args, trControl, tuneGrid)
  model_list[[i]] <- model
  
  #Calculating RRMSE
  y <- all.vars(formula)[1]
  mean_y <- mean(simplify2array(data[y]))
  rrmse <- rrmse_count(mean_y, model)
  rrmse_list[i] <- rrmse
  json_report[[a]]["RRMSE"] <- rrmse
}

#Saving model info and rrmse to report json file
write(toJSON(json_report), "report_json.json")
