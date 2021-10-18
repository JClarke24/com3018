# ---------------------------------------------------------
# train_test_model() :
#
# Train the model based on the ethnicity that we have specified.
#
# INPUT       :   Data frame - dataset          - data
#                 String     - ethnicity        - specific ethnicity
#                 Bool       - kfold            - Determines if kfold should be used 
#
# OUTPUT      :   Model      - regression       
#
# ---------------------------------------------------------
train_test_model <- function(dataset, ethnicity, kfold = FALSE){
  
  # Check if the ethnicity is correct
  if(is.na(ethnicity) || is.null(ethnicity) || (ethnicity %notin% ETHNICITIES)){
    stop("Given ethnicity is incorrect")
  }
  
  print("---------------------------------------------------------")
  print(paste0("Training model for ethnicity: ", ethnicity))
    
  # Filter out the non needed Ethnicities      
  dataset <- dataset %>% filter(Ethnicity == ethnicity)
  
  # Plot the correlation between the regions for that specific Ethnicity
  plot_correlation_arrests_unemployment_ethnicity(dataset = dataset)
  
  # Get the model for that ethnicity
  regression <- train_model(dataset = dataset, 
                              field_name_output = "Value", 
                                field_name_inputs = "Arrests",
                                  poly_degree = 2,
                                    kfold = kfold)
  return(regression)
}


# ---------------------------------------------------------
# my_modeling_formula() :
#
# Create a formula for column names & given output
#
# INPUT       :   Data frame - dataset            - data
#                 List       - field_name_inputs  - name of the inputs field
#                 String     - field_name_output  - name of the output field
#                 Int        - poly_degree        - degree of the polynomial
#
# OUTPUT      :   Formula - R formula object
#
# ---------------------------------------------------------
my_modelling_formula <- function(dataset, field_name_inputs, field_name_output, poly_degree){
  
  formular<-paste(field_name_output,"~", field_name_inputs)
  return(formular)
  
}

# ---------------------------------------------------------
# train_model() :
#
# Training the linear and non linear model
#
# INPUT       :   Data frame - dataset            - data
#                 List       - field_name_inputs  - name of the inputs field
#                 String     - field_name_output  - name of the output field
#                 Int        - poly_degree        - degree of the polynomial
#                 Bool       - kfold              - determines if we apply kfold
#
# OUTPUT      :   Model - R model object
#
# ---------------------------------------------------------
train_model <- function(dataset, field_name_inputs, field_name_output, poly_degree, kfold){
  
  # Getting the formula based on the fields
  formular <- my_modelling_formula(dataset = dataset, 
                                   field_name_inputs = field_name_inputs,
                                   field_name_output = field_name_output, 
                                   poly_degree = poly_degree)
  
  # Check if one of the paremeters (kfold) is not true
  if(!kfold){
    
    # Split the data into training and testing records
    training_records <- round(nrow(dataset)*(HOLDOUT/100))
    training_data <- dataset[1:training_records,]
    validation_data <- dataset[-(1:training_records),]
    
    # Train our linear model
    linear_model <- lm(Value ~ polym(Arrests, degree = 1), data=training_data)
    
    # Train our non linear model
    polym_model <- lm(Value ~ polym(Arrests, degree = 2), data=training_data)
    
    # Plot the accuracy of the linear model and the error rate 
    A_scatter_plot_error(model = linear_model, 
                          dataset_train = training_data, 
                            dataset_valid = validation_data,
                              output_name = field_name_output, 
                                predictor_name = field_name_inputs)
    
    # Plot the accuracy of the non linear model and the error rate
    A_scatter_plot_error(model = polym_model, 
                           dataset_train = training_data, 
                             dataset_valid = validation_data, 
                               output_name = field_name_output, 
                                 predictor_name = field_name_inputs,
                                   degree = 2)
    

    # Print the overall summary of the two models
    print(summary(linear_model))
    print(summary(polym_model))
    
    return (linear_model)
  }
  
  # Case the kfold is true  
  else{
    
    dataset <- stratified_dataset(dataset = dataset)
    
    # Initialising some variables which will hold the best result through each iteration
    best_r2_linear <- -1
    best_r2_polym <- -1
    
    best_linear <- NULL
    best_polym <- NULL
    
    best_train_linear <- NULL
    best_valid_linear <- NULL
    
    best_train_polym <- NULL
    best_valid_polym <- NULL
    
    
    # Start the process of the kfold method
    for(k in 1:KFOLDS){
      splitData <- stratified_split(dataset, k)
      
      # Training the 2 models 
      linear_model <- lm(Value ~ polym(Arrests, degree = 1), data=splitData$train)
      polym_model <- lm(Value ~ polym(Arrests, degree = 2), data=splitData$train)
    
      print(paste0("LINEAR R2 = ", Nr2(linearModel = linear_model)," for fold Number = ", k))
      print(paste0("POLYM R2 = ", Nr2(linearModel = polym_model)," for fold Number = ", k))
      
      # Get the best metrics so far for linear model
      if(best_r2_linear <= Nr2(linearModel = linear_model)){
          best_r2_linear <- Nr2(linearModel = linear_model)
          best_linear <- linear_model
          best_train_linear <- splitData$train
          best_valid_linear <- splitData$validation
      }
      
      # Get the best metrics so far for non linear model
      if(best_r2_polym <= Nr2(linearModel = polym_model)){
          best_r2_polym <- Nr2(linearModel = polym_model)
          best_polym <-  polym_model
          best_train_polym <- splitData$train
          best_valid_polym <- splitData$validation
      }
      
    }
    
    # Plot the accuracy of the linear model and the error rate 
    A_scatter_plot_error(model = best_linear, 
                          dataset_train = best_train_linear, 
                            dataset_valid = best_valid_linear, 
                             output_name = field_name_output, 
                               predictor_name = field_name_inputs,
                                 k_fold = TRUE)
    
    # Plot the accuracy of the non linear model and the error rate
    A_scatter_plot_error(model = best_polym, 
                           dataset_train = best_train_polym, 
                             dataset_valid = best_valid_polym, 
                               output_name = field_name_output, 
                                 predictor_name = field_name_inputs,
                                   degree = 2,
                                     k_fold = TRUE)
    
    # Returning only the linear model
    return(best_linear)
  }
}

# ---------------------------------------------------------
# From COM3018 lab 2
# Nrmse()
#
# Calculate Root Mean Squared Error RMSE metric
#
# INPUT:      vector double - actual    - values for expected values
#             vector double - predicted - values of predicted values
#
# OUTPUT :    float         - calculated RMSE
# ---------------------------------------------------------
Nrmse<-function(actual,predicted){
  return(sqrt(mean((actual-predicted)^2)))
}

# ---------------------------------------------------------
# From COM3018 lab 2
# Nrmse()
#
# Calculate Mean Absolute Error MAE metric
#
# INPUT:      vector double - actual    - values for expected values
#             vector double - predicted - values of predicted values
#
# OUTPUT :    float         - calculated RMSE
# ---------------------------------------------------------
Nmae<-function(actual,predicted){
  return((mean(abs(actual-predicted))))
}

# ---------------------------------------------------------
# From COM3018 lab 2
# Nr2()
#
# Calculate the r2 metric (adjusted to allow for multiple inputs)
# REMEMBER this is a measure of the fit of the training data to the model
# it is NOT a measure of how good a predictor the model is on unseen data
#
# INPUT:      object - linearModel - trained linear model
#
# OUTPUT :    double -  R2 extracted from the model object
# ---------------------------------------------------------
Nr2<-function(linearModel){
  return(as.numeric(summary(linearModel)$adj.r.squared))
}

# ---------------------------------------------------------
# From COM3018 lab 4
# stratified_dataset()
#
# Calculates how many records should be in each fold and allocates each record
# to a fold ID by adding a new column to the dataset
#
# INPUT:      Data Frame - dataset - data
#
# OUTPUT :    Data Frame -  dataset - data allocated to fold ID
# ---------------------------------------------------------
stratified_dataset <- function(dataset){
  
  recordsPerFold <- ceiling(nrow(dataset)/KFOLDS)
  foldIds <- rep(seq(1:KFOLDS), recordsPerFold)
  foldIds <- foldIds[1:nrow(dataset)]
  
  dataset$foldId <- foldIds
  
  dataset<-dataset[order(runif(nrow(dataset))),]
  
  return(dataset)
}

# ---------------------------------------------------------
# From COM3018 lab 4
# stratified_split() :
#
# Generate the TRAIN and VALIDATION dataset based on the current fold
#
# INPUT   :   data frame         - dataset        - data
#
# OUTPUT  :   list               - train & validation datasets
# ---------------------------------------------------------
stratified_split<-function(dataset,fold){
  
  validation<-subset(dataset, subset= foldId==fold, select=-foldId)
  train<-subset(dataset, subset= foldId!=fold,select=-foldId)
  
  splitData <- list(train=train, validation=validation)
  return(splitData)
}


