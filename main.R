# PRACTICAL BUSINESS ANALYTICS COURSEWORK
# COM3018
#
# AHPER TEAM
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# 05 NOVEMBER 2020
#
# ---------------------------------------------------------

# clears all objects in "global environment"
rm(list=ls())

# Remove warnings
options(warn = -1)

# Get rid of scientific notation
options(scipe = 999)

# ---------------------------------------------------------
# User defined functions are next

#---------------------------------------------------------
# get_the_scaled_ordinals_categorical(): 
# 
# Gets the categorical and scaled ordinals for a given 
# dataset
#
# INPUT:  Data Frame  - dataset         - data 
#         List        - field_types     - list of field types
#
# OUTPUT: Data Frame  - dataset_scaled  - data
#
#---------------------------------------------------------
get_the_scaled_ordinals_categorical <- function(dataset, field_types){
  
  # Creates a sub-set frame of just the ordinal fields and sets any outliers to a mean value
  ordinal_dataset <- dataset[,which(field_types == TYPE_ORDINAL)]
  ordinal_dataset <- NPREPROCESSING_outlier(as.data.frame(ordinal_dataset), OUTLIER_CONF)
  
  # Normalises the ordinals using z-scale and then rescales the values to fit between 0 and 1
  zscaled_dataset<-as.data.frame(scale(ordinal_dataset, center=TRUE, scale=TRUE))
  ordinals_dataset<-as.data.frame(Nrescaleentireframe(zscaled_dataset))
  
  categorical_dataset <- NPREPROCESSING_categorical(dataset = dataset, field_types = field_types)
  
  # Merges ordinals and categoricals back together
  dataset_scaled <- cbind(categorical_dataset,ordinals_dataset)
  
  dataset <- dataset %>% mutate(Value = ordinals_dataset$Value)
  dataset <- dataset %>% mutate(Arrests = ordinals_dataset$Arrests)
  
  
  return(dataset)
  
}

#---------------------------------------------------------
# print_field_types(): 
# 
# Printing the fieldtypes in a formatted table. If the
# parameter 'discreet' is TRUE, we will also return the 
# results
#
# INPUT:  dataset       - data frame
#         field_types   - list
#         discreet      - boolean - DEFAULT = FALSE
#
# OUTPUT: field_types   - data frame
#
#---------------------------------------------------------
print_field_types <- function(dataset, field_types, discreet = FALSE){
  
  if(discreet == TRUE){
    field_types <- NPREPROCESSING_discreetNumeric(dataset = dataset, 
                                                     field_types = field_types, 
                                                       cutoff = DISCREET_BINS)
    # Prints out the type of each field in a friendly user table
    results <- data.frame(field = names(dataset), types = field_types)
    print(formattable::formattable(results))
    
    return(field_types)
  }
  
  # Prints out the type of each field in a friendly user table
  results <- data.frame(field = names(dataset), types = field_types)
  print(formattable::formattable(results))
  
}

#---------------------------------------------------------
# main()
#
# Keeps all objects as local to this function
# INPUT: None
#
# OUTPUT :None
#
#---------------------------------------------------------
main<-function(){
  
  # Reads the dataset(s) and output information about fields to the viewer
  arrests_dataset <- as.data.table(NreadDataset(csvFilename = DATASET_FILENAME_ARRESTS))
  unemployment_dataset <- as.data.table(NreadDataset(csvFilename = DATASET_FILENAME_UNEMPLOYMENT_REGION))
  
  
  # Print the number of samples before cleaning (arrests_dataset)
  print("-----------------------------")
  print(paste("Before modifying (arrests_dataset) | Filtering redundent data, nrows:", arrests_dataset[,.N]))
  
  # Filtering out the redundant data. Specifying which Ethnicities, Gender and AgeGroup we are interested in
  arrests_dataset <- arrests_dataset[(Ethnicity %chin% ETHNICITIES) & (Gender %chin% FIELD_VALUE_ALL) & (AgeGroup %chin% FIELD_VALUE_ALL)]
                       
  arrests_dataset
  # Print the number of samples after cleaning (arrests_dataset)
  print(paste("After modifying (arrests_dataset) | Filtering redundent data, nrows:", arrests_dataset[,.N]))
  
  
  # Print the number of samples before cleaning (unemployment_dataset)
  print("-----------------------------")
  print(paste("Before modifying (unemployment_dataset) | Filtering redundent data, nrows:",unemployment_dataset[,.N]))
  
  # Filtering out the redundant data. Specifying which Ethnicities, Sex and Age we are interested in
  unemployment_dataset <- unemployment_dataset[(Ethnicity %chin% ETHNICITIES) & (Sex %chin% FIELD_VALUE_ALL) & (Age %chin% FIELD_VALUE_ALL)]


  # Print the number of samples after cleaning (unemployment_dataset)
  print(paste("After modifying (unemployment_dataset) | Filtering redundent data, nrows:",unemployment_dataset[,.N]))
  print("-----------------------------")

  # Encode region to more general names to match the other dataset
  arrests_dataset <- APREPROCESSING_encodeRegion(dataset = arrests_dataset)
  
  # Encode years to more names years to match the other dataset
  arrests_dataset <- APREPROCESSING_encodeYear(dataset = arrests_dataset)
  
  # Combining same samples with different number of arrests due to encoding
  arrests_dataset <- APREPROCESSING_combine_ethnicity_region(dataset = arrests_dataset) 
  
  
  # Getting only the years above the 2006
  arrests_dataset <- arrests_dataset[(Time > STARTING_YEAR) & (Time <= ENDING_YEAR)]
  unemployment_dataset <- unemployment_dataset[(Time > STARTING_YEAR) & (Time <= ENDING_YEAR)]
  

  # Determines which fields are numeric and which are symbolic
  field_types_arrests_dataset <- NPREPROCESSING_initialFieldType(dataset = as.data.frame(arrests_dataset))
  field_types_unemployment_dataset <- NPREPROCESSING_initialFieldType(dataset = as.data.frame(unemployment_dataset))
  
  
  # Print field types
  print_field_types(dataset = arrests_dataset, field_types = field_types_arrests_dataset)
  print_field_types(dataset = unemployment_dataset, field_types = field_types_unemployment_dataset)
  
  
  
  # Converts the numeric fields to numeric ones. The CSV files holds different type
  # of values in the same field, which makes the field to be non-numeric.
  arrests_dataset <- APREPROCESSING_ConvertToNumeric(dataset = as.data.frame(arrests_dataset), 
                                                     field_types = field_types_arrests_dataset)
  
  unemployment_dataset <- APREPROCESSING_ConvertToNumeric(dataset = as.data.frame(unemployment_dataset), 
                                                          field_types = field_types_unemployment_dataset)
  
  
  # Prints out the pretty table about the field types of a give dataset. It describes
  # the details and uniqueness of each column
  NPREPROCESSING_prettyDataset(dataset = as.data.frame(arrests_dataset))
  NPREPROCESSING_prettyDataset(dataset = as.data.frame(unemployment_dataset))
  
  # Finding the type of a numeric value (ordinal, discreet, etc.)
  field_types1_arrests_dataset <- print_field_types(dataset = as.data.frame(arrests_dataset), field_types = field_types_arrests_dataset, discreet = TRUE)
  field_types1_unemployment_dataset <- print_field_types(dataset = as.data.frame(unemployment_dataset), field_types = field_types_unemployment_dataset, discreet = TRUE)
  
  # Plot the datasets
  plot_unemployment_dataset(unemployment_dataset = unemployment_dataset[Region == 'All'])
  plot_arrests_dataset(arrests_dataset = arrests_dataset[Region == 'All'])
  
  # Merge the datasets
  merged_dataset <- merge_datasets(arrests_dataset = arrests_dataset, unemployment_dataset = unemployment_dataset)

  # Plot the correlation of the Unemployment and arrests rate on all ethnicities combined
  plot_initial_correlation(merged_dataset = merged_dataset[Region != 'All'])
  
  
  # Randomise dataset
  merged_dataset_ready <- merged_dataset[order(runif(nrow(merged_dataset))),]
  merged_dataset_ready <- merged_dataset_ready[complete.cases(merged_dataset_ready)]
  
  # Removing the data points which will affect our accuracy
  merged_dataset_ready <- merged_dataset_ready[(Region != 'All')]

  
  #Train model using holdout method
  for(ethnicity in ETHNICITIES){
    train_test_model(as.data.frame(merged_dataset_ready), ethnicity)
    
  }
  
  #Train model using K-fold validation
  for(ethnicity in ETHNICITIES){
    train_test_model(as.data.frame(merged_dataset_ready), ethnicity, kfold = TRUE)
    
  }
  
} # endof main()

#---------------------------------------------------------
# This is where R starts execution

# clears the console area
cat("\014")
gc()

# Loads the library which will load all the other libriaries
library(pacman)

# Including the scripts that will be used throughout the process
source("preparation.R")
source("plot.R")
source("global_vars.R")
source("modelling.R")

# Load the all libraries we need
pacman::p_load(char = MYLIBRARIES, install = TRUE, character.only = TRUE)

# Sets the starting number used to generate a sequence of random numbers
set.seed(123)

#---------------------------------------------------------
print("Starting the process")
main()
print("Ending the procces")
