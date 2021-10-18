# ---------------------------------------------------------
# Global Environment variables
# - i.e. available to all functions
# Placing constant in uppercase named variables
DATASET_FILENAME_ARRESTS <- "number-of-arrests.csv"
DATASET_FILENAME_UNEMPLOYMENT_REGION <- "unemployment-by-region.csv"

TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded

STARTING_YEAR     <- 2008
ENDING_YEAR       <- 2017
FIELD_VALUE_ALL   <- "All"
ETHNICITIES       <- c('Asian', 'Black', 'White', 'Other', 'Mixed')
YEARS             <- c(2012,2013,2014,2015,2016,2017)

DISCREET_BINS     <- 10                    # Number of empty bins to determine discreet
OUTLIER_CONF      <- 0.95                  # Confidence p-value for outlier detection
OUTLIER_CONF_Z    <- 0.40
MAX_LITERALS      <- 20                    # Maximum number of hotcoding new fields

HOLDOUT           <- 70
KFOLDS            <- 5

`%notin%` <- Negate(`%in%`)

# ---------------------------------------------------------
# Define and then load the libraries used in this project
# Library from CRAN     Version
# pacman	               0.5.1
# formattable 	         0.2.0.1
# outliers	             0.14
# caret                  6.0.84
# scatterplot3d          0.3.41
# RColorBrewer           1.1-2
# graphics               4.0.2
# ggplot2                3.3.2
# dplyr                  1.0.2
# data.table             1.13.0

MYLIBRARIES<-c("formattable",
               "outliers",
               "caret",
               "scatterplot3d",
               "graphics",
               "RColorBrewer",
               "ggplot2",
               "dplyr",
               "data.table")

