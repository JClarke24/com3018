# ************************************************
# Pre-Processing a Dataset functions

# To manually set a field type
# This will store $name=field name, $type=field type
manualTypes <- data.frame()

#---------------------------------------------------------
# APREPROCESSING_encodeRegion()
# 
# Replace the cities with the region they belong to
#
# INPUT:  Data Frame - dataset - data
#
# OUTPUT: Data Frame - dataset - encoded data
#
#---------------------------------------------------------
APREPROCESSING_encodeRegion<-function(dataset){
  print('Encoding the Regions. Starting...')
  
  dataset[dataset == "Avon and Somerset"] <- "South West"
  dataset[dataset == "Bedfordshire"] <- "East Midlands"
  dataset[dataset == "Cambridgeshire"] <- "East of England"
  dataset[dataset == "Cheshire"] <- "North West"
  dataset[dataset == "Cleveland"] <- "Yorkshire and The Humber"
  dataset[dataset == "Cumbria"] <- "North West"
  dataset[dataset == "Derbyshire"] <- "East Midlands"
  dataset[dataset == "Devon and Cornwall"] <- "West England"
  dataset[dataset == "Dorset"] <- "South West"
  dataset[dataset == "Durham"] <- "North West"
  dataset[dataset == "Dyfed-Powys"] <- "Wales"
  dataset[dataset == "Essex"] <- "East of England"
  dataset[dataset == "Gloucestershire"] <- "South West"
  dataset[dataset == "Greater Manchester"] <- "North West"
  dataset[dataset == "Gwent"] <- "Wales"
  dataset[dataset == "Hampshire"] <- "South East"
  dataset[dataset == "Hertfordshire"] <- "East of England"
  dataset[dataset == "Humberside"] <- "Yorkshire and The Humber"
  dataset[dataset == "Kent"] <- "South East"
  dataset[dataset == "Lancashire"] <- "North West"
  dataset[dataset == "Leicestershire"] <- "East Midlands"
  dataset[dataset == "Lincolnshire"] <- "East Midlands"
  dataset[dataset == "London, City of"] <- "London"
  dataset[dataset == "Merseyside"] <- "North West"
  dataset[dataset == "Metropolitan Police"] <- "London"
  dataset[dataset == "Norfolk"] <- "East of England"
  dataset[dataset == "North Wales"] <- "Wales"
  dataset[dataset == "North Yorkshire"] <- "Yorkshire and The Humber"
  dataset[dataset == "Northamptonshire"] <- "East Midlands"
  dataset[dataset == "Northumbria"] <- "North East"
  dataset[dataset == "Nottinghamshire"] <- "East Midlands"
  dataset[dataset == "South Wales"] <- "Wales"
  dataset[dataset == "South Yorkshire"] <- "Yorkshire and The Humber"
  dataset[dataset == "Staffordshire"] <- "West Midlands"
  dataset[dataset == "Suffolk"] <- "East of England"
  dataset[dataset == "Surrey"] <- "South East"
  dataset[dataset == "Sussex"] <- "South East"
  dataset[dataset == "Thames Valley"] <- "South East"
  dataset[dataset == "Warwickshire"] <- "West Midlands"
  dataset[dataset == "West Mercia"] <- "West Midlands"
  dataset[dataset == "West Midlands"] <- "West Midlands"
  dataset[dataset == "West Yorkshire"] <- "Yorkshire and The Humber"
  dataset[dataset == "Wiltshire"] <- "South West"
  
  print('Encoding the Regions. Finishing...')
  print("-----------------------------")
  return (dataset)
}

#---------------------------------------------------------
# APREPROCESSING_encodeYear()
#
# Replace the years with the region they belong to
#
# INPUT:  Data Frame - dataset - data
#
# OUTPUT: Data Frame - dataset - encoded data
#
#---------------------------------------------------------
APREPROCESSING_encodeYear<-function(dataset){
  print('Encoding the Years. Starting...')
  
  dataset[dataset == "2006/07"] <- 2006
  dataset[dataset == "2007/08"] <- 2007
  dataset[dataset == "2008/09"] <- 2008
  dataset[dataset == "2009/10"] <- 2009
  dataset[dataset == "2010/11"] <- 2010
  dataset[dataset == "2011/12"] <- 2011
  dataset[dataset == "2012/13"] <- 2012
  dataset[dataset == "2013/14"] <- 2013
  dataset[dataset == "2014/15"] <- 2014
  dataset[dataset == "2015/16"] <- 2015
  dataset[dataset == "2016/17"] <- 2016
  dataset[dataset == "2017/18"] <- 2017
  
  dataset[,Time := as.numeric(Time)]
  
  print('Encoding the Years. Finishing...')
  print("-----------------------------")
  
  return (dataset)
  
}
#---------------------------------------------------------
# APREPROCESSING_combine_ethnicity_region()
# 
# After encoding the cities, we will have same regions with
# different number of arrests (5 cities in one region). 
# Therefore we will have to add them up and be one row. Also,
# the returned dataset will consist of only 4 fields, which 
# will help us to merge the dataset
#
# INPUT:  Data Frame - dataset - data
#
# OUTPUT: Data Frame - temp_df - data
#---------------------------------------------------------
APREPROCESSING_combine_ethnicity_region <- function(dataset){
  
  print('Combining ethnicities based on the region. Starting...')
  
  # Creatinga local data frame
  temp_df <- data.table(V1 = numeric(),
                        V2 = character(),
                        V3 = character(),
                        V4 = numeric(),
                        V5 = numeric())
  
  # Converting the number of arrests to numeric and removing white spaces
  # to avoid errors
  dataset[, XNumberofarrests := lapply(XNumberofarrests, function(x) trimws(x, which = c("both")))]
  dataset[, XNumberofarrests := lapply(XNumberofarrests, function(x) as.numeric(gsub("\\,", "", x)))]
  dataset[, XNumberofarrests := as.numeric(XNumberofarrests)]
  
  # Converting the population to numeric and removing white spaces
  # to avoid errors (Used the nature of data table which will apply the function
  # to each sample on a specific field)
  dataset[, PopulationbyethnicitygenderandPFAbasedon2011Census := lapply(PopulationbyethnicitygenderandPFAbasedon2011Census, function(x) trimws(x, which = c("both")))]
  dataset[, PopulationbyethnicitygenderandPFAbasedon2011Census := lapply(PopulationbyethnicitygenderandPFAbasedon2011Census, function(x) as.numeric(gsub("\\,", "", x)))]
  dataset[, PopulationbyethnicitygenderandPFAbasedon2011Census := as.numeric(PopulationbyethnicitygenderandPFAbasedon2011Census)]
  
  # This for loop will iterate through every single sample and replace
  # similar samples into one sample.
  for(temp_ethnicity in unique(dataset$Ethnicity)){
    for(temp_region in unique(dataset$Geography)){
      for(temp_year in unique(dataset$Time)){
        
        # Select the specific rows 
        temp_total_arrests <- dataset %>% filter(Ethnicity == temp_ethnicity, 
                                                  Geography == temp_region,
                                                    Time == temp_year)

        # Get the number of NA's in that row
        nas_in_arrests <- temp_total_arrests %>% 
                            filter(is.na(XNumberofarrests)) %>% 
                              nrow
        
        nas_in_pop <- temp_total_arrests %>% 
                        filter(is.na(PopulationbyethnicitygenderandPFAbasedon2011Census)) %>% 
                          nrow
        
        # Get the sum of the number of arrests 
        temp_arrests <- sum(as.numeric(temp_total_arrests$XNumberofarrests), na.rm = TRUE)
        temp_pop <- sum(as.numeric(temp_total_arrests$PopulationbyethnicitygenderandPFAbasedon2011Census), na.rm = TRUE)
        
        # Adding small number based on the sum to replace the NA's
        temp_arrests <- (temp_arrests + ((nas_in_arrests/nrow(temp_total_arrests)) * temp_arrests)) %>% round(digits = 1)
        temp_pop <- (temp_pop + ((nas_in_pop/nrow(temp_total_arrests)) * temp_pop)) %>% round(digits = 1)
        
        #temp_arrests <- ((temp_arrests + ((nas_in_arrests/nrow(temp_total_arrests)) * temp_arrests))/nrow(temp_total_arrests)) %>% round(digits = 1)
        
        # Prepare and bind the row into our local data table 
        temp_row <- data.table(temp_year, temp_region, temp_ethnicity, temp_arrests, temp_pop)
        temp_df <- temp_df %>% list(temp_row) %>% rbindlist(use.names = FALSE)
        
      }
    }
  }
  
  
  # Modifying the col names
  x <- c("Time", "Region", "Ethnicity","Arrests", "Population")
  colnames(temp_df) <- x
  
  # Getting a rate based on the population to avoid the error rate of the different population
  temp_df[, Arrests := round((Arrests/Population) * 100, digits = 1)]
  
  print('Combining ethnicities based on the region. Finish...')
  print("-----------------------------")
  return(temp_df)
}

#---------------------------------------------------------
# APREPROCESSING_ConvertToNumeric()
# 
# Converts the fields that contain only numbers, to a
# numeric field
#
# INPUT:  Data Frame - dataset      - data
#         List       - field_types  - List of field types
#
# OUTPUT: Data Frame - dataset      - data
#---------------------------------------------------------
APREPROCESSING_ConvertToNumeric<-function(dataset, field_types){
  
  # Iterates through each field
  for (field in 1:(ncol(dataset))){
    if(field_types[field] == TYPE_NUMERIC){
  
      dataset[,field]<-as.numeric(dataset[,field])
    }
  }

  return(as.data.table(dataset))
}

#---------------------------------------------------------
# Remove_NonNumericSymbols() :
# 
# Convert non-numeric symbols to 0 in fields that should be numeric
# 
# INPUT:  Data Frame - dataset - dataset
#         List - field_types - field types or the given dataset
# 
# OUTPUT: Data Frame - Updated dataset
#---------------------------------------------------------
APREPROCESSING_RemoveNonNumericSymbols<-function(dataset, field_types){
  for (field in 1:(ncol(dataset))){
    if(field_types[field] == TYPE_NUMERIC){
      dataset[,field]<-as.numeric(dataset[,field])
    }
  }
  dataset[is.na(dataset)]<-0
  
  return(dataset)
}

#---------------------------------------------------------
# merge_datasets(): 
# 
# Merging the 2 given datasets 
#
# INPUT:  Data Frame  -  arrests_dataset      - data 
#         Data Frame  -  unemployment_dataset - data 
#
# OUTPUT: Data Frame  -  merged_dataset       - data
#
#---------------------------------------------------------
merge_datasets <- function(arrests_dataset, unemployment_dataset){
  
  # Print the number of samples after cleaning (arrests_dataset)
  print(paste("Before merging the arrests_dataset nrows:", nrow(arrests_dataset), ", unemployment_dataset, nrows:", nrow(unemployment_dataset)))
  print("-----------------------------")
  
  # Merge the dataset
  merged_dataset <- unemployment_dataset[arrests_dataset, on = .(Ethnicity == Ethnicity, Region == Region, Time == Time), Arrests := Arrests]
  
  # Print the number of samples after cleaning (unemployment_dataset)
  print(paste("After merging the two dataset, nrows:", nrow(merged_dataset)))
  print("-----------------------------")
  
  print(c("Number of fields in merged dataset:",ncol(merged_dataset)))
  print("------------------------------")
  return(merged_dataset)
}

# ************************************************
# Nrescale() :
#
# These are the real values, that we scale between 0-1
# i.e. x-min / (max-min)
#
# INPUT:   vector - input - values to scale
#
# OUTPUT : vector - scaled values to [0.0,1.0]
# ************************************************
Nrescale<-function(input){
  
  clean_input <- na.omit(input)
  
  minv<-min(clean_input)
  maxv<-max(clean_input)
  
  return((input-minv)/(maxv-minv))
}

# ************************************************
# Nrescaleentireframe() :
#
# Rescle the entire dataframe to [0.0,1.0]
#
# INPUT:   data frame - dataset - numeric data frame
#
# OUTPUT : data frame - scaled numeric data frame
# ************************************************
Nrescaleentireframe<-function(dataset){
  
  scaled<-sapply(as.data.frame(dataset),Nrescale)
  return(scaled)
}

# ************************************************
# NPREPROCESSING_removePunctuation()
#
# INPUT: String - fieldName - name of field
#
# OUTPUT : String - name of field with punctuation removed
# ************************************************
NPREPROCESSING_removePunctuation<-function(fieldName){
  return(gsub("[[:punct:][:blank:]]+", "", fieldName))
}


# ************************************************
# NPREPROCESSING_prettyDataset()
# Output simple dataset field analysis results as a table in "Viewer"
#
# REQUIRES: formattable
#
# INPUT: data frame    - dataset, full dataset used for train/test
#                      - Each row is one record, each column in named
#                      - Values are not scaled or encoded
#        String - OPTIONAL string which is used in table as a header
#
# OUTPUT : none
#
# Requires the library: PerformanceAnalytics
#                       formattable
# ************************************************
NPREPROCESSING_prettyDataset<-function(dataset,...){
  
  params <- list(...)
  
  tidyTable<-data.frame(Field=names(dataset),
                        Catagorical=FALSE,
                        Symbols=0,
                        Name=0,
                        Min=0.0,
                        Mean=0.0,
                        Max=0.0,
                        Skew=0.0,
                        stringsAsFactors = FALSE)
  
  if (length(params)>0){
    names(tidyTable)[1]<-params[1]
  }
  
  for (i in 1:ncol(dataset)){
    isFieldAfactor<-!is.numeric(dataset[,i])
    #isFieldAfactor<- !is.na(as.numeric(dataset[,i]))
    tidyTable$Catagorical[i]<-isFieldAfactor
    if (isFieldAfactor){
      tidyTable$Symbols[i]<-length(unique(dataset[,i]))  #Number of symbols in catagorical
      #Gets the count of each unique symbol
      symbolTable<-sapply(unique(dataset[,i]),function(x) length(which(dataset[,i]==x)))
      majoritySymbolPC<-round((sort(symbolTable,decreasing = TRUE)[1]/nrow(dataset))*100,digits=0)
      tidyTable$Name[i]<-paste(names(majoritySymbolPC),"(",majoritySymbolPC,"%)",sep="")
    } else
    {
      #Removes NA values
      datasubset <- subset(dataset[,i], !is.na(dataset[,i]))
      
      tidyTable$Max[i]<-round(max(datasubset),2)
      tidyTable$Mean[i]<-round(mean(datasubset),2)
      tidyTable$Min[i]<-round(min(datasubset),2)
      tidyTable$Skew[i]<-round(PerformanceAnalytics::skewness(datasubset,method="moment"),2)
    }
  }
  
  #Sort table so that all numerics are first
  t<-formattable::formattable(tidyTable[order(tidyTable$Catagorical),],
                              list(Catagorical = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   Symbols = formatter("span",style = x ~ style(color = "black"),x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                   Min = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Min, nsmall=2, big.mark=","))),
                                   Mean = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",format(Mean, nsmall=2, big.mark=","))),
                                   Max = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Max, nsmall=2, big.mark=","))),
                                   Skew = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",sprintf("%.2f", Skew)))
                              ))
  print(t)
}

#---------------------------------------------------------
# NPREPROCESSING_setInitialFieldType() :
#
# Set  each field for NUMERIC or SYNBOLIC
#
# INPUT: String - name - name of the field to manually set
#        String - type - manual type
#
# OUTPUT : None
#---------------------------------------------------------
NPREPROCESSING_setInitialFieldType<-function(name,type){
  
  #Sets in the global environment
  manualTypes<<-rbind(manualTypes,data.frame(name=name,type=type,stringsAsFactors = FALSE))
}

#---------------------------------------------------------
# NPREPROCESSING_initialFieldType() :
#
# Test each field for NUMERIC or SYNBOLIC
#
# INPUT: Data Frame - dataset - data
#
# OUTPUT : Vector - Vector of types {NUMERIC, SYMBOLIC}
#---------------------------------------------------------
NPREPROCESSING_initialFieldType<-function(dataset){
  
  field_types<-vector()
  for(field in 1:(ncol(dataset))){
    
    entry<-which(manualTypes$name==names(dataset)[field])
    if (length(entry)>0){
      field_types[field]<-manualTypes$type[entry]
      next
    }
    
    #Remove punctuation
    dataset_field <- gsub(",", ".", gsub("\\,", "", dataset[,field]))
    dataset_field <- gsub(",", ".", gsub("\\%", "", dataset[,field]))
    dataset_field <- as.numeric(dataset_field)
    
    #if (is.numeric(dataset[,field])) {
    #if (!is.na(as.numeric(dataset[,field]))) {
    
    #Checks if field can be converted to numeric without any NA values
    if(!is.na(dataset_field)){
      field_types[field]<-TYPE_NUMERIC
    }
    else {
      field_types[field]<-TYPE_SYMBOLIC
    }
  } 
  return(field_types)
}

#---------------------------------------------------------
# NPREPROCESSING_discreetNumeric() :
#
# Test NUMERIC field if DISCREET or ORDINAL
#
# INPUT: data frame      - dataset     - input data
#        vector strings  - field_types - Types per field, either {NUMERIC, SYMBOLIC}
#        int             - cutoff      - Number of empty bins needed to determine discreet (1-10)
#
# OUTPUT : vector strings - Updated with types per field {DISCREET, ORDINAL}
#---------------------------------------------------------
# Uses histogram
# Plots histogram for visulisation
#---------------------------------------------------------

NPREPROCESSING_discreetNumeric<-function(dataset,field_types,cutoff){
  
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){
    
    #Only for fields that are all numeric
    if (field_types[field]==TYPE_NUMERIC) {
      
      #Scale the whole field (column) to between 0 and 1
      scaled_column<-Nrescale(dataset[,field])
      
      #Generate the "cutoff" points for each of 10 bins
      #so we will get 0-0.1, 0.1-0.2...0.9-1.0
      cutpoints<-seq(0,1,length=11)
      
      #This creates an empty vector that will hold the counts of ther numbers in the bin range
      bins<-vector()
      
      #Now we count how many numbers fall within the range
      #length(...) is used to count the numbers that fall within the conditional
      for (i in 2:11){
        bins<-append(bins,length(scaled_column[(scaled_column<=cutpoints[i])&(scaled_column>cutpoints[i-1])]))
      }
      
      # the 10 bins will have a % value of the count (i.e. density)
      bins<-(bins/length(scaled_column))*100.0
      
      graphTitle<-"AUTO:"
      
      #If the number of bins with less than 1% of the values is greater than the cutoff
      #then the field is deterimed to be a discreet value
      
      if (length(which(bins<1.0))>cutoff)
        field_types[field]<-TYPE_DISCREET
      else
        field_types[field]<-TYPE_ORDINAL
      
      #Bar chart helps visulisation. Type of field is the chart name
      barplot(bins, main=paste(graphTitle,field_types[field]),
              xlab=names(dataset[field]),
              names.arg = 1:10,bty="n")
      
    } #endif numeric types
  } #endof for
  return(field_types)
}

#---------------------------------------------------------
# NreadDataset() :
#
# Read a CSV file from working directory
#
# INPUT:   string     - csvFilename - CSV filename
#
# OUTPUT : data frame - contents of the headed CSV file

#---------------------------------------------------------
NreadDataset<-function(csvFilename){
  
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)
  
  # The field names "confuse" some of the library algorithms
  # As they do not like spaces, punctuation, etc.
  names(dataset)<-NPREPROCESSING_removePunctuation(names(dataset))
  
  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}

#---------------------------------------------------------
# NPREPROCESSING_outlier() :
#
# Determine if a value of a record is an outlier for each field
#
# INPUT:   data frame - ordinals   - numeric fields only
#          double     - confidence - Confidence above which is determined an outlier [0,1]
#                                  - Set to negative Confidence if NOT remove outliers
#
# OUTPUT : data frame - ordinals with any outlier values replaced with the median of the field
#---------------------------------------------------------
# ChiSquared method
# Uses   library(outliers)
# https://cran.r-project.org/web/packages/outliers/outliers.pdf
#---------------------------------------------------------

NPREPROCESSING_outlier<-function(ordinals,confidence){
  #For every ordinal field in our dataset
  for(field in 1:(ncol(ordinals))){
    
    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    NplotOutliers(sorted,outliers,colnames(ordinals)[field])
    
    #If found records with outlier values
    if ((length(outliers>0))){
      #070819NRT If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence>0){
        outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
        sorted<-unique(sort(outliersGone,decreasing=TRUE))
        #NplotOutliers(sorted,vector(),colnames(ordinals)[field])
        ordinals[,field]<-outliersGone #Put in the values with the outliers replaced by means
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers),"Replaced with MEAN"))
      } else {
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers)))
      }
    }
  }
  return(ordinals)
}

#---------------------------------------------------------
# NplotOutliers() :
#
# Scatter plot of field values and colours outliers in red
#
# INPUT: Vector - sorted    -  points to plot as literal values
#        Vector - outliers  - list of above points that are considered outliers
#        String - fieldName - name of field to plot
#
# OUTPUT : None
#---------------------------------------------------------
NplotOutliers<-function(sorted,outliers,fieldName){
  if(length(sorted)>0){
    options(scipen = 999)
    plot(1:length(sorted),sorted,pch=1,xlab="Unique records",ylab=paste("Sorted values",fieldName),bty="n")
  
  if (length(outliers)>0)
    points(outliers,sorted[outliers],col="red",pch=19)
  }
}

plot_outliers<-function(sorted, outliers,fieldName,confidence){
  gg <- ggplot(mapping = aes(x = 1:length(sorted),y = sorted)) + 
          geom_point(color = 'black') +
            labs(title = paste('Outlier for field:',fieldName), 
                subtitle = paste('Outlier with confidence of:',confidence,'%'), 
                  y = 'Sorted values', 
                    x = 'Unique Records',
                        caption="")
        
}

#---------------------------------------------------------
# NPREPROCESSING_categorical() :
#
# Transform SYMBOLIC or DISCREET fields using 1-hot-encoding
#
# INPUT: data frame    - dataset      - symbolic fields
#        vector string - field_types  - types per field {ORDINAL, SYMBOLIC, DISCREET}
#
# OUTPUT : data frame    - transformed dataset
#
#---------------------------------------------------------
# Small number of literals only otherwise too many dimensions
# Uses 1-hot-encoding if more than 2 unique literals in the field
# Otherwise converts the 2 literals into one field of {0,1}
#---------------------------------------------------------
NPREPROCESSING_categorical<-function(dataset,field_types){
  
  #This is a dataframe of the transformed categorical fields
  catagorical<-data.frame(first=rep(NA,nrow(dataset)),stringsAsFactors=FALSE)
  
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){
    
    #Only for fields marked SYMBOLIC or DISCREET
    if ((field_types[field]==TYPE_SYMBOLIC)||(field_types[field]==TYPE_DISCREET)) {
      
      #Create a list of unique values in the field (each is a literal)
      literals<-as.vector(unique(dataset[,field]))
      numberLiterals<-length(literals)
      
      #if there are just two literals in the field we can convert to 0 and 1
      if (numberLiterals==2){
        transformed<-ifelse (dataset[,field]==literals[1],0.0,1.0)
        catagorical<-cbind(catagorical,transformed)
        colnames(catagorical)[ncol(catagorical)]<-colnames(dataset)[field]
        
      } else
      {
        #We have now to one-hot encoding FOR SMALL NUMBER of literals
        if (numberLiterals<=MAX_LITERALS){
          for(num in 1:numberLiterals){
            nameOfLiteral<-literals[num]
            hotEncoding<-ifelse (dataset[,field]==nameOfLiteral,1.0,0.0)
            
            # 5/3/2018 - do not convert the field if their are too few literals
            # Use log of number of recrods as the measure
            literalsActive<-sum(hotEncoding==1)
            if (literalsActive>log(length(hotEncoding))) {
              catagorical<-cbind(catagorical,hotEncoding)
              #060819 field name has the "_" seperator to make easier to read
              colnames(catagorical)[ncol(catagorical)]<-paste(colnames(dataset)[field],
                                                              "_",
                                                              NPREPROCESSING_removePunctuation(nameOfLiteral),
                                                              sep="")
            }
            else {
              print(paste("Ignoring in field:",names(dataset)[field],
                          "Literal:",nameOfLiteral,
                          "Too few=",literalsActive))
            }
          }
        } else {
          stop(paste("Error - too many literals in:",names(dataset)[field], numberLiterals))
        }
        
      }
    }
  }
  
  return(catagorical[,-1]) #Remove that first column that was full of NA due to R
}