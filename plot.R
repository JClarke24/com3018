#---------------------------------------------------------
# plot_initial_correlation()
#
# This function will plot the initial correlation on all
# the ethnicities and regions combined.
#
# INPUT: Data Frame - merged_dataset - Merged dataset
#
# OUTPLUT: N/A
#
#---------------------------------------------------------
plot_initial_correlation <- function(merged_dataset){
  
  # Creating a temporary data table
  df_to_plot <- data.table::as.data.table(merged_dataset)
  
  # Getting the max Unemployment and Arrests rate (For graph's axis)
  max_x <- max(df_to_plot[,Arrests], na.rm = TRUE)
  max_y <- max(df_to_plot[,Value], na.rm = TRUE)
  
  
  gg <- ggplot(data = df_to_plot, # Defining the Region we are interested in
               mapping = aes(y = Value, 
                             x = Arrests)) + 
          geom_point(aes(col = Ethnicity), # Drawing the points on the graph
                     size = 3) +
            labs(title = 'Merged Dataset', # Adding labels to the graph
                  subtitle = 'Percentage of unemployment over arrests based on ethnicity in all regions combined', 
                    y = 'Percentage of unemployment', 
                      x = 'Percentage of arrests',
                        fill = 'Ethnicities',
                          caption="") +
              scale_y_continuous(breaks = seq(0, max_y, max_y/4), # Modifying the axis based on our dataset
                                    labels = sprintf("%1.2f%%", seq(0, max_y, max_y/4))) +
                scale_x_continuous(breaks = seq(0, max_x, max_x/4), # Modifying the axis based on our dataset
                                    labels = sprintf("%1.2f%%", seq(0, max_x, max_x/4))) +
                  scale_colour_brewer(palette = "Dark2") + # Change the background to a white colour
                    theme(text = element_text(size = 18))# Colouring the graph
  
  
  plot(gg)
}

#---------------------------------------------------------
# plot_unemployment_dataset()
# 
# This function will plot the general overview about the 
# the unemployment rate of each ethnicity from STARTING YEAR
# to ENDING YEAR
#
# INPUT:  Data Frame - unemployment_dataset - Dataset of arrests
#
# OUTPUT: N/A
#---------------------------------------------------------
plot_unemployment_dataset <- function(unemployment_dataset){
  
  # Creating a temporary data table
  df_to_plot <- data.table::as.data.table(unemployment_dataset)
  
  # Getting the max unemployment rate based on ethnicity
  df_to_plot[,.(Value = max(Value)), by = Ethnicity]
  
  # Getting the max Unemployment rate and Year (For graph's axis)
  max_x <- max(df_to_plot[,Time], na.rm = TRUE)
  max_y <- max(df_to_plot[,Value], na.rm = TRUE)

  
  gg <- ggplot(data = df_to_plot, # Defining the dataset we want to plot
                mapping = aes(fill = Ethnicity, 
                 y = Value, 
                  x = Time)) + 
          geom_bar(position = "dodge", # Drawing the bar on the graph
                    stat = "identity",
                      color = 'black') +
            labs(title = 'Unemployment Dataset', # Adding labels to the graph
                  subtitle = 'Percentage of unemployment based on ethnicity in all regions combined', 
                    y = 'Percentage of unemployment', 
                      x = 'Years',
                        fill = 'Ethnicities',
                          caption="No data available for 2011") +
              geom_text(mapping = aes(label = Value), # Customising the labels
                          vjust = 1.6, 
                            colour = "black",
                              position = position_dodge(0.9), 
                                size = 3.5) +
                scale_y_continuous(breaks = seq(0, max_y, max_y/4), # Modifying the axis based on our dataset
                                    labels = sprintf("%1.2f%%", seq(0, max_y, max_y/4))) +
                  scale_x_continuous(breaks = seq(0, max_x, 1)) +
                    scale_fill_brewer(palette = "Reds")+
                      theme(text = element_text(size = 18)) # Colouring the graph
  

  plot(gg)
}

#---------------------------------------------------------
# plot_arrests_dataset()
# 
# This function will plot the general overview about the 
# the number of arrests of each ethnicity from STARTING YEAR
# to ENDING YEAR
#
# INPUT:  Data Frame - arrests_dataset - Dataset of arrests
#
# OUTPUT: N/A
#---------------------------------------------------------
plot_arrests_dataset <- function(arrests_dataset){
  
  # Creating a temporary data table
  df_to_plot <- data.table::as.data.table(arrests_dataset)
  
  # Getting the max numberofarrests based on ethnicity
  df_to_plot[,.(Arrests = max(Arrests)), by = Ethnicity]

  # Getting the max numberofarrests and Year (For graph's axis)
  max_x <- max(df_to_plot[,Time], na.rm = TRUE)
  max_y <- max(df_to_plot[,Arrests], na.rm = TRUE)
  

  
  gg <- ggplot(data = df_to_plot,  # Defining the Region we are interested in
                 mapping = aes(fill = Ethnicity, 
                      y = Arrests, 
                        x = Time)) + 
          geom_bar(position = "dodge", # Drawing the bar on the graph
                      stat = "identity",
                        color = 'black') +
            labs(title = 'Arrests Dataset', # Adding labels to the graph
                  subtitle = 'Percentage of arrests rate based on ethnicity in all regions combined', 
                    y = 'Percentage of arrests', 
                      x = 'Years',
                        fill = 'Ethnicities') +
              geom_text(mapping = aes(label = Arrests), # Customising the labels
                          vjust = 1.6, 
                            colour = 'black',
                              position = position_dodge(0.9), 
                                size = 3.5) +
                scale_y_continuous(breaks = seq(0, max_y, max_y/4), # Modifying the axis based on our dataset
                                    labels = sprintf("%1.2f%%", seq(0, max_y, max_y/4)))  +
                  scale_x_continuous(breaks=seq(0, max_x, 1)) +
                    scale_fill_brewer(palette = "Greens") +
                      theme(text = element_text(size = 18))# Colouring the graph
  

  plot(gg)
}

#---------------------------------------------------------
# plot_correlation_arrests_unemployment_ethnicity()
# 
# This function will plot the correlation about the 
# the unemployment and arrests rate of each region 
# based on the Ethnicity
#
# INPUT:  Data Frame - arrests_dataset - Dataset of arrests
#
# OUTPUT: N/A
#---------------------------------------------------------
plot_correlation_arrests_unemployment_ethnicity <- function(dataset){
  
  
  # Getting the max numberofarrests and Year (For graph's axis)
  max_y <- max(dataset[,'Value'], na.rm = TRUE)
  max_x <- max(dataset[,'Arrests'], na.rm = TRUE)
  
  # Adding custom palette to the graph
  colourCount = length(unique(dataset[,'Region']))
  getPalette = colorRampPalette(brewer.pal(9, "Dark2"))

  
  gg <- ggplot(data = dataset, 
                mapping = aes(x = Arrests, y = Value)) + 
          geom_point(aes(col = Region), # Drawing the points on the graph
                      size=3) +  
            geom_smooth(method = "lm", col="firebrick", size=2) + # Adding a linear regression on the function to get a general correlation
                labs(title = 'Merged Dataset Correlation', # Adding labels to the graph
                      subtitle = paste('Correlation between unemployment rate and arrests rate for ethnicity:', unique(dataset$Ethnicity)), 
                        x = 'Arrests Rate',
                          y = 'Unemployment Rate', 
                            col = 'Regions',
                              caption="") + 
                scale_y_continuous(breaks=seq(0, max_y, max_y/4), # Modifying the axis based on our dataset
                                    labels = sprintf("%1.2f%%", seq(0, max_y, max_y/4))) +
                  scale_x_continuous(breaks=seq(0, max_x, max_x/4),
                                      labels = sprintf("%1.2f%%", seq(0, max_x, max_x/4))) +
                    scale_colour_manual(values  = getPalette(colourCount)) + # Colouring the graph
                      theme_set(theme_classic()) +
                        theme(text = element_text(size = 18))
                  
  
  plot(gg)
  
}

#---------------------------------------------------------
# A_scatter_plot_error()
# 
# This function will plot the error between the actual and
# predicted value.
#
# INPUT:  Model      - model           - The trained model
#         Data Frame - dataset_train   - Training dataset
#         Data Frame - dataset_valid   - Testing dataset
#         String     - output_name     - Output field
#         String     - predictor_name  - Predictor field
#         Int        - degree          - Degree of the model
#         Bool       -k_fold           - Determines the usage of kfold
#
# OUTPUT: N/A
#---------------------------------------------------------
A_scatter_plot_error<-function(model, dataset_train, dataset_valid, output_name, predictor_name, degree = 1, k_fold = FALSE){
  
  # Getting the max Arrests and Year (For graph's axis)
  max_y <- max(dataset_valid[,'Value'], na.rm = TRUE)
  max_x <- max(dataset_valid[,'Arrests'], na.rm = TRUE)
  
  # Extract predictor (input) values from dataset into a data frame
  x_value <- dataset_valid %>% select(predictor_name) %>% data.frame
  
  # Get predictions from the model using the VALIDATION dataset
  y_predicted <- predict(model, x_value, se.fit = TRUE)
  
  x_value$lci <- y_predicted$fit - 1.96 * y_predicted$se.fit
  x_value$fit <- y_predicted$fit
  x_value$uci <- y_predicted$fit + 1.96 * y_predicted$se.fit

  # Extract the expected response (output) values from VALIDATION dataset
  # into a data frame
  y_actual <- dataset_valid[,output_name]
  

  # Calculate the metrics using functions in lab2functions.R
  RMSE <- round(Nrmse(actual = y_actual, predicted = y_predicted$fit), digits = 2)
  mae <- round(Nmae(actual = y_actual, predicted = y_predicted$fit), digits = 2)
  r2 <- round(Nr2(model), digits = 2)

  
  # Calculate the error (residual) for each row in VALIDATION dataset
  error <- (y_actual - y_predicted$fit)
  
  # Create a data frame, so that we can sort these
  #   the input predictor (x)
  #   the expected value (actual_y)
  #   and the residuals in the model
  results <- data.frame(x_value$Arrests, y_actual, error)
  
  
  
  gg <- ggplot(x_value, aes(x = Arrests, y = fit)) + # Specifying the dataset
          geom_smooth(aes(ymin = lci, ymax = uci), size = 2) + # Adding the regression model on the graph
            geom_point(data = dataset_valid, aes(x = Arrests, y = Value), size = 2)+ # Creating data points on the graph
              geom_segment(aes(x = dataset_valid$Arrests, y = y_actual, xend = dataset_valid$Arrests, yend = y_predicted$fit), # Drawing the line between the regression and data points
                                color = "orangered") +
                scale_x_continuous(breaks=seq(0, max_x, max_x/4),  labels = sprintf("%1.2f%%", seq(0, max_y, max_y/4))) + # Modifying the axis based on our dataset
                  scale_y_continuous(breaks=seq(0, max_y, max_y/4), labels = sprintf("%1.2f%%", seq(0, max_y, max_y/4))) +
                    theme_set(theme_classic()) + # Change the background to a white colour
                      theme(text = element_text(size = 18))
  
  # Will add specific labels based on the parameters that have been provided above
  if(k_fold){
    if(degree == 1){
      gg <- gg + labs(title = paste("Linear Regression Errors For", unique(dataset_valid$Ethnicity), "Ethnicity"), 
                        subtitle = "Plot the best kfold predictions over the actual data set", 
                          y = 'Unemployment Rate',
                            x = 'Arrests Rate', 
                              caption = paste0("MAE =", mae, "| RMSE =", RMSE ,"| R2 =",r2)) 
    }
    else{
      gg <- gg + labs(title = paste("Polynomial Regression ( Degree =",degree,") Errors For", unique(dataset_valid$Ethnicity), "Ethnicity"), 
                        subtitle = "Plot the best kfold predictions over the actual data set", 
                          y = 'Unemployment Rate',
                            x = 'Arrests Rate', 
                              caption = paste0("MAE =", mae, "| RMSE =", RMSE ,"| R2 =",r2)) 
    }
  }
  else{
    if(degree == 1){
      gg <- gg + labs(title = paste("Linear Regression Errors For", unique(dataset_valid$Ethnicity), "Ethnicity"), 
                        subtitle = "Plot the predictions over the actual data set", 
                          y = 'Unemployment Rate',
                            x = 'Arrests Rate', 
                              caption = paste0("MAE =", mae, "| RMSE =", RMSE ,"| R2 =",r2)) 
    }
    else{
      gg <- gg + labs(title = paste("Polynomial Regression ( Degree =",degree,") Errors For", unique(dataset_valid$Ethnicity), "Ethnicity"), 
                        subtitle = "Plot the predictions over the actual data set", 
                          y = 'Unemployment Rate',
                            x = 'Arrests Rate', 
                              caption = paste0("MAE =", mae, "| RMSE =", RMSE ,"| R2 =",r2)) 
    }
  }
  

  plot(gg)
  
}













