#################################################################
##                   MLAA - Assignment AT1a                    ##
##               MLR model on Financial Data set               ##
##                    Author: Leah Nguyen                      ##
#################################################################

#################################################################
##    Task 3A - Modelling - ALL LOCATION & INDUSTRIES          ##
#################################################################

##---------------------------------------------------------------
##  Load the Libraries                                         --
##---------------------------------------------------------------
library(modelr)       # computing regression model performance metrics
library(caret)        # streamline the model training process
library(xts)          # convert df to ts object
library(tidyverse)    # data wrangling
library(dplyr)        # data wrangling
library(hydroGOF)     # Goodness-of-Fit Functions for Comparison

# convert variables to factor
aggregated_transactions$industry <- as.numeric(aggregated_transactions$industry)
aggregated_transactions$location <- as.numeric(aggregated_transactions$location)
aggregated_transactions$month_number <- as.numeric(aggregated_transactions$month_number)
aggregated_transactions$year_number <- as.numeric(aggregated_transactions$year_number)

# Using the aggregated dataset from part 2 (aggregated_transactions)
df <- aggregated_transactions %>% mutate(time_number=c(1:nrow(arrange(aggregated_transactions, date))))
str(df)

calculate_predictions <- function(df, industries, locations) {
  
  output = data.frame()
  
  for (ind in industries) {
    for (loc in locations) {
      
      temp = df[df$industry == ind & df$location == loc, ]}}
      if (length(unique(temp$date)) >= 36) {
        
        # arrange dataset by date
        arrange(temp, date)
        
        # Add a number to represent date order
        temp$time_number = c(1:nrow(temp))
        
        # Split test train
        # Assign observations to training and testing sets
        trainset_all <- train_set
        testset_all <- test_set
        
        # create new vector that specify column names
        x <- c("date", "industry", "location", "monthly_amount",
               "month_number","year_number")
        
        
        # create new df for fitted and predict df
        trainset_all <- data.frame(trainset_all)
        testset_all <- data.frame(testset_all)
        
        
        # Rename the fitted dataset column names
        colnames(trainset_all) <- x
        colnames(testset_all) <- x
        
        
        # Now to run the earlier linear model with monthly_amount as the target variable
        model_all <- lm(monthly_amount ~ time_number, data=trainset_all)
        summary(model_all)
        
        
        # Calculate the mean standard error
        training.mse <- mean(residuals(model_all)^2)
        
        # Calculate root mean squared error
        training.rmse <- sqrt(training.mse)
        
        
        # create new df for prediction row
        december_2016 = data.frame(date = "2016/12/01",
                                   industry=ind,
                                   location=loc,
                                   monthly_amount = 0,
                                   month_number=12,
                                   year_number=2016,
                                   time_number=(nrow(temp)+1)
        )
        
        
        # convert the date column to the right date format to merge with predicted df later
        december_2016$date <- as.Date(december_2016$date,format = "%Y/%m/%d")
        
        # Ensure temp is of type data frame
        temp = as.data.frame(temp)
        
        ## Use predict function + model we just built and apply to dec_2016 data frame
        # Add the December 2016 row
        temp <- rbind(temp, december_2016)
        temp$prediction <- predict(model_all, temp)
        testset_all$prediction = predict(model_all, testset_all)
        
        # Get the last prediction value (which is the Dec 2016 value).
        train_dec_2016_prediction = tail(temp$prediction, 1)
        
        
        # Output a prediction based on all rows and add it to the temp data frame
        temp$prediction = predict(model_all, temp)
        
        # Get the last prediction value (which is the Dec 2016 value).
        train_dec_2016_prediction = tail(temp$prediction, 1)
        
        tempOutput = c(ind, loc, training.mse, training.rmse, train_dec_2016_prediction)
        
      } else {
        
        tempOutput = c(ind, loc, NA, NA, NA)
      }
      
      output = rbind(output, tempOutput)
    }}}

