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
aggregated_transactions$industry <- as.factor(aggregated_transactions$industry)
aggregated_transactions$location <- as.factor(aggregated_transactions$location)
aggregated_transactions$month_number <- as.factor(aggregated_transactions$month_number)
aggregated_transactions$year_number <- as.numeric(aggregated_transactions$year_number)


month_number <- as.factor(12)
year_number <- 2016


# Create a dataframe containing just the December 2016 data
dec_2016 <- data.frame(year_number, month_number)


# initalise objects to hold fitted model and summary performance results
industry <- list()
location <- list()
R_Square <- list()
Adjusted_R_Square <- list()
RMSE_Error <- list()
pred_dec_2016 <- list()


# create a loop to build model that loop through all industries and locations, 
# then calculate the evaluation metrics for each
for(i in 1:10)
{
  for(j in 1:10)
  {
    test <- aggregated_transactions %>% 
      filter(industry==i & location==j)
    
    if(nrow(test) > 15)
    {
      # Row counts to check
      ind = ceiling(nrow(test[test$date < "2016-01-01",]))
      train_ind <- seq_len(ind)
      
      # create train-test split
      trainset <- test[train_ind,]
      testset <- test[-train_ind,]
      
      
      # train the model
      # Model 1: monthly_amount ~ date + month_number
      data.lm1 = lm(formula = monthly_amount ~ month_number, data = trainset)
      
      # Model 2: monthly_amount ~ date + month_number + year_number
      data.lm2 = lm(formula = monthly_amount ~ year_number + month_number, data = trainset)
      
      # # Model 3: monthly_amount ~ date ~ month_number
      # data.lm3 = lm(formula = monthly_amount ~ year_number + month_number, data = trainset)
      
       
      
      # Output prediction based on the test set
      a <- predict(data.lm, testset)
      
      # Get the last prediction value (which is the Dec 2016 value)
      b <- predict(data.lm, dec_2016)
      
      
      # store the prediction output as a list
      test_pred_add1 <- list(a)
      
      # convert the list into a data frame
      test_prediction1 <- as.data.frame(test_pred_add1)
      
      # rename the data frame first column
      names(test_prediction1)[1] <- "Test_Prediction" 
      
      # combine the df with the test data set
      test_actual_pred1 <- cbind(testset , test_prediction1)
      
      # calculate the RMSE for each of the observation
      rmse <- hydroGOF::rmse(test_actual_pred1$Test_Prediction, test_actual_pred1$monthly_amount)
      
      
      # append all results into the predicted df 
      industry <- append(industry , i)
      location <- append(location , j)
    
      # Calculate R Square
      R_Square <- append(R_Square , summary(data.lm)$r.squared)
      
      # Calculate RMSE
      RMSE_Error <- append(RMSE_Error , rmse)
      
      # Calculate Adjusted R Square
      Adjusted_R_Square <- append(Adjusted_R_Square , summary(data.lm)$adj.r.squared)
      
      # Predict the December 2016 transaction amount
      pred_dec_2016 <- append(pred_dec_2016 , b)
    }
    
  }
}

yan <- as.data.frame(do.call(rbind, Map(data.frame, A=industry, B=location , C=R_Square, D=Adjusted_R_Square, E = RMSE_Error, F = pred_dec_2016))) 

# formatting the column names to justify the information for each column
colnames(yan) <- c("industry", "location", "R_Square", "Adjusted_R_Square","RMSE","Dec_2016_Pred")
arrange(yan, Adjusted_R_Square)

