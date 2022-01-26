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
library(RcppRoll)     # used to calculate rolling mean
library(broom)        
#### function ####

model_summary <- function(mod){
  mod_summary <- list()
  
  mod_summary$r2 <- summary(mod)$adj.r.squared
  mod_summary$rse <- summary(mod)$sigma
  mod_summary$aug <- mod %>% augment()
  # calculate RMSE
  mod_summary$RMSE <- sqrt(mean(mod_summary$aug$.resid ^ 2))
  
  #inspect RSE, Adj R-squared
  # 
  # sprintf("RMSE: %0.3f", mod_summary$RMSE)
  # sprintf("RSE: %0.4f", mod_summary$rse)
  # sprintf("Adj R-sqr: %0.4f", mod_summary$r2) 
  print(paste0("Adj R-sqr: ", mod_summary$r2))
  print(paste0("RMSE: ", mod_summary$RMSE)) 
  # or RMSE(pred = mod1$fitted.values, obs = mod1$model$monthly_mean)
  print(paste0("RSE: ", mod_summary$rse))
  
  
}

# print RMSE, RSE and AdjR2 for model
# cross validate model with out-of-sample and print average out-of-sample RMSE 
fit_model_cv <- function (df, formula, ind=1, loc=1){
  df_subset <- df %>% filter(industry==ind, location==loc)
  trControl <- trainControl(method = "cv",number = 15, verboseIter = FALSE)
  mod <- train(formula, df_subset, method = "lm", trControl = trControl)
  
  print(formula)
  print("cross validation")
  print(mod$results)
  print("final model")
  model_summary(mod$finalModel)
  return(mod)
}

glimpse(aggregated_transactions)
# create a model per combo, add the model to a list so it can later be used to
# predict out of sample for all combos
combos <- aggregated_transactions %>% select(industry,location) %>% distinct()

aggregated_transactions$industry <- as.factor(aggregated_transactions$industry)
aggregated_transactions$location <- as.factor(aggregated_transactions$location)
aggregated_transactions$month_number <- as.factor(aggregated_transactions$month_number)
aggregated_transactions$year_number <- as.factor(aggregated_transactions$year_number)

# initalise objects to hold fitted model and summary performance results
mods_all <- list()
mods_df <- data_frame()

# loop to fit models for all industry locations 
for (i in 1:nrow(combos)) {
  c <- combos[i,]
  print(c)
  #cross validate the model
  mod <- fit_model_cv(aggregated_transactions, ind = c$industry, loc=c$location, 
                      formula = monthly_amount ~ year_number + month_number)
  
  #add the final model to list of trained models
  mods_all[[i]] <- list(industry = c$industry,
                        location = c$location, 
                        model = mod, 
                        RMSE = RMSE(pred = mod$finalModel$fitted.values,
                                    obs = mod$finalModel$model$.outcome), 
                        AdjR2 = summary(mod$finalModel)$adj.r.squared, 
                        RMSE.cv = mod$results$RMSE, 
                        AdjR2.cv = mod$results$Rsquared)
  
  #add stats to dataframe of all models 
  mods_df <- rbind( mods_df, 
                    data_frame(industry = c$industry,
                               location = c$location,
                               RMSE = RMSE(pred = mod$finalModel$fitted.values,
                                           obs = mod$finalModel$model$.outcome), 
                               AdjR2 = summary(mod$finalModel)$adj.r.squared, 
                               RMSE.cv = mod$results$RMSE, 
                               AdjR2.cv = mod$results$Rsquared, 
                               month_number = factor(12), 
                               year_number = 2016))
  
}



#### predict dec 2016 for all industries and locations ####
mods_df <- mods_df %>% mutate(monthly_amount = NA)

#genereate predictions for all models 
for (i in 1:nrow(mods_df)) {
  #retrieve model from mods list 
  mod <- mods_all[[i]]
  #prep input for prediction 
  input <- mods_df[i,] %>% select(year,month)
  #save prediction back to mods_df monthly_amount coln
  mods_df[i,]$monthly_amount <- predict(mod$model, input)
  
}


#### save all predictions to file ####
# Prepare a data set for all predections
pred_all <- mods_df %>% 
  mutate(date = dmy(paste("01", as.character(month), as.integer(year), sep = "-"))) %>% 
  select(date, industry, location, monthly_amount)


# write to file 
write_csv(pred_all, "./transactions_dec2016.csv")