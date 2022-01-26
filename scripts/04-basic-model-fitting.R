#################################################################
##                   MLAA - Assignment AT1a                    ##
##               MLR model on Financial Data set               ##
##                    Author: Leah Nguyen                      ##
#################################################################

#################################################################
##                Task 2A.iii - Modelling                      ##
#################################################################

##---------------------------------------------------------------
##  Load the Libraries                                         --
##---------------------------------------------------------------
library(modelr)       # computing regression model performance metrics
library(caret)        # streamline the model training process
library(xts)          # convert df to ts object


##---------------------------------------------------------------
##  1. Train-Test Split                                        --
##---------------------------------------------------------------

# create helper function with the creation of train-test split
create_train_test <- function(df){
  # Test set
  # Use data for the year 2016 to test accuracy
  test_df <- df[df$date >=	"2016-01-01",]   
  # Training set
  # Use data for the year 2016 for forecasting
  train_df <- df[(df$date < "2016-01-01"),]
  return (list(train_df, test_df))
}

# Create train and test set
# create new variable and apply train-test split function
split_list <- create_train_test(aggregated_transactions) 

# 1. train set
train_set <- split_list[[1]] 


# 2. test set
test_set <- split_list[[2]]


# create new df filter by only Location 1 & Industry 1
agg_1_1 <- aggregated_transactions %>% 
  filter(industry == 1, location == 1)


# # Create train and test
# use the adjusted data set with new features
split_list_1_1 <- create_train_test(agg_1_1) 

# 1. train set: filter by only Location 1 & Industry 1
train_set_1_1 <- split_list_1_1[[1]] 

# 2. test set: filter by only Location 1 & Industry 1
test_set_1_1 <- split_list_1_1[[2]]

# create new df to plot our test-train set
# add new column to specify the value type
train_set_1_1$type <- "Train"
test_set_1_1$type <- "Test"

# create data frame to display year range of test and train data
train_test <- sqldf(
  "SELECT * FROM train_set_1_1
  UNION ALL
  SELECT * FROM test_set_1_1
  ORDER BY type;
  "
)


# plot year range of train set & test set (L1 & I1)
ggplot(train_test) +
  aes(x = date, y = monthly_amount, colour = type) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(x = "Year", y = "Total monthly amount", 
       title = "Train-Test Split", 
       subtitle = "Location 1 & Industry 1") +
  theme_classic() +
  theme(legend.position = "bottom", plot.title = element_text(size = 15L))


##---------------------------------------------------------------
##  2. Modelling: Basic Model Fitting - L1 & I1                --
##---------------------------------------------------------------

##::::::::::::::::::::::::::::
##  M1: date + month_number
##::::::::::::::::::::::::::::
 
M1_1 <- lm(monthly_amount~date+month_number, data =train_set_1_1)
summary(M1_1)
# Call:
#   lm(formula = monthly_amount ~ date + month_number, data = train_set_1_1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -12943.4  -1553.1    494.6   2858.6  10733.4 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -1.758e+05  5.029e+04  -3.495 0.001954 ** 
#   date            1.981e+01  3.123e+00   6.342 1.80e-06 ***
#   month_number02  1.828e+04  4.561e+03   4.008 0.000551 ***
#   month_number03  2.508e+04  4.563e+03   5.496 1.38e-05 ***
#   month_number04  9.805e+03  4.568e+03   2.146 0.042623 *  
#   month_number05  2.188e+04  4.575e+03   4.783 8.00e-05 ***
#   month_number06  1.448e+04  4.584e+03   3.159 0.004389 ** 
#   month_number07  1.903e+04  4.595e+03   4.142 0.000395 ***
#   month_number08  2.004e+04  4.608e+03   4.349 0.000236 ***
#   month_number09  1.453e+04  4.622e+03   3.144 0.004546 ** 
#   month_number10  2.475e+04  4.639e+03   5.335 2.04e-05 ***
#   month_number11  1.983e+04  4.658e+03   4.258 0.000296 ***
#   month_number12  4.360e+03  4.678e+03   0.932 0.360955    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5584 on 23 degrees of freedom
# Multiple R-squared:  0.8329,	Adjusted R-squared:  0.7457 
# F-statistic: 9.555 on 12 and 23 DF,  p-value: 2.627e-06


##::::::::::::::::::::::::::::
##  M2: month_number
##::::::::::::::::::::::::::::

M1_2 <-lm(monthly_amount~month_number, data =train_set_1_1)
summary(M1_2)
# Call:
#   lm(formula = monthly_amount ~ month_number, data = train_set_1_1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -14715.9  -6444.4    625.6   6639.3  12664.7 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      142568       5233  27.244  < 2e-16 ***
#   month_number02    18895       7401   2.553 0.017455 *  
#   month_number03    26248       7401   3.547 0.001641 ** 
#   month_number04    11588       7401   1.566 0.130484    
# month_number05    24258       7401   3.278 0.003179 ** 
#   month_number06    17471       7401   2.361 0.026703 *  
#   month_number07    22616       7401   3.056 0.005431 ** 
#   month_number08    24239       7401   3.275 0.003200 ** 
#   month_number09    19347       7401   2.614 0.015209 *  
#   month_number10    30155       7401   4.075 0.000436 ***
#   month_number11    25854       7401   3.493 0.001873 ** 
#   month_number12    10976       7401   1.483 0.151073    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 9064 on 24 degrees of freedom
# Multiple R-squared:  0.5407,	Adjusted R-squared:  0.3302 
# F-statistic: 2.569 on 11 and 24 DF,  p-value: 0.02583

##::::::::::::::::::::::::::::
##  M3: 1 VARIABLE - date
##::::::::::::::::::::::::::::

M1_3 <-lm(monthly_amount~date, data =train_set_1_1)
summary(M1_3)
# Call:
#   lm(formula = monthly_amount ~ date, data = train_set_1_1)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -17585  -7236   1484   6569  17030 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.764e+05  7.681e+04  -2.297   0.0279 *  
#   date         2.083e+01  4.729e+00   4.405   0.0001 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 8966 on 34 degrees of freedom
# Multiple R-squared:  0.3633,	Adjusted R-squared:  0.3446 
# F-statistic:  19.4 on 1 and 34 DF,  p-value: 1e-04

# ----------------------------------------------------------------------------
# CONCLUSION: 
# we end up choosing the M1_1 model since its best fitting results


##################################################################
##        Task 2A.iv - Making the Prediction on Dec 2016        ##
##################################################################

##----------------------------------------------------------------
##  Dec 2016 Prediction                                         --
##----------------------------------------------------------------

# create new vector that specify column names
x <- c("date", "industry", "location", "monthly_amount",
       "month_number","year_number","type")


# create new df for fitted df
fit_df <- data.frame(train_set_1_1[-7]) %>% 
  # assign value type for the the dataset
  mutate(type="Fitted")


# Rename the fitted dataset column names
colnames(fit_df) <- x


# create new df for prediction df
pred_df <- data.frame(test_set_1_1[-7]) %>% 
  # assign value type for the the dataset
  mutate(type="Predicted")


# Rename the predicted dataset column names
colnames(pred_df) <- x


# create new df for prediction row
dec2016<-data.frame(date = "2016/12/01",
                    industry="1",
                    location="1",
                    monthly_amount = 0,
                    month_number="12",
                    year_number="2016",
                    type="Predicted"
)


# convert the date column to the right date format to merge with predicted df later
dec2016$date <- as.Date(dec2016$date,format = "%Y/%m/%d")


## Use predict function + model we just built and apply to dec_2016 data frame
fit_df$monthly_amount <- predict(M1_1, train_set_1_1)
pred_df$monthly_amount <- predict(M1_1, test_set_1_1)
pred <- predict(M1_1, dec2016)
dec2016$monthly_amount <- pred

# combine the predicted df with the new predicted row
pred_df <- sqldf(
  "SELECT * FROM pred_df 
  UNION ALL
  SELECT * FROM dec2016"
)

# create new column to specify value type for the dataset
agg_1_1$type <- "Real"


# make sure our variables have the right format for plotting
fit_df <- as.data.frame(fit_df)
pred_df <- as.data.frame(pred_df)


# merge all necessary variables into 1 single df variable with rbind
newData <- rbind(fit_df, pred_df,agg_1_1)


# Plot goodness of the fir from our MLR
newData %>%
  filter(!(year_number %in% "2016")) %>%
  filter(!(type %in% "Predicted")) %>%
  ggplot() +
  aes(x = date, y = monthly_amount, colour = type) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Year",
    y = "Mean Transaction Amount",
    title = "Fit from MLR"
  ) +
  theme_light()


# Plot the comparison between our MLR prediction on test set with the actual values
ggplot(newData) +
  aes(x = date, y = monthly_amount, colour = type) +
  geom_line(size = 0.7) +
  scale_color_hue(direction = 1) +
  labs(x = "Year", y = "Mean transaction amount", 
       title = "December 2016 Prediction") +
  theme_light() +
  theme(plot.title = element_text(face = "bold"))

#################################################################
##                Task 2B - Describe the model                 ##
#################################################################

##----------------------------------------------------------------
##  Evaluation Metrics                                          --
##----------------------------------------------------------------

# Predictions vs Test data
evaluation_metrics_test_1_1 <- data.frame(
  X = "Predictions vs Test data",
  R2 = rsquare(M1_1, data = test_set_1_1),
  RMSE = rmse(M1_1, data = test_set_1_1),
  MAE = mae(M1_1, data = test_set_1_1)
)
# table formatting
rmarkdown::paged_table(evaluation_metrics_test_1_1)
#                          X        R2     RMSE      MAE
# 1 Predictions vs Test data 0.5264429 11293.81 10457.18


# Fitted value vs Train data
evaluation_metrics_train_1_1 <- data.frame(
  X = "Fitted value vs Train data",
  R2 = rsquare(M1_1, data = train_set_1_1),
  RMSE = rmse(M1_1, data = train_set_1_1),
  MAE = mae(M1_1, data = train_set_1_1)
)
# table formatting
rmarkdown::paged_table(evaluation_metrics_train_1_1)
#                            X        R2     RMSE      MAE
# 1 Fitted value vs Train data 0.8329209 4463.717 3172.824


# Model test error
test_error <- evaluation_metrics_test_1_1$RMSE/mean(test_set_1_1$monthly_amount) 
# view the result
test_error # [1] 0.06164264


# plot the model and combine it into 1 single frame
par(mfrow=c(2,2)) 
plot(M1_1)
