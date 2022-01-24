########################################################
# LOAD THE LIBRARIES
########################################################
library(here)         # assess the file path
library(DataExplorer) # EDA visualizations
library(tidyverse)    # data wrangling
library(kableExtra)   # write table
library(ggplot2)      # data visualization
library(ggradar)      # plot seasonal trend
library(sqldf)        # using SQL
library(car)          # calculate the VIF 
library(dplyr)        # data processing
library(gganimate)    # create animated plots
library(ggpubr)       # combine plots into single page
theme_set(theme_pubr())
library(reshape2)     # transpose table
library(fmsb)         # create radar chart
library(modelr)       # computing regression model performance metrics
library(caret)        # streamline the model training process
library(forecast)     # times-series forecasting
library(xts)          # convert df to ts object


########################################################
# DATA UNDERSTANDING: EDA PART 1 - THE DATASET
########################################################

# ------------------------------------------------------
# OVERVIEW OF THE DATASET
# ------------------------------------------------------
# read the dataset and covert it into a df object
df <- read_csv(here("dataset/transactions.csv"))

# Quick overview of the dataset
df_overview <- 
  # index and column data types, non-null values and memory usage
  introduce(df) %>%                  
  # transpose the info for better display
  t()                                             



# format the table for better display in R markdown
df_overview %>% 
  ## turn output into a table format
  kbl() %>%                                   
  # apply bootstrap theme to the table
  kable_styling(bootstrap_options = "striped",
                full_width = F) 



# inspect columns data type
sapply(df, class) 



# Plot the Quick summary information
plot_missing(df)


# ------------------------------------------------------
# MISSING VALUE
# ------------------------------------------------------
# convert character values of character columns to upper case for better checking
missing_df <- data.frame(lapply(df, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))


# check if there is there is missing values assigned under new category
## 1. date column
sprintf(paste0("Is there any missing value observation categories in date column (T/F)?: ", 
               missing_df[1] %in% c("NA","N/A","NULL","")))


## 2. customer_id column
sprintf(paste0("Is there any missing value observation categories in customer_id column (T/F)?: ", 
               missing_df[1] %in% c("NA","N/A","NULL","")))


# 3. Check for any transaction with zero values
sprintf(paste0("How many rows contained 0 value in monthly transaction amount?: ", 
               sum(df$monthly_amount==0)))


# ------------------------------------------------------
# DATA DISTRIBUTION: DATA SKEWNESS/IMBALANCE
# ------------------------------------------------------

# combine 2 plots into 1 plot
par(mfrow=c(1,2))
# plot data distribution of MONTHLY_AMOUNT group by INDUSTRY
hist(df$industry, # create historgram
     main = "Trans by Industry", 
     xlab="Industry", 
     xlim = c(0,10), 
     ylim=c(0,50000), 
     las=0)

## plot data distribution of MONTHLY_AMOUNT group by LOCATION
hist(df$location, # create histogram
     main = "Trans by Location", 
     xlab="Location", 
     xlim = c(0,10), 
     ylim=c(0,50000), 
     las=0)

# ------------------------------------------------------
# DATA DISTRIBUTION: OUTLIERS
# ------------------------------------------------------
# combine 2 plots into 1 plot
par(mfrow=c(1,2)) 
# plot boxplot to check for outliers
boxplot(monthly_amount~industry, 
        data=df, 
        main='Transaction vs. Industry',
        xlab='Industry', 
        ylab='Transaction Amount', 
        horizontal=TRUE) + 
  scale_fill_grey() + 
  theme_classic()

boxplot(monthly_amount~location, 
        data=df, 
        main='Transaction vs. Location',
        xlab='Location', 
        ylab='Transaction Amount', 
        horizontal=TRUE) + 
  scale_fill_grey() + 
  theme_classic()


# ------------------------------------------------------
# DATA TRANSFORMATION
# ------------------------------------------------------
# convert date column into the date format
df$date <- as.Date(df$date,"%d/%m/%Y")

# convert customer_id column into character format
df$customer_id = as.character(df$customer_id, format = "")

# convert location column into character format
df$location <- as.character(df$location)

# convert industry column into character format
df$industry <- as.character(df$industry)

# filter out value with 0 transaction amount
df<-filter(df, monthly_amount!=0)


# ------------------------------------------------------
# DATA VISUAIZATION: CORRELATION PLOT
# ------------------------------------------------------
plot_correlation(df)


########################################################
# DATA UNDERSTANDING: EDA PART 2 - BUSINESS INSIGHTS
########################################################
# ------------------------------------------------------
# NEW DF: TRANSACTION AMOUNT VS. TRANSACTION NUMBER
# ------------------------------------------------------
# create new df contain total transaction amount
transaction_amount <- sqldf(
"SELECT
  date,
  'Transaction Amount' as type,             -- specify value type
  SUM(monthly_amount) AS value              -- sum total transaction amount
FROM df
GROUP BY date                                -- filter by date
ORDER BY date
"
)



# create new df contain number of transaction
transaction_count <- sqldf(
"SELECT
  date,
    'Transaction Count' as type,             -- specify value type
  COUNT(*) as value                         -- count total number of transactions
FROM df
GROUP BY date                                -- filter by date
ORDER BY date
"
)



# merge 2 df into 1 new TRANSACTION df vertically 
transaction_df <- rbind(transaction_amount, 
                        transaction_count)


# ------------------------------------------------------
# NEW DF: INDUSTRY VS. LOCATION
# ------------------------------------------------------
# create new INDUSTRY df contain total transaction by industry over time
industry <- sqldf(
"SELECT
  date,
  industry,
  SUM(monthly_amount) AS transaction_amount, -- sum total transaction amount
  COUNT(*) as transaction_count              -- count total number of transactions
FROM df
GROUP BY 
  date,                                      -- filter by date
  industry                                   -- filter by industry
ORDER BY date
"
)



# create new LOCATION df contain total transaction by location over time
location <- sqldf(
"SELECT
  date,
  location,
  SUM(monthly_amount) AS transaction_amount, -- sum total transaction amount
  COUNT(*) as transaction_count              -- count total number of transactions
FROM df
GROUP BY 
  date,                                      -- filter by date
  location                                   -- filter by location
ORDER BY date
"
)


# ------------------------------------------------------
# Data Visualization: Transaction amount vs. transaction number
# ------------------------------------------------------
# plot transaction amount over time
monthly_amount_plot <- transaction_df %>%
  # filter by transaction amount only
  filter(type=="Transaction Amount") %>%   
  # assign x and y-axis from the dataset
  ggplot(aes(x = date, y = value/1e6)) +     
  # add the line graph, color, and the size
  geom_line(color = "indianred", size=1.6) + 
  # the relationship graph between x and y
  geom_smooth(formula = y~x,                    
              method = 'loess') +
  labs(x = "Year", 
       y = "Total transaction amount (in millions)",
       title = "Monthly Transaction Amount",
       subtitle = "2013 to 2016") +
  theme_minimal()



# plot total transaction number over time
monthly_count_plot <- transaction_df %>%
  # filter by total transaction number count only
  filter(type=="Transaction Count") %>% 
  # assign x and y-axis from the dataset
  ggplot(aes(x = date, y = value)) +    
  # add the line graph, color, and the size
  geom_line(color = "indianred", size=1.6) +   
  # the relationship graph between x and y
  geom_smooth(formula = y~x,                   
              method = 'loess') +
  labs(x = "Year", 
       y = "Total transaction number",
       title = "Total Transaction Number",
       subtitle = "2013 to 2016") +
  theme_minimal()



## combine individual plots into a single page  
ggarrange(monthly_amount_plot,
          monthly_count_plot,
          ncol = 2, nrow = 1)


# ------------------------------------------------------
# Data Visualization: Time-series and Seasonal Trend
# ------------------------------------------------------
# convert df to ts object
ts_df <- ts(transaction_df[-2], frequency=12, start = c(2013, 1), end=c(2016,11))

# decompose the dataset
decompose_ts_df <- decompose(ts_df)

# plot decompose multicaptive time-series plot
autoplot(decompose_ts_df)


# Create new MONTHLY_TREND df to plot seasonal transaction trend
new_ts_df <- sqldf(
"SELECT
   strftime('%m', date) as month,   --extract month from date column                   
   strftime('%Y',                   --extract year from date column
   date * 3600 * 24,
   'unixepoch') as year,
  SUM(monthly_amount) AS transaction_amount
FROM df
GROUP BY
  month,
  year
ORDER BY
  month,
  year
"
)

# transpose the dataset to prepare for the data visualization
monthly_trend <- recast(new_ts_df, 
                        year + variable ~ month, 
                        id.var = c("month", "year"))
monthly_trend <- data.frame(monthly_trend[,-1],            # use the first column as the data index
                            row.names = monthly_trend[,1]) # use the first row as the header
monthly_trend <- subset(monthly_trend, select = -variable) # remove the unecessary column

# create new vector specify month column names
colnames(monthly_trend) <- c('January', 'February', 'March', 'April', 
                             'May', 'June', 'July', 'August', 'September', 
                             'October', 'November', 'December')
# -------------------------------------------
# PLOT RADAR CHART FOR ALL YEARS
# -------------------------------------------
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(1400e6,12) , rep(0,12) , monthly_trend)
 
# Color vector
colr_1 <- rgb(0.2,0.5,0.5,0.9)
colr_2 <- rgb(0.8,0.2,0.5,0.9)
colr_3 <- rgb(0.7,0.5,0.1,0.9)
colr_4 <- "#FC4E07"

# set color theme for radar border
colors_border=c(colr_1, 
                colr_2, 
                colr_3, 
                colr_4)

# plot with default options:
seasonal_mul_plot <- 
  radarchart(data, axistype=0,
    #custom polygon
    pcol=colors_border, plwd=2 , plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
    #custom labels
    vlcex=0.8, title=paste("Transaction Seasonal Trend"), cex.main = 1
    )
# Add a legend
legend(seasonal_mul_plot, 
       x=1.5, y=0.5, 
       legend = 
         rownames(data[-c(1,2),]), 
       bty = "n", pch=15 ,
       col=colors_border, 
       text.col = "black", 
       cex=1.2, 
       pt.cex=1, 
       title = "Year")

# -------------------------------------------
## BAR CHART FOR INDIVIDUAL YEARS
# -------------------------------------------
ggplot(new_ts_df) +
 aes(x = month, fill = year, weight = transaction_amount/1e6) +
 geom_bar() +
 scale_fill_brewer(palette = "Dark2", 
 direction = -1) +
 labs(y = "Total transaction amount (in millions)", 
      title = "Monthly Seasonal Trend", 
 subtitle = "Individual Year (from 2013 to 2016)") +
 coord_flip() +
 theme_minimal() +
 theme(legend.position = "none", 
 plot.title = element_text(size = 15L)) +
 facet_wrap(vars(year), scales = "free", nrow = 1L)



#######################################################
# DATA VISUALIZATION: Monthly transaction amount by country and location over time
#######################################################
# plot transaction info by industry
industry_amount_plot <- location_amount_plot <- ggplot(industry) +
  aes(x = date, y = transaction_amount, colour = industry) +
  geom_line(size = 1) +
  scale_color_hue(direction = 1) +
  labs(x = "Year", 
       y = "Total transaction amount (in millions)",
       title = "Transaction Amount",
       subtitle = "By industry (from 2013 to 2016)") +
  theme_minimal()



# plot transaction info by location
location_amount_plot <- ggplot(location) +
  aes(x = date, y = transaction_amount, colour = location) +
  geom_line(size = 1) +
  scale_color_hue(direction = 1) +
  labs(x = "Year", 
       y = "Total transaction amount (in millions)",
       title = "Transaction Amount",
       subtitle = "By location (from 2013 to 2016)") +
  theme_minimal()



# combine plots into a single page  
ggarrange(industry_amount_plot, location_amount_plot,
          ncol = 2, nrow = 1) 


#######################################################
# DATA PREPROCESSING: FEATURE ENGINEERING
#######################################################

# We may want to do this again, so write a function
aggregate_transactions <- function(df) {
    
  # Aggregate the data, grouping by date, industry and location, and calculating the mean monthly_amount
  output = df %>%
    group_by(date, industry, location) %>%
    summarize(monthly_amount = mean(monthly_amount, na.rm = TRUE))
  
  # Let's also create a column for the month number and another one for year number
  output = output %>%
    mutate(month_number = format(as.Date(date), "%m")) %>%
    mutate(year_number = format(as.Date(date), "%Y"))
  
  # Make sure the new columns are of the correct type
  output$month_number = as.character(output$month_number)
  output$year_number = as.character(output$year_number)
  
  transform(output, month_number = as.integer(month_number), year_number = as.integer(year_number))
  
  return(output)

}

aggregated_transactions <- aggregate_transactions(df)

rmarkdown::paged_table(aggregated_transactions)


#######################################################
# DATA PREPROCESSING: TRAIN-TEST SPLIT
#######################################################
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

# Create train and test
# use the adjusted data set with new features
split_list <- create_train_test(aggregated_transactions) 

# train set
train_set <- split_list[[1]] 

# test set
test_set <- split_list[[2]]



# create new df filter by only Location 1 & Industry 1
agg_1_1 <- aggregated_transactions %>% 
  filter(industry == 1, location == 1)

# # Create train and test
# use the adjusted data set with new features
split_list_1_1 <- create_train_test(agg_1_1) 

# train set
train_set_1_1 <- split_list_1_1[[1]] 

# test set
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
 labs(x = "Year", y = "Total monthly amount", title = "Train-Test Split", subtitle = "Location 1 & Industry 1") +
 theme_classic() +
 theme(legend.position = "bottom", plot.title = element_text(size = 15L))



#######################################################
# DATA VISUALIZATION: Mean Monthly Transaction Amount (Location 1 & Industry 1)
#######################################################
aggregated_transactions %>%
 filter(industry >= 1L & industry <= 1L) %>%
 filter(location >= 1L & location <= 
 1L) %>%
 ggplot() +
  aes(x = date, y = monthly_amount, colour = year_number) +
  geom_line(size = 0.7) +
  geom_point(size=1) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(
    x = "year",
    y = "Transaction amount",
    title = "Mean Monthly Transaction Amount",
    subtitle = "Industry 1 & Location 1",
    color = "Year"
  ) +
  ggthemes::theme_par() +
  theme(
    plot.title = element_text(size = 12L),
    plot.subtitle = element_text(hjust = 0.5)
  )


#######################################################
# DATA VISUALIZATION: Mean Transaction Amount (Annual Month View - Location 1 & Industry 1)
#######################################################
aggregated_transactions %>%
 filter(industry %in% "1") %>%
 ggplot() +
 aes(x = month_number, fill = year_number, weight = monthly_amount) +
 geom_bar() +
 scale_fill_manual(values = c(`2013` = "#EBEBAE", `2014` = "#BBE395", `2015` = "#379E54", `2016` = "#004529"
 )) +
 labs(x = "Month", y = "Total transaction amount", title = "Transaction Amount by Month", subtitle = "Location 1 & Industry 1", 
 fill = "year") +
 theme_classic() +
 theme(legend.position = "bottom", 
       plot.title = element_text(size = 15L),
       axis.title.x=element_blank(),
       axis.text.x=element_blank(),
       axis.ticks.x=element_blank()) +
 facet_wrap(vars(year_number), nrow = 1L) +
  coord_flip()

################################################################################
# BASIC MODEL FITTING: LOCATION 1 & INDUSTRY 1
################################################################################
################################################################################
# 1. MODEL SELECTION
################################################################################

# --------------------------------------------------------
# M1: 2 VARIABLE - date + month_number
# --------------------------------------------------------
M1_1 <- lm(monthly_amount~date+month_number, data =train_set_1_1)
summary(M1_1)



# --------------------------------------------------------
# M2: 1 VARIABLE - month_number
# --------------------------------------------------------
M1_2 <-lm(monthly_amount~month_number, data =train_set_1_1)
summary(M1_2)



# --------------------------------------------------------
# M3: 1 VARIABLE - date
# --------------------------------------------------------
M1_3 <-lm(monthly_amount~date, data =train_set_1_1)
summary(M1_3)



################################################################################
# 2. MAKING THE PREDICTION
################################################################################
# assign new variable for column names
x <- c("date", "industry", "location", "monthly_amount","month_number","year_number","type")



# create new df for fitted df
fit_df <- data.frame(train_set_1_1[-7]) %>% 
  # assign value type for the the dataset
  mutate(type="Fitted")



# Rename the dataset columns
colnames(fit_df) <- x



# create new df for prediction df
pred_df <- data.frame(test_set_1_1[-7]) %>% 
  # assign value type for the the dataset
  mutate(type="Predicted")
colnames(pred_df) <- x


# create new df for prediction df
dec2016<-data.frame(date = "2016/12/01",
                    industry="1",
                    location="1",
                    monthly_amount = 0,
                    month_number="12",
                    year_number="2016",
                    type="Predicted"
                    )

# convert the date column to the right date format
dec2016$date <- as.Date(dec2016$date,format = "%Y/%m/%d")

## Use predict function + model we just built and apply to dec_2016 dataframe
fit_df$monthly_amount <- predict(M1_1, train_set_1_1)
pred_df$monthly_amount <- predict(M1_1, test_set_1_1)
pred <- predict(M1_1, dec2016)
dec2016$monthly_amount <- pred

# combine the predicted df with the fitting df
pred_df <- sqldf(
  "SELECT * FROM pred_df 
  UNION ALL
  SELECT * FROM dec2016"
)


# assign value type for the the dataset
agg_1_1$type <- "Real"

# covert into df format
fit_df <- as.data.frame(fit_df)
pred_df <- as.data.frame(pred_df)

# To add this to the existing dataset (agg_1_1) we use rbind
newData <- rbind(fit_df, pred_df,agg_1_1)

# Plot fit from MLR
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

# Plot prediction from MLR
ggplot(newData) +
 aes(x = date, y = monthly_amount, colour = type) +
 geom_line(size = 0.7) +
 scale_color_hue(direction = 1) +
 labs(x = "Year", y = "Mean transaction amount", 
      title = "December 2016 Prediction") +
 theme_light() +
 theme(plot.title = element_text(face = "bold"))



################################################################################
# 3. EVALUATION METRICS
################################################################################
# Predictions vs Test data
evaluation_metrics_test_1_1 <- data.frame(
  X = "Predictions vs Test data",
  R2 = rsquare(M1_1, data = test_set_1_1),
  RMSE = rmse(M1_1, data = test_set_1_1),
  MAE = mae(M1_1, data = test_set_1_1)
)
# table formatting
rmarkdown::paged_table(evaluation_metrics_test_1_1)



# Fitted value vs Train data
evaluation_metrics_train_1_1 <- data.frame(
  X = "Fitted value vs Train data",
  R2 = rsquare(M1_1, data = train_set_1_1),
  RMSE = rmse(M1_1, data = train_set_1_1),
  MAE = mae(M1_1, data = train_set_1_1)
)
# table formatting
rmarkdown::paged_table(evaluation_metrics_train_1_1)



# TEST ERROR
test_error <- evaluation_metrics_test_1_1$RMSE/mean(test_set_1_1$monthly_amount) 
test_error


# plot the model
par(mfrow=c(2,2)) 
plot(M1_1)






################################################################################
# ADVANCED MODEL FITTING
################################################################################
################################################################################
# 1. MODELLING
################################################################################
# industries <- sort(unique(aggregated_transactions$industry))
# locations <- sort(unique(aggregated_transactions$location))
# 
# for ind in industries{
#   for loc in locations{
#     temp = aggregated_transactions[aggregated_transactions$industry=ind & aggregated_transactions$location=loc,]
#   
#   }
# }


## -------------------------------------------------------------------------------------------------------------
#evaluation metrics
# Predictions vs Test data
# data.frame(
#   X = " Predictions vs Test data",
#   R2 = rsquare(pred, data = test_set_1_1),
#   RMSE = rmse(pred, data = test_set_1_1),
#   MAE = mae(pred, data = test_set_1_1)
# )


##########################################################
# APPENDIX
##########################################################
# --------------------------------------------------------
# APPENDIX 1
# --------------------------------------------------------
# load data description csv file
dd <- read_csv(here("dataset/data_description.csv"))


# display the information under table format
dd %>%
  kbl() %>%
  kable_styling(full_width = F)

