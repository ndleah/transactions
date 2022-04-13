## ----setup, echo=FALSE, 	message = FALSE,warning = FALSE-------------------
knitr::opts_chunk$set(center = TRUE)


## ----residuals, echo=FALSE, out.width="40%",fig.cap="Cross-Industry Standard Process for Data Mining (CRISP-DM project, 2000)",fig.align="center"----
knitr::include_graphics("img/CRISP-DM.png")


## ---- message = FALSE, warning = FALSE------------------------------------
##----------------------------------------------------------------
##  Load the Libraries                                          --
##----------------------------------------------------------------
library(here)            # assess the file path
library(DataExplorer)    # EDA visualizations
library(tidyverse)       # data wrangling
library(kableExtra)      # write table
library(bannerCommenter) # create comment banner
library(ggplot2)         # data visualization
library(forecast)        # times-series forecasting
library(ggradar)         # plot seasonal trend
library(sqldf)           # using SQL
library(dplyr)           # data processing
library(ggpubr)          # combine plots into single page
theme_set(theme_pubr())
library(reshape2)        # transpose table
library(fmsb)            # create radar chart
library(modelr)          # computing regression model performance metrics
library(caret)           # streamline the model training process
library(xts)             # convert df to ts object


## ----message=FALSE, center=TRUE-------------------------------------------
##----------------------------------------------------------------
##  Dataset Overview                                            --
##----------------------------------------------------------------
# Load Data File
df <- read_csv(here("dataset/transactions.csv"))


# Quick overview of the data set then transpose for better table display. 
# The information including:
# * index and column data types
# * non-null values
# * memory usage
df_overview <- introduce(df) %>% t()          
                                             

# turn the table into the Markdown format
df_overview %>% kbl() %>% kable_styling(bootstrap_options = "striped", full_width = F)
# rows	94247
# columns	5
# discrete_columns	4
# continuous_columns	1
# all_missing_columns	0
# total_missing_values	0
# complete_rows	94247
# total_observations	471235
# memory_usage	4201744


## -------------------------------------------------------------------------
# inspect columns data type
sapply(df, class) 
# date    customer_id       industry       location monthly_amount 
# "Date"    "character"    "character"    "character"      "numeric" 


## ----fig.align="center", fig.cap="Missing values plot"--------------------
# Check for missing value in each column by plotting
plot_missing(df)


## -------------------------------------------------------------------------
# convert character values of columns to upper case for better missing value inspection
missing_df <- data.frame(
  lapply(df, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))


# check if there is there is missing values assigned under new category
##::::::::::::::::::
##  1. date column  
##::::::::::::::::::
sprintf(paste0("Is there any missing value observation categories in date column (T/F)?: ", 
               missing_df[1] %in% c("NA","N/A","NULL","")))
# "FALSE"


##::::::::::::::::::::::::::
##  2. customer_id column   
##::::::::::::::::::::::::::
sprintf(paste0("Is there any missing value observation categories in customer_id column (T/F)?: ", 
               missing_df[1] %in% c("NA","N/A","NULL","")))
# "FALSE"


# 3. Check for any transaction with zero values
sprintf(paste0("How many rows contained 0 value in monthly transaction amount?: ", 
               sum(df$monthly_amount==0)))
# "FALSE"


## ----fig.align="center", fig.cap="Data distribution", message=FALSE, warning=FALSE----
##---------------------------------------------------------------
##  Data Distribution                                          --
##---------------------------------------------------------------

##:::::::::::::::::::::::::::::::
##  1. Data Skewness/Imbalance   
##:::::::::::::::::::::::::::::::

# combine 2 plots into 1 plot
par(mfrow=c(1,2))
# plot data distribution 
# 1. MONTHLY_AMOUNT group by INDUSTRY
hist(df$industry,
     main = "Trans by Industry", 
     xlab="Industry", 
     xlim = c(0,10), 
     ylim=c(0,50000), 
     las=0)
## 2. MONTHLY_AMOUNT group by LOCATION
hist(df$location,
     main = "Trans by Location", 
     xlab="Location", 
     xlim = c(0,10), 
     ylim=c(0,50000), 
     las=0)


## ----fig.align="center", fig.cap="Boxplot to check for outliers when plotting Monthly Amount against Location & Industry", message=FALSE, warning=FALSE----
##---------------------------------------------------------------
##  Data Distribution                                          --
##---------------------------------------------------------------
##:::::::::::::::::::::::::::::::
##  2. Data Outliers   
##:::::::::::::::::::::::::::::::

# combine 2 plots into 1 plot
par(mfrow=c(1,2)) 
# plot boxplot to check for outliers
# 1. industry
boxplot(monthly_amount~industry, 
        data=df, 
        main='Transaction vs. Industry',
        xlab='Industry', 
        ylab='Transaction Amount', 
        horizontal=TRUE) + 
  scale_fill_grey() + 
  theme_classic()
# 2. location
boxplot(monthly_amount~location, 
        data=df, 
        main='Transaction vs. Location',
        xlab='Location', 
        ylab='Transaction Amount', 
        horizontal=TRUE) + 
  scale_fill_grey() + 
  theme_classic()


## ----fig.align="center", fig.cap="Correlation Plot", message=FALSE, warning=FALSE----
##---------------------------------------------------------------
##  Data Transformation                                        --
##---------------------------------------------------------------
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

########################################################
# DATA VISUAIZATION: CORRELATION PLOT
########################################################
plot_correlation(df)


## ----fig.align="center", fig.cap="Transaction amount vs. transaction number trend over time", message=FALSE, warning=FALSE----
##----------------------------------------------------------------
##  Data Visualization                                          --
##----------------------------------------------------------------

##:::::::::::::::::::::::::::::::::::::::::
##  1. Transaction Amount & # Transaction  
##:::::::::::::::::::::::::::::::::::::::::

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


## ----fig.align="center", fig.cap="Seasonal Trend Over the Years", message=FALSE, warning=FALSE----
##----------------------------------------------------------------
##  Data Visualization                                          --
##----------------------------------------------------------------

##::::::::::::::::::::::::::::::::::::::
##  2. Time-series and Seasonal Trend 
##::::::::::::::::::::::::::::::::::::::

## Create new MONTHLY_TREND df to plot seasonal transaction trend
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

## transpose the dataset to prepare for the data visualization
monthly_trend <- recast(new_ts_df, 
                        year + variable ~ month, 
                        id.var = c("month", "year"))
monthly_trend <- data.frame(monthly_trend[,-1],            # use the first column as the data index
                            row.names = monthly_trend[,1]) # use the first row as the header
monthly_trend <- subset(monthly_trend, select = -variable) # remove the unecessary column


## create new vector specify month column names
colnames(monthly_trend) <- c('January', 'February', 'March', 'April', 
                             'May', 'June', 'July', 'August', 'September', 
                             'October', 'November', 'December')


## To use the fmsb package, I have to add 2 lines to the dataframe: 
## the max and min of each variable to show on the plot!
data <- rbind(rep(1400e6,12) , rep(0,12) , monthly_trend)


## create color vector
colr_1 <- rgb(0.2,0.5,0.5,0.9)
colr_2 <- rgb(0.8,0.2,0.5,0.9)
colr_3 <- rgb(0.7,0.5,0.1,0.9)
colr_4 <- "#FC4E07"


## set color theme for radar border
colors_border=c(colr_1, 
                colr_2, 
                colr_3, 
                colr_4)


## plot with default options:
seasonal_mul_plot <- radarchart(data, axistype=0,
                                #custom polygon
                                pcol=colors_border, plwd=2 , plty=1,
                                #custom the grid
                                cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
                                #custom labels
                                vlcex=0.8, title=paste("Transaction Seasonal Trend"), cex.main = 1
  )

## Add a legend
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


# Individual Years Bar Charts
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



## ----fig.align="center", fig.cap="Transaction amount by Location vs. Industry", message=FALSE, warning=FALSE----
##::::::::::::::::::::::::::::::::::::::::::::::::
##  3. Transaction Amount by Location & Industry  
##::::::::::::::::::::::::::::::::::::::::::::::::

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

# plot transaction info by industry
industry_amount_plot <- ggplot(industry) +
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


## ----message=FALSE, warning=FALSE-----------------------------------------
# write a reusable function
aggregate_transactions <- function(df) {
  
  # aggregate the data, grouping by date, industry and location, 
  # and calculating the mean monthly_amount
  output = df %>%
    group_by(date, industry, location) %>%
    summarize(monthly_amount = mean(monthly_amount, na.rm = TRUE))
  
  # create a column for the month number and another one for year number
  output = output %>%
    # create new column for month number
    mutate(month_number = format(as.Date(date), "%m")) %>%
    # create new column for month number
    mutate(year_number = format(as.Date(date), "%Y"))
  
  # Make sure the new columns are of the correct type
  output$month_number = as.character(output$month_number)
  output$year_number = as.character(output$year_number)
  
  transform(output, month_number = as.integer(month_number), year_number = as.integer(year_number))
  return(output)
}

# create a new variable that store new df with transformed features
aggregated_transactions <- aggregate_transactions(df)
# A tibble: 3,886 x 6
# Groups:   date, industry [470]
# date       industry location monthly_amount month_number year_number
# <date>     <chr>    <chr>             <dbl> <chr>        <chr>      
#   1 2013-01-01 1        1               136081. 01           2013       
# 2 2013-01-01 1        10              188735. 01           2013       
# 3 2013-01-01 1        2               177840. 01           2013       
# 4 2013-01-01 1        3               141632. 01           2013       
# 5 2013-01-01 1        4               221058. 01           2013       
# 6 2013-01-01 1        5               178138. 01           2013       
# 7 2013-01-01 1        6               133400. 01           2013       
# 8 2013-01-01 1        7               231599. 01           2013       
# 9 2013-01-01 1        8               143778. 01           2013       
# 10 2013-01-01 1        9               157416. 01           2013       
# ... with 3,876 more rows


# turn the df into a Markdown table format 
rmarkdown::paged_table(aggregated_transactions)


## -------------------------------------------------------------------------
#################################################################
##                Task 2A.iii - Modelling                      ##
#################################################################
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


## -------------------------------------------------------------------------
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



## ----message=FALSE, warning=FALSE-----------------------------------------
#############################################################################
##  Task 2A.ii - Visualization: Mean Monthly Transaction Amount (L1 & I1) ##
#############################################################################

agg_1_1 %>%
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


## -------------------------------------------------------------------------
# Plot Mean Transaction Amount Bar chart
agg_1_1 %>%
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


## -------------------------------------------------------------------------
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


## -------------------------------------------------------------------------
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


## -------------------------------------------------------------------------
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


## ----message=FALSE, warning=FALSE-----------------------------------------
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
testset_all <- data.frame(test_set_1_1[-7]) %>% 
  # assign value type for the the dataset
  mutate(type="Predicted")


# Rename the predicted dataset column names
colnames(testset_all) <- x


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
testset_all$monthly_amount <- predict(M1_1, test_set_1_1)
pred <- predict(M1_1, dec2016)
dec2016$monthly_amount <- pred

# combine the predicted df with the new predicted row
testset_all <- sqldf(
  "SELECT * FROM testset_all 
  UNION ALL
  SELECT * FROM dec2016"
)


## -------------------------------------------------------------------------
# create new column to specify value type for the dataset
agg_1_1$type <- "Real"


# make sure our variables have the right format for plotting
fit_df <- as.data.frame(fit_df)
testset_all <- as.data.frame(testset_all)


# merge all necessary variables into 1 single df variable with rbind
newData <- rbind(fit_df, testset_all,agg_1_1)


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


## -------------------------------------------------------------------------
#################################################################
##                Task 2B - Describe the model                 ##
#################################################################

##----------------------------------------------------------------
##  Evaluation Metrics                                          --
##----------------------------------------------------------------

# Predictions vs Test data
evaluation_metrics_test_1_1 <- data.frame(
  X = "Predictions vs Test data",
  R2 = summary(M1_1)$r.squared,
  RMSE = modelr::rmse(M1_1, data = test_set_1_1),
  MAE = modelr::mae(M1_1, data = test_set_1_1)
)
# table formatting
rmarkdown::paged_table(evaluation_metrics_test_1_1)


## -------------------------------------------------------------------------
library(modelr)
# Fitted value vs Train data
evaluation_metrics_train_1_1 <- data.frame(
  X = "Fitted value vs Train data",
  R2 = rsquare(M1_1, train_set_1_1),
  RMSE = modelr::rmse(M1_1, train_set_1_1),
  MAE = modelr::mae(M1_1, train_set_1_1)
)
# table formatting
rmarkdown::paged_table(evaluation_metrics_train_1_1)
#                            X        R2     RMSE      MAE
# 1 Fitted value vs Train data 0.8329209 4463.717 3172.824


## -------------------------------------------------------------------------
# Model test error
test_error <- (evaluation_metrics_test_1_1$RMSE)/mean(test_set_1_1$monthly_amount) 
# view the result
test_error # [1] 0.06164264


## ----message=FALSE, warning=FALSE-----------------------------------------
# format the aggregate data columns to make sure they have right formats when performing modelling
aggregated_transactions$industry <- as.numeric(aggregated_transactions$industry)
aggregated_transactions$location <- as.numeric(aggregated_transactions$location)
aggregated_transactions$month_number <- as.numeric(aggregated_transactions$month_number)
aggregated_transactions$year_number <- as.numeric(aggregated_transactions$year_number)

# Using the aggregated dataset from part 2 (aggregated_transactions)
df <- aggregated_transactions

calculate_predictions <- function(df, industries, locations) {
  
  output = data.frame()
  
  # Want to make sure we capture a couple of months from the preceeding year to capture the seasonal data
  testingNum = 9
  
  for (ind in industries) {
    for (loc in locations) {
      
      # Create a subset of the data
      temp = df[df$industry == ind & df$location == loc, ]
      
      # Check to make sure you have at least 'trainingMultiplier' times the number of training rows than testing rows
      if (length(unique(temp$date)) >= testingNum) {
        
        # Arrange dataset by date
        arrange(temp, date)
        
        # Add a number to represent date order
        temp$time_number = c(1:nrow(temp))
        
        # Training number is the number of rows minus the testingNum
        trainingNum = nrow(temp) - testingNum
        
        # Training set is all rows from the start minus the number in the test set. We'll arrange again, just in case.
        trainingSet = head(arrange(temp, time_number), trainingNum)
        
        # Testing set is the last 'testingNum' rows. We'll arrange again, just in case.
        testingSet = tail(arrange(temp, time_number), testingNum)
        
        # Run the model
        training.model = lm(monthly_amount~time_number, data=trainingSet)
        # testing.model = lm(monthly_amount~time_number, data=testingSet)
        
        # Calculate the mean standard error
        training.mse <- mean(residuals(training.model)^2)
        #testing.mse <- mean(residuals(testing.model)^2)
        
        # Calculate root mean squared error
        training.rmse <- sqrt(training.mse)
        # testing.rmse <- sqrt(testing.mse)
        
        ### Now, add an extra row into temp for the December 2016 prediction, giving December 2016 a monthly_amount of 0
        
        # Create a dataframe containing just the December 2016 data
        december_2016 = data.frame(date = "2016-12-01",
                                   industry=ind,
                                   location=loc,
                                   monthly_amount=0,
                                   month_number=12,
                                   year_number=2016,
                                   time_number=(nrow(temp)+1))
        
        # Ensure temp is of type data frame
        temp = as.data.frame(temp)
        #testingSet = as.data.frame(testingSet)
        
        # Add the December 2016 row
        temp = rbind(temp, december_2016)
        # testingSet = rbind(testingSet, december_2016)
        
        # Output a prediction based on all rows and add it to the temp data frame
        temp$prediction = predict(training.model, temp)
        testingSet$prediction = predict(training.model, testingSet)
        
        # Get the last prediction value (which is the Dec 2016 value).
        train_dec_2016_prediction = tail(temp$prediction, 1)
        # test_dec_2016_prediction = tail(testingSet$prediction, 1)
        
        # Create row to add to the output data frame, including industry and location variables
        dataRow = c(ind,loc,training.rmse,train_dec_2016_prediction)
        
      } else {
        # Append entry to output data frame when not enough data to compute
        dataRow = c(ind,loc,NA,NA)
      }
      
      # Add the row to the output dataframe
      output = rbind(output, dataRow)
    }
  }
  
  #Add column names to the output data frame
  colnames(output) <- c("Industry","Location", "RMSE", "Dec 2016 Prediction")
  
  #Return the output
  return(output)
}

# Get the list of unique industries, sorted in numerical order 
industries <- sort(unique(df$industry))
# Get the list of unique locations, sorted in numerical order
locations <- sort(unique(df$location))

# Calculate the predictions for all industry and location pairs
all_predictions <- calculate_predictions(df, industries, locations)

# Order by RMSE
rmarkdown::paged_table(arrange(all_predictions, RMSE))


## ----message=FALSE, include=FALSE-----------------------------------------
################################
# Industry 6 & Location 1
################################
# set up a temporary data set of aggregated values
i6_l1_data <- df[df$industry == 6 & df$location == 1, ]
i6_l1_data$time_number = c(1:nrow(arrange(i6_l1_data, date)))

# arrange the data by time number ot make sure it's worked properly
arrange(i6_l1_data, time_number)

# Retraining the model based on the original data split and month_number of 9
# number of months used to capture seasonality, may need to test out other values. Initially set to 9
month_number = 9
#Training number is the number of rows minus the month_number
trainingNum = nrow(arrange(i6_l1_data, date, time_number)) - month_number

#Training set is all rows from the start minus the number in the test set
trainingSet = head(arrange(i6_l1_data, date, time_number), trainingNum)

# train the model
i6_l1.lm <- lm(monthly_amount~time_number, data=trainingSet)

################################
# Industry 10 & Location 8
################################

# set up a temporary data set of aggregated values
i10_l8_data <- df[df$industry == 10 & df$location == 8, ]
i10_l8_data$time_number = c(1:nrow(arrange(i10_l8_data, date)))
# arrange the data by time number ot make sure it's worked properly
arrange(i10_l8_data, time_number)

# Retraining the model based on the original data split and month_number of 9
# number of months used to capture seasonality, may need to test out other values. Initially set to 9
month_number = 9
#Training number is the number of rows minus the month_number
trainingNum_2 = nrow(arrange(i10_l8_data, date, time_number)) - month_number

#Training set is all rows from the start minus the number in the test set
trainingSet_2 = head(arrange(i10_l8_data, date, time_number), trainingNum)

# train the model
i10_l8.lm <- lm(monthly_amount~time_number, data=trainingSet_2)


## -------------------------------------------------------------------------
# the diagnostic plot 
################################
# Industry 6 & Location 1
################################
par(mfrow=c(2,2))
plot(i6_l1.lm)


## -------------------------------------------------------------------------
################################
# Industry 10 & Location 8
################################
par(mfrow=c(2,2))
plot(i10_l8.lm)


## ----message=FALSE, warning=FALSE-----------------------------------------
# plot the model
i6_l1_model <- ggplot(data = trainingSet, mapping = aes(x = time_number, y =
                                                          monthly_amount)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title="Model 2, Using time_number as only predictor",
       subtitle = "industry 6, location 1",
       x="month number",
       y="Monthly Amount")

i10_l8_model <- ggplot(data = trainingSet_2, mapping = aes(x = time_number, y =
                                                          monthly_amount)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title="Model 2, Using time_number as only predictor",
       subtitle = "industry 10, location 8",
       x="month number",
       y="Monthly Amount")

## combine individual plots into a single page  
ggarrange(i6_l1_model,
          i10_l8_model,
          ncol = 2, nrow = 1)


## ----message=FALSE, warning=FALSE, center=TRUE----------------------------
## load data description csv file
dd <- read_csv(here("dataset/data_description.csv"))

# display the information under table format
dd %>%
  kbl() %>%
  kable_styling(full_width = F)

