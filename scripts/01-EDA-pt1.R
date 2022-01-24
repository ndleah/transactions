########################################################
# DATA UNDERSTANDING: EDA PART 1 - THE DATASET
########################################################
# ------------------------------------------------------
# LOAD THE LIBRARIES
# ------------------------------------------------------
library(here)         # assess the file path
library(DataExplorer) # EDA visualizations
library(tidyverse)    # data wrangling
library(kableExtra)   # write table


# ------------------------------------------------------
# OVERVIEW OF THE DATASET
# ------------------------------------------------------
# Load Data File
df <- read_csv(here("dataset/transactions.csv"))


# Quick overview of the dataset then transpose for better table display:
# * index and column data types
# * non-null values
# * memory usage
df_overview <- introduce(df) %>% t()          
                                             

# turn the table into the Markdown format
df_overview %>% kbl() %>% kable_styling(bootstrap_options = "striped", full_width = F)


# inspect columns data type
sapply(df, class) 

# ------------------------------------------------------
# MISSING VALUE
# ------------------------------------------------------
# Check for missing value in each column by plotting
plot_missing(df)


# convert character values of columns to upper case for better missing value inspection
missing_df <- data.frame(
  lapply(df, function(v) {
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

# ------------------------------------------------------
# DATA DISTRIBUTION: OUTLIERS
# ------------------------------------------------------
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
# VARIABLE CORRELATION
# ------------------------------------------------------
plot_correlation(df)

