########################################################
# DATA UNDERSTANDING: EDA PART 2 - BUSINESS INSIGHTS
########################################################
# ------------------------------------------------------
# LOAD THE LIBRARIES
# ------------------------------------------------------
library(ggplot2)      # data visualization
library(forecast)     # times-series forecasting
library(ggradar)      # plot seasonal trend
library(sqldf)        # using SQL
library(dplyr)        # data processing
library(ggpubr)       # combine plots into single page
theme_set(theme_pubr())
library(reshape2)     # transpose table
library(fmsb)         # create radar chart


# ------------------------------------------------------
# CREATE NEW DF: TRANSACTION AMOUNT VS. TRANSACTION NUMBER
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
# CREATE NEW DF: INDUSTRY VS. LOCATION
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
# 1. MULTIPLICAPTIVE DECOMPOSITION SEASONAL PLOT
## convert df to ts object
ts_df <- ts(transaction_amount[-2], frequency=6, start = 2013,end = 2016)


## decompose the dataset
decompose_ts_df <- decompose(ts_df)


## plot decompose multicaptive time-series plot
autoplot(decompose_ts_df)+ 
  xlab("Year") + 
  ylab("Transaction Amount") + 
  ggtitle("Classical Multiplicative Decomposition of Transaction Amount")


# 2. SEASONAL RADAR CHART 
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
seasonal_mul_plot <- 
  radarchart(data, axistype=0,
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


# 3. BAR CHART FOR INDIVIDUAL YEARS
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


# -------------------------------------------------------------------------
# DATA VISUALIZATION: Monthly transaction amount by country and location
# -------------------------------------------------------------------------
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

