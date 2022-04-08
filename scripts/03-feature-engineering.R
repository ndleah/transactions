#################################################################
##                   MLAA - Assignment AT1a                    ##
##               MLR model on Financial Data set               ##
##                    Author: Leah Nguyen                      ##
#################################################################

#################################################################
##                Task 2A.i - Aggregated Data set              ##
#################################################################

##---------------------------------------------------------------
##  Load the Libraries                                         --
##---------------------------------------------------------------
library(tidyverse)       # data wrangling
library(kableExtra)      # write table
library(dplyr)           # data processing
library(ggplot2)         # data visualization


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
  output$month_number = as.integer(output$month_number)
  output$year_number = as.integer(output$year_number)
  
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

#############################################################################
##  Task 2A.ii - Visualization: Mean Monthly Transaction Amount (L1 & I1) ##
#############################################################################

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



