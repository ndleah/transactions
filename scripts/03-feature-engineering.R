#######################################################
# DATA PREPARATION: FEATURE ENGINEERING
#######################################################
# write a reusable function
aggregate_transactions <- function(df) {
  
  # aggregate the data, grouping by date, industry and location, and calculating the mean monthly_amount
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

# turn the df into a Markdown table format 
rmarkdown::paged_table(aggregated_transactions)
