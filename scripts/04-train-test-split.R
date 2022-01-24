#######################################################
# DATA PREPARATION: TRAIN-TEST SPLIT
#######################################################
# ----------------------------------------------------
# 1. ADVANCED MODEL FITTTING
# ----------------------------------------------------
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

# ----------------------------------------------------
# 2. BASIC MODEL FITTTING
# ----------------------------------------------------
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
  labs(x = "Year", y = "Total monthly amount", title = "Train-Test Split", subtitle = "Location 1 & Industry 1") +
  theme_classic() +
  theme(legend.position = "bottom", plot.title = element_text(size = 15L))
