###########################################################
# MODELLING: BASIC MODEL FITTING - LOCATION 1 & INDUSTRY 1
###########################################################
# ----------------------------------------------------
# LOAD THE LIBRARIES
# ----------------------------------------------------
library(modelr)       # computing regression model performance metrics
library(caret)        # streamline the model training process
library(xts)          # convert df to ts object

# ------------------------------------------------------------------------------
# DATA VISUALIZATION: Mean Monthly Transaction Amount (Location 1 & Industry 1)
# ------------------------------------------------------------------------------
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


# -----------------------------------------------------------------------------
# DATA VISUALIZATION: Mean Transaction Amount
# -----------------------------------------------------------------------------
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


# --------------------------------------------------------
# 1. MODEL SELECTION
# --------------------------------------------------------
# M1: date + month_number
M1_1 <- lm(monthly_amount~date+month_number, data =train_set_1_1)
summary(M1_1)


# M2: month_number
M1_2 <-lm(monthly_amount~month_number, data =train_set_1_1)
summary(M1_2)


# M3: 1 VARIABLE - date
M1_3 <-lm(monthly_amount~date, data =train_set_1_1)
summary(M1_3)

# CONCLUSION: we end up choosing the M1_1 model since its best fitting results

# --------------------------------------------------------
# 2. MAKING THE PREDICTION
# --------------------------------------------------------
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


# ------------------------------------------------------------------------------
# 3. EVALUATION METRICS
# ------------------------------------------------------------------------------
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


# Model test error
test_error <- evaluation_metrics_test_1_1$RMSE/mean(test_set_1_1$monthly_amount) 
# view the result
test_error


# plot the model and combine it into 1 single frame
par(mfrow=c(2,2)) 
plot(M1_1)
