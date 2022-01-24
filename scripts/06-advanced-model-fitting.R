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
