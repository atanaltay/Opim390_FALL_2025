# Load libraries
install.packages("xgboost")
library(caret)
library(rpart)
library(randomForest)
library(xgboost)
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the data
df <- read.csv("week10/data/hitters.csv")
str(df)
df[is.na(df$Salary),]

df = df[!is.na(df$Salary),]
# Train-test split
set.seed(1975)
indxTrain <- createDataPartition(y = df$Salary, p = 0.8, list = FALSE)
training <- df[indxTrain, ]
testing <- df[-indxTrain, ]

# Cross-validation setup
ctrl <- trainControl(method = "cv", number = 10)

# Define tuning grids
tree_grid <- expand.grid(cp = seq(0.001, 0.05, by = 0.01))
rf_grid <- expand.grid(mtry = 2:5)

#Parameter	Meaning	Typical Values for XGB
#nrounds:	Number of boosting iterations (trees)	100
#eta:	Learning rate — how much each tree contributes	0.01 to 0.3
#max_depth:	Maximum depth of a tree (controls model complexity)	3 to 10
#gamma:	Minimum loss reduction required to make a split (regularization)	0 to 5
#colsample_bytree:	Fraction of features to consider at each tree	0.5 to 1
#min_child_weight:	Minimum sum of instance weight (hessian) in a child — controls overfitting	1 to 10
#subsample:	Fraction of training instances to use per tree	0.5 to 1

xgb_grid <- expand.grid(
  nrounds = 100,
  max_depth = c(3, 5),
  eta = c(0.05, 0.1),
  gamma = c(0,1,2,3,4,5),
  colsample_bytree = 0.5,
  min_child_weight = 1,
  subsample = 1
)

# Model configs
model_list <- list(
  list(name = "Decision Tree", method = "rpart", tuneGrid = tree_grid),
  list(name = "Random Forest", method = "rf", tuneGrid = rf_grid),
  list(name = "XGBoost", method = "xgbTree", tuneGrid = xgb_grid)
)

# Results storage
results <- data.frame(
  Model = character(),
  CV_RMSE = numeric(),
  Test_RMSE = numeric(),
  Best_Params = character(),
  stringsAsFactors = FALSE
)
#To store the actua models
model_fits <- list()   # <-- store fit objects with their names


# Training loop
set.seed(123)
for (model_cfg in model_list) {
  cat("\n📦 Training:", model_cfg$name, "\n")
  
  fit <- train(
    Salary ~ .,
    data = training,
    method = model_cfg$method,
    trControl = ctrl,
    tuneGrid = model_cfg$tuneGrid
  )
  # Store the fitted model, named by model_cfg$name
  model_fits[[model_cfg$name]] <- fit
  
  # Predictions and RMSEs
  preds <- predict(fit, newdata = testing)
  cv_rmse <- min(fit$results$RMSE)
  test_rmse <- postResample(preds, testing$Salary)[["RMSE"]]
  best_params <- paste0(names(fit$bestTune), "=", unlist(fit$bestTune), collapse = ", ")
  
  # Print best tuning params
  cat("✅ Best Parameters: ", best_params, "\n")
  
  # Store results
  results <- rbind(results, data.frame(
    Model = model_cfg$name,
    CV_RMSE = round(cv_rmse, 3),
    Test_RMSE = round(test_rmse, 3),
    Best_Params = best_params
  ))
}

# Final summary
cat("\n📊 Summary of Model Performance:\n")
print(results)

# Plot RMSEs
results_long <- pivot_longer(results, cols = c(CV_RMSE, Test_RMSE), names_to = "Error_Type", values_to = "RMSE")
ggplot(results_long, aes(x = Model, y = RMSE, fill = Error_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cross-Validation and Test RMSE by Model", y = "RMSE") +
  theme_minimal()

# Best model by CV RMSE
best_model <- results %>% filter(CV_RMSE == min(CV_RMSE))
cat("\n🏆 Best model based on cross-validation RMSE:\n")
print(best_model)

names(model_fits)

#variable importances:

rf_imp = varImp(model_fits$XGBoost,scale=FALSE)
rf_imp$importance
imp_df <- data.frame(
  Variable = rownames(rf_imp$importance),
  Importance = rf_imp$importance$Overall
)

ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Variable Importance (Random Forest)",
       x = "Variable",
       y = "Importance") +
  theme_minimal()



#trying a linear model:
# Cross-validated OLS regression
lmFit <- train(
  Salary ~ ., 
  data = training,
  method = "lm",
  trControl = ctrl,
  preProcess = c("center","scale")
)

# View model summary
summary(lmFit)

# evaluating linear model performance on testing set
lmPredict <- predict(lmFit,newdata = testing)

# output performance metrics
postResample(lmPredict, testing$Salary)
