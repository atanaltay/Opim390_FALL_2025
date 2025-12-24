# ============================================================
# Model comparison (Decision Tree, Random Forest, XGBoost, KNN, Logistic Regression)
# - 5-fold CV hyperparameter tuning
# - holdout test set evaluation (confusion matrix + ROC/AUC + lift)
# ============================================================

# install.packages(c("caret","rpart","rpart.plot","randomForest","xgboost","glmnet","ROCR"))
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(glmnet)
library(ROCR)

# ---------------------------
# 1) Load and prep data
# ---------------------------
df <- read.csv("week13/data/highsales_r.csv")

str(df)

# Ensure target is a factor with levels c("Yes","No") (caret's twoClassSummary expects first level = "event")
df$HighSales <- factor(df$HighSales, levels = c("Yes","No"))

prop.table(table(df$HighSales))

library(GGally)
library(ggplot2)

# Create the plot matrix colored by HighSales
ggpairs(df[,c(-1)], aes(color = HighSales, alpha = 0.5)) +
  theme_bw() +
  labs(title = "Plot Matrix of Numeric Features Colored by HighSales")


set.seed(1975)
indxTrain <- createDataPartition(y = df$HighSales, p = 0.8, list = FALSE)
training <- df[indxTrain, ]
testing  <- df[-indxTrain, ]

# Common formula (exclude Store ID/index variable)
form <- HighSales ~ . - Store

# ---------------------------
# 2) Cross-validation setup (5-fold CV)
# ---------------------------
ctrl5 <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Utility: evaluate on test set with Confusion Matrix + AUC + Lift
evaluate_model <- function(fit, testing, positive_class = "Yes", model_name = "Model") {
  # class predictions (default threshold = 0.5 if model uses class = max prob)
  pred_class <- predict(fit, newdata = testing)
  
  # probability predictions (caret returns columns named by factor levels)
  pred_prob  <- predict(fit, newdata = testing, type = "prob")
  
  cat("\n============================================================\n")
  cat("TEST EVALUATION:", model_name, "\n")
  cat("Best Tune:\n")
  print(fit$bestTune)
  
  cat("\nConfusion Matrix (positive =", positive_class, ")\n")
  print(confusionMatrix(pred_class, testing$HighSales, positive = positive_class))
  
  # ROCR expects probabilities for the positive class
  pr <- prediction(pred_prob[, positive_class], testing$HighSales)
  
  # AUC
  perf_auc <- performance(pr, "auc")
  auc_val <- as.numeric(perf_auc@y.values[[1]])
  cat("\nTest AUC:", auc_val, "\n")
  
  # ROC curve
  perf_roc <- performance(pr, "tpr", "fpr")
  plot(perf_roc, main = paste0(model_name, " - ROC (AUC = ", round(auc_val, 4), ")"))
  abline(0, 1)
  
  # Lift curve
  perf_lift <- performance(pr, "lift", "rpp")
  plot(perf_lift, main = paste0(model_name, " - Lift Curve"))
  
  # Return a compact summary row
  out <- data.frame(
    Model = model_name,
    ROC_Test = auc_val,
    Accuracy_Test = confusionMatrix(pred_class, testing$HighSales, positive = positive_class)$overall[["Accuracy"]],
    stringsAsFactors = FALSE
  )
  return(out)
}

results_summary <- data.frame(Model=character(), ROC_Test=double(), Accuracy_Test=double(), stringsAsFactors = FALSE)

# ============================================================
# 3) DECISION TREE (rpart) - tune cp
# ============================================================
set.seed(1975)
cp_grid <- expand.grid(cp = seq(0.0005, 0.05, length.out = 40))

tree_fit <- train(
  form,
  data = training,
  method = "rpart",
  metric = "ROC",
  trControl = ctrl5,
  tuneGrid = cp_grid
)

print(tree_fit)
rpart.plot(tree_fit$finalModel)

results_summary <- rbind(results_summary, evaluate_model(tree_fit, testing, model_name = "Decision Tree (rpart)"))

# ============================================================
# 4) RANDOM FOREST (rf) - tune mtry (#features tried at each split)
# ============================================================
# mtry must be between 1 and #predictors
p <- ncol(training) - 2  # minus target (HighSales) and Store
mtry_grid <- expand.grid(mtry = unique(pmax(1, round(seq(1, p, length.out = min(10, p))))))

set.seed(1975)
rf_fit <- train(
  form,
  data = training,
  method = "rf",
  metric = "ROC",
  trControl = ctrl5,
  tuneGrid = mtry_grid,
  ntree = 500,
  importance = TRUE
)

print(rf_fit)
results_summary <- rbind(results_summary, evaluate_model(rf_fit, testing, model_name = "Random Forest (rf)"))

# ============================================================
# 5) XGBOOST (xgbTree) - tune common params
# ============================================================
xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(2, 4, 6),
  eta = c(0.05, 0.1, 0.3),
  gamma = c(0, 1),
  colsample_bytree = c(0.7, 1.0),
  min_child_weight = c(1, 5),
  subsample = c(0.7, 1.0)
)

set.seed(1975)
xgb_fit <- train(
  form,
  data = training,
  method = "xgbTree",
  metric = "ROC",
  trControl = ctrl5,
  tuneGrid = xgb_grid,
  verbose = FALSE
)

print(xgb_fit)
results_summary <- rbind(results_summary, evaluate_model(xgb_fit, testing, model_name = "XGBoost (xgbTree)"))

# ============================================================
# 6) KNN - tune k
# ============================================================
knn_grid <- expand.grid(k = seq(3, 51, by = 2))

set.seed(1975)
knn_fit <- train(
  form,
  data = training,
  method = "knn",
  metric = "ROC",
  trControl = ctrl5,
  tuneGrid = knn_grid,
  preProcess = c("center", "scale")
)

print(knn_fit)
results_summary <- rbind(results_summary, evaluate_model(knn_fit, testing, model_name = "KNN"))

# ============================================================
# 7) LOGISTIC REGRESSION with REGULARIZATION (glmnet) - tune alpha & lambda
# ============================================================
# alpha: 0=ridge, 1=lasso, between=elastic net
glmnet_grid <- expand.grid(
  alpha  = c(0, 0.25, 0.5, 0.75, 1),
  lambda = 10 ^ seq(-4, 1, length.out = 30)
)

set.seed(1975)
logreg_fit <- train(
  form,
  data = training,
  method = "glmnet",
  metric = "ROC",
  trControl = ctrl5,
  tuneGrid = glmnet_grid,
  preProcess = c("center", "scale"),
  family = "binomial"
)

print(logreg_fit)
results_summary <- rbind(results_summary, evaluate_model(logreg_fit, testing, model_name = "Logistic Reg (glmnet)"))

# ---------------------------
# 8) Compare models (CV + Test)
# ---------------------------
cat("\n============================================================\n")
cat("CROSS-VALIDATION RESULTS (caret):\n")
resamps <- resamples(list(
  Tree = tree_fit,
  RF   = rf_fit,
  XGB  = xgb_fit,
  KNN  = knn_fit,
  GLMN = logreg_fit
))
print(summary(resamps, metric = "ROC"))

cat("\n============================================================\n")
cat("TEST SET SUMMARY:\n")
print(results_summary[order(-results_summary$ROC_Test), ])

# Optional: write test-set probability predictions for each model
# (useful for downstream analyses / ensemble)
write_probs <- function(fit, testing, fname) {
  probs <- predict(fit, newdata = testing, type = "prob")
  out <- cbind(probs, Actual = testing$HighSales)
  write.csv(out, fname, row.names = FALSE)
}

write_probs(tree_fit,  testing, "tree_test_probs.csv")
write_probs(rf_fit,    testing, "rf_test_probs.csv")
write_probs(xgb_fit,   testing, "xgb_test_probs.csv")
write_probs(knn_fit,   testing, "knn_test_probs.csv")
write_probs(logreg_fit,testing, "glmnet_test_probs.csv")
