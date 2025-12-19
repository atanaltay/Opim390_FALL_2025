# uncomment next line and execute if package not installed yet
# install.packages("caret")
# install.packages("glmnet")
# install.packages("glm")
install.packages("ROCR")
install.packages("MLmetrics")

# load necessary libraries
library(caret)
library(glmnet)
library(ROCR) #Contains ROC curve calculation functions
library("MLmetrics") #contains Accuracy(), Recall(), F1_Score, Precision(), AUC()
vignette("ROCR")
# read in file from working directory
df<- read.csv("week12/data/highsales_r.csv")

# review encoding of variables
# input variables need to be numerically encoded
str(df)

# create dummy binary variables for all categorical variables
df_dummy<- dummyVars("HighSales~ .", data = df, fullRank = TRUE)
df_dummy <- data.frame(predict(df_dummy, newdata = df))
df_dummy <- cbind(df_dummy, df$HighSales)
df <- df_dummy
colnames(df)
colnames(df)[colnames(df)=="df$HighSales"] = "HighSales"

# encode target variable as a categorical variable
df$HighSales <- factor(df$HighSales,levels = c("Yes","No"))

# compute class proportion table on target variable
prop.table(table(df$HighSales))

#########Data Exploration##############
# Load necessary packages
#install.packages("GGally")       # if not already installed
library(GGally)
library(ggplot2)

# Create the plot matrix colored by HighSales
ggpairs(df[,c(-1)], aes(color = HighSales, alpha = 0.5)) +
  theme_bw() +
  labs(title = "Plot Matrix of Numeric Features Colored by HighSales")

#######################

# set random number seed for randomized partition
# different seed values will generate different partitions of the data
set.seed(1975)

# use the createDataPartition to randomly select observations to be placed in the training set
# specify the target variable (y)
# set the percentage of data to set aside for training (p)
#when there is class imbalance, we need to sample in a stratified manner.
indxTrain <- createDataPartition(y = df$HighSales,
                                 p = 0.8,
                                 list = FALSE)

# define training set for cross-validation
training <- df[indxTrain,]

# define test set 
testing <- df[-indxTrain,]

# compute class proportion table on target variable
prop.table(table(training$HighSales))

prop.table(table(testing$HighSales))

?glm
# Let's fit logistic regression model (use all other variables as predictors)
model <- glm(HighSales ~ . - Store, data = training, family = binomial)
# View model summary
summary(model)

#also glmnet() function supports multinomial log reg.

#Another package for multinomial Log. Reg:
# install.packages("VGAM")    # if needed
#library(VGAM)
#model <- vglm(HighSales ~ . - Store, 
#              multinomial, 
#              data = training)
#summary(model)


# generate predicted probabilities on testing set
logregPredictProb <- predict(model,newdata = testing, type = "response")
logregPredictProb

# Classify based on 0.5 threshold
logregPredictClass <- ifelse(logregPredictProb >= 0.5, "Yes", "No")

# Convert to factor to match the levels of actual outcome if needed
logregPredictClass <- factor(logregPredictClass, levels = levels(testing$HighSales))
logregPredictClass

# output performance metrics using "Yes" as the positive class
# using 50% probability threshold
#sensitivity-> Recall, True Positive Rate
#Specificity -> True Negative Rate
#Pos Pred Value -> Precision
confusionMatrix(logregPredictClass, testing$HighSales, positive = "Yes")

#Metrics from MLmetrics package
Accuracy(logregPredictClass, testing$HighSales)
Recall(logregPredictClass, testing$HighSales)
Precision(logregPredictClass, testing$HighSales)

# create a grid of parameter values to assess in k-fold cross-validation
# for regularized logistic regression, two parameters: alpha and lambda
# alpha parameter controls the mix between ridge regularization and lasso regularization
# alpha = 0 -> ridge, alpha = 1 -> lasso
# lambda corresponds to the regularization penalty that dampens the size of the coefficients
grid <-expand.grid(.alpha=c(0,0.5,1), .lambda = 10^seq(-2, 1000, length = 100))

# specify model training process
# method = k-fold cross-validation 
# number = number of folds
#twoClassSummary considers:
#ROC (AUC)
#Sensitivity (Recall)
#Specificity (True Neg. Rate)
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# train model via 10-fold cross-validation
# first argument identifies y variable (before the ~) and the set of x variables (after the ~)
# note we are excluding the Store index variable 
# second argument (data) specifies training set 
# third argument (method) specifies prediction method (regularized logistic regression)
# fourth argument (trControl) specifies training process (cross-validation)
# fifth argument (preProcess) specifies that we need to center and scale variables
# sixth argument (tuneLength) specifies number of different combinations of the two parameters
# alpha and lambda to evaluate, the specific values of alpha and lambda are selected automatically
# alpha parameter controls the mix between ridge regularization and lasso regularization
# alpha = 0 -> ridge, alpha = 1 -> lasso
# lambda corresponds to the regularization penalty that dampens the size of the coefficients
#As the target class is imbalanced here we want to maximize AUC
logregFit <- train(HighSales ~ . - Store, data = training, method = "glmnet",
                trControl = ctrl, tuneGrid = grid, preProcess = c("center","scale"),
                metric="ROC") # also Sens and Spec

#in order to maximize accuracy:
# ctrl_acc <- trainControl(
#   method = "cv",
#   number = 10
#   # classProbs not required for Accuracy
#   # summaryFunction defaults to defaultSummary (returns Accuracy, Kappa)
# )
# 
# logregFit_acc <- train(
#   HighSales ~ . - Store,
#   data = training,
#   method = "glmnet",
#   trControl = ctrl_acc,
#   tuneGrid = grid,                 # must include alpha, lambda
#   preProcess = c("center","scale"),
#   metric = "Accuracy"
# )

# if you want to maximize precision or recall:
# ctrl_precision <- trainControl(
#   method = "cv",
#   number = 10,
#   classProbs = TRUE,
#   summaryFunction = prSummary
# )
# 
# logregFit_precision <- train(
#   HighSales ~ . - Store,
#   data = training,
#   method = "glmnet",
#   trControl = ctrl_precision,
#   tuneGrid = grid,
#   preProcess = c("center","scale"),
#   metric = "Precision"
# )



# identify values of alpha and lambda that maximize AUC for ROC curve in the 10-fold cross-validation
logregFit
logregFit$bestTune

# output the regularized logistic regression coefficients
coef(logregFit$finalModel, logregFit$bestTune$lambda)

# generate predicted probabilities on testing set
logregPredictProb <- predict(logregFit,newdata = testing, type = "prob")
# generate classifications on testing set using threshold of 0.5
logregPredictClass <- factor(ifelse(logregPredictProb$Yes >= 0.5, "Yes", "No"),
                          levels = c("Yes","No"))

#logregPredictTest = predict(logregFit,newdata = testing, type = "raw") -> returns "Yes" or "No"
#logregPredictTest

#output performance metrics using "Yes" as the positive class
# using 50% probability threshold
confusionMatrix(logregPredictClass, testing$HighSales, positive = "Yes")
#confusionMatrix(logregPredictClass, testing$HighSales, positive = "Yes",mode = "prec_recall" )
#sensitivity -> Recall, Pos Pred Value -> precision

# AUC under ROC curve, Sensitivity, Specificity
tcs <-twoClassSummary(data = data.frame(obs=testing$HighSales, pred = logregPredictClass, 
                                        logregPredictProb, 1 - logregPredictProb ), 
                      lev = levels(testing$HighSales))
tcs

#You may also use prSummary for calculating Precision, Recall, ...
prSummary(data = data.frame(obs=testing$HighSales, pred = logregPredictClass, 
                            logregPredictProb, 1 - logregPredictProb ), 
          lev = levels(testing$HighSales))
#F measure is the harmonic mean of precision and recall

logregPredictProb
# data structure for constructing ROC and lift curves
pred <- prediction(logregPredictProb[,1], testing$HighSales)

# plot AUC 
perAUC <- performance(pred,"tpr", "fpr")
plot(perAUC, main = paste('AUC:', tcs[1]))
abline(0,1)

# plot lift 
perLift <- performance(pred,"lift","rpp") #rpp-> rate of positive predictions
plot(perLift, main = 'Lift Curve')

# create a data frame with the probability estimates and test set observations 
# write this data frame to a csv file
#df_test <- cbind(logregPredictProb, testing$HighSales)
#write.csv(df_test, "logregprobability_predictions.csv")



