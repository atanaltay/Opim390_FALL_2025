# uncomment next line and execute if package not installed yet
# install.packages("caret")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("randomForest")

# load necessary libraries
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

# read in file from working directory
df<- read.csv("week13/data/highsales_r.csv")

# investigate variable encoding
str(df)

# encode target variable as a categorical variable
df$HighSales <- factor(df$HighSales,levels = c("Yes","No"))

# compute class proportion table on target variable
prop.table(table(df$HighSales))

# set random number seed for randomized partition
# different seed values will generate different partitions of the data
set.seed(1975)

# use the createDataPartition to randomly select observations to be placed in the training set
# specify the target variable (y)
# set the percentage of data to set aside for training (p)
indxTrain <- createDataPartition(y = df$HighSales, p=0.8, list=FALSE)

# define training set
training <- df[indxTrain,]

# we will use cross-validation, so no need for a stand-alone validation set

# define test set 
testing <- df[-indxTrain,]

# specify model training process
# method = k-fold cross-validation 
# number = number of folds
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# create a grid of parameter values to assess in k-fold cross-validation
# for random forest, a critical parameter is mtry, the number of randomly
# selected candidate variables considered at each split in tree
# testing values of mtry from 1 to 10 (the number of variables)
grid <-expand.grid(.mtry=c(1:10))

# train model via 10-fold cross-validation
# first argument identifies y variable (before the ~) and the set of x variables (after the ~)
# note we are excluding the Store index variable
# second argument (data) specifies training set 
# third argument (method) specifies prediction method (random forest)
# fourth argument (trControl) specifies training process (cross-validation)
# fifth argument (tuneGrid) specifies the values of the parameter mtry to evaluate
# note: this uses a random forest with a default size of 500 trees
classForestFit <- train(HighSales ~ . - Store, data = training, method = "rf", trControl = ctrl, tuneGrid = grid)

# identify value of mtry that maximizes AUC for the ROC curve in the 10-fold cross-validation
classForestFit

# output variable importance metric for random forest
varImp(classForestFit, scale = FALSE, conditional=TRUE)

# plot AUC for ROC curve as a function of mtry 
plot(classForestFit)

# generate classifications on testing set using threshold of 0.5
classForestPredictClass <- predict(classForestFit,newdata = testing)

# generate predicted probabilities on testing set
classForestPredictProb <- predict(classForestFit,newdata = testing, type = "prob")

# output performance metrics using "Yes" as the positive class
# using 50% probability threshold
confusionMatrix(classForestPredictClass, testing$HighSales, positive = "Yes" )

# AUC under ROC curve, Sensitivity, Specificity
tcs <-twoClassSummary(data = data.frame(obs=testing$HighSales, pred = classForestPredictClass, 
                                        classForestPredictProb, 1 - classForestPredictProb ), lev = levels(testing$HighSales))
tcs

# data structure for constructing ROC and lift curves
pred <- prediction(classForestPredictProb[,1], testing$HighSales)

# plot AUC 
perAUC <- performance(pred,"tpr", "fpr")
plot(perAUC, main = paste('AUC:', tcs[1]))
abline(0,1)

# plot lift 
perLift <- performance(pred,"lift","rpp")
plot(perLift, main = 'Lift Curve')

# create a data frame with the probability estimates and test set observations 
# write this data frame to a csv file
df_test <- cbind(classForestPredictProb, testing$HighSales)
write.csv(df_test, "classforestprobability_predictions.csv")
