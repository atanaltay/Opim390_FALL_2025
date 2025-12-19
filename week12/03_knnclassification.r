# uncomment next line and execute if package not installed yet
#install.packages("caret")
# install.packages("ROCR")

# load necessary libraries
library(caret)
library(ROCR)

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
colnames(df)[colnames(df)=="df$HighSales"] = "HighSales"

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

# define training set for cross-validation 
training <- df[indxTrain,]

# define test set 
testing <- df[-indxTrain,]

# specify model training process
# method = k-fold cross-validation 
# number = number of folds
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# create a grid of parameter values to assess in k-fold cross-validation
# for k-nn, the critical parameter is k, the number of neighbors
# testing values of k from 1 to 100
grid <-expand.grid(.k=c(1:100))

# train model via 10-fold cross-validation
# first argument identifies y variable (before the ~) and the set of x variables (after the ~)
# note we are excluding the Store index variable and the binary variables for this k-NN 
# second argument (data) specifies training set 
# third argument (method) specifies prediction method (k-NN)
# fourth argument (trControl) specifies training process (cross-validation)
# fifth argument (tuneGrid)  specifies the values of the parameter k to evaluate
# sixth argument (preProcess) specifies that we need to center and scale variables
knnFit <- train(HighSales ~ . - Store - ShelfLocGood - ShelfLocMedium - UrbanYes - USYes, data = training, method = "knn",
                trControl = ctrl, tuneGrid = grid, preProcess = c("center","scale")
                ,metric="ROC") #Sens, Spec

# list output
knnFit
knnFit$bestTune

# plot classification accuracy as a function of k 
plot(knnFit)

# generate classifications on testing set using threshold of 0.5
knnPredictClass <- predict(knnFit,newdata = testing)

# generate predicted probabilities on testing set
knnPredictProb <- predict(knnFit,newdata = testing, type = "prob")

# output performance metrics using "Yes" as the positive class
# using 50% probability threshold
confusionMatrix(knnPredictClass, testing$HighSales, positive = "Yes" )

# AUC under ROC curve, Sensitivity, Specificity
tcs <-twoClassSummary(data = data.frame(obs=testing$HighSales, pred = knnPredictClass, 
                                        knnPredictProb, 1 - knnPredictProb ), 
                      lev = levels(testing$HighSales))
tcs

prSummary(data = data.frame(obs=testing$HighSales, pred = knnPredictClass, 
                            knnPredictProb, 1 - knnPredictProb ), 
          lev = levels(testing$HighSales))



# data structure for constructing ROC and lift curves
pred <- prediction(knnPredictProb[,1], testing$HighSales)

# plot AUC 
perAUC <- performance(pred,"tpr", "fpr")
plot(perAUC, main = paste('AUC:', tcs[1]))
abline(0,1)

# plot lift 
perLift <- performance(pred,"lift","rpp")
plot(perLift, main = 'Lift Curve')

# create a data frame with the probability estimates and test set observations 
# write this data frame to a csv file
df_test <- cbind(knnPredictProb, testing$HighSales)
write.csv(df_test, "knnprobability_predictions.csv")
