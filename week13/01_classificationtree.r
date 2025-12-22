# uncomment next line and execute if package not installed yet
# install.packages("caret")
# install.packages("rpart")
# install.packages("rpart.plot")

# load necessary libraries
library(caret)
library(rpart)
library(rpart.plot)
library(ROCR)

# read in file from working directory
df<- read.csv("week13/data/highsales_r.csv")

# review encoding of variables
str(df)

# encode target variable as a categorical variable
df$HighSales <- factor(df$HighSales,levels=c("Yes","No"))

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

#plain tree
regTree = rpart(HighSales ~ . - Store,data=training)
regTree
rpart.plot(regTree) #nodes show the probability and proportion of observations
rpart.rules(regTree)
regTree$cptable

predictClass <- predict(regTree,newdata = testing, type = "class")
predictClass

# Convert to factor to match the levels of actual outcome if needed
logregPredictClass <- factor(predictClass, levels = levels(testing$HighSales))
logregPredictClass

# output performance metrics using "Yes" as the positive class
# using 50% probability threshold
#sensitivity-> Recall, True Positive Rate
#Specificity -> True Negative Rate
#Pos Pred Value -> Precision
confusionMatrix(predictClass, testing$HighSales, positive = "Yes")


# specify model training process
# method = k-fold cross-validation 
# number = number of folds
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# train model via 10-fold cross-validation
# first argument identifies y variable (before the ~) and the set of x variables (after the ~)
# note we are excluding the Store index variable 
# second argument (data) specifies training set 
# third argument (method) specifies prediction method (classification tree)
# fourth argument (trControl) specifies training process (cross-validation)
# fifth argument (tuneLength) specifies number of different values of complexity parameter to evaluate
# the specific values of cp evaluated are selected automatically
classTreeFit <- train(HighSales ~ . - Store, data = training, method = "rpart", 
                    trControl = ctrl, tuneLength = 200)

# list output
classTreeFit

# visualize the tree
rpart.plot(classTreeFit$finalModel)
?rpart.plot
# list rules of tree
rpart.rules(classTreeFit$finalModel)

classTreeFit$bestTune

# generate classifications on testing set using threshold of 0.5
classTreePredictClass <- predict(classTreeFit,newdata = testing)

# generate predicted probabilities on testing set
classTreePredictProb <- predict(classTreeFit,newdata = testing, type = "prob")

# output performance metrics using "Yes" as the positive class
# using 50% probability threshold
confusionMatrix(classTreePredictClass, testing$HighSales, positive = "Yes" )

# AUC under ROC curve, Sensitivity, Specificity
tcs <-twoClassSummary(data = data.frame(obs=testing$HighSales, pred = classTreePredictClass, 
                                        classTreePredictProb, 1 - classTreePredictProb ), lev = levels(testing$HighSales))
tcs

# data structure for constructing ROC and lift curves
pred <- prediction(classTreePredictProb[,1], testing$HighSales)

# plot AUC 
perAUC <- performance(pred,"tpr", "fpr")
plot(perAUC, main = paste('AUC:', tcs[1]))
abline(0,1)

# plot lift 
perLift <- performance(pred,"lift","rpp")
plot(perLift, main = 'Lift Curve')

# create a data frame with the probability estimates and test set observations 
# write this data frame to a csv file
df_test <- cbind(classTreePredictProb, testing$HighSales)
write.csv(df_test, "classtreeprobability_predictions.csv")

