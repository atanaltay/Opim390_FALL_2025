# uncomment next line and execute if package not installed yet
# install.packages("caret")
# install.packages("ROCR")

# load necessary libraries
library(caret)
library(ROCR)

# read in file from working directory
df<- read.csv("highsales_r.csv")

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
df$HighSales <- as.factor(df$HighSales)

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
# for neural network
# Two parameters are: size and decay
# size = number of neurons in the single hidden layer
# decay = regularization parameter to avoid overfitting
# Here we examine size = 5, 10, 15, 20 and decay = 0.1, 0.2, 0.3. 0.4, 0.5
grid <-expand.grid(.size = seq(5,20,5), .decay = seq(0.1,0.5,0.1)) 

# train model via 10-fold cross-validation
# first argument identifies y variable (before the ~) and the set of x variables (after the ~)
# note we are excluding the Store index variable 
# second argument (data) specifies training set 
# third argument (method) specifies prediction method (neural network)
# fourth argument (trControl) specifies training process (cross-validation)
# fifth argument (tuneGrid) specifies values of size and decay to evaluate
# sixth argument (preProcess) specifies that we need to center and scale variables
# seventh argument (linout) specifies the output layer uses a logistic activation function 
nnFit <- train(HighSales ~ . - Store, data = training, method = "nnet",
               trControl = ctrl, tuneGrid = grid, preProcess = c("center","scale"), linout = FALSE)

# identify the neural network parameters (size and decay) that minimize RMSE
nnFit

# generate classifications on testing set using threshold of 0.5
nnPredictClass <- predict(nnFit,newdata = testing)

# generate predicted probabilities on testing set
nnPredictProb <- predict(nnFit,newdata = testing, type = "prob")

# output performance metrics using "Yes" as the positive class
# using 50% probability threshold
confusionMatrix(nnPredictClass, testing$HighSales, positive = "Yes" )

# AUC under ROC curve, Sensitivity, Specificity
tcs <-twoClassSummary(data = data.frame(obs=testing$HighSales, pred = nnPredictClass, 
                                        nnPredictProb, 1 - nnPredictProb ), lev = levels(testing$HighSales))
tcs

# data structure for constructing ROC and lift curves
pred <- prediction(nnPredictProb[,2], testing$HighSales)

# plot AUC 
perAUC <- performance(pred,"tpr", "fpr")
plot(perAUC, main = paste('AUC:', tcs[1]))
abline(0,1)

# plot lift 
perLift <- performance(pred,"lift","rpp")
plot(perLift, main = 'Lift Curve')

# create a data frame with the probability estimates and test set observations 
# write this data frame to a csv file
df_test <- cbind(nnPredictProb, testing$HighSales)
write.csv(df_test, "neuralnetworkprobability_predictions.csv")


