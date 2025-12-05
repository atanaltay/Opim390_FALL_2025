# uncomment next line and execute if package not installed yet
# install.packages("caret")
 install.packages("rpart")
 install.packages("rpart.plot")

# load necessary libraries
library(caret)
library(rpart)
library(rpart.plot)
?rpart
# read in file from working directory
df<- read.csv("week10/data/carseatsales_r.csv")
str(df)
# set random number seed for randomized partition
# different seed values will generate different partitions of the data
set.seed(1975)

# use the createDataPartition to randomly select observations to be placed in the training set
# specify the target variable (y)
# set the percentage of data to set aside for training (p)
indxTrain <- createDataPartition(y = df$Sales, p=0.8, list=FALSE)

# define training set for cross-validation
training <- df[indxTrain,]

# define test set 
testing <- df[-indxTrain,]

#plain tree
rpart
regTree = rpart(Sales ~ . - Store,data=training)
regTree
rpart.plot(regTree)
rpart.rules(regTree)
# in order to control the tree we play with "cp" param
# cp-> tree complexity
# It's used to prune the tree and prevent overfitting 
#by controlling how deep or complex the tree can grow.
#At each split, rpart calculates the improvement in the model’s accuracy 
#(i.e., how much the split reduces error). It only keeps the split if the improvement is greater than the cp value.
# smaller the cp -> more splits -> larger tree
#larger cp -> fewer splits -> smaller tree
# default value is cp= 0.01

printcp(regTree)
plotcp(regTree) # rel error -> training error / null model error
# evaluating regression tree performance on testing set
treePredict <- predict(regTree,newdata = testing)

# output performance metrics
postResample(treePredict, testing$Sales)

regTree = rpart(Sales ~ . - Store,data=training,cp=0.05)
rpart.plot(regTree)
treePredict <- predict(regTree,newdata = testing)
postResample(treePredict, testing$Sales)

# So, how do we decide on the best cp value?

# train model via 10-fold cross-validation
# first argument identifies y variable (before the ~) and the set of x variables (after the ~)
# note we are excluding the Store variable as is just an index
# second argument (data) specifies training set 
# third argument (method) specifies prediction method (regression tree)
# fourth argument (trControl) specifies training process (cross-validation)
# fifth argument (tuneLength) specifies number of different values of complexity parameter to evaluate
# the specific values of cp evaluated are selected automatically
# for a tree, the complexity parameter (cp) prunes the tree and prevents overfitting
# specify model training process
# method = k-fold cross-validation 
# number = number of folds
ctrl <- trainControl(method = "cv", number = 10)

regTreeFit <- train(Sales ~ . - Store, data = training, method = "rpart", 
                    trControl = ctrl, tuneLength = 100)

# identify value of cp that minimizes RMSE in the 10-folds cross-validation
regTreeFit$bestTune
regTreeFit
# visualize the tree
rpart.plot(regTreeFit$finalModel)

# list rules of tree
rpart.rules(regTreeFit$finalModel)
# evaluating regression tree performance on testing set
treePredict <- predict(regTreeFit,newdata = testing)

# output performance metrics
postResample(treePredict, testing$Sales)

# create a data frame with the regression tree estimates and test set observations 
# write this data frame to a csv file
df_test <- cbind(treePredict, testing$Sales)
View(df_test)
write.csv(df_test, "regressiontree_predictions.csv")

