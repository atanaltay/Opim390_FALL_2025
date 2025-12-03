# uncomment next line and execute if package not installed yet
install.packages("caret")

# load necessary libraries
library(caret)

# read in file from working directory
df<- read.csv("week9/data/creditscore.csv")

# review encoding of variables
# k-nn requires all variables to be numerically encoded
# categorical variables need to be ignored or transformed to numeric values
str(df)

# set random number seed for randomized partition
# different seed values will generate different partitions of the data
set.seed(1975)

# use the createDataPartition to randomly select observations to be placed in the training set
# specify the target variable (y)
# set the percentage of data to set aside for training (p)
indxTrain <- createDataPartition(y = df$CreditScore, p=0.8, list=FALSE)

# define training set for cross-validation
training <- df[indxTrain,]

# define test set 
testing <- df[-indxTrain,]

# specify model training process
# method = k-fold cross-validation 
# number = number of folds
ctrl <- trainControl(method = "cv", number = 10)

# create a grid of parameter values to assess in k-fold cross-validation
# for k-nn, the critical parameter is k, the number of neighbors
# testing values of k from 1 to 100
grid <-expand.grid(.k=c(1:100))

# train model via 10-fold cross-validation
# first argument identifies y variable (before the ~) and the set of x variables (after the ~)
# we are xcluding the INdividual variable as it is just an index
# second argument (data) specifies training set 
# third argument (method) specifies prediction method (k-NN)
# fourth argument (trControl) specifies training process (cross-validation)
# fifth argument (tuneGrid) specifies the values of the parameter k to evaluate
# sixth argument (preProcess) specifies that we need to center and scale variables
knnFit <- train(CreditScore ~ . - Individual, data = training, method = "knn",
                trControl = ctrl, tuneGrid = grid, preProcess = c("center","scale"))


# identify value of k that minimizes the RMSE in the 10-fold cross-validation
knnFit

# plot RMSE as a function of k 
plot(knnFit)
knnFit$bestTune
# evaluating k-NN performance on testing set
knnPredict <- predict(knnFit,newdata = testing)

# output performance metrics
postResample(knnPredict, testing$CreditScore)

# create a data frame with the k-NN regression estimates and test set observations 
# write this data frame to a csv file
#df_test <- cbind(knnPredict, testing$Sales)
#write.csv(df_test, "knnregression_creditscore_predictions_a.csv")

knnFit2 <- train(CreditScore ~ . - Individual - HomeOwner, data = training, method = "knn",
                trControl = ctrl, tuneGrid = grid, preProcess = c("center","scale"))


# identify value of k that minimizes the RMSE in the 10-fold cross-validation
knnFit2

# plot RMSE as a function of k 
plot(knnFit2)

# evaluating k-NN performance on testing set
knnPredict2 <- predict(knnFit2,newdata = testing)

# output performance metrics
postResample(knnPredict2, testing$CreditScore)

