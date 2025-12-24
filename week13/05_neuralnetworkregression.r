# uncomment next line and execute if package not installed yet
# install.packages("caret")
# install.packages("neuralnet")

# load necessary libraries
library(caret)

# read in file from working directory
df<- read.csv("carseatsales_r.csv")

# review encoding of variables
# input variables need to be numerically coded
str(df)

# create dummy binary variables for all categorical variables
df_dummy<- dummyVars(" ~ .", data = df)
df_dummy <- data.frame(predict(df_dummy, newdata = df))
df <- df_dummy

# confirm variable encoding
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

# specify model training process
# method = k-fold cross-validation 
# number = number of folds
ctrl <- trainControl(method = "cv", number = 10)

# create a grid of parameter values to assess in k-fold cross-validation
# for neural network, two parameters are: size and decay
# size = number of neurons in the single hidden layer
# decay = regularization parameter to avoid overfitting
# Here we examine size = 5, 10, 15, 20 and decay = 0.1, 0.2, 0.3. 0.4, 0.5
grid <-expand.grid(.size = seq(5,20,5), .decay = seq(0.1,0.5,0.1)) 

# train model via 10-fold cross-validation
# first argument identifies y variable (before the ~) and the set of x variables (after the ~)
# note we are excluding the Store index variable 
# and variables serving as base for binary encoded categoricals
# second argument (data) specifies training set 
# third argument (method) specifies prediction method (neural network)
# fourth argument (trControl) specifies training process (cross-validation)
# fifth argument (tuneGrid) specifies values of size and decay to evaluate
# sixth argument (preProcess) specifies that we need to center and scale variables
# seventh argument (linout) specifies the output layer use a linear activation function 
nnFit <- train(Sales ~ . - Store - ShelfLocBad - UrbanNo - USNo, data = training, method = "nnet",
               trControl = ctrl, tuneGrid = grid, preProcess = c("center","scale"), linout = TRUE)

# identify the neural network parameters (size and decay) that minimize RMSE
nnFit

# evaluating neural network performance on testing set
nnPredict <- predict(nnFit,newdata = testing)

# output performance metrics
postResample(nnPredict, testing$Sales)

# create a data frame with the neural network regression estimates and test set observations 
# write this data frame to a csv file
df_test <- cbind(nnPredict, testing$Sales)
write.csv(df_test, "nnregression_predictions.csv")


