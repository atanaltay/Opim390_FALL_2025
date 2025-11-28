# uncomment next line and execute if package not installed yet
install.packages("caret")

# load necessary libraries
library(caret)

# read in file from working 
df<- read.csv("week9/data/carseatsales_r.csv")
str(df)


#Creating dummy variables for categorical variables from Caret package
# using dummyVars() function from library Caret
#dummy_transformer = dummyVars("~.",data=df,fullRank = TRUE)
#then we need to call predict to transform the dummies
#df_dummy =  data.frame(predict(dummy_transformer,newdata = df))
#df_dummy

# set random number seed for randomized partition
# different seed values will generate different partitions of the data

set.seed(1975)

# use the createDataPartition to randomly select observations to be placed in the training set
# specify the target variable (y)
# set the percentage of data to set aside for training (p)
indxTrain <- createDataPartition(y = df$Sales, p=0.8, list=FALSE)
indxTrain

# define training set for cross-validation
training <- df[indxTrain,]

# define test set 
testing <- df[-indxTrain,]



