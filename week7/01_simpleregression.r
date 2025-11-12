library(ggplot2)

# read in data file from working directory into a data frame
armands_df <- read.csv("week7/data/armands_r.csv")

# view the data in the data frame
View(armands_df)
str(armands_df)

ggplot(data=armands_df,aes(Population,Sales))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~x)


# estimate the simple linear regression model
armands_SLR <- lm(Sales ~ Population, data = armands_df)
summary(armands_SLR)


# calculate the predicted values
armands.pred=predict(armands_SLR)

#calculate RMSE:
sqrt(mean(residuals(armands_SLR)**2))


#Plotting the fit:

library(ggplot2)

# Generate a sequence of Population values for prediction
vals <- seq(min(armands_df$Population), max(armands_df$Population), length.out = length(armands_df))

# Create a new dataframe for predictions
dfnew <- data.frame(Population = vals)

# Generate predictions using the fitted model
dfnew$Sales <- predict(armands_SLR, newdata = dfnew)

# Plot observed data and predictions
ggplot(data = armands_df, aes(x = Population, y = Sales)) +
  geom_point(color = "blue", alpha = 0.7) +  # Scatter plot for observed data
  geom_line(data = dfnew, aes(x = Population, y = Sales), color = "red", size = 1) +  # Line plot for predictions
  labs(title = "Predicted vs Observed Sales",
       x = "Population",
       y = "Sales") +
  theme_minimal()

# calculate the residuals
armands.res=resid(armands_SLR)

# plot the residuals against the predicted values
plot(armands.pred,armands.res,ylab="Residuals",xlab="Predicted Values",main="Plot of Residuals vs. Predicted Values")
abline(h = 0,col="red")

# plot the residuals against the independent variable
plot(armands_df$Population,armands.res,ylab="Residuals",xlab="Actual",main="Plot of Residuals vs. Student Population")
abline(h=0, col="red")

# calculate the standardized residuals and create a normal probability plot
armands.stdres=rstandard(armands_SLR)
qqnorm(armands.stdres,ylab="Standardized Residuals",xlab="Normal Scores",main=" Normal Probability Plot of Standardized Residuals")
qqline(armands.stdres, col = "red", lwd = 2)  # Add reference line
list(armands.stdres)

# calculate the leverage values and Cook's D statistics
hatvalues(armands_SLR) # threshold including intercept 2p/n ==> 4/10 = 0.4 , >1 extreme l
# no point exceeds threshold

cooks.distance(armands_SLR) #  >1 extreme



# calculate the confidence and prediction intervals for a new observation
newdata=data.frame(Population=14)
predict(armands_SLR,newdata,interval="confidence",level=0.95)
predict(armands_SLR,newdata,interval="prediction",level=0.95)
