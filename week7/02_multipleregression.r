# read in data file from working directory into a data frame
butler_with_deliveries_df <- read.csv("week7/data/butlerwithdeliveries_r.csv")

# view the data in the data frame
View(butler_with_deliveries_df)
str(butler_with_deliveries_df)
library(ggplot2)

ggplot(data=butler_with_deliveries_df,aes(Miles,Time))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~x)

ggplot(data=butler_with_deliveries_df,aes(Deliveries,Time))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~x)

#Multicollinearity?
ggplot(data=butler_with_deliveries_df,aes(Deliveries,Miles))+
  geom_point()+
  geom_smooth(method = "lm",formula = y~x)

library(GGally)

# Create a scatterplot matrix using ggpairs()
ggpairs(butler_with_deliveries_df[,-1], 
        title = "Scatterplot Matrix",
)+
  theme_minimal()


# estimate the multiple linear regression model
butler_MLR <- lm(Time ~ Miles + Deliveries, data= butler_with_deliveries_df)
summary(butler_MLR)

# calculate the predicted values 
butler.pred=predict(butler_MLR)

# calculate the residuals 
butler.res=resid(butler_MLR)

# plot the residuals against the predicted values and the residuals against the independent variable
plot(butler.pred,butler.res,ylab="Residuals",xlab="Predicted Values",main="Plot of Residuals vs. Predicted Values")
abline(a=0,b=0, col="red",lw=2)

# plot the residuals against each of the independent variables
plot(butler_with_deliveries_df$Miles,butler.res,ylab="Residuals",xlab="Miles",main="Plot of Residuals vs. Miles")
abline(a=0,b=0, col="red",lw=2)

plot(butler_with_deliveries_df$Deliveries,butler.res,ylab="Residuals",xlab="Deliveries",main="Plot of Residuals vs. Deliveries")
abline(a=0,b=0, col="red",lw=2)

# calculate the standardized residuals and create a normal probability plot
butler.stdres=rstandard(butler_MLR)
qqnorm(butler.stdres,ylab="Standardized Residuals",xlab="Normal Scores",main=" Normal Probability Plot of Standardized Residuals")
qqline(butler.stdres,lw=2,col="red")
list(butler.stdres)

# identifying possible outliers
butler.stdres[abs(butler.stdres) > 2]

# calculate the leverage values
hatvalues(butler_MLR)

# identifying any observations with large leverage values
hatvalues(butler_MLR)[hatvalues(butler_MLR) > 0.03] # 8/300 -> 2P/n
                      
# calculate the Cook's D statistic
cooks.distance(butler_MLR)

# identifying any observations with large Cook's D statistic values
cooks.distance(butler_MLR)[cooks.distance(butler_MLR) > 1] # P/n

# calculate the confidence and prediction intervals for a new observation
newdata=data.frame(Miles=85,Deliveries=3)
predict(butler_MLR,newdata,interval="confidence",level=0.95)
predict(butler_MLR,newdata,interval="prediction",level=0.95)

# calculate the confidence and prediction intervals for multiple new observations
new_butler_df<-read.csv("week6/data/newbutler_r.csv")
predict(butler_MLR,new_butler_df,interval="confidence",level=0.95)
predict(butler_MLR,new_butler_df,interval="prediction",level=0.95)
