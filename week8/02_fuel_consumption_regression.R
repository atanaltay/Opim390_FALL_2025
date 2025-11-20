library(tidyverse)
library("corrplot")


df = read.csv("week8/data/Auto.csv")

#Learn about the data
View(df)
summary(df)
str(df)
names(df)

#fixing horsepower, just droppping '?' entries

df$horsepower = as.numeric(df$horsepower)

df[is.na(df$horsepower),]

df = na.omit(df)

df[is.na(df$horsepower),]

cor_matrix <- round(cor(df[,-9]),2)

corrplot(cor_matrix, type = "lower", order = "hclust",  method="square",
         tl.col = "black", tl.srt = 45, addCoef.col = "white")

#scatter matrix
library(ggplot2)
library(GGally)
#group by color
ggpairs(df[,-9])+
  theme_minimal()

#fitting simple linear regression
#1 - trying with 1 variable: horsepower:
lm.fit = lm(mpg ~ horsepower,data=df)
lm.fit
summary(lm.fit)

lm.fit$coefficients
names(lm.fit)
coef(lm.fit)

confint(lm.fit)

#The predict() function can be used to produce confidence intervals and predict()
#prediction intervals for the prediction of mpg for a given value of horsepower.
#prediction interval is always wider for higher uncertainity
df$horsepower
predict(lm.fit, data.frame(horsepower = (c(100, 120, 150))),
       interval = "confidence")
predict(lm.fit, data.frame(horsepower = (c(100, 120, 150))),
        interval = "prediction")

#abline(a,b) a> slope b>intercept

#plot(predict(lm.fit), residuals(lm.fit))
#plot(predict(lm.fit), rstudent(lm.fit))
par(mfrow = c(1, 2))
plot(df$horsepower,df$mpg,pch = 19,    # Solid circle marker
     col = "blue",  # Color of points
     cex = 0.5)
abline(lm.fit,col="red",lwd=2)

plot(predict(lm.fit), rstudent(lm.fit),pch = 19,    # Solid circle marker
     col = "blue",cex = 0.5)
abline(a=0,b=0,col="red",lwd=2)

##qqplot (normal probability plot)
# Standardized Residuals
std_resid <- rstandard(lm.fit)

# Q-Q Plot
qqnorm(std_resid, pch = 19, col = "blue",
       main = "Normal Probability Plot of Standardized Residuals",
       xlab = "Theoretical Quantiles", ylab = "Standardized Residuals")
qqline(std_resid, col = "red", lwd = 2)  # Add reference line


#multiple linear regression

###
lm.fit <- lm(mpg ~ horsepower + year, data = df)
summary(lm.fit)
###
lm.fit <- lm(mpg ~ ., data = df[-9]) # all variables
summary(lm.fit)
###
#variance inflation factor

lm.fit <- lm(mpg ~ ., data = df[-9]) # all variables
summary(lm.fit)

library(car)
#install.packages("car")
lm.fit <- lm(mpg ~ ., data = df[-9])

vif(lm.fit)
###
##VIF=1 -> no collinearity, 1<VIF<5 low to moderate, 5<VIF<10 mod to high, VIF>=10 severe 

#high leverage points

#As a rule of thumb, an observation can potentially exert great
#influence on a multiple regression model if its leverage exceeds 3(p + 1)/n
# p/n
dim(df)

3*9/392 #-> scores over 0.068 are potentially leverage points

hatvalues(lm.fit)[hatvalues(lm.fit)>0.068]

df[14,]
df[95,]

#Cook’s distance indicates an observation is influential, and as a rule of thumb a
#value of at least 1 for the Cook’s D statistic indicates that the corresponding observation is
#influential and should be studied further.  0-0.5 no influence, 0.5-1 moderate, >1 high

cooks.distance(lm.fit)[cooks.distance(lm.fit)>1] #no high influence points


lm.fit1 = lm(mpg~horsepower+factor(origin),data=df)
summary(lm.fit1)

#let's try the full model:

lm.fit = lm(mpg ~ .,data=df[-9])
summary(lm.fit)

plot(predict(lm.fit), rstudent(lm.fit),pch = 19,    # Solid circle marker
     col = "blue",cex = 0.5)
abline(a=0,b=0,col="red",lwd=2)
std_resid <- rstandard(lm.fit)

# Q-Q Plot
qqnorm(std_resid, pch = 19, col = "blue",
       main = "Normal Probability Plot of Standardized Residuals",
       xlab = "Theoretical Quantiles", ylab = "Standardized Residuals")
qqline(std_resid, col = "red", lwd = 2)  # Add reference line
# there is high amount of non-linearity and heteroscedasticity

# let's remove insignificant variables (cyl, acc)
lm.fit = lm(mpg ~ .-cylinders-horsepower,data=df[-9])
summary(lm.fit)

plot(predict(lm.fit), rstudent(lm.fit),pch = 19,    # Solid circle marker
     col = "blue",cex = 0.5)
abline(a=0,b=0,col="red",lwd=2)

# there is high amount of non-linearity and heteroscedasticity

# non linear transformations of predictors

###
lm.fit2 <- lm(mpg ~ horsepower + I(horsepower^2),data=df)
summary(lm.fit2)
###
lm.fit <- lm(mpg ~ horsepower,data=df)

anova(lm.fit, lm.fit2)
#The anova() function performs a hypothesis
#test comparing the two models. The null hypothesis is that the two models
#fit the data equally well, and the alternative hypothesis is that the second
#model is superior.
###

# Sort horsepower values for a smooth curve
sorted_idx <- order(df$horsepower)
sorted_horsepower <- df$horsepower[sorted_idx]

# Generate predictions using the sorted horsepower values
predicted_mpg <- predict(lm.fit2, newdata = data.frame(horsepower = sorted_horsepower))

par(mfrow = c(2, 2))
# Create scatter plot
plot(df$horsepower, df$mpg, pch = 19, col = "blue", cex = 0.5,
     main = "MPG vs Horsepower with Prediction Curve",
     xlab = "Horsepower", ylab = "MPG")

# Add the smooth prediction curve
lines(sorted_horsepower, predicted_mpg, col = "red", lwd = 2)
plot(predict(lm.fit2), rstudent(lm.fit2),pch = 19,    # Solid circle marker
     col = "blue",cex = 0.5)
abline(a=0,b=0,col="red",lwd=2)
# non linearity problem is solved however there is still heteroscedasticity
std_resid <- rstandard(lm.fit)
# Q-Q Plot
qqnorm(std_resid, pch = 19, col = "blue",
       main = "Normal Probability Plot of Standardized Residuals",
       xlab = "Theoretical Quantiles", ylab = "Standardized Residuals")
qqline(std_resid, col = "red", lwd = 2)  # Add reference line


### other polynomial methods poly()
lm.fit5 <- lm(mpg ~ poly(horsepower, 5),data=df)
summary(lm.fit5)

# because of heteroscedasticity, we may need to transform
# mpg variable, so let's check its distribution:
library(ggplot2)
ggplot(df, aes(x=mpg)) + 
  geom_density()

ggplot(df, aes(x=log(mpg))) + 
  geom_density()

#let's fit a model with log transformed mpg:
lm.fit = lm(log(mpg)~displacement+horsepower+weight+year+factor(origin),data=df)
summary(lm.fit)
par(mfrow = c(1, 2))
plot(predict(lm.fit), rstudent(lm.fit),pch = 19,    # Solid circle marker
     col = "blue",cex = 0.5)
abline(a=0,b=0,col="red",lwd=2)
std_resid <- rstandard(lm.fit)
# Q-Q Plot
qqnorm(std_resid, pch = 19, col = "blue",
       main = "Normal Probability Plot of Standardized Residuals",
       xlab = "Theoretical Quantiles", ylab = "Standardized Residuals")
qqline(std_resid, col = "red", lwd = 2)  # Add reference line

#Major improvement in adjusted R-square value. it's gone up from 0.82 to 0.88.
#Heteroscedasticity not too bad with the log transformation, but still room for improvement.
#let's remove displacement
lm.fit = lm(log(mpg)~horsepower+weight+year+factor(origin),data=df)
summary(lm.fit)

plot(predict(lm.fit), rstudent(lm.fit),pch = 19,    # Solid circle marker
     col = "blue",cex = 0.5)
abline(a=0,b=0,col="red",lwd=2)

#still the same problem: moderate heteroscedasticity

## Interaction Terms

#increased horsepower -> effects  weight, also vice-versa

# Load required libraries
library(ggplot2)
library(dplyr)

# Create the categorical variable based on median horsepower
df <- df %>%
  mutate(hp_med = ifelse(horsepower > median(horsepower), "Above Median", "Below Median"))
df
# Plot the regression lines with different colors for "hp_med"
ggplot(df, aes(x = weight, y = mpg, color = hp_med)) +
  geom_point(alpha = 0.6) +   # Scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Linear regression lines
  theme_minimal() +  # Clean theme
  labs(title = "MPG vs Weight by Horsepower Category", color = "Horsepower Level") +
  theme(legend.position = "top")

# var1:var2 > adds the var1xvar2 term
# var1*var2 > adds main effects and interaction term

lm.fit = lm(log(mpg)~horsepower*weight + year + C(origin),data=df)
summary(lm.fit)
par(mfrow = c(2, 2))
plot(predict(lm.fit), rstudent(lm.fit),pch = 19,    # Solid circle marker
     col = "blue",cex = 0.5)
abline(a=0,b=0,col="red",lwd=2)

qqnorm(std_resid, pch = 19, col = "blue",
       main = "Normal Probability Plot of Standardized Residuals",
       xlab = "Theoretical Quantiles", ylab = "Standardized Residuals")
qqline(std_resid, col = "red", lwd = 2)  # Add reference line
#No heteroscedasticity in our residuals and the adjusted R-square value is at 0.89 now.

lm.fit = lm(log(mpg)~horsepower*weight + year + C(origin) + I(horsepower^2),data=df)
summary(lm.fit)

plot(predict(lm.fit), rstudent(lm.fit),pch = 19,    # Solid circle marker
     col = "blue",cex = 0.5)
abline(a=0,b=0,col="red",lwd=2)

#the results are the same as the previous model, this is a more complex model
#so we should pick the simpler one!




