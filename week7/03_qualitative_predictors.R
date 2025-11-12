# Qualitative Predictors

Carseats = read.csv("week7/data/Carseats.csv")

###
head(Carseats)
###
str(Carseats)
names(Carseats)


# Ensure ShelveLoc is a factor
Carseats$ShelveLoc <- as.factor(Carseats$ShelveLoc)
Carseats$Urban <- as.factor(Carseats$Urban)
Carseats$US <- as.factor(Carseats$US)

summary(Carseats$US)
contrasts(Carseats$US)
contrasts(Carseats$ShelveLoc)

# Load the libraries
library(ggplot2)
library(GGally)
# Create a scatterplot matrix using ggpairs()
ggpairs(Carseats, 
        title = "Scatterplot Matrix",
)+
  theme_minimal()

hist(Carseats$Sales,breaks = 20)

#using all features
lm.fit <- lm(Sales ~ + CompPrice+Income+Advertising+Population+Price+ShelveLoc+Age+Education+Urban+US, 
             data = Carseats)
summary(lm.fit)

ggplot(data = Carseats,aes(Urban,Sales))+
  geom_boxplot()

ggplot(data = Carseats,aes(Sales,group = Urban,colour = Urban))+
  geom_density()

ggplot(data = Carseats,aes(US,Sales))+
  geom_boxplot()
ggplot(data = Carseats,aes(Sales,group = US,colour = US))+
  geom_density()



ggplot(data = Carseats,aes(ShelveLoc,Sales))+
  geom_boxplot()

ggplot(data = Carseats,aes(Sales,group = ShelveLoc,colour = ShelveLoc))+
  geom_density()

#calculate RMSE:
sqrt(mean(residuals(lm.fit)**2))

#using only significant features
lm.fit2 <- lm(Sales ~ + CompPrice+Income+Advertising+Price+ShelveLoc+Age, 
             data = Carseats)

summary(lm.fit2)
#calculate RMSE:
sqrt(mean(residuals(lm.fit2)**2))

#Slightly lower RMSE, around the same R2
# As the second model is simple we may pick the model with less params.

###
#display the assignment of dummy vars:
contrasts(Carseats$ShelveLoc)
contrasts(Carseats$US)
contrasts(Carseats$Urban)




