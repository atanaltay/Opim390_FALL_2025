# stepwise regression


# install packages if not previously installed
# install.packages("olsrr")

# active necessary library packages
install.packages("olsrr")
library(olsrr)


# read in data file from working directory into a data frame
carseats <- read.csv("week7/data/Carseats.csv")
View(carseats)
str(carseats)
# Ensure ShelveLoc is a factor
carseats$ShelveLoc <- as.factor(carseats$ShelveLoc)
carseats$Urban <- as.factor(carseats$Urban)
carseats$US <- as.factor(carseats$US)

# estimate the full multiple linear regression model
carseats_mod <- lm(Sales~ ., data=carseats)

# estimate the stepwise linear regression model
ols_step_both_p(carseats_mod, pent=.05, prem=.05) #pent pvalue entering, prem pvalue removing a variable

#forward selection
# estimate the forward selection linear regression model
ols_step_forward_p(carseats_mod, pent = .05, progress = TRUE, details = FALSE)

# estimate the backward selection linear regression model
ols_step_backward_p(carseats_mod, prem = .05)

# estimate the best subsets linear regression model
ols_step_best_subset(carseats_mod, progress = TRUE, details = FALSE)

