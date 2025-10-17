#Descriptive statistics
getwd()
#Frequency distributions for categoric data and Bar plots
softdrink_df <- read.csv("week3/data/softdrink_r.csv")

View(softdrink_df)

head(softdrink_df,20)

table(softdrink_df)
barplot(table(softdrink_df),col="blue")

barplot(sort(table(softdrink_df)))

data = table(softdrink_df)
data
sum(data)
relative_data = data/sum(data)

relative_data

barplot(relative_data)

percent_data = relative_data*100
percent_data

barplot(sort(percent_data,decreasing = TRUE))


df = read.csv("week3/data/startingsalaries_r.csv")
head(df)
monthly_salary <-df$Monthly.Starting.Salary....

summary(monthly_salary)

summary(df)

summary(df)[2]


fivenum(monthly_salary) #min - q1  Median q3 Max

quantile(monthly_salary, .25)

quantile(monthly_salary, .5) # median

quantile(monthly_salary, probs = seq(0, 1, 1/4))

str(monthly_salary)
sd(monthly_salary) # standard deviation
median(monthly_salary)
mean(monthly_salary)

#histogram
range(monthly_salary)
#try about 5 bins
hist(monthly_salary,breaks = 5,col="blue")

#custom binning continous variables

?cut
df$salary_group <- cut(df$Monthly.Starting.Salary....,
                      breaks = c(5700, 5800, 5900, 6000, 6100, 6200,6300),
                      include.lowest = T,
                      right = F,dig.lab=5)
df$salary_group
barplot(table(df$salary_group))

#boxplot(monthly_salary, xlab="Monthly Starting Salary", horizontal=TRUE)
boxplot(monthly_salary, xlab="Monthly Starting Salary")


# outliers

#with z-transform:
# with operations
df$Salary_z = (df$Monthly.Starting.Salary.... - mean(df$Monthly.Starting.Salary....))/sd(df$Monthly.Starting.Salary....)
df$Salary_z
#with scale() function
scale(df$Monthly.Starting.Salary....) == df$Salary_z

View(df[df$Salary_z>2,])

#with IQR
quantile(df$Monthly.Starting.Salary....)

# Calculate lower and upper bounds
iqr <- IQR(df$Monthly.Starting.Salary....)
lower_bound <- quantile(df$Monthly.Starting.Salary...., probs = 0.25) - 1.5 * iqr
upper_bound <- quantile(df$Monthly.Starting.Salary...., probs = 0.75) + 1.5 * iqr

# Find outliers
df[df$Monthly.Starting.Salary.... < lower_bound,]

df[df$Monthly.Starting.Salary.... > upper_bound,]



#Creates comparative boxplots for the Starting Salary data by majors

major_salaries_df <- read.csv("week3/data/majorsalaries_r.csv")
major_salaries_df
# A single box plot of a categorical variable
boxplot(major_salaries_df$Monthly.Starting.Salary...., xlab = "Monthly Starting Salary ($)")

# box plot of salaries by major
boxplot(Monthly.Starting.Salary.... ~ Major,
        data = major_salaries_df,
        xlab = "Business Major",
        ylab = "Monthly Starting Salary ($)")



#covariance - correlation

electronics_df <- read.csv("week3/data/electronics_r.csv")#Reads in CSV file Electronics.csv as a data frame
electronics_df
number_commericals <- electronics_df$No..of.Commercials  #Reads in column of data from electronics_df as data object number_commercials
sales_volume <- electronics_df$Sales.Volume #Reads in column of data from electronics_df as data object sales_volume
cov(number_commericals, sales_volume) #Calculates covariance of number_commercials and sales_volume variables
cor(number_commericals, sales_volume) #Calculates correlation coefficient of number_commercials and sales_volume variables

head(electronics_df)
plot(electronics_df$No..of.Commercials,electronics_df$Sales.Volume,type = "p")
#type p for points, type l for lines, type b for both
