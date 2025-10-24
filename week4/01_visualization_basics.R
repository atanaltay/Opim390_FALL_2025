#review histograms

audit_df = read.csv("week4/data/audit_r.csv")
audit_df$Audit.Time

hist(audit_df$Audit.Time,main="Distribution of Audit Times",
     xlab = "Times (hr)",
     col = "blue",
     xlim = c(10,35),
     breaks=50)

heights = rnorm(100000,mean=170,sd=5)
hist(heights,breaks=20)

#review bar charts

genders = sample(c("Male","Female"),
                 1000,replace = TRUE,
                 prob=c(0.7,0.3))
table(genders)/sum(table(genders))

barplot(sort(table(genders),decreasing = TRUE))

#review of scatter plots


sales = rnorm(1000,3500,200)
costs = rnorm(1000,500,100)

hist(sales)
hist(costs)

revenue = 200*sales - 120*costs - 100 + rnorm(1000,mean=1000,sd=100)
revenue
plot(sales,revenue)
plot(costs,revenue)
cor(sales,revenue)
cor(costs,revenue)

#plot function with more attributes

plot(x=sales,y=revenue,
     main="Revenue by Sales",
     xlab = "Cost",ylab = "Revenue",
     ) # xlim = c(2000,6000)

#abline(a = mean(revenue),b=0,col="blue")
abline(h=mean(revenue),col="red")
abline(v=mean(sales),col="blue")

abline(lm(revenue~sales),col="green")
grid()
#grid(nx=5,ny=5,col="black",lty=5)

#scatter matrix

nyc_data = read.csv("week4/data/nycdata_r.csv")
str(nyc_data)

pairs(nyc_data[,-1])
#####################

kirkland_df = read.csv("week4/data/kirkland_r.csv")
head(kirkland_df)
summary(kirkland_df)
sales <- kirkland_df$Sales...100s.
kirkland_df$Month
month <- paste0(kirkland_df$Month, "-01-2023")
month
month <- as.Date(month, "%b-%d-%Y")
month
plot(month,sales,type="o")

############Barplots in detail (stacki side by side)

kirkland_df = read.csv("week4/data/kirkland_r.csv")
kirkland_df$Month <- factor(kirkland_df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
kirkland_df
barplot(kirkland_df$Sales, names.arg = kirkland_df$Month,
        col = "skyblue", main = "Monthly Sales",
        xlab = "Month", ylab = "Sales")

#multiline charts
regional_df <- read.csv("week4/data/kirklandregional_r.csv")
regional_df

north <- regional_df$North
south <- regional_df$South
month <- paste0(regional_df$Month, "-01-2023")
month <- as.Date(month, "%b-%d-%Y")

plot(month, north,
     type = "l",
     col = "red",
     xlab = "Month",
     ylab = "Sales ($100s)")
lines(month,south,col="blue")
legend("topright",legend=c("North","South"),
       col=c("red","blue"))

#stacked bar chart

df <- data.frame(
  Fruit = c("Apple", "Banana", "Cherry"),
  StoreA = c(10, 20, 30),
  StoreB = c(15, 25, 35)
)








