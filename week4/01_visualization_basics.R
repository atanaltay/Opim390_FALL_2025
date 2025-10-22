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




