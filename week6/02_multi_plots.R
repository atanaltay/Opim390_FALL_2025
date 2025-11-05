#multi plots on same graph: ##########################

#pizza delivery times

cook_times = rnorm(100,15,2)
delivery_times = rnorm(100,20,3)

hist(cook_times,breaks=30,col = "blue",xlim = c(10,40))
hist(delivery_times,breaks=30,col = "red",add=TRUE,xlim = c(10,40))

var_cook = 2**2
var_deliver = 3**2
sd_sum = sqrt(var_cook + var_deliver)
sd_sum
mean_sum = 35

sd(cook_times+delivery_times)

#plot sum of vals, and the calculated together:
hist(rnorm(100,35,3.52),breaks=30,col="blue")
hist((cook_times+delivery_times),breaks=30,col="red",add=TRUE)

# multiple plots in a matrix

power_func <- function(vals,power){
  
  return (vals**power)
}

idx = 0:100

#On same plot
plot(idx,power_func(idx,2),type="l",col="blue",)
lines(idx,power_func(idx,3),col="red")

# On different plots

par(mfrow=c(2,2))
#inserted from left to right
plot(idx,power_func(idx,2),type="l",col="blue",main = "Pow 2")
plot(idx,power_func(idx,3),type="l",col="blue",main = "Pow 3")
plot(idx,idx,type="l",col="red",main="Linear")
plot(idx,2*idx,type="l",col="red",main="2x")

#sharing axis

#define data to plot
x <- 1:10
y1 <- c(2, 4, 4, 5, 7, 6, 5, 8, 12, 19)
y2 <- c(2, 2, 3, 4, 4, 6, 5, 9, 10, 13)

#define plotting area as one row and two columns
par(mfrow = c(1, 2))

#create first line plot
plot(x, y1, type='l', col='red')

#create second line plot
plot(x, y2, type='l', col='blue', ylim=c(min(y1), max(y1)))


