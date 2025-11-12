
#distribution for a single die roll
rolls = sample(size=10000,x=c(1,2,3,4,5,6),replace = TRUE,prob=c(1/6,1/6,1/6,1/6,1/6,1/6))
table(rolls)/sum(table(rolls))

barplot(table(rolls)/sum(table(rolls)))

#sample mean
sum((table(rolls)*c(1,2,3,4,5,6)))/length(rolls)

real_mean = sum((c(1,2,3,4,5,6)))/6
real_mean

runs = 10000
?append
means = c()
for(i in 1:runs){
  rolls = sample(size=100,x=c(1,2,3,4,5,6),replace = TRUE,prob=c(1/6,1/6,1/6,1/6,1/6,1/6))
  sample_mean = sum((table(rolls)*c(1,2,3,4,5,6)))/length(rolls)
  means = append(means,sample_mean,)
}
means
hist(means,breaks=30)
mean(means)       
