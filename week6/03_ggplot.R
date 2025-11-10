########for more information: https://ggplot2.tidyverse.org

df = read.csv("week6/data/Auto.csv")
head(df)
str(df)

df[is.na(suppressWarnings(as.numeric(df$horsepower))), ]

df = df[df$horsepower!='?',]
df
df$horsepower
df$horsepower = as.numeric(df$horsepower)

str(df)

install.packages("tidyverse")
library(ggplot2)
library(dplyr)

###usage:

# Start with ggplot(data, aes(...)) → define the dataset and aesthetics (x, y, color, fill, size, etc.).
#Add geoms (geometric objects) like bars, points, lines.
#Add layers for labels, themes, scales.
#faceting specifications (like facet_wrap()) 
#and coordinate systems (like coord_flip()).
# and stying themes

# component-1: data
# comp-2: aestetics mappings
# component 3: layers (geom)


# points on plot
ggplot(df, aes(x = weight, y = mpg)) +
  geom_point()+
  labs(title="mpg by weight")+
  theme_light()

ggplot(df, aes(x = weight, y = mpg)) +
  geom_point(color="blue",size=1)+
  theme_light()

# if we want to change aestetics per object you should also add
# specific aes to geometry objects
ggplot(df, aes(x = horsepower, y = mpg)) +
  geom_point(aes(color = factor(origin))) +
  theme_light()

########## CONTINUEE #############

df%>%filter(origin==1)


ggplot(df, aes(x = weight, y = mpg)) +
  geom_point(shape=2,size=2)+
  labs(x="Weight",y="MPG",title = "mpg by weight")+
  theme_light()


plot(df$weight,df$mpg,type = "p")

## adding lines on same plot area

idx = 0:100
pow2 = idx**2
pow3 = idx**3

dff = data.frame(idx=idx,x1=pow2,x2=pow3)

ggplot(dff)+
  geom_line(aes(x=idx,y=pow2), color="blue",size=0.5)+
  geom_line(aes(x=idx,y=pow3), color="red")+
  theme_bw()


# points and trend line

ggplot(df, aes(x = weight, y = mpg)) +
  geom_point() +
  geom_smooth(formula = "y ~ x")  


ggplot(df, aes(x = weight, y = mpg)) +
  geom_point() +
  geom_smooth(formula = "y ~ x", method = "lm")  

ggplot(df, aes(x = weight, y = mpg,colour = as.factor(origin))) +
  geom_point()+
  geom_smooth(formula = "y ~ x", method = "lm")

#changing the theme

ggplot(df, aes(x = weight, y = horsepower,colour = as.factor(origin))) +
  geom_point() +
  theme_bw()

ggplot(df, aes(x = weight, y = horsepower,colour = as.factor(origin))) +
  geom_point() +
  theme_minimal()

#histograms

ggplot(df,aes(mpg,fill = "red")) +
  geom_histogram(bins = 50,color="black")

ggplot(df,aes(mpg,fill = "red")) +
  geom_histogram(bins = 50,color="black",fill="blue")

ggplot(df,aes(mpg,fill = )) +
  geom_density(color="black",)

# Histogram with kernel density
ggplot(df, aes(x = mpg)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill ="blue") +
  geom_density(color="red")

# side by side plots by a categorical variable

ggplot(df,aes(weight,mpg,color=origin)) +
  geom_point()+
  facet_wrap(~origin)+
  geom_smooth(formula = "y~x",method="lm")

## adding multiple information
ggplot(df, aes(horsepower, mpg,color=weight)) + 
  geom_point() + 
  geom_smooth(method="lm")

#frequency polygon

ggplot(df,aes(weight,fill = )) +
  geom_histogram(bins = 50,color="black", fill="blue") +
  geom_freqpoly()

#box plots

ggplot(df,aes(as.factor(cylinders),horsepower))+
  geom_boxplot()+
  theme_bw()


#density of mpg by origin
ggplot(df, aes(mpg, colour = as.factor(origin))) + 
  geom_density()

ggplot(df, aes(mpg, fill = as.factor(origin))) + 
  geom_histogram(binwidth = 0.5) + 
  facet_wrap(~as.factor(origin), ncol = 1)


#bar plots

ggplot(df, aes(origin)) + 
  geom_bar()

drugs <- data.frame(
  drug = c("a", "b", "c"),
  effect = c(4.2, 9.7, 6.1)
)
ggplot(drugs, aes(drug,effect)) + 
  geom_bar(stat = "identity") #geom_bar directly tries to count observations, so we explicitly set stat to identity to get use of the effect

ggplot(drugs, aes(drug,effect)) + 
  geom_col()

#https://ggplot2-book.org/statistical-summaries


#heatmap
# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y) #Create a data frame from all combinations of the supplied vectors or factors.
data
?expand.grid
?runif
data$Z <- runif(400, 0, 5) # random pick from uniform distribution
data$Z
data
# Heatmap 
heatmap_plot = ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile()

# Print the heatmap
print(heatmap_plot)


#using library corrplot
# for more information: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

cor_matrix <- round(cor(df[,-9]),2)
cor_matrix
install.packages("corrplot")
library("corrplot")

?corrplot

corrplot(cor_matrix)

corrplot(cor_matrix,method = "square")


corrplot(cor_matrix,type = "lower")

corrplot(cor_matrix,type = "lower",method="number")

#scatter plot

install.packages("GGally")

# Load the libraries

library(ggplot2)
install.packages("GGally")
library(GGally)
df
# Create a scatterplot matrix using ggpairs()
ggpairs(df[,-9], 
        title = "Scatterplot Matrix",
  )+
  theme_minimal()


#group by color
ggpairs(df[,-9], aes(color = as.factor(origin)),
        title = "Scatterplot Matrix",
)+
  theme_minimal()

#side by side plots

# Load libraries
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

# Create example plots
p1 <- ggplot(df, aes(x = weight, y = mpg)) + geom_point() + ggtitle("Plot 1")
p2 <- ggplot(df, aes(x = horsepower, y = mpg)) + geom_point() + ggtitle("Plot 2")
p3 <- ggplot(df, aes(x = cylinders, y = mpg)) + geom_point() + ggtitle("Plot 3")
p4 <- ggplot(df, aes(x = displacement, y = mpg)) + geom_point() + ggtitle("Plot 4")

# Arrange the plots in a 2x2 grid
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)


