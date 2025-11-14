library(tidyverse)
#dataset info: https://archive.ics.uci.edu/dataset/9/auto+mpg

df = read.csv("week8/data/Auto.csv")

head(df)
View(df)

str(df)

summary(df)

#there are non-numeric rows in horsepower
non_numeric_rows <- df %>%
  filter(is.na(as.numeric(horsepower)))
non_numeric_rows

df$horsepower = as.numeric(df$horsepower)

df%>%filter(is.na(horsepower))
#options: 1) remove obervations 2) impute values


#understanding origin..
table(df$origin)

df%>%filter(origin==2)

#origin 1-> US, 2->europe, 3-> Japan

table(df$name)

# Are extracting the brand name meaningful?

brands = df %>% separate(name, c('Brand'), sep=" ")%>%
  select(Brand)
brands
length(table(brands))

df$Brand = brands

barplot(sort(table(brands),decreasing = TRUE))

#which brand have highest mean mpg
df_to_plot = df%>%group_by(Brand)%>%summarise(mean_mpg=mean(mpg))%>%
  arrange(desc(mean_mpg))
df_to_plot[df_to_plot$mean_mpg==max(df_to_plot$mean_mpg),]

#which brand have lowest mean mpg
df_to_plot = df%>%group_by(Brand)%>%summarise(mean_mpg=mean(mpg))%>%
  arrange(mean_mpg)
df_to_plot


barplot(df_to_plot$mean_mpg,names.arg = df_to_plot$Brand)


#How does mpg's distrubution look like?

hist(df$mpg,breaks = 30)

# log transformation of skewed mpg:

hist(log(df$mpg),breaks = 30) # now the distribution looks kind of more normal

#what to do with NA horsepower values?
#we may drop na horsepower
library(tidyverse)
df[!is.na(df$horsepower),]

df_dropped = drop_na(df,any_of(names(df)))
df_dropped
df_dropped[is.na(df_dropped$horsepower),]

#is horsepower related with mpg?
plot(df$mpg,df$horsepower)

# or we may impute horsepower by another anchor variable

# is horsepower affected by cylinders or origin?

boxplot(mpg ~ cylinders,
        data = df,
        xlab = "cylinders",
        ylab = "horsepower")

boxplot(mpg ~ origin,
        data = df,
        xlab = "origin",
        ylab = "horsepower")

#Density of hp by cylinders
ggplot(df, aes(x = horsepower, fill = as.factor(cylinders))) +
  geom_density(alpha = 0.7) +  # Transparency for better visibility of overlaps
  labs(
    title = "Density Plot of Horsepower by Cylinders",
    x = "Horsepower",
    y = "Density",
    fill = "Cylinders"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


#Density plot of MPG by cylinders
ggplot(df, aes(x = mpg, fill = as.factor(cylinders))) +
  geom_density(alpha = 0.7) + # Use alpha for transparency to see overlaps
  labs(
    title = "Density Plot of MPG by Cylinders",
    x = "Miles Per Gallon (MPG)",
    y = "Density",
    color = "Cylinders",
    fill = "Cylinders"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# how is horsepower distributed?
ggplot(df, aes(x = horsepower)) +
  geom_density()+
  theme_minimal()


#median hp by cylinders:

df[is.na(df$horsepower),"cylinders"] #we should impute  4 and 6 cylinder median hps

df%>%group_by(as.factor(cylinders))%>%summarise(med_hp = median(horsepower,na.rm = TRUE))

# Extract numeric values safely as single numbers
# Convert cylinders to a factor for proper grouping
df <- df %>%
  mutate(cylinders = as.factor(cylinders))

# Calculate median horsepower for each cylinder group, ensuring numeric values
# Impute missing horsepower with median by cylinders
df <- df %>%
  mutate(horsepower = as.numeric(horsepower))
df <- df %>%
  group_by(cylinders) %>%
  mutate(horsepower = ifelse(is.na(horsepower), median(horsepower, na.rm = TRUE), horsepower)) %>%
  ungroup()
sum(df[is.na(df$horsepower),]) # All NA's are removed


#checking the distributions of all variables with histogram


  # Set up the plotting layout (3x3 grid)
  par(mfrow = c(3, 3)) 

  # Loop through all columns except the 9th column
  for (name in names(df)[-9]) {
    if (is.numeric(df[[name]])) {  # Check if the column is numeric
      hist(
        df[[name]], 
        breaks = 30, 
        main = paste("Histogram of", name), 
        xlab = name, 
        col = "blue", 
        border = "black"
      )
    }
  }
  
  #scatter matrix
  library(ggplot2)
  library(GGally)
  str(df)

  df$cylinders = as.numeric(df$cylinders)
  
  
  ggpairs(df[,-9])+
    theme_minimal()
  df$horsepower = as.numeric(df$horsepower)

  library("corrplot")
  #using library corrplot
  df$cylinders = as.numeric(df$cylinders)
  cor_matrix <- round(cor(df[,c(1:8)]),2)
  par(mfrow=c(1,1))
  corrplot(cor_matrix, type = "upper", order = "hclust", 
           tl.col = "black", tl.srt = 45, addCoef.col = "white")
  
  
  #outliers
  # Calculate lower and upper bounds
  df = data.frame(df)
  iqr <- IQR(df$mpg)
  lower_bound <- quantile(df$mpg, probs = 0.25) - 1.5 * iqr
  upper_bound <- quantile(df$mpg, probs = 0.75) + 1.5 * iqr
  
  # Find outliers
  df[df$mpg < lower_bound,]
  
  df[df$mpg > upper_bound,]
