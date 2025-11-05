### reading files without loading the whole content


readLines("week5/data/ufo.csv",n = 5)



######dealing with delimited files##########

#sddeliveries -> csv

deliveries_df = read.csv("week5/data/sdeliveries_r.csv")

View(deliveries_df)
#lets format the date field and create a column of Date class
deliveries_df$date_sdelivery = as.Date(deliveries_df$Date.of.SDelivery,format = "%m/%d/%Y")
deliveries_df$date_sdelivery
str(deliveries_df)

deliveries_df[order(deliveries_df$date_sdelivery,decreasing = TRUE),]

# read txt, default seperator is tab

?read.table
purchase_prices_df = read.table("week5/data/tblpurchaseprices_r.txt",
                                header = TRUE)
View(purchase_prices_df)
str(purchase_prices_df)

#getting rid of $ sign, we use gsub(), as $ is spec. char we escape it
#gsub(): for replacing strings
str(purchase_prices_df$KegPurchasePrice)
dat <- gsub("\\$","",purchase_prices_df$KegPurchasePrice)
dat = as.numeric(dat)
max(dat)
min(dat)

#there are no headers in the following file, and the delimiter is '|' 
mov_temp = read.table("week5/data/movieusers.txt")
View(mov_temp)
movie_user_df = read.table("week5/data/movieusers.txt",header = FALSE,sep="|",
                           col.names = c("id","age","gender","occupation","postcode"))
View(movie_user_df)

#######WORKING WITH RECORDS#############

# dealing with missing values

#is.na(df)
#is.na(df$field)
#complete.cases

# using tidyverse for rew and column operations
install.packages("tidyverse") # ggplot2(visualizing) + dplyr(querying) + tidyr(reshaping tidying)
library(tidyverse)

treadwear_df = read.csv("week5/data/treadwear_r.csv")
View(treadwear_df)
summary(treadwear_df) # NA's are reported
?is.na
is.na(treadwear_df$Miles)
treadwear_df[is.na(treadwear_df$Miles),]


?complete.cases #-> return rows with no missing values
treadwear_df[!complete.cases(treadwear_df),]


#count number of missing for each field
colSums(is.na(treadwear_df))

#dropping rows w,th missing values:
?drop_na

drop_na(treadwear_df) # clean df, only keeps the complete rows


####DUBLICATE RECORDS ###########

treadnew = read.csv("week5/data/treadwearnew_r.csv")
View(treadnew)
?duplicated
duplicated(treadnew)
treadnew[duplicated(treadnew),]

treadnew[treadnew$ID.Number==81518,]
treadnew[0:4,]
which(duplicated(treadnew)) # which indices are true

#number of dublicates
sum(duplicated(treadnew))

#subsetting data

treadwear_df = read.csv("week5/data/treadwear_r.csv")
?subset
newdata_df <- subset(treadwear_df, Position.on.Automobile=="LF" & Tread.Depth <= 2)
View(newdata_df)

#selecting rows
#subset(airquality, Temp > 80, select = c(Ozone, Temp))
newdata_df2 <- subset(treadwear_df, Position.on.Automobile=="LF" & Tread.Depth <= 2,select = 1:3)
View(newdata_df2)

# combining records
?rbind
?cbind

treadwear_df = read.csv("week5/data/treadwear_r.csv")
treadwear_df_new = read.csv("week5/data/treadwearnew_r.csv")
head(treadwear_df)
head(treadwear_df_new)
combinedtread_df <- rbind(treadwear_df,treadwear_df_new)
View(combinedtread_df)

dim(treadwear_df)
dim(combinedtread_df)

#working with fields
library(dplyr)
library(tidyr)
vignette("dplyr")
#######dplyr
treadwear_df = read.csv("week5/data/treadwear_r.csv")
# operator: %>%
#filter() picks cases based on their values.
#select() picks variables based on their names.
#mutate() adds new variables that are functions of existing variables
#summarise() reduces multiple values down to a single summary.
#arrange() changes the ordering of the rows.
#returns dataframe
treadwear_df%>%filter(Position.on.Automobile=="LF" & Miles>5000)
treadwear_df%>%filter(Position.on.Automobile=="LR" & Tread.Depth>10)

treadwear_df %>% 
  select(Miles, Life.of.Tire..Months.)

treadwear_df%>%filter(Position.on.Automobile=="LR" & Tread.Depth>10)%>%
  select(Miles,Position.on.Automobile)

treadwear_df%>%filter(Position.on.Automobile=="LR" & Tread.Depth>10)%>%
  select(ID.Number,Tread.Depth)

treadwear_df%>%mutate(log_miles = log(Miles))%>%select(ID.Number,Miles,log_miles)

treadwear_df%>%mutate(miles_per_month=Miles/Life.of.Tire..Months.,log_miles = log(Miles))%>%
  select(ID.Number,Miles,miles_per_month,log_miles)

#create new column for old tires
hist(treadwear_df$Life.of.Tire..Months.,breaks = 10)

treadwear_df%>%mutate(is_old = Life.of.Tire..Months.>median(Life.of.Tire..Months.))

treadwear_df%>%mutate(is_old = Life.of.Tire..Months.>median(Life.of.Tire..Months.))%>%
  filter(is_old==TRUE)


new_df <- treadwear_df%>%mutate(is_old = Life.of.Tire..Months.>median(Life.of.Tire..Months.))
View(new_df)

#sorting rows by column/field
treadwear_df %>% 
  arrange(desc(Miles))

treadwear_df%>%filter(Position.on.Automobile=="LR" & Tread.Depth>10)%>%
  select(ID.Number,Tread.Depth)%>%arrange(desc(Tread.Depth))


##################


brewersaddresses_df <- read.csv("week5/data/tblbrewersaddresses_r.csv")
brewersaddresses_df
brewersaddresses_df$Address

brewersaddresses_df%>%count()


#separating fields
tst = brewersaddresses_df %>% separate(Address, c('StreetAddress', 'CityName', 'State', 'ZipCode'), sep=", ")
tst
#uniting fields
tst%>%unite(col = "address",c("StreetAddress","CityName", "State", "ZipCode"),sep = ',')

# you may use paste() for combining character columns, also there is the unify() method of dplyr

#merging fields
treadwear_df <- read.csv("week5/data/treadwear_r.csv")
treadwearmodels_df <- read.csv("week5/data/treadwearmodels_r.csv")
treadwearmodels_df
treadwear_df
?merge
# merge combines dataframes by using common column name, here it is ID.Number
#one-to-many merge
# merge (x,y,by_x,by_y)
mergededtread_df <- merge(treadwear_df,treadwearmodels_df,by.x = "ID.Number",by.y = "ID.Number")
View(mergededtread_df)

city_ids <- c(1,2,3,4,5)
cities <- c("london","paris","new york","barcelona","izmir")
names <- c("jack","john","william")
names_city = c(1,1,2)

peopledf <- data.frame(name=names,cityid=names_city)
citiesdf <- data.frame(id=city_ids,city=cities)

peopledf

citiesdf

#Outer join: merge(x = df1, y = df2, by = "KEY", all = TRUE)
#Left outer: merge(x = df1, y = df2, by = "KEY", all.x = TRUE)
#Right outer: merge(x = df1, y = df2, by = "KEY", all.y = TRUE)
#Cross join: merge(peopledf,citiesdf,by.x = "cityid",by.y = "id")

#outer join
merge(peopledf,citiesdf,by.x = "cityid",by.y = "id",all=TRUE)

#left outer join
merge(peopledf,citiesdf,by.x = "cityid",by.y = "id",all.x=TRUE)

#right outer join
merge(peopledf,citiesdf,by.x = "cityid",by.y = "id",all.y=TRUE)

# all matching (inner join)
merge(peopledf,citiesdf,by.x = "cityid",by.y = "id")


#cartesian be careful, matches all records with others
merge(peopledf,citiesdf) # cartesian join

mergededtread_df <- merge(treadwear_df,treadwearmodels_df,by = "ID.Number")
View(mergededtread_df)

#unstack 
?spread
stackedtablehs3pa_df <- read.csv("week5/data/stackedtablehs3pa_r.csv")
df_unstacked <- stackedtablehs3pa_df %>%spread(key=Year, value=Three.Point.Attempts)
df_unstacked

cols_to_rename <- 2:7
colnames(df_unstacked)[cols_to_rename] <- paste("Year_", colnames(df_unstacked)[cols_to_rename],sep = "")

#stack 
#?gather (newcol, value,selection cols)
newstackedtablehs3pa_df <- df_unstacked %>% gather(Year, Three.Point.Attempts,-Team)
#apply to all columns except team
newstackedtablehs3pa_df

write.csv(newstackedtablehs3pa_df, "stacked.csv")

#group_by, summarise

mov_users = read.table("week5/data/movieusers.txt",header = FALSE, 
           col.names = c("id","age","gender","occupation","postcode"),sep = "|")

head(mov_users)

mov_users%>%group_by(gender)%>%count()
mov_users%>%group_by(gender,occupation)%>%count()

grp_data = mov_users%>%group_by(gender)%>%summarise(mean_age=mean(age,na.rm=TRUE))
grp_data

#mean, max, sd, min, n

grp_data = mov_users%>%group_by(gender)%>%summarise(cnt=n())
grp_data

grp_data = mov_users%>%group_by(occupation)%>%summarise(mean_age=mean(age,na.rm=TRUE))
grp_data

grp_data = mov_users%>%group_by(occupation)%>%summarise(mean_age=max(age,na.rm=TRUE))
grp_data%>%arrange(desc(mean_age))


stackedtablehs3pa_df <- read.csv("week5/data/stackedtablehs3pa_r.csv")

stackedtablehs3pa_df %>%
  group_by(Team) %>%
  summarise(
    sum = sum(Three.Point.Attempts,na.rm=TRUE),
    mean = mean(Three.Point.Attempts,na.rm=TRUE)
  ) 

stackedtablehs3pa_df %>%
  group_by(Team,Year) %>%
  summarise(
    sum = sum(Three.Point.Attempts,na.rm=TRUE),
    mean = mean(Three.Point.Attempts,na.rm=TRUE)
  )

#Bar plot (stacked by Team)


summarized_data = stackedtablehs3pa_df %>%
  group_by(Team,Year) %>%
  summarise(
    sum = sum(Three.Point.Attempts,na.rm=TRUE),
    mean = mean(Three.Point.Attempts,na.rm=TRUE)
  )
summarized_data

######GOTO GGPlot

ggplot(summarized_data, aes(x = factor(Team), y = sum, fill = Year)) +
  geom_bar(stat = "identity", position = "stack") + #for beside position="dodge"
  labs(title = "Total Three-Point Attempts by Team and Year",
       x = "Year", y = "Total Three-Point Attempts") +
  theme_minimal()

# Line plot to show trends in mean Three-Point Attempts
ggplot(summarized_data%>%filter(Team=="Birchard Spiders"), aes(x = Year, y = mean)) +
  geom_line(size = 1,color="red") +
  geom_point(size = 3) +
  labs(title = "Mean Three-Point Attempts by Team Over Years",
       x = "Year", y = "Mean Three-Point Attempts") +
  theme_minimal()

