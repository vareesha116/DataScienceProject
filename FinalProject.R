

rm(list=ls()) # clear workspace
cat("\014")  # clear console
graphics.off() # shut down all open graphics devices 

### Load Required Libraries

# install.packages("sf")
# install.packages("geojsonio", "mapproj", )
# install.packages("foreign")
# install.packages("nnet")
# install.packages("reshape2")
# install.packages("rgdal")
# install.packages("highcharter")

library(class)
library(geojsonio)
library(broom)
library(MASS)
library(readr)
library (rpart)
library(caret)
library(lubridate)
library(foreign)
library(nnet)
library(reshape2) 
library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggmap) 
library(rgdal)
library(highcharter)
library(viridis)
library(xts)
library(purrr)
library(plotly)
library(knitr)

### Read dataset containing crime data in Chicago from 2012 to 2017
data = read.csv('Chicago_Crimes_2012_to_2017.csv')
summary(data)

### DATA CLEANING

# Dealing with NAs: 1 NA in district, 14 in Ward, 40 in Community Area and 37,083 NAs in Coordinates
# None of them overlap
# It is only about 2% of the data so we can omit the NAs

data_subset = na.omit(data)

## Organizing Date and Time

# Extracting time in a 24 hour clock format from the date
time = format(as.POSIXct(data_subset$Date, format="%m/%d/%Y %I:%M:%S %p"),"%H:%M")

year = format(as.POSIXct(data_subset$Date, format="%m/%d/%Y %I:%M:%S %p"),"%Y") # Extracting year

# Extracting month: January is 1, Dec is 12
month = format(as.POSIXct(data_subset$Date, format="%m/%d/%Y %I:%M:%S %p"),"%m") 
day = format(as.POSIXct(data_subset$Date, format="%m/%d/%Y %I:%M:%S %p"),"%d") #Day 

# Extracting days of the week: Sunday is 1, Saturday is 7
weekday = c("1", "2", "3", "4", "5", "6", "7")[as.POSIXlt(data_subset$Date, format="%m/%d/%Y %I:%M:%S %p")$wday + 1] 

data_subset$day = day
data_subset$year = year
data_subset$month = month
data_subset$weekday = weekday
data_subset$time = time


### Read the base offence level rating from the United States Sentencing Commission handbook
### and match the Primary Types with each offence levels
offence_levels = read.csv("offence_level.csv")
levels = lapply(data_subset$Primary.Type, function(x) offence_levels$level[match(x, offence_levels$Crime)])
n = length(levels[[1]])
levels = structure(levels, row.names = c(NA, -n), class = "data.frame")
levels = t(levels)
data_subset$levels = levels
summary(data_subset)

### Factoring District and Levels columns

data_subset$District = factor(data_subset$District)
data_subset$Primary.Type = factor(data_subset$Primary.Type)
data_subset$levels = factor(data_subset$levels)

####### EDA

######      Top 10 Primary Type     #####
type = count(data_subset, Primary.Type)
type = type[order(-type$n),]
summary(data_subset$Primary.Type)
top_10_type = type[1:10, ]
par(mar = c(7, 4, 2, 2) + 0.3) #add room for the rotated labels
end_point = 0.5 + nrow(top_10_type) + nrow(top_10_type) #this is the line which does the trick (together with barplot "space = 1" parameter)
barplot(top_10_type$n,
        las = 2,cex.axis = 0.7,cex.names=0.5, 
        main = "Count of top 10 crimes",
        sub = "Primary Crime Type", 
        ylab = "Count of Crimes", 
        col = c("#BF360C","#D84315","#E64A19","#F4511E","#FF5722","#FF7043","#FF8A65","#FFAB91","#FFCCBC","#FBE9E7"))
text( seq(1, end_point, by = 1.2),par("usr")[3]-0.5, 
      srt = 40, adj = 1.1, xpd = TRUE,
      labels = c(as.character(top_10_type$Primary.Type[1:10])," "), cex = 0.65)

#####     Yearly Top 5      #####
type_year = count(data_subset, Primary.Type, year)
type_year_top1 = type_year[(type_year$Primary.Type==top_10_type$Primary.Type[1]),]
type_year_top2 = type_year[(type_year$Primary.Type==top_10_type$Primary.Type[2]),]
type_year_top3 = type_year[(type_year$Primary.Type==top_10_type$Primary.Type[3]),]
type_year_top4 = type_year[(type_year$Primary.Type==top_10_type$Primary.Type[4]),]
type_year_top5 = type_year[(type_year$Primary.Type==top_10_type$Primary.Type[5]),]

plot(n ~ year, type_year_top1[1:5,],
     type = "o",
     lwd = 3,
     main = "Yearly count of top 5 Crimes in Chicago",
     col = c("#4DD0E1"),
     ylim = c(0,90000),
     ylab = "Annual Crime Count",
     xlab = "Year"
)
lines(n ~ year, type_year_top2[1:5,],
      type = "o",
      lwd = 3,
      col = c("#4DB6AC"))
lines(n ~ year, type_year_top3[1:5,],
      type = "o",
      lwd = 3,
      col = c("#F06292"))
lines(n ~ year, type_year_top4[1:5,],
      type = "o",
      lwd = 3,
      col = c("#BA68C8"))
lines(n ~ year, type_year_top5[1:5,],
      type = "o",
      lwd = 3,
      col = c("#FFB74D"))

legend("topright", legend=c(top_10_type$Primary.Type[1:5]),
       col=c("#4DD0E1", "#4DB6AC","#F06292","#BA68C8","#FFB74D"), lty=1, lwd =3, cex=0.8)


#####     Monthly Top 5     #####

type_month = count(data_subset, Primary.Type, month)
type_month_top1 = type_month[(type_month$Primary.Type==top_10_type$Primary.Type[1]),]
type_month_top2 = type_month[(type_month$Primary.Type==top_10_type$Primary.Type[2]),]
type_month_top3 = type_month[(type_month$Primary.Type==top_10_type$Primary.Type[3]),]
type_month_top4 = type_month[(type_month$Primary.Type==top_10_type$Primary.Type[4]),]
type_month_top5 = type_month[(type_month$Primary.Type==top_10_type$Primary.Type[5]),]

plot(n ~ month, type_month_top1[1:12,],
     type = "o",
     lwd = 3,
     main = "Month-wise count of top 5 Crimes in Chicago",
     col = c("#4DD0E1"),
     ylim = c(0,45000),
     ylab = "Crime Count",
     xlab = "Month",
     xaxt = "none",
     las = 0
)
lines(n ~ month, type_month_top2[1:12,],
      type = "o",
      lwd = 3,
      col = c("#4DB6AC"))
lines(n ~ month, type_month_top3[1:12,],
      type = "o",
      lwd = 3,
      col = c("#F06292"))
lines(n ~ month, type_month_top4[1:12,],
      type = "o",
      lwd = 3,
      col = c("#BA68C8"))
lines(n ~ month, type_month_top5[1:12,],
      type = "o",
      lwd = 3,
      col = c("#FFB74D"))
text( seq(1, 12, by = 1),par("usr")[3]-0.5, 
      srt = 90, adj = 1.3, xpd = TRUE,
      labels = c("January","February","March","April","May","June","July","August","September","October","November","December"), cex = 0.65)
legend("topright", legend=c(top_10_type$Primary.Type[1:5]),
       col=c("#4DD0E1", "#4DB6AC","#F06292","#BA68C8","#FFB74D"), lty=1, lwd =3, cex=0.8)

#####     Weekly Top 5     #####

type_weekday = count(data_subset, Primary.Type, weekday)
type_weekday_top1 = type_weekday[(type_weekday$Primary.Type==top_10_type$Primary.Type[1]),]
type_weekday_top2 = type_weekday[(type_weekday$Primary.Type==top_10_type$Primary.Type[2]),]
type_weekday_top3 = type_weekday[(type_weekday$Primary.Type==top_10_type$Primary.Type[3]),]
type_weekday_top4 = type_weekday[(type_weekday$Primary.Type==top_10_type$Primary.Type[4]),]
type_weekday_top5 = type_weekday[(type_weekday$Primary.Type==top_10_type$Primary.Type[5]),]

par(mar = c(7, 4, 2, 2) + 0.3)
plot(n ~ weekday, type_weekday_top1[1:7,],
     type = "o",
     lwd = 3,
     main = "Day of Week count of top 5 Crimes in Chicago",
     col = c("#4DD0E1"),
     ylim = c(0,75000),
     ylab = "Crime Count",
     xlab = "",
     sub = "Weekday",
     las = 0,
     xaxt="none"
)
text( seq(1, 8, by = 1),par("usr")[3]-0.5, 
      srt = 90, adj = 1.3, xpd = TRUE,
      labels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"), cex = 0.65)
lines(n ~ weekday, type_weekday_top2[1:7,],
      type = "o",
      lwd = 3,
      col = c("#4DB6AC"))
lines(n ~ weekday, type_weekday_top3[1:7,],
      type = "o",
      lwd = 3,
      col = c("#F06292"))
lines(n ~ weekday, type_weekday_top4[1:7,],
      type = "o",
      lwd = 3,
      col = c("#BA68C8"))
lines(n ~ weekday, type_weekday_top5[1:7,],
      type = "o",
      lwd = 3,
      col = c("#FFB74D"))
legend("topright", legend=c(top_10_type$Primary.Type[1:5]),
       col=c("#4DD0E1", "#4DB6AC","#F06292","#BA68C8","#FFB74D"), lty=1, lwd =3, cex=0.8)

#####     Day of Month Top 5     #####

type_day = count(data_subset, Primary.Type, day)
type_day_top1 = type_day[(type_day$Primary.Type==top_10_type$Primary.Type[1]),]
type_day_top2 = type_day[(type_day$Primary.Type==top_10_type$Primary.Type[2]),]
type_day_top3 = type_day[(type_day$Primary.Type==top_10_type$Primary.Type[3]),]
type_day_top4 = type_day[(type_day$Primary.Type==top_10_type$Primary.Type[4]),]
type_day_top5 = type_day[(type_day$Primary.Type==top_10_type$Primary.Type[5]),]

par(mar = c(7, 4, 2, 2) + 0.3)
plot(n ~ day, type_day_top1[1:31,],
     type = "o",
     lwd = 3,
     main = "Day of Month count of top 5 Crimes in Chicago",
     col = c("#4DD0E1"),
     ylim = c(0,20000),
     ylab = "Crime Count",
     las = 0
)

lines(n ~ day, type_day_top2[1:31,],
      type = "o",
      lwd = 3,
      col = c("#4DB6AC"))
lines(n ~ day, type_day_top3[1:31,],
      type = "o",
      lwd = 3,
      col = c("#F06292"))
lines(n ~ day, type_day_top4[1:31,],
      type = "o",
      lwd = 3,
      col = c("#BA68C8"))
lines(n ~ day, type_day_top5[1:31,],
      type = "o",
      lwd = 3,
      col = c("#FFB74D"))
legend("topright", legend=c(top_10_type$Primary.Type[1:5]),
       col=c("#4DD0E1", "#4DB6AC","#F06292","#BA68C8","#FFB74D"), lty=1, lwd =3, cex=0.8)

#####     Monthly Total Crime     #####

type_month_tot = count(data_subset, month)
type_month_tot$norm_n = (type_month_tot$n/mean(type_month_tot$n[1:12]))-1
norm_sd_month_tot = sd(type_month_tot$n[1:12])/mean(type_month_tot$n[1:12])
type_month_tot$norm_n_1SD = type_month_tot$norm_n + norm_sd_month_tot
type_month_tot$norm_n_m1SD = type_month_tot$norm_n - norm_sd_month_tot

par(mar = c(7, 4, 2, 2) + 0.3)
plot(norm_n ~ month, type_month_tot[1:12,],
     type = "o",
     lwd = 3,
     main = "Seasonal trend of Crimes in Chicago",
     col = c("#4070E1"),
     ylim = c(-0.5,0.5),
     ylab = "Scaled Crime Count (mean corresponds to 0)",
     xlab = "Months",
     las = 0
)
lines(norm_n_1SD ~ month, type_month_tot[1:12,],
      type = "l",
      lwd = 1,
      col = c("#4070E1"))
lines(norm_n_m1SD ~ month, type_month_tot[1:12,],
      type = "l",
      lwd = 1,
      col = c("#4070E1"))
lines(c(rep(0,12)) ~ month,type_month_tot[1:12,], lty = 2)
text(seq(1, 12, by = 1),
     par("usr")[3]-0.5, 
     srt = 90, adj = 1.3, xpd = TRUE,
     labels = c("January","February","March","April","May","June","July","August","September","October","November","December"), 
     cex = 0.65)
legend("topright", legend=c("Normalized crimes","One Standard Deviation"),
       col=c("#4070E1"), lty=c(1), lwd =c(3,1))

#####     referred from https://rstudio-pubs-static.s3.amazonaws.com/      #####


#####     Basic Plot Data     #####

#naming and selecting columns 
crimeData <- data %>% 
  separate(Location, into = c("lat", "lon"), sep = ",") %>% 
  dplyr::select(`Case.Number`, Date, `Primary.Type`, 
                `Location.Description`, Ward, lon, lat, Arrest)

colnames(crimeData) <- c("case", "date", "offense", "setting",
                         "ward", "lon", "lat", "arrest") 

crimeData$ward <- as.character(crimeData$ward)

# cleaning longitude, latitude data 
crimeData$lon <- parse_number(crimeData$lon, na = ")")  
crimeData$lat <- parse_number(crimeData$lat, na = "(")

# formatting date variables 
crimeData$date <- as.POSIXlt(crimeData$date, format = "%m/%d/%Y %I:%M:%S %p")

crimeData$day <- as.factor(crimeData$date$mday)
crimeData$hour <- as.factor(crimeData$date$hour)
crimeData$month <- as.factor(crimeData$date$mon+1)
crimeData$year <- as.factor(crimeData$date$year+1900)
crimeData$weekday <- as.factor(crimeData$date$wday+1)

# the date column needs to be a "date" type for visualizations 
crimeData$date <- as.Date(crimeData$date, format = "%m/%d/%Y %I:%M:%S %p")

# formatting date variables 
crimeData$date <- as.POSIXlt(crimeData$date, format = "%m/%d/%Y %I:%M:%S %p")

crimeData$day <- as.factor(crimeData$date$mday)
crimeData$hour <- as.factor(crimeData$date$hour)
crimeData$month <- as.factor(crimeData$date$mon+1)
crimeData$year <- as.factor(crimeData$date$year+1900)
crimeData$weekday <- as.factor(crimeData$date$wday+1)

# the date column needs to be a "date" type for visualizations 
crimeData$date <- as.Date(crimeData$date, format = "%m/%d/%Y %I:%M:%S %p")



# summary table crime type and date
crimes_by_type_day <- crimeData %>% 
  group_by(offense, date) %>% 
  summarize(total = n()) %>%
  filter(offense != "CONCEALED CARRY LICENSE VIOLATION", 
         offense!= "NON-CRIMINAL",
         offense!= "NON-CRIMINAL (SUBJECT SPECIFIED)", 
         offense!= "NON - CRIMINAL", 
         offense!= "OTHER NARCOTIC VIOLATION", 
         offense!= "OTHER OFFENSE")

crimes_broad <- crimes_by_type_day %>% 
  ggplot(aes(x = date, y = total)) + 
  geom_line(color = "#333333") + 
  facet_wrap(~offense, scales = "free") + 
  ggtitle(label = "Breakdown of Crime Trends by Type of Offense") +
  scale_y_continuous(name = "Reports of Violations")

crimes_broad


### Logistic Regression

# remove all unnecessary variables
drops = c('X', 'Latitude', 'Longitude', 'Location.Description', 'Case.Number', 'Date', 'Block', 'IUCR', 'Primary.Type', 'Description', 'Domestic', 'Beat', 'Ward', 'Community.Area', 'FBI.Code', 'X.Coordinate', 'Y.Coordinate', 'Updated.On', 'Location', 'date', 'time', 'Max_Lat', 'Max_Long', 'Min_Lat', 'Min_Long', 'year') 
data_lr = data_subset[ , !(names(data_subset) %in% drops)]

# separate the last 10% of data to be test data
train = data_lr[1:(0.9*nrow(data_lr)), ]
test = data_lr[(0.9*nrow(data_lr)+1):nrow(data_lr), ]

# partition the train data into 10 portions as it cannot all be processed at once
inTrain = createDataPartition(train$levels, p = 0.55, list = F)
train.sub = train[inTrain, ]

partition = function(train, test){
  submission = data.frame(ID = test$ID)
  response = data.frame(lev = train$levels)
  crime = as.character(unique(train$levels))
  crime = sort(crime)
  for (i in crime){
    response[i] = 0
    response[i][response$lev == i,] = 1
    fit = glm(response[ ,i] ~ District + day + month + weekday, data = train, family = binomial)
    pred = predict(fit,test, type = "response")
    submission[i] = pred
    print(paste0(round((ncol(submission)/length(crime)*100), 0), '% completed'))
  }
  return(submission)
}

### Takes about 10 minutes to run
submission = partition(train.sub, test)

### Evaluation for Logistic Regression

crime_prob = submission[ ,-1]
predictions = colnames(crime_prob)[max.col(crime_prob, ties.method="first")]

second = crime_prob
# Takes about 10 minutes to run
for(i in 1:nrow(second)){
  column = colnames(second)[max.col(second[i, ], ties.method="first")]
  second[i, as.character(column)] = 0
  print(paste0(round((i/nrow(second)*100), 0), "% completed"))
}
second_max = colnames(second)[max.col(second, ties.method="first")]

third = second
# Takes about 10 minutes to run
for(i in 1:nrow(third)){
  column = colnames(third)[max.col(third[i, ], ties.method="first")]
  third[i, as.character(column)] = 0
  print(paste0(round((i/nrow(third)*100), 0), "% completed"))
}
third_max = colnames(third)[max.col(third, ties.method="first")]


correct1 = ifelse(predictions == test$levels, 1, 0)
correct2 = ifelse(predictions == test$levels | second_max == test$levels , 1, 0)
correct3 = ifelse(predictions == test$levels | second_max == test$levels | third_max == test$levels , 1, 0)
sum(correct1)/length(correct1)
sum(correct2)/length(correct2)
logr_accuracy = sum(correct3)/length(correct3)*100


### Multinomial Regression

# Takes about 10 minutes to run
mr <- multinom(levels ~ District + month + weekday, data = train)
summary(mr)

pred = predict(mr,test, type = "probs")
data.frame(pred)

# Evaluation
predictions_ml = colnames(pred)[max.col(pred, ties.method="first")]

second_ml = pred
for(i in 1:nrow(second_ml)){
  column = colnames(second_ml)[max.col(second_ml[i, ], ties.method="first")]
  second_ml[i, as.character(column)] = 0
  print(paste0(round((i/nrow(second_ml)*100), 0), "% completed"))
}
second_max_ml = colnames(second_ml)[max.col(second_ml, ties.method="first")]

third_ml = second_ml
for(i in 1:nrow(third_ml)){
  column = colnames(third_ml)[max.col(third_ml[i, ], ties.method="first")]
  third_ml[i, as.character(column)] = 0
  print(paste0(round((i/nrow(third_ml)*100), 0), "% completed"))
}
third_max_ml = colnames(third_ml)[max.col(third_ml, ties.method="first")]

correct1_ml = ifelse(predictions_ml == test$levels, 1, 0)
correct2_ml = ifelse(predictions_ml == test$levels | second_max_ml == test$levels , 1, 0)
correct3_ml = ifelse(predictions_ml == test$levels | second_max_ml == test$levels | third_max_ml == test$levels , 1, 0)
sum(correct1_ml)/length(correct1_ml)
sum(correct2_ml)/length(correct2_ml)
logr_accuracy_ml = sum(correct3_ml)/length(correct3_ml)*100

evaluation = data.frame("Logistic Regression" = logr_accuracy, "Multinomial Regression" = logr_accuracy_ml)
barplot(t(as.matrix(evaluation)), ylim=c(0,100), beside = TRUE, legend = TRUE, args.legend = c(xjust = 1, yjust = 0.5),
        ylab = "Accuracy (%)", xlab = "Model", main = "Percentage of Correct Classifications")

### Calculating Severity Ratio

w_probs = data.frame(matrix(0, nrow(submission), 1))
intensities = data.frame(colnames(submission))

# Takes about 5 minutes to run
for(i in 1:nrow(submission)){
  weighted_prob = 0
  tot_prob = 0
  for(j in 2:ncol(submission)){
  weighted_prob = submission[i, j] * as.numeric(intensities[j,1])
  tot_prob = weighted_prob + tot_prob
  }
  w_probs[i, 1] = tot_prob
  print(paste0(round((i/nrow(submission)*100), 0), "% completed"))
}

colnames(w_probs) = "E"
future = cbind(test$District, w_probs)
colnames(future) = c("District", "E")


totals = data.frame(aggregate(future$E, by = list(District = future$District), FUN = sum))

### Calculating Crime Rates

districts = data.frame(count(train, District))
districts$crime_rate = districts$n/nrow(train)

### remove district 13 as it only has 1 crime committed from 2012-2017
districts = districts[-13, ]

totals$x1 = totals$x * districts$crime_rate
totals$severity_ratio = totals$x1 / sum(totals$x1)

# 12,000 police units as mentioned by the Chicago PD
total_units = 12000

totals$Units = totals$severity_ratio*total_units

final_table = data.frame(cbind(totals$District, totals$x, totals$severity_ratio, totals$Units))
colnames(final_table) = c("District Number", "Total Crimes", "Severity Ratio", "Police Units Suggested")
final_table[ , 4] = ceiling(final_table[ ,4])
final_table[ ,3] = round(final_table[ ,3], 3)
final_table[ ,2] = ceiling(final_table[ ,2])

# Output the final results into a csv file
# write.csv(final_table, "Results.csv")

### Clustering for finding the centers in each district for optimal placement of units

#install.packages("class")

coordinates = data.frame(cbind(data_subset$District, data_subset$Latitude, data_subset$Longitude))
colnames(coordinates) = c("District", "Latitude", "Longitude")

list_centers = list()

# Takes about 5 minutes to run
for(i in 1:nrow(final_table)){
  x = final_table[i, 1]
  district_subset = coordinates[which(coordinates$District == x), ]
  k = final_table[i, 4]
  clustering = kmeans(district_subset[ ,2:3], k, nstart = 10)
  df = data.frame(clustering$centers)
  list_centers[[i]] = df
  names(list_centers[[i]]) = as.character(x)
  print(paste0(round((i/nrow(final_table)*100), 0),'% completed'))
  
}


### Getting a coordinate range for each District

district_coor = data_subset %>% dplyr::select(District, Latitude, Longitude)
district_coordinates = district_coor %>%
  group_by(District) %>%
  summarise(
    Max_Lat = max(Latitude, na.rm = T),
    Max_Long = max(Longitude, na.rm = T),
    Min_Lat = min(Latitude, na.rm = T),
    Min_Long = min(Longitude, na.rm = T)
  ) %>% arrange(District)

# Following lines take about 5 minutes to run
Max_Lat = lapply(data_subset$District, function(x) district_coordinates$Max_Lat[match(x, district_coordinates$District)])
Max_Long = lapply(data_subset$District, function(x) district_coordinates$Max_Long[match(x, district_coordinates$District)])
Min_Lat = lapply(data_subset$District, function(x) district_coordinates$Min_Lat[match(x, district_coordinates$District)])
Min_Long = lapply(data_subset$District, function(x) district_coordinates$Min_Long[match(x, district_coordinates$District)])

Max_Lat = structure(Max_Lat, row.names = c(NA, -n), class = "data.frame")
Max_Long = structure(Max_Long, row.names = c(NA, -n), class = "data.frame")
Min_Lat = structure(Min_Lat, row.names = c(NA, -n), class = "data.frame")
Min_Long = structure(Min_Long, row.names = c(NA, -n), class = "data.frame")

Max_Lat = t(Max_Lat)
Max_Long = t(Max_Long)
Min_Lat = t(Min_Lat)
Min_Long = t(Min_Long)

### Visualizing Results

chicago = geojson_read("Police_District_Boundary_View.geojson",  what = "sp")


plot_table = final_table
colnames(plot_table) = c("id", "crimes", "Severity", "units")
plot_table$id = as.character(plot_table$id)
spdf_fortified = tidy(chicago)
summary(spdf_fortified)
spdf_fortified = spdf_fortified %>%
  left_join(. , plot_table, by = c("id" = "id"))

# Visualizing severity ratio of crime across district. Note that district 31 has 0 crime, therefore is grey
map = ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = Severity, x = long, y = lat, group = group), colour = "white") +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  theme_void() +
  coord_map()
map


# Visualizing the suggested locations of police units just in police district 20
twenty = list_centers[[19]]
names(twenty) = c("lat", "long")

x_lim = c(as.numeric(district_coordinates[20, 5]), as.numeric(district_coordinates[20, 3]))
y_lim = c(as.numeric(district_coordinates[20, 4]), as.numeric(district_coordinates[20, 2]))
xlim
dis_twenty = map +  geom_point(data = twenty, aes(x = long, y = lat), shape = 23, fill="blue", color="darkred", size = 2) + coord_sf(xlim = x_lim, ylim = y_lim, expand = FALSE)
dis_twenty

############ END CODE #############

               
                         
               