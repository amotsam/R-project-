install.packages("geosphere")

library(geosphere)

library(tidyverse)

setwd("C:/Users/amotsam")

Uber_Jun <- read.csv('uber-raw-data-jun14.csv')

Uber_Jul <-  read.csv('uber-raw-data-jul14.csv')

Uber_Aug <-  read.csv('uber-raw-data-aug14.csv')

Uber_Sep <-  read.csv('uber-raw-data-sep14-first-2-weeks.csv')

Uber_Sep2<- read.csv('uber-raw-data-sep14.csv')

nyc_weather<- read.csv('nyc.csv')



# Filtering and joining external data 

nyc_weather<- subset(nyc_weather, as.Date(Date, format = "%Y-%m-%d") >= as.Date("2014-06-01",format = "%Y-%m-%d"))

nyc_weather<- subset(nyc_weather, as.Date(Date, format = "%Y-%m-%d") < as.Date("2014-09-17",format = "%Y-%m-%d"))

nyc_weather<- nyc_weather[,c(2,4,23)]

nyc_weather$Date<- as.Date(nyc_weather$Date)







           ##### DELETE THISS- begin #### 

Uber_Sep2<- subset(Uber_Sep2, as.Date(Date.Time, format = "%m/%d/%Y") > as.Date("9/16/2014",format = "%m/%d/%Y"))

for (i in 1:481698){

  print(i)

  flush.console() 



  Uber_Sep2[i,5]<-distm (c(Uber[i,3], Uber[i,2]), c(-73.9739882,40.7813241), fun = distHaversine)

}





###### Take dist museum< 1000, and aggregate demands per 15 minutes starting from 00:00





Uber_Sep2_Filtered<-subset(Uber_Sep2, V5<=1000)

Uber_Sep2_Filtered$interval<-substring(Uber_Sep2_Filtered$Date.Time, first = 10, last = 18)

Uber_Sep2_Filtered$interval<-floor_date(as.POSIXct(Uber_Sep2_Filtered$interval,format="%H:%M:%S"), "15 mins")

Uber_Sep2_Filtered$interval<-substring(Uber_Sep2_Filtered$interval, first = 11, last = 19)



Uber_Sep2_Filtered$Date<- as.POSIXct(substring(Uber_Sep2_Filtered$Date.Time, first = 1, last = 9),format="%m/%d/%Y") 



intervals4<-Uber_Sep2_Filtered %>% 

  group_by(Date, interval) %>%

  summarize(n())



ggplot(data = intervals4) +

  geom_bar(mapping = aes(x = interval, y = `n()`), stat = "identity")+

  coord_flip()+

  theme(text = element_text(size=7),

        axis.text.x = element_text(angle=90, hjust=1))



######################################### september test results- DELETEEEEEE END ############

write.csv(Uber_June,"Uber_June.csv")

write.csv(Uber_Jul,"Uber_Jul.csv")

write.csv(Uber_Aug,"Uber_Aug.csv")

write.csv(Uber_Sep,"Uber_Sep.csv")



###################### Rearrange Data #####################################

# Calculate the dist for every Loc from the museum 

Uber_Jun$Dist_Museum <- NA

Uber_Jul$Dist_Museum <- NA

Uber_Aug$Dist_Museum <- NA

Uber_Sep$Dist_Museum <- NA



### for testing, bind all data and than calc

Uber_SEPPP$Date.Time<- as.POSIXct(Uber_SEPPP$Date.Time, format= "%Y-%m-%d %H:%M:%S")

Uber_SEPPP$Date.Time<- format(Uber_SEPPP$Date.Time, "%m/%d/%Y %H:%M:%S")

Uber_SEPPP<-Uber_SEPPP[,-7]



#for (i in 1:2835678){

#  print(i)

#  flush.console() 

  

#  Uber[i,6]<-distm (c(Uber[i,3], Uber[i,2]), c(-73.9739882,40.7813241), fun = distHaversine)

#}



Uber_SEPPP<- subset(Uber_Filtered,Uber_Filtered$V7>=55303 )



Uber_Filtered<- subset(Uber, Dist_Museum<=1000)

for (i in 1:68752) { 

  Uber_Filtered[i,7] <- i 

}

Uber_Filtered<-subset(Uber_Filtered,Uber_Filtered$V7<55303 )

Uber_Filtered<-Uber_Filtered[,-7]

Uber_Filtered<- rbind(Uber_Filtered,Uber_SEPPP)



Uber_Filtered$Date.Time<- as.POSIXct(Uber_Filtered$Date.Time, format="%m/%d/%Y %H:%M:%S")

Uber_Filtered<- Uber_Filtered[order(Uber_Filtered$Date.Time),]



Uber_Jun_Filtered<- subset(Uber_Filtered, Date.Time >= as.Date("6/1/2014"))

Uber_Jun_Filtered<- subset(Uber_Jun_Filtered, Date.Time < as.Date("7/1/2014",format = "%m/%d/%Y"))

Uber_Jul_Filtered<- subset(Uber_Filtered, Date.Time >= as.Date("2014/07/01"))

Uber_Jul_Filtered<- subset(Uber_Jul_Filtered, Date.Time < as.Date("2014/08/01"))

Uber_Aug_Filtered<- subset(Uber_Filtered, Date.Time >= as.Date("2014/08/01"))

Uber_Aug_Filtered<-subset(Uber_Aug_Filtered, Date.Time < as.Date("2014/09/01"))

Uber_Sep_Filtered<- subset(Uber_Filtered, Date.Time >= as.Date("2014/09/01"))





# Find the day of every date 

Uber_Filtered$day<-weekdays(as.Date(Uber_Filtered$interval))



# How many demands every interval has per date

intervals<-Uber_Filtered %>% 

  group_by(interval) %>%

  summarize(n())



names(intervals)[1]<-paste("Date")

names(intervals)[2]<-paste("sum_of_interval")



# seperate the day and hour of every date, level the day. 

intervals$day<- weekdays(as.Date(intervals$Date))

intervals$day <- factor(intervals$day, levels= c("Sunday", "Monday", 

                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

intervals$hour<- strftime(intervals$Date, format="%H:%M:%S") 

intervals$Date<- as.Date(intervals$Date)



intervals<-merge(intervals, nyc_weather,by="Date", all.intervals=TRUE)

levels <- levels(intervals$Events)

levels[length(levels) + 1] <- "Shiny"

intervals$Events <- factor(intervals$Events, levels = levels)

intervals$Events[is.na(intervals$Events)]<- "Shiny"



# Aggregate all demnads for a certain day & interval time 

intervals_aggregated<-aggregate(intervals$sum_of_interval, by=list(day=intervals$day,hour=intervals$hour), FUN=sum)

names(intervals_aggregated)[3]<-paste("demand_for_picks")

# Find the number of every interval appearance in the data (to calc the mean)



table1<-intervals %>% group_by(day,hour) %>%

  summarize(num_of_interval = n())  

intervals_aggregated <- intervals_aggregated[order(intervals_aggregated$day),] 

intervals_aggregated$sum_of_interval<-table1$num_of_interval









########################## Exploratory analysis and background ########################



# What is the effect of "day in a week" and "time in a day" of the demand for Uber pickups? 

# lets see, for every day of the week, how much demand we have? 

Demands_by_day<-aggregate(intervals_aggregated$demand_for_picks, by=list(day=intervals_aggregated$day), FUN=sum)



# Barchart 

ggplot(data = Demands_by_day) +

  geom_bar(mapping = aes(x = day, y = x), stat = "identity")



### Explore the distubtion of demand per weeks in month, Are they behave differently? 



Uber_Jun_Filtered_2_weeks<- subset(Uber_Jun_Filtered, as.Date(Date.Time, format = "%m/%d/%Y") < as.Date("6/16/2014",format = "%m/%d/%Y"))

intervals_jun<-Uber_Jun_Filtered_2_weeks %>% 

  group_by(interval) %>%

  summarize(n())



# seperate the day and hour of every date, level the day. 

intervals_jun$day<- weekdays(as.Date(intervals_jun$interval))

intervals_jun$day <- factor(intervals_jun$day, levels= c("Sunday", "Monday", 

                                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

intervals_jun$hour<- strftime(intervals_jun$interval, format="%H:%M:%S") 



# Find the number of every interval appearance in the data (to calc the mean)



table_Jun<-intervals_jun %>% group_by(day,hour) %>%

  summarize(num_of_interval = n()) 

intervals_aggregated_Jun<-table_Jun

intervals_aggregated_Jun<- intervals_aggregated_Jun[order(intervals_aggregated_Jun$day),] 

intervals_aggregated_Jun$sum_of_interval<-table_Jun$num_of_interval



# Aggregate all demnads for a certain day & interval time 

intervals_aggregated_Jun<-aggregate(intervals_jun$`n()`, by=list(day=intervals_jun$day,hour=intervals_jun$hour), FUN=sum)

names(intervals_aggregated_Jun)[3]<-paste("demand_for_picks")



ggplot(data = intervals_aggregated_Jun) +

  geom_bar(mapping = aes(x = hour, y = demand_for_picks, color=day), stat = "identity")+

  coord_flip()+

  theme(text = element_text(size=7),

        axis.text.x = element_text(angle=90, hjust=1))  # first 2 weeks of Jun



Uber_Jul_Filtered_2_weeks<- subset(Uber_Jul_Filtered, as.Date(Date.Time, format = "%m/%d/%Y") < as.Date("7/16/2014",format = "%m/%d/%Y"))

intervals_jul<-Uber_Jul_Filtered_2_weeks %>% 

  group_by(interval) %>%

  summarize(n())



# seperate the day and hour of every date, level the day. 

intervals_jul$day<- weekdays(as.Date(intervals_jul$interval))

intervals_jul$day <- factor(intervals_jul$day, levels= c("Sunday", "Monday", 

                                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

intervals_jul$hour<- strftime(intervals_jul$interval, format="%H:%M:%S") 



# Find the number of every interval appearance in the data (to calc the mean)



table_Jul<-intervals_jul %>% group_by(day,hour) %>%

  summarize(num_of_interval = n()) 

intervals_aggregated_Jul<-table_Jul

intervals_aggregated_Jul<- intervals_aggregated_Jul[order(intervals_aggregated_Jul$day),] 

intervals_aggregated_Jul$sum_of_interval<-table_Jul$num_of_interval



# Aggregate all demnads for a certain day & interval time 

intervals_aggregated_Jul<-aggregate(intervals_jul$`n()`, by=list(day=intervals_jul$day,hour=intervals_jul$hour), FUN=sum)

names(intervals_aggregated_Jul)[3]<-paste("demand_for_picks")



ggplot(data = intervals_aggregated_Jul) +

  geom_bar(mapping = aes(x = hour, y = demand_for_picks, color=day), stat = "identity")+

  coord_flip()+

  theme(text = element_text(size=7),

        axis.text.x = element_text(angle=90, hjust=1))  # first 2 weeks of Jul



Uber_Aug_Filtered_2_weeks<- subset(Uber_Aug_Filtered, as.Date(Date.Time, format = "%m/%d/%Y") < as.Date("8/16/2014",format = "%m/%d/%Y"))

intervals4<-Uber_Aug_Filtered_2_weeks %>% 

  group_by(interval) %>%

  summarize(n())



# seperate the day and hour of every date, level the day. 

intervals4$day<- weekdays(as.Date(intervals4$interval))

intervals4$day <- factor(intervals4$day, levels= c("Sunday", "Monday", 

                                                   "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

intervals4$hour<- strftime(intervals4$interval, format="%H:%M:%S") 



# Find the number of every interval appearance in the data (to calc the mean)



table6<-intervals4 %>% group_by(day,hour) %>%

  summarize(num_of_interval = n()) 

intervals_aggregated6<-table6

intervals_aggregated6<- intervals_aggregated6[order(intervals_aggregated6$day),] 

intervals_aggregated6$sum_of_interval<-table6$num_of_interval



# Aggregate all demnads for a certain day & interval time 

intervals_aggregated6<-aggregate(intervals4$`n()`, by=list(day=intervals4$day,hour=intervals4$hour), FUN=sum)

names(intervals_aggregated6)[3]<-paste("demand_for_picks")



ggplot(data = intervals_aggregated6) +

  geom_bar(mapping = aes(x = hour, y = demand_for_picks, color=day), stat = "identity")+

  coord_flip()+

  theme(text = element_text(size=7),

        axis.text.x = element_text(angle=90, hjust=1)) # first 2 weeks of Aug 

  

 ############### September 2 first weeks 

  Uber_Sep_Filtered$day<-weekdays(as.Date(Uber_Sep_Filtered$interval))

  

  # How many demands every interval has per date

  intervals_Sep_first<-Uber_Sep_Filtered %>% 

    group_by(interval) %>%

    summarize(n())

  

  # seperate the day and hour of every date, level the day. 

  intervals_Sep_first$day<- weekdays(as.Date(intervals_Sep_first$interval))

  intervals_Sep_first$day <- factor(intervals_Sep_first$day, levels= c("Sunday", "Monday", 

                                                   "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

  intervals_Sep_first$hour<- strftime(intervals_Sep_first$interval, format="%H:%M:%S") 

  

  # Aggregate all demnads for a certain day & interval time 

  intervals_aggregated5<-aggregate(intervals_Sep_first$`n()`, by=list(day=intervals_Sep_first$day,hour=intervals_Sep_first$hour), FUN=sum)

  names(intervals_aggregated5)[3]<-paste("demand_for_picks")

  

  # Find the number of every interval appearance in the data (to calc the mean)

  

  table5<-intervals_Sep_first %>% group_by(day,hour) %>%

    summarize(num_of_interval = n())  

  intervals_aggregated5 <- intervals_aggregated5[order(intervals_aggregated5$day),] 

  intervals_aggregated5$sum_of_interval<-table5$num_of_interval

  

  # distribution of first 2 weeks of September

  ggplot(data = intervals_aggregated5) +

    geom_bar(mapping = aes(x = hour, y = demand_for_picks, color=day), stat = "identity")+

    coord_flip()+

    theme(text = element_text(size=7),

          axis.text.x = element_text(angle=90, hjust=1)) # Sep 2 first weeks 

  

   

### june last 2 weeks 

  

  Uber_Jun_Filtered_2_weeks<- subset(Uber_June_Filtered, as.Date(Date.Time, format = "%m/%d/%Y") > as.Date("6/16/2014",format = "%m/%d/%Y"))

  intervals_jun<-Uber_Jun_Filtered_2_weeks %>% 

    group_by(interval) %>%

    summarize(n())

  

  # seperate the day and hour of every date, level the day. 

  intervals_jun$day<- weekdays(as.Date(intervals_jun$interval))

  intervals_jun$day <- factor(intervals_jun$day, levels= c("Sunday", "Monday", 

                                                           "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

  intervals_jun$hour<- strftime(intervals_jun$interval, format="%H:%M:%S") 

  

  # Find the number of every interval appearance in the data (to calc the mean)

  

  table_Jun<-intervals_jun %>% group_by(day,hour) %>%

    summarize(num_of_interval = n()) 

  intervals_aggregated_Jun<-table_Jun

  intervals_aggregated_Jun<- intervals_aggregated_Jun[order(intervals_aggregated_Jun$day),] 

  intervals_aggregated_Jun$sum_of_interval<-table_Jun$num_of_interval

  

  # Aggregate all demnads for a certain day & interval time 

  intervals_aggregated_Jun<-aggregate(intervals_jun$`n()`, by=list(day=intervals_jun$day,hour=intervals_jun$hour), FUN=sum)

  names(intervals_aggregated_Jun)[3]<-paste("demand_for_picks")

  

  ggplot(data = intervals_aggregated_Jun) +

    geom_bar(mapping = aes(x = hour, y = demand_for_picks, color=day), stat = "identity")+

    coord_flip()+

    theme(text = element_text(size=7),

          axis.text.x = element_text(angle=90, hjust=1)) # jun last 2 weeks

  

  

########### july last 2 weeks

  Uber_Jul_Filtered_2_weeks<- subset(Uber_Jul_Filtered, as.Date(Date.Time, format = "%m/%d/%Y") > as.Date("7/16/2014",format = "%m/%d/%Y"))

  intervals_jul<-Uber_Jul_Filtered_2_weeks %>% 

    group_by(interval) %>%

    summarize(n())

  

  # seperate the day and hour of every date, level the day. 

  intervals_jul$day<- weekdays(as.Date(intervals_jul$interval))

  intervals_jul$day <- factor(intervals_jul$day, levels= c("Sunday", "Monday", 

                                                     "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

  intervals_jul$hour<- strftime(intervals_jul$interval, format="%H:%M:%S") 

  

  # Find the number of every interval appearance in the data (to calc the mean)

  

  table_Jul<-intervals_jul %>% group_by(day,hour) %>%

    summarize(num_of_interval = n()) 

  intervals_aggregated_Jul<-table_Jul

  intervals_aggregated_Jul<- intervals_aggregated_Jul[order(intervals_aggregated_Jul$day),] 

  intervals_aggregated_Jul$sum_of_interval<-table_Jul$num_of_interval

  

  # Aggregate all demnads for a certain day & interval time 

  intervals_aggregated_Jul<-aggregate(intervals_jul$`n()`, by=list(day=intervals_jul$day,hour=intervals_jul$hour), FUN=sum)

  names(intervals_aggregated_Jul)[3]<-paste("demand_for_picks")

  

  ggplot(data = intervals_aggregated_Jul) +

    geom_bar(mapping = aes(x = hour, y = demand_for_picks, color=day), stat = "identity")+

    coord_flip()+

    theme(text = element_text(size=7),

          axis.text.x = element_text(angle=90, hjust=1)) # last 2 weeks of jul



  

# last 2 weeks of Aug



  

  Uber_Aug_Filtered_2_weeks<- subset(Uber_Aug_Filtered, as.Date(Date.Time, format = "%m/%d/%Y") > as.Date("8/16/2014",format = "%m/%d/%Y"))

  intervals4<-Uber_Aug_Filtered_2_weeks %>% 

  group_by(interval) %>%

  summarize(n())

  

  # seperate the day and hour of every date, level the day. 

  intervals4$day<- weekdays(as.Date(intervals4$interval))

  intervals4$day <- factor(intervals4$day, levels= c("Sunday", "Monday", 

                                                     "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

  intervals4$hour<- strftime(intervals4$interval, format="%H:%M:%S") 

  

  # Find the number of every interval appearance in the data (to calc the mean)

  

  table6<-intervals4 %>% group_by(day,hour) %>%

    summarize(num_of_interval = n()) 

  intervals_aggregated6<-table6

  intervals_aggregated6<- intervals_aggregated6[order(intervals_aggregated6$day),] 

  intervals_aggregated6$sum_of_interval<-table6$num_of_interval

  

  # Aggregate all demnads for a certain day & interval time 

  intervals_aggregated6<-aggregate(intervals4$`n()`, by=list(day=intervals4$day,hour=intervals4$hour), FUN=sum)

  names(intervals_aggregated6)[3]<-paste("demand_for_picks")

  

  ggplot(data = intervals_aggregated6) +

    geom_bar(mapping = aes(x = hour, y = demand_for_picks, color=day), stat = "identity")+

    coord_flip()+

    theme(text = element_text(size=7),

          axis.text.x = element_text(angle=90, hjust=1)) # Aug last 2 weeks 

  



  intervals_aggregated6 # Aug last 2 weeks 

  ggplot(data = intervals_aggregated6) +

    geom_bar(mapping = aes(x = hour, y = demand_for_picks, color=day), stat = "identity")+

    coord_flip()+

    theme(text = element_text(size=7),

          axis.text.x = element_text(angle=90, hjust=1))

  

  intervals_aggregated_Jul# jul last 2 weeks 

  ggplot(data = intervals_aggregated_Jul) +

    geom_bar(mapping = aes(x = hour, y = demand_for_picks, color=day), stat = "identity")+

    coord_flip()+

    theme(text = element_text(size=7),

          axis.text.x = element_text(angle=90, hjust=1))

  intervals_aggregated_Jun  # jun last 2 weeks

  ggplot(data = intervals_aggregated_Jun) +

    geom_bar(mapping = aes(x = hour, y = demand_for_picks, color=day), stat = "identity")+

    coord_flip()+

    theme(text = element_text(size=7),

          axis.text.x = element_text(angle=90, hjust=1)) #last 2 weeks plots

  

  ######### june

  intervals2<-Uber_June_Filtered %>% 

    group_by(interval) %>%

    summarize(n())

  

  # seperate the day and hour of every date, level the day. 

  intervals2$day<- weekdays(as.Date(intervals2$interval))

  intervals2$day <- factor(intervals2$day, levels= c("Sunday", "Monday", 

                                                     "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

  intervals2$hour<- strftime(intervals2$interval, format="%H:%M:%S") 

  

  # Find the number of every interval appearance in the data (to calc the mean)

  

  table2<-intervals2 %>% group_by(day,hour) %>%

    summarize(num_of_interval = n()) 

  intervals_aggregated2<-table2

  intervals_aggregated2<- intervals_aggregated2[order(intervals_aggregated2$day),] 

  intervals_aggregated2$sum_of_interval<-table1$num_of_interval

  

  # Aggregate all demnads for a certain day & interval time 

  intervals_aggregated2<-aggregate(intervals2$`n()`, by=list(day=intervals2$day,hour=intervals2$hour), FUN=sum)

  names(intervals_aggregated2)[3]<-paste("demand_for_picks")

  

  ggplot(data = intervals_aggregated2) +

    geom_bar(mapping = aes(x = hour, y = demand_for_picks, color=day), stat = "identity")+

    coord_flip()+

    theme(text = element_text(size=7),

          axis.text.x = element_text(angle=90, hjust=1)) # all june

  

  

  ## all months last 2 weeks 

  intervals_aggregated_last_2_weeks<- rbind(intervals_aggregated6,intervals_aggregated_Jul,intervals_aggregated_Jun)

  ggplot(data = intervals_aggregated_last_2_weeks) +

    geom_bar(mapping = aes(x = hour, y = demand_for_picks, color=day), stat = "identity")+

    coord_flip()+

    theme(text = element_text(size=7),

          axis.text.x = element_text(angle=90, hjust=1))

  

## Sep last 2 weeks #### DELETTTTTTEEEEEEEEEEEEEEEEEEEEEE

  Uber_Sep_last_2_weeks<- Uber_Sep2

  





  

  

### What is the relationship between hour and num_of_pickups?       hour ~ num_of_pickups

# We can see that the mass of the demand is in 6:00-8:45

ggplot(data = intervals_aggregated) +

  geom_bar(mapping = aes(x = hour, y = demand_for_picks, color=day), stat = "identity")+

  coord_flip()+

  theme(text = element_text(size=7),

        axis.text.x = element_text(angle=90, hjust=1))





# calculate the median of demand for every interval- used for Poisson regression 



intervals_aggregated$median_of_demand<- intervals_aggregated$demand_for_picks / intervals_aggregated$sum_of_interval



## plot the median pickup of every hour 

ggplot(data = intervals_aggregated) +

  geom_bar(mapping = aes(x = hour, y = median_of_demand, color=day), stat = "identity")+

  coord_flip()+

  theme(text = element_text(size=7),

        axis.text.x = element_text(angle=90, hjust=1)) 



## plot the median pickup of every day 

ggplot(data = intervals_aggregated) +

  geom_bar(mapping = aes(x = day, y = median_of_demand), stat = "identity")+

  theme(text = element_text(size=7),

        axis.text.x = element_text(angle=90, hjust=1)) 

########!!!!!!!!!!!!~~~~~~~~~Predictions ~~~~~~~~~~~~~~~~~~~~~!!!!!!!!!!!!!!!!!#############



 ###################### Linear Model  



# We want to see if certain interval has strange behaviour, as we can see, we have few outliers and that data is not very noisy

 plot(intervals_aggregated$day,intervals_aggregated$demand_for_picks,       # day ~ demand of picks

      bg="lightblue",

      col="black",pch=21,frame=FALSE)

 

# Same here- few outliers that are not suppose to affect our model 

 ggplot(data = intervals_aggregated, mapping = aes(x = hour, y = demand_for_picks)) + 

   geom_boxplot()+

   coord_flip()+

 theme(text = element_text(size=7),

       axis.text.x = element_text(angle=90, hjust=1)) 

  

 ###### Model 1-  Build linear model regression, 

 #### We keep the median decimal for more accurate prediction, we will round that number on test results 

 names(intervals)[2]<-paste("sum_of_interval")

 

 fit3 <- lm(sum_of_interval ~Date+hour+day+Events+Mean.TemperatureF ,data = intervals)

 summary(fit3) 

 

 fit1 <- lm(median_of_demand ~ hour, data = intervals_aggregated)

 summary(fit1)

 

 

 # The "hour" (interval) variable explain much better the demand for pickups than "day" var.

 # lets try to improve it with multiple regression 

 

# Best model for not median sum of intervals

 intervals$Sample <- runif(9457, min = 0, max = 1)

 Training <- subset(intervals, intervals$Sample > 0.3)

 Test <- subset(intervals, intervals$Sample <= 0.3)

 poisson.model<-glm(sum_of_interval ~hour+day+Date+Events+Mean.TemperatureF, data = Training, family = quasipoisson(link = "log"))

 summary(poisson.model)

 Test$Prediction <- round(predict(poisson.model, newdata = Test, type = "response"))



 

 RMSE3<- sqrt(mean((Test$Prediction-Test$sum_of_interval)^2))

 RMSE3 # 4.001

 

#Lets calc the RMSE for our model



intervals_aggregated$Sample <- runif(672, min = 0, max = 1)

Training <- subset(intervals_aggregated, intervals_aggregated$Sample > 0.3)

Test <- subset(intervals_aggregated, intervals_aggregated$Sample <= 0.3)





fit22 <- lm(median_of_demand~day + hour,data = Training)

summary(fit22)

Test$Prediction <- round(predict(fit22,newdata=Test))

RMSE2<- sqrt(mean((Test$Prediction-Test$median_of_demand)^2))

Test$Prediction[Test$Prediction<0]<-0

RMSE2  # 2.837



# ~~~~~~~~~~~~Lets try to shrink the RMSE by training the data more- K folds cross validations





####################################################################################







##### Model 2- the Poisson regression, Lambda- median of demand of every interval time of every day.

## best model for Median calc data 

intervals_aggregated$Sample <- runif(672, min = 0, max = 1)

Training <- subset(intervals_aggregated, intervals_aggregated$Sample > 0.3)

Test <- subset(intervals_aggregated, intervals_aggregated$Sample <= 0.3)



poisson.model<-glm(median_of_demand~day+hour, data = Training, family = quasipoisson(link = "log"))

summary(poisson.model)

Test$Prediction <- round(predict(poisson.model, newdata = Test, type = "response"))

test_d$num_of_pickups <- round(predict(poisson.model, newdata = test_d, type = "response"))



RMSE3<- sqrt(mean((Test$Prediction-Test$median_of_demand)^2))

RMSE3  # 2.52, , best RMSE for median based model. 



##### Model 3- The poisson regression with external vars



intervals$Sample <- runif(9457, min = 0, max = 1)

Training <- subset(intervals, intervals$Sample > 0.3)

Test <- subset(intervals, intervals$Sample <= 0.3)

poisson.model<-glm(sum_of_interval ~hour+day+Date+Events+Mean.TemperatureF, data = Training, family = quasipoisson(link = "log"))

summary(poisson.model)

Test$Prediction <- round(predict(poisson.model, newdata = Test, type = "response"))





RMSE3<- sqrt(mean((Test$Prediction-Test$sum_of_interval)^2))

RMSE3 

### DELETE ### 

nyc_weather_test<- read.csv('nyc.csv')

nyc_weather_test<- subset(nyc_weather_test, as.Date(Date, format = "%Y-%m-%d") >= as.Date("2014-09-17",format = "%Y-%m-%d"))

nyc_weather_test<- subset(nyc_weather_test, as.Date(Date, format = "%Y-%m-%d") <= as.Date("2014-09-30",format = "%Y-%m-%d"))

nyc_weather_test<- nyc_weather_test[,c(2,4,23)]

nyc_weather_test$Date<- as.Date(nyc_weather_test$Date)

nyc_weather_test$Date<-as.POSIXct(nyc_weather_test$Date)





intervals4<-merge(intervals4, nyc_weather_test,by="Date", all.intervals=TRUE)

Training <- intervals4

Test <- intervals4

poisson.model<-glm(sum_of_interval ~hour+day+Date+Mean.TemperatureF, data = Training, family = quasipoisson(link = "log"))

summary(poisson.model)

Test$Prediction <- round(predict(poisson.model, newdata = Test, type = "response"))

#END# 

 



### DELETE THIS 

setdiff(test_d$Time_Interval,test_d5$date_time) # intervals that are missing in the prediction set



################ prediction ############################### 

ifMinus<- function(v){

  if(v<0)

    retrun (0)

}

test_d <- read.csv('test_data.csv')

test_d$num_of_pickups<- NA



test_d$day<- weekdays(as.Date(test_d$Time_Interval))

test_d$hour<- strftime(test_d$Time_Interval, format="%H:%M:%S")



Training<- intervals_aggregated

fit22 <- lm(median_of_demand~day + hour,data = Training)

test_d$num_of_pickups<- round(predict(fit22,newdata=test_d))

test_d$num_of_pickups[test_d$num_of_pickups<0]<-0



write.csv(test_d,"test_data.csv")
