## Code block #1 -- Read file and convert date to date format
setwd("C:/Users/DLeddy/Documents/GitHub/RepData_PeerAssessment1")
activity_df<-read.csv("./activity/activity.csv",colClasses=c("integer","character","integer"))
activity_df$date<-as.Date(activity_df$date, "%Y-%m-%d")


## Code block #2 -- Total number of steps per day, histogram & mean/median
library(dplyr)
activity_by_day<- group_by(activity_df, date)
steps_by_day<-summarize(activity_by_day, daily_steps=sum(steps, na.rm=TRUE))

hist(steps_by_day$daily_steps, breaks=10, main="Total Steps by Day")

print(paste("Median num steps per day: ", median(steps_by_day$daily_steps)))
print(paste("Mean num steps per day: ", round(mean(steps_by_day$daily_steps),0)))

## Code block #3 -- Time series summary across intervals, Max interval
activity_by_interval<- group_by(activity_df, interval)
steps_by_interval<-summarize(activity_by_interval, int_steps=round(mean(steps, na.rm=TRUE),1))
plot(steps_by_interval$interval, steps_by_interval$int_steps, type="l", ylab="Avg # Steps", xlab="Interval", main="Avg Steps by 5-Minute Interval")

print(paste("Interval with highest avg num steps: ", steps_by_interval$interval[steps_by_interval$int_steps==max(steps_by_interval$int_steps)]))

##Code block #4a -- Calculate # of NAs, substitute interval mean for NAs
tot_nas<-sum(is.na(activity_df$steps))
print(paste("Total NAs = ", tot_nas))

activity_noNAs<-activity_df
for (i in 1:length(activity_noNAs$steps)) {
     if(is.na(activity_noNAs$steps[i])) {
          activity_noNAs$steps[i]<-steps_by_interval$int_steps[steps_by_interval$interval==activity_noNAs$interval[i]]
     }
}

##Code block 4b -- Redo histogram, mean, medium on new data
activity_by_day_noNAs<- group_by(activity_noNAs, date)
steps_by_day_noNAs<-summarize(activity_by_day_noNAs, daily_steps=sum(steps, na.rm=TRUE))

hist(steps_by_day_noNAs$daily_steps, breaks=10, main="Total Steps by Day")

print(paste("Median num steps per day: ", median(steps_by_day_noNAs$daily_steps)))
print(paste("Mean num steps per day: ", round(mean(steps_by_day_noNAs$daily_steps),0)))

c1<-c("Mean", "Median")
c2<-c(median(steps_by_day$daily_steps), round(mean(steps_by_day$daily_steps),0))
c3<-c(median(steps_by_day_noNAs$daily_steps), round(mean(steps_by_day_noNAs$daily_steps),0
comparison<-cbind(c2, c3)
rownames(comparison)<-c1
colnames(comparison)<-c("Original", "NAs Removed")
print(comparison)

##Code segment #5a -- Weekend vs. Weekday, then get interval means
activity_noNAs$day<-weekdays(as.POSIXlt(activity_noNAs$date))
activity_noNAs$workweek<-"Weekday"
wkend_days<-c("Saturday", "Sunday")
activity_noNAs$workweek[activity_noNAs$day %in% wkend_days] <- "Weekend"
activity_noNAs$workweek<-as.factor(activity_noNAs$workweek)

activity_by_interval_noNAs<- group_by(activity_noNAs, interval, workweek)
steps_by_interval_noNAs<-summarize(activity_by_interval_noNAs, int_steps=round(mean(steps, na.rm=TRUE),1))

##Code segment #5b -- plot with factors
library(ggplot2)

h<-qplot(interval, int_steps, data=steps_by_interval_noNAs, facets=.~workweek, geom="line", ylab="Steps", main="Avg Steps by Day Type") 
h

