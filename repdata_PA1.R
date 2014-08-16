##Read file from 
ds<-read.csv("activity.csv")

## change date to date format
ds$date<-as.Date(ds$date,"%Y-%m-%d")

## histogram of the total number of steps taken each day
day.sum<-with(ds, tapply(steps, date, sum)) 
par(mar=c(10,4,4,2))
hist(day.sum,breaks=15,main="Number of Steps taken per day November 2012",xlab="Number of Steps",ylab="Number of Days",col="red")

##the mean total number of steps taken per day
mean(day.sum,na.rm=T)

##the median total number of steps taken per day
median(day.sum,na.rm=T)

##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, ##averaged across all days (y-axis)

int.mean<-with(ds, tapply(steps, interval, mean,na.rm=T)) 
new<-as.data.frame.table(int.mean, responseName = "Mean")
names(new)[names(new) == 'Var1'] <- 'Interval'
new$Interval<-as.character(new$Interval)
par(mar=c(12,4,4,2))
with(new,plot(Interval,Mean,type="l",xaxt="n"))
axis(1,at = seq(100,2300, by = 100),las=2)

##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_interval<-new[which.max(new$Mean),]
as.numeric(max_interval[1,1])

##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

sum(is.na(ds$steps))

##filling missing values in the dataset, using the mean for the appropriate interval
fill<-ds
new<-aggregate(steps ~ interval,data = ds,FUN=function(x) mean(x) )
new$interval<-as.numeric(new$interval)

for(i in 1:as.numeric(nrow(fill))) {if(is.na(fill[i,1])){
		a<-as.numeric(fill[i,3])
		b<-subset(new,interval==a)
		fill[i,1]<-b[1,2]
	}
}

##Make a histogram of the total number of steps taken each day 
day.fill<-with(fill, tapply(steps, date, sum)) 
par(mar=c(10,4,4,2))
hist(day.fill,breaks=15,main="Number of Steps taken per day Nov. 2012 (Missing Values Replaced)",xlab="Number of Steps",ylab="Number of Days",col="red")

##Calculate and report the mean using replacement values
mean(day.fill)


##median total number of steps taken per day using replacement values
median(day.fill)

##Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

##Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a ##given date is a weekday or weekend day.

week<-fill
week$day <- weekdays(week$date)
  attach(week)
  week$day[day=="Sunday"] <- "Weekend"
  week$day[day=="Saturday"] <- "Weekend"
  week$day[day=="Monday"] <- "Weekday"
  week$day[day=="Tuesday"] <- "Weekday"
  week$day[day=="Wednesday"] <- "Weekday"
  week$day[day=="Thursday"] <- "Weekday"
  week$day[day=="Friday"] <- "Weekday"
  detach(week)
  
a<-aggregate(steps ~ day+interval,data = week,FUN=function(x) mean(x) )

library(lattice)

xyplot(steps ~ interval | day, data = a,type="l",ylab="Number of Steps",layout=c(1,2))














