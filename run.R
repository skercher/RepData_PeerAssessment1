# !/usr/bin/env Rscript

setwd("~/coursera/reproducible-research/RepData_PeerAssessment1")

zipFile <- "activity.zip"
fileName <- "activity.csv"

if(!file.exists(fileName)){	
	unzip(zipFile)
}

# read csv into dataset
dataFrame <- read.csv(fileName, as.is=TRUE)

# omit all NA
dataFrame2 <- na.omit(dataFrame)

# aggregate steps per day
tableSteps <- aggregate(steps ~ date, dataFrame2 , sum)

# create histogram for Total Setps in a day
hist(tableSteps$steps,xlab = "Total Steps In a Day", main = "Histogram of TOTAL #  of Steps per Day", col=4)

# mean steps
mean(tableSteps$steps,na.rm = TRUE)

# median steps
median(tableSteps$steps,na.rm = TRUE)

# aggregate steps per interval
tableIntervalSteps <- aggregate(steps ~ interval, dataFrame2, mean)

# create plot steps per interval
plot(tableIntervalSteps$interval,tableIntervalSteps$steps,type="l",col=1,main="Average number of Daily Steps", xlab = "Interval", ylab = "Average #  of steps")

# max average steps interval
maxAverageStepsInterval <- which.max(tableIntervalSteps$steps)

# table max interval steps
tableIntervalSteps[maxAverageStepsInterval,]

# missing data NA
dataFrameNA <- dataFrame[!complete.cases(dataFrame),]

# num of rows
nrow(dataFrameNA)

# perform imputation 
for(i in 1:nrow(dataFrame)){
        if(is.na(dataFrame$steps[i])){
                intervalVal <- dataFrame$interval[i]
                rowId <- which(tableIntervalSteps$interval == intervalVal)
                stepsVal <- tableIntervalSteps$steps[rowId]
                dataFrame$steps[i] <- stepsVal
        }
}

# aggregate steps per date to get total steps in day
tableDateStepsImpute <- aggregate(steps ~ date, dataFrame, sum)

# create histogram of total number of steps in a day
hist(tableDateStepsImpute$steps, col=1, main="Histogram of total number of steps per day", xlab = "Total # of Steps in a Day")

# mean of total # steps per day
mean(tableDateStepsImpute$steps)

# median of total # steps per day
median(tableDateStepsImpute$steps)

# convert date to date class
dataFrame$date <- as.Date(dataFrame$date, "%Y-%m-%d")

# add column of day of week
dataFrame$day <- weekdays(dataFrame$date)

# add column called weekday
dataFrame$dayType <- c("weekday")

for(i in 1:nrow(dataFrame)){
        if(dataFrame$day == "Saturday" || dataFrame$day == "Sunday"){
                dataFrame$dayType[i] <- "weekend"
        }
}

# convert day time
dataFrame$dayType <- as.factor(dataFrame$dayType)

# aggregate steps as interval to get average number of steps in interval across all days
tableIntervalStepsImputed <- aggregate(steps ~ interval + dayType, dataFrame, mean)

# create plot
library(ggplot2)

qplot(interval, steps, data=tableIntervalStepsImputed, geom=c("line"), xlab = "Interval", ylab = "# of Steps", main = "") + facet_wrap(~ dayType, ncol = 1)

