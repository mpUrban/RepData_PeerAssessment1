


## Loading and preprocessing the data

zipUrl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
if(!file.exists('./data')){dir.create('./data')}
download.file(zipUrl, destfile = './data/dataset.zip', method = 'curl')
unzip(zipfile = './data/dataset.zip', exdir = './data/')

library(dplyr)
library(ggplot2)
library(xtable)

# Reading CSV data

df <- read.csv('./data/activity.csv')

## What is mean total number of steps taken per day?

# Calculating table of total daily steps
totSteps <- df %>%
        group_by(date) %>%
        summarise(totSteps = sum(steps, na.rm = TRUE))

head(totSteps)


# Plotting histogram of daily step frequency
hist(totSteps$totSteps,
     xlab = 'Frequency of Total Steps',
     breaks = 10,
     main = 'Histogram of Total Steps')

# Calculating the mean of daily total steps, rounded
meanDailySteps <- round(mean(totSteps$totSteps, na.rm = TRUE))
print(meanDailySteps)

# Calculating the median of daily total steps
medianDailySteps <- median(totSteps$totSteps, na.rm = TRUE)
print(medianDailySteps)


## What is the average daily activity pattern?

avgDaily <- df %>%
        group_by(interval) %>%
        summarise(avgInterval = mean(steps, na.rm = TRUE))

p1 <- ggplot(data=avgDaily, aes(x=avgDaily$interval, y=avgDaily$avgInterval, group=1)) +
        geom_line() + 
        labs(title = 'Time Series of Average Daily Steps',
             x = 'Average Interval',
             y = 'Average Daily Steps')

print(p1)

# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?

peak <- avgDaily$interval[which.max(avgDaily$avgInterval)]
print(peak)     

## Imputing missing values

# Total of rows with missing values:
naCount <- sum(is.na(df$steps))
print(naCount)

# Impute missing values, using median value

dfImputed <- df

dfImputed$steps[which(is.na(dfImputed$steps))] <- avgDaily$avgInterval

totStepsImp <- dfImputed %>%
        group_by(date) %>%
        summarise(totSteps = sum(steps, na.rm = TRUE))

hist(totStepsImp$totSteps,
     xlab = 'Frequency of Total Steps',
     breaks = 10,
     main = 'Histogram of Total Steps')

# Do these values differ from the estimates from the first part of the
# assignment? 

#Yes, there is a large amount of missing data, however the shape is similar

# What is the impact of imputing missing data on the estimates of
# the total daily number of steps?

# Calculating the mean of daily total steps, rounded
meanDailyStepsImp <- round(mean(totStepsImp$totSteps, na.rm = TRUE))
print(meanDailyStepsImp)

# Calculating the median of daily total steps
medianDailyStepsImp <- median(totStepsImp$totSteps, na.rm = TRUE)
print(medianDailyStepsImp)

imputationResults <- data.frame(
        Means = c(meanDailySteps, meanDailyStepsImp),
        Medians = c(medianDailySteps, medianDailyStepsImp)
)

row.names(imputationResults) <- c('Original', 'Imputed')

print(xtable(imputationResults), type="html")


## Are there differences in activity patterns between weekdays and weekends?

weekdays(as.Date(df$date[1]))

dfImp2 <- dfImputed

dfImp2$day <- weekdays(as.Date(dfImp2$date))

dfImp2$partOfWeek <- 'weekday'
dfImp2$partOfWeek[dfImp2$day == 'Saturday' | dfImp2$day == 'Sunday'] <- 'weekend'


partOfWeekResults <- dfImp2  %>%
        group_by(partOfWeek, interval) %>%
        summarise(avgInterval = mean(steps, na.rm = TRUE))


p2 <- ggplot(partOfWeekResults, aes(x = interval, y = avgInterval, group = 1)) + 
        geom_line() +
        facet_grid(rows = vars(partOfWeek)) + 
        xlab('Time Interval (5min)') +
        ylab('Average Daily Steps per Interval')+
        ggtitle('Variance of Weekday vs Weekend Activity Pattern')

print(p2)


